namespace Filbert.Rpc

open System.IO
open System.Net.Sockets
open Filbert.Core
open Filbert.Encoder
open Filbert.Decoder

// make sure we don't have any arithmetic overflows
open Checked

exception UndesignatedProtocolException     of string * string * string
exception UnableToReadHeaderException       of string * string * string
exception UnableToReadDataException         of string * string * string
exception UndesignatedServerException       of string * string * string
exception NoSuchModuleException             of string * string * string
exception NoSuchFunctionException           of string * string * string
exception UnknownException                  of string

type BertRpcClient private (serviceUrl, port) = 
    /// Encodes a BERP (Binary ERlang Packets) asynchronously
    let encodeBERP action modName funName args (stream : Stream) =
        // encode the request into a BERT tuple, e.g. { call, nat, add, [1, 2] }
        let reqBert = Tuple [| Atom action; Atom modName; Atom funName; args |]

        // encode the request BERT
        use memStream = new MemoryStream()
        encode reqBert memStream

        // how many bytes does the BERT take?
        let header = getBigEndianBytesInt <| int memStream.Length
    
        async {
            do! stream.AsyncWrite(header)
            do! stream.AsyncWrite(memStream.ToArray())
        }

    /// Decodes a BERP (Binary ERlang Packets) asynchronously
    let decodeBERP (stream : Stream) =
        async {
            // read the header (4 bytes)
            let! header = stream.AsyncRead(4)
            return decode stream
        }

    let handleError = 
        function 
        | Tuple [| Atom "server"; Integer 0; Binary cls; Binary detail; Binary backTrace |] 
            -> raise <| UndesignatedServerException(string cls, string detail, string backTrace)
        | Tuple [| Atom "server"; Integer 1; Binary cls; Binary detail; Binary backTrace |] 
            -> raise <| NoSuchModuleException(string cls, string detail, string backTrace)
        | Tuple [| Atom "server"; Integer 2; Binary cls; Binary detail; Binary backTrace |] 
            -> raise <| NoSuchFunctionException(string cls, string detail, string backTrace)
        | Tuple [| Atom "protocol"; Integer 0; Binary cls; Binary detail; Binary backTrace |] 
            -> raise <| UndesignatedProtocolException(string cls, string detail, string backTrace)
        | Tuple [| Atom "protocol"; Integer 1; Binary cls; Binary detail; Binary backTrace |] 
            -> raise <| UnableToReadHeaderException(string cls, string detail, string backTrace)
        | Tuple [| Atom "protocol"; Integer 2; Binary cls; Binary detail; Binary backTrace |] 
            -> raise <| UnableToReadDataException(string cls, string detail, string backTrace)
        | unknown -> raise <| UnknownException(string unknown)

    // makes a RPC call
    let rpc encode =
        async {
            let client = new TcpClient(serviceUrl, port)
            let stream = client.GetStream()

            // encodes the request BERP into the network stream
            do! encode stream

            // now wait for and decode the response
            let! response = decodeBERP stream

            stream.Close()
            client.Close()

            match response with
            | Tuple [| Atom "error"; _ as errorDetails |] -> handleError errorDetails
            | _                                           -> ()

            return response
        }

    let call modName funName arg = async { return! rpc (encodeBERP "call" modName funName arg) }
    let cast modName funName arg = async { do! rpc (encodeBERP "cast" modName funName arg) |> Async.Ignore }

    /// Static helper method to start a BERT rpc client
    static member Start (serviceUrl, port) = BertRpcClient(serviceUrl, port)

    /// Makes a synchronous request, this is mapped to a BERT tuple of the form:
    ///     { call, modName, funName, arg }
    /// e.g. { call, nat, add, [1, 2] }
    /// Successful responses will be in the format of a BERT tuple { reply, Result }
    member this.Call(modName, funName, arg) = call modName funName arg

    /// Makes an asynchronous request (fire-and-forget), this is mapped to a BERT tuple
    /// of the form:
    ///     { cast, modName, funName, arg }
    /// e.g. { cast, nat, die, 666 }
    member this.Cast(modName, funName, arg) = cast modName funName arg