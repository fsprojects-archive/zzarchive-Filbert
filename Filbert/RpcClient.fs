namespace Filbert.Rpc

open System.IO
open System.Net.Sockets
open Filbert.Core
open Filbert.Encoder
open Filbert.Decoder

// make sure we don't have any arithmetic overflows
open Checked

/// Represents the different types of error that can be returned by the server
type ErrorType =
    | Protocol      of uint32
    | Server        of uint32
    | User          of uint32
    | Proxy         of uint32

    override this.ToString() =
        match this with
        | Protocol(0u)  -> "Undesignated"
        | Protocol(1u)  -> "Unable to read header"
        | Protocol(2u)  -> "Unable to read data"
        | Server(0u)    -> "Undesignated"
        | Server(1u)    -> "No such module"
        | Server(2u)    -> "No such function"
        | User(n)       -> sprintf "User error code : %d" n
        | Proxy(n)      -> sprintf "Proxy error code : %d" n
        | _             -> sprintf "Unknown error code : %A" this

exception UndesignatedProtocolException
exception UnableToReadHeaderException
exception UnableToReadDataException
exception UndesignatedServerException
exception NoSuchModuleException
exception NoSuchFunctionException

[<AutoOpen>]
module BERP =
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

            let len = bigEndianInteger header
            printf "Response BERT is %d bytes long" len

            return decode stream
        }

type BertRpcClient private (serviceUrl, port) = 
    let call modName funName arg =
        async {
            use client = new TcpClient(serviceUrl, port)
            use stream = client.GetStream()

            do! encodeBERP "call" modName funName arg stream
            let! response = decodeBERP stream

            match response with
            | Tuple [| Atom "error"; _ as errorDetails |] 
                -> printf "Error : %A" errorDetails
            | _ -> printf "Received response : %A" response
        }
        |> Async.RunSynchronously

    let cast moduleName funName arg =
        async {
            use client = new TcpClient(serviceUrl, port)
            use stream = client.GetStream()

            do! encodeBERP "cast" modName funName arg stream
            let! response = decodeBERP stream

            match response with
            | Tuple [| Atom "error"; _ as errorDetails |] 
                -> printf "Error : %A" errorDetails
            | Tuple [| Atom "noreply" |] 
                -> printfn "Request was successful"
        }
        |> Async.RunSynchronously

    /// Static helper method to start a BERT rpc client
    static member Start (serviceUrl, port) = RpcClient(serviceUrl, port)

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