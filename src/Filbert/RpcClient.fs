namespace Filbert.Rpc

open System.IO
open System.Net.Sockets
open Filbert.Core
open Filbert.Encoder
open Filbert.Decoder

// make sure we don't have any arithmetic overflows
open Checked

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
            | Tuple [| Atom "error"; _ as errorDetails |] 
                -> failwith <| string errorDetails
            | _ -> ()

            return response
        }

    let call modName funName args = 
        async { return! rpc (encodeBERP "call" modName funName (List args)) }

    let cast modName funName args = 
        async { do! rpc (encodeBERP "cast" modName funName (List args)) |> Async.Ignore }

    /// Static helper method to start a BERT rpc client
    static member Start (serviceUrl, port) = BertRpcClient(serviceUrl, port)

    /// Makes a synchronous request, this is mapped to a BERT tuple of the form:
    ///     { call, modName, funName, arg }
    /// e.g. { call, nat, add, [1, 2] }
    /// Successful responses will be in the format of a BERT tuple { reply, Result }
    member this.Call(modName, funName, [<System.ParamArrayAttribute>] args) = 
        call modName funName args

    /// Makes a synchronous request and returns the response as a Task<Bert>
    member this.CallAsTask(modName, funName, [<System.ParamArrayAttribute>] args) = 
        this.Call(modName, funName, args) |> Async.StartAsTask

    /// Makes an asynchronous request (fire-and-forget), this is mapped to a BERT tuple
    /// of the form:
    ///     { cast, modName, funName, arg }
    /// e.g. { cast, nat, die, [ 666 ] }
    member this.Cast(modName, funName, [<System.ParamArrayAttribute>] args) = 
        cast modName funName args

    /// Makes an asynchronous request (fire-and-forget) and returns a Task<unit>
    member this.CastAsTask(modName, funName, [<System.ParamArrayAttribute>] args) = 
        this.Cast(modName, funName, args) |> Async.StartAsTask