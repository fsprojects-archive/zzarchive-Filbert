module Filbert.RpcClient

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

let decodeBERP (stream : Stream) =
    async {
        // read the header (4 bytes)
        let! header = stream.AsyncRead(4)

        let len = bigEndianInteger header
        printf "Response BERT is %d bytes long" len

        return decode stream
    }

let call (modName : string, funName : string, args : Bert) =
    async {
        let client = new TcpClient("localhost", 9997)
        use stream = client.GetStream()

        do! encodeBERP "call" modName funName args stream
        let! response = decodeBERP stream

        printf "Received %A" response
    }
    |> Async.RunSynchronously

let cast (moduleName : string, funName : string, args : Bert) =
    let bert = Tuple [| Atom "cast"; Atom moduleName; Atom funName; args |]
    ()