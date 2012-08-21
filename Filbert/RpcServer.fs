module Filbert.RpcServer

open System
open System.IO
open System.Net
open Filbert.Core
open Filbert.Encoder
open Filbert.Decoder

// make sure we don't have any arithmetic overflows
open Checked

type HttpListener with
    static member Run (url:string, handler: (HttpListenerRequest -> HttpListenerResponse -> Async<unit>)) =
        let listener = new HttpListener()
        listener.Prefixes.Add url
        listener.Start()
        let asynctask = Async.FromBeginEnd(listener.BeginGetContext,listener.EndGetContext)
        async {
            while true do
                let! context = asynctask
                Async.Start (handler context.Request context.Response)
        } |> Async.Start
        listener

type BertRpcServer private () =
    static member Start (baseUrl) =
        HttpListener.Run(baseUrl, (fun req resp ->
            async {
                let inputBert = decode req.InputStream
                let outputBert = 
                    match inputBert with
                    | Tuple [| Atom "call"; _; _; _ |] -> Tuple [| Atom "reply"; Atom "hello world!" |]
                    | Tuple [| Atom "cast"; _; _; _ |] -> Tuple [| Atom "noreply" |]
                    | _ -> Tuple [| Atom "error"; Tuple [| Atom "server"; Integer 2 |] |]

                use memStream = new MemoryStream()
                encode outputBert memStream
                let header = int memStream.Length

                // write header bytes
                do! resp.OutputStream.AsyncWrite(getBigEndianBytesInt header)
                do! resp.OutputStream.AsyncWrite(memStream.ToArray())
                resp.OutputStream.Close()
            }
        )) |> ignore