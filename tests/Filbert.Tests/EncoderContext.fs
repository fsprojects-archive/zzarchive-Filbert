module EncoderContext

open System
open System.IO
open NUnit.Framework
open FsUnit
open Filbert.Core
open Filbert.Utils

[<TestFixture>]
type ``Given an encoder context`` () =
    let bufferSize = 1024

    [<Test>]
    member test.``when a byte is written to the context it should be written to the stream`` () =
        use memStream = new MemoryStream()
        use ctx       = new EncoderContext(memStream)

        ctx.WriteByte 131uy
        (ctx :> IDisposable).Dispose()
        memStream.ToArray() |> should equal [| 131uy |]

    [<Test>]
    member test.``when an array of bytes is written to the context they should be written to the stream`` () =
        use memStream = new MemoryStream()
        use ctx       = new EncoderContext(memStream)

        ctx.WriteBytes [| 1uy; 2uy; 3uy |]
        (ctx :> IDisposable).Dispose()
        memStream.ToArray() |> should equal [| 1uy; 2uy; 3uy |]

    [<Test>]
    member test.``when writing multiple large arrays everything should be written to the stream`` () =
        use memStream = new MemoryStream()
        use ctx       = new EncoderContext(memStream)

        let input1 = [| for i = 1 to 1000 do yield 1uy |]
        ctx.WriteBytes input1

        let input2 = [| for i = 1 to 1000 do yield 2uy |]
        ctx.WriteBytes input2
        
        (ctx :> IDisposable).Dispose()
        memStream.ToArray() |> should equal <| Array.append input1 input2

    [<Test>]
    member test.``when the buffer is full the next write should flush the current buffer to the stream`` () =
        use memStream = new MemoryStream()
        use ctx       = new EncoderContext(memStream)

        let input1 = [| for i = 1 to bufferSize do yield 1uy |]
        ctx.WriteBytes input1

        ctx.WriteByte 2uy
        memStream.ToArray() |> should equal input1

        (ctx :> IDisposable).Dispose()
        memStream.ToArray() |> should equal <| Array.append input1 [| 2uy |]

    [<Test>]
    member test.``when writing an array larger than the current buffer is flushed`` () =
        use memStream = new MemoryStream()
        use ctx       = new EncoderContext(memStream)

        let smallArr  = [| 3uy; 2uy; 1uy; |]
        ctx.WriteBytes smallArr

        let largeArr  = [| for i = 1 to bufferSize + 1 do yield 2uy |]
        ctx.WriteBytes largeArr
        memStream.ToArray() |> should equal <| Array.append smallArr largeArr