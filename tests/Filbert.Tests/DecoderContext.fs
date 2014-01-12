module DecoderContext

open System
open System.IO
open NUnit.Framework
open FsUnit
open Filbert.Core
open Filbert.Utils

[<TestFixture>]
type ``Given a decoder context`` () =
    let maxBufferSize = 1024

    [<Test>]
    [<ExpectedException(typeof<InsufficientNumberOfBytes>)>]
    member test.``when the stream is empty, read byte should except with InsufficientNumberOfBytes`` () =
        use memStream = new MemoryStream([||])
        use ctx       = new DecoderContext(memStream)

        ctx.ReadByte() |> should throw typeof<InsufficientNumberOfBytes>

    [<Test>]
    member test.``when there is sufficient data in the stream, read byte should return them all`` () =
        let input     = [| 1uy; 2uy; 3uy; |]
        use memStream = new MemoryStream(input)
        use ctx       = new DecoderContext(memStream)

        ctx.ReadByte() |> should equal <| 1uy
        ctx.ReadByte() |> should equal <| 2uy
        ctx.ReadByte() |> should equal <| 3uy
    
    [<Test>]
    member test.``when there is sufficient data in the stream, read bytes should return them all`` () =
        let input     = [| 1uy; 2uy; 3uy; |]
        use memStream = new MemoryStream(input)
        use ctx       = new DecoderContext(memStream)

        ctx.ReadBytes 3 |> should equal <| [| 1uy; 2uy; 3uy |]
    
    [<Test>]
    [<ExpectedException(typeof<InsufficientNumberOfBytes>)>]
    member test.``when there is insufficient data in the stream, read bytes should except with InsufficientNumberOfBytes`` () =
        let input     = [| 1uy; 2uy; 3uy; |]
        use memStream = new MemoryStream(input)
        use ctx       = new DecoderContext(memStream)

        ctx.ReadBytes 4 |> should throw typeof<InsufficientNumberOfBytes>

    [<Test>]
    member test.``when there is insufficient data in buffer, more data should be loaded from stream and returned`` () =
        let input     = [| yield 1uy; for i = 1 to 1024 do yield 2uy |]
        use memStream = new MemoryStream(input)
        use ctx       = new DecoderContext(memStream)

        ctx.ReadByte()     |> should equal 1uy
        ctx.ReadBytes 1024 |> should equal <| [| for i = 1 to 1024 do yield 2uy |]

    [<Test>]
    member test.``when the buffer is not enough to hold data for the request, a bespoke array is used to load from stream`` () =
        let input     = [| for i = 1 to 1000 do yield 1uy
                           for i = 1 to 2000 do yield 2uy
                           for i = 1 to 72 do yield 3uy |]        
        use memStream = new MemoryStream(input)
        use ctx       = new DecoderContext(memStream)

        // this is coming from the first buffer
        ctx.ReadBytes 1000 |> should equal <| [| for i = 1 to 1000 do yield 1uy |]

        // this should be coming from a bespoke array
        let arr = ctx.ReadBytes 2000 
        arr |> should equal <| [| for i = 1 to 2000 do yield 2uy |]

        // the rest of the data in the stream is all in a new buffer
        let arr' = ctx.ReadBytes 72
        arr' |> should equal <| [| for i = 1 to 72 do yield 3uy |]