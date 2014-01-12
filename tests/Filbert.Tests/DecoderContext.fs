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

        ctx.ReadBytes 3 |> should equal <| new ArraySegment<byte>(input, 0, 3)
    
    [<Test>]
    [<ExpectedException(typeof<InsufficientNumberOfBytes>)>]
    member test.``when there is insufficient data in the stream, read bytes should except with InsufficientNumberOfBytes`` () =
        let input     = [| 1uy; 2uy; 3uy; |]
        use memStream = new MemoryStream(input)
        use ctx       = new DecoderContext(memStream)

        ctx.ReadBytes 4 |> should throw typeof<InsufficientNumberOfBytes>

    [<Test>]
    member test.``when there is insufficient data in buffer, more data should be loaded from stream and returned`` () =
        let input     = [| 1..1025 |] |> Array.map (fun _ -> 1uy)
        use memStream = new MemoryStream(input)
        use ctx       = new DecoderContext(memStream)

        ctx.ReadByte()     |> should equal 1uy
        ctx.ReadBytes 1024 |> should equal <| new ArraySegment<byte>(input, 1, 1024)

    [<Test>]
    member test.``when the buffer is not enough to hold data for the request, a bespoke array is used to load from stream`` () =
        let input     = [| 1..3072 |] |> Array.map (fun _ -> 1uy)
        use memStream = new MemoryStream(input)
        use ctx       = new DecoderContext(memStream)

        // this is coming from the first buffer
        ctx.ReadBytes 1000 |> should equal <| new ArraySegment<byte>(input, 0, 1000)

        // this should be coming from a bespoke array
        let seg = ctx.ReadBytes 2000 
        seg.Array.Length |> should equal 2000
        seg.Count        |> should equal 2000
        seg.Offset       |> should equal 0

        // the rest of the data in the stream is all in a new buffer
        let seg = ctx.ReadBytes 72
        seg.Array.Length |> should equal maxBufferSize
        seg.Count        |> should equal 72
        seg.Offset       |> should equal 0