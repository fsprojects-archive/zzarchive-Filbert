module Filbert.Tests.Decoder

open System
open System.IO
open NUnit.Framework
open FsUnit
open Filbert.Decoder

let test tag byteVals expected =
    let arr = Array.append [| 131uy; tag |] byteVals
    use stream = new MemoryStream(arr)

    decode stream |> should equal expected

[<TestFixture>]
type ``Given an 8-bit small integer`` () =
    let test = test 97uy

    [<Test>]
    member this.``when it's 0 it should return 0`` () = test [| 0uy |] 0

    [<Test>]
    member this.``when it's 255 it should return 255`` () = test [| 255uy |] 255

[<TestFixture>]
type ``Given a 32-bit integer`` () =
    let test = test 98uy

    [<Test>]
    member this.``when it's 256 it should return 256`` () = 
        test [| 0uy; 0uy; 1uy; 0uy |] 256

    [<Test>]
    member this.``when it's -1 it should return -1`` () = 
        test [| 255uy; 255uy; 255uy; 255uy |] -1

    [<Test>]
    member this.``when it's 2147483647 it should return 2147483647`` () =
        test [| 127uy; 255uy; 255uy; 255uy |] 2147483647

    [<Test>]
    member this.``when it's -2147483648 it should return -2147483648`` () =
        test [| 128uy; 0uy; 0uy; 0uy |] -2147483648

[<TestFixture>]
type ``Given a float`` () =
    let test = test 99uy

    [<Test>]
    member this.``when it's 0.0 it should return 0.0`` () =
        let byteVals = [| 48uy; 46uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 
                          48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 
                          48uy; 48uy; 101uy; 43uy; 48uy; 48uy; 48uy; 0uy; 0uy; 0uy; 0uy |]
        test byteVals 0.0

    [<Test>]
    member this.``when it's 99.99 it should return 99.99`` () =
        let byteVals = [| 57uy; 46uy; 57uy; 57uy; 56uy; 57uy; 57uy; 57uy; 57uy; 57uy; 
                          57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 53uy; 48uy; 48uy; 
                          48uy; 48uy; 101uy; 43uy; 48uy; 48uy; 49uy; 0uy; 0uy; 0uy; 0uy |]
        test byteVals 99.99

    [<Test>]
    member this.``when it's -1234.56 it should return -1234.56`` () =
        let byteVals = [| 45uy; 49uy; 46uy; 50uy; 51uy; 52uy; 53uy; 53uy; 57uy; 57uy; 
                          57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 48uy; 
                          48uy; 48uy; 48uy; 101uy; 43uy; 48uy; 48uy; 51uy; 0uy; 0uy; 0uy |]
        test byteVals -1234.56

[<TestFixture>]
type ``Given an atom`` () =
    let test = test 100uy

    [<Test>]
    member this.``when the atom is a it should return the string 'a'`` () =
        test [| 0uy; 1uy; 97uy |] "a"

    [<Test>]
    member this.``when the atom is abc it should return the string 'abc'`` () =
        test [| 0uy; 3uy; 97uy; 98uy; 99uy |] "abc"

    /// Note: max allowed length for atom is 255uy;  term_to_binary(a repeated 256 times) will except
    [<Test>]
    member this.``when the atom is a repeated 255 times it should return a string where 'a' is repeated 255 times`` () =
        let byteVals = Array.create<byte> 255 97uy |> Array.append [| 0uy; 255uy |] 
        let strVal = Array.create<char> 255 'a' |> (fun arr -> new string(arr))
        test byteVals strVal

    [<Test>]
    [<ExpectedException(typeof<InvalidAtomLength>)>]
    member this.``when received an atom with length greater than 255 then it should except`` () =
        let byteVals = [| 1..256 |] 
                       |> Array.map (fun _ -> 97uy)
                       |> Array.append [| 1uy; 0uy |] 
        test byteVals ""

    [<Test>]
    [<ExpectedException(typeof<InvalidAtomLength>)>]
    member this.``when received an atom with 0 length then it should except`` () =
        test [| 0uy; 0uy |] ""
            
[<TestFixture>]
type ``Given a small tuple`` () =
    let test : byte[] -> obj[] -> unit = test 104uy

    [<Test>]
    member this.``when the tuple is empty it should return an empty array`` () =
        test [| 0uy |] [||]

    [<Test>]
    member this.``when the tuple is { 1 } it should return an [| 1 |]`` () =
        test [| 1uy; 97uy; 1uy |] [| 1 |]

    [<Test>]
    member this.``when the tuple is { 1uy;  1234uy;  a } it should return an [| 1; "a" |]`` () =
        test [| 3uy; 97uy; 1uy; 98uy; 0uy; 0uy; 4uy; 210uy; 100uy; 0uy; 1uy; 97uy |] [| 1; 1234; "a" |]

    [<Test>]
    member this.``when the tuple is 1 repeated 255 times ({ 1uy;  1uy;  ... }) it should return [| 1; 1; ... |]`` () =
        let byteVals = [| for i = 1 to 255 do yield! [| 97uy; 1uy |] |]
                       |> Array.append [| 255uy |]
        let expected = Array.create<obj> 255 1
        test byteVals expected

[<TestFixture>]
type ``Given a large tuple`` () =
    let test : byte[] -> obj[] -> unit = test 105uy

    [<Test>]
    member this.``when the tuple is 1 repeated 256 times ({ 1uy;  1uy;  ... }) it should return [| 1; 1; ... |]`` () =
        let byteVals = [| for i = 1 to 256 do yield! [| 97uy; 1uy |] |]
                       |> Array.append [| 0uy; 0uy; 1uy; 0uy |]
        let expected = Array.create<obj> 256 1
        test byteVals expected