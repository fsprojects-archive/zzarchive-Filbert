module Filbert.Tests.Decoder

open System
open System.IO
open NUnit.Framework
open FsUnit
open Filbert.Core
open Filbert.Decoder

let test tag byteVals expected =
    let arr = Array.append [| 131uy; tag |] byteVals
    use stream = new MemoryStream(arr)

    let actual = decode stream 
    actual |> should equal expected

[<TestFixture>]
type ``Given an 8-bit small integer`` () =
    let test = test 97uy

    [<Test>]
    member x.``when it's 0 it should return 0`` () = test [| 0uy |] (Integer 0)

    [<Test>]
    member x.``when it's 255 it should return 255`` () = test [| 255uy |] (Integer 255)

[<TestFixture>]
type ``Given a 32-bit integer`` () =
    let test = test 98uy

    [<Test>]
    member x.``when it's 256 it should return 256`` () = 
        test [| 0uy; 0uy; 1uy; 0uy |] (Integer 256)

    [<Test>]
    member x.``when it's -1 it should return -1`` () = 
        test [| 255uy; 255uy; 255uy; 255uy |] (Integer -1)

    [<Test>]
    member x.``when it's 2147483647 it should return 2147483647`` () =
        test [| 127uy; 255uy; 255uy; 255uy |] (Integer 2147483647)

    [<Test>]
    member x.``when it's -2147483648 it should return -2147483648`` () =
        test [| 128uy; 0uy; 0uy; 0uy |] (Integer -2147483648)

[<TestFixture>]
type ``Given a float`` () =
    let test = test 99uy

    [<Test>]
    member x.``when it's 0.0 it should return 0.0`` () =
        let byteVals = [| 48uy; 46uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 
                          48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 
                          48uy; 48uy; 101uy; 43uy; 48uy; 48uy; 48uy; 0uy; 0uy; 0uy; 0uy |]
        test byteVals (Float 0.0)

    [<Test>]
    member x.``when it's 99.99 it should return 99.99`` () =
        let byteVals = [| 57uy; 46uy; 57uy; 57uy; 56uy; 57uy; 57uy; 57uy; 57uy; 57uy; 
                          57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 53uy; 48uy; 48uy; 
                          48uy; 48uy; 101uy; 43uy; 48uy; 48uy; 49uy; 0uy; 0uy; 0uy; 0uy |]
        test byteVals (Float 99.99)

    [<Test>]
    member x.``when it's -1234.56 it should return -1234.56`` () =
        let byteVals = [| 45uy; 49uy; 46uy; 50uy; 51uy; 52uy; 53uy; 53uy; 57uy; 57uy; 
                          57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 48uy; 
                          48uy; 48uy; 48uy; 101uy; 43uy; 48uy; 48uy; 51uy; 0uy; 0uy; 0uy |]
        test byteVals (Float -1234.56)

[<TestFixture>]
type ``Given an atom`` () =
    let test = test 100uy

    [<Test>]
    member x.``when the atom is a it should return the string 'a'`` () =
        test [| 0uy; 1uy; 97uy |] (Atom "a")

    [<Test>]
    member x.``when the atom is abc it should return the string 'abc'`` () =
        test [| 0uy; 3uy; 97uy; 98uy; 99uy |] (Atom "abc")

    /// Note: max allowed length for atom is 255uy;  term_to_binary(a repeated 256 times) will except
    [<Test>]
    member x.``when the atom is a repeated 255 times it should return a string where 'a' is repeated 255 times`` () =
        let byteVals = Array.create<byte> 255 97uy |> Array.append [| 0uy; 255uy |] 
        let strVal = Array.create<char> 255 'a' |> (fun arr -> new string(arr))
        test byteVals (Atom strVal)

    [<Test>]
    [<ExpectedException(typeof<InvalidAtomLength>)>]
    member x.``when received an atom with length greater than 255 then it should except`` () =
        let byteVals = [| 1..256 |] 
                       |> Array.map (fun _ -> 97uy)
                       |> Array.append [| 1uy; 0uy |] 
        test byteVals (Atom "")

    [<Test>]
    [<ExpectedException(typeof<InvalidAtomLength>)>]
    member x.``when received an atom with 0 length then it should except`` () =
        test [| 0uy; 0uy |] (Atom "")
            
[<TestFixture>]
type ``Given a small tuple`` () =
    let test = test 104uy

    [<Test>]
    member x.``when the tuple is empty it should return an empty array`` () =
        test [| 0uy |] (Tuple [||])

    [<Test>]
    member x.``when the tuple is { 1 } it should return an [| 1 |]`` () =
        test [| 1uy; 97uy; 1uy |] (Tuple [| (Integer 1) |])

    [<Test>]
    member x.``when the tuple is { 1uy;  1234uy;  a } it should return an [| 1; "a" |]`` () =
        let expected = (Tuple [| (Integer 1); (Integer 1234); (Atom "a") |])
        test [| 3uy; 97uy; 1uy; 98uy; 0uy; 0uy; 4uy; 210uy; 100uy; 0uy; 1uy; 97uy |] expected

    [<Test>]
    member x.``when the tuple is 1 repeated 255 times ({ 1uy;  1uy;  ... }) it should return [| 1; 1; ... |]`` () =
        let byteVals = [| for i = 1 to 255 do yield! [| 97uy; 1uy |] |]
                       |> Array.append [| 255uy |]
        let expected = Tuple(Array.create<Bert> 255 (Integer 1))
        test byteVals expected

[<TestFixture>]
type ``Given a large tuple`` () =
    let test = test 105uy

    [<Test>]
    member x.``when the tuple is 1 repeated 256 times ({ 1uy;  1uy;  ... }) it should return [| 1; 1; ... |]`` () =
        let byteVals = [| for i = 1 to 256 do yield! [| 97uy; 1uy |] |]
                       |> Array.append [| 0uy; 0uy; 1uy; 0uy |]
        let expected = Tuple(Array.create<Bert> 256 (Integer 1))
        test byteVals expected

[<TestFixture>]
type ``Given a nill`` () =
    let test = test 106uy

    [<Test>]
    member x.``when received nil it should return None`` () = test [||] Nil

[<TestFixture>]
type ``Given a string (bytelist)`` () =
    let test = test 107uy

    [<Test>]
    member x.``when the string is 'a' it should return 'a'`` () =
        test [| 0uy; 1uy; 97uy |] (ByteList [| 97uy |])

    [<Test>]
    member x.``when the string is 'abc' it should return 'abc'`` () =
        test [| 0uy; 3uy; 97uy; 98uy; 99uy |] (ByteList [| 97uy; 98uy; 99uy |])

    // Note: the max length for a string is 65534
    [<Test>]
    member x.``when the string is 'a' repeated 65534 times it should return 'aa...'`` () =
        let byteVals = [| for i = 1 to 65534 do yield 97uy |]
                       |> Array.append [| 255uy; 254uy |]
        let expected = ByteList <| [| for i = 1 to 65534 do yield 97uy |]
        test byteVals expected

[<TestFixture>]
type ``Given a list`` () =
    let test = test 108uy

    [<Test>]
    member x.``when it's [{ bert, true }, 1, { 1, 2 }, [ 1, a ]] it should return a list of 4 berts`` () =
        let byteVals = [| 0uy; 0uy; 0uy; 4uy; 104uy; 2uy; 100uy; 0uy; 4uy; 98uy; 101uy; 114uy; 116uy; 
                          100uy; 0uy; 4uy; 116uy; 114uy; 117uy; 101uy; 97uy; 1uy; 104uy; 2uy; 97uy; 1uy; 97uy; 2uy; 
                          108uy; 0uy; 0uy; 0uy; 2uy; 97uy; 1uy; 100uy; 0uy; 1uy; 97uy; 106uy; 106uy |]
        let expected = List [| Boolean true; 
                               Integer 1; 
                               Tuple [| Integer 1; Integer 2 |]; 
                               List [| Integer 1; Atom "a" |] |]
        test byteVals expected

[<TestFixture>]
type ``Given a binary`` () =
    let test = test 109uy

    [<Test>]
    member x.``when the binary array is empty <<>> it should return empty array`` () =
        test [| 0uy; 0uy; 0uy; 0uy |] (Binary [||])

    [<Test>]
    member x.``when the binary array is <<"Roses are red">> it should return an array of 13 bytes`` () =
        let expected = [| 82uy; 111uy; 115uy; 101uy; 115uy; 32uy; 97uy; 
                          114uy; 101uy; 32uy; 114uy; 101uy; 100uy |]
        let byteVals = Array.append [| 0uy; 0uy; 0uy; 13uy |] expected
                          
        test byteVals (Binary expected)

[<TestFixture>]
type ``Given a small big`` () =
    let test = test 110uy

    [<Test>]
    member x.``when the big num is 111111111111111 it should return 111111111111111I`` () =
        let byteVals = [| 6uy; 0uy; 199uy; 241uy; 78uy; 18uy; 14uy; 101uy |]
        test byteVals (BigInteger 111111111111111I)

    [<Test>]
    member x.``when the big num is 111... (30 times) it should return 111...I`` () =
        let byteVals = [| 13uy; 0uy; 199uy; 113uy; 28uy; 7uy; 26uy; 197uy; 126uy; 178uy; 
                          250uy; 244uy; 4uy; 103uy; 1uy |]
        test byteVals (BigInteger 111111111111111111111111111111I)

[<TestFixture>]
type ``Given a complex BERT`` () =
    let test = test 104uy

    [<Test>]
    member x.``when it's { bert, true } it should return boolean true`` () =
        let byteVals = [| 2uy; 100uy; 0uy; 4uy; 98uy; 101uy; 114uy; 116uy; 
                          100uy; 0uy; 4uy; 116uy; 114uy; 117uy; 101uy |]
        test byteVals (Boolean true)

    [<Test>]
    member x.``when it's { bert, false } it should return boolean false`` () =
        let byteVals = [| 2uy; 100uy; 0uy; 4uy; 98uy; 101uy; 114uy; 116uy; 100uy; 0uy; 
                          5uy; 102uy; 97uy; 108uy; 115uy; 101uy |]
        test byteVals (Boolean false)

    [<Test>]
    member x.``when it's { bert, nil } it should return empty array`` () =
        let byteVals = [| 2uy; 100uy; 0uy; 4uy; 98uy; 101uy; 114uy; 116uy; 100uy; 0uy; 
                          3uy; 110uy; 105uy; 108uy |]
        test byteVals EmptyArray

    [<Test>]
    member this.``when it's { bert, time, 1255, 295581, 446228 } it should return 2009-10-11 14:12:01 and 446,228 microseconds`` () =
        let byteVals = [| 5uy; 100uy; 0uy; 4uy; 98uy; 101uy; 114uy; 116uy; 100uy; 0uy; 
                          4uy; 116uy; 105uy; 109uy; 101uy; 98uy; 0uy; 0uy; 4uy; 231uy; 
                          98uy; 0uy; 4uy; 130uy; 157uy; 98uy; 0uy; 6uy; 207uy; 20uy |]
        let expected = DateTime(2009, 10, 11, 21, 13, 1).AddSeconds(0.446228)
        test byteVals (Time expected)

    [<Test>]
    member this.``when it's { bert, dict, [{ name, <<"Tom">> }, { age, 30 }] } then it should return a dictionary`` () =
        let byteVals = [| 3uy; 100uy; 0uy; 4uy; 98uy; 101uy; 114uy; 116uy; 100uy; 0uy; 4uy; 100uy; 105uy; 99uy; 116uy; 108uy; 0uy; 0uy; 
                          0uy; 2uy; 104uy; 2uy; 100uy; 0uy; 4uy; 110uy; 97uy; 109uy; 101uy; 109uy; 0uy; 0uy; 0uy; 3uy; 84uy; 111uy; 109uy; 
                          104uy; 2uy; 100uy; 0uy; 3uy; 97uy; 103uy; 101uy; 97uy; 30uy; 106uy  |]

        let expected = [| (Atom "name", Binary [| 84uy; 111uy; 109uy |]); (Atom "age", Integer 30) |]
                       |> Map.ofArray
                       |> Dictionary

        test byteVals expected