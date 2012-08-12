module Filbert.Tests.Encoder

open System
open System.IO
open NUnit.Framework
open FsUnit
open Filbert.Core
open Filbert.Encoder

let getLongStr (moreThanThis : int) =
    let chars = { 0..moreThanThis } |> Seq.map (fun _ -> 'a') |> Seq.toArray
    new string(chars)

let test tag bert expected =
    use stream = new MemoryStream()
    encode bert stream

    let actual = stream.ToArray()
    actual |> should equal <| (expected |> Array.append [| 131uy; tag |])

[<TestFixture>]
type ``Given an integer`` () =
    [<Test>]
    member x.``when it's 0 it should return SMALL_INT_EXT 0`` () =
        test 97uy (Integer 0) [| 0uy |]

    [<Test>]
    member x.``when it's 255 it should return SMALL_INT_EXT 255`` () =
        test 97uy (Integer 255) [| 255uy |]

    [<Test>]
    member x.``when it's 256 it should return INT_EXT 256`` () = 
        test 98uy (Integer 256) [| 0uy; 0uy; 1uy; 0uy |]

    [<Test>]
    member x.``when it's -1 it should return INT_EXT -1`` () =
        test 98uy (Integer -1) [| 255uy; 255uy; 255uy; 255uy |]

    [<Test>]
    member x.``when it's 2147483647 it should return INT_EXT 2147483647`` () =
        test 98uy (Integer 2147483647) [| 127uy; 255uy; 255uy; 255uy |]

    [<Test>]
    member x.``when it's -2147483648 it should return INT_EXT -2147483648`` () =
        test 98uy (Integer -2147483648) [| 128uy; 0uy; 0uy; 0uy |]

[<TestFixture>]
type ``Given a float`` () =
    let test = test 99uy

    [<Test>]
    member x.``when it's 0.0 it should return FLOAT_EXT 0.0`` () =
        let expected = [| 48uy; 46uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 
                          48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 48uy; 
                          48uy; 48uy; 101uy; 43uy; 48uy; 48uy; 48uy; 0uy; 0uy; 0uy; 0uy |]
        test (Float 0.0) expected

    [<Test>]
    member x.``when it's 99.99 it should return FLOAT_EXT 99.99`` () =
        let expected = [| 57uy; 46uy; 57uy; 57uy; 56uy; 57uy; 57uy; 57uy; 57uy; 57uy; 
                          57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 53uy; 48uy; 48uy; 
                          48uy; 48uy; 101uy; 43uy; 48uy; 48uy; 49uy; 0uy; 0uy; 0uy; 0uy |]
        test (Float 99.99) expected

    [<Test>]
    member x.``when it's -1234.56 it should return FLOAT_EXT -1234.56`` () =
        let expected = [| 45uy; 49uy; 46uy; 50uy; 51uy; 52uy; 53uy; 53uy; 57uy; 57uy; 
                          57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 57uy; 48uy; 
                          48uy; 48uy; 48uy; 101uy; 43uy; 48uy; 48uy; 51uy; 0uy; 0uy; 0uy |]
        test (Float -1234.56) expected

[<TestFixture>]
type ``Given an atom`` () =
    let test = test 100uy

    [<Test>]
    [<ExpectedException(typeof<InvalidAtomLength>)>]
    member x.``when the length of the atom is greater than max allowed it should except`` () =
        let longStr = getLongStr Constants.maxAtomLen
        test (Atom longStr) [||]

    [<Test>]
    [<ExpectedException(typeof<InvalidAtomLength>)>]
    member x.``when the length of the atom is 0 it should except`` () =
        test (Atom "") [||]

    [<Test>]
    member x.``when the atom is a it should return ATOM_EXT a`` () =
        test (Atom "a") [| 0uy; 1uy; 97uy |]

    [<Test>]
    member x.``when the atom is abc it should return ATOM_EXT abc`` () =
        test (Atom "abc") [| 0uy; 3uy; 97uy; 98uy; 99uy |]

    [<Test>]
    member x.``when the atom is a repeated 255 times it should return ATOM_EXT where a is repeated 255 times`` () =
        let expected = Array.create<byte> 255 97uy |> Array.append [| 0uy; 255uy |] 
        let str = Array.create<char> 255 'a' |> (fun arr -> new string(arr))
        test (Atom str) expected

[<TestFixture>]
type ``Given a tuple`` () =
    [<Test>]
    member x.``when the tuple is empty it should return SMALL_TUPLE_EXT with 0 arity and no elements`` () =
        test 104uy (Tuple [||]) [| 0uy |]

    [<Test>]
    member x.```when the tuple is { 1 } it should return SMALL_TUPLE_EXT with 1 arity and 1`` () =
        test 104uy (Tuple [| Integer 1 |]) [| 1uy; 97uy; 1uy |]

    [<Test>]
    member x.```when the tuple is { 1, 1234, a } it should return SMALL_TUPLE_EXT with 3 arity and 1, 1234 and a`` () =
        let expected = [| 3uy; 97uy; 1uy; 98uy; 0uy; 0uy; 4uy; 210uy; 100uy; 0uy; 1uy; 97uy |]
        test 104uy (Tuple [| Integer 1; Integer 1234; Atom "a" |]) expected

    [<Test>]
    member x.``when the tuple is 1 repeated 255 times { 1, 1, ... } it should return SMALL_TUPLE_EXT with 255 arity and 1 repeated 255 times`` () =
        let expected = [| for i = 1 to 255 do yield! [| 97uy; 1uy |] |]
                       |> Array.append [| 255uy |]
        test 104uy (Tuple <| Array.create<Bert> 255 (Integer 1)) expected

    [<Test>]
    member x.``when the tuple is 1 repeated 256 times { 1, 1, ... } it should return LARGE_TUPLE_EXT`` () =
        let expected = [| for i = 1 to 256 do yield! [| 97uy; 1uy |] |]
                       |> Array.append [| 0uy; 0uy; 1uy; 0uy |]
        test 105uy (Tuple <| Array.create<Bert> 256 (Integer 1)) expected

[<TestFixture>]
type ``Given a nill`` () =
    let test = test 106uy

    [<Test>]
    member x.``when received a nil it should return NIL_EXT`` () = test Nil [||]

[<TestFixture>]
type ``Given a string (bytelist)`` () =
    let test = test 107uy

    [<Test>]
    member x.``when the string is 'a' it should return STRING_EXT 'a'`` () =
        test (ByteList [| 97uy |]) [| 0uy; 1uy; 97uy |] 

    [<Test>]
    member x.``when the string is 'abc' it should return STRING_EXT 'abc'`` () =
        test (ByteList [| 97uy; 98uy; 99uy |]) [| 0uy; 3uy; 97uy; 98uy; 99uy |]

    // Note: the max length for a string is 65534
    [<Test>]
    member x.``when the string is 'a' repeated 65534 times it should return STRING_EXT 'aa...'`` () =
        let expected = [| for i = 1 to 65534 do yield 97uy |]
                       |> Array.append [| 255uy; 254uy |]
        test (ByteList <| [| for i = 1 to 65534 do yield 97uy |]) expected

    [<Test>]
    [<ExpectedException(typeof<InvalidStringLength>)>]
    member x.``when the string's length is greater than 65534 it should except`` () =
        test (ByteList <| [| for i = 1 to 65535 do yield 97uy |]) [||]

[<TestFixture>]
type ``Given a binary`` () =
    let test = test 109uy

    [<Test>]
    member x.``when the binary array is empty <<>> it should return BINARY_EXT with 0 length`` () =
        test (Binary [||]) [| 0uy; 0uy; 0uy; 0uy |]

    [<Test>]
    member x.``when the binary array is <<"Roses are red">> it should return BINARY_EXT with 13 length`` () =
        let bytes = [| 82uy; 111uy; 115uy; 101uy; 115uy; 32uy; 97uy; 
                       114uy; 101uy; 32uy; 114uy; 101uy; 100uy |]
        let expected = Array.append [| 0uy; 0uy; 0uy; 13uy |] bytes
                          
        test (Binary bytes) expected

[<TestFixture>]
type ``Given an empty array`` () =
    [<Test>]
    member x.``it should return SMALL_TUPLE_EXT for { bert, nil }`` () =
        let expected = [| 2uy; 
                          100uy; 0uy; 4uy; 98uy; 101uy; 114uy; 116uy; 
                          100uy; 0uy; 3uy; 110uy; 105uy; 108uy |]
        test 104uy EmptyArray expected

[<TestFixture>]
type ``Given a boolean`` () =
    [<Test>]
    member x.``when it's true it should return SMALL_TUPLE_EXT for { bert, true }`` () =
        let expected = [| 2uy; 
                          100uy; 0uy; 4uy; 98uy; 101uy; 114uy; 116uy; 
                          100uy; 0uy; 4uy; 116uy; 114uy; 117uy; 101uy |]
        test 104uy (Boolean true) expected

    [<Test>]
    member x.``when it's false it should return SMALL_TUPLE_EXT for { bert, false }`` () =
        let expected = [| 2uy; 
                          100uy; 0uy; 4uy; 98uy; 101uy; 114uy; 116uy; 
                          100uy; 0uy; 5uy; 102uy; 97uy; 108uy; 115uy; 101uy |]
        test 104uy (Boolean false) expected

[<TestFixture>]
type ``Given a dictionary`` () =
    [<Test>]
    member x.``when it's a dictionary it should return SMALL_TUPLE_EXT for { bert, dict, ... }`` () =
        let expected = [| 3uy; 
                          100uy; 0uy; 4uy; 98uy; 101uy; 114uy; 116uy; 
                          100uy; 0uy; 4uy; 100uy; 105uy; 99uy; 116uy; 
                          108uy; 0uy; 0uy; 0uy; 2uy; 
                          104uy; 2uy; 100uy; 0uy; 4uy; 110uy; 97uy; 109uy; 101uy; 109uy; 0uy; 0uy; 0uy; 3uy; 84uy; 111uy; 109uy; 
                          104uy; 2uy; 100uy; 0uy; 3uy; 97uy; 103uy; 101uy; 97uy; 30uy; 106uy |]
        let kvpPairs = [| (Atom "age", Integer 30);
                          (Atom "name", Binary [| 84uy; 111uy; 109uy |])
                       |]
                       |> Map.ofArray
        test 104uy (Dictionary kvpPairs) expected

[<TestFixture>]
type ``Given a time`` () =
    /// Note: you can use http://www.epochconverter.com/ to generate some test dates
    [<Test>]
    member x.``when the datetime is 2012 Aug 12th 18:50:53.534812 then it should return SMALL_TUPLE_EXT { bert, time, 1344, 797453, 534812 }`` () =
        let expected = [| 5uy;
                          100uy; 0uy; 4uy; 98uy; 101uy; 114uy; 116uy;
                          100uy; 0uy; 4uy; 116uy; 105uy; 109uy; 101uy;
                          98uy; 0uy; 0uy; 5uy; 64uy;
                          98uy; 0uy; 12uy; 43uy; 13uy;
                          98uy; 0uy; 8uy; 41uy; 28uy |]
        let date = new DateTime(2012, 8, 12, 18, 50, 53)
        let t = date.AddTicks(5348120L)
        test 104uy (Time t) expected