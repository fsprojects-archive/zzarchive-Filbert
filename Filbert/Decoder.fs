module Filbert.Decoder

open System
open System.IO

// InsufficientNumberOfBytes args : required number of bytes, number of bytes read
exception InsufficientNumberOfBytes of int * int
exception EndOfStreamReached
exception UnsupportTag              of int
exception InvalidVersion            of int
exception InvalidAtomLength         of int

/// Converts a byte array with 4 elements into an int
let bigEndianInteger arr =
    if BitConverter.IsLittleEndian 
    then BitConverter.ToInt32(arr |> Array.rev, 0)
    else BitConverter.ToInt32(arr, 0)

/// Converts a byte array with 2 elements into a short
let rec bigEndianShort arr =
    if BitConverter.IsLittleEndian
    then BitConverter.ToInt16(arr |> Array.rev, 0)
    else BitConverter.ToInt16(arr, 0)

/// Converts a byte array into a string
let str = System.Text.Encoding.ASCII.GetString

let bigint (b : byte) = bigint (int b)

/// Converts a byte array into a big integer
let bigInteger (arr : byte[]) = 
    arr |> Array.mapi (fun i digit -> (bigint digit) * (256I ** i)) |> Array.sum

/// Reads a number of bytes from the stream
let readBytes n (stream : Stream) = 
    let buffer = Array.zeroCreate<byte> n
    match stream.Read(buffer, 0, n) with
    | 0 -> raise EndOfStreamReached
    | n' when n' < n -> raise <| InsufficientNumberOfBytes(n, n')
    | _ -> buffer

/// Reads one byte from the stream
let readByte (stream : Stream) = (readBytes 1 stream).[0]

/// Reads a big integer from n bytes from the stream
let readBigInt n (stream : Stream) =
    let sign = stream |> readByte |> function | 0uy -> 1I | 1uy -> -1I
    stream |> readBytes n |> bigInteger |> (fun n -> n * sign)

/// Parses the Erlang External Term Format 
/// see http://erlang.org/doc/apps/erts/erl_ext_dist.html
let rec decodeType (stream : Stream) : obj =
    match readByte stream |> int with
    // SMALL_INTEGER (unsigned 8 bit integer) 
    | 97  -> stream |> readByte |> int :> obj
    // INTEGER (32 bit integer in big-endian format)
    | 98  -> stream |> readBytes 4 |> bigEndianInteger :> obj
    // FLOAT 
    | 99  -> stream |> readBytes 31 |> str |> float :> obj
    // ATOM
    | 100 -> let len = stream |> readBytes 2 |> bigEndianShort |> int
             match len with
             | n when n > 255 -> raise <| InvalidAtomLength len
             | 0 -> raise <| InvalidAtomLength len
             | n -> stream |> readBytes n |> str :> obj
    // SMALL_TUPLE
    | 104 -> let arity = stream |> readByte |> int
             match arity with
             | 0 -> [||] :> obj
             | n -> stream |> readTuple n :> obj
    // LARGE_TUPLE
    | 105 -> let arity = stream |> readBytes 4 |> bigEndianInteger
             stream |> readTuple arity :> obj
    // NIL
    | 106 -> None :> obj
    // STRING
    | 107 -> let len = stream |> readBytes 2 |> bigEndianShort |> int
             stream |> readBytes len |> str :> obj
    // LIST
    | 108 -> let len = stream |> readBytes 4 |> bigEndianInteger
             // TODO
             raise <| System.NotImplementedException()
    // BINARY
    | 109 -> let len = stream |> readBytes 4 |> bigEndianInteger
             stream |> readBytes len :> obj
    // SMALL_BIG
    | 110 -> let n = stream |> readByte |> int
             stream |> readBigInt n :> obj
    // LARGE_BIG
    | 111 -> let n = stream |> readBytes 4 |> bigEndianInteger
             stream |> readBigInt n :> obj
    | n -> raise <| UnsupportTag n
and readTuple arity (stream : Stream) =
    [| 1..arity |] |> Array.map (fun _ -> decodeType stream)

let decode (stream : Stream) =
    match stream |> readByte |> int with
    | 131 -> decodeType stream
    | n   -> raise <| InvalidVersion n