module Filbert.Decoder

open System
open System.IO
open Filbert.Core

// InsufficientNumberOfBytes args : required number of bytes, number of bytes read
exception InsufficientNumberOfBytes of int * int
exception EndOfStreamReached
exception UnsupportTag              of byte
exception InvalidVersion            of int
exception InvalidAtomLength         of int
exception InvalidStringLength       of int
exception UnsupportedComplexBert    of Bert[]
exception InvalidKeyValueTuple      of Bert

let readLen f arr = if BitConverter.IsLittleEndian then f(arr |> Array.rev, 0) else f(arr, 0)

/// Converts a byte array with 4 elements into an int
let bigEndianInteger arr = arr |> readLen BitConverter.ToInt32

/// Converts a byte array with 4 elements into an uint32
let bigEndianUinteger arr = arr |> readLen BitConverter.ToUInt32

/// Converts a byte array with 2 elements into a short
let rec bigEndianShort arr = arr |> readLen BitConverter.ToInt16

/// Converts a byte array with 2 elements into a ushort
let rec bigEndianUshort arr = arr |> readLen BitConverter.ToUInt16

/// Converts a byte array into a string
let str = System.Text.Encoding.ASCII.GetString

/// Converts a byte value to a bigint
let byteToBigint (b : byte) = bigint (int b)

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
    /// Converts a byte array into a big integer
    let bigInteger (arr : byte[]) = 
        arr |> Array.mapi (fun i digit -> (byteToBigint digit) * (256I ** i)) |> Array.sum

    let sign = stream |> readByte |> function | 0uy -> 1I | 1uy -> -1I
    stream |> readBytes n |> bigInteger |> (fun n -> n * sign)

/// Converts a set of megasecond, second and microsecond values into a DateTime value
let toDateTime (mega : int) (sec :int) (micro : int) =
    Constants.unixEpoch.AddSeconds(1000000.0 * float mega + float sec + float micro / 1000000.0)

/// Reads a complex BERT type
let readComplexBert (items : Bert[]) =
    match items with
    | [| Atom(Constants.bert); Atom(Constants.true')  |]
        -> Boolean true
    | [| Atom(Constants.bert); Atom(Constants.false') |]
        -> Boolean false
    | [| Atom(Constants.bert); Atom(Constants.nil)    |]
        -> EmptyArray
    | [| Atom(Constants.bert); Atom(Constants.time); Integer(mega); Integer(sec); Integer(micro) |]
        -> toDateTime mega sec micro |> Time
    | [| Atom(Constants.bert); Atom(Constants.dict); List(arr) |]
        -> arr
           |> Array.map (fun kvp -> match kvp with
                                    | Tuple([| key; value |]) -> key, value
                                    | _ -> raise <| InvalidKeyValueTuple kvp)
           |> Map.ofArray
           |> Dictionary
    | _ -> raise <| UnsupportedComplexBert(items)

/// Parses the Erlang External Term Format 
/// see http://erlang.org/doc/apps/erts/erl_ext_dist.html
let rec decodeType (stream : Stream) : Bert =
    match readByte stream with
    // SMALL_INTEGER (unsigned 8 bit integer) 
    | Tags.smallInteger -> stream |> readByte |> int |> Integer
    // INTEGER (32 bit integer in big-endian format)
    | Tags.integer      -> stream |> readBytes 4 |> bigEndianInteger |> Integer
    // FLOAT 
    | Tags.float        -> stream |> readBytes 31 |> str |> float |> Float
    // ATOM
    | Tags.atom         -> let len = stream |> readBytes 2 |> bigEndianUshort |> int
                           match len with
                           | n when n > Constants.maxAtomLen -> raise <| InvalidAtomLength len
                           | 0 -> raise <| InvalidAtomLength len
                           | n -> stream |> readBytes n |> str |> Atom
    // SMALL_TUPLE
    | Tags.smallTuple   -> let arity = stream |> readByte
                           match arity with
                           | 0uy -> [||] |> Tuple
                           | n -> stream |> readTuple (int64 n)
    // LARGE_TUPLE
    | Tags.largeTuple   -> let arity = stream |> readBytes 4 |> bigEndianUinteger |> int64
                           stream |> readTuple arity
    // NIL
    | Tags.nil          -> Nil
    // STRING (list of bytes)
    | Tags.string       -> let len = stream |> readBytes 2 |> bigEndianUshort |> int
                           if len > Constants.maxStringLength 
                           then raise <| InvalidStringLength len
                           else stream |> readBytes len |> ByteList
    // LIST
    | Tags.list         -> let len = stream |> readBytes 4 |> bigEndianUinteger |> int64
                           stream |> readBerts len |> List
    // BINARY
    | Tags.binary       -> let len = stream |> readBytes 4 |> bigEndianUinteger |> int
                           match len with 
                           | 0 -> Binary [||]
                           | n -> stream |> readBytes n |> Binary
    // SMALL_BIG
    | Tags.smallBig     -> let n = stream |> readByte |> int
                           stream |> readBigInt n |> BigInteger
    // LARGE_BIG
    | Tags.largeBig     -> let n = stream |> readBytes 4 |> bigEndianUinteger |> int
                           stream |> readBigInt n |> BigInteger
    | n                 -> raise <| UnsupportTag n
and readBerts n (stream : Stream) =
    seq { 1L..n } |> Seq.map (fun _ -> decodeType stream) |> Seq.toArray
and readTuple arity (stream : Stream) =
    let berts = readBerts arity stream
    match berts.[0] with
    | Atom(Constants.bert) -> readComplexBert(berts)
    | _                    -> Tuple berts

let decode (stream : Stream) =
    match stream |> readByte |> int with
    | Constants.version -> decodeType stream
    | n   -> raise <| InvalidVersion n