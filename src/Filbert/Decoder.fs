module Filbert.Decoder

open System
open System.IO
open Filbert.Core
open Filbert.Utils

// make sure we don't have any arithmetic overflows
open Checked

[<AutoOpen>]
module ArraySegmentReaders =
    /// Reads a single byte
    let inline byteReader (seg : ArraySegment<byte>) = seg.Array.[seg.Offset]

    /// Reads some bytes into an array
    let inline byteArrayReader n (seg : ArraySegment<byte>) = 
        let arr = Array.zeroCreate<byte> n
        Buffer.BlockCopy(seg.Array, seg.Offset, arr, 0, n)
        arr

/// Converts a byte array using the specified function using big endian
let inline readBigEndian f arr = 
    // reverse the array in-place if the system is little endian
    // NOTE : to date, all versions of windows are little-endian and the BitConverter.IsLittleEndian
    // is actually harded to return true.. leave this check here for future extensibility for the same
    // reason there's even a BitConverter.IsLittleEndian property
    if BitConverter.IsLittleEndian then Array.Reverse(arr)

    f(arr, 0)

/// Converts a byte array with 4 elements into an int
let bigEndianInteger = (readBigEndian BitConverter.ToInt32)

/// Converts a byte array with 4 elements into an uint32
let bigEndianUinteger = (readBigEndian BitConverter.ToUInt32)

/// Converts a byte array with 2 elements into a short
let rec bigEndianShort = (readBigEndian BitConverter.ToInt16)

/// Converts a byte array with 2 elements into a ushort
let rec bigEndianUshort = (readBigEndian BitConverter.ToUInt16)

/// Converts a byte array into a string
let str = (System.Text.Encoding.ASCII.GetString)

/// Converts a byte value to a bigint
let inline byteToBigInt (b : byte) = bigint (int b)

/// Reads a big integer from n bytes
let inline readBigInt n (ctx : DecoderContext) =
    /// Converts a byte array into a big integer
    let bigInteger (buffer : byte[]) =
        buffer |> Seq.mapi (fun i digit -> (byteToBigInt digit) * (256I ** i)) |> Seq.sum

    let sign = ctx.ReadByte() |> byteReader |> function | 0uy -> 1I | 1uy -> -1I
    ctx.ReadBytes n |> byteArrayReader n |> bigInteger |> (fun n -> n * sign)

/// Converts a set of megasecond, second and microsecond values into a DateTime value
let inline toDateTime (mega : int) (sec :int) (micro : int) =
    Constants.unixEpoch.AddSeconds(1000000.0 * float mega + float sec + float micro / 1000000.0)

[<AutoOpen>]
module Decoders = 
    let decodeSmallInt = byteReader >> int >> Integer
    let decodeInt      = byteArrayReader 4 >> bigEndianInteger >> Integer
    let decodeFloat    = byteArrayReader 31 >> str >> float >> Float
    let decodeUint     = byteArrayReader 4 >> bigEndianUinteger
    let decodeUshort   = byteArrayReader 2 >> bigEndianUshort

/// Reads a complex BERT type
let inline readComplexBert (items : Bert[]) =
    match items with
    | [| Atom(Constants.bert); Atom(Constants.true')  |]
        -> Boolean true
    | [| Atom(Constants.bert); Atom(Constants.false') |]
        -> Boolean false
    | [| Atom(Constants.bert); Atom(Constants.nil)    |]
        -> Nil
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
let rec decodeType (ctx : DecoderContext) : Bert =
    match byteReader <| ctx.ReadByte() with
    // SMALL_INTEGER (unsigned 8 bit integer) 
    | Tags.smallInteger -> ctx.ReadByte() |> decodeSmallInt
    // INTEGER (32 bit integer in big-endian format)
    | Tags.integer      -> ctx.ReadBytes 4 |> decodeInt
    // FLOAT 
    | Tags.float        -> ctx.ReadBytes 31 |> decodeFloat
    // ATOM
    | Tags.atom         -> let len = ctx.ReadBytes 2 |> decodeUshort |> int
                           match len with
                           | n when n > Constants.maxAtomLen -> raise <| InvalidAtomLength len
                           | 0 -> raise <| InvalidAtomLength len
                           | n -> ctx.ReadBytes len |> byteArrayReader len |> str |> Atom
    // SMALL_TUPLE
    | Tags.smallTuple   -> let arity = ctx.ReadByte() |> byteReader |> int
                           match arity with
                           | 0 -> [||] |> Tuple
                           | n -> ctx |> readTuple n
    // LARGE_TUPLE
    | Tags.largeTuple   -> let arity = ctx.ReadBytes 4 |> decodeUint |> int
                           ctx |> readTuple arity
    // NIL
    | Tags.nil          -> EmptyArray
    // STRING (list of bytes)
    | Tags.string       -> let len = ctx.ReadBytes 2 |> decodeUshort |> int
                           if len > Constants.maxStringLength 
                           then raise <| InvalidStringLength len
                           else ctx.ReadBytes len |> byteArrayReader len |> ByteList
    // LIST
    | Tags.list         -> let len = ctx.ReadBytes 4 |> decodeUint |> int
                           let berts = ctx |> readBerts len
                           
                           // as per specified by LIST_EXT, it should end with NIL_EXT for a
                           // proper list, for simplicity sake, for now impropert list is not
                           // going to be supported here
                           match ctx.ReadByte() |> byteReader with
                           | Tags.nil -> List berts
                           | _ -> raise ImpropertyListNotSupported
    // BINARY
    | Tags.binary       -> let len = ctx.ReadBytes 4 |> decodeUint |> int
                           match len with 
                           | 0 -> Binary [||]
                           | n -> ctx.ReadBytes n |> byteArrayReader n |> Binary
    // SMALL_BIG
    | Tags.smallBig     -> let n = ctx.ReadByte() |> byteReader |> int
                           ctx |> readBigInt n |> BigInteger
    // LARGE_BIG
    | Tags.largeBig     -> let n = ctx.ReadBytes 4 |> decodeUint |> int
                           ctx |> readBigInt n |> BigInteger
    | n                 -> raise <| UnsupportTag n

/// Reads a number of BERTs from the stream
and readBerts n (ctx : DecoderContext) =
    let arr = Array.zeroCreate<Bert> n
    for i = 0 to (n-1) do arr.[i] <- decodeType ctx

    arr

/// Reads a number of BERTs from the stream and return a complex BERT type
/// or a Tuple of berts depending on whether the magic 'bert' atom is the
/// first item in the tuple
and readTuple arity (ctx : DecoderContext) =
    let berts = readBerts arity ctx
    match berts.[0] with
    | Atom(Constants.bert) -> readComplexBert(berts)
    | _                    -> Tuple berts

let decode (stream : Stream) =
    let ctx = new DecoderContext(stream)
    match byteReader <| ctx.ReadByte() with
    | Constants.version -> decodeType ctx
    | n   -> raise <| InvalidVersion n