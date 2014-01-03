module Filbert.Decoder

open System
open System.IO
open Filbert.Core
open Filbert.Utils

// make sure we don't have any arithmetic overflows
open Checked

let ``1 byte pool``   = new BufferPool(8 * 1024, 1)
let ``2 bytes pool``  = new BufferPool(24 * 1024, 2)
let ``4 bytes pool``  = new BufferPool(24 * 1024, 4)
let ``31 bytes pool`` = new BufferPool(1024, 31)

/// Returns the buffer to its pool (if necessary)
let returnBuffer buffer (pool : BufferPool) = pool.Put buffer

/// Uses the buffer to do some work and then return it back to the pool
let useThenReturnBuffer f (buffer, pool) = 
    let x = f buffer
    returnBuffer buffer pool
    x

/// Converts a byte array using the specified function using big endian
let readBigEndian f arr = 
    if BitConverter.IsLittleEndian then f(arr |> Array.rev, 0) else f(arr, 0)

/// Converts a byte array with 4 elements into an int
let bigEndianInteger = useThenReturnBuffer (readBigEndian BitConverter.ToInt32)

/// Converts a byte array with 4 elements into an uint32
let bigEndianUinteger = useThenReturnBuffer (readBigEndian BitConverter.ToUInt32)

/// Converts a byte array with 2 elements into a short
let rec bigEndianShort = useThenReturnBuffer (readBigEndian BitConverter.ToInt16)

/// Converts a byte array with 2 elements into a ushort
let rec bigEndianUshort = useThenReturnBuffer (readBigEndian BitConverter.ToUInt16)

/// Converts a byte array into a string
let str = useThenReturnBuffer (System.Text.Encoding.ASCII.GetString)

/// Converts a byte value to a bigint
let byteToBigInt (b : byte) = bigint (int b)

/// Reads a number of bytes from the stream into a buffer array from one of the pools
let readBytes n (stream : Stream) = 
    let buffer, pool = match n with 
                       | 1  -> ``1 byte pool``.Get(),   ``1 byte pool``
                       | 2  -> ``2 bytes pool``.Get(),  ``2 bytes pool``
                       | 4  -> ``4 bytes pool``.Get(),  ``4 bytes pool``
                       | 31 -> ``31 bytes pool``.Get(), ``31 bytes pool``

    match stream.Read(buffer, 0, n) with
    | 0 -> 
        returnBuffer buffer pool
        raise EndOfStreamReached
    | n' when n' < n -> 
        returnBuffer buffer pool
        raise <| InsufficientNumberOfBytes(n, n')
    | _ -> buffer, pool

/// Reads a number of bytes without using pooled buffers
let readBytesWithoutBufferPool n (stream : Stream) =
    let buffer = Array.zeroCreate<byte> n
    match stream.Read(buffer, 0, n) with
    | 0 -> raise EndOfStreamReached
    | n' when n' < n -> raise <| InsufficientNumberOfBytes(n, n')
    | _ -> buffer

/// Reads one byte from the stream
let readByte (stream : Stream) = 
    match stream.ReadByte () with
    | -1 -> raise EndOfStreamReached
    | x  -> x

/// Reads a big integer from n bytes from the stream
let readBigInt n (stream : Stream) =
    /// Converts a byte array into a big integer
    let bigInteger (buffer : byte[]) =
        buffer |> Seq.mapi (fun i digit -> (byteToBigInt digit) * (256I ** i)) |> Seq.sum

    let sign = stream |> readByte |> function | 0 -> 1I | 1 -> -1I
    stream |> readBytesWithoutBufferPool n |> bigInteger |> (fun n -> n * sign)

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
let rec decodeType (stream : Stream) : Bert =
    match readByte stream |> byte with
    // SMALL_INTEGER (unsigned 8 bit integer) 
    | Tags.smallInteger -> stream |> readByte |> Integer
    // INTEGER (32 bit integer in big-endian format)
    | Tags.integer      -> stream |> readBytes 4 |> bigEndianInteger |> Integer
    // FLOAT 
    | Tags.float        -> stream |> readBytes 31 |> str |> float |> Float
    // ATOM
    | Tags.atom         -> let len = stream |> readBytes 2 |> bigEndianUshort |> int
                           match len with
                           | n when n > Constants.maxAtomLen -> raise <| InvalidAtomLength len
                           | 0 -> raise <| InvalidAtomLength len
                           | n -> stream |> readBytesWithoutBufferPool n |> System.Text.Encoding.ASCII.GetString |> Atom
    // SMALL_TUPLE
    | Tags.smallTuple   -> let arity = stream |> readByte
                           match arity with
                           | 0 -> [||] |> Tuple
                           | n -> stream |> readTuple (int64 n)
    // LARGE_TUPLE
    | Tags.largeTuple   -> let arity = stream |> readBytes 4 |> bigEndianUinteger |> int64
                           stream |> readTuple arity
    // NIL
    | Tags.nil          -> EmptyArray
    // STRING (list of bytes)
    | Tags.string       -> let len = stream |> readBytes 2 |> bigEndianUshort |> int
                           if len > Constants.maxStringLength 
                           then raise <| InvalidStringLength len
                           else stream |> readBytesWithoutBufferPool len |> ByteList
    // LIST
    | Tags.list         -> let len = stream |> readBytes 4 |> bigEndianUinteger |> int64
                           let berts = stream |> readBerts len
                           
                           // as per specified by LIST_EXT, it should end with NIL_EXT for a
                           // proper list, for simplicity sake, for now impropert list is not
                           // going to be supported here
                           match stream |> readByte |> byte with
                           | Tags.nil -> List berts
                           | _ -> raise ImpropertyListNotSupported
    // BINARY
    | Tags.binary       -> let len = stream |> readBytes 4 |> bigEndianUinteger |> int
                           match len with 
                           | 0 -> Binary [||]
                           | n -> stream |> readBytesWithoutBufferPool n |> Binary
    // SMALL_BIG
    | Tags.smallBig     -> let n = stream |> readByte |> int
                           stream |> readBigInt n |> BigInteger
    // LARGE_BIG
    | Tags.largeBig     -> let n = stream |> readBytes 4 |> bigEndianUinteger |> int
                           stream |> readBigInt n |> BigInteger
    | n                 -> raise <| UnsupportTag n

/// Reads a number of BERTs from the stream
and readBerts n (stream : Stream) =
    seq { 1L..n } |> Seq.map (fun _ -> decodeType stream) |> Seq.toArray

/// Reads a number of BERTs from the stream and return a complex BERT type
/// or a Tuple of berts depending on whether the magic 'bert' atom is the
/// first item in the tuple
and readTuple arity (stream : Stream) =
    let berts = readBerts arity stream
    match berts.[0] with
    | Atom(Constants.bert) -> readComplexBert(berts)
    | _                    -> Tuple berts

let decode (stream : Stream) =
    match stream |> readByte |> byte with
    | Constants.version -> decodeType stream
    | n   -> raise <| InvalidVersion n