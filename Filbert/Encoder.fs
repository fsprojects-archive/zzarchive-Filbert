module Filbert.Encoder

open System
open System.IO
open Filbert.Core

// make sure we don't have any arithmetic overflows
open Checked

/// Converts a value to a byte array in big endian format using
let getBigEndianBytes n (f : 'a -> byte[]) = 
    if BitConverter.IsLittleEndian then f n |> Array.rev else f n

/// Converts a signed 32-bit integer into a byte array
let getBigEndianBytesInt (n : int) = getBigEndianBytes n BitConverter.GetBytes

/// Converts an unsigned 32-bit integer into a byte array
let getBigEndianBytesUint (n : uint32) = getBigEndianBytes n BitConverter.GetBytes

/// Converts an unsigned 16-bit integer into a byte array
let getBigEndianBytesUshort (n : uint16) = getBigEndianBytes n BitConverter.GetBytes

/// Writes one byte to the stream
let writeOneByte byteVal (stream : Stream) = stream.WriteByte byteVal

/// Writes an array of bytes to the stream
let writeBytes (stream : Stream) (byteArr : byte[]) = stream.Write (byteArr, 0, byteArr.Length)    

/// Encodes a 8-bit unsigned integer
let encodeSmallInt (n : int) (stream : Stream) = 
    stream |> writeOneByte Tags.smallInteger
    stream |> writeOneByte (byte n)

/// Encodes a 32-bit signed integer
let encodeInt (n : int) (stream : Stream) = 
    stream |> writeOneByte Tags.integer
    getBigEndianBytesInt n |> writeBytes stream

/// Encodes a float
let encodeFloat (f : float) (stream : Stream) = 
    stream |> writeOneByte Tags.float
    let fStr = f.ToString("e20")
    
    seq {
        let strBytes = Text.Encoding.ASCII.GetBytes fStr
        yield! strBytes

        // provide padding at the end to make sure we get 31 bytes
        for i in 1..(31 - strBytes.Length) do yield 0uy
    }
    |> Seq.take 31
    |> Seq.toArray
    |> writeBytes stream
    
/// Encodes an atom
let encodeAtom (str : string) (stream : Stream) =
    match str.Length with 
    | 0 -> raise <| InvalidAtomLength str.Length
    | n when n > Constants.maxAtomLen -> raise <| InvalidAtomLength str.Length
    | n ->
        stream |> writeOneByte Tags.atom
        str.Length |> uint16 |> getBigEndianBytesUshort |> writeBytes stream
        Text.Encoding.ASCII.GetBytes str |> writeBytes stream

/// Encodes a byte list (string)
let encodeByteList (bytes : byte[]) (stream : Stream) =
    if bytes.Length > Constants.maxStringLength then raise <| InvalidStringLength bytes.Length

    stream |> writeOneByte Tags.string
    bytes.Length |> uint16 |> getBigEndianBytesUshort |> writeBytes stream
    bytes |> writeBytes stream

/// Encodes a binary array
let encodeBinary (bytes : byte[]) (stream : Stream) =
    if bytes.LongLength > Constants.maxBinaryLength then raise <| InvalidBinaryLength bytes.LongLength
    
    stream |> writeOneByte Tags.binary
    
    // use long length to accommodate the uint length value required by the format
    bytes.LongLength |> uint32 |> getBigEndianBytesUint |> writeBytes stream
    bytes |> writeBytes stream

/// Encodes a big integer
let encodeBigInt (n : bigint) (stream : Stream) =
    match n with 
    | _ when n >= 0I && n <= Constants.maxSmallInt
        -> encodeSmallInt (int n) stream
    | _ when n >= Constants.minInteger && n <= Constants.maxInteger
        -> encodeInt (int n) stream
    | n -> let posValue = abs n
           let bytes = posValue.ToByteArray()
           if bytes.LongLength > 255L
           then stream |> writeOneByte Tags.largeBig
                bytes.LongLength |> uint32 |> getBigEndianBytesUint |> writeBytes stream
           else stream |> writeOneByte Tags.smallBig
                stream |> writeOneByte (bytes.Length |> byte)

           match n.Sign with
           | 1 -> stream |> writeOneByte 0uy    // positive
           | -1 -> stream |> writeOneByte 1uy   // negative

           posValue.ToByteArray() |> writeBytes stream

let rec encodeBert (stream : Stream) bert =
    match bert with
    // SMALL_INTEGER (unsigned 8 bit integer)
    | Integer(n) when n >= 0 && n <= 255 -> encodeSmallInt n stream
    // INTEGER (32 bit integer in big-endian format)
    | Integer(n)        -> encodeInt n stream
    | Float(f)          -> encodeFloat f stream
    | Atom(str)         -> encodeAtom str stream
    | Nil               -> stream |> writeOneByte Tags.nil
    | ByteList(bytes)   -> encodeByteList bytes stream
    | Binary(bytes)     -> encodeBinary bytes stream
    | List(berts)       -> encodeList berts stream
    | Tuple(berts)      -> encodeTuple berts stream
    | BigInteger(n)     -> encodeBigInt n stream

    // complex types
    | EmptyArray        -> encodeEmptyArray stream
    | Boolean(b)        -> encodeBoolean b stream
    | Dictionary(map)   -> encodeDictionary map stream
    | Time(t)           -> encodeTime t stream

/// Encodes a list
and encodeList (berts : Bert[]) (stream : Stream) =
    if berts.LongLength > Constants.maxListLength then raise <| InvalidListLength berts.LongLength
    
    stream |> writeOneByte Tags.list

    // use long length to accommodate the uint length value required by the format
    berts.LongLength |> uint32 |> getBigEndianBytesUint |> writeBytes stream
    berts |> Array.iter (encodeBert stream)

    // as per specified by STRING_EXT, end the list with NIL_EXT as tail
    stream |> writeOneByte Tags.nil

/// Encodes a tuple
and encodeTuple (berts : Bert[]) (stream : Stream) =
    if berts.LongLength > Constants.maxTupleLength then raise <| InvalidTupleLength berts.LongLength
    
    // writes the correct tag and length bytes according to the number of items in the tuple
    if berts.LongLength <= 255L 
    then stream |> writeOneByte Tags.smallTuple
         stream |> writeOneByte (berts.Length |> byte)
    else stream |> writeOneByte Tags.largeTuple
         berts.LongLength |> uint32 |> getBigEndianBytesUint |> writeBytes stream
        
    berts |> Array.iter (encodeBert stream)

/// Encodes an empty array
and encodeEmptyArray = encodeTuple [| Atom(Constants.bert); Atom(Constants.nil) |]

/// Encodes a boolean
and encodeBoolean b =
    match b with
    | true -> encodeTuple [| Atom(Constants.bert); Atom(Constants.true') |]
    | _ -> encodeTuple [| Atom(Constants.bert); Atom(Constants.false') |]
    
/// Encodes a dictionary
and encodeDictionary map =
    let kvpPairs = map |> Seq.map (fun kvp -> Tuple([| kvp.Key; kvp.Value |])) |> Seq.toArray
    encodeTuple [| Atom(Constants.bert); Atom(Constants.dict); List(kvpPairs) |]

/// Encodes a time value
and encodeTime t =
    if t < Constants.unixEpoch then raise <| InvalidTime t
    
    // how many ticks (10 millionth of a second = 0.1 microsecond) since zero hour
    let elapsedTicks = (t - Constants.unixEpoch).Ticks

    let ticksPerMegasecond, ticksPerSecond = 10000000000000L, 10000000L
    let mega = elapsedTicks / ticksPerMegasecond |> int
    let seconds = (elapsedTicks % ticksPerMegasecond) / ticksPerSecond |> int
    let micro = elapsedTicks % ticksPerSecond / 10L |> int

    let tuple = [| Atom(Constants.bert); Atom(Constants.time); 
                   Integer(mega); Integer(seconds); Integer(micro) |]

    encodeTuple tuple

let encode bert (stream : Stream) =
    stream.WriteByte Constants.version
    encodeBert stream bert