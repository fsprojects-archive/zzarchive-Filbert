module Filbert.Encoder

open System
open System.IO
open Filbert.Core
open Filbert.Utils

// make sure we don't have any arithmetic overflows
open Checked

/// Converts a value to a byte array in big endian format using
let getBigEndianBytes n (f : 'a -> byte[]) = 
    let bytes = f n
    if BitConverter.IsLittleEndian then Array.Reverse bytes
    bytes

/// Converts a signed 32-bit integer into a byte array
let internal getBigEndianBytesInt (n : int) = getBigEndianBytes n BitConverter.GetBytes

/// Converts an unsigned 32-bit integer into a byte array
let getBigEndianBytesUint (n : uint32) = getBigEndianBytes n BitConverter.GetBytes

/// Converts an unsigned 16-bit integer into a byte array
let getBigEndianBytesUshort (n : uint16) = getBigEndianBytes n BitConverter.GetBytes

/// Writes one byte to the stream
//let writeOneByte byteVal (stream : Stream) = stream.WriteByte byteVal
//
///// Writes an array of bytes to the stream
//let writeBytes (stream : Stream) (byteArr : byte[]) = stream.Write (byteArr, 0, byteArr.Length)    

/// Encodes a 8-bit unsigned integer
let encodeSmallInt (n : int) (ctx : EncoderContext) = 
    ctx.WriteByte Tags.smallInteger
    ctx.WriteByte (byte n)

/// Encodes a 32-bit signed integer
let encodeInt (n : int) (ctx : EncoderContext) = 
    ctx.WriteByte Tags.integer
    getBigEndianBytesInt n |> ctx.WriteBytes

/// Encodes a float
let encodeFloat (f : float) (ctx : EncoderContext) = 
    ctx.WriteByte Tags.float
    let fStr = f.ToString("e20")
    
    seq {
        let strBytes = Text.Encoding.ASCII.GetBytes fStr
        yield! strBytes

        // provide padding at the end to make sure we get 31 bytes
        for i in 1..(31 - strBytes.Length) do yield 0uy
    }
    |> Seq.take 31
    |> Seq.toArray
    |> ctx.WriteBytes
    
/// Encodes an atom
let encodeAtom (str : string) (ctx : EncoderContext) =
    match str.Length with 
    | 0 -> raise <| InvalidAtomLength str.Length
    | n when n > Constants.maxAtomLen -> raise <| InvalidAtomLength str.Length
    | n ->
        ctx.WriteByte Tags.atom
        str.Length |> uint16 |> getBigEndianBytesUshort |> ctx.WriteBytes
        str |> Text.Encoding.ASCII.GetBytes |> ctx.WriteBytes

/// Encodes a byte list (string)
let encodeByteList (bytes : byte[]) (ctx : EncoderContext) =
    if bytes.Length > Constants.maxStringLength then raise <| InvalidStringLength bytes.Length

    ctx.WriteByte Tags.string
    bytes.Length |> uint16 |> getBigEndianBytesUshort |> ctx.WriteBytes
    bytes |> ctx.WriteBytes

/// Encodes a binary array
let encodeBinary (bytes : byte[]) (ctx : EncoderContext) =
    if bytes.LongLength > Constants.maxBinaryLength then raise <| InvalidBinaryLength bytes.LongLength
    
    ctx.WriteByte Tags.binary
    
    // use long length to accommodate the uint length value required by the format
    bytes.LongLength |> uint32 |> getBigEndianBytesUint |> ctx.WriteBytes
    bytes |> ctx.WriteBytes

/// Encodes a big integer
let encodeBigInt (n : bigint) (ctx : EncoderContext) =
    match n with 
    | _ when n >= 0I && n <= Constants.maxSmallInt
        -> encodeSmallInt (int n) ctx
    | _ when n >= Constants.minInteger && n <= Constants.maxInteger
        -> encodeInt (int n) ctx
    | n -> let posValue = abs n

           // trim trailing 0 bytes
           let bytes = match posValue.ToByteArray() with 
                       | bytes when bytes.[bytes.Length - 1] = 0uy
                            -> bytes.[0..bytes.Length - 2]
                       | bytes -> bytes           

           if bytes.LongLength > 255L
           then ctx.WriteByte Tags.largeBig
                bytes.LongLength |> uint32 |> getBigEndianBytesUint |> ctx.WriteBytes
           else ctx.WriteByte Tags.smallBig
                ctx.WriteByte (bytes.Length |> byte)

           match n.Sign with
           | 1  -> ctx.WriteByte 0uy   // positive
           | -1 -> ctx.WriteByte 1uy   // negative

           bytes |> ctx.WriteBytes

let rec encodeBert (ctx : EncoderContext) bert =
    match bert with
    // SMALL_INTEGER (unsigned 8 bit integer)
    | Integer(n) when n >= 0 && n <= 255 -> encodeSmallInt n ctx
    // INTEGER (32 bit integer in big-endian format)
    | Integer(n)        -> encodeInt n ctx
    | Float(f)          -> encodeFloat f ctx
    | Atom(str)         -> encodeAtom str ctx
    | Nil               -> encodeNil ctx
    | ByteList(bytes)   -> encodeByteList bytes ctx
    | Binary(bytes)     -> encodeBinary bytes ctx
    | List(berts)       -> encodeList berts ctx
    | Tuple(berts)      -> encodeTuple berts ctx
    | BigInteger(n)     -> encodeBigInt n ctx

    // complex types
    | EmptyArray        -> ctx.WriteByte Tags.nil
    | Boolean(b)        -> encodeBoolean b ctx
    | Dictionary(map)   -> encodeDictionary map ctx
    | Time(t)           -> encodeTime t ctx

/// Encodes a list
and encodeList (berts : Bert[]) (ctx : EncoderContext) =
    if berts.LongLength > Constants.maxListLength then raise <| InvalidListLength berts.LongLength
    
    ctx.WriteByte Tags.list

    // use long length to accommodate the uint length value required by the format
    berts.LongLength |> uint32 |> getBigEndianBytesUint |> ctx.WriteBytes
    berts |> Array.iter (encodeBert ctx)

    // as per specified by STRING_EXT, end the list with NIL_EXT as tail
    ctx.WriteByte Tags.nil

/// Encodes a tuple
and encodeTuple (berts : Bert[]) (ctx : EncoderContext) =
    if berts.LongLength > Constants.maxTupleLength then raise <| InvalidTupleLength berts.LongLength
    
    // writes the correct tag and length bytes according to the number of items in the tuple
    if berts.LongLength <= 255L 
    then ctx.WriteByte Tags.smallTuple
         ctx.WriteByte (berts.Length |> byte)
    else ctx.WriteByte Tags.largeTuple
         berts.LongLength |> uint32 |> getBigEndianBytesUint |> ctx.WriteBytes
        
    berts |> Array.iter (encodeBert ctx)

/// Encodes a nil
and encodeNil = encodeTuple [| Atom(Constants.bert); Atom(Constants.nil) |]

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
    use ctx = new EncoderContext(stream)
    ctx.WriteByte Constants.version
    encodeBert ctx bert