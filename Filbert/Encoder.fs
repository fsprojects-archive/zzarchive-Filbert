module Filbert.Encoder

open System
open System.IO
open Filbert.Core

// make sure we don't have any arithmetic overflows
open Checked

let getBigEndianBytesInt (n : int) = 
    if BitConverter.IsLittleEndian 
    then BitConverter.GetBytes(n) |> Array.rev 
    else BitConverter.GetBytes(n)

let getBigEndianBytesUint (n : uint32) = 
    if BitConverter.IsLittleEndian 
    then BitConverter.GetBytes(n) |> Array.rev 
    else BitConverter.GetBytes(n)

let getBigEndianBytesUshort (n : uint16) = 
    if BitConverter.IsLittleEndian 
    then BitConverter.GetBytes(n) |> Array.rev 
    else BitConverter.GetBytes(n)

/// Encodes a 8-bit unsigned integer
let encodeSmallInt (n : int) = seq { yield Tags.smallInteger; yield byte n }

/// Encodes a 32-bit signed integer
let encodeInt (n : int) = seq { yield Tags.integer; yield! getBigEndianBytesInt n }

/// Encodes a float
let encodeFloat (f : float) = 
    seq {
        yield Tags.float
        let fStr = f.ToString()
        yield! fStr.Substring(0, min 31 fStr.Length) |> Seq.map byte
    }

/// Encodes an atom
let encodeAtom (str : string) =
    if str.Length > Constants.maxAtomLen then raise <| InvalidAtomLength str.Length
    seq {
        yield Tags.atom
        yield! str.Length |> uint16 |> getBigEndianBytesUshort
        yield! Text.Encoding.ASCII.GetBytes str
    }

/// Encodes a byte list (string)
let encodeByteList (bytes : byte[]) =
    if bytes.Length > Constants.maxStringLength then raise <| InvalidStringLength bytes.Length
    seq {
        yield Tags.string
        yield! bytes.Length |> uint16 |> getBigEndianBytesUshort
        yield! bytes
    }

/// Encodes a binary array
let encodeBinary (bytes : byte[]) =
    if bytes.LongLength > Constants.maxBinaryLength then raise <| InvalidBinaryLength bytes.LongLength
    seq {
        yield Tags.binary

        // use long length to accommodate the uint length value required by the format
        yield! bytes.LongLength |> uint32 |> getBigEndianBytesUint
        yield! bytes
    }

/// Encodes a big integer
let encodeBigInt (n : bigint) =
    seq {
        match n with 
        | _ when n >= 0I && n <= Constants.maxSmallInt
            -> yield! encodeSmallInt(int n)
        | _ when n >= Constants.minInteger && n <= Constants.maxInteger
            -> yield! encodeInt(int n)
        | _ when n >= Constants.minSmallBigInt && n <= Constants.maxSmallBigInt
            -> yield Tags.smallBig
               if n >= 0I then yield 0uy else yield 1uy
               // TODO
        | _ -> yield Tags.largeBig
               if n >= 0I then yield 0uy else yield 1uy
               // TODO
    }

let rec encodeBert bert =
    seq {
        match bert with
        // SMALL_INTEGER (unsigned 8 bit integer)
        | Integer(n) when n <= 255 -> yield! encodeSmallInt n
        // INTEGER (32 bit integer in big-endian format)
        | Integer(n)        -> yield! encodeInt n
        | Float(f)          -> yield! encodeFloat f
        | Atom(str)         -> yield! encodeAtom str
        | Nil               -> yield Tags.nil
        | ByteList(bytes)   -> yield! encodeByteList bytes
        | Binary(bytes)     -> yield! encodeBinary bytes
        | List(berts)       -> yield! encodeList berts
        | Tuple(berts)      -> yield! encodeTuple(berts)     
        | BigInteger(n)     -> yield! encodeBigInt(n)

        // complex types
        | EmptyArray        -> yield! encodeEmptyArray()
        | Boolean(b)        -> yield! encodeBoolean(b)
        | Dictionary(map)   -> yield! encodeDictionary(map)
        | Time(t)           -> yield! encodeTime(t)
    }

/// Encodes a list
and encodeList (berts : Bert[]) =
    if berts.LongLength > Constants.maxListLength then raise <| InvalidListLength berts.LongLength
    seq {
        yield Tags.list

        // use long length to accommodate the uint length value required by the format
        yield! berts.LongLength |> uint32 |> getBigEndianBytesUint
        yield! berts |> Seq.collect encodeBert
    }

/// Encodes a tuple
and encodeTuple (berts : Bert[]) =
    if berts.LongLength > Constants.maxTupleLength then raise <| InvalidTupleLength berts.LongLength
    seq {
        // yield the correct tag and length bytes according to the number of items in the tuple
        if berts.LongLength <= 255L 
        then yield Tags.smallTuple; yield berts.Length |> byte            
        else yield Tags.largeTuple; yield! berts.LongLength |> uint32 |> getBigEndianBytesUint
        
        yield! berts |> Seq.collect encodeBert
    }

/// Encodes an empty array
and encodeEmptyArray =
    let bytes = encodeTuple([| Atom(Constants.bert); Atom(Constants.nil) |]) |> Seq.toArray
    (fun () -> bytes)

/// Encodes a boolean
and encodeBoolean =
    let trueBytes = encodeTuple([| Atom(Constants.bert); Atom(Constants.true') |]) |> Seq.toArray
    let falseBytes = encodeTuple([| Atom(Constants.bert); Atom(Constants.false') |]) |> Seq.toArray
    (fun value -> if value then trueBytes else falseBytes)
    
/// Encodes a dictionary
and encodeDictionary map =
    let kvpPairs = map |> Seq.map (fun kvp -> Tuple([| kvp.Key; kvp.Value |])) |> Seq.toArray
    encodeTuple([| Atom(Constants.bert); Atom(Constants.dict); List(kvpPairs) |])

/// Encodes a time value
and encodeTime t =
    if t < Constants.unixEpoch then raise <| InvalidTime t
    
    // how many ticks (10 millionth of a second = 0.1 microsecond) since zero hour
    let elapsedTicks = (t - Constants.unixEpoch).Ticks

    let ticksPerMegasecond, ticksPerSecond = 10000000000000L, 1000000L
    let mega = elapsedTicks / ticksPerMegasecond |> int
    let seconds = (elapsedTicks % ticksPerMegasecond) / ticksPerSecond |> int
    let micro = elapsedTicks % ticksPerSecond |> int

    encodeTuple([| Atom(Constants.bert); Atom(Constants.time); 
                   Integer(mega); Integer(seconds); Integer(micro) |])