namespace Filbert.Core

open System
open System.Collections.Generic
open System.Numerics

[<RequireQualifiedAccess>]
module Tags = 
    [<Literal>] 
    let smallInteger  = 97uy
    [<Literal>] 
    let integer       = 98uy
    [<Literal>] 
    let float         = 99uy
    [<Literal>] 
    let atom          = 100uy
    [<Literal>] 
    let smallTuple    = 104uy
    [<Literal>] 
    let largeTuple    = 105uy
    [<Literal>] 
    let nil           = 106uy
    [<Literal>] 
    let string        = 107uy
    [<Literal>] 
    let list          = 108uy
    [<Literal>] 
    let binary        = 109uy
    [<Literal>] 
    let smallBig      = 110uy
    [<Literal>] 
    let largeBig      = 111uy

[<RequireQualifiedAccess>]
module Constants =
    [<Literal>] 
    let bert            = "bert"
    [<Literal>] 
    let true'           = "true"
    [<Literal>] 
    let false'          = "false"
    [<Literal>]
    let nil             = "nil"
    [<Literal>]
    let dict            = "dict"
    [<Literal>]
    let time            = "time"
    [<Literal>]
    let version         = 131uy

    let maxSmallInt     = 255I          // max unsigned 8 bit int
    let maxInteger      = 2147483647I   // max signed 32 bit int
    let minInteger      = -2147483648I  // min signed 32 bit int    
    let maxAtomLen      = 255
    let maxStringLength = 65534
    let maxBinaryLength = 4294967295L   // max uint32
    let maxListLength   = 4294967295L   // max uint32
    let maxTupleLength  = 4294967295L   // max uint32
    let unixEpoch       = DateTime(1970, 1, 1)

// The different types you can have in BERT (see spec @ http://bert-rpc.org/)
type Bert =
    // primitive types
    | Integer       of int      // 4
    | BigInteger    of bigint   // 12345678901234567890123456789
    | Float         of float    // 8.1516
    | Atom          of string   // foo
    | Tuple         of Bert[]   // { coord, 23, 42 }
    | EmptyArray                // this is the primitive Nil, which in Erlang equals to []
    | ByteList      of byte[]   // [ 1, 2, 3 ]
    | List          of Bert[]   // [ a, [ 1, 2 ] ]
    | Binary        of byte[]   // <<"Roses are red\0Violets are blue">>

    // complex types
    | Nil                       // { bert, nil }
    | Boolean       of bool     // { bert, true } or { bert, false }
    // Dictionary args : a map where both key and value can be any BERT term
    | Dictionary    of Map<Bert, Bert>  // { bert, dict, [{name, <<"Tom">>}, {age, 30}]}
    // Time args : megaseconds (millions of seconds) * seconds * microseconds (millionth of a second)
    | Time          of DateTime         // { bert, time, 1255, 295581, 446228 }

    /// Creates a new Dictionary
    static member FromDict dict = 
        (dict :> seq<_>) |> Seq.map (| KeyValue |) |> Map.ofSeq |> Dictionary

    override this.ToString() =
        match this with
        | Integer(n)        -> string n
        | BigInteger(bi)    -> string bi
        | Float(f)          -> string f
        | Atom(str)         -> str
        | Tuple(arr)        -> sprintf "{ %s }" (arr |> Seq.map string |> Seq.reduce (fun acc elem -> acc + ", " + elem))
        | List(arr)         -> sprintf "[ %s ]" (arr |> Seq.map string |> Seq.reduce (fun acc elem -> acc + ", " + elem))
        | EmptyArray        -> "[]"
        | ByteList(bytes)
        | Binary(bytes)     -> sprintf "<<\"%s\">>" (System.Text.Encoding.ASCII.GetString bytes)
        | Nil               -> "nil"
        | Boolean(b)        -> string b
        | Dictionary(map)   -> "" // TODO
        | Time(dt)          -> string dt

[<AutoOpen>]
module Exceptions =    
    // InsufficientNumberOfBytes args : required number of bytes, number of bytes read
    exception InsufficientNumberOfBytes of int * int
    exception EndOfStreamReached
    exception UnsupportTag              of byte
    exception InvalidVersion            of byte
    exception InvalidAtomLength         of int
    exception InvalidStringLength       of int
    exception InvalidBinaryLength       of int64
    exception InvalidListLength         of int64
    exception InvalidTupleLength        of int64
    exception UnsupportedComplexBert    of Bert[]
    exception InvalidKeyValueTuple      of Bert
    exception InvalidTime               of DateTime
    exception ImpropertyListNotSupported