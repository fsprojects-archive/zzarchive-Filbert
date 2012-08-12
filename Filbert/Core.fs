namespace Filbert.Core

open System
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

    // the maximum value that can go into a SMALL_BIG_EXT
    let maxSmallBigInt  = [| 0..255 |] |> Array.sumBy (fun i -> 9I * (256I ** i))
    // the minimum value that can go into a SMALL_BIG_EXT
    let minSmallBigInt  = -1I * maxSmallBigInt

// The different types you can have in BERT (see spec @ http://bert-rpc.org/)
type Bert =
    // primitive types
    | Integer       of int      // 4
    | BigInteger    of bigint   // 12345678901234567890123456789
    | Float         of float    // 8.1516
    | Atom          of string   // foo
    | Tuple         of Bert[]   // { coord, 23, 42 }
    | Nil                       // this is the primitive Nil    
    | ByteList      of byte[]   // [ 1, 2, 3 ]
    | List          of Bert[]   // [ a, [ 1, 2 ] ]
    | Binary        of byte[]   // <<"Roses are red\0Violets are blue">>

    // complex types
    | EmptyArray                // { bert, nil }
    | Boolean       of bool     // { bert, true } or { bert, false }
    // Dictionary args : array of key * value tuple, key and value can both be any BERT term
    | Dictionary    of Map<Bert, Bert>  // { bert, dict, [{name, <<"Tom">>}, {age, 30}]}
    // Time args : megaseconds (millions of seconds) * seconds * microseconds (millionth of a second)
    | Time          of DateTime  // { bert, time, 1255, 295581, 446228 }

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