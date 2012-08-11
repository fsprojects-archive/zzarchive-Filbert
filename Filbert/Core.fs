namespace Filbert.Core

open System
open System.Numerics

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
    let version         = 131

    let maxAtomLen      = 255
    let maxStringLength = 65534
    let unixEpoch       = DateTime(1970, 1, 1)

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
    | Dictionary    of (Bert * Bert)[]  // { bert, dict, [{name, <<"Tom">>}, {age, 30}]}
    // Time args : megaseconds (millions of seconds) * seconds * microseconds (millionth of a second)
    | Time          of DateTime  // { bert, time, 1255, 295581, 446228 }