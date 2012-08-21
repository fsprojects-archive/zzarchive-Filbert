module Filbert.Encoder

open System.IO
open Filbert.Core

/// Converts a signed 32-bit integer into a byte array
val internal getBigEndianBytesInt    : int -> byte[]

/// Encodes the given BERT into the output stream
val public encode : Bert -> Stream -> unit