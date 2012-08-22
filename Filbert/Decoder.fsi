module Filbert.Decoder

open System.IO
open Filbert.Core

/// Converts a byte array with 4 elements into an int
val internal bigEndianInteger       : byte[] -> int

/// Decodes the stream into a BERT
val public decode : Stream -> Bert