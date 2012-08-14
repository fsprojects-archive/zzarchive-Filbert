module Filbert.Encoder

open System.IO
open Filbert.Core

/// Encodes the given BERT into the output stream
val encode : Bert -> Stream -> unit