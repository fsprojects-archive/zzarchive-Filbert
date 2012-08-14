module Filbert.Decoder

open System.IO
open Filbert.Core

/// Decodes the stream into a BERT
val decode : Stream -> Bert