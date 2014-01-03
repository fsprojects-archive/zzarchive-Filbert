module Filbert.Decoder

open System.IO
open Filbert.Core
open Filbert.Utils

/// Decodes the stream into a BERT
val public decode : Stream -> Bert