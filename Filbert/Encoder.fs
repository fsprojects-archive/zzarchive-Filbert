module Filbert.Encoder

open System
open System.IO
open Filbert.Core
open Checked

let getBigEndianBytesint (n : int) = 
    if BitConverter.IsLittleEndian 
    then BitConverter.GetBytes(n) |> Array.rev 
    else BitConverter.GetBytes(n)

let getBigEndianBytesUshort (n : uint16) = 
    if BitConverter.IsLittleEndian 
    then BitConverter.GetBytes(n) |> Array.rev 
    else BitConverter.GetBytes(n)

let encode bert =
    seq {
        match bert with
        | Integer(n) when n <= 255 
            -> yield Tags.smallInteger; yield byte n
        | Integer(n) 
            -> yield Tags.integer; yield! getBigEndianBytesInt n
        | Float(f)
            -> yield Tags.float
               let fStr = f.ToString()
               yield! fStr.Substring(0, min 31 fStr.Length) |> Seq.map byte
        | Atom(str)
            -> yield Tags.atom
               yield! str.Length |> uint16 |> getBigEndianBytesUshort
               yield! Text.Encoding.ASCII.GetBytes str
        | _
            -> failwith "TODO"        
    }