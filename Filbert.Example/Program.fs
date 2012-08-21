
open System.IO
open Filbert.Core
open Filbert.Encoder
open Filbert.Decoder
open Filbert.RpcClient

[<EntryPoint>]
let main args =
    let mysterWord = [| 131uy; 107uy; 0uy; 8uy; 104uy; 97uy; 122uy; 101uy; 108uy; 110uy; 117uy; 116uy |]
    let bert = Dictionary(Map.ofList [(Atom "Filbert", Atom "means"); 
                                      (ByteList mysterWord, Atom "!")])
    
    // encode this BERT
    use memStream = new MemoryStream()
    encode bert memStream

    printfn "%d bytes encoded..." memStream.Length

    // now decode it
    memStream.Position <- 0L
    let bert' = decode memStream

    match bert = bert' with
    | true  -> printfn "decoded successfully, they're a match!"
    | false -> printfn "back to work YC *cracks whip*"

    call("nat", "add", List [| Integer 1; Integer 2|])

    printfn "Press any key to exit.."
    
    System.Console.ReadKey() |> ignore
    1