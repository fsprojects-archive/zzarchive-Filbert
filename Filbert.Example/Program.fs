
open System.IO
open Filbert.Core
open Filbert.Encoder
open Filbert.Decoder
open Filbert.Rpc

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

    printfn "Making rpc call"
    
    let client = BertRpcClient.Start("localhost", 9997)
    let result = client.Call("nat", "add", List [| Integer 5; Integer 2 |]) |> Async.RunSynchronously

    printfn "Got %O" result

    printfn "Press any key to exit.."
    
    System.Console.ReadKey() |> ignore
    1