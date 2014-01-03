module Filbert.Utils

open System.Collections.Concurrent

type BufferPool (size : int, arraySize : int) =
    let factory _ = Array.zeroCreate<byte> arraySize
    let pool      = new ConcurrentBag<byte[]>({ 1..size } |> Seq.map factory)
   
    let get () =
        match pool.TryTake() with
        | true, obj -> obj
        | _ -> factory()

    let put (arr : byte[]) = 
        for i = 0 to arr.Length-1 do arr.[i] <- 0uy
        pool.Add arr

    member this.Size    = pool.Count
    member this.Get ()  = get()
    member this.Put arr = put arr