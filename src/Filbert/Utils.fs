module Filbert.Utils

open System
open System.Collections.Concurrent
open System.IO
open System.Threading

open Filbert.Core

type internal BufferPool (size : int, arraySize : int) =
    let factory _ = Array.zeroCreate<byte> arraySize
    let pool      = new ConcurrentBag<byte[]>({ 1..size } |> Seq.map factory)
   
    let get () = 
        match pool.TryTake() with
        | true, arr -> arr
        | _ -> factory()

    let put (arr : byte[]) =
        Array.Clear(arr, 0, arraySize)
        pool.Add arr

    member this.Size    = pool.Count
    member this.Get ()  = get()
    member this.Put arr = put arr

type internal DecoderContext (stream : Stream) =
    static let maxBufferSize = 1024
    static let bufferPool    = new BufferPool(128, maxBufferSize)

    let mutable buffer     = bufferPool.Get()
    let mutable bufferSize = stream.Read(buffer, 0, maxBufferSize)
    let mutable index      = 0

    let initBuffer () =
        buffer     <- bufferPool.Get()
        bufferSize <- stream.Read(buffer, 0, maxBufferSize)
        index      <- 0

    let swapBuffer () =
        let buffer' = bufferPool.Get()

        // block copy the last bytes from the current buffer to the new buffer
        let remCount = bufferSize - index
        if remCount > 0 then Buffer.BlockCopy(buffer, index, buffer', 0, remCount)

        // now we can return the current buffer to the pool and switch to the new buffer
        bufferPool.Put buffer
        buffer <- buffer'
            
        // read from the stream again to fill up the rest of the buffer
        bufferSize <- remCount + stream.Read(buffer, remCount, maxBufferSize - remCount)

        index <- 0 // reset the index now that we've swapped out the buffer

    // read a large block of data from the stream (when n > max buffer size)
    let readLargeArray n =
        // create a byte array large enough for the specified number of bytes
        let arr = Array.zeroCreate<byte> n

        // block copy the last bytes from the current buffer to the new array
        let remCount = bufferSize - index
        if remCount > 0 then Buffer.BlockCopy(buffer, index, arr, 0, remCount)
        
        // read from the stream to get the rest of the data for the output array
        let count = stream.Read(arr, remCount, n - remCount)
        let totalCount = remCount + count
        if totalCount < n then raise <| InsufficientNumberOfBytes(n, totalCount)

        // reinitialize our buffer
        bufferPool.Put buffer
        initBuffer()

        arr

    let rec readBytes n =
        if n > maxBufferSize then readLargeArray n
        else 
            let newIndex = index + n
            if newIndex <= bufferSize then
                try
                    let arr = Array.zeroCreate<byte> n
                    Buffer.BlockCopy(buffer, index, arr, 0, n)
                    arr
                finally
                    index <- newIndex
            else
                swapBuffer()
                if n > bufferSize then raise <| InsufficientNumberOfBytes(n, bufferSize)
                readBytes n

    let readByte () =
        if bufferSize = 0 then raise <| InsufficientNumberOfBytes(1, bufferSize)
        if index = bufferSize then initBuffer()
        try
            buffer.[index]
        finally
            index <- index + 1
            
    member this.ReadByte()   = readByte()

    member this.ReadBytes(n) = readBytes n
        
    interface IDisposable with 
        member this.Dispose() = bufferPool.Put buffer