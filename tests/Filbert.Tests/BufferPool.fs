module BufferPool

open NUnit.Framework
open FsUnit
open Filbert.Utils

[<TestFixture>]
type ``Given a buffer pool`` () =
    [<Test>]
    member test.``when initialized it should have the correct size`` () =
        let pool = new BufferPool(5, 10)
        pool.Size |> should equal 5

    [<Test>]
    member test.``when a buffer is taken out it should reduce the pool's size`` () =
        let pool = new BufferPool(5, 10)
        
        let buffer = pool.Get()
        buffer.Length   |> should equal 10
        pool.Size       |> should equal 4

    [<Test>]
    member test.``when a buffer is put back it should increase the pool's size`` () =
        let pool = new BufferPool(5, 10)
        
        pool.Get() |> pool.Put
        pool.Size  |> should equal 5

    [<Test>]
    member test.``when a buffer is put back it should be reset`` () =
        let pool = new BufferPool(1, 10)
        
        let buffer = pool.Get()
        buffer.[0] <- 42uy // make it dirty

        pool.Put buffer
        let buffer' = pool.Get()
        buffer      |> should equal buffer' // should still be the same instance of array
        buffer'.[0] |> should equal 0uy     // but the change we made is reset

    [<Test>]
    member test.``when the pool is exhausted then the new buffers are created`` () =
        let pool = new BufferPool(1, 10)

        let buffer1 = pool.Get()
        pool.Size   |> should equal 0

        let buffer2 = pool.Get()
        pool.Size   |> should equal 0

        pool.Put buffer1
        pool.Put buffer2
        pool.Size   |> should equal 2