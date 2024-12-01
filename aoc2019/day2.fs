module aoc2019.day02

open Microsoft.FSharp.Collections
open aoc2019.util

let solve() =
    let mutable inp = (aocIO.getInput() |> Seq.head).Split ',' |> Seq.map int |> Seq.toArray
    
    inp[1] <- 12;
    inp[2] <- 2
    
    inp
    |> Seq.chunkBySize 4
    |> Seq.iter (fun x ->
        if x.Length = 4 && x[0] <> 99 then
            inp[x[3]] <- (if x[0] = 1 then inp[x[1]] + inp[x[2]] else inp[x[1]] * inp[x[2]])
        else
            ())
    
    printfn $"%A{inp}"
    let ans1 = inp[0]
    
    printfn $"%A{ans1}"
    //helper.submitAnswer 1 ans1
    
