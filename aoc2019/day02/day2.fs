module aoc2019.day02

open Microsoft.FSharp.Collections
open helper

let solve() =
    let mutable inp = (helper.getInput() |> Seq.head).Split ',' |> Seq.map int |> Seq.toArray
    
    inp[1] <- 12;
    inp[2] <- 2;
    
    let mutable ip = 0
    while inp[ip] <> 99 do
        match inp[ip] with
        | 1 ->
            let n1 = inp[inp[ip+1]]
            let n2 = inp[inp[ip+2]]
            inp[inp[ip+3]] <- n1+n2
            ip <- ip+4
        | 2 -> 
            let n1 = inp[inp[ip+1]]
            let n2 = inp[inp[ip+2]]
            inp[inp[ip+3]] <- n1*n2
            ip <- ip+4
        | _ -> failwith "wtf"
            
    let ans1 = inp[0]
    printfn "%A" ans1
    helper.submitAnswer 1 ans1
    
