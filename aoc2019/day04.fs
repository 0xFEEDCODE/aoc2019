module aoc2019.day04

open aoc2019.util

let rec getAdjacentN (l: int list) (acc: int list list) =
    match l with
    | [] -> acc
    | h::t ->
        let next = t |> Seq.tryHead
        let prev = acc |> Seq.tryLast
        
        if next.IsSome && h+1 = next.Value then
            if prev.IsSome && h = (prev.Value|>Seq.last) then
                let newAcc = (acc[0..acc.Length-2] @ [prev.Value @ [next.Value]])
                getAdjacentN t newAcc
            else
                getAdjacentN t (acc @ [[h; next.Value]])
        else
            if prev.IsSome && h = (prev.Value|>Seq.last) then
                getAdjacentN t acc
            else
                getAdjacentN t (acc @ [[h;]])

let isValid n allowLargeGroupMatching =
    let getDigits n =
        Seq.initInfinite (pown 10)
        |> Seq.takeWhile(fun x -> x <= n)
        |> Seq.map(fun x -> (n / x) % 10)
        |> Seq.rev
        |> Seq.toArray
        
    let digits = getDigits n
    let digitPairs = digits |> Array.pairwise
    
    let arePairsMatching = (digitPairs |> Array.map(fun (d1, d2) -> d1 = d2))
    
    (digits |> Array.length) = 6 &&
        (digitPairs |> Array.forall(fun (d1, d2) -> d1 <= d2)) &&
        if allowLargeGroupMatching then
            arePairsMatching |> Array.contains true
        else
            let matchingPairIndexes = arePairsMatching |> Array.mapi(fun i x -> (i, x)) |> Array.where snd |> Array.map fst |> Array.toList
            let adjacent = getAdjacentN matchingPairIndexes []
            adjacent |> Seq.exists(fun adj -> (adj |> Seq.length) = 1)

let solve() =
    let range = (aocIO.getInput() |> Seq.head).Split '-' |> Seq.map(int)
    let rangeStart = range |> Seq.head
    let rangeEnd = range |> Seq.last
    
    let a1 = {rangeStart..rangeEnd} |> Seq.where(fun n -> isValid n true) |> Seq.length
    printfn $"%A{a1}"
    
    let a2 = {rangeStart..rangeEnd} |> Seq.where(fun n -> isValid n false) |> Seq.length
    printfn $"%A{a2}"
    
    
