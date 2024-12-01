
module aoc2019.day01
open util

let calc x = (x/3) - 2
let rec fn x acc =
    if (x <= 0) then
        acc
    else
        fn (calc x) (acc+x)

let solve() =
    let inp = aocIO.getInput(false) |> Seq.map int
    
    let ans1 = inp |> Seq.sumBy(calc)
    printfn $"%A{ans1}"
    
    //helper.submitAnswer 1 ans1
    let ans2 = inp |> Seq.sumBy(fun x -> (fn (int x) 0) - int x)
        
    printfn $"%A{ans2}"
    //helper.submitAnswer 2 ans2
    0

