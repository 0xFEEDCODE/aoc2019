
module aoc2019.day01
open helper

let rec fn x acc =
    if (x <= 0) then
        acc
    else
        fn ((x / 3)-2) (acc+x)

let solve() =
    let inp = helper.getInput(false)
    
    let ans1 = inp |> Seq.sumBy(fun n -> ((int n) / 3)-2)
    printfn "%A" ans1
    
    //helper.submitAnswer 1 ans1
    let ans2 = inp |> Seq.sumBy(fun x -> (fn (int x) 0) - int x)
        
    printfn "%A" (ans2)
    helper.submitAnswer 2 ans2
    0

