
module aoc2019.day01
open helper

let solve() =
    let inp = helper.getInput()
    
    let ans1 = inp |> Seq.sumBy(fun n -> ((int n) / 3)-2)
    printfn "%A" ans1
    
    //helper.submitAnswer 1 ans1
    
    let mutable fuel = inp |> Seq.map(fun n -> ((int n) /3)-2, 0)
    while(not (fuel |> Seq.forall(fun x -> fst x <= 0))) do
        fuel <- fuel |> Seq.map(fun (f,s) -> (f/3)-2, if f > 0 then s + f else s)
        
    printfn "%A" (fuel |> Seq.sumBy(snd))
    //helper.submitAnswer 2 (fuel |> Seq.sumBy(snd))
    0

