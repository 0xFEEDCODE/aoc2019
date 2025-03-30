
module aoc2019.day01
open util

let calcFuel1 mass = int (floor (double mass/3.)) - 2

let rec calcFuel2 mass =
    let rec loop unit total =
        if unit <= 0 then
            total
        else
            let new_fuel = calcFuel1 unit
            let new_total = total + max 0 new_fuel
            loop new_fuel new_total
    loop mass 0

let solve() =
    let masses = aocIO.getInput(false) |> Seq.map int
    
    let a1 = masses |> Seq.map(calcFuel1) |> Seq.sum
    printfn $"%A{a1}"
    
    let a2 = masses |> Seq.map(calcFuel2) |> Seq.sum
    printfn $"%A{a2}"
    
    
    0

