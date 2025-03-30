module aoc2019.day02
open Microsoft.FSharp.Collections
open aoc2019.util

let solve() =
    let mutable input = (aocIO.getInput() |> Seq.head).Split ',' |> Seq.map int |> Seq.toArray
    
    let runProgram (program: int array) =
        let mutable program = program |> Seq.toArray
        program
        |> Seq.chunkBySize 4
        |> Seq.iter (fun seq ->
            if seq.Length = 4 && seq[0] <> 99 then
                program[seq[3]] <- (if seq[0] = 1 then program[seq[1]] + program[seq[2]] else program[seq[1]] * program[seq[2]])
            else
                ())
        
        let outp = program[0]
        outp
    
    let mutable program = input
    program[1] <- 12
    program[2] <- 2
    let a1 = runProgram input
    printfn $"%A{a1}"
    
    let target = 19690720
    let noun_idx = 1
    let verb_idx = 2
    
    let findHighestPossibleArgVal parameter_idx target =
        let mutable argVal = 0
        let mutable outp = 0
        
        outp <- runProgram program
        while(outp < target) do
            argVal <- argVal + 1
            program[parameter_idx] <- argVal
            outp <- runProgram program
            
        if (outp = target) then argVal else argVal-1
    
    let noun = findHighestPossibleArgVal noun_idx target
    program[noun_idx] <- noun
    
    let verb = findHighestPossibleArgVal verb_idx target
    program[verb_idx] <- verb
    
    let outp = runProgram program
    printfn $"%A{outp}"
    if (outp <> target) then
        failwith "F"
        
    printfn $"%A{(noun, verb)}"
    let a2 = (100 * noun + verb)
    printfn $"%A{a2}"
    
