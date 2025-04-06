open System.Diagnostics
open aoc2019

let sw = Stopwatch()



sw.Start()
(*
day01.solve()
day02.solve()
day03.solve()
day12.solve()
*)
day04.solve()
sw.Stop()


printfn $"Time taken - %A{sw.Elapsed}"