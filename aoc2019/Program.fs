open System.Diagnostics
open System.IO
open aoc2019

let sw = Stopwatch()

let c = File.ReadAllText("test.txt")



sw.Start()
(*
day01.solve()
day02.solve()
day20.solve()
*)
day02.solve()
sw.Stop()

printfn $"Time taken - %A{sw.Elapsed}"