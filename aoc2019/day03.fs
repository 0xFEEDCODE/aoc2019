module aoc2019.day03

open aoc2019.util

type Point2D with
    member this.LinearDistance (other: Point2D) =
        (sqrt (((pown (this.x - other.x) 2) + (pown (this.y - other.y) 2)) |> float))

type Vector =
    { Start: int; End: int }
    
    member this.Distance =
        this.End - this.Start

type Dir = | L | U | R | D
type Line =
    { P1: Point2D; P2: Point2D }
    
    member this.MinX = min this.P1.x this.P2.x
    member this.MinY = min this.P1.y this.P2.y
    member this.MaxX = max this.P1.x this.P2.x
    member this.MaxY = max this.P1.y this.P2.y
    
    member this.XVector = {Start = this.MinX; End = this.MaxX}
    member this.YVector = {Start = this.MinY; End = this.MaxY }
    
    member this.XPoints = [|this.P1.x; this.P2.x|]
    member this.YPoints = [|this.P1.y; this.P2.y|]
    member this.IsHLine = this.P1.x <> this.P2.x
    member this.IsVLine = this.P1.y <> this.P2.y
    
    member this.LineDirection =
        if (this.IsVLine) then
            if this.P2.y < this.P1.y then U else D
        else 
            if this.P2.x < this.P1.x then L else R
            
    member this.Length =
        if this.IsVLine then this.YVector.Distance else this.XVector.Distance
        
    member this.GetCrossedPoint(line: Line) =
        let intersect (v1 : Vector) (v2 : Vector) =
            let fstV, sndV =
                if v1.Start <= v2.Start then (v1,v2) else (v2,v1)
                
            if (fstV.Start <= sndV.Start && sndV.Start <= fstV.End) then
                Some({Start = sndV.Start
                      End = sndV.Start + abs fstV.End  - abs sndV.Start })
            else
                None
            
        let intersectX = intersect this.XVector line.XVector
        let intersectY = intersect this.YVector line.YVector
        if(intersectX.IsSome && intersectY.IsSome && intersectX.Value.Distance <> 0 && intersectY.Value.Distance <> 0) then
            let x,y = if this.XVector.Distance = 0 && line.YVector.Distance = 0 then (this.MinX, line.MinY) else (line.MinX, this.MinY)
            
            Some(Point2D(x,y))
        else
            None
    
let getRelativePosOfWire (origin: Point2D) (wire: string) =
    let direction = wire[0]
    let length = System.Int32.Parse(wire[1..])
    
    let calcFn =
        match direction with
        | 'R' | 'D' -> (+)
        | 'U' | 'L' -> (-)
    
    match direction with
    | 'R' | 'L' -> Point2D((origin.x, length) ||> calcFn, origin.y)
    | 'U' | 'D' -> Point2D(origin.x, (origin.y, length) ||> calcFn)

let solve() =
    let wires = aocIO.getInput() |> Seq.map(fun line -> line.Split ',' |> Seq.toArray) |> Seq.toArray
    let centralPort = Point2D(0,0)
    
    let points = wires |> Array.map(fun wire -> wire |> Array.scan(getRelativePosOfWire) centralPort)
    let lines = points |> Array.map(fun p -> p |> Array.windowed 2 |> Array.map(fun p -> {P1 = p[0]; P2 = p[1] }))
    
    let crossPoints = lines[0] |> Array.map(fun l1 -> lines[1]
                                                   |> Array.map(fun l2 -> if l1 <> l2 then l1.GetCrossedPoint(l2) else None))
                      |> Array.collect id
                      |> Array.where(_.IsSome)
                      |> Array.map Option.get
                      |> Array.distinct
                      
    let a1 = crossPoints |> Array.map(fun p -> (p, centralPort.GetManhattanDistance(p))) |> Array.minBy(snd) |> snd
    printfn $"%A{a1}"
    
    let getNSteps (origin: Point2D) (dest: Point2D) (path: Line seq) =
        let rec core (origin: Point2D) (path: Line seq) acc =
            let line = path |> Seq.head
            let isFound, nSteps = 
                if (origin.x = dest.x && line.IsVLine && (if (line.LineDirection = U) then line.P2.y <= dest.y else line.P2.y >= dest.y)) ||
                   (origin.y = dest.y && line.IsHLine && (if (line.LineDirection = L) then line.P2.x <= dest.x else line.P2.x >= dest.x))
                 then
                        true, acc + (line.P1.LinearDistance(dest) |> int)
                else
                    false, acc + line.Length
            if isFound then nSteps else core line.P2 (path |> Seq.skip 1) nSteps
        core origin path 0
     
    let firstCrossPoint = crossPoints |> Array.head
    let a2 = lines |> Seq.sumBy(getNSteps centralPort firstCrossPoint)
    
    printfn $"%A{a2}"