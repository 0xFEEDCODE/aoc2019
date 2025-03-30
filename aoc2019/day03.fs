module aoc2019.day03

open aoc2019.util

type Vector =
    { Start: int; End: int }
    
    member this.Distance =
        this.End - this.Start

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
                      
    for cp in crossPoints do
        printfn $"%A{cp}"
        
    let a1 = crossPoints |> Array.map(centralPort.GetManhattanDistance) |> Array.min
    let a = crossPoints |> Array.map(fun p -> (p, centralPort.GetManhattanDistance(p))) |> Array.sortByDescending(snd)
    for (p,s) in a do
        printfn $"%A{(p,s)}"
    0
