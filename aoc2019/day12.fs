module aoc2019.day12
open aoc2019.util

open Microsoft.FSharp.Core

let getVelocityChange (a: Point3D) (b: Point3D) =
    let change x y =
        if x < y then 1
        elif x > y then -1
        else 0

    Point3D(change a.x b.x, change a.y b.y, change a.z b.z)

let updateVelocities (positions: Point3D array) (velocities: Point3D array) =
    for i in 0 .. positions.Length - 1 do
        for j in 0 .. positions.Length - 1 do
            if i <> j then
                velocities[i] <- velocities[i] + getVelocityChange positions[i] positions[j]

let updatePositions (positions: Point3D array) (velocities: Point3D array) =
    for i in 0 .. positions.Length - 1 do
        positions[i] <- positions[i] + velocities[i]

let solve () =
    let io = new aocIO 2019

    let initialPosition =
        io.getInput ()
        |> Seq.map (fun x ->
            let nums = x |> String.extractAllNums |> Seq.toArray
            Point3D(nums[0], nums[1], nums[2]))
        |> Seq.toArray

    let initialVelocity =
        Seq.init (initialPosition |> Seq.length) (fun _ -> Point3D(0, 0, 0))
        |> Seq.toArray

    let mutable orbits =
        (initialPosition |> Seq.toArray, initialVelocity |> Seq.toArray)

    let g, v = (ref (fst orbits), ref (snd orbits))

    let nOrbits = ((initialPosition |> Seq.length) - 1)

    for i in 0..999 do
        updateVelocities g.Value v.Value
        updatePositions g.Value v.Value

    let ans1 =
        seq { 0..nOrbits }
        |> Seq.sumBy (fun i ->
            let gv = g.Value[i]
            let vv = v.Value[i]
            (abs gv.x + abs gv.y + abs gv.z) * (abs vv.x + abs vv.y + abs vv.z))
        
    printfn $"%A{ans1}"

    let find (pred: Point3D -> int) =

        let mutable moons = (initialPosition |> Seq.toArray, initialVelocity |> Seq.toArray)
        let g, v = (ref (fst moons), ref (snd moons))

        let time =
            Seq.initInfinite (fun t -> (uint64 t) + 1UL)
            |> Seq.find (fun time ->
                updateVelocities g.Value v.Value
                updatePositions g.Value v.Value

                let isBackToInitial =
                    seq { 0..nOrbits }
                    |> Seq.forall (fun i ->
                        (pred ((fst moons)[i]) = pred (initialPosition[i]))
                        && (pred ((snd moons)[i]) = pred (initialVelocity[i])))

                time > 0UL && isBackToInitial)

        time

    let ans2 = [ find (_.x); find (_.y); find (_.z) ] |> Seq.reduce Math.lcm
    printfn $"%A{ans2}"

    0
