namespace AdventOfCode.Day03

module Puzzles =
    type Point = 
        {
            X: int32
            Y: int32
            L: int32
            D: string
        }

    type Line =
        {
            P1: Point
            P2: Point
            LN: int32
        }

    let (|Direction|_|) (prefix: string) (input: string option) = 
        if input.IsNone then
            None
        else if input.Value.StartsWith(prefix) then
            Some(input.Value.Substring(prefix.Length) |> Operators.int32)
        else
            None

    let getDistance p1 p2 =
        { Line.P1 = p1; Line.P2 = p2; Line.LN = (System.Math.Abs(p1.X - p2.X) + (System.Math.Abs(p1.Y - p2.Y))) }

    let getWireLength point wirea wireb =
        match wirea.D with
        | "L" -> getDistance point wireb
        | "R" -> getDistance point wirea
        | "U" -> getDistance point wirea
        | "D" -> getDistance point wirea
        | _   -> raise(System.Exception("Invalid wire direction"))

    let rec getLines x y l line =
        let head = Seq.tryHead line
            
        match head with
        | Direction "L" length ->
            let ll = l + length
            let ln = { Line.P1 = {Point.X = x-length; Point.Y = y; L = l; D = "L"}; Line.P2 = {Point.X = x; Point.Y = y; L = l; D = "L"}; Line.LN = ll }
            ln :: getLines (x - length) y ll (Seq.skip 1 line)
        | Direction "R" length ->
            let ll = l + length
            let ln = { Line.P1 = {Point.X = x; Point.Y = y; L = l; D = "R"}; Line.P2 = {Point.X = x + length; Point.Y = y; L = l; D = "R"}; Line.LN = ll }
            ln :: getLines (x + length) y ll (Seq.skip 1 line)
        | Direction "U" length -> 
            let ll = l + length
            let ln = { Line.P1 = {Point.X = x; Point.Y = y; L = l; D = "U"}; Line.P2 = {Point.X = x; Point.Y = y - length; L = l; D = "U"}; Line.LN = ll }
            ln :: getLines x (y - length) ll (Seq.skip 1 line)
        | Direction "D" length -> 
            let ll = l + length
            let ln = { Line.P1 = {Point.X = x; Point.Y = y; L = l; D = "D"}; Line.P2 = {Point.X = x; Point.Y = y + length; L = l; D = "D"}; Line.LN = ll }
            ln :: getLines x (y + length) ll (Seq.skip 1 line)
        | _ ->
            List.Empty

    let getIntersection (a: Point) (b: Point) (c: Point) (d: Point) =
        let a1 = b.Y - a.Y
        let b1 = a.X - b.X
        let c1 = a1 * (a.X) + b1 * (a.Y)

        let a2 = d.Y - c.Y
        let b2 = c.X - d.X
        let c2 = a2 * (c.X) + b2 * (c.Y)

        let dt = a1 * b2 - a2 * b1
        if dt <> 0 then
            let x = (b2 * c1 - b1 * c2) / dt
            let y = (a1 * c2 - a2 * c1) / dt

            let t1 = System.Math.Min(a.X, b.X)
            let t2 = System.Math.Max(a.X, b.X)
            let t3 = System.Math.Min(a.Y, b.Y)
            let t4 = System.Math.Max(a.Y, b.Y)

            let t5 = System.Math.Min(c.X, d.X)
            let t6 = System.Math.Max(c.X, d.X)
            let t7 = System.Math.Min(c.Y, d.Y)
            let t8 = System.Math.Max(c.Y, d.Y)

            if t1 <= x && x <= t2 && t3 <= y && y <= t4 && t5 <= x && x <= t6 && t7 <= y && y <= t8 then
                let tm = { Point.X = x; Point.Y = y; L = 0; D = ""}
                let d1 = getWireLength tm a b
                let d2 = getWireLength tm c d

                Some({ Point.X = x; Point.Y = y; Point.L = a.L + d1.LN + c.L + d2.LN; Point.D = ""})
            else
                None
        else
            None

    let rec getIntersections2 a b line =
        let head = Seq.tryHead line

        if head.IsSome then
            let it = getIntersection a b head.Value.P1 head.Value.P2
            let tail = getIntersections2 a b (Seq.skip 1 line)
            
            if it.IsSome then it.Value :: tail else tail
        else
            List.Empty

    let rec getIntersections1 line1 line2 =
        let head = Seq.tryHead line1

        if head.IsSome then
            let it = getIntersections2 head.Value.P1 head.Value.P2 line2
            Seq.append it (getIntersections1 (Seq.skip 1 line1) line2)
        else
            Seq.empty

    let rec getDistances target points = 
        let head = Seq.tryHead points

        if head.IsSome then
            let dst = getDistance target head.Value
            dst :: (getDistances target (Seq.skip 1 points))
        else
            List.Empty
        

    let puzzles filename =
        let lines = System.IO.File.ReadAllLines filename
        let line1 = lines.[0].Split(",", System.StringSplitOptions.RemoveEmptyEntries) |> List.ofSeq
        let line2 = lines.[1].Split(",", System.StringSplitOptions.RemoveEmptyEntries) |> List.ofSeq

        let map1 = getLines 0 0 0 line1
        let map2 = getLines 0 0 0 line2

        let irs = getIntersections1 map1 map2
        let dst = getDistances { Point.X = 0; Point.Y = 0; L = 0; D = "" } irs

        printfn "-- (XCORD,YCORD, WLENG) --> MNHTN"
        printfn "-- ============================="
        for distance in dst do
            printfn "-- (% 5d,% 5d,% 5d) --> % 5d" distance.P2.X distance.P2.Y distance.P2.L distance.LN

        printfn ""
        printfn ""
        printfn "XCORD, YCORD: X and Y coordinates of the intersections"
        printfn "WLENG: wire length to the intersection point"
        printfn "MNHTN: Manhattan distance of the intersection point from (0,0)"
