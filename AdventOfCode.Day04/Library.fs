namespace AdventOfCode.Day04

open System

module Puzzles =
    [<Literal>]
    let lower = 152085

    [<Literal>]
    let higher = 670283

    type Piece =
        {
            A: int32
            B: int32
        }

    let rec getDigits number =
        if number <> 0 then
            let modulo = number % 10
            getDigits((number - modulo) / 10) @ [modulo]
        else
            List.Empty

    let rec verifyGrowing pieces = 
        let head = Seq.tryHead pieces

        if head.IsSome then
            (head.Value.A <= head.Value.B) && verifyGrowing (List.skip 1 pieces)
        else
            true

    let rec checkCriteria1 (digits: int list) =
        let pieces = 
            [
                {Piece.A = digits.[0]; Piece.B = digits.[1]}
                {Piece.A = digits.[1]; Piece.B = digits.[2]}
                {Piece.A = digits.[2]; Piece.B = digits.[3]}
                {Piece.A = digits.[3]; Piece.B = digits.[4]}
                {Piece.A = digits.[4]; Piece.B = digits.[5]}
            ]

        verifyGrowing pieces

    let rec checkCriteria2 digits =
        let grps = 
            digits |>
            Seq.countBy id |>
            Seq.filter (fun item -> (snd item) >= 2)
        Seq.length grps > 0

    let rec checkCriteria3 digits =
        let grps = 
            digits |>
            Seq.countBy id |>
            Seq.filter (fun item -> (snd item) = 2)
        Seq.length grps > 0

    let rec getMatches mcount1 mcount2 start stop =
        if start <= stop then
            let digits = getDigits start

            let c1 = checkCriteria1 digits
            let c2 = checkCriteria2 digits
            let c3 = checkCriteria3 digits

            if c1 && c2 then
                printfn "%d matches criteria 1" start
            if c1 && c3 then
                printfn "%d matches criteria 2" start

            let t1 = if c1 && c2 then mcount1 + 1 else mcount1
            let t2 = if c1 && c3 then mcount2 + 1 else mcount2

            getMatches t1 t2 (start + 1) stop
        else
            (mcount1, mcount2)

    let puzzles (filename: string) =
        let (m1, m2) = getMatches 0 0 lower higher

        printfn "%d numbers match criteria 1" m1
        printfn "%d numbers match criteria 2" m2

        filename
