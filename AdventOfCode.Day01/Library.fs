namespace AdventOfCode.Day01

module Puzzles =
    let calculateMassP1 number =
        (Operators.floor (number / 3.0f)) - 2.0f

    let rec calculateMassP2 number =
        let mass = calculateMassP1 number

        match mass with
        | mass when mass <= 0.0f -> 0.0f
        | _ -> mass + calculateMassP2(mass)

    let puzzle1 numbers =
        let masses = numbers |> List.map(calculateMassP1)
        List.sum(masses)

    let puzzle2 numbers =
        let masses = numbers |> List.map(calculateMassP2)
        List.sum(masses)
        
    let puzzles filename = 
        let lines = System.IO.File.ReadAllLines(filename)
        let numbers = lines |> List.ofSeq |> List.map(Operators.float32)

        let res1 = puzzle1 numbers
        let res2 = puzzle2 numbers

        printfn "Total mass for puzzle 1: %.0f" res1
        printfn "Total mass for puzzle 2: %.0f" res2