namespace AdventOfCode.Day05

module Puzzles =
    let puzzles filename =
        let line = System.IO.File.ReadAllText filename
        let opcodes = line.Split(",", System.StringSplitOptions.RemoveEmptyEntries) 
                        |> List.ofSeq 
                        |> List.map(Operators.int)

        printfn ""
        printfn "Question 1 - please type 1 at the prompt"
        printfn "======================================"
        printfn ""
        let memory1 = AdventOfCode.Shared.Intcode.execute opcodes

        printfn ""
        printfn "Question 2 - please type 5 at the prompt"
        printfn "======================================"
        printfn ""
        let memory2 = AdventOfCode.Shared.Intcode.execute opcodes

        (memory1, memory2)
