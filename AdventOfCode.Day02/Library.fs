namespace AdventOfCode.Day02

open AdventOfCode.Shared.Intcode

//module Puzzles =
//    [<Literal>]
//    let puzzle2target = 19690720

//    let rec finder noun verb target opcodes =
//        let start1 = AdventOfCode.Shared.Intcode.poke opcodes 1 noun
//        let start2 = AdventOfCode.Shared.Intcode.poke start1 2 verb

//        let cpu = { CPU.State = Boot; CPU.Instruction = Instruction.Default; CPU.Address = 0; CPU.RelativeBase = 0; CPU.Memory = start2; CPU.Data = None }
//        let result = AdventOfCode.Shared.Intcode.execute cpu
//        let nextNoun = if verb = opcodes.Length - 1 then noun + 1 else noun
//        let nextVerb = if verb = opcodes.Length - 1 then 0 else verb + 1
        
//        if result.Memory.[0] = target then
//            (noun, verb, opcodes)
//        else
//            finder nextNoun nextVerb target opcodes
        
//    let puzzle1 opcodes = 
//        let start1 = AdventOfCode.Shared.Intcode.poke opcodes 1 12
//        let start2 = AdventOfCode.Shared.Intcode.poke start1 2 2
        
//        let cpu = { CPU.State = Boot; CPU.Instruction = Instruction.Default; CPU.Address = 0; CPU.RelativeBase = 0; CPU.Memory = start2; CPU.Data = None }
//        let result = AdventOfCode.Shared.Intcode.execute cpu
//        printfn "Final element in position 0: %d" result.Memory.[0]

//    let puzzle2 opcodes =
//        let (noun, verb, _) = finder 0 0 puzzle2target opcodes
//        let summary = (100*noun + verb)

//        printfn "Output found with noun / verb %d / %d -> %d" noun verb summary

//    let puzzles filename = 
//        let line = System.IO.File.ReadAllText filename
//        let opcodes = line.Split(",", System.StringSplitOptions.RemoveEmptyEntries) 
//                        |> List.ofSeq 
//                        |> List.map(Operators.int)

//        puzzle1 opcodes
//        puzzle2 opcodes