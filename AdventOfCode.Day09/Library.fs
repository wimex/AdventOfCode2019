namespace AdventOfCode.Day09

open AdventOfCode.Shared.Intcode

module Puzzles =
    let rec runToCompletition cpu =
        if cpu.State = Halt then cpu
        else
            let state = AdventOfCode.Shared.Intcode.execute cpu
            runToCompletition state

    let puzzles filename = 
        let line = System.IO.File.ReadAllText filename
        let opcodes = line.Split(",", System.StringSplitOptions.RemoveEmptyEntries) 
                        |> List.ofSeq 
                        |> List.map(Operators.int)
                       
        let memory = Array.zeroCreate 34463338 |> List.ofSeq
        let cpu =  { CPU.State = Boot; CPU.Instruction = Instruction.Default; CPU.Address = 0; CPU.RelativeBase = 0; CPU.Memory = List.append opcodes memory; CPU.Data = None }
        let status = runToCompletition cpu

        status