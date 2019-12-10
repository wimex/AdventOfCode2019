namespace AdventOfCode.Day05

open AdventOfCode.Shared.Intcode

module Puzzles =
    let rec runToHalt cpu result = 
        if cpu.State = Halt then result
        else
            let ncpu = AdventOfCode.Shared.Intcode.execute cpu
            let nresult = if ncpu.Data.IsSome then ncpu.Data.Value else result

            runToHalt ncpu nresult

    let puzzles filename =
        let line = System.IO.File.ReadAllText filename
        let opcodes = line.Split(",", System.StringSplitOptions.RemoveEmptyEntries) 
                        |> List.ofSeq 
                        |> List.map(Operators.int)
                            
        let state1 = { CPU.State = Boot; CPU.Instruction = Instruction.Default; CPU.Address = 0; CPU.RelativeBase = 0; CPU.Memory = opcodes; CPU.Data = None }
        let state2 = AdventOfCode.Shared.Intcode.execute state1
        let state3 = { CPU.State = state2.State; CPU.Instruction = state2.Instruction; CPU.Address = state2.Address; CPU.RelativeBase = state2.RelativeBase; CPU.Memory = state2.Memory; CPU.Data = Some(1) }
        let statex = runToHalt state3 0
        
        
        let state5 = { CPU.State = Boot; CPU.Instruction = Instruction.Default; CPU.Address = 0; CPU.RelativeBase = 0; CPU.Memory = opcodes; CPU.Data = None }
        let state6 = AdventOfCode.Shared.Intcode.execute state5
        let state7 = { CPU.State = state6.State; CPU.Instruction = state6.Instruction; CPU.Address = state6.Address; CPU.RelativeBase = state6.RelativeBase; CPU.Memory = state6.Memory; CPU.Data = Some(5) }
        let statey = runToHalt state7 0

        printfn "Diagnostic code 1: %d" statex
        printfn "Diagnostic code 2: %d" statey
