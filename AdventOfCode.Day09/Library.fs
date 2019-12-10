namespace AdventOfCode.Day09

open AdventOfCode.Shared.Intcode

module Puzzles =
    let rec runToCompletition cpu inputs =
        match cpu.State with
        | Halt  -> cpu
        | Input ->
            let cpu = { CPU.State = cpu.State; CPU.Instruction = cpu.Instruction; CPU.Address = cpu.Address;CPU.RelativeBase = cpu.RelativeBase; CPU.Memory = cpu.Memory; CPU.Data = Some(List.head inputs)}
            let state = AdventOfCode.Shared.Intcode.execute cpu
            runToCompletition state (List.skip 1 inputs)
        | _     ->
            let state = AdventOfCode.Shared.Intcode.execute cpu
            runToCompletition state inputs

    let puzzles filename = 
        let line = System.IO.File.ReadAllText filename
        let opcodes = line.Split(",", System.StringSplitOptions.RemoveEmptyEntries) 
                        |> List.ofSeq 
                        |> List.indexed
                        |> List.map(fun (i,j) -> (int64(i), int64(j)))
                        |> Map.ofSeq
                       
        let cpu =  { CPU.State = Boot; CPU.Instruction = Instruction.Default; CPU.Address = 0L; CPU.RelativeBase = 0L; CPU.Memory = opcodes; CPU.Data = None }
        let status = runToCompletition cpu [1L]

        status