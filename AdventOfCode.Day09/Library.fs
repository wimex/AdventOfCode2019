namespace AdventOfCode.Day09

open AdventOfCode.Shared.Intcode

module Puzzles =
    let rec runToCompletition cpu inputs outputs =
        match cpu.State with
        | Halt  -> (cpu, outputs)
        | Input ->
            let cpu = { CPU.State = cpu.State; CPU.Instruction = cpu.Instruction; CPU.Address = cpu.Address;CPU.RelativeBase = cpu.RelativeBase; CPU.Memory = cpu.Memory; CPU.Data = Some(List.head inputs)}
            let state = AdventOfCode.Shared.Intcode.execute cpu
            runToCompletition state (List.skip 1 inputs) outputs
        | Output ->
            let state = AdventOfCode.Shared.Intcode.execute cpu
            runToCompletition state inputs (cpu.Data::outputs)
        | _     ->
            let state = AdventOfCode.Shared.Intcode.execute cpu
            runToCompletition state inputs outputs

    let puzzles filename = 
        let line = System.IO.File.ReadAllText filename
        let opcodes = line.Split(",", System.StringSplitOptions.RemoveEmptyEntries) 
                        |> List.ofSeq 
                        |> List.indexed
                        |> List.map(fun (i,j) -> (int64(i), int64(j)))
                        |> Map.ofSeq
                       
        let cpu =  { CPU.State = Boot; CPU.Instruction = Instruction.Default; CPU.Address = 0L; CPU.RelativeBase = 0L; CPU.Memory = opcodes; CPU.Data = None }
        let (status1, outputs1) = runToCompletition cpu [1L] List.Empty
        let (status2, outputs2) = runToCompletition cpu [2L] List.Empty

        printfn ""
        printfn "BOOST keycode: %d" outputs1.Head.Value
        printfn "Coordinates: %d" outputs2.Head.Value