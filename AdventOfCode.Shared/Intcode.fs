namespace AdventOfCode.Shared

open System

module Intcode =
    [<Literal>]
    let debugmode = true

    type Instruction = 
        {
            Opcode: int64
            Mode1: int64
            Mode2: int64
            Mode3: int64
        }
    with
        static member Default = { Opcode = 0L; Mode1 = 0L; Mode2 = 0L; Mode3 = 0L }

    type State =
    | Noop
    | Boot
    | Execute
    | Input
    | Output
    | Halt

    type CPU<'A> =
        {
            State: State
            Instruction: Instruction
            Address: int64
            RelativeBase: int64
            Memory: Map<int64, int64>
            Data: 'A option
        }
    with
        static member Default = { State = Noop; Instruction = Instruction.Default; Address = 0L; RelativeBase = 0L; Memory = Map.empty; Data = None }

    let poke (map: Map<int64, int64>) address value =
        map.Add(address, value)
    
    let rec getDigits number =
        if number <> 0L then
            let modulo = number % 10L
            getDigits((number - modulo) / 10L) @ [modulo]
        else
            List.Empty

    let printDiagnostics parameters =
        if debugmode then
            printf "# "
            for item in parameters do
                printf "%d " item
            printfn ""

    let getValue address mode rbase (opcodes: Map<int64, int64>) =
        match mode with
        | 0L -> if opcodes.ContainsKey opcodes.[address] then opcodes.[opcodes.[address]] else 0L
        | 1L -> if opcodes.ContainsKey address then opcodes.[address] else 0L
        | 2L -> if opcodes.ContainsKey (rbase + opcodes.[address]) then opcodes.[rbase + opcodes.[address]] else 0L
        | _ -> raise(Exception("Unable to read in unknown addressing mode"))

    let setValue address mode rbase value (opcodes: Map<int64, int64>) =
        match mode with
        | 0L -> poke opcodes opcodes.[address] value
        | 1L -> poke opcodes address value
        | 2L -> poke opcodes (rbase + opcodes.[address]) value
        |  _ -> raise(Exception("Unable to write in unknown addressing mode"))

    let getInstruction address (opcodes: Map<int64, int64>) =
        let opcode = opcodes.[address]
        let digits = getDigits opcode

        let instruction = if digits.Length >= 2 then 10L * digits.[digits.Length - 2] + digits.[digits.Length - 1] else digits.[digits.Length - 1]
        let mode1 = if digits.Length >= 3 then digits.[digits.Length - 3] else 0L
        let mode2 = if digits.Length >= 4 then digits.[digits.Length - 4] else 0L
        let mode3 = if digits.Length >= 5 then digits.[digits.Length - 5] else 0L

        {
            Instruction.Opcode = instruction;
            Instruction.Mode1 = mode1;
            Instruction.Mode2 = mode2;
            Instruction.Mode3 = mode3;
         }

    let parseInstruction cpu  =
        //printfn ". %s %d %d %d" (instruction.Opcode.ToString().PadLeft(5, '0')) instruction.Mode1 instruction.Mode2 instruction.Mode3
        let instruction = cpu.Instruction
        let memory = cpu.Memory

        match instruction.Opcode with
        |  1L ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1L]; memory.[cpu.Address + 2L]; memory.[cpu.Address + 3L]; ]

            let operand1 = getValue (cpu.Address + 1L) instruction.Mode1 cpu.RelativeBase memory
            let operand2 = getValue (cpu.Address + 2L) instruction.Mode2 cpu.RelativeBase memory

            let value = operand1 + operand2
            let target = cpu.Address + 3L
            
            let nextMemory = setValue target instruction.Mode3 cpu.RelativeBase value memory
            let nextAddress = cpu.Address + 4L
            let nextInstruction = getInstruction nextAddress nextMemory
            {
                CPU.State = Execute;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = cpu.RelativeBase;
                CPU.Memory = nextMemory;
                CPU.Data = None;
            }
        |  2L ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1L]; memory.[cpu.Address + 2L]; memory.[cpu.Address + 3L]; ]

            let operand1 = getValue (cpu.Address + 1L) instruction.Mode1 cpu.RelativeBase memory
            let operand2 = getValue (cpu.Address + 2L) instruction.Mode2 cpu.RelativeBase memory

            let value = operand1 * operand2
            let target = cpu.Address + 3L
            
            let nextMemory = setValue target instruction.Mode3 cpu.RelativeBase value memory
            let nextAddress = cpu.Address + 4L
            let nextInstruction = getInstruction nextAddress nextMemory
            {
                CPU.State = Execute;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = cpu.RelativeBase;
                CPU.Memory = nextMemory;
                CPU.Data = None;
            }
        |  3L ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1L]; ]

            if cpu.Data.IsSome then
                printfn "? %d" cpu.Data.Value

                let nextAddress = cpu.Address + 2L
                let nextMemory = setValue (cpu.Address + 1L) instruction.Mode1 cpu.RelativeBase cpu.Data.Value memory
                let nextInstruction = getInstruction nextAddress nextMemory
                {
                    CPU.State = Execute;
                    CPU.Instruction = nextInstruction;
                    CPU.Address = nextAddress;
                    CPU.RelativeBase = cpu.RelativeBase;
                    CPU.Memory = nextMemory;
                    CPU.Data = None
                }
            else
                printfn "? ?"

                {
                    CPU.State = Input;
                    CPU.Instruction = instruction
                    CPU.Address = cpu.Address;
                    CPU.RelativeBase = cpu.RelativeBase;
                    CPU.Memory = memory;
                    CPU.Data = None;
                }
        |  4L ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1L]; ]

            let output = getValue (cpu.Address + 1L) instruction.Mode1 cpu.RelativeBase memory
            printfn "! %d" output

            let nextAddress = cpu.Address + 2L
            let nextInstruction = getInstruction nextAddress memory
            {
                CPU.State = Output;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = cpu.RelativeBase;
                CPU.Memory = memory;
                CPU.Data = Some(output);
            }
        |  5L ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1L]; memory.[cpu.Address + 2L]; ]

            let operand1 = getValue (cpu.Address + 1L) instruction.Mode1 cpu.RelativeBase memory
            let operand2 = getValue (cpu.Address + 2L) instruction.Mode2 cpu.RelativeBase memory

            let nextAddress = if operand1 <> 0L then operand2 else cpu.Address + 3L
            let nextInstruction = getInstruction nextAddress memory
            {
                CPU.State = Execute;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = cpu.RelativeBase;
                CPU.Memory = memory;
                CPU.Data = None;
            }
        |  6L ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1L]; memory.[cpu.Address + 2L]; ]

            let operand1 = getValue (cpu.Address + 1L) instruction.Mode1 cpu.RelativeBase memory
            let operand2 = getValue (cpu.Address + 2L) instruction.Mode2 cpu.RelativeBase memory

            let nextAddress = if operand1 = 0L then operand2 else cpu.Address + 3L
            let nextInstruction = getInstruction nextAddress memory
            {
                CPU.State = Execute;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = cpu.RelativeBase;
                CPU.Memory = memory;
                CPU.Data = None;
            }
        |  7L ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1L]; memory.[cpu.Address + 2L]; memory.[cpu.Address + 3L]; ]

            let operand1 = getValue (cpu.Address + 1L) instruction.Mode1 cpu.RelativeBase memory
            let operand2 = getValue (cpu.Address + 2L) instruction.Mode2 cpu.RelativeBase memory

            let value = if operand1 < operand2 then 1L else 0L
            let target = getValue (cpu.Address + 3L) 1L cpu.RelativeBase memory

            let nextAddress = cpu.Address + 4L
            let nextMemory = setValue target 1L cpu.RelativeBase value memory
            let nextInstruction = getInstruction nextAddress nextMemory
            {
                CPU.State = Execute;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = cpu.RelativeBase;
                CPU.Memory = nextMemory;
                CPU.Data = None;
            }
        |  8L ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1L]; memory.[cpu.Address + 2L]; memory.[cpu.Address + 3L]; ]

            let operand1 = getValue (cpu.Address + 1L) instruction.Mode1 cpu.RelativeBase memory
            let operand2 = getValue (cpu.Address + 2L) instruction.Mode2 cpu.RelativeBase memory

            let value = if operand1 = operand2 then 1L else 0L
            let target = getValue (cpu.Address + 3L) 1L cpu.RelativeBase memory
            
            let nextAddress = cpu.Address + 4L
            let nextMemory = setValue target 1L cpu.RelativeBase value memory
            let nextInstruction = getInstruction nextAddress nextMemory
            {
                CPU.State = Execute;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = cpu.RelativeBase;
                CPU.Memory = nextMemory;
                CPU.Data = None;
            }
        |  9L ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1L]; ]
            
            let relativeBaseModifier = getValue (cpu.Address + 1L) instruction.Mode1 cpu.RelativeBase memory

            let nextAddress = cpu.Address + 2L
            let nextRelativeBase = cpu.RelativeBase + relativeBaseModifier
            let nextInstruction = getInstruction nextAddress memory
            {
                CPU.State = Execute;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = nextRelativeBase;
                CPU.Memory = memory;
                CPU.Data = None;
            }
        | 99L ->
            printDiagnostics [ memory.[cpu.Address]; ]

            {
                CPU.State = Halt;
                CPU.Instruction = cpu.Instruction;
                CPU.Address = cpu.Address;
                CPU.RelativeBase = cpu.RelativeBase;
                CPU.Memory = cpu.Memory;
                Data = None; 
            }
        |  _ -> raise(Exception("Unknown instruction"))

    let rec execute cpu  =
        let instruction = if cpu.State = Boot then { CPU.Instruction = (getInstruction cpu.Address cpu.Memory); CPU.Address = cpu.Address; CPU.RelativeBase = cpu.RelativeBase; CPU.Memory = cpu.Memory; CPU.State = Execute; Data = None } else cpu
        let result = parseInstruction instruction

        if result.State = Execute then
            execute result
        else
            result