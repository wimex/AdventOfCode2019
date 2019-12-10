namespace AdventOfCode.Shared

open System

module Intcode =
    [<Literal>]
    let debugmode = true

    type Instruction = 
        {
            Opcode: int32
            Mode1: int32
            Mode2: int32
            Mode3: int32
        }
    with
        static member Default = { Opcode = 0; Mode1 = 0; Mode2 = 0; Mode3 = 0 }

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
            Address: int32
            RelativeBase: int32
            Memory: List<int32>
            Data: 'A option
        }
    with
        static member Default = { State = Noop; Instruction = Instruction.Default; Address = 0; RelativeBase = 0; Memory = List.Empty; Data = None }

    let poke list address value =
        list |> List.mapi (fun i v -> if i = address then value else v)
    
    let rec getDigits number =
        if number <> 0 then
            let modulo = number % 10
            getDigits((number - modulo) / 10) @ [modulo]
        else
            List.Empty

    let printDiagnostics parameters =
        if debugmode then
            printf "# "
            for item in parameters do
                printf "%d " item
            printfn ""

    let getValue address mode rbase (opcodes: List<int32>) =
        match mode with
        | 0 -> opcodes.[opcodes.[address]]
        | 1 -> opcodes.[address]
        | 2 -> opcodes.[rbase + opcodes.[address]]
        | _ -> raise(Exception("Unknown addressing mode"))

    let setValue address value (opcodes: List<int32>) = 
        poke opcodes address value

    let getInstruction address (opcodes: List<int32>) =
        let opcode = opcodes.[address]
        let digits = getDigits opcode

        let instruction = if digits.Length >= 2 then 10 * digits.[digits.Length - 2] + digits.[digits.Length - 1] else digits.[digits.Length - 1]
        let mode1 = if digits.Length >= 3 then digits.[digits.Length - 3] else 0
        let mode2 = if digits.Length >= 4 then digits.[digits.Length - 4] else 0
        let mode3 = if digits.Length >= 5 then digits.[digits.Length - 5] else 0

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
        let address = cpu.Address

        match instruction.Opcode with
        |  1 ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1]; memory.[cpu.Address + 2]; memory.[cpu.Address + 3]; ]

            let operand1 = getValue (cpu.Address + 1) instruction.Mode1 cpu.RelativeBase memory
            let operand2 = getValue (cpu.Address + 2) instruction.Mode2 cpu.RelativeBase memory

            let value = operand1 + operand2
            let target = memory.[cpu.Address + 3]
            
            let nextMemory = setValue target value memory
            let nextAddress = cpu.Address + 4
            let nextInstruction = getInstruction nextAddress nextMemory
            {
                CPU.State = Execute;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = cpu.RelativeBase;
                CPU.Memory = nextMemory;
                CPU.Data = None;
            }
        |  2 ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1]; memory.[cpu.Address + 2]; memory.[cpu.Address + 3]; ]

            let operand1 = getValue (cpu.Address + 1) instruction.Mode1 cpu.RelativeBase memory
            let operand2 = getValue (cpu.Address + 2) instruction.Mode2 cpu.RelativeBase memory

            let value = operand1 * operand2
            let target = memory.[cpu.Address + 3]
            
            let nextMemory = setValue target value memory
            let nextAddress = cpu.Address + 4
            let nextInstruction = getInstruction nextAddress nextMemory
            {
                CPU.State = Execute;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = cpu.RelativeBase;
                CPU.Memory = nextMemory;
                CPU.Data = None;
            }
        |  3 ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1]; ]

            if cpu.Data.IsSome then
                printfn "? %d" cpu.Data.Value

                let nextAddress = cpu.Address + 2
                let nextMemory = setValue memory.[cpu.Address + 1] cpu.Data.Value memory
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
        |  4 ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1]; ]

            let output = getValue (cpu.Address + 1) instruction.Mode1 cpu.RelativeBase memory
            printfn "! %d" output

            let nextAddress = cpu.Address + 2
            let nextInstruction = getInstruction nextAddress memory
            {
                CPU.State = Output;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = cpu.RelativeBase;
                CPU.Memory = memory;
                CPU.Data = Some(output);
            }
        |  5 ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1]; memory.[cpu.Address + 2]; ]

            let operand1 = getValue (cpu.Address + 1) instruction.Mode1 cpu.RelativeBase memory
            let operand2 = getValue (cpu.Address + 2) instruction.Mode2 cpu.RelativeBase memory

            let nextAddress = if operand1 <> 0 then operand2 else cpu.Address + 3
            let nextInstruction = getInstruction nextAddress memory
            {
                CPU.State = Execute;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = cpu.RelativeBase;
                CPU.Memory = memory;
                CPU.Data = None;
            }
        |  6 ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1]; memory.[cpu.Address + 2]; ]

            let operand1 = getValue (cpu.Address + 1) instruction.Mode1 cpu.RelativeBase memory
            let operand2 = getValue (cpu.Address + 2) instruction.Mode2 cpu.RelativeBase memory

            let nextAddress = if operand1 = 0 then operand2 else cpu.Address + 3
            let nextInstruction = getInstruction nextAddress memory
            {
                CPU.State = Execute;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = cpu.RelativeBase;
                CPU.Memory = memory;
                CPU.Data = None;
            }
        |  7 ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1]; memory.[cpu.Address + 2]; memory.[cpu.Address + 3]; ]

            let operand1 = getValue (cpu.Address + 1) instruction.Mode1 cpu.RelativeBase memory
            let operand2 = getValue (cpu.Address + 2) instruction.Mode2 cpu.RelativeBase memory

            let value = if operand1 < operand2 then 1 else 0
            let target = getValue (cpu.Address + 3) 1 cpu.RelativeBase memory

            let nextAddress = cpu.Address + 4
            let nextMemory = setValue target value memory
            let nextInstruction = getInstruction nextAddress nextMemory
            {
                CPU.State = Execute;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = cpu.RelativeBase;
                CPU.Memory = nextMemory;
                CPU.Data = None;
            }
        |  8 ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1]; memory.[cpu.Address + 2]; memory.[cpu.Address + 3]; ]

            let operand1 = getValue (cpu.Address + 1) instruction.Mode1 cpu.RelativeBase memory
            let operand2 = getValue (cpu.Address + 2) instruction.Mode2 cpu.RelativeBase memory

            let value = if operand1 = operand2 then 1 else 0
            let target = getValue (cpu.Address + 3) 1 cpu.RelativeBase memory
            
            let nextAddress = cpu.Address + 4
            let nextMemory = setValue target value memory
            let nextInstruction = getInstruction nextAddress nextMemory
            {
                CPU.State = Execute;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = cpu.RelativeBase;
                CPU.Memory = nextMemory;
                CPU.Data = None;
            }
        |  9 ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1]; ]
            
            let nextAddress = cpu.Address + 2
            let nextRelativeBase = cpu.RelativeBase + memory.[cpu.Address + 1]
            let nextInstruction = getInstruction nextAddress memory
            {
                CPU.State = Execute;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = nextRelativeBase;
                CPU.Memory = memory;
                CPU.Data = None;
            }
        | 99 ->
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