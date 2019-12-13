namespace AdventOfCode.Shared

open System

module Intcode =
    [<Literal>]
    let debugmode = false

    type Instruction = 
        {
            Opcode: int64
            Operand1: int64
            Mode1: int64

            Operand2: int64
            Mode2: int64

            Operand3: int64
            Mode3: int64

            Length: int64
        }
    with
        static member Default = { Opcode = 0L; Operand1 = 0L; Mode1 = 0L; Operand2 = 0L; Mode2 = 0L; Operand3 = 0L; Mode3 = 0L; Length = 0L }

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

    let getValue (opcodes: Map<int64, int64>) rbase mode operand =
        match mode with
        | 0L -> if opcodes.ContainsKey operand then opcodes.[operand] else 0L
        | 1L -> operand
        | 2L -> if opcodes.ContainsKey (rbase + operand) then (opcodes.[rbase + operand]) else 0L
        |  _ -> raise(Exception("Unable to read in unknown addressing mode"))

    let setValue (opcodes: Map<int64, int64>) rbase mode operand value =
        match mode with
        | 0L -> poke opcodes operand value
        | 2L -> poke opcodes (rbase + operand) value
        |  _ -> raise(Exception("Unable to write in unknown addressing mode"))

    let getInstructionLength instruction =
        match instruction with
        |  1L -> 4L
        |  2L -> 4L
        |  3L -> 2L
        |  4L -> 2L
        |  5L -> 3L
        |  6L -> 3L
        |  7L -> 4L
        |  8L -> 4L
        |  9L -> 2L
        | 99L -> 1L
        |   _ -> raise(Exception("Unknown instruction"))

    let getInstruction address (opcodes: Map<int64, int64>) =
        let opcode = opcodes.[address]
        let digits = getDigits opcode

        let instruction = if digits.Length >= 2 then 10L * digits.[digits.Length - 2] + digits.[digits.Length - 1] else digits.[digits.Length - 1]
        let mode1 = if digits.Length >= 3 then digits.[digits.Length - 3] else 0L
        let mode2 = if digits.Length >= 4 then digits.[digits.Length - 4] else 0L
        let mode3 = if digits.Length >= 5 then digits.[digits.Length - 5] else 0L

        let operand1 = if opcodes.ContainsKey (address + 1L) then opcodes.[address + 1L] else 0L
        let operand2 = if opcodes.ContainsKey (address + 2L) then opcodes.[address + 2L] else 0L
        let operand3 = if opcodes.ContainsKey (address + 3L) then opcodes.[address + 3L] else 0L

        let length = getInstructionLength instruction

        {
            Instruction.Opcode = instruction;
            Instruction.Operand1 = operand1;
            Instruction.Mode1 = mode1;

            Instruction.Operand2 = operand2;
            Instruction.Mode2 = mode2;

            Instruction.Operand3 = operand3;
            Instruction.Mode3 = mode3;

            Instruction.Length = length
         }

    let parseInstruction cpu  =
        //printfn ". %s %d %d %d" (instruction.Opcode.ToString().PadLeft(5, '0')) instruction.Mode1 instruction.Mode2 instruction.Mode3
        let instruction = cpu.Instruction
        let memory = cpu.Memory

        match instruction.Opcode with
        |  1L ->
            printDiagnostics [ memory.[cpu.Address]; memory.[cpu.Address + 1L]; memory.[cpu.Address + 2L]; memory.[cpu.Address + 3L]; ]

            let value1 = getValue memory cpu.RelativeBase instruction.Mode1 instruction.Operand1
            let value2 = getValue memory cpu.RelativeBase instruction.Mode2 instruction.Operand2
            let result = value1 + value2
            
            let nextMemory = setValue memory cpu.RelativeBase instruction.Mode3 instruction.Operand3 result
            let nextAddress = cpu.Address + instruction.Length
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
            
            let value1 = getValue memory cpu.RelativeBase instruction.Mode1 instruction.Operand1
            let value2 = getValue memory cpu.RelativeBase instruction.Mode2 instruction.Operand2
            let result = value1 * value2
            
            let nextMemory = setValue memory cpu.RelativeBase instruction.Mode3 instruction.Operand3 result
            let nextAddress = cpu.Address + instruction.Length
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
                
                let nextMemory = setValue memory cpu.RelativeBase instruction.Mode1 instruction.Operand1 cpu.Data.Value
                let nextAddress = cpu.Address + instruction.Length
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

            let output = getValue memory cpu.RelativeBase instruction.Mode1 instruction.Operand1
            printfn "! %d" output

            let nextAddress = cpu.Address + instruction.Length
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

            let value1 = getValue memory cpu.RelativeBase instruction.Mode1 instruction.Operand1
            let value2 = getValue memory cpu.RelativeBase instruction.Mode2 instruction.Operand2

            let nextAddress = if value1 <> 0L then value2 else cpu.Address + instruction.Length
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
            
            let value1 = getValue memory cpu.RelativeBase instruction.Mode1 instruction.Operand1
            let value2 = getValue memory cpu.RelativeBase instruction.Mode2 instruction.Operand2

            let nextAddress = if value1 = 0L then value2 else cpu.Address + instruction.Length
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
            
            let value1 = getValue memory cpu.RelativeBase instruction.Mode1 instruction.Operand1
            let value2 = getValue memory cpu.RelativeBase instruction.Mode2 instruction.Operand2
            let result = if value1 < value2 then 1L else 0L

            let nextMemory = setValue memory cpu.RelativeBase instruction.Mode3 instruction.Operand3 result
            let nextAddress = cpu.Address + instruction.Length
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
            
            let value1 = getValue memory cpu.RelativeBase instruction.Mode1 instruction.Operand1
            let value2 = getValue memory cpu.RelativeBase instruction.Mode2 instruction.Operand2
            let result = if value1 = value2 then 1L else 0L
            
            let nextMemory = setValue memory cpu.RelativeBase instruction.Mode3 instruction.Operand3 result
            let nextAddress = cpu.Address + instruction.Length
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

            let modifier = getValue memory cpu.RelativeBase instruction.Mode1 instruction.Operand1
            let result = cpu.RelativeBase + modifier
            
            let nextAddress = cpu.Address + instruction.Length
            let nextInstruction = getInstruction nextAddress memory
            {
                CPU.State = Execute;
                CPU.Instruction = nextInstruction;
                CPU.Address = nextAddress;
                CPU.RelativeBase = result;
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

    let read filename =
        let line = System.IO.File.ReadAllText filename
        let opcodes = line.Split(",", System.StringSplitOptions.RemoveEmptyEntries) 
                        |> List.ofSeq 
                        |> List.indexed
                        |> List.map(fun (i,j) -> (int64(i), int64(j)))
                        |> Map.ofSeq

        opcodes