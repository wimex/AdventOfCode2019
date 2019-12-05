namespace AdventOfCode.Intcode

open System

module Computer =
    type Instruction = 
        {
            Opcode: int32
            Address: int32
            Mode1: int32
            Mode2: int32
            Mode3: int32
        }

    let poke list address value =
        list |> List.mapi (fun i v -> if i = address then value else v)
    
    let rec getDigits number =
        if number <> 0 then
            let modulo = number % 10
            getDigits((number - modulo) / 10) @ [modulo]
        else
            List.Empty

    let getInstruction address (opcodes: List<int32>) =
        let opcode = opcodes.[address]
        let digits = getDigits opcode

        let instruction = if digits.Length >= 2 then 10 * digits.[digits.Length - 2] + digits.[digits.Length - 1] else digits.[digits.Length - 1]
        let mode1 = if digits.Length >= 3 then digits.[digits.Length - 3] else 0
        let mode2 = if digits.Length >= 4 then digits.[digits.Length - 4] else 0
        let mode3 = if digits.Length >= 5 then digits.[digits.Length - 5] else 0

        {
            Instruction.Opcode = instruction;
            Instruction.Address = address;
            Instruction.Mode1 = mode1;
            Instruction.Mode2 = mode2;
            Instruction.Mode3 = mode3;
         }

    let getValue address mode (opcodes: List<int32>) =
        match mode with
        | 0 -> opcodes.[opcodes.[address]]
        | 1 -> opcodes.[address]
        | _ -> raise(Exception("Unknown addressing mode"))

    let setValue address value (opcodes: List<int32>) = 
        poke opcodes address value

    let parseInstruction instruction (opcodes: List<int32>) =
        //printfn ". %s %d %d %d" (instruction.Opcode.ToString().PadLeft(5, '0')) instruction.Mode1 instruction.Mode2 instruction.Mode3

        match instruction.Opcode with
        |  1 ->
            printfn "# %d %d %d %d" opcodes.[instruction.Address] opcodes.[instruction.Address + 1] opcodes.[instruction.Address + 2] opcodes.[instruction.Address + 3]

            let operand1 = getValue (instruction.Address + 1) instruction.Mode1 opcodes
            let operand2 = getValue (instruction.Address + 2) instruction.Mode2 opcodes

            let value = operand1 + operand2
            let target = opcodes.[instruction.Address + 3]

            let next = setValue target value opcodes
            (instruction.Address + 4, next)
        |  2 ->
            printfn "# %d %d %d %d" opcodes.[instruction.Address] opcodes.[instruction.Address + 1] opcodes.[instruction.Address + 2] opcodes.[instruction.Address + 3]

            let operand1 = getValue (instruction.Address + 1) instruction.Mode1 opcodes
            let operand2 = getValue (instruction.Address + 2) instruction.Mode2 opcodes

            let value = operand1 * operand2
            let target = opcodes.[instruction.Address + 3]

            let next = setValue target value opcodes
            (instruction.Address + 4, next)
        |  3 ->
            printfn "# %d %d" opcodes.[instruction.Address] opcodes.[instruction.Address + 1]

            printf "? "

            let input = Console.ReadLine();
            let (parsed, value) = Int32.TryParse(input)
            
            if parsed then
                let next = setValue opcodes.[instruction.Address + 1] value opcodes
                (instruction.Address + 2, next)
            else
                (instruction.Address, opcodes)
        |  4 ->
            printfn "# %d %d" opcodes.[instruction.Address] opcodes.[instruction.Address + 1]

            printfn "! %d" (getValue (instruction.Address + 1) instruction.Mode1 opcodes)
            (instruction.Address + 2, opcodes)
        |  5 ->
            printfn "# %d %d %d" opcodes.[instruction.Address] opcodes.[instruction.Address + 1] opcodes.[instruction.Address + 2]

            let operand1 = getValue (instruction.Address + 1) instruction.Mode1 opcodes
            let operand2 = getValue (instruction.Address + 2) instruction.Mode2 opcodes

            let value = if operand1 <> 0 then operand2 else instruction.Address + 3
            (value, opcodes)
        |  6 ->
            printfn "# %d %d %d" opcodes.[instruction.Address] opcodes.[instruction.Address + 1] opcodes.[instruction.Address + 2]

            let operand1 = getValue (instruction.Address + 1) instruction.Mode1 opcodes
            let operand2 = getValue (instruction.Address + 2) instruction.Mode2 opcodes

            let value = if operand1 = 0 then operand2 else instruction.Address + 3
            (value, opcodes)
        |  7 ->
            printfn "# %d %d %d %d" opcodes.[instruction.Address] opcodes.[instruction.Address + 1] opcodes.[instruction.Address + 2] opcodes.[instruction.Address + 3]

            let operand1 = getValue (instruction.Address + 1) instruction.Mode1 opcodes
            let operand2 = getValue (instruction.Address + 2) instruction.Mode2 opcodes

            let value = if operand1 < operand2 then 1 else 0
            let target = getValue (instruction.Address + 3) 1 opcodes

            let next = setValue target value opcodes
            (instruction.Address + 4, next)
        |  8 ->
            printfn "# %d %d %d %d" opcodes.[instruction.Address] opcodes.[instruction.Address + 1] opcodes.[instruction.Address + 2] opcodes.[instruction.Address + 3]

            let operand1 = getValue (instruction.Address + 1) instruction.Mode1 opcodes
            let operand2 = getValue (instruction.Address + 2) instruction.Mode2 opcodes

            let value = if operand1 = operand2 then 1 else 0
            let target = getValue (instruction.Address + 3) 1 opcodes

            let next = setValue target value opcodes
            (instruction.Address + 4, next)
        | 99 -> (-1, opcodes)
        |  _ -> raise(Exception("Unknown instruction"))

    let rec intcode address opcodes =
        let instruction = getInstruction address opcodes
        let (na, no) = parseInstruction instruction opcodes

        if na < 0 then
            no
        else
            intcode na no

    let execute =
        let program = "3,225,1,225,6,6,1100,1,238,225,104,0,1101,65,39,225,2,14,169,224,101,-2340,224,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,1001,144,70,224,101,-96,224,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,1101,92,65,225,1102,42,8,225,1002,61,84,224,101,-7728,224,224,4,224,102,8,223,223,1001,224,5,224,1,223,224,223,1102,67,73,224,1001,224,-4891,224,4,224,102,8,223,223,101,4,224,224,1,224,223,223,1102,54,12,225,102,67,114,224,101,-804,224,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1101,19,79,225,1101,62,26,225,101,57,139,224,1001,224,-76,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1102,60,47,225,1101,20,62,225,1101,47,44,224,1001,224,-91,224,4,224,1002,223,8,223,101,2,224,224,1,224,223,223,1,66,174,224,101,-70,224,224,4,224,102,8,223,223,1001,224,6,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,226,226,224,102,2,223,223,1005,224,329,101,1,223,223,1107,226,677,224,1002,223,2,223,1005,224,344,101,1,223,223,8,226,677,224,102,2,223,223,1006,224,359,101,1,223,223,108,677,677,224,1002,223,2,223,1005,224,374,1001,223,1,223,1108,226,677,224,1002,223,2,223,1005,224,389,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,404,1001,223,1,223,1108,677,677,224,102,2,223,223,1006,224,419,1001,223,1,223,1008,226,677,224,102,2,223,223,1005,224,434,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,449,1001,223,1,223,1007,226,677,224,102,2,223,223,1005,224,464,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,479,101,1,223,223,1007,226,226,224,102,2,223,223,1005,224,494,101,1,223,223,7,677,677,224,102,2,223,223,1006,224,509,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,524,1001,223,1,223,108,226,677,224,1002,223,2,223,1006,224,539,101,1,223,223,8,226,226,224,102,2,223,223,1006,224,554,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,569,1001,223,1,223,1108,677,226,224,1002,223,2,223,1006,224,584,101,1,223,223,1107,677,226,224,1002,223,2,223,1005,224,599,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,614,1001,223,1,223,7,226,677,224,102,2,223,223,1005,224,629,1001,223,1,223,107,677,226,224,1002,223,2,223,1005,224,644,1001,223,1,223,1107,677,677,224,102,2,223,223,1006,224,659,101,1,223,223,1008,226,226,224,1002,223,2,223,1006,224,674,1001,223,1,223,4,223,99,226"
        //let program = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
        let opcodes = program.Split(",", StringSplitOptions.RemoveEmptyEntries)
                        |> List.ofSeq
                        |> List.map(Operators.int32)

        intcode 0 opcodes
