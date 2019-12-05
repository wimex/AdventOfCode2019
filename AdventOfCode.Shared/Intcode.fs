namespace AdventOfCode.Shared

open System

module Intcode =
    [<Literal>]
    let debugmode = false

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

    let printDiagnostics parameters =
        if debugmode then
            printf "# "
            for item in parameters do
                printf "%d " item
            printfn ""

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
            printDiagnostics [ opcodes.[instruction.Address]; opcodes.[instruction.Address + 1]; opcodes.[instruction.Address + 2]; opcodes.[instruction.Address + 3]; ]

            let operand1 = getValue (instruction.Address + 1) instruction.Mode1 opcodes
            let operand2 = getValue (instruction.Address + 2) instruction.Mode2 opcodes

            let value = operand1 + operand2
            let target = opcodes.[instruction.Address + 3]

            let next = setValue target value opcodes
            (instruction.Address + 4, next)
        |  2 ->
            printDiagnostics [ opcodes.[instruction.Address]; opcodes.[instruction.Address + 1]; opcodes.[instruction.Address + 2]; opcodes.[instruction.Address + 3]; ]

            let operand1 = getValue (instruction.Address + 1) instruction.Mode1 opcodes
            let operand2 = getValue (instruction.Address + 2) instruction.Mode2 opcodes

            let value = operand1 * operand2
            let target = opcodes.[instruction.Address + 3]

            let next = setValue target value opcodes
            (instruction.Address + 4, next)
        |  3 ->
            printDiagnostics [ opcodes.[instruction.Address]; opcodes.[instruction.Address + 1]; ]

            printf "? "

            let input = Console.ReadLine();
            let (parsed, value) = Int32.TryParse(input)
            
            if parsed then
                let next = setValue opcodes.[instruction.Address + 1] value opcodes
                (instruction.Address + 2, next)
            else
                (instruction.Address, opcodes)
        |  4 ->
            printDiagnostics [ opcodes.[instruction.Address]; opcodes.[instruction.Address + 1]; ]

            printfn "! %d" (getValue (instruction.Address + 1) instruction.Mode1 opcodes)
            (instruction.Address + 2, opcodes)
        |  5 ->
            printDiagnostics [ opcodes.[instruction.Address]; opcodes.[instruction.Address + 1]; opcodes.[instruction.Address + 2]; ]

            let operand1 = getValue (instruction.Address + 1) instruction.Mode1 opcodes
            let operand2 = getValue (instruction.Address + 2) instruction.Mode2 opcodes

            let value = if operand1 <> 0 then operand2 else instruction.Address + 3
            (value, opcodes)
        |  6 ->
            printDiagnostics [ opcodes.[instruction.Address]; opcodes.[instruction.Address + 1]; opcodes.[instruction.Address + 2]; ]

            let operand1 = getValue (instruction.Address + 1) instruction.Mode1 opcodes
            let operand2 = getValue (instruction.Address + 2) instruction.Mode2 opcodes

            let value = if operand1 = 0 then operand2 else instruction.Address + 3
            (value, opcodes)
        |  7 ->
            printDiagnostics [ opcodes.[instruction.Address]; opcodes.[instruction.Address + 1]; opcodes.[instruction.Address + 2]; opcodes.[instruction.Address + 3]; ]

            let operand1 = getValue (instruction.Address + 1) instruction.Mode1 opcodes
            let operand2 = getValue (instruction.Address + 2) instruction.Mode2 opcodes

            let value = if operand1 < operand2 then 1 else 0
            let target = getValue (instruction.Address + 3) 1 opcodes

            let next = setValue target value opcodes
            (instruction.Address + 4, next)
        |  8 ->
            printDiagnostics [ opcodes.[instruction.Address]; opcodes.[instruction.Address + 1]; opcodes.[instruction.Address + 2]; opcodes.[instruction.Address + 3]; ]

            let operand1 = getValue (instruction.Address + 1) instruction.Mode1 opcodes
            let operand2 = getValue (instruction.Address + 2) instruction.Mode2 opcodes

            let value = if operand1 = operand2 then 1 else 0
            let target = getValue (instruction.Address + 3) 1 opcodes

            let next = setValue target value opcodes
            (instruction.Address + 4, next)
        | 99 -> (-1, opcodes)
        |  _ -> raise(Exception("Unknown instruction"))

    let rec execute address opcodes =
        let instruction = getInstruction address opcodes
        let (na, no) = parseInstruction instruction opcodes

        if na < 0 then
            no
        else
            execute na no