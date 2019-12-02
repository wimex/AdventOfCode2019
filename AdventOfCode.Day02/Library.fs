namespace AdventOfCode.Day02

module Puzzles =
    let update list key value = 
        list |> List.mapi (fun i v -> if i = key then value else v)

    let rec parser pointer (opcodes : _ list) =
        let opcode = opcodes.[pointer]
        let position1= opcodes.[pointer + 1]
        let position2 = opcodes.[pointer + 2]
        let target = opcodes.[pointer + 3]

        if position1 > opcodes.Length || position2 > opcodes.Length then
            opcodes
        else
            match opcode with
            | 1 -> 
                    let value = opcodes.[position1] + opcodes.[position2]
                    let next = update opcodes target value

                    parser (pointer + 4) next

            | 2 ->
                    let value = opcodes.[position1] * opcodes.[position2]
                    let next = update opcodes target value

                    parser (pointer + 4) next
            | 99 -> 
                    opcodes
            | _ -> 
                    raise(System.Exception("Invalid value"))

    let intcode noun verb opcodes =
        let step1 = update opcodes 1 noun
        let step2 = update step1 2 verb

        let result = parser 0 step2

        result

    let rec finder noun verb target opcodes =
        let result = intcode noun verb opcodes

        let nextNoun = if verb = opcodes.Length - 1 then noun + 1 else noun
        let nextVerb = if verb = opcodes.Length - 1 then 0 else verb + 1
        
        if result.[0] = target then
            (noun, verb, opcodes)
        else
            finder nextNoun nextVerb target opcodes
        

    let puzzle1 opcodes = 
        let result = intcode 12 2 opcodes
        printfn "Final element in position 0: %d" result.[0]

    let puzzle2 opcodes =
        let (noun, verb, _) = finder 0 0 19690720 opcodes
        let summary = (100*noun + verb)

        printfn "Output found with noun / verb %d / %d -> %d" noun verb summary

    let puzzles filename = 
        let line = System.IO.File.ReadAllText filename
        let opcodes = line.Split(",", System.StringSplitOptions.RemoveEmptyEntries) 
                        |> List.ofSeq 
                        |> List.map(Operators.int)

        puzzle1 opcodes
        puzzle2 opcodes