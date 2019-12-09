namespace AdventOfCode.Day07

open System
open AdventOfCode.Shared.Intcode

module Puzzles =
    let mutable globalMax = 0

    let rec permute list taken = 
        seq {
            if Set.count taken = List.length list then yield [] else
                for item in list do
                    if not (Set.contains item taken) then
                        for permutation in permute list (Set.add item taken) do
                            yield item::permutation
        }

    let rec feeder1 opcodes list value =
        if Seq.length list = 0 then value
        else
            let mutable inputs = [Seq.head list; value]
            let mutable output = 0

            let inputfn () =
                let head = Seq.head inputs
                inputs <- List.skip 1 inputs

                let send = head |> string
                send

            let outputfn (data) =
                output <- Int32.Parse data

            AdventOfCode.Shared.Intcode.execute 0 opcodes (Func<string> inputfn) (Action<string> outputfn) |> ignore
            feeder1 opcodes (List.skip 1 list) output

    let rec finder1 opcodes permutations max =
        if Seq.length permutations = 0 then max
        else
            let (input: List<int32>) = List.head permutations
            
            printfn "%d%d%d%d%d" input.[0] input.[1] input.[2] input.[3] input.[4]
            
            let value = feeder1 opcodes input 0
            let (_, cm) = max
            
            let nm = if value > cm then (input, value) else max

            printfn "---> %d" cm
            finder1 opcodes (List.skip 1 permutations) nm

    let rec feeder2x (memories: List<List<int32>>) position value =
        let inputFn () =
            value |> string

        let outputFn (data) =
            let outer = Int32.Parse(data)
            let npos = if position = 4 then 0 else position + 1

            globalMax <- outer
            feeder2x memories npos outer |> ignore

        AdventOfCode.Shared.Intcode.execute 0 memories.[position] (Func<string> inputFn) (Action<string> outputFn) |> ignore
        globalMax

    let rec feeder2 opcodes (list: List<int32>) max =
        if Seq.length list = 0 then max
        else
            let mutable inputPos = 0
            globalMax <- 0

            let inputFn (data) =
                inputPos <- inputPos + 1
                list.[inputPos - 1] |> string

            let (_, memory1) = AdventOfCode.Shared.Intcode.execute 0 opcodes (Func<string> inputFn) null
            let (_, memory2) = AdventOfCode.Shared.Intcode.execute 0 opcodes (Func<string> inputFn) null
            let (_, memory3) = AdventOfCode.Shared.Intcode.execute 0 opcodes (Func<string> inputFn) null
            let (_, memory4) = AdventOfCode.Shared.Intcode.execute 0 opcodes (Func<string> inputFn) null
            let (_, memory5) = AdventOfCode.Shared.Intcode.execute 0 opcodes (Func<string> inputFn) null

            let value = feeder2x [memory1;memory2;memory3;memory4;memory5] 0 0
            let (_, cm) = max
            
            let nm = if value > cm then (list, value) else max
            feeder2 opcodes (List.skip 1 list) nm

    let rec finder2 opcodes permutations max =
        if Seq.length permutations = 0 then max
        else
            let (input: List<int32>) = List.head permutations
            
            printfn "%d%d%d%d%d" input.[0] input.[1] input.[2] input.[3] input.[4]
            feeder2 opcodes input ([], 0)

    let puzzle1 opcodes =
        let permutations = permute [0;1;2;3;4] Set.empty
        let (input, maximum) = finder1 opcodes (List.ofSeq permutations) ([], 0)

        printfn ""
        printfn "%d%d%d%d%d" input.[0] input.[1] input.[2] input.[3] input.[4]
        printfn "---> %d" maximum

    let puzzle2 opcodes =
        let permutations = permute [5;6;7;8;9] Set.empty
        let (input, maximum) = finder2 opcodes (List.ofSeq permutations) ([], 0)

        printfn ""
        printfn "%d%d%d%d%d" input.[0] input.[1] input.[2] input.[3] input.[4]
        printfn "---> %d" maximum

    let puzzles filename = 
        let line = System.IO.File.ReadAllText filename
        let opcodes = line.Split(",", System.StringSplitOptions.RemoveEmptyEntries) 
                        |> List.ofSeq 
                        |> List.map(Operators.int)

        //printfn "Puzzle 1"
        //puzzle1 opcodes
        //printfn ""

        printfn "Puzzle 2"
        puzzle2 opcodes
        printfn ""