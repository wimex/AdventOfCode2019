namespace AdventOfCode.Day03

module Puzzles =
    let (|Direction|_|) (prefix: string) (input: string) = 
        if input.StartsWith(prefix) then
            Some(input.Substring(prefix.Length) |> Operators.int32)
        else
            None
        
    let rec setRow target operator x y length =
        if length < 0 then
            target
        else
            Array2D.set target x y "+"
            setRow target (x operator 1) y (length operator 1)

    let rec drawLine target x y line =
        let head = Seq.head line
        
        match head with
        | Direction "L" length ->
            let na = setRow target  x y length
            let nx = x - length
            let ny = y
            let nl = Seq.skip 1 line

            drawLine na nx ny nl
        | Direction "R" length ->
            let source = Array2D.init length length (fun i j -> "+") 

            Array2D.blit source 0 0 target x y length length
            drawLine target (x+length) y line
        | Direction "U" length -> 
            target
        | Direction "D" length -> 
            target
        | _ ->
            target

    let drawMap line = 
        let matrix = Array2D.init 10000 10000 (fun x y -> ".")
        drawLine matrix 5000 5000 line

    let puzzles filename =
        let lines = System.IO.File.ReadAllLines filename
        let line1 = lines.[0].Split(",", System.StringSplitOptions.RemoveEmptyEntries) |> List.ofSeq
        let line2 = lines.[1].Split(",", System.StringSplitOptions.RemoveEmptyEntries) |> List.ofSeq

        let map1 = drawMap line1
        let map2 = drawMap line2
        
        for r = 0 to Array2D.length1 map1 - 1 do
         for c = 0 to Array2D.length2 map1 - 1 do
          printfn "%A " map1.[r, c]
          
        for r = 0 to Array2D.length1 map2 - 1 do
         for c = 0 to Array2D.length2 map2 - 1 do
          printfn "%A " map2.[r, c]
