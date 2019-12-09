namespace AdventOfCode.Day08

open System

module Puzzles =
    [<Literal>]
    let width = 25

    [<Literal>]
    let height = 6

    let rec calculator layer zeros ones twos =
        if List.length layer = 0 then (zeros, ones, twos)
        else
            match layer.Head with
            | 0 -> calculator (List.skip 1 layer) (zeros + 1) ones twos
            | 1 -> calculator (List.skip 1 layer) zeros (ones + 1) twos
            | 2 -> calculator (List.skip 1 layer) zeros ones (twos + 1)
            | _ -> raise(new Exception("Invalid layer value"))

    let rec checksum pixels size result =
        if List.length pixels = 0 then result
        else
            let (nz, no, nt) = calculator (List.take size pixels) 0 0 0
            let (pz, po, pt) = result

            if nz < pz then checksum (List.skip size pixels) size (nz, no, nt)
            else checksum (List.skip size pixels) size (pz, po, pt)

   
    let rec getLayers pixels size offset =
        if size * offset >= List.length pixels then List.Empty
        else
            let list = List.ofSeq pixels
            let map = Array2D.init width height (fun x y -> list.[size * offset + x + y * width])
            map :: getLayers pixels size (offset + 1)

    let doBlend pixel1 pixel2 =
        match pixel1 with
        | 0 -> pixel1
        | 1 -> pixel1
        | 2 -> pixel2
        | _ -> raise(new Exception("Unknown pixel value"))

    let rec blendLayers (result: int[,]) (layers: List<int[,]>) =
        if List.length layers = 0 then result
        else
            let layer = layers.Head
            let blend = Array2D.init width height (fun x y -> doBlend (result.[x,y]) (layer.[x,y]))

            blendLayers blend (List.skip 1 layers)

    let puzzles filename = 
        let line = System.IO.File.ReadLines filename |> Seq.head |> Seq.map(fun item -> Int32.Parse(item.ToString()))
        let pixels = List.ofSeq line
        let size = width * height

        let (zeros, ones, twos) = checksum pixels size (Int32.MaxValue, Int32.MaxValue, Int32.MaxValue)

        printfn "Checksum:"
        printfn "%d, %d, %d -> %d" zeros ones twos (ones * twos)
        printfn ""

        let layers = getLayers pixels size 0
        let blend = blendLayers (Array2D.init width height (fun x y -> 2)) layers

        printfn "Message:"
        for r = 0 to (Array2D.length2 blend) - 1 do
            for c = 0 to (Array2D.length1 blend) - 1 do
                if blend.[c,r] = 0 then printf " " else
                if blend.[c,r] = 1 then printf "*" else
                printf " "
            
            printfn ""