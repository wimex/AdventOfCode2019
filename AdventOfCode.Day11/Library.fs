namespace AdventOfCode.Day11

open System
open AdventOfCode.Shared.Intcode

module Puzzles =
    type Coordinate =
        {
            X: int64
            Y: int64
        }

    type Tile =
        {
            Paints: int64
            Color: int64
        }

    type Direction =
    | Up
    | Down
    | Left
    | Right

    let getPosition position direction turn =
        match (direction, turn) with
        | (Up, 0L)     -> ({Coordinate.X = position.X - 1L; Coordinate.Y = position.Y}, Left)
        | (Up, 1L)     -> ({Coordinate.X = position.X + 1L; Coordinate.Y = position.Y}, Right)
        | (Down, 0L)   -> ({Coordinate.X = position.X + 1L; Coordinate.Y = position.Y}, Right)
        | (Down, 1L)   -> ({Coordinate.X = position.X - 1L; Coordinate.Y = position.Y}, Left)
        | (Left, 0L)   -> ({Coordinate.X = position.X; Coordinate.Y = position.Y - 1L}, Down)
        | (Left, 1L)   -> ({Coordinate.X = position.X; Coordinate.Y = position.Y + 1L}, Up)
        | (Right, 0L)  -> ({Coordinate.X = position.X; Coordinate.Y = position.Y + 1L}, Up)
        | (Right, 1L)  -> ({Coordinate.X = position.X; Coordinate.Y = position.Y - 1L}, Down)
        | (_, _)       -> raise(new Exception("Unable to handle turn"))

    let rec puzzle cpu (map: Map<Coordinate, Tile>) direction position =
        match cpu.State with
        | Halt  -> map
        | Input ->
            let tile = if map.ContainsKey position then map.[position] else { Tile.Paints = 0L; Tile.Color = 0L }
            let ncpu = { CPU.State = cpu.State; CPU.Instruction = cpu.Instruction; CPU.Address = cpu.Address;CPU.RelativeBase = cpu.RelativeBase; CPU.Memory = cpu.Memory; CPU.Data = Some(tile.Color)}
            let state = AdventOfCode.Shared.Intcode.execute ncpu
            puzzle state map direction position
            
        | Output ->
            let tile = if map.ContainsKey position then map.[position] else { Tile.Paints = 0L; Tile.Color = 0L }
            let color = cpu.Data.Value
            let state1 = AdventOfCode.Shared.Intcode.execute cpu
            let turn = state1.Data.Value

            let nmap = map.Add(position, {Tile.Paints = tile.Paints + 1L; Tile.Color = color})
            let (npos, ndir) = getPosition position direction turn

            let state2 = AdventOfCode.Shared.Intcode.execute state1
            puzzle state2 nmap ndir npos
        | _      ->
            let state = AdventOfCode.Shared.Intcode.execute cpu
            puzzle state map direction position

    let puzzles filename =
        let opcodes = AdventOfCode.Shared.Intcode.read filename
        let cpu =  { CPU.State = Boot; CPU.Instruction = Instruction.Default; CPU.Address = 0L; CPU.RelativeBase = 0L; CPU.Memory = opcodes; CPU.Data = None }
        
        let spos = { Coordinate.X = 0L; Coordinate.Y = 0L }
        let smap1 = Map.empty.Add(spos, {Tile.Paints = 0L; Tile.Color = 0L})
        let smap2 = Map.empty.Add(spos, {Tile.Paints = 0L; Tile.Color = 1L})

        let map1 = puzzle cpu smap1 Up spos
        let map2 = puzzle cpu smap2 Up spos

        Console.Clear()
        let count = map1 |> Map.toList |> List.where (fun (_, value) -> value.Paints > 0L)
        let mutable maxx = 0
        let mutable maxy = 0

        for item in map2 |> Map.toList do
            let (key, value) = item
            
            let x = (key.X) |> int32
            let y = (12L - key.Y) |> int32

            if x > maxx then
                maxx <- x
            if y > maxy then
                maxy <- y

            Console.SetCursorPosition(x, y)

            if(value.Color = 1L) then
                Console.Write "*"
            else
                Console.Write " "
                       

        Console.SetCursorPosition(0, maxy + 1)
        printfn "Number of tiles: %d" count.Length