module Tests

open System
open Xunit
open AdventOfCode.Shared
open AdventOfCode.Shared.Intcode

[<Fact>]
let ``My test`` () =
    let memory = [103L; 5L; 99L] |> List.indexed |> List.map(fun (i,j) -> (int64(i), int64(j))) |> Map.ofSeq
    let data = 1234L
    
    let cpu = {
        CPU.State = Boot;
        CPU.Instruction = Instruction.Default;
        CPU.Address = 0L;
        CPU.RelativeBase = 0L;
        CPU.Memory = memory;
        Data = None; 
    }
    let input = Intcode.execute cpu
       
    let write = {
        CPU.State = input.State;
        CPU.Instruction = input.Instruction;
        CPU.Address = input.Address;
        CPU.RelativeBase = input.RelativeBase;
        CPU.Memory = input.Memory;
        Data = Some(data); 
    }
    let result = Intcode.execute write

    Assert.Equal(data, result.Memory.[5L])
