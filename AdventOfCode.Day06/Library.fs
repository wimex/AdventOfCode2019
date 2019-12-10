namespace AdventOfCode.Day06

open System

module Puzzles =
    type Tree =
    | Leaf of string
    | Node of string * List<Tree>

    type Node =
        {
            Name: string
            Parent: string option
            Children: List<Node>
        }

    let rec findNode nodes name =
        if List.length nodes = 0 then None
        else
            let node = List.head nodes
            if node.Name = name then Some(node)
            else
                findNode (List.append nodes node.Children) name

    //let rec createTree lines root =
    //    match (lines, root) with
    //    | ([], root) -> root
    //    | ()

    //    if List.length lines = 0 then root
    //    else
    //        let (left, right) = List.head lines

    //        let oldparent = findNode root left
    //        let oldchild = findNode root right

    //        let test=Node ("hello", List.Empty)
            
    //        ""
        //    let (left, right) = List.head lines
        //    let oldparent = findNode root left
        //    let oldchild = findNode root right
            
        //    let newchild = if oldchild.IsSome then { Node.n}
        //    let newparent = if oldparent.IsSome then { Node.Name = oldparent.Value.Name; Node.Parent = oldparent.Value.Parent; Node.Children = List.append oldparent.Value.Children [newchild] } else { Node.Name = left; Node.Parent = None; Node.Children = [newchild] }

        //    let oldparent = if Map.containsKey left cache then cache.[left] else { Node.Name = left; Node.Parent = None; Node.Children = List.Empty }
        //    let oldchild = if Map.containsKey right cache then cache.[right] else { Node.Name = right; Node.Parent = None; Node.Children = List.Empty }
            
        //    let newchild = { Node.Name = oldchild.Name; Node.Parent = Some(left); Node.Children = oldchild.Children }
        //    let newparent = { Node.Name = oldparent.Name; Node.Parent = oldparent.Parent; Node.Children = List.append oldparent.Children [newchild] }

        //    let map1 = cache.Add (newparent.Name, newparent)
        //    let map2 = map1.Add (newchild.Name, newchild)

        //    createTree (List.skip 1 lines) map2

    let rec walk node length =
        if List.length node.Children = 0 then length
        else
            length + (walk (List.head node.Children) length + 1)

    let puzzles filename = 
        //let lines = System.IO.File.ReadAllLines filename |> List.ofSeq |> List.map (fun item -> (item |> string).Split(")")) |> List.map (fun item -> (item.[0], item.[1]))
        //let tree = createTree lines
        //let tree = createTree lines Map.empty
        //let root = tree |> Map.filter (fun key value -> value.Parent.IsNone) |> Map.toList

        //if Seq.length root <> 1 then raise(new Exception("Multiple roots"))
        
        //let (_, node) = root.Head
        //let length = walk node 0
        //printfn "Tree length: %d" length
        ""