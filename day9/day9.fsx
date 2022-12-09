
open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

type Instruction = Down | Up | Right | Left

let parse (line: string) = 
    let splits = line.Split(" ")
    match splits[0] with
    | "R" -> (Right, int splits[1])
    | "L" -> (Left, int splits[1])
    | "D" -> (Down, int splits[1])
    | "U" -> (Up, int splits[1])

let moveHead (dir: Instruction) ((x, y): int * int): int * int = 
    match dir with
    | Right -> (x + 1 , y)
    | Left ->  (x - 1, y)
    | Up -> (x, y + 1)
    | Down -> (x, y - 1)

let moveTail ((x, y) : int * int) ((a, b): int * int) = 
    if x = a && y - b = 2 then (x, y - 1)
    else if x = a && b - y = 2 then (x, y + 1)
    else if y = b && x - a = 2 then (x - 1, y)
    else if y = b && a - x = 2 then (x + 1, y)
    else if x + 1 = a && y + 2 = b then (x + 1, y + 1)
    else if x + 1 = a && y - 2 = b then (x + 1, y - 1)
    else if x + 2 = a && y + 1 = b then (x + 1, y + 1)
    else if x + 2 = a && y - 1 = b then (x + 1, y - 1)
    else if x - 1 = a && y + 2 = b then (x - 1, y + 1)
    else if x - 1 = a && y - 2 = b then (x - 1, y - 1)
    else if x - 2 = a && y + 1 = b then (x - 1, y + 1)
    else if x - 2 = a && y - 1 = b then (x - 1, y - 1)
    else if x + 2 = a && y + 2 = b then (x + 1, y + 1)
    else if x + 2 = a && y - 2 = b then (x + 1, y - 1)
    else if x - 2 = a && y + 2 = b then (x - 1, y + 1)
    else if x - 2 = a && y - 2 = b then (x - 1, y - 1)
    else (x, y)


let rec move (visited: (int * int) list) (currHead: int * int) (currTail: int * int) ((dir, step): Instruction * int) =
    if step = 0 then (visited, currHead, currTail)
    else
        let newHead = moveHead dir currHead
        let newTail = moveTail currTail newHead
        move (newTail :: visited) newHead  newTail (dir, step - 1)

let rec moveMany (visited: (int * int) list) (currHead: int * int) (knots: (int * int) list) ((dir, step): Instruction * int) =
    if step = 0 then (visited, currHead, knots)
    else
        let newHead = moveHead dir currHead
        let newKnots = 
            knots
            |> List.fold (fun (prev, acc) knot -> 
                let newKnot = moveTail knot prev in
                (newKnot, newKnot :: acc)
                ) (newHead, [])
            |> snd
            |> List.rev
        moveMany ((List.last newKnots) :: visited) newHead  newKnots (dir, step - 1)

let task1 =
    readFile ()
    |> List.map parse
    |> List.fold (fun (visited, head, tail) instr -> move visited head tail instr) ([], (0,0), (0,0))
    |> (fun (a, b, c) -> a)
    |> List.distinct
    |> List.length

let task2 =
    readFile ()
    |> List.map parse
    |> List.fold (fun (visited, head, knots) instr -> 
        moveMany visited head knots instr) ([], (0,0), List.replicate 9 (0,0))
    |> (fun (a, b, c) -> a)
    |> List.distinct
    |> List.length

printfn $"Task 1: {task1}" // 6067
printfn $"Task 2: {task2}" // 2471
