open System
open System.IO

type Instruction = {
    From: int
    To: int
    Count: int
}

type Stacks = char list array

let parseInstruction (l: string) =
    let s = l.Replace("from ", "").Replace("move ", "").Replace("to ", "")
    let nums = s.Split(" ")
    { From = int nums[1]; To = int nums[2]; Count = int nums[0]}

// Why do regex when you can have 20 lines of strange list operations
let parseStacks (lines: string list) = 
    let chuncks = 
        lines 
        |> List.rev
        |> List.map  (Seq.chunkBySize 4)
        |> List.map (Seq.map String.Concat)
    let size = 
        chuncks 
        |> List.head 
        |> Seq.rev 
        |> Seq.head
        |> int
    let stacks: Stacks = Array.create (size + 1) []
    let rows = 
        chuncks
        |> List.tail
        |> List.iter (Seq.iteri (fun idx str -> 
            if not <| String.IsNullOrWhiteSpace str
            then 
                stacks[idx + 1] <- Seq.item 1 str :: stacks[idx + 1]
        ))
    stacks

let readFile () = 
    let lines = 
        File.ReadLines "input.txt"
        |> Seq.toList
    let stacks = 
        lines
        |> List.takeWhile  (fun l -> l <> "")
        |> parseStacks
    let instructions = 
        lines
        |> List.skipWhile (fun l -> l <> "")
        |> List.tail
        |> List.map parseInstruction
    (stacks, instructions)

let rec crateMover9000 (stacks: Stacks) (instruction: Instruction) = 
    match instruction.Count with
    | 0 -> stacks
    | x -> 
        let toList = stacks[instruction.To]
        let fromList = stacks[instruction.From]
        let newToList = List.head fromList :: toList
        let newFromList = List.tail fromList
        stacks[instruction.To] <- newToList
        stacks[instruction.From] <- newFromList
        crateMover9000 stacks { instruction with Count = x - 1}

let crateMover9001 (stacks: Stacks) (instruction: Instruction) =
    let toList = stacks[instruction.To]
    let fromList = stacks[instruction.From]
    let newToList = List.take (instruction.Count) fromList @ toList
    let newFromList = List.skip (instruction.Count) fromList
    stacks[instruction.To] <- newToList
    stacks[instruction.From] <- newFromList
    stacks

let rec move crateMover stacks instructions = 
    match instructions with
    | [] -> stacks
    | x :: xs ->  move crateMover (crateMover stacks x) xs

let solve crateMover = 
    let (stacks, instructions) = readFile ()
    let stacks = move crateMover stacks instructions
    [1 .. stacks.Length - 1] 
    |> List.map (fun i -> List.head stacks[i])
    |> String.Concat

let task1 = solve crateMover9000 
let task2 = solve crateMover9001

printfn $"Task 1: {task1}" // MQTPGLLDN
printfn $"Task 2: {task2}" // LVZPSTTCZ
