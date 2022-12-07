
open System.IO
open System

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

type FolderContent = Dir of string | File of string * int

let appendPath parent child = 
    if parent = "/" then parent + child 
    else parent + "/" + child

let parse currDir map (instr: string) = 
    if instr.StartsWith("$ cd ") 
    then 
        let dir = instr.Replace("$ cd ", "")
        match dir with
        |"/" -> (dir, map)
        | ".." -> 
            let dirUp = 
                currDir
                |> Seq.rev
                |> Seq.skipWhile (fun c -> c <> '/')
                |> Seq.tail
                |> Seq.rev
                |> Seq.toList
                |> String.Concat
            (dirUp, map)
        | d -> 
            (appendPath currDir d, map)
    else if instr.StartsWith("$ ls") then (currDir, map)
    else 
        let splits = instr.Split(" ")
        let fileContent = 
            if splits[0] = "dir" then Dir splits[1]
            else File (splits[1], int splits[0])
        let updatedMap = 
            map 
            |> Map.change currDir (
                fun v ->  
                    match v with
                    | Some l -> fileContent :: l |> Some
                    | None -> Some [fileContent]
                )
        (currDir, updatedMap)

let rec sumDir map dir =
    Map.find dir map
    |> List.fold (fun tot c -> 
            match c with
            | Dir d -> 
                tot + sumDir map (appendPath dir d)
            | File (n, size) -> tot + size
        ) 0

let directories (map: Map<string, FolderContent list>) = 
    map
    |> Map.keys  
    |> Seq.map (fun key -> (key, sumDir map key))
    |> Seq.toList 

let fileSystem = 
    readFile ()
    |> List.fold (
        fun (currDir, map) line ->
            parse currDir map line) 
        ("", Map.empty)
    |>snd
    |> directories
let task1 = 
    fileSystem
    |> List.filter (fun (n, s) -> s <= 100000)
    |> List.map snd
    |> List.sum

let rootSize =
    fileSystem
    |> List.find (fun (n, s) -> n = "/")
    |> snd
let unused  = 70000000 - rootSize
let task2 =
    fileSystem
    |> List.sortBy snd
    |> List.find (fun (n, s) -> s + unused >= 30000000)
    |> snd

printfn $"Task 1: {task1}" // 1749646
printfn $"Task 2: {task2}" // 1498966
