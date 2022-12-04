
open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let parseLine (l: string) = 
    let pairs = l.Split(",")
    let first = pairs[0].Split("-")
    let second = pairs[1].Split("-")
    Set([int first[0] .. int first[1]]), Set([int second[0] .. int second[1]])

let task1 = 
    readFile()
    |> List.map parseLine
    |> List.filter (fun (a, b) -> 
        let union  = Set.union a b
        a = union || b = union
    )
    |> List.length

let task2 = 
    readFile()
    |> List.map parseLine
    |> List.filter (fun (a,b) ->
        let intersection = Set.intersect a b
        not <| Set.isEmpty intersection
    )
    |> List.length

printfn $"Task 1: {task1}" // 567
printfn $"Task 2: {task2}" // 907
