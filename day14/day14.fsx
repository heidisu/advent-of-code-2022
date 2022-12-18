
open System.IO

let parse (line: string) = 
    line.Split(" -> ")
    |> Array.toList
    |> List.map (fun s -> 
        let parts = s.Split(",")
        (int <| parts[0], int <| parts[1])
        )

let readFile () = 
    File.ReadLines "test-input.txt"
    |> Seq.toList
    |> List.map parse

let task1 =
    let tuples = readFile()
    let maxHeight = 
        tuples
        |> List.concat
        |> List.map snd
        |> List.max
    let maxWidth = 
        tuples
        |> List.concat
        |> List.map fst
        |> List.max
    let arr = Array2D.create maxHeight maxWidth
    (maxWidth, maxHeight)

let task2 = "task 2"

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
