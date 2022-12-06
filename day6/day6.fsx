open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let search messageSize input =
    input
    |> Seq.windowed messageSize
    |> Seq.indexed
    |> Seq.find (fun (i, chars) -> Set(chars).Count = messageSize)
    |> fst
    |> (fun idx -> idx + messageSize)

let task1 = 
    readFile()
    |> List.head 
    |> search 4
    
let task2 =
    readFile()
    |> List.head 
    |> search 14

printfn $"Task 1: {task1}" // 1300
printfn $"Task 2: {task2}" // 3986
