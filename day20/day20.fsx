
open System.IO

let readFile () = 
    File.ReadLines "test-input.txt"
    |> Seq.toList
    |> List.map int
    |> List.toArray

let task1 =
    let numbers = readFile ()
    let size = Array.length numbers
    let first = 
        [1 .. 10]
        |> List.fold (fun (lst, idx) i -> 
            let number = numbers[idx]
            printfn "%A %A %A" lst idx number
            let listIdx = List.findIndex (fun t -> t = number) lst
            let move = if number < 0 then listIdx + size + number else listIdx + number + 1
            let toIdx = move % size
            printfn "%A %A %A" listIdx move toIdx
            let removed = List.removeAt listIdx lst
            let newList = 
                if listIdx < toIdx then List.insertAt (toIdx - 1) number removed 
                else List.insertAt toIdx number removed 
            printfn "%A %A" newList ((idx + 1) % size)
            (newList, (idx + 1) % size)) (Array.toList numbers, 0)
    first
let task2 = "task 2"

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
