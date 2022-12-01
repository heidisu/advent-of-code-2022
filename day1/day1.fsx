open System.IO

let fileToElfCalories () = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.fold (
        fun (curr, groups) l 
            -> if l = "" then ([], curr :: groups) else (int l :: curr, groups) ) 
        ([], [])
    |> snd
    |> List.map List.sum
 

let calories = fileToElfCalories ()
let task1 = List.max calories
let task2 = 
    calories
    |> List.sortDescending 
    |> List.take 3 
    |> List.sum


printfn $"Task 1: {task1}" // 66306
printfn $"Task 2: {task2}" // 195292