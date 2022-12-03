open System
open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let charToNum (c: char) = 
    if (Char.IsLower c) then int c - 96 else int c - 38

let task1 = 
    readFile ()
    |> List.map Seq.toList
    |> List.map (fun l -> 
        let half = (List.length l) / 2
        (List.take half l, List.skip half l))
    |> List.map (fun (a, b)-> (Set(a), Set(b)))
    |> List.map (fun (a, b) -> Set.intersect a b)
    |> List.map (fun s -> s |> Set.toList |> List.head)
    |> List.map charToNum
    |> List.sum

let task2 =
    readFile ()
    |> List.map Seq.toList
    |> List.chunkBySize 3
    |> List.map (fun l-> (Set(List.item 0 l), Set(List.item 1 l), Set(List.item 2 l)))
    |> List.map (fun (a, b, c) -> Set.intersect (Set.intersect a b) c)
    |> List.map (fun s -> s |> Set.toList |> List.head)
    |> List.map charToNum 
    |> List.sum

printfn $"Task 1: {task1}" // 7428
printfn $"Task 2: {task2}" // 2650
