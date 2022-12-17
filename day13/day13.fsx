open System.IO
open System.Text.Json

type Packet = ItemList of Packet list | Number of int

let rec parseArray (acc: Packet list) (size:int) (idx: int) (arr: JsonElement) =
    if idx = size then acc
    else 
        let elem = parseElement (arr.Item(idx))
        parseArray (acc @ [elem]) size (idx + 1) arr

and parseElement (elem: JsonElement) = 
    if elem.ValueKind = JsonValueKind.Array then
        let arrayLength = elem.GetArrayLength()
        ItemList <| parseArray [] arrayLength 0 elem
    else Number (elem.GetInt32())

let parse (str: string) = 
    let jObj = JsonDocument.Parse(str)
    let root = jObj.RootElement
    parseElement root
         
printfn "%A" (parse "[[]]")
printfn "%A" (parse "[1, 2, [3]]")

let rec compare left right = 
    printfn "%A %A" left right
    match (left, right) with
    | Number m, Number n -> if m <= n then true else false
    | ItemList l1, ItemList l2 -> 
        match l1, l2 with
        | [], [] -> true
        | _, [] -> false
        | [], _ -> true
        | Number m :: xs, Number n :: ys -> 
            if m < n then true else if m > n then false else compare (ItemList xs) (ItemList ys)
        | x :: xs, y :: ys ->
            if compare x y then compare (ItemList xs) (ItemList ys) else false
    | Number n, ItemList l -> compare (ItemList [Number n] ) (ItemList l)
    | ItemList l, Number n -> compare (ItemList l) (ItemList [Number n])

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.fold (fun (acc, prev) l -> 
            if l = "" then (acc @ [prev], [])
            else (acc, prev @ [l])
        ) ([], [])
    |> fst
    |> List.map (fun l -> (List.item 0 l |> parse, List.item 1 l |> parse))
    |> List.mapi (fun i (a, b) -> (i + 1, compare a b))
    |> List.filter (fun (i, b) -> b = true)
    |> List.map (fun (i, b) -> i)
    |> List.fold (fun acc n -> n + acc) 0 

let task1 = 
    readFile ()
let task2 = "task 2"

printfn $"Task 1: {task1}" // 1777 too low
printfn $"Task 2: {task2}"
