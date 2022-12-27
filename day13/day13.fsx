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
         
let rec compare left right = 
    match (left, right) with
    | Number m, Number n -> if m <= n then Some true else Some false
    | ItemList l1, ItemList l2 -> 
        match l1, l2 with
        | [], [] -> None
        | _, [] -> Some false
        | [], _ -> Some true
        | Number m :: xs, Number n :: ys -> 
            if m < n then Some true else if m > n then Some false else compare (ItemList xs) (ItemList ys)
        | x :: xs, y :: ys ->
            match compare x y with
            | None -> compare (ItemList xs) (ItemList ys)
            | x -> x
    | Number n, ItemList l -> compare (ItemList [Number n] ) (ItemList l)
    | ItemList l, Number n -> compare (ItemList l) (ItemList [Number n])

let a = parse "[[[]]]"
let b = parse "[[]]"
printfn "compare %A" (compare b a)

let rec bubbleSort (arr: Packet array) =
    let prev = Array.copy arr
    for i in 0 .. Array.length arr - 2 do
        if compare arr[i] arr[i + 1] = Some false then
            let tmp = arr[i]
            arr[i] <- arr[i + 1]
            arr[i + 1] <- tmp
    if prev = arr then arr else bubbleSort arr

let task1 = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.fold (fun (acc, prev) l -> 
            if l = "" then (acc @ [prev], [])
            else (acc, prev @ [l])
        ) ([], [])
    |> fst
    |> List.map (fun l -> (List.item 0 l |> parse, List.item 1 l |> parse))
    |> List.mapi (fun i (a, b) -> (i + 1, compare a b))
    |> List.filter (fun (i, b) -> b = Some true)
    |> List.map fst
    |> List.fold (fun acc n -> n + acc) 0 

let task2 =
    let div1  = parse "[[2]]"
    let div2 = parse "[[6]]"
    let packets = 
        File.ReadLines "input.txt"
        |> Seq.toList
        |> List.filter (fun l -> l <> "")
        |> List.map parse
        |> fun l -> l @ [div1; div2]
        |> List.toArray
    bubbleSort packets |> ignore
    let idx1 = Array.findIndex (fun p -> p = div1) packets + 1
    let idx2 = Array.findIndex (fun p -> p = div2) packets + 1
    idx1 * idx2

printfn $"Task 1: {task1}" // 6420
printfn $"Task 2: {task2}" // 22000
