
open System.IO

let parse (line: string) = 
    line.Split(" -> ")
    |> Array.toList
    |> List.map (fun s -> 
        let parts = s.Split(",")
        (int <| parts[0], int <| parts[1])
        )

let print arr =
    for i in 0 .. Array2D.length1 arr - 1 do
        for j in 0 .. Array2D.length2 arr - 1 do
            printf "%c" (arr[i, j])
        printfn ""
    printfn "" 

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map parse

let insertRock (arr: char [,]) (a: int, b: int) (c: int, d: int) =
    let x1 = min a c
    let x2 = max a c
    let y1 = min b d
    let y2 = max b d
    for i in x1 .. x2 do
        for j in y1 .. y2 do
            arr[i, j] <- '#'

let rec drip (arr: char [,]) maxX maxY (x: int, y: int) =
    if x < 0 || x >= maxX || y + 1 < 0 || y + 1 >= maxY then None
    else
        let first = arr[x, y + 1]
        if first = '#' || first = 'o' 
        then 
            if arr[x - 1, y + 1] = '.' 
            then drip arr maxX maxY (x - 1, y + 1)
            else 
                if x + 1 >= maxX
                then None 
                else if 
                    arr[x + 1, y + 1] = '.' 
                    then drip arr maxX maxY (x + 1, y + 1)
                    else 
                        arr[x, y] <- 'o'
                        Some (x, y)
        else drip arr maxX maxY (x, y + 1) 

let rec fill arr = 
    let maxX = Array2D.length1 arr
    let maxY = Array2D.length2 arr
    [ 1 .. 100000 ]
    |> List.skipWhile (fun _ -> Option.isSome <| drip arr maxX maxY (500, 0))
    |> List.head
    |> fun x -> x - 1

let rec fill2 arr = 
    let maxX = Array2D.length1 arr
    let maxY = Array2D.length2 arr
    [ 1 .. 100000 ]
    |> List.skipWhile (fun i -> drip arr maxX maxY (500, 0) <> Some (500, 0))
    |> List.head
    |> fun x -> x

let createArray tuples  =
    let maxHeight = 
        tuples
        |> List.concat
        |> List.map fst
        |> List.max
    let maxWidth = 
        tuples
        |> List.concat
        |> List.map snd
        |> List.max
    let arr = Array2D.create (maxHeight + 1)(maxWidth + 1) '.'
    tuples
    |> List.map (List.windowed 2)
    |> List.iter (fun l -> l |> List.iter (fun w -> 
        insertRock arr w[0] w[1]
    ))
    arr


let task1 =
    let tuples = readFile()
    let arr = createArray tuples
    fill arr

let task2 =
    let tuples = readFile ()
    let maxHeight = 
        tuples
        |> List.concat
        |> List.map fst
        |> List.max
    let maxWidth = 
        tuples
        |> List.concat
        |> List.map snd
        |> List.max
    let newTuples = [(0, maxWidth + 2); (maxHeight + 400, maxWidth + 2)] :: tuples
    let arr = createArray newTuples
    fill2 arr

printfn $"Task 1: {task1}" // 1016
printfn $"Task 2: {task2}" // 15402
