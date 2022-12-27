
open System.IO

let readFile buffer = 
    let lines = 
        File.ReadLines "input.txt"
        |> Seq.toList
    let height = List.length lines
    let width = lines |> List.head |> Seq.length
    let array = Array2D.create (height + 2 * buffer) (width + 2 * buffer) '.'
    let mutable elves = []
    for i in 0 ..  height - 1 do
        let line = List.item  i lines
        for j in 0 .. width - 1 do
            let symbol = line[j]
            if symbol = '#' then elves <- (i + buffer, j + buffer) :: elves
            array[i + buffer, j + buffer] <- symbol
    (array, elves |> List.rev |> List.toArray)
        
let print (arr: char [,]) =
    for i in 0 .. Array2D.length1 arr - 1 do
        for j in 0 .. Array2D.length2 arr - 1 do
            printf "%c" arr[i, j]
        printfn ""
    printfn ""

let north = [
    (-1, 0)
    (-1, -1)
    (-1, 1)
]

let south = [
    (1, 0)
    (1, -1)
    (1, 1)
]

let west = [
    (0, -1)
    (-1, -1)
    (1, -1)
]

let east = [
    (0, 1)
    (-1, 1)
    (1, 1)
]

type Dir = North | South | West | East

let dirList (dir: Dir) = 
    match dir with
    | North -> [North; South; West; East]
    | South -> [South; West; East; North]
    | West -> [West; East; North; South]
    | East -> [East; North; South; West]

let neighbours (dir: Dir) = 
    match dir with
    | North -> north
    | South -> south
    | West -> west
    | East -> east

let testNeighbours (dir: Dir) (arr: char [,]) (x, y) = 
    neighbours dir
    |> List.map (fun (a, b) -> arr[x + a, y + b])
    |> List.forall (fun c -> c = '.')        

let move (dir: Dir) (x, y) = 
    match dir with
    | North -> (x - 1, y)
    | South -> (x + 1, y)
    | West -> (x, y - 1)
    | East -> (x, y + 1)

let next (dir: Dir) = 
    match dir with
    | North -> South
    | South -> West
    | West -> East
    | East -> North

let nbs = north @ south @ west @ east

let allEmpty (arr: char [,]) (x, y) =
    nbs
    |> List.forall (fun (a, b) -> arr[x + a, y + b] = '.')

let round (arr: char [,]) elves (dir: Dir) =
    let proposedMoves = Array.create (Array.length elves) (-1, -1)
    let directions = dirList dir
    elves |>
        Array.iteri (fun i (x, y) -> 
            if allEmpty arr (x, y) then proposedMoves[i] <- (x, y)
            else
                let proposed = 
                    directions 
                    |> List.map (fun d -> (d, testNeighbours d arr (x, y)))
                    |> List.tryFind snd
                match proposed with
                | Some (d, _) -> proposedMoves[i] <- move d (x, y)
                | None -> proposedMoves[i] <- (x, y)    
        )

    let duplicates = 
        proposedMoves
        |> Array.groupBy id
        |> Array.filter (fun (p, l) -> Array.length l > 1)
        |> Array.map fst
    let actualMoves = 
        proposedMoves
        |> Array.mapi (fun i p -> if Array.contains p duplicates then elves[i] else p)
    elves
    |> Array.iteri (fun i (x, y) ->
        let (a, b) = actualMoves[i]
        arr[x, y] <- '.'
        arr[a, b] <- '#'
    )
    (arr, actualMoves, next dir)

let task1 =
    let (arr, elves) = readFile 10
    let (res, mv, dir) = 
        [1 .. 10]
        |> List.fold (fun (arr, actualMoves, dir) i -> round arr actualMoves dir) (arr, elves, North)
    let mutable elves = []
    res
    |> Array2D.iteri (fun x y c -> 
        if c = '#' then elves <- (x, y) :: elves)
    let minX = 
        elves 
        |> List.minBy fst |> fst
    let maxX = elves |> List.maxBy fst |> fst
    let minY = elves |> List.minBy snd |> snd
    let maxY = elves |> List.maxBy snd |> snd
    let mutable cnt = 0
    for i in minX .. maxX do
        for j in minY .. maxY do
            if res[i, j] = '.' then cnt <- cnt + 1
    cnt

let rec searchNoMove (prev: char [,]) elves dir (cnt: int) = 
    if cnt % 100 = 0 then printfn "iter: %A" cnt
    let cp = Array2D.copy prev
    let (res, mv, d) = round cp elves dir
    if res = prev then cnt
    else searchNoMove res mv d (cnt + 1)

let task2 =
    let (arr, elves) = readFile 500
    searchNoMove arr elves North 1

printfn $"Task 1: {task1}" // 3684
printfn $"Task 2: {task2}" // 862