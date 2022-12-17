#r "nuget: FSharp.Collections.ParallelSeq"

open FSharp.Collections.ParallelSeq
open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map (fun l -> l |> Seq.toArray)
    |> List.toArray

let print arr = 
    let width = Array.length arr
    let height = Array.length arr[0]
    for i in 0 .. width - 1 do
        for j in 0 .. height - 1 do
            printf "%A" (arr[i][j])
        printfn ""

let print2d arr = 
    let width = Array2D.length1 arr
    let height = Array2D.length2 arr
    for i in 0 .. width - 1 do
        for j in 0 .. height - 1 do
            printf "%A" (arr[i, j])
        printfn ""

// https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
let rec dijkstra (arr: char array array) (dist:  int [,]) (prev: (int * int) [,]) (goal: int * int) (q: (int * int) list) (u: int * int) = 
    if List.isEmpty q || not <| List.contains goal q then prev
    else 
        let (x, y) = u
        let nbCandidates = [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]
        let neighbours = 
            nbCandidates
            |> List.filter (fun c -> List.contains c q)
        neighbours
        |> List.iter (
            fun (a, b) ->
                let alt = dist[x, y] + 1
                let diff = (arr[a][b] |> int) - (arr[x][y] |> int)
                if diff <= 1 && alt < dist[a, b]
                then 
                    dist[a, b] <- alt
                    prev[a, b] <- u
            )
        let nextU = 
            q
            |> List.map (fun (i, j) -> ((i, j), dist[i, j]))
            |> List.sortBy (fun (pt, dist) -> dist)
            |> List.head
            |> fst
        dijkstra arr dist prev goal (q |> List.filter (fun x -> x <> nextU)) nextU

let searchArr (arr: char array array) c = 
    let width = Array.length arr
    let height = Array.length arr[0]
    let mutable res = []
    for i in 0 .. width - 1 do
        for j in 0 .. height - 1 do
            if arr[i][j] = c then res <- (i, j) :: res
    res

let rec traverse (prev: (int * int) [,]) start acc (x, y) = 
    if (x, y) = start then Some acc
    else if (x, y) = (-1, -1) then None else traverse prev start ((x, y) :: acc) prev[x, y]

let searchFromPoint arr height width goal (x, y) = 
    printfn "%A" (x, y)
    let dist = Array2D.create height width 100000000
    let prev =  Array2D.create height width (-1, -1)
    let q = 
        [0 .. width - 1] 
        |> List.allPairs [0 .. height - 1]
        |> List.filter (fun (a, b) -> (a, b) <> (x, y))
    dist[x, y] <- 0
    let prev = dijkstra arr dist prev goal q (x, y)
    traverse prev (x, y) [] goal

let task1 =
    let arr = readFile ()
    let height = Array.length arr
    let width = Array.length arr[0]
    let start  = searchArr  arr 'S' |> List.head
    let goal = searchArr arr 'E'|> List.head
    arr[fst start][snd start] <- 'a'
    arr[fst goal][snd goal] <- 'z'
    List.length (searchFromPoint arr height width goal start).Value
    
let task2 =
    let arr = readFile ()
    let height = Array.length arr
    let width = Array.length arr[0]
    let start  = searchArr  arr 'S' |> List.head
    let goal = searchArr arr 'E' |> List.head
    arr[fst start][snd start] <- 'a'
    arr[fst goal][snd goal] <- 'z'

    searchArr arr 'a'
    |> PSeq.map (searchFromPoint arr height width goal)
    |> PSeq.choose id
    |> PSeq.map List.length
    |> PSeq.min

printfn $"Task 1: {task1}" // 468
printfn $"Task 2: {task2}" // 459
