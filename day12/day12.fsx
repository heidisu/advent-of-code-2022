
open System.IO

let readFile () = 
    File.ReadLines "test-input.txt"
    |> Seq.toList
    |> List.map (fun l -> l |> Seq.toArray)
    |> List.toArray

let print (trees: char array array) = 
    let width = Array.length trees
    let height = Array.length trees[0]
    for i in 0 .. width - 1 do
        for j in 0 .. height - 1 do
            printf "%A" (trees[i][j])
        printfn ""

let rec search (arr: char array array) (dist: int array array) (prev: (int * int) array array) (goal: int * int) (q: (int * int) list) (u: int * int) = 
    if List.isEmpty q || not <| List.contains goal q then (dist, prev)
    else 
        let (x, y) = u
        let nbCandidates = [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]
        let neighbours = 
            nbCandidates
            |> List.filter (fun c -> List.contains c q)
        neighbours
        |> List.iter (
            fun (a, b) ->
                let graphDist = 1
                let alt = dist[x][y] + graphDist
                let diff = (arr[a][b] |> int) - (arr[x][y] |> int)
                if diff <= 1 && (dist[a][b] = -1 || alt < dist[a][b] )
                then 
                    dist[a][b] <- alt
                    prev[a][b] <- u
            )
        let nextU = 
            q
            |> List.map (fun (i, j) -> (i, j, dist[i][j]))
            |> List.filter (fun (i, j, k) -> k <> -1)
            |> List.sortBy (fun (i, j, k) -> k)
            |> List.map (fun (i, j, _) -> (i, j))
            |> List.head
        search arr dist prev goal (q |> List.filter (fun x -> x<> nextU)) nextU

let dijkstra (arr: char array array) =
    let height = Array.length arr
    let width = Array.length arr[0]

    let dist: int array array = Array.create height (Array.create width -1)
    let prev: (int * int) array array = Array.create height (Array.create width (-1, -1))
    dist[0][0] <- 0

    let q = 
        [0 .. height] 
        |> List.allPairs [0 .. width]
    
    let goal = (2, 5)
    arr[0][0] <- 'a'
    arr[2][5] <- 'z'
    search arr dist prev goal (q |> List.fiter (fun x -> x <> u) (0,0)


let task1 = 
    readFile ()
    |> dijkstra

let task2 = "task 2"

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
