
open System.IO

let readFile () = 
    File.ReadLines "test-input.txt"
    |> Seq.toList

let print (trees: int array array) = 
    let width = Array.length trees
    let height = Array.length trees[0]
    for i in 0 .. width - 1 do
        for j in 0 .. height - 1 do
            printf "%A" (trees[i][j])
        printfn ""

let visibleX (trees: int array array) height y min max =
    let mutable vals = []
    for x in min .. max do
        vals <-trees[x][y] :: vals
    vals |> List.exists (fun v -> v >= height) |> not

let visibleY (trees: int array array) height x min max =
    let mutable vals = []
    for y in min .. max do
        vals <- trees[x][y] :: vals
    vals |> List.exists (fun v -> v >= height) |> not


let isVisible (trees: int array array) size i j =
    if i = 0 || i = (size - 1) || j = 0 || j = (size - 1) then true
    else
        let tree = trees[i][j]
        let visibleHor = visibleX trees tree j 0 (i - 1) || visibleX trees tree j (i + 1) (size - 1)
        let visibleVer = visibleY trees tree i 0 (j - 1) || visibleY trees tree i (j + 1) (size - 1)
        visibleHor || visibleVer

let rec searchLeft (trees: int array array) prev count i j = 
    if i < 0 || trees[i][j] <= prev then count
    else searchLeft trees (trees[i][j]) (count + 1) (i - 1) j

let rec searchRight (trees: int array array) prev count i j = 
    if i > (Array.length trees - 1)  || trees[i][j] <= prev then count
    else searchRight trees (trees[i][j]) (count + 1) (i + 1) j

let rec searchUp (trees: int array array) prev count i j = 
    if j < 0 || trees[i][j] <= prev then count
    else searchUp trees (trees[i][j]) (count + 1) i (j - 1)

let rec searchDown (trees: int array array) prev count i j = 
    if j > (Array.length trees - 1)  || trees[i][j] <= prev then count
    else searchDown trees (trees[i][j]) (count + 1) i (j + 1)

let scenicScore (trees: int array array) i j = 
    let size = Array.length trees 
    if i = 0 || i = (size - 1) || j = 0 || j = (size - 1) then 0
    else 
        let left = searchLeft trees 0  0 (i) j
        let right = searchRight trees 0  0 (i) j
        let up = searchUp trees 0  0 i (j)
        let down = searchDown trees 0   0 i (j)
        printfn $"i:{i} j:{j} l: {left} r: {right} u: {up} d: {down}"
        left * right * up * down

let trees =
    readFile ()
    |> List.map (fun l -> l |> Seq.map (fun c -> c |> string |> int) |> Seq.toArray)
    |> List.toArray

let size = Array.length trees
let task1 = 
    trees
    |> Array.mapi (fun i a -> a |> Array.mapi (fun j aa -> isVisible trees size i j ))
    |> Array.map ( fun a -> Array.toList a)
    |> Array.toList
    |> List.collect id
    |> List.filter (fun v -> v = true)
    |> List.length

let task2  = 
    trees
    |> Array.mapi (fun i a -> a |> Array.mapi (fun j aa -> ((i, j), trees[i][j], scenicScore trees i j)))
    |> Array.map Array.toList
    |> Array.toList
    |> List.collect id
  //  |> List.map (fun (i, j, l) -> l)
  //  |> List.max
printfn $"Task 1: {task1}" // 1870
printfn $"Task 2: {task2}"
for v in task2 do printfn "%A" v
