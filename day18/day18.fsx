
open System.IO

let parse (line: string) = 
    let splits = line.Split(",")
    (int splits[0], int splits[1], int splits[2])

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map parse


let uncoveredSides triples (x, y, z) = 
    let candidates = [
        (x - 1, y, z)
        (x + 1, y, z)
        (x, y - 1, z)
        (x, y + 1, z)
        (x, y, z - 1)
        (x, y, z + 1)
    ]
    let neighbours = 
        triples
        |> List.filter (fun t -> List.contains t candidates)
    6 - (List.length neighbours)

let task1 = 
    let triples = readFile ()
    triples
    |> List.map (uncoveredSides triples)
    |> List.sum

printfn $"Task 1: {task1}" // 4604 

let inGroup ((x, y, z): int * int * int) (group: (int * int * int) Set) =
    let isNeighbour = 
        group
        |> Set.exists (fun (a, b, c) -> abs(a - x) + abs(b - y) + abs (c - z) = 1)
    if isNeighbour then (true, Set.add (x, y, z) group) else (false, group)

let group (groups: (int * int * int) Set list) (pt: int * int * int) =
    let newGroups = 
        groups
        |> List.map (inGroup  pt)
    let unchanged = 
        newGroups 
        |> List.filter (fun (inGroup, gr) -> inGroup = false) 
        |> List.map snd
    let changed =
        newGroups 
        |> List.filter (fun (inGroup, gr) -> inGroup = true) 
        |> List.map snd
        |> List.fold (fun acc s -> Set.union s acc) (Set.singleton pt)
    changed :: unchanged

let task2 =
    let triples = readFile ()
    let (xMax, yMax, zMax) = 
        triples
        |> List.fold(
            fun (mx, my, mz) (x, y, z) -> 
            (max x mx, max y my, max z mz)) (0, 0, 0)
 
    let zeroes = 
        [0 .. zMax + 2]
        |> List.allPairs [0 .. yMax + 2]
        |> List.allPairs [0 .. xMax + 2]
        |> List.map (fun (x, (y, z)) -> (x, y, z))
        |> List.filter (fun pt ->
            triples |> List.contains pt |> not)

    let groups = List.fold (fun acc pt -> group acc pt) [] zeroes
    let setWithoutOrigin = groups |> List.filter (fun s -> Set.contains (0,0,0)s |> not)
    let total = triples @ (List.collect (Set.toList) setWithoutOrigin)
    total
    |> List.map (uncoveredSides total)
    |> List.sum
    
printfn $"Task 2: {task2}" // 2604
