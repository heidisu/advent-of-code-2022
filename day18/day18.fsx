
open System.IO

let parse (line: string) = 
    let splits = line.Split(",")
    (int splits[0], int splits[1], int splits[2])

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map parse

let sides triples (x, y, z) = 
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
    |> List.map (sides triples)
    |> List.sum

let inGroup ((x, y, z): int * int * int) (group: (int * int * int) Set) =
    let candidates = [
        (x - 1, y, z)
        (x + 1, y, z)
        (x, y - 1, z)
        (x, y + 1, z)
        (x, y, z - 1)
        (x, y, z + 1)
    ]
    let nb = 
        group
        |> Set.exists (fun pt -> candidates |> List.contains pt)
    if nb then (true, Set.add (x, y, z) group) else (false, group)

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
    let xMax = 
        triples
        |> List.map (fun (x, y, z) -> x)
        |> List.max 
    let yMax = 
        triples
        |> List.map (fun (x, y, z) -> y)
        |> List.max
    let zMax = 
        triples
        |> List.map (fun (x, y, z) -> z) 
        |> List.max   
    let array = Array3D.create (xMax + 2) (yMax + 2) (zMax + 2) 0
    triples
    |> List.iter (fun (x, y, z) -> array[x, y, z] <- 1)
    let mutable zeroes = []
    for x in 0 .. xMax + 1 do
        for y in 0 .. yMax + 1 do
            for z in 0 .. zMax + 1 do 
                let curr = array[x, y, z]
                if curr =  0 then
                    zeroes <- (x, y, z) :: zeroes

    let groups = List.fold (fun acc pt -> group acc pt) [] zeroes
    let setWithoutOrigin = groups |> List.filter (fun s -> Set.contains (0,0,0)s |> not)
    let total = triples @ (List.collect (Set.toList) setWithoutOrigin)
    total
    |> List.map (sides total)
    |> List.sum
    
printfn $"Task 1: {task1}" // 4604 
printfn $"Task 2: {task2}" // 2604
