
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

let inside (triples: (int * int * int) list)  (x, y, z) = 
    let xTrapped = 
        List.exists (fun (a, b, c) -> a < x && b = y && c = z) triples && 
        List.exists (fun (a, b, c) -> a > x && b = y && c = z) triples
    let yTrapped = 
        List.exists (fun (a, b, c) -> b < y && a = x && c = z) triples && 
        List.exists (fun (a, b, c) -> b > y && a = x && c = z) triples
    let zTrapped = 
        List.exists (fun (a, b, c) -> c < z && b = y && a = x) triples && 
        List.exists (fun (a, b, c) -> c > z && b = y && a = x) triples
    xTrapped  && yTrapped && zTrapped

let sides2 triples (x, y, z) = 
    let candidates = [
        (x - 1, y, z)
        (x + 1, y, z)
        (x, y - 1, z)
        (x, y + 1, z)
        (x, y, z - 1)
        (x, y, z + 1)
    ]
    triples
    |> List.filter (fun t -> List.contains t candidates)
    |> List.length         

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
    let array = Array3D.create (xMax + 1) (yMax + 1) (zMax + 1) 0
    triples
    |> List.iter (fun (x, y, z) -> array[x, y, z] <- 1)
    let mutable insides = []
    for x in 0 .. xMax do
        for y in 0 .. yMax do
            for z in 0 .. zMax do 
                let curr = array[x, y, z]
                if curr =  0 && inside triples (x, y, z) then
                    printfn "%A" (x, y, z)
                    //insides <- (x, y, z) :: insides
                    array[x, y, z] <- 1
    for x in 0 .. xMax do
        for y in 0 .. yMax do
            for z in 0 .. zMax do 
                let curr = array[x, y, z]
                if inside triples (x, y, z) then
                    insides <- (x, y, z) :: insides
    //let insideSum = insides |> List.map (sides2 triples) |>List.sum
    let outsides = 
        triples
        |> Set.ofList
        |> Set.difference (Set.ofList insides)
        |> Set.toList
    let totals = 
        triples
        |> Set.ofList
        |> Set.union (Set.ofList insides)
        |> Set.toList
    printfn "Outsides: %A (%A %A)" (List.length outsides) (List.length triples) (List.length totals)
    totals
    |> List.map (sides totals)
    |> List.sum
    //task1 - insideSum

printfn $"Task 1: {task1}" // 4604 
printfn $"Task 2: {task2}" // 4298 too high 4278 too high 4274 too high // 2592 er feil // 2594 er feil // 3107 feil // 2596 feil
