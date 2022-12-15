
open System.IO

let parseLine (line: string) = 
    let coords = 
        line
            .Replace("Sensor at x=", "")
            .Replace(" y=", "")
            .Replace(": closest beacon is at x=", ",")
    let numbers = coords.Split(",")
    let sensor = (int numbers[0], int numbers[1])
    let beacon = (int numbers[2], int numbers[3])
    let manhattan = abs(fst sensor - fst beacon) + abs(snd sensor - snd beacon)
    (sensor, beacon, manhattan)

let withinDistance (y: int) sensor manhattan =
    let (a, b) = sensor
    let z = manhattan - abs(b - y)
    (a - z, z + a)

let readFile () = 
    let items = 
        File.ReadLines "test-input.txt"
        |> Seq.toList
        |> List.map parseLine
    
    let idx = 10
    let count = 
        items
        |> List.map (fun (s,  b, m) -> withinDistance idx s m)
        |> List.map (fun (a, b) -> [a .. b] |> Set.ofList)
        |> List.fold (fun acc s -> Set.union acc s) Set.empty
        |> Set.count
    let beacons = 
        items
        |> List.filter (fun (s, b, m) -> snd b = idx)
        |> List.length
    count - beacons

let task1 =
    readFile ()
let task2 = "task 2"

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
