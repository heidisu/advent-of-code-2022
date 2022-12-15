
open System.IO

type SensorData = {    
    Sensor: int * int
    Beacon: int * int
    Manhattan: int
}

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
    {
        Sensor = sensor
        Beacon = beacon
        Manhattan = manhattan
    }

let withinDistance (y: int) sensorData =
    let (a, b) = sensorData.Sensor
    let z = sensorData.Manhattan - abs(b - y)
    (a - z, z + a)

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map parseLine

let task1 =
    let data = readFile ()
    let idx = 2000000
    let points = 
        data
        |> List.map (withinDistance idx)
        |> List.map (fun (a, b) -> [a .. b] |> Set.ofList)
        |> List.fold (fun acc s -> Set.union acc s) Set.empty
    let beacons = 
        data
        |> List.map (fun data -> data.Beacon)
        |> List.filter (fun (x, y) -> y = idx && Set.contains x points)
        |> List.distinct
        |> List.length
    (Set.count points) - beacons
let task2 = "task 2"

printfn $"Task 1: {task1}" // 5240818
printfn $"Task 2: {task2}"
