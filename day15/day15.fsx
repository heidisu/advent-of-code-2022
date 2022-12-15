
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
    let z = abs(b - y)
    (a - manhattan + z, manhattan - z - a)

let readFile () = 
    File.ReadLines "test-input.txt"
    |> Seq.toList
    |> List.map parseLine
    |> List.map (fun (s,  b, m) -> withinDistance 10 s m)
    |> List.skip 3

let task1 =
    readFile ()
let task2 = "task 2"

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
