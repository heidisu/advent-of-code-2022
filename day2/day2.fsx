open System.IO

type Hand = Rock | Paper | Scissors
type Outcome = Loose | Draw | Win

exception BadInput of string

let charToHand char =
    match char with
    | 'X' | 'A' -> Rock
    | 'Y' | 'B' -> Paper
    | 'Z' | 'C' -> Scissors
    | c -> raise <| BadInput $"Bad input {c}"

let charToOutcome c = 
    match c with
    | 'X' -> Loose
    | 'Y' -> Draw
    | 'Z' -> Win
    | c -> raise <| BadInput $"Bad input {c}"

let readFile secondPlayerMapper = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map (fun l -> (charToHand l[0], secondPlayerMapper l[2]))

let getHand handOutcome = 
    let secondHand = 
        match handOutcome with
        | (x, Draw) -> x
        | (Paper, Win) -> Scissors
        | (Paper, Loose) -> Rock
        | (Rock, Win) -> Paper
        | (Rock, Loose) -> Scissors
        | (Scissors, Win) -> Rock
        | (Scissors, Loose) -> Paper
    (fst handOutcome, secondHand)

let round (a, b) = 
    match (a, b) with
    | (Scissors, Rock)
    | (Paper, Scissors)
    | (Rock, Paper) -> 6
    | (Scissors, Scissors)
    | (Paper, Paper)
    | (Rock, Rock) -> 3
    | _ -> 0

let handPoints hand =
    match hand with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let play hands = 
    hands
    |> List.fold (fun tot hands -> tot + round hands + handPoints (snd hands) ) 0

let task1 = readFile charToHand |> play
let task2 = 
    readFile charToOutcome
    |> List.map getHand
    |> play

printfn $"Task 1: {task1}" // 12855
printfn $"Task 2: {task2}" // 13726