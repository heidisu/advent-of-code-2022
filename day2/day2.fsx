open System.IO

type Hand = Rock | Paper | Scissors
type Outcome = Loose | Draw | Win

exception BadInput of string

let charToHand letter =
    match letter with
    | 'X' | 'A' -> Rock
    | 'Y' | 'B' -> Paper
    | 'Z' | 'C' -> Scissors
    | l -> raise <| BadInput $"Bad input {l}"

let charToOutcome letter = 
    match letter with
    | 'X' -> Loose
    | 'Y' -> Draw
    | 'Z' -> Win
    | l -> raise <| BadInput $"Bad input {l}"

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.map (fun l -> (l[0], l[2]))

let getHand hand outcome = 
    match (hand, outcome) with
    | (x, Draw) -> x
    | (Paper, Win) -> Scissors
    | (Paper, Loose) -> Rock
    | (Rock, Win) -> Paper
    | (Rock, Loose) -> Scissors
    | (Scissors, Win) -> Rock
    | (Scissors, Loose) -> Paper

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
    |> Seq.fold (fun tot hands -> tot + round hands + handPoints (snd hands) ) 0

let task1 = 
    readFile ()
    |> Seq.map (fun (a, b) ->  (charToHand a, charToHand b))
    |> play
let task2 = 
    readFile ()
    |> Seq.map (fun (a, b) -> 
        let firstHand = charToHand a in
        (firstHand, getHand firstHand (charToOutcome b)))
    |> play

printfn $"Task 1: {task1}" // 12855
printfn $"Task 2: {task2}" // 13726