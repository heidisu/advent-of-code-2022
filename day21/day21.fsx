open System.IO

type Op = 
    Number of int64 
    | Mult of (string * string) 
    | Add of (string * string) 
    | Div of (string * string)
    | Sub of (string * string)
    | Unknown

let parse (line: string): string * Op = 
    let splits = line.Split(": ")
    let parts = splits[1].Split(" ")
    let key = splits[0]
    let numParts = Array.length parts
    let op = 
        match numParts with
        | 1 -> Number (int64 parts[0])
        | _ -> 
            let first = parts[0]
            let second = parts[2]
            match parts[1] with
            | "*" -> Mult (first, second)
            | "/" -> Div (first, second)
            | "+" -> Add (first , second)
            | "-" -> Sub (first, second)
    (key, op)


let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map parse
    |> Map.ofList

let rec eval (map: Map<string, Op>) (key: string)  =
    match Map.find key map with
    | Number n -> n
    | Mult (a, b) -> (eval map a) * (eval map b)
    | Div (a, b) -> (eval map a) / (eval map b)
    | Sub (a, b) -> (eval map a) - (eval map b)
    | Add (a, b) -> (eval map a) + (eval map b)

let rec printEval (map: Map<string, Op>) (key: string)  =
    match Map.find key map with
    | Number n -> printf "%A" n
    | Unknown -> printf "x"
    | Mult (a, b) -> 
        printf "(" 
        printEval map a
        printf ")"
        printf " * "
        printf "("
        printEval map b
        printf ")"
    | Div (a, b) -> 
        printf "(" 
        printEval map a
        printf ")"
        printf " / "
        printf "("
        printEval map b
        printf ")"
    | Sub (a, b) -> 
        printf "(" 
        printEval map a
        printf ")"
        printf " - "
        printf "("
        printEval map b
        printf ")"
    | Add (a, b) -> 
        printf "(" 
        printEval map a
        printf ")"
        printf " + "
        printf "("
        printEval map b
        printf ")"

let task1 = 
    let map = readFile ()
    eval map "root"

let task2 =
    let map = readFile ()
    let equationMap = Map.add "humn" (Unknown) map
    printEval equationMap "fflg"
    printfn ""
    let solution = 3087390115721L
    let mp = Map.add "humn" (Number 3087390115721L) map
    let left = eval mp "fflg"
    let right = eval mp "qwqj"
    printfn "%A = %A" left right
    solution
    

printfn $"Task 1: {task1}" // 160274622817992
printfn $"Task 2: {task2}" // 3087390115721
