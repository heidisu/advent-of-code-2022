
open System.IO

let readFile () = 
    File.ReadLines "test-input.txt"
    |> Seq.toList

let snafuToInt (str: string) =
    let numbers = 
        str
        |> Seq.rev
        |> Seq.mapi (fun i c -> 
            let pow = 5.0**i |> int64
            if c = '-' then pow * (int64 (-1))
            else if c = '=' then pow * (int64 (-2))
            else (int64 (string c)) * pow
        )
    //printfn "%A" (Seq.toList numbers)
    numbers |> Seq.sum 

let rec intToSnafu' (acc: string) (pow: int) (num: int64) = 
    if num = 0 then acc 
    else if pow = 0 then 
        let factor = 5.0**(float(pow + 1)) |> int64
        let rest = num % factor
        let symbol = 
            if rest = 3 then "=" 
            else if rest = 4 then "-"
            else $"{rest}"
        let newNum = 
            if rest = 3 then num + factor - rest
            else if rest = 4 then num + factor - rest
            else num - rest
        printfn "symbol: %A factor: %A rest: %A num: %A" symbol factor rest newNum
        intToSnafu' (symbol + acc) (pow + 1)  newNum
    else 
        let factor = 5.0**(float(pow)) |> int64
        let rest = num / factor
        let symbol = 
            if rest = 3 then "=" 
            else if rest = 4 then "-"
            else $"{rest}"
        let newNum = 
            if rest = 3 then num + factor - rest
            else if rest = 4 then num + factor - rest
            else num - rest * factor
        printfn "symbol: %A factor: %A rest: %A num: %A" symbol factor rest newNum
        intToSnafu' (symbol + acc) (pow + 1)  newNum

let rec startPow (num: int64) (pow: int) =
    let factor = 5.0**(float(pow)) |> int64
    if  factor * 2L >= num then pow else startPow num (pow + 1)

let rec intToSnafu (acc: string) (pow: int) (num: int64) =
    if num = 0L || pow < 0 then acc
    else 
        let factor = 5.0**(float(pow)) |> int64
        let nextFactor =  5.0**(float(pow - 1)) |> int64
        let foo = 
            if num > factor then 2L
            else if factor >= num && num > 0L then 1L
            else if 0L > num && (factor - num)  = nextFactor * 2L then -2L
            else if 0L > num && (factor - num)  = nextFactor then -1L
            else if 0L >= num && num > -1L * factor then 0L
            else if -1L * factor >= num && num > -2L * factor then -1L
            else -2L
        let symbol = match foo with
        | 2L -> "2"
        | 1L -> "1"
        | 0L -> "0"
        | -1L -> "-"
        | -2L -> "="
        printfn "acc: %A symbol: %A foo: %A num: %A newNum: %A pow: %A factor: %A nextFactor: %A" acc symbol foo num (num - (foo * factor)) pow factor nextFactor
        intToSnafu (acc + symbol) (pow - 1) (num - (foo * factor))

printfn "%A" (snafuToInt "1=-0-2")
printfn "%A" (startPow 1747L 0)
printfn "%A" (intToSnafu "" (startPow 1747L 0) 1747L)

let task1 =
    readFile ()
    |> List.map snafuToInt
    |> List.sum
let task2 = "task 2"

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
