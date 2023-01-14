
open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let execute clock x acc (l: string) = 
    if l = "noop" then (clock + 1, x, (clock, x) :: acc)
    else
        let splits = l.Split(" ")
        let num =  int <| splits[1]
        let newAcc = (clock + 1, x) :: (clock, x) :: acc
        (clock + 2, x + num, newAcc)

let cycles = 
    readFile ()
    |> List.fold (fun (clock, x, acc) l -> execute clock x acc l) (1, 1, [])
    |> fun (_, _, acc) -> acc 
    |> List.rev

let task1 =
    cycles
    |> List.fold (fun (next, acc) (clock, x) ->
            if clock = next then
                let strength = int64(clock * x)
                (next + 40, acc + strength)
            else (next, acc)
        ) (20, 0L)
    |> snd

let print (clock, x) = 
    let xPos = (clock - 1) % 40 
    let symbol = if xPos = x - 1 || xPos = x || xPos = x + 1 then "#" else "."
    printf $"{symbol}"
    if xPos = 39 then printfn ""

let task2 =
    cycles
    |> List.iter print

printfn $"Task 1: {task1}" // 14920
printfn $"Task 2: BUCACBUZ" // BUCACBUZ
