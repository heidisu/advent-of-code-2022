
open System.IO
open System
open System.Collections.Generic


let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

type Monkey = {
    Id: int
    Items:int list
    Operation: int-> int
    Test: int -> bool
    IfTrue: int
    IfFalse: int
}

let testMonkeys = [|
    { 
        Id = 0
        Items = [79; 98]
        Operation = fun x -> x * 19
        Test = fun x -> x % 23 = 0
        IfTrue = 2
        IfFalse = 3
    }
    { 
        Id = 1
        Items = [54; 65; 75; 74]
        Operation = fun x -> x + 6
        Test = fun x -> x % 19 = 0
        IfTrue = 2
        IfFalse = 0
    }
    { 
        Id = 2
        Items = [79; 60; 97]
        Operation = fun x -> x * x
        Test = fun x -> x % 13 = 0
        IfTrue = 1
        IfFalse = 3
    }
    { 
        Id = 3
        Items = [74]
        Operation = fun x -> x + 3
        Test = fun x -> x % 17 = 0
        IfTrue = 0
        IfFalse = 1
    }
|]

let visitedItems = Array.create 8 0

let play (monkeys: Monkey array) (monkey: Monkey) = 
    monkey.Items
    |> List.iter (fun i -> 
        let worryLevel = (monkey.Operation i) / 3
        //printfn "%A" worryLevel
        let toMonkey = if monkey.Test worryLevel then monkey.IfTrue else monkey.IfFalse
        let newMonkey = monkeys[toMonkey]
        monkeys[toMonkey] <- { newMonkey with Items = newMonkey.Items @ [worryLevel]}
        visitedItems[monkey.Id] <- visitedItems[monkey.Id] + 1
    )
    { monkey with Items = []}

let round monkeys = 
    for i in 0 .. Array.length monkeys - 1 do
        monkeys[i] <- play monkeys monkeys[i]

let task1 = 
    for i in 1 .. 20 do
        round testMonkeys
    visitedItems
    |> Array.toList
    |> List.mapi (fun i visited -> (i, visited))
    |> List.sortByDescending (fun (i, tot) -> tot)
    |> fun l -> (snd l[0]) * (snd l[1])



type Operation = Double | Multiply of int | Add of int

type Monkey2 = {
    Id: int
    Operation: Operation
    Test: int
    IfTrue: int
    IfFalse: int
}

let primeFactors: Dictionary<int, Map<int, int>> = Dictionary()

let rec factorize (acc: int list) (num: int) (div: int) = 
    if div > num then acc 
    else
        if num % div = 0 then factorize (div :: acc) (num / div) div
        else factorize acc num (div  + 1)

let rec bigFactor (acc: int list) (num: bigint) (div: bigint) = 
    if div > num then acc 
    else
        if num % div = bigint 0 then bigFactor (int div :: acc) (num / div) div
        else bigFactor acc num (div  + bigint 1)

printfn "%A" (factorize [] 74 2)
printfn "%A" (factorize [] 2356 2)

let getOrAddPrime num : Map<int, int> = 
    if primeFactors.ContainsKey num then primeFactors.Item num 
    else 
        let factors = 
            factorize [] num 2
            |> List.groupBy id
            |> List.map (fun (n, l) -> (n, List.length l))
            |> Map.ofList
        primeFactors.Add(num, factors)
        factors

let add (num: int) (mp: Map<int, int>) = 
    printfn "before: %A" mp
    let factors = factorize  [] num 2
    let mutable remainingFactors = []
    let newMp = 
        factors
        |> List.fold (fun(m: Map<int, int>) k -> 
            if m.ContainsKey k then
                let v = m.Item k
                m.Add(k, (v - 1))
            else 
                remainingFactors <- k :: remainingFactors
                m
        ) mp
    let mptoInt =
        newMp
        |> Map.toList
        |> List.map (fun (a, b) ->  pown a b)
        |> List.fold (fun s a -> s * (int64 a)) 1L
    let total = mptoInt + (List.fold (fun a n -> a * (int64 n)) 1L remainingFactors)
    let tot = int total
    if total <> int64 tot then printfn "HJELP!!!!!!!!"
    let res = getOrAddPrime tot
    //printfn "after: %A factors: %A remainingFactors: %A res: %A" newMp factors remainingFactors res
    res
    (*let mpToBigint =
        mp
        |> Map.toList
        |> List.map (fun (a, b) ->  pown a b)
        |> List.fold (fun s a -> s * (bigint a)) (bigint 1)
    let total = (bigint num) + mpToBigint
    let res = 
        bigFactor [] total (bigint 2)
        |> List.groupBy id
        |> List.map (fun (n, l) -> (n, List.length l))
        |> Map.ofList
        *)
    //printfn "%A %A %A %A" num mp mpNum res

let testMonkeys2: Monkey2 array = [|
    { 
        Id = 0
        Operation = Multiply 19
        Test = 23
        IfTrue = 2
        IfFalse = 3
    }
    { 
        Id = 1
        Operation = Add 6
        Test = 19
        IfTrue = 2
        IfFalse = 0
    }
    { 
        Id = 2
        Operation = Double
        Test = 13
        IfTrue = 1
        IfFalse = 3
    }
    { 
        Id = 3
        Operation = Add 3
        Test = 17
        IfTrue = 0
        IfFalse = 1
    }
|]

let monkeyItems =  Array.create 8 []
monkeyItems[0] <- [
    getOrAddPrime 79
    getOrAddPrime 98
]

monkeyItems[1] <- [
    getOrAddPrime 54
    getOrAddPrime 65
    getOrAddPrime 75
    getOrAddPrime 74
]

monkeyItems[2] <- [
    getOrAddPrime 79
    getOrAddPrime 60
    getOrAddPrime 97
]

monkeyItems[3] <- [
    getOrAddPrime 74
]

let visitedItems2 = Array.create 8 0

let play2 (monkey: Monkey2) = 
    let items = monkeyItems[monkey.Id]
    let num = items.Length
    items
    |> List.iter (fun i -> 
        let worryLevel: Map<int, int> = 
            match  monkey.Operation with
            | Double -> i |> Map.toList |> List.map (fun (n, i) -> (n, 2 * i)) |> Map.ofList
            | Multiply m -> 
                Map.change m (fun x -> 
                    match x with
                    | Some v -> Some (v + 1)
                    | None -> Some 1
                    ) i
            | Add n -> add n i
        // printfn "i: %A worrylevel: %A op: %A" i worryLevel monkey.Operation
        let toMonkey = if worryLevel.ContainsKey monkey.Test then monkey.IfTrue else monkey.IfFalse
        monkeyItems[toMonkey] <- monkeyItems[toMonkey] @ [worryLevel]
    )
    monkeyItems[monkey.Id] <- []
    visitedItems2[monkey.Id] <- visitedItems2[monkey.Id] + num

let round2 (monkeys: Monkey2 array) = 
    for i in 0 .. Array.length monkeys - 1 do
        play2 monkeys[i]

let task2 =
    for i in 1 .. 20 do
        if i % 1000 = 0 then printfn "iter: %A" i 
        round2 testMonkeys2
    printfn "%A" visitedItems2
    visitedItems2
    |> Array.toList
    |> List.mapi (fun i visited -> (i, visited))
    |> List.sortByDescending (fun (i, tot) -> tot)
    |> List.take 2
    // |> fun l -> (snd l[0]) * (snd l[1])


(*
let monkeyItems: bigint list array  = [|
     [79L; 98L]
     [54L; 65L; 75L; 74L]
     [79L; 60L; 97L]
     [74L]
|]*)

(*
let prodMonkeys: Monkey array = [|
    {
        Id = 0
        Items = [80]
        Operation = fun x -> x * (bigint 5)
        Test = fun x -> x % (bigint 2) = 0
        IfTrue = 4
        IfFalse = 3
    }
    {
        Id = 1
        Items = [75; 83; 74]
        Operation = fun x -> x + (bigint 7)
        Test = fun x -> x % (bigint 7) = 0
        IfTrue = 5
        IfFalse = 6
    }
    {
        Id = 2
        Items = [86; 67; 61; 96; 52; 63; 73]
        Operation = fun x -> x + (bigint 5)
        Test = fun x -> x % (bigint 3) = 0
        IfTrue = 7
        IfFalse = 0
    }
    {
        Id = 3
        Items = [85; 83; 55; 85; 57; 70; 85; 52]
        Operation = fun x -> x + (bigint 8)
        Test = fun x -> x % (bigint 17) = 0
        IfTrue = 1
        IfFalse = 5
    }
    {
        Id = 4
        Items = [67; 75; 91; 72; 89]
        Operation = fun x -> x + (bigint 4)
        Test = fun x -> x % (bigint 11) = 0
        IfTrue = 3
        IfFalse = 1
    }
    {
        Id = 5
        Items = [66; 64; 68; 92; 68; 77]
        Operation = fun x -> x * (bigint 2)
        Test = fun x -> x % (bigint 19) = 0
        IfTrue = 6
        IfFalse = 2
    }
    {
        Id = 6
        Items = [97; 94; 79; 88]
        Operation = fun x -> x * x
        Test = fun x -> x % (bigint 5) = 0
        IfTrue = 2
        IfFalse = 7
    }
    {
        Id = 7
        Items = [77; 85]
        Operation = fun x -> x + (bigint 6)
        Test = fun x -> x % (bigint 13) = 0
        IfTrue = 4
        IfFalse = 0
    }
|]

let prodMonkeyItems: bigint list array = [|
    [80]
    [75; 83; 74]
    [86; 67; 61; 96; 52; 63; 73]
    [85; 83; 55; 85; 57; 70; 85; 52]
    [67; 75; 91; 72; 89]
    [66; 64; 68; 92; 68; 77]
    [97; 94; 79; 88]
    [77; 85]
|] *)

(*
let foo : (int * bigint) list array = Array.create 10001 []

foo[0] <- [
    (0, 80)
    (1,75) 
    (1, 83) 
    (1, 74)
    (2, 86); (2, 67); (2, 61); (2, 96); (2, 52); (2, 63); (2, 73)
    (3, 85); (3, 83); (3, 55); (3, 85); (3, 57); (3, 70) ; (3, 85) ; (3, 52)
    (4, 67); (4, 75); (4, 91); (4, 72); (4, 89)
    (5, 66); (5, 64); (5, 68); (5, 92); (5, 68); (5, 77)
    (6, 97); (6, 94); (6, 79); (6, 88)
    (7, 77); (7, 85)
]*)


(*

let play2 round (monkey: Monkey) = 
    // printfn "Round %A" round
    let prevLevels = 
        foo[round - 1]
        |> List.filter (fun (i, level) -> i = monkey.Id)
        |> List.map (fun (i, level) -> 
            let worryLevel = (monkey.Operation level) / (bigint 3)
            //printfn "%A" worryLevel
            let toMonkey = if monkey.Test worryLevel then monkey.IfTrue else monkey.IfFalse
            (toMonkey, worryLevel)
        )
    let newLevels = 
        foo[round]
        |> List.filter (fun (i, level) -> i = monkey.Id)
        |> List.map (fun (i, level) -> 
            let worryLevel = (monkey.Operation level) / (bigint 3)
            //printfn "%A" worryLevel
            let toMonkey = if monkey.Test worryLevel then monkey.IfTrue else monkey.IfFalse
            (toMonkey, worryLevel)
        )

    foo[round] <- List.filter (fun (i, l) -> i <> monkey.Id) foo[round] @ prevLevels @ newLevels
    // printfn "%A" <| foo[round]
    visitedItems[monkey.Id] <- visitedItems[monkey.Id] + List.length prevLevels + List.length newLevels
    (*    
    let groups = levels |> List.groupBy fst
    groups
    |> List.iter(fun (i, list) -> 
        prodMonkeyItems[i] <- prodMonkeyItems[i] @ List.map snd list
    ) *)
    //monkeyItems[toMonkey] <- monkeyItems[toMonkey] @ [worryLevel]
    // visitedItems[monkey.Id] <- visitedItems[monkey.Id] + List.length levels
    // prodMonkeyItems[monkey.Id] <- []



let round2 round monkeys = 
    for i in 0 .. Array.length monkeys - 1 do
        play2 round monkeys[i]
        *)

(*let task1 = 
    for i in 1 .. 10000 do
        round2 i testMonkeys
        if i % 100 = 0 then printfn "%A %A" i visitedItems
    testMonkeys
    |> Array.map (fun m -> m.Items)
    |> Array.toList
    visitedItems
    |> Array.toList
    |> List.mapi (fun i visited -> (i, visited))
    |> List.sortByDescending (fun (i, tot) -> tot)
    |> fun l -> int64(snd l[0]) * int64(snd l[1])
*)

// let task2 = "task 2"

printfn $"Task 1: {task1}" // 100345
printfn $"Task 2: {task2}"
