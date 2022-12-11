
open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

type Monkey = {
    Id: int
    Items:bigint list
    Operation: bigint-> bigint
    Test: bigint -> bool
    IfTrue: int
    IfFalse: int
}

let testMonkeys = [|
    { 
        Id = 0
        Items = [79L; 98L]
        Operation = fun x -> x * (bigint 19)
        Test = fun x -> x % (bigint 23) = 0
        IfTrue = 2
        IfFalse = 3
    }
    { 
        Id = 1
        Items = [54L; 65L; 75L; 74L]
        Operation = fun x -> x + (bigint 6)
        Test = fun x -> x % (bigint 19) = 0
        IfTrue = 2
        IfFalse = 0
    }
    { 
        Id = 2
        Items = [79L; 60L; 97L]
        Operation = fun x -> x * x
        Test = fun x -> x % (bigint 13) = 0
        IfTrue = 1
        IfFalse = 3
    }
    { 
        Id = 3
        Items = [74L]
        Operation = fun x -> x + (bigint 3)
        Test = fun x -> x % (bigint 17) = 0
        IfTrue = 0
        IfFalse = 1
    }
|]

type Operation = Double | Muliply of int | Add of int

type Monkey2 = {
    Id: int
    Items: map list
    Operation: Operation
    Test: int
    IfTrue: int
    IfFalse: int
}

let testMonkeys: Monkey2 array = [|
    { 
        Id = 0
        Items = [Map.ofList [(79, 1)]; Map.ofList [(2, 1); (7, 2)]]
        Operation = Multiply 19
        Test = 23
        IfTrue = 2
        IfFalse = 3
    }
    { 
        Id = 1
        Items = [
            Map.ofList [(3, 1); (2, 3)]
            Map.ofList [(5, 1); (13, 1)]
            Map.ofList [(3, 1); (5, 2)]
            54L; 65L; 75L; 74L
            ]
        Operation = fun x -> Add 6
        Test = 19
        IfTrue = 2
        IfFalse = 0
    }
    { 
        Id = 2
        Items = [79L; 60L; 97L]
        Operation = Double
        Test = 13
        IfTrue = 1
        IfFalse = 3
    }
    { 
        Id = 3
        Items = [74L]
        Operation = Add 3
        Test = 17
        IfTrue = 0
        IfFalse = 1
    }
|]

(*
let monkeyItems: bigint list array  = [|
     [79L; 98L]
     [54L; 65L; 75L; 74L]
     [79L; 60L; 97L]
     [74L]
|]*)

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
|]

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
]


let visitedItems = Array.create 8 0

let play (monkeys: Monkey array) (monkey: Monkey) = 
    monkey.Items
    |> List.iter (fun i -> 
        let worryLevel = (monkey.Operation i) / (bigint 3)
        //printfn "%A" worryLevel
        let toMonkey = if monkey.Test worryLevel then monkey.IfTrue else monkey.IfFalse
        let newMonkey = monkeys[toMonkey]
        monkeys[toMonkey] <- { newMonkey with Items = newMonkey.Items @ [worryLevel]}
        visitedItems[monkey.Id] <- visitedItems[monkey.Id] + 1
    )
    { monkey with Items = []}

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

let round monkeys = 
    for i in 0 .. Array.length monkeys - 1 do
        monkeys[i] <- play monkeys monkeys[i]

let round2 round monkeys = 
    for i in 0 .. Array.length monkeys - 1 do
        play2 round monkeys[i]

let task1 = 
    for i in 1 .. 10000 do
        round2 i prodMonkeys
        if i % 100 = 0 then printfn "%A %A" i visitedItems
    testMonkeys
    |> Array.map (fun m -> m.Items)
    |> Array.toList
    visitedItems
    |> Array.toList
    |> List.mapi (fun i visited -> (i, visited))
    |> List.sortByDescending (fun (i, tot) -> tot)
    |> fun l -> int64(snd l[0]) * int64(snd l[1])

let task2 = "task 2"

printfn $"Task 1: {task1}" // 100345
printfn $"Task 2: {task2}"
