module SumOfMultiples

let multiples (upperBound: int) (number: int) : Set<int> =
    if number = 0 then Set.ofList [0]
    else
    [number..(upperBound-1)]
    |> List.filter (fun n -> n % number = 0)
    |> Set.ofList

let sum (numbers: int list) (upperBound: int): int =
    let allMultiples =
        List.map (multiples upperBound) numbers
        |> Set.unionMany
    Seq.sum allMultiples
