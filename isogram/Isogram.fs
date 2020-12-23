module Isogram

let rec isIsogramHelper (seenChars : char Set) (remainingChars : char list) : bool =
    match remainingChars with
    | [] -> true
    | c::cs ->
        if Set.contains c seenChars then false
        else isIsogramHelper (Set.add c seenChars) cs

let isIsogram (str : string): bool =
    let alphabet = (Set.ofList ['a'..'z'])
    str
    |> String.map System.Char.ToLower
    |> Seq.filter (fun char -> Set.contains char alphabet)
    |> Seq.toList
    |> isIsogramHelper Set.empty
