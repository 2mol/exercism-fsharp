module Pangram


let isPangram (input: string): bool =
    let charset =
        Seq.toList input
        |> List.map (System.Char.ToLower)
        |> Set.ofList
    Set.isSubset (Set.ofList ['a'..'z']) charset
