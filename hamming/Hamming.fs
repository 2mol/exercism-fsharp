module Hamming

let rec distanceHelper (strand1: char list) (strand2: char list): int option =
    match strand1, strand2 with
    | [], [] -> Some(0)
    | x::xs, y::ys ->
        if x = y
            then distanceHelper xs ys
            else Option.map((+) 1) (distanceHelper xs ys)
    | _ -> None

let distance (strand1: string) (strand2: string): int option =
    distanceHelper (Seq.toList strand1) (Seq.toList strand2)
