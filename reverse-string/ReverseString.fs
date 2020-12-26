module ReverseString

let reverse (input: string): string =
    Seq.rev input
    |> Seq.map string
    |> String.concat ""
