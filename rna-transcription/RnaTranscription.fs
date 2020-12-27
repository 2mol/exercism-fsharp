module RnaTranscription

let translate (c : System.Char) : System.Char =
    match c with
    | 'G' -> 'C'
    | 'C' -> 'G'
    | 'T' -> 'A'
    | 'A' -> 'U'
    | _ -> failwith "invalid character"

let toRna (dna: string): string =
    Seq.map (string << translate) dna
    |> String.concat ""
