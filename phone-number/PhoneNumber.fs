module PhoneNumber

let punctuationChars: Set<char> = Set.ofSeq "- .()+"
let alphabet: Set<char> = (Set.ofList ['a'..'z'])
let badPunctuation: Set<char> = Set.ofSeq "~!@#$%^&*:"


let cleanHelper
    (n1: char)
    (n2: char)
    (trimmedInput: string)
    : Result<uint64, string> =
    if String.exists System.Char.IsLetter trimmedInput
        then Error "letters not permitted"
    elif String.exists (fun c -> Set.contains c badPunctuation) trimmedInput
        then Error "punctuations not permitted"
    elif n1 = '0' then Error "area code cannot start with zero"
    elif n1 = '1' then Error "area code cannot start with one"
    elif n2 = '0' then Error "exchange code cannot start with zero"
    elif n2 = '1' then Error "exchange code cannot start with one"
    else
        match System.UInt64.TryParse trimmedInput with
        | (true, n) -> Ok n
        | (false, _) -> Error "unknown invalid character"


let clean (input: string): Result<uint64, string> =
    let strippedInput =
        String.filter (fun c -> not <| Set.contains c punctuationChars) input

    // (NXX)-NXX-XXXX
    // N: 2-9
    // X: 0-9
    // ignore characters: - + . ( )
    match Seq.toList strippedInput with
    | i when Seq.length i < 10 -> Error "incorrect number of digits"
    | i when Seq.length i > 11 -> Error "more than 11 digits"
    | [   n1;_;_;n2;_;_;_;_;_;_] ->
        cleanHelper n1 n2 strippedInput
    | [c0;n1;_;_;n2;_;_;_;_;_;_] ->
        if c0 <> '1' then Error "11 digits must start with 1"
        else cleanHelper n1 n2 strippedInput.[1..]
    | _ -> failwith "unreachable branch"
