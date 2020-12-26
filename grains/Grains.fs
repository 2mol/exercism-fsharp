module Grains

let square (n: int): Result<uint64,string> =
    if n > 0 && n <= 64
    then Ok (pown 2UL (n-1))
    else Error "square must be between 1 and 64"

let addIfOk acc number =
    match number with
    | Ok n -> acc + n
    | Error _ -> acc

let total: Result<uint64,string> =
    List.map square [1..64]
    |> List.fold addIfOk 0UL
    |> Ok
