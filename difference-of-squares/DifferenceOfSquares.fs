module DifferenceOfSquares

let squareOfSum (number: int): int =
    pown (List.sum [1..number]) 2

let sumOfSquares (number: int): int =
    [1..number]
    |> List.sumBy (fun n -> pown n 2)

let differenceOfSquares (number: int): int =
    (squareOfSum number) - (sumOfSquares number)
