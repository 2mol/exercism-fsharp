module Leap

let isDivisibleBy (n: int) (divisor: int) : bool =
    n % divisor = 0

let leapYear (year: int) : bool =
    let yearIsDivBy = isDivisibleBy year
    yearIsDivBy 400 || (yearIsDivBy 4 && not <| yearIsDivBy 100)

