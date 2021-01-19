module Leap

let leapYear (year: int) : bool =
    let yearIsDivBy (divisor: int) =
        year % divisor = 0
    yearIsDivBy 400 || (yearIsDivBy 4 && not (yearIsDivBy 100))

