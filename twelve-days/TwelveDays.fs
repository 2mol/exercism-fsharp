module TwelveDays

let things = [
    "a Partridge in a Pear Tree"
    "two Turtle Doves"
    "three French Hens"
    "four Calling Birds"
    "five Gold Rings"
    "six Geese-a-Laying"
    "seven Swans-a-Swimming"
    "eight Maids-a-Milking"
    "nine Ladies Dancing"
    "ten Lords-a-Leaping"
    "eleven Pipers Piping"
    "twelve Drummers Drumming"
]

let days = [
    "first";"second";"third";"fourth";"fifth";"sixth";
    "seventh";"eighth";"ninth";"tenth";"eleventh";"twelfth";
]

let baseSentence (n: int) =
    sprintf "On the %s day of Christmas my true love gave to me: " days.[n - 1]

let rec reciteN (n: int): string =
    let givenThings: string list =
        if n = 1 then [things.[0]]
        else
            (things.[1..n-1] |> List.rev)
            @ ["and " + things.[0]]
    baseSentence n + (String.concat ", " givenThings) + "."

let recite start stop =
    List.map reciteN [start..stop]
    |> fun l -> l.[..stop]
