module QueenAttack

let isValidCoordinate (coord : int) : bool =
    coord >= 0 && coord < 8

let create (position: int * int) =
    isValidCoordinate (fst position)
    && isValidCoordinate (snd position)

let canAttack (queen1 : int * int) (queen2 : int * int) : bool =
    // normalize the position, so that queen1 would be at (0, 0)
    let (x, y) = (fst queen2 - fst queen1, snd queen2 - snd queen1)

    (create queen1 && create queen2) &&
        ( x = 0
        || y = 0
        || abs x = abs y
        )
