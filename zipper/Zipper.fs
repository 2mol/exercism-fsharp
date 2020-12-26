module Zipper

type Tree<'a> =
    { Node : 'a
    ; Left : Tree<'a> option
    ; Right : Tree<'a> option
    }

type Side = Left | Right

type Provenance<'a> =
    { ParentNode : 'a
    ; SiblingTree : Tree<'a> option
    ; Side : Side
    }

type Zipper<'a> =
    { FocusedTree : Tree<'a>
    ; Provenances : Provenance<'a> list
    }

let tree value left right =
    { Node = value
    ; Left = left
    ; Right = right
    }

let fromTree tree =
    { FocusedTree = tree
    ; Provenances = []
    }

let downSideways (side : Side) (zipper : Zipper<'a>) : Zipper<'a> option =
    let newFocusTree =
        match side with
        | Left -> zipper.FocusedTree.Left
        | Right -> zipper.FocusedTree.Right
    match newFocusTree with
    | None -> None
    | Some subTree ->
        let newProvenance =
            { ParentNode = zipper.FocusedTree.Node
            ; SiblingTree =
                match side with
                    | Left -> zipper.FocusedTree.Right
                    | Right -> zipper.FocusedTree.Left
            ; Side = side
            }
        Some
            { FocusedTree = subTree
            ; Provenances = newProvenance :: zipper.Provenances
            }

let left (zipper : Zipper<'a>) : Zipper<'a> option =
    downSideways Left zipper

let right (zipper : Zipper<'a>) : Zipper<'a> option =
    downSideways Right zipper

let value (zipper : Zipper<'a>) : 'a =
    zipper.FocusedTree.Node

let setValue (newValue : 'a) (zipper : Zipper<'a>) : Zipper<'a> =
    let newFocusTree =
        { Node = newValue
        ; Left = zipper.FocusedTree.Left
        ; Right = zipper.FocusedTree.Right
        }
    { FocusedTree = newFocusTree
    ; Provenances = zipper.Provenances
    }

let up (zipper : Zipper<'a>) : Zipper<'a> option =
    match zipper.Provenances with
    | [] -> None
    | x::xs ->
        let newFocusedTree =
            match x.Side with
            | Left ->
                { Node = x.ParentNode
                ; Left = Some zipper.FocusedTree
                ; Right = x.SiblingTree
                }
            | Right ->
                { Node = x.ParentNode
                ; Left = x.SiblingTree
                ; Right = Some zipper.FocusedTree
                }
        Some
            { FocusedTree = newFocusedTree
            ; Provenances = xs
            }

let rec toTree zipper =
    match zipper.Provenances with
    | [] ->
        zipper.FocusedTree
    | x::xs ->
        match up zipper with
        | Some z -> toTree z
        | None -> failwith "should never happen, check your brain"

let setRight (subTree : Tree<'a> option) (zipper : Zipper<'a>) : Zipper<'a>=
    let newFocusTree =
        { Node = zipper.FocusedTree.Node
        ; Left = zipper.FocusedTree.Left
        ; Right = subTree
        }
    { FocusedTree = newFocusTree
    ; Provenances = zipper.Provenances
    }

let setLeft (subTree : Tree<'a> option) (zipper : Zipper<'a>) : Zipper<'a>=
    let newFocusTree =
        { Node = zipper.FocusedTree.Node
        ; Left = subTree
        ; Right = zipper.FocusedTree.Right
        }
    { FocusedTree = newFocusTree
    ; Provenances = zipper.Provenances
    }
