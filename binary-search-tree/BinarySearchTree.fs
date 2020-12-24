module BinarySearchTree

type BinaryTree<'A> =
    Node of 'A * (BinaryTree<'A> option) * (BinaryTree<'A> option)

let rec insert
    (Node (elemNode, left, right) : BinaryTree<'A>)
    (elemNew : 'A)
    : BinaryTree<'A> =
    let
        createChildNode node elem =
            // if child node is empty, create a new one
            // if not empty, recursively insert elem into child node
            match node with
            | None -> Node (elem, None, None)
            | Some childNode -> insert childNode elem
    if elemNew <= elemNode
    then
        // insert smaller on the left, leave right unchanged
        let nodeNew = createChildNode left elemNew
        Node (elemNode, Some nodeNew, right)
    else
        // insert larger on the right, leave left unchanged
        let nodeNew = createChildNode right elemNew
        Node (elemNode, left, Some nodeNew)

let create items =
    match items with
    | [] -> failwith "can't make an empty tree, sozzers!"
    | x::xs ->
        Seq.fold
            insert
            (Node (x, None, None))
            xs

let left (Node (_, left, _)) = left

let right (Node (_, _, right)) = right

let data (Node (elem, _, _)) = elem

let rec sortedData (Node (elem, left, right)) =
    match left, right with
    | None, None -> [elem]
    | Some nodeLeft, None -> List.append (sortedData nodeLeft) [elem]
    | None, Some nodeRight -> elem :: (sortedData nodeRight)
    | Some nodeLeft, Some nodeRight ->
        List.append (sortedData nodeLeft) (elem :: (sortedData nodeRight))
