// This is the file you need to modify for your own solution.
// The unit tests will use this code, and it will be used by the benchmark tests
// for the "Mine" row of the summary table.

// Remember to not only run the unit tests for this exercise, but also the
// benchmark tests using `dotnet run -c Release`.
// Please refer to the instructions for more information about the benchmark tests.

module TreeBuilding

open TreeBuildingTypes

type Tree = Map<int, int list> * int

let recordId (t, nodeId) =
    nodeId

let children ((t, nodeId) : Tree) : Tree list  =
    t.TryFind nodeId
    |> Option.defaultValue []
    |> List.map (fun i -> (t, i))

let isBranch (tree : Tree) : bool =
    not <| List.isEmpty (children tree)

let insertOrAdd
    (dict : Map<int, int list>)
    (record : Record)
    : Map<int, int list>
    =
    if record.RecordId = 0 && record.ParentId = 0 then
        // skip fucking root node
        dict
    elif record.RecordId <= record.ParentId then
        failwith "child can't have lower id than its parent"
    elif record.RecordId = 0 then
        failwith "invalid root node"
    else
        let subList =
            dict.TryFind record.ParentId
            |> Option.defaultValue []
        dict.Add (record.ParentId, List.sort <| record.RecordId :: subList)

let buildTree (records : list<Record>) : Tree =
    match records with
    | [] -> failwith "Empty input"
    | _ ->
        let nodesDict : Map<int, int list> =
            List.fold insertOrAdd Map.empty records
        (nodesDict, 0)
