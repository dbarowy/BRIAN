open Parser
open Evaluator
open System
open MathNet.Numerics.LinearAlgebra

[<EntryPoint>]
let main args =
    let file = args[0]
    let text = IO.File.ReadAllText file
    match parse text with
    | Some ast ->
        printfn "success"
        printfn "%A" ast
        let uniqueList = getUniqueVariableList ast
        printfn "List of all components:" 
        printfn "%A" uniqueList
        let M = initializeMatrix ast
        printfn "%A" M
        0
    | None ->
        printfn "Invalid program."
        1