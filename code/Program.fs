open Parser
open Evaluator
open System

[<EntryPoint>]
let main args =
    let file = args[0]
    let text = IO.File.ReadAllText file
    match parse text with
    | Some ast ->
        printfn "success"
        printfn "%A" ast
        let res = prettyprint ast
        printfn "%s" res
        0
    | None ->
        printfn "Invalid program."
        1