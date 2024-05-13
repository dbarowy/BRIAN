open Parser
open Evaluator
open System
open MathNet.Numerics.LinearAlgebra

[<EntryPoint>]
let main args =
    (* Check that input is given *)
    if (args.Length <> 1) then
        printfn "Usage: dotnet run <file.txt>"
        exit 1
    let file = args[0]
    let text = IO.File.ReadAllText file
    match parse text with
    | Some ast ->
        printfn "success"
        printfn "%A" ast
        let m = eval ast
        0
    | None ->
        printfn "Invalid program."
        1