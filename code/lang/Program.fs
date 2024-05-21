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
        printfn "Successful parse"
        // after evalutation, eval should print all the new relationships found
        // from the original BRN provided by the user
        // or if a contradiction is found
        // eval will also print new relationships that result
        // from a query (provided in the input) being sunstituted for an activation or inhibition relationship
        // as well as which of these relationships is possible (or if only no relationship is possible)
        let m = eval ast
        0
    | None ->
        printfn "Invalid program."
        1