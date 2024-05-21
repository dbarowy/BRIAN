open Parser
open Evaluator
open System
open MathNet.Numerics.LinearAlgebra

(**
* This program takes a text file input given to `dotnet run`
* and attempts to parse and evaluate the BRN described in the file.
* Upon successful evaluation, it prints out all newfound relationships 
* from the BRN or if evaluation is unsuccessful, supplies the user
* with a helpful message, which could include discovery of a contradiction
* in the BRN.
* 
* @pre: path to text file containing the BRN in string form
* @return: depending on identified relationships in the supplied BRN, prints:
*         - newfound relationships between all variables in the BRN;
*         - if a user supplies a query between a pair of variables,
*           the possible types of relationships between them as well as downstream 
*           effects if these relationships were added to the BRN
*         - identification of a contradictory relationship between
*           a pair of relationships in the supplied BRN.
*     Or tells the user that the input provided was invalid (i.e. unable to be parsed)
*)
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
        // eval will also print new relationships that result
        // from a query (provided in the input) being substituted for an activation or inhibition relationship
        // as well as which of these relationships is possible (or if only no relationship is possible)
        let m = eval ast
        0
    | None ->
        printfn "Invalid program."
        1