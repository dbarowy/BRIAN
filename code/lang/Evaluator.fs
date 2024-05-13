module Evaluator

open MathNet.Numerics.LinearAlgebra
open AST

(** 
* Helper method to prettyprint method.
*)
let variableprint (v: Variable) : string = 
    match v with
    | Gene(s) -> s
    | Protein(s) -> s
    | Phenotype(s) -> s
    | Other(s) -> s

let relationprint (r: Relation) : string =
    match r with
    | Activation(v1, v2) -> (variableprint v1) + " activates " + (variableprint v2)
    | Inhibition(v1, v2) -> (variableprint v1) + " inhibits " + (variableprint v2)

let rec prettyprint (s: Sequence) : string =
    match s with
    | [] -> ""
    | x::xs -> (relationprint x) + "\n" + (prettyprint xs)

let variablesFromRelation (r: Relation) : Variable list =
    match r with
    | Activation(v1, v2) -> [v1; v2]
    | Inhibition(v1, v2) -> [v1; v2]

let rec getVariableList (s: Sequence) : Variable list= 
    match s with
        | [] -> []
        | x::xs -> 
            (variablesFromRelation x) @ (getVariableList xs)

let getUniqueVariableList (s: Sequence) =
    let newList = getVariableList s
    Seq.distinct newList |> List.ofSeq

let initializeMatrix (s: Sequence) =
    let uniqueList = getUniqueVariableList s
    let length = List.length uniqueList
    let mutable m = Matrix<double>.Build.Dense(length, length)
    for relation in s do
        match relation with
        | Activation(v1, v2) ->  
            let index1 = List.findIndex (fun x -> x = v1) uniqueList
            let index2 = List.findIndex (fun x -> x = v2) uniqueList
            m.[index1,index2] <- 1
        | Inhibition(v1, v2) -> 
            let index1 = List.findIndex (fun x -> x = v1) uniqueList
            let index2 = List.findIndex (fun x -> x = v2) uniqueList
            m.[index1,index2] <- 2
    m
