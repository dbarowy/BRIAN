module Evaluator

open MathNet.Numerics.LinearAlgebra
open AST
open System

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

let rec matrixDeriveRowWise (m: Matrix<double>) (r:int) (l:int)= 
    if r >= l then
        [m]
    else
        matrixDeriveRowWiseCol m r 0 l false

and matrixDeriveRowWiseCol (m: Matrix<double>) (r:int) (c:int) (l:int) (found:bool) : Matrix<double> list = 
    if c>=l then
        if found then
            []
        else
            if r >=l then
                [m]
            else
                matrixDeriveRowWise m (r+1) l

    else
        let matrixElement = m.[r,c]
        if matrixElement > 0 then
            let matrixCopy = Matrix<double>.Build.DenseOfMatrix(m)
            for col in 0 .. (l-1) do
                if col <> c then
                    matrixCopy.[r,col] <- 0
            let matrixFurtherRows =  matrixDeriveRowWise matrixCopy (r+1) l
            matrixFurtherRows @ (matrixDeriveRowWiseCol m r (c+1) l true)
        else
            matrixDeriveRowWiseCol m r (c+1) l found

let rec transposeMap list =
    match list with
    | [] ->
        []
    | head::tail ->
        (Matrix.transpose(head)) :: (transposeMap tail)

let rec matrixDeriveColWise m = 
    let m = Matrix.transpose(m)
    let mList = matrixDeriveRowWise m 0 m.RowCount
    let mList2 = transposeMap mList
    mList2

let rec colDerivativeMap list =
    match list with
    | [] ->
        []
    | head::tail ->
        (matrixDeriveColWise head) @ (colDerivativeMap tail)

let rec squareMap (list:  Matrix<double> list) =
    match list with
    | [] ->
        []
    | head::tail ->
        (head.Multiply(head)) :: (squareMap tail)

let fourToOnes (m:Matrix<double>) = 
    let length = m.RowCount 
    for row in 0 .. (length-1) do
        for col in 0 .. (length-1) do
            let valueAtM = m.[row,col]
            if valueAtM = 4 then
                m.[row,col] <- 1
    m

let rec addInValuesFromMatrix (m:Matrix<double>) (m2:Matrix<double>) : Matrix<double> =
    let length = m.RowCount 
    for row in 0 .. (length-1) do
        for col in 0 .. (length-1) do
            let valueAtM = m.[row,col]
            let valueAtM2 = m2.[row,col]
            if valueAtM2 > 0 then 
                if valueAtM = 0 then
                    m.[row,col] <- valueAtM2
                //differing values when valueAtM != 0
                elif valueAtM <> valueAtM2 then
                    //4 on a diagnal is not a real contradiction
                    if not (valueAtM = 1 && valueAtM2 = 4) && not (row = col && valueAtM = 2 && valueAtM2 = 4) then
                        m.[0,0] <- 5
    let m = fourToOnes m
    m

let rec addInValuesFromList (m:Matrix<double>)  (mList: Matrix<double> list) : Matrix<double> =
    match mList with
    | [] -> m
    | head::tail ->
        let newMatrix = addInValuesFromMatrix m head
        if (newMatrix.[0,0] = 5) then
            //error message
            newMatrix
        else
            addInValuesFromList newMatrix tail


let rec getMatrixDerivatives (m: Matrix<double>) (iterations: int)= 
    let maxIterations = int (System.Math.Round(Math.Log(m.RowCount, 2)))
    printfn "%A" maxIterations
    if (iterations >= maxIterations) then
        m
    else
        let mListRowExclusion= matrixDeriveRowWise m 0 m.RowCount
        let mListColExclusion = colDerivativeMap mListRowExclusion
        let mListSquare = squareMap mListColExclusion
        let mAdd = addInValuesFromList m mListSquare
        //if (newMatrix.[0,0] = 5) then
        printfn "iteration: %A" iterations
        printfn "%A" mAdd
        getMatrixDerivatives mAdd (iterations + 1)

let eval (s: Sequence) = 
    let M = initializeMatrix s
    printfn "original matrix: "
    printfn "%A" M
    
    let M = getMatrixDerivatives M 0
    //printfn "%A" M
    M
