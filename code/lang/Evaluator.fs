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
                    //avoids not real contradictions
                    if not (valueAtM = 1 && valueAtM2 = 4) && not (row = col && valueAtM = 2 && valueAtM2 = 4) then
                        m.[row,col] <- 5
    let m = fourToOnes m
    m

let rec findFive (m:Matrix<double>) (r:int) (c:int) (l: int) : bool = 
    if (l <= 0 ) then
        false
    elif (c < l && r < l && m.[r,c] = 5) then
        true
    else
        if c >= l then
            if r >= l then
                false
            else
              findFive m (r+1) 0 l  
        else
            findFive m r (c+1) l  

let rec addInValuesFromList (m:Matrix<double>)  (mList: Matrix<double> list) : Matrix<double> =
    match mList with
    | [] -> m
    | head::tail ->
        let newMatrix = addInValuesFromMatrix m head
        if (findFive newMatrix 0 0 newMatrix.RowCount) then
            //error message
            newMatrix
        else
            addInValuesFromList newMatrix tail


let rec getMatrixDerivatives (m: Matrix<double>) (iterations: int) = 
    printfn("derivations")
    printfn("%A") m
    let maxIterations = int (System.Math.Ceiling(Math.Log(m.RowCount, 2)))
    if (iterations >= maxIterations) then
        m
    else
        let mListRowExclusion= matrixDeriveRowWise m 0 m.RowCount
        let mListColExclusion = colDerivativeMap mListRowExclusion
        let mListSquare = squareMap mListColExclusion
        printfn("%A") mListSquare
        let mAdd = addInValuesFromList m mListSquare
        //if (newMatrix.[0,0] = 5) then
        getMatrixDerivatives mAdd (iterations + 1)

let rec getNewRelationships (m1:Matrix<double>) (m2:Matrix<double>) (variableList: Variable list) (r:int) (c:int) (l: int) (relationList: Relation list) : Relation list = 
    if (c < l && r < l) then
        if (m2.[r,c] = 5) then
            
            let variable1 = variableList[r]
            let variable2 = variableList[c]
            let contradictoryRelationship = 
                if (m2.[r,c] = 1) then
                    Activation(variable1, variable2)
                elif (m2.[r,c] = 2) then
                    Inhibition(variable1, variable2)
                //default
                else
                    Activation(variable1, variable2)
            let contradictionString = prettyprint [contradictoryRelationship]
            let message = "Contradiction Found At: " + contradictionString
            failwith message
            exit 1

    let relationList = 
        if (c < l && r < l) then
            if (m1.[r,c] = 0 && m2.[r,c] > 0) then
                let variable1 = variableList[r]
                let variable2 = variableList[c]
                if (m2.[r,c] = 1) then
                    let foundRelationship = Activation(variable1, variable2)
                    relationList @ [foundRelationship]
                elif (m2.[r,c] = 2) then
                    let foundRelationship = Inhibition(variable1, variable2)
                    relationList @ [foundRelationship]
                else 
                    relationList
            else 
                relationList
        else
            relationList
    if c >= l then
        if r >= l then
            relationList
        else
            getNewRelationships m1 m2 variableList (r+1) 0 l relationList
    else
        getNewRelationships m1 m2 variableList r (c+1) l relationList

let eval (s: Sequence) = 
    let uniqueList = getUniqueVariableList s
    //printfn "list of unique components: "
    //printfn "%A" uniqueList
    let M = initializeMatrix s
    let originalM = Matrix<double>.Build.DenseOfMatrix(M)
    //printfn "original matrix: "
    //printfn "%A" M
    let M = getMatrixDerivatives M 0
    //printfn "derivative add matrix: "
    //printfn "%A" M
    printfn "New found relations: "
    let foundRelations = 
        try
        getNewRelationships originalM M uniqueList 0 0 M.RowCount []
        with
        | Failure(msg) -> printfn "%s" msg; []
    printfn "%s" (prettyprint foundRelations)
    foundRelations
    
