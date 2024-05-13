module Evaluator

open MathNet.Numerics.LinearAlgebra
open AST

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
            //printfn "%A" m
            // set all other elements of the row to 0
            for col in 0 .. (l-1) do
                if col <> c then
                    matrixCopy.[r,col] <- 0
            let matrixFurtherRows =  matrixDeriveRowWise matrixCopy (r+1) l
            //printfn "%A" m
            //printfn "%A" (matrixDeriveRowWiseCol m r (c+1) l true)
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

let getMatrixDerivatives (m: Matrix<double>) = 
    let mListRowExclusion= matrixDeriveRowWise m 0 m.RowCount
    let mListColExclusion = colDerivativeMap mListRowExclusion
    printfn "%A" mListColExclusion
    mListColExclusion
