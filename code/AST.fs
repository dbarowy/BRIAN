module AST

type Variable = 
|   Gene of string
|   Protein of string
|   Phenotype of string
|   Other of string 

type Relation = 
|   Activation of Variable*Variable
|   Inhibition of Variable*Variable

type Sequence = Relation list

