namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser
open Evaluator


[<TestClass>] // test suite: should test one module
type TestClass () =

   [<TestMethod>]
   member this.TestMethodPassing () =
       let input = "gene1 inhibits Protein1
                           Protein2 activates *phenotype1
                           Protein2 inhibits gene3
                           *phenotype2 activates gene3
                           #unknown_siRNA inhibits gene4-a"
       let expected = [Inhibition (Gene "gene1", Protein "Protein1");
                                       Activation (Protein "Protein2", Phenotype "*phenotype1");
                                       Inhibition (Protein "Protein2", Gene "gene3");
                                       Activation (Phenotype "*phenotype2", Gene "gene3");
                                       Inhibition (Other "#unknown_siRNA", Gene "gene4-a")]
       let result = parse input
       match result with
       | Some ast ->
           Assert.AreEqual(expected, ast)
       | None -> Assert.IsTrue(false)
