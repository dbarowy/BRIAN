namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open MathNet.Numerics.LinearAlgebra
open AST
open Parser
open Evaluator

(**
* Test suite for parser.
*)
[<TestClass>]
type TestParser () =

   [<TestMethod>]
    member this.TestMethod0 () =
        let input = "A activates b
                            b activates *c"
        let expected = [Activation (Protein "A", Gene "b");
                                        Activation (Gene "b", Phenotype "*c")]
        let result = parse input
        match result with
        | Some ast ->
            Assert.AreEqual(expected, ast)
        | None -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestMethod1 () =
       let input = "A activates #unknown
                            b inhibits *c"
       let expected = [Activation (Protein "A", Other "#unknown");
                                       Inhibition (Gene "b", Phenotype "*c")]
       let result = parse input
       match result with
       | Some ast ->
           Assert.AreEqual(expected, ast)
       | None -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestMethod2 () =
       let input = "A hates b
                            b inhibits *c"
       let result = parse input
       // since there is no 'hates' relationship in our language,
       // test should fail if an AST is produced
       match result with
       | Some ast -> Assert.IsTrue(false)
       | None -> Assert.IsTrue(true)

(**
* Test suite for evaluator.
*)
[<TestClass>]
type TestEvaluator () =

    // test to see if 'prettyprint' function is working correctly
    [<TestMethod>]
    member this.TestMethod3 () = 
        let input = [Activation (Protein "A", Gene "b");
                                        Activation (Gene "b", Phenotype "*c")]
        let result = prettyprint input
        let expected = "A activates b\nb activates *c\n"
        Assert.AreEqual(result, expected)

    // test to see if 'getUniqueVariableList' function is working correctly
    [<TestMethod>]
    member this.TestMethod4 () = 
            let input = [Activation (Protein "A", Gene "b");
                                        Activation (Gene "b", Phenotype "*c")]
            let result = getUniqueVariableList input
            let expected = [Protein "A"; Gene "b"; Phenotype "*c"]
            Assert.AreEqual(result, expected)
    
    // test to see if 'initializeMatrixs' function is working correctly
    [<TestMethod>]
    member this.TestMethod5 () = 
            let input = [Activation (Protein "A", Gene "b");
                                        Activation (Gene "b", Phenotype "*c")]
            let result = initializeMatrix input
            let x = {{0, 1, 0}, {0, 0, 1}, {0, 0, 0}}
            let expected = Matrix<double>.Build.DenseOfArray(x)
            Assert.AreEqual(result, expected)