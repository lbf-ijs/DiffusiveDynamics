(* Mathematica Test File *)
Needs["DiffusiveDynamics`Analyze2D`"];
(*compile the function*)
DiffusiveDynamics`Analyze2D`Private`compiledGetContigIntervalsUncompiled[];

Test[(*test easy contig *)
    ind={1, 2, 3, 6, 7};
    compiledGetContigIntervals@ind
    ,
    {{1, 3}, {6, 7}}
    ,
    TestID->"Analyze2D->contig easy"
]

Test[
    ind={1, 2, 3, 6, 7, 10};
    compiledGetContigIntervals@ind
    ,
    {{1, 3}, {6, 7}, {10,10}}
    ,
    TestID->"Analyze2D->contig one at end"
]

Test[
    ind={1,  3,  7, 10};
    compiledGetContigIntervals@ind
    ,
    {{1, 1}, {3, 3}, {7,7},{10,10}}
    ,
    TestID->"Analyze2D->contig all single"
]

Test[
    DiffusiveDynamics`Analyze2D`Private`AverageOneDiffBin[{{"x" -> 1}, {"x" -> 2}, {"x" -> 3}}, {"x"}]
    ,
    {"x" -> 2, "xError" -> 1}
    ,
    TestID->"Analyze2D->AverageOneDiffBin->test average one diff bin (one rule)"
]

Test[
    DiffusiveDynamics`Analyze2D`Private`AverageOneDiffBin[
        {{"x" -> 1, "y" -> 2}, {"x" -> 2, "z" -> "w"}, {"x" -> 3, "z" -> "zzzz"}}, 
         {"x"}]
    ,
    {"x" -> 2, "y" -> 2, "z" -> "w", "xError" -> 1}
    ,
    TestID->"Analyze2D->AverageOneDiffBin->test average one diff bin (other members are preserved, but only the first occurance)"
]

Test[
DiffusiveDynamics`Analyze2D`Private`AverageOneDiffBin[
    {{"x" -> 1, "y" -> 2}, {"x" -> 2, "z" -> "w", "y" -> 2}, {"x" -> 3, "z" -> "zzzz", "y" -> 5}}
   , {"x", "y"}]
    ,
    {"x" -> 2, "y" -> 3, "z" -> "w", "xError" -> 1, "yError" -> Sqrt[3]}
    ,
    TestID->"Analyze2D->AverageOneDiffBin->test average one diff bin (two rules)"
]

Test[

EstimateDiffusionError[
     {{{{"s" -> 1, "x" -> 1}, {"s" -> 2, "x" -> 10}}},
      {{{"s" -> 1, "x" -> 2}, {"s" -> 2, "x" -> 20}}},
      {{{"s" -> 1, "x" -> 3}, {"s" -> 2, "x" -> 30}}}},
     "Quantities" -> {"x"}]
    ,
    {{{"s" -> 1, "x" -> 2, "xError" -> 1}, {"s" -> 2, "x" -> 20, "xError" -> 10}}}
    ,
    TestID->"Analyze2D->EstimateDiffusionError->basic test (one bin)"
]


diff1 = {{"Dx" -> 10, "Dy" -> 15, "Da" -> 0}};
diff2 = {{"Dx" -> 10, "Dy" -> 15, "Da" -> 90}};
diffMissing = {{"Dx" -> 10, "Dy" -> Missing[], "Da" -> Missing[]}};

Test[
    GetDiffusionsRMSD[diff1, diff2, DistanceFunction -> EuclideanDistance]
    ,
    5
    ,
    TestID->"Analyze2D->GetDiffusionsRMSD->basic test (one bin)"
]

Test[
    GetDiffusionsRMSD[diff1, diffMissing, DistanceFunction -> EuclideanDistance]
    ,
    Missing["To little points with valid diffusions"]
    ,
    TestID->"Analyze2D->GetDiffusionsRMSD->All missing"
]

Test[
    GetDiffusionsRMSD[diff1~Join~diff1, diffMissing~Join~diff2, DistanceFunction -> EuclideanDistance]
    ,
    5
    ,
    TestID->"Analyze2D->GetDiffusionsRMSD->Some missing"
]
