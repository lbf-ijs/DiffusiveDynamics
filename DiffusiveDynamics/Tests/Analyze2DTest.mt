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