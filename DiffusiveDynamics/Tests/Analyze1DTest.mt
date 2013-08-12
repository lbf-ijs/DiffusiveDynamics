(* Mathematica Test File *)
Needs["DiffusiveDynamics`Analyze1D`"];
(*compile the function*)
DiffusiveDynamics`Analyze1D`Private`compiledSelectBinUncompiled[];

Test[(*test easy contig *)
    data={{1.,1.5},{2.,2.5},{3.,3.5}};
    compiledSelectBin1D[data,1,3]
    ,
    {{1.,1.5},{2.,2.5}}
    ,
    TestID->"Analyze2D->compiledSelectBin1D"
]

Test[(*test GetBinCentersAndWidths1D*)
    DiffusiveDynamics`Analyze1D`Private`GetBinCentersAndWidths1D[{0,1000,500}]
    ,
    {{250,500},{750,500}}
    ,
    TestID->"Analyze2D->GetBinCentersAndWidths1D"
]

Test[
    DiffusiveDynamics`Analyze1D`Private`GetBinsFromBinsOrSpec1D[{0,1000,500}]
    ,
    {{250,500},{750,500}}
    ,
    TestID->"Analyze2D->GetBinsFromBinsOrSpec1D from spec"
]

Test[
    data={{50,100},{150,100},{250,100}};
    DiffusiveDynamics`Analyze1D`Private`GetBinsFromBinsOrSpec1D[data]
    ,
    data
    ,
    TestID->"Analyze2D->GetBinsFromBinsOrSpec1D from bins"
]
(*TODO: Test also incorrect input. How is this done in MUnit*)

Test[
    data={{50,100},{150,100},{250,100}};
    DiffusiveDynamics`Analyze1D`Private`GetCellRangeFromBins1D[data]
    ,
    {0,300}
    ,
    TestID->"Analyze2D->GetCellRangeFromBins1D same width bins"
]

Test[
    data={{0,200},{1000,400}};
    DiffusiveDynamics`Analyze1D`Private`GetCellRangeFromBins1D[data]
    ,
    {-100,1200}
    ,
    TestID->"Analyze2D->GetCellRangeFromBins1D diffrent width bins"
]


