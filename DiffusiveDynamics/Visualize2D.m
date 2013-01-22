(* Mathematica Package *)

BeginPackage["DiffusiveDynamics`Visualize2D`",{"DiffusiveDynamics`Utils`"}]
(* Exported symbols added here with SymbolName::usage *)  

   
ClearAll[DiffusionParameters2DPlots];
DiffusionParameters2DPlots::usage="TODO";

QucikDensityHistogram::usage="TODO";

Begin["`Private`"] (* Begin Private Context *) 

ClearAll[QucikDensityHistogram];
SetAttributes[QucikDensityHistogram,HoldFirst];
QucikDensityHistogram[data_,binspec_,hspec_:Automatic] :=
    Module[ {bins,counts,datarange,wh},
        If[ Length[data]===0,
            Return[Missing[]]
        ];
        {bins,counts} = HistogramList[data,binspec,hspec];
        ClearAll["System`HistogramListDump`modelData$*"]; (*fix memory leak*)
        datarange = bins[[All,{1,-1}]];
        wh = Flatten[Differences[#]&/@datarange];
        ListContourPlot[Transpose@counts,InterpolationOrder->0,DataRange->datarange,AspectRatio->wh[[2]]/wh[[1]],
              ColorFunction->(If[ #<.1,
                                  White,
                                  ColorData[{"SolarColors","Reverse"}][#]
                              ]&),
              PlotRange->Full, ImageSize->Medium]
    ];

ClearAll[ShowStepsDensityHistogram];
ShowStepsDensityHistogram[steps_] :=
    Module[ {g,plr},
        g = DensityHistogram[steps,{"Raw",25},PerformanceGoal->"Speed"];
        plr = {Min@#1,Max@#2}&@@Transpose[PlotRange/.Options[g]];
        Return[Rasterize[Show[g,PlotRange->{plr,plr}],ImageSize->Medium]];
    ];


DiffusionParameters2DPlots[Dx_,Dy_,Da_,Energy_,CellRange_] :=
    Module[ {br = (2 CellRange/Max[CellRange])~Join~{1}, plotRange = Transpose[{-CellRange, CellRange}]},
        {Plot3D[Dx[x, y], {x, CellRange[[1]], -CellRange[[1]]}, {y, 
        CellRange[[2]], -CellRange[[2]]}, BoxRatios -> br, 
        ImageSize -> Small, PlotLabel -> "DIFF X"],
        Plot3D[Dy[x, y], {x, CellRange[[1]], -CellRange[[1]]}, {y, 
        CellRange[[2]], -CellRange[[2]]}, BoxRatios -> br, 
        ImageSize -> Small, PlotLabel -> "DIFF Y"],
        Plot3D[Da[x, y], {x, CellRange[[1]], -CellRange[[1]]}, {y, 
        CellRange[[2]], -CellRange[[2]]}, BoxRatios -> br, 
        ImageSize -> Small, PlotLabel -> "ALPHA"],
        Plot3D[Energy[x, y], {x, CellRange[[1]], -CellRange[[1]]}, {y, 
        CellRange[[2]], -CellRange[[2]]}, BoxRatios -> br, 
        ImageSize -> Small, PlotLabel -> "ENERGY"]}
    ];


End[] (* End Private Context *)

EndPackage[]