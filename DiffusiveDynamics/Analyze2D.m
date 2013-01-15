(* Mathematica Package *)

(* Created by the Wolfram Workbench 14.1.2013 *)

BeginPackage["DiffusiveDynamics`Analyze2D`"]
(* Exported symbols added here with SymbolName::usage *) 

ClearAll[TestPrint];
TestPrint::usage="TestPrint[] print of string"; 

ClearAll[DiffusionParameters2DPlots];
DiffusionParameters2DPlots::usage="TODO";

Begin["`Private`"]
(* Implementation of the package *)

TestPrint[msg_]:=Block[{$VerbosePrint=True},
	Puts["This is a message: "~StringJoin~msg];];

DiffusionParameters2DPlots[Dx_,Dy_,Da_,Energy_,CellRange_] :=
    Module[ {br = (2 CellRange/Max[CellRange])~Join~{1},plotRange = Transpose[{-CellRange, CellRange}]},
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


End[]

EndPackage[]

