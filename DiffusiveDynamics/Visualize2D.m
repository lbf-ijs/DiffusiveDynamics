(* Mathematica Package *)

BeginPackage["DiffusiveDynamics`Visualize2D`",{"DiffusiveDynamics`Utils`"}]
(* Exported symbols added here with SymbolName::usage *)  

   
ClearAll[DiffusionParameters2DPlots, QucikDensityHistogram];
DiffusionParameters2DPlots::usage="DiffusionParameters2DPlots[Dx,Dy,Da,Energy,CellRange] plots the diffuzion parameter functions. Dx, Dy, Dy, Energy are functions of two coordinates";
   
QucikDensityHistogram::usage="QucikDensityHistogram[data, binspec, hspec] Draws a 2D histogram that renders quicjer than the built in.";
  
(*Gets the bin centers and widths from binspec*)
ClearAll[GetBinCenters, GetBinCentersAndWidths, GetBinsAsRectangles, DrawBinsAsRectangles,GetBinsFromBinsOrSpec];
GetBinCenters         ::usage="GetBinCenters[binSpec] \nGets bin centers from binSpec {{minX,maxY,dx},{minY,maxY,dy}}";
GetBinCentersAndWidths::usage="GetBinCentersAndWidths[binSpec] \nGets bin centers and widths from binSpec {{minX,maxY,dx},{minY,maxY,dy}}";
GetBinsAsRectangles   ::usage="GetBinsAsRectangles[bins] gets rectangles from bins {{{x,y},{dx,dy}},..}";
DrawBinsAsRectangles  ::usage="DrawBinsAsRectangles[binsOrSpec] 
draws the bins from a binSpec {{minX,maxY,dx},{minY,maxY,dy}} or from raw bins  {{{x,y},{dx,dy}},..}";
GetCellRangeFromBins::usage="GetCellRangeFromBins[bins] get the cellrange in the form {{minX,MinY},{MaxX,MaxY}}"
GetBinsFromBinsOrSpec::usage="GetBinsFromBinsOrSpec[binsOrSpec] Gets the bins {{{x,y},{dx,dy}},..} from binspec  {{minX,maxY,dx},{minY,maxY,dy}}. If a list of bins is passed, just return the bins."

ClearAll[GetDiffFrom2DTensor, Get2DTensorRepresentation, DrawDiffusionTensorRepresentations];
GetDiffFrom2DTensor::usage="GetDiffFrom2DTensor[D2tensor] returns {Dx,Dy,Dalpha} from the 2D tensor."
Get2DTensorRepresentation::usage="Draws a represnetation of the diffusion tensor from the tensor or the diffusion points Dx, Dy, Da."; 
DrawDiffusionTensorRepresentations::usage="DrawDiffusionTensorRepresentations[diffInfos, cellRange] 
takes a list of diffusion info rules and plots the tensor represntation in each bin."

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
    Module[ {br = (2 CellRange/Max[CellRange])~Join~{1}},
        {Plot3D[Dx[x, y], {x, CellRange[[1]], -CellRange[[1]]}, {y, 
        CellRange[[2]], -CellRange[[2]]}, BoxRatios -> br,
        ImageSize -> Small,  PlotLabel -> "DIFF X"],
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


GetBinCenters[binSpec_] :=
    Block[ {$VerboseIndentLevel = $VerboseIndentLevel+1},
        Module[ {},
            Puts["********GetBinCenters********"];
            
            (*calculate all the centers of the bin.
            generate a range of the bin min and max and step. 
            Get the centers using moving average and get a 2D grid of points using Tuples*)
            Tuples[{
	            MovingAverage[Range@@binSpec[[1]],2],
	            MovingAverage[Range@@binSpec[[2]],2]
            }]
           
        ]
    ];

(*Gets the bin centers and widths from binspec*)

GetBinCentersAndWidths[binSpec_] :=
    Block[ {$VerboseIndentLevel = $VerboseIndentLevel+1},
        Module[ {bincenters,binwidth},
            Puts["********GetBinCentersAndWidth********"];
            bincenters=GetBinCenters[binSpec];
            binwidth = Transpose[binSpec][[3]];
            {#,binwidth}&/@bincenters            
        ]
    ];
     
(*Turns bins in the form of { {{x,y},{xWidth,yWidth}}, ..} into rectangles*) 

GetBinsAsRectangles[bins_] :=
    Block[ {$VerboseIndentLevel = $VerboseIndentLevel+1},
        Module[ {},
            Puts["********GetBinsAsRectangles********"];
            Rectangle[#[[1]]-#[[2]]/2,#[[1]]+#[[2]]/2]&/@bins
            
        ]
    ]; 
    
GetBinsFromBinsOrSpec[binsOrBinSpec_]:=
	Module[{isBinSpec, isBinList},
	    Puts["********GetBinsFromBinsOrSpec********",LogLevel->2];
	    isBinSpec=(Dimensions@binsOrBinSpec=={2,3}); (*this is a bin spec*)
	    isBinList=(Rest@Dimensions@binsOrBinSpec=={2,2}); (*this are bins*)
	    Assert[isBinSpec || isBinList,"binsOrBinSpec is in wrong format! "<>ToString@Shallow@binsOrBinSpec];
		                
	    If[isBinSpec, (*then*)GetBinCentersAndWidths@binsOrBinSpec, (*else*)binsOrBinSpec]
	]    

Options[DrawBinsAsRectangles]={"Fill"->Opacity@0 (*The fill of the bins (FaceForm)*),
    						   "Outline"->Directive[Black,Thick] (*The outline (EdgeForm)*)}
(*Draws bins either from a binspec {{minx,maxx,xWidth},{miny,maxy,yWidth}} or from a list of bins
 { {{x,y},{xWidth,yWidth}}, ..} *)
DrawBinsAsRectangles[binsOrBinSpec_,opts:OptionsPattern[]]:=
      Block[ {$VerboseIndentLevel = $VerboseIndentLevel+1},
            Puts["********DrawBinsAsRectangles********"];
            PutsE["binsOrBinSpec: ",binsOrBinSpec, LogLevel->2];
            Puts[$VerboseIndentString,"dimensions: ",Dimensions@binsOrBinSpec];
            PutsOptions[DrawBinsAsRectangles, {opts}, LogLevel->2];
            Graphics[{FaceForm@OptionValue@"Fill",EdgeForm@OptionValue@"Outline"}~Join~
            GetBinsAsRectangles@GetBinsFromBinsOrSpec[binsOrBinSpec]]                     
    ]; 


(*takes a list of bins {{{x,y},{dx,dy}},..} and returns cellRange {{minX,minY},{MaxX,MaxY}}*)
(*Todo can be compiled if necessary*)
GetCellRangeFromBins[bins_]:= 
	Block[{minPoints, maxPoints,minX,maxX,minY,maxY},
	    minPoints = (#[[1]] - #[[2]]/2) & /@ bins;
        maxPoints = (#[[1]] + #[[2]]/2) & /@ bins;
        minX=Min[minPoints[[All,1]]];
        minY=Min[minPoints[[All,2]]];
        maxX=Max[maxPoints[[All,1]]];
        maxY=Max[maxPoints[[All,2]]];
		{{minX,minY},{maxX,maxY}}
	];

GetDiffFrom2DTensor[tensor_] :=
    Module[{evec,eval,Dx,Dy,alpha},
        Puts["********GetDiffFrom2DTensor********",LogLevel->2];
        {eval,evec} = Eigensystem[tensor];
        {Dx,Dy} = Sqrt[1/eval];
        alpha = ArcCos[evec[[1,1]]]/Degree*Sign[ArcSin[evec[[1,2]]]];
        alpha = Mod[alpha,180];
        Puts["Dx: ",Dx," Dy: ",Dy," alpha: ",alpha,LogLevel->3];
        Return[{Dx,Dy,alpha}]
    ];


Options[Get2DTensorRepresentation] = {"Center"->{0,0},"ShapeType"->"Disk","Scale"->1,"Verbose"->False};
Get2DTensorRepresentation[Dx_,Dy_,alpha_,opt:OptionsPattern[]] :=
Block[ {$VerbosePrint = OptionValue@"Verbose"},
    Module[ {e1,e2,RM,s,c},
        If[ MatchQ[Dx,Missing[___] ]|| MatchQ[Dy,Missing[___]] ||MatchQ[alpha,Missing[___]],
            Return[{}]
        ];
        PutsOptions[Get2DTensorRepresentation, {opts}, LogLevel->2];
        RM = RotationMatrix[alpha Degree];
        c = OptionValue["Center"];
        s = OptionValue["Scale"];
        e1 = RM.{Dx,0} *s;
        e2 = RM.{0,Dy} *s;
        Return[
        (*Roate the graphic*) 
        
         Switch[OptionValue["ShapeType"],
           "Arrows",
             {Arrow[{c,e1+c}],
              Arrow[{c,e2+c}],
              Arrow[{c,-e1+c}],
              Arrow[{c,-e2+c}]},
           "Circle",
             Rotate[Circle[c,{Dx,Dy}*s],alpha Degree,c],
           "Disk",
             Rotate[Disk[c,{Dx,Dy}*s],alpha Degree,c],
           _,
             Rotate[Disk[c,{Dx,Dy}*s],alpha Degree,c]
          ](*Switch*)
        ]
    ]
];

Get2DTensorRepresentation[tensor_,opt:OptionsPattern[]] :=
    Module[ {Dx,Dy,alpha},
        Block[ {$VerbosePrint = OptionValue["Verbose"]},
            {Dx,Dy,alpha} = GetDiffFrom2DTensor[tensor];
            Return[
              Get2DTensorRepresentation[Dx,Dy,alpha,opt]
            ]
        ]
    ];



(*takes a list of diffusion info rules and plots the tensor represntation in each bin*)
Options[DrawDiffusionTensorRepresentations] = {
                                    "FillStyle"->Directive@Opacity@0,
                                    "OutlineStyle"->Directive[Thick,Black],
                                    "Scale"->6,
                                    "ShapeType"->"Disk",
                                    "CellRange"->Automatic,
                                    "Verbose":>$VerbosePrint,  (*Log output*)
						            "VerboseLevel":>$VerboseLevel,
						            Axes->True,
						            Frame->True,
						            PlotRange->Automatic, 
						            AspectRatio->Automatic
						            }~Join~Options@Graphics;
DrawDiffusionTensorRepresentations[diffInfos_,opts:OptionsPattern[]] :=
Block[ {$VerbosePrint = $VerbosePrint||OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1},
    Module[{values,reps,cellRange,bins,plotRange,aspectRatio},
        Puts["********DrawDiffusionTensorRepresentations********"];
        PutsOptions[DrawDiffusionTensorRepresentations,{opts},LogLevel->2];
        values=GetValues[{"x","y","Dx","Dy","D\[Alpha]"},diffInfos];
		PutsE["DiffValues:\n",values,LogLevel->3];     
		
		reps=Get2DTensorRepresentation[#[[3]],#[[4]],#[[5]],Center->#[[1;;2]],"ShapeType"->OptionValue["ShapeType"],Scale->OptionValue["Scale"]]&/@values;
		PutsE["reps:\n",reps,LogLevel->3]; 
        
        bins=GetValues[{{"x", "y"}, {"xWidth", "yWidth"}},diffInfos];
        cellRange=If[#===Automatic, GetCellRangeFromBins@bins,#]&@OptionValue@"CellRange";
        plotRange=If[#===Automatic, Transpose@cellRange,#]&@OptionValue@"CellRange";
        aspectRatio=If[#===Automatic,((plotRange[[2,2]]-plotRange[[2,1]])/(plotRange[[1,2]]-plotRange[[1,1]])),#]&@OptionValue@"AspectRatio";
        PutsE["cellRange: ",cellRange,LogLevel->2];
        PutsE["plotRange: ",plotRange,LogLevel->2];
        PutsE["aspectRatio: ",aspectRatio,LogLevel->2];
        Puts["Passed Options:", {FilterRules[{opts}~Join~Options@DrawDiffusionTensorRepresentations,Options@Graphics]},LogLevel->5];
        
        Graphics[
        	{FaceForm@OptionValue["FillStyle"],EdgeForm@OptionValue["OutlineStyle"]}~Join~reps,
        	PlotRange->plotRange,AspectRatio->aspectRatio,FilterRules[{opts}~Join~Options@DrawDiffusionTensorRepresentations,Options@Graphics]]
    ]
]



End[] (* End Private Context *)

EndPackage[]