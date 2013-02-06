(* Mathematica Package *)

BeginPackage["DiffusiveDynamics`Visualize2D`",{"DiffusiveDynamics`Utils`"}]
(* Exported symbols added here with SymbolName::usage *)  

   
ClearAll[DiffusionParameters2DPlots, QucikDensityHistogram, Draw2DHistogram, Draw3DHistogram];
DiffusionParameters2DPlots::usage="DiffusionParameters2DPlots[Dx,Dy,Da,Energy,CellRange] plots the diffuzion parameter functions. Dx, Dy, Dy, Energy are functions of two coordinates";
   
QucikDensityHistogram::usage="QucikDensityHistogram[data, binspec, hspec] Draws a 2D histogram that renders quicjer than the built in.";
Draw2DHistogram::usage="Draw2DHistogram[bindata] Draws a discrete density 2D plot";
Draw3DHistogram::usage="Draw3DHistogram[bindata] Draws a 3D barchart kind histogram";    
DrawSmooth3DHistogram::usage="TODO";
(*Gets the bin centers and widths from binspec*)
ClearAll[GetBinCenters, GetBinCentersAndWidths, GetBinsAsRectangles, DrawBinsAsRectangles,GetBinsFromBinsOrSpec,GetBinCentersAndWidthsFromDiffusionInfo];
GetBinCenters         ::usage="GetBinCenters[binSpec] \nGets bin centers from binSpec {{minX,maxY,dx},{minY,maxY,dy}}";
GetBinCentersAndWidths::usage="GetBinCentersAndWidths[binSpec] \nGets bin centers and widths from binSpec {{minX,maxY,dx},{minY,maxY,dy}}";
GetBinsAsRectangles   ::usage="GetBinsAsRectangles[bins] gets rectangles from bins {{{x,y},{dx,dy}},..}";
DrawBinsAsRectangles  ::usage="DrawBinsAsRectangles[binsOrSpec] 
draws the bins from a binSpec {{minX,maxY,dx},{minY,maxY,dy}} or from raw bins  {{{x,y},{dx,dy}},..}";
GetCellRangeFromBins::usage="GetCellRangeFromBins[bins] get the cellrange in the form {{minX,MinY},{MaxX,MaxY}}"
GetBinsFromBinsOrSpec::usage="GetBinsFromBinsOrSpec[binsOrSpec] Gets the bins {{{x,y},{dx,dy}},..} from binspec  {{minX,maxY,dx},{minY,maxY,dy}}. If a list of bins is passed, just return the bins."

GetBinCentersAndWidthsFromDiffusionInfo::usage="GetBinCentersAndWidthsFromDiffusionInfo[diffinfo] Gets the bins from a list of diffusion info";

ClearAll[GetDiffFrom2DTensor,Get2DTensorFromDiff, Get2DTensorRepresentation, DrawDiffusionTensorRepresentations];
GetDiffFrom2DTensor::usage="GetDiffFrom2DTensor[D2tensor] returns {Dx,Dy,Dalpha} from the 2D tensor."
Get2DTensorFromDiff::uasge="Get2DTensorFromDiff[Dx, Dy, Da] returns a 2D tensor that represents the covariance matrix of the bivariate normal Gaussian distribution."
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
    
    
    

SetAttributes[Draw2DHistogram,HoldFirst];
Draw2DHistogram[bindata_] :=
    Block[ {bins,counts,datarange,wh},
        If[ Length[bindata]===0,
            Return[Missing[]]
        ];
        {bins,counts} = bindata;
        datarange = bins[[All,{1,-1}]];
        wh = Flatten[Differences[#]&/@datarange];
        ListContourPlot[Transpose@counts,InterpolationOrder->0,DataRange->datarange,AspectRatio->wh[[2]]/wh[[1]],
              ColorFunction->(If[ #<.1,
                                  White,
                                  ColorData[{"SolarColors","Reverse"}][#]
                              ]&),
              PlotRange->Full, ImageSize->Medium]
    ];
        

SetAttributes[Draw3DHistogram,HoldFirst];
Options[Draw3DHistogram]={"PlotColor" -> Directive[Blue],(*The color used for the histogram*)
    					  "FillStyle" -> Automatic,  
    					  "OutlineStyle" -> Automatic,
    					  Axes -> True,
    					  BoxRatios -> Automatic, (*Wil make the x and y units 1 to 1*)
						  "Verbose":>$VerbosePrint,  (*Log output*)
 					      "VerboseLevel":>$VerboseLevel
            }~Join~Options@Graphics3D;
Draw3DHistogram[bindata_, opts:OptionsPattern[]] :=
    Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1,
            bins,counts,datarange,wh, maxC, cuboids,i,j,color, fillStyle, outlineStyle, boxRatios, injectedOptions},
        If[ Length[bindata]===0,
            Return[Missing[]]
        ];
        Puts["***Draw3DHistogram***"];
        PutsOptions[Draw3DHistogram,{opts},LogLevel->2];
        {bins,counts} = bindata;
        PutsE[bins,LogLevel->3];
		PutsE[counts,LogLevel->3];
        datarange = bins[[All,{1,-1}]];
        wh = Flatten[Differences[#] & /@ datarange];
        maxC = Max@counts;
        (*get the cuboids. Iterate over the bins and take the appropriate counts*)

        cuboids = N@Flatten[
        	Table[Cuboid[{bins[[1, i]], bins[[2, j]], 0}, {bins[[1, i + 1]], bins[[2, j + 1]], counts[[i, j]]}],
        	  {i, Length@bins[[1]]-1}, {j, Length@bins[[2]]-1}]
        	, 1];
        (*take only non-flat cuboids (more than 1% of highest cuboid)*)
		cuboids = Select[cuboids, #[[2, 3]]/maxC > .01 &];	
        color = OptionValue@"PlotColor";
        boxRatios = If[#===Automatic, {wh[[1]]/wh[[2]], 1, 1}, #]&@OptionValue@BoxRatios;
        fillStyle = If[#===Automatic, Directive[color,Opacity@.5], #]&@OptionValue@"FillStyle";
        outlineStyle= If[#===Automatic, Directive[color,Opacity@.8], #]&@OptionValue@"OutlineStyle";
        injectedOptions=FilterRules[{opts}~Join~Options@Draw3DHistogram,Options@Graphics3D];
        
        Graphics3D[{EdgeForm@outlineStyle, FaceForm@fillStyle}~Join~cuboids, BoxRatios -> boxRatios, injectedOptions]	
    ];    

ClearAll[DrawSmooth3DHistogram];
SetAttributes[DrawSmooth3DHistogram,HoldFirst];
Options[DrawSmooth3DHistogram]={"PlotColor" -> Directive[Blue],(*The color used for the histogram*)
    					  "FillStyle" -> Automatic,  
    					  "OutlineStyle" -> Automatic,
    					  Axes -> True,
    					  BoxRatios -> Automatic, (*Wil make the x and y units 1 to 1*)
						  "Verbose":>$VerbosePrint,  (*Log output*)
 					      "VerboseLevel":>$VerboseLevel
            }~Join~Options@ListPlot3D;
DrawSmooth3DHistogram[bindata_, opts:OptionsPattern[]] :=
    Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1,
            bins,counts,datarange,wh, maxC, points,i,j,color, fillStyle, outlineStyle, boxRatios, injectedOptions},
        If[ Length[bindata]===0,
            Return[Missing[]]
        ];
        Puts["***Draw3DDrawSmooth3DHistogram***"];
        PutsOptions[DrawSmooth3DHistogram,{opts},LogLevel->2];
        {bins,counts} = bindata;
        PutsE[bins,LogLevel->3];
		PutsE[counts,LogLevel->3];
        datarange = bins[[All,{1,-1}]];
        wh = Flatten[Differences[#] & /@ datarange];
       (*get the bin centers and counts*)
		points=Flatten[
		     Table[{(bins[[1, i]] + bins[[1, i + 1]])/2, (bins[[2, j]] + bins[[2, j + 1]])/2, counts[[i, j]]}, 
		       {i, Length@bins[[1]]-1}, {j, Length@bins[[2]]-1}],
		     1];
        color = OptionValue@"PlotColor";
        boxRatios = If[#===Automatic, {wh[[1]]/wh[[2]], 1, 1}, #]&@OptionValue@BoxRatios;
        fillStyle = If[#===Automatic, Directive[color,Opacity@.5], #]&@OptionValue@"FillStyle";
        outlineStyle= If[#===Automatic, Directive[color,Opacity@.8], #]&@OptionValue@"OutlineStyle";
        injectedOptions=FilterRules[{opts}~Join~Options@Draw3DHistogram,Options@ListPlot3D];
        
        ListPlot3D[points, PlotStyle->fillStyle,MeshStyle->outlineStyle,BoxRatios->boxRatios,injectedOptions]
        	
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


SetAttributes[GetBinCentersAndWidthsFromDiffusionInfo,HoldFirst];
GetBinCentersAndWidthsFromDiffusionInfo[diffInfos_]:=
    GetValues[{{"x", "y"}, {"xWidth", "yWidth"}}, diffInfos[[All, 1]]]
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

Get2DTensorFromDiff[Dx_,Dy_,Da_]:=
	RotationMatrix[Da*Degree].{{1/Dx^2, 0}, {0, 1/Dy^2}}.RotationMatrix[-Da*Degree];

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
                                    "MarkNonNormal" -> False, (*Should non normal distributions be marked?*)
                                    "NonNormalFillStyle"->Directive[Red,Opacity@.5],
                                    "NonNormalOutlineStyle"->None,
                                    "Verbose":>$VerbosePrint,  (*Log output*)
						            "VerboseLevel":>$VerboseLevel,
						            Axes->True,
						            Frame->True,
						            PlotRange->Automatic, 
						            AspectRatio->Automatic
						            }~Join~Options@Graphics;
DrawDiffusionTensorRepresentations[diffInfos_,opts:OptionsPattern[]] :=
Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1},
    Module[{values,reps,nonNormalReps,nonNormals,cellRange,bins,plotRange,aspectRatio},
        Puts["********DrawDiffusionTensorRepresentations********"];
        PutsOptions[DrawDiffusionTensorRepresentations,{opts},LogLevel->2];
        
        values=GetValues[{"x","y","Dx","Dy","Da"},diffInfos];
		PutsE["DiffValues:\n",values,LogLevel->3];
		     
		nonNormals=GetValues["IsNormal",diffInfos];
		PutsE["nonNormals:\n",nonNormals,LogLevel->3];
		reps=Get2DTensorRepresentation[#[[3]],#[[4]],#[[5]],Center->#[[1;;2]],"ShapeType"->OptionValue["ShapeType"],Scale->OptionValue["Scale"]]&/@values;
		PutsE["reps:\n",reps,LogLevel->3]; 
        
        nonNormalReps=If[OptionValue@"MarkNonNormal"
            ,(*then*)
            Get2DTensorRepresentation[#[[3]],#[[4]],#[[5]],Center->#[[1;;2]],"ShapeType"->OptionValue["ShapeType"],Scale->OptionValue["Scale"]]&/@Pick[values,nonNormals,False]
            ,(*else return empty list*){}];
            (*Todo: should perhaps delete the normal reps?*)
        bins=GetValues[{{"x", "y"}, {"xWidth", "yWidth"}},diffInfos];
        cellRange=If[#===Automatic, GetCellRangeFromBins@bins,#]&@OptionValue@"CellRange";
        plotRange=If[#===Automatic, Transpose@cellRange,#]&@OptionValue@PlotRange;
        aspectRatio=If[#===Automatic,((plotRange[[2,2]]-plotRange[[2,1]])/(plotRange[[1,2]]-plotRange[[1,1]])),#]&@OptionValue@"AspectRatio";
        PutsE["cellRange: ",cellRange,LogLevel->2];
        PutsE["plotRange: ",plotRange,LogLevel->2];
        PutsE["aspectRatio: ",aspectRatio,LogLevel->2];
        Puts["Passed Options:", {FilterRules[{opts}~Join~Options@DrawDiffusionTensorRepresentations,Options@Graphics]},LogLevel->5];
        
        Graphics[
        	{FaceForm@OptionValue["FillStyle"],EdgeForm@OptionValue["OutlineStyle"]}~Join~reps~Join~
        	{FaceForm@OptionValue["NonNormalFillStyle"],EdgeForm@OptionValue["NonNormalOutlineStyle"]}~Join~nonNormalReps,
        	PlotRange->plotRange,AspectRatio->aspectRatio,FilterRules[{opts}~Join~Options@DrawDiffusionTensorRepresentations,Options@Graphics]]
    ]
]



End[] (* End Private Context *)

EndPackage[]