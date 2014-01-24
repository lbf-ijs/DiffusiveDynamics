(* Mathematica Package *)

BeginPackage["DiffusiveDynamics`Visualize2D`",{"DiffusiveDynamics`Utils`","ErrorBarPlots`"}]
(* Exported symbols added here with SymbolName::usage *)  

   
ClearAll[DiffusionParameters2DPlots, QucikDensityHistogram, Draw2DHistogram, Draw3DHistogram];
DiffusionParameters2DPlots::usage="DiffusionParameters2DPlots[Dx,Dy,Da,Energy,CellRange] plots the diffuzion parameter functions. Dx, Dy, Dy, Energy are functions of two coordinates";
   
QucikDensityHistogram::usage="QucikDensityHistogram[data, binspec, hspec] Draws a 2D histogram that renders quicjer than the built in.";
Draw2DHistogram::usage="Draw2DHistogram[bindata] Draws a discrete density 2D plot";
Draw3DHistogram::usage="Draw3DHistogram[bindata] Draws a 3D barchart kind histogram";    
DrawSmooth3DHistogram::usage="TODO";
(*Gets the bin centers and widths from binspec*)

ClearAll[GetHistogramListFrom2DPDF, GetStepsHistogramDiffusion];
GetHistogramListFrom2DPDF::usage="GetHistogramListFrom2DPDF[dist,{xmin,xmax,xn},{ymin,ymax,yn}]
Returns the data in the same format as HistogramList, when given a PDF and the ranges to plot";
GetStepsHistogramDiffusion::usage="
Get the theoretical steps histogram form the diffusion. Returns same format as HistogramList"

ClearAll[GetBinCenters, GetBinCentersAndWidths, GetBinsAsRectangles, DrawBinsAsRectangles,GetBinsFromBinsOrSpec,GetBinCentersAndWidthsFromDiffusionInfo];
GetBinCenters         ::usage="GetBinCenters[binSpec] \nGets bin centers from binSpec {{minX,maxY,dx},{minY,maxY,dy}}";
GetBinCentersAndWidths::usage="GetBinCentersAndWidths[binSpec] \nGets bin centers and widths from binSpec {{minX,maxY,dx},{minY,maxY,dy}}";
GetBinsAsRectangles   ::usage="GetBinsAsRectangles[bins] gets rectangles from bins {{{x,y},{dx,dy}},..}";
DrawBinsAsRectangles  ::usage="DrawBinsAsRectangles[binsOrSpec] 
draws the bins from a binSpec {{minX,maxY,dx},{minY,maxY,dy}} or from raw bins  {{{x,y},{dx,dy}},..}";
GetCellRangeFromBins::usage="GetCellRangeFromBins[bins] get the cellrange in the form {{minX,MinY},{MaxX,MaxY}}"
GetBinsFromBinsOrSpec::usage="GetBinsFromBinsOrSpec[binsOrSpec] Gets the bins {{{x,y},{dx,dy}},..} from binspec  {{minX,maxY,dx},{minY,maxY,dy}}. If a list of bins is passed, just return the bins."

GetBinCentersAndWidthsFromDiffusionInfo::usage="GetBinCentersAndWidthsFromDiffusionInfo[diffinfo] Gets the bins from a list of diffusion info";

ClearAll[GetDiffFrom2DTensor,Get2DTensorFromDiff, Get2DCovarianceFromDiff, Get2DTensorRepresentation, GetDiffFrom2DCovariance, DrawDiffusionTensorRepresentations];
GetDiffFrom2DTensor::usage="GetDiffFrom2DTensor[D2tensor] returns {Dx,Dy,Dalpha} from the 2D tensor."
GetDiffFrom2DCovariance::usage="GetDiffFrom2DTensor[2Dcovar] returns {Dx,Dy,Dalpha} from the 2D covariance matrix."
Get2DTensorFromDiff::uasge="Get2DTensorFromDiff[Dx, Dy, Da] returns a 2D tensor that represents the sigma matrix of the bivariate normal Gaussian distribution."
Get2DCovarianceFromDiff::usage="Get2DCovarianceFromDiff[Dx, Dy, Da] returns a 2D covariance matrix representing the diffusion. Can be plugged in directly into the bivariate normal Gaussian distribution"
Get2DTensorRepresentation::usage="Get2DTensorRepresentation[Dx,Dy,alpha,opts] \nDraws a represnetation of the diffusion tensor from the tensor or the diffusion points Dx, Dy, Da."; 

DrawDiffusionTensorRepresentations::usage="DrawDiffusionTensorRepresentations[diffInfo,binIndex_,opts] 
takes a list of diffusion info rules and plots the tensor represntation in each bin."

ClearAll[GetFreeEnergy];
GetFreeEnergy::usage="Get a list of free energy points";

(*For patteren matching lists of diffusion info*)
listOfRules = {{__Rule}..}; 
listOfListOfRules = {{{__Rule} ..} ..};
listOfListOfListOfRules ={{{{__Rule} ..} ..}..};
diffInfoOneBin = {__Rule};

(*Definitions of list depth: {bins,  diffs}*)
diffInfos = {diffInfoOneBin..};
(*Definitions of list depth: {bins, stride, diffs}*)
diffInfosWithStride = listOfListOfRules;
(*Definitions of list depth: {set, bins, stride, diffs}*)
listOfdiffInfosWithStride = {diffInfosWithStride..}; (*Spelling mistake fix*)
listOfDiffInfosWithStride = {diffInfosWithStride..};

NumericOrSymbolQ=(NumericQ[#] ||Head[#]===Symbol)&;

RowLegend::usage=="Gives a row line legend";
ClearAll[DiffBinRectangle];
DiffBinRectangle[{center_,w_}] := Rectangle[center-w/2.,center+w/2.];

  
ClearAll[GetBinIndexFromPoint];
GetBinIndexFromPoint::usage="TODO";
GetBinIndexFromPoint[pt_,bins_] :=
    Block[ {i},
       (*Puts[StringForm["GetBinIndexFromPoint: pt `1` bins `2`",pt,bins]];*)
        For[i = 1,i<=Length[bins],i++,
        If[ PointInBinQ[pt,bins[[i]]],
            Return[i]
        ]];
        (*If we get here return null*)                                        
        Return[-1];
    ];  
   

DrawErrDiagram::usage="Draws a stride plot with errors. Data must be in the format {{{x,y},ErrorBar},...}";
GetMeanAndErrFromBins::usage="Gives the average of a certain quantaty from diff over all the bins ";

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
Options[Draw2DHistogram]= {InterpolationOrder->0,
                           ColorFunction->(If[ #<.1,
                                  White,
                                  ColorData[{"SolarColors","Reverse"}][#]
                              ]&)
                              
                            }~Join~ 
   Options@ListContourPlot;
Draw2DHistogram[bindata_, opts:OptionsPattern[]] :=
(*TODO: Danger if a function is passed insted of bindata, it gets evaluated twice, every time bindata is called*)
    Block[ {bins,counts,datarange,wh, iopts},
        If[ Length[bindata]===0,
            Return[Missing[]]
        ]; 
        {bins,counts} = bindata;
        datarange = bins[[All,{1,-1}]];
        wh = Flatten[Differences[#]&/@datarange];
        iopts=FilterRules[{opts}~Join~Options@Draw2DHistogram,Options@ListContourPlot];
        ListContourPlot[Transpose@counts,DataRange->datarange,AspectRatio->wh[[2]]/wh[[1]], iopts
              ]
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


SetAttributes[DrawSmooth3DHistogram,HoldFirst];
Options[DrawSmooth3DHistogram]={"PlotColor" -> Directive[Blue],(*The color used for the histogram*)
    					  "FillStyle" -> Automatic,  
    					  "OutlineStyle" -> Automatic,
    					  Axes -> True,
    					  BoxRatios -> Automatic, (*Wil make the x and y units 1 to 1*)
						  "Verbose":>$VerbosePrint,  (*Log output*)
 					      "VerboseLevel":>$VerboseLevel,
 					      PlotRange-> All
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
        injectedOptions=FilterRules[{opts}~Join~Options@DrawSmooth3DHistogram,Options@ListPlot3D];
        
        ListPlot3D[points, PlotStyle->fillStyle,MeshStyle->outlineStyle,BoxRatios->boxRatios,injectedOptions]
        	
    ];   

ClearAll[ShowStepsDensityHistogram];
ShowStepsDensityHistogram[steps_] :=
    Module[ {g,plr},
        g = DensityHistogram[steps,{"Raw",25},PerformanceGoal->"Speed"];
        plr = {Min@#1,Max@#2}&@@Transpose[PlotRange/.Options[g]];
        Return[Rasterize[Show[g,PlotRange->{plr,plr}],ImageSize->Medium]];
    ];


(*
returns a list of {x, y, FreeEnergy}

rw -- data in the form {step, x,y}*)

Options@GetFreeEnergy= {"binSpec" -> "Sturges", (*bin specification*)
                        "hSpec"   -> "PDF",
                        "SetMinToZero" -> True,
                        "ReturnHistogramList" -> True
                       };
                       
GetFreeEnergy[rw_,opts:OptionsPattern[]]:=
Block[{$VerboseIndentLevel = $VerboseIndentLevel+1, 
       bins, counts, points, i,j},
    (*Flatten the list, if multiple trajs given, take the {x, y} and create histogram list*)
    
        
    {bins,counts}=N@HistogramList[Flatten[rw, 1][[All, {2, 3}]], OptionValue@"binSpec", OptionValue@"hSpec"];
    counts=-Log@counts;
    If[OptionValue@"SetMinToZero", counts = counts - Min@withoutIndeterminate[counts] ];
    
    If[OptionValue@"ReturnHistogramList",
        Return[{bins,counts}]
    ,(*else*)
	    points=Flatten[ 
	         Table[{(bins[[1, i]] + bins[[1, i + 1]])/2, (bins[[2, j]] + bins[[2, j + 1]])/2, counts[[i, j]]}, 
	           {i, Length@bins[[1]]-1}, {j, Length@bins[[2]]-1}],
	         1];
	    points
    ]
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
        ImageSize -> Small, PlotLabel -> "ENERGY",PlotRange->All]}
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
(*One time got negative width/heights in some data, this is the reason for the Abs*)
GetCellRangeFromBins[bins_]:= 
	Module[{minPoints, maxPoints,minX,maxX,minY,maxY},
	    minPoints = (#[[1]] - Abs[#[[2]]/2]) & /@ bins;
        maxPoints = (#[[1]] + Abs[#[[2]]/2]) & /@ bins;
        minX=Min[minPoints[[All,1]]];
        minY=Min[minPoints[[All,2]]];
        maxX=Max[maxPoints[[All,1]]];
        maxY=Max[maxPoints[[All,2]]];
		{{minX,minY},{maxX,maxY}}
	];

GetDiffFrom2DTensor[tensor_] :=
    Module[{evec,eval,Dx,Dy,alpha},
        Puts["********GetDiffFrom2DTensor********",LogLevel->5];
        {eval,evec} = Eigensystem[tensor];
        {Dx,Dy} = Sqrt[1/eval];
        alpha = ArcCos[evec[[1,1]]]/Degree*Sign[ArcSin[evec[[1,2]]]];
        alpha = Mod[alpha,180];
        Puts["Dx: ",Dx," Dy: ",Dy," alpha: ",alpha,LogLevel->5];
        Return[N@{Dx,Dy,alpha}]
    ];

GetDiffFrom2DCovariance[covar_] :=
    Module[{evec,eval,Dx,Dy,alpha},
        Puts["********GetDiffFrom2DTensor********",LogLevel->5];
        {eval,evec} = Eigensystem[covar];
        {Dx,Dy} = eval/2;
        alpha = ArcCos[evec[[1,1]]]/Degree*Sign[ArcSin[evec[[1,2]]]];
        alpha = Mod[alpha,180];
        Puts["Dx: ",Dx," Dy: ",Dy," alpha: ",alpha,LogLevel->5];
        Return[N@{Dx,Dy,alpha}]
    ];

Get2DTensorFromDiff[Dx_,Dy_,Da_]:=
	N@RotationMatrix[Da*Degree].{{1/Dx^2, 0}, {0, 1/Dy^2}}.RotationMatrix[-Da*Degree];
Get2DTensorFromDiff[{Dx_,Dy_,Da_}]:=Get2DTensorFromDiff[Dx,Dy,Da];
Get2DTensorFromDiff[all___]:=Throw["Wrong arguments Get2DTensorFromDiff: ",all];

Get2DCovarianceFromDiff[Dx_,Dy_,Da_]:=
    Chop[N@RotationMatrix[Da*Degree].{{2*Dx, 0}, {0, 2*Dy}}.RotationMatrix[-Da*Degree],10^-8];	
Get2DCovarianceFromDiff[{Dx_,Dy_,Da_}]:=Get2DCovarianceFromDiff[Dx,Dy,Da];
Get2DCovarianceFromDiff[all___]:=Throw["Wrong arguments for Get2DCovarianceFromDiff: ",all];
    

Options[Get2DTensorRepresentation] = {"Center"->{0,0},"ShapeType"->"Circle","Scale"->1,"Verbose":>$VerbosePrint};
Get2DTensorRepresentation[Dx_,Dy_,alpha_,opts:OptionsPattern[]] :=
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


(*Wrap plots in an event handler, so that they respond to clicks*)
ClearAll[WrapBinClickEventHandler];
(*Must be HoldFirst, so binIndex emulates. HoldAll does not work becuase binsList does not get expanded!*)
Attributes@WrapBinClickEventHandler=HoldFirst; 
WrapBinClickEventHandler[binIndex_,binsList_,plot_]:=
    MouseAppearance[EventHandler[plot,
     {{"MouseClicked", 1}:>Block[{pt},
         pt = MousePosition["Graphics"];
         binIndex = GetBinIndexFromPoint[pt,binsList];
         (*Print["pt: ",pt, " binIndex: ",binIndex];*)
         If[ binIndex==-1,binIndex = 1];
     ]},PassEventsDown->False],"LinkHand"];                   


(*takes a list of diffusion info rules and plots the tensor represntation in each bin*)
Options[DrawDiffusionTensorRepresentations] := {
                                    "FillStyle"->Automatic,
                                    PlotStyle->Automatic,
                                    "Scale"->1,
                                    "ShapeType"->"Disk",
                                    "CellRange"->Automatic,
                                    "MarkNonNormal" -> False, (*Should non normal distributions be marked?*)
                                    "NonNormalFillStyle"->Directive[Red,Opacity@.3],
                                    "NonNormalOutlineStyle"->None,
                                    "Verbose":>$VerbosePrint,  (*Log output*)
						            "VerboseLevel":>$VerboseLevel,
						            "Clickable" -> False,
						            "MarkSelectedBin" -> False,
						            "MarkedBinDynamic" -> Automatic,
                                    "TransposeDiffusions"->False,
						            "MarkedBinFillStyle" -> None,
						            "MarkedBinOutlineStyle" -> Directive[Thick, Black, Opacity@.5],
						            "ShowMinBins" -> False,
						            "MinBinsOutlineStyle" -> Automatic,
						            "MinBinsFillStyle" -> Automatic,
						            ImageSize->Medium,
						            Axes->True,
						            Frame->True,
						            PlotRange->Automatic, 
						            AspectRatio->Automatic
						            }~Join~Options@Graphics;
Attributes@DrawDiffusionTensorRepresentations={HoldRest};
						            
DrawDiffusionTensorRepresentations[diffInfos:listOfRules,binIndex_,opts:OptionsPattern[]] :=
Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1,br},
    Module[{values,reps,nonNormalReps,nonNormals,cellRange,bins,plotRange,aspectRatio,plotStyle, fillStyle, markedBinDynamic,
        minbins,minbinsOutlineStyle,minbinsFillStyle},
        Puts["********DrawDiffusionTensorRepresentations********"];
        PutsOptions[DrawDiffusionTensorRepresentations,{opts},LogLevel->2];
        
        plotStyle=If[#===Automatic,Directive[ColorData[1][1]],#]&@OptionValue[PlotStyle];
        fillStyle=If[#===Automatic,None,#]&@OptionValue["FillStyle"];
        Puts["plotStyle: ",plotStyle];
        Puts["fillStyle: ",fillStyle];
		values=GetValues[{"x","y","Dx","Dy","Da"},diffInfos];
		If[OptionValue@"TransposeDiffusions",
            values=values/.{x_,y_,Dx_,Dy_,Da_}:>{y,x,Dx,Dy, Mod[90-Da,180]};  
		];

		Puts["Length of values: "Length@values,LogLevel->3];
		PutsE["DiffValues:\n",values,LogLevel->5];
		
		     
		nonNormals=GetValues["IsNormal",diffInfos];
		PutsE["nonNormals:\n",nonNormals,LogLevel->3];
		reps=Get2DTensorRepresentation[#[[3]],#[[4]],#[[5]],Center->#[[1;;2]],"ShapeType"->OptionValue["ShapeType"],Scale->OptionValue["Scale"]]&/@values;
		
		PutsE["reps:\n",reps,LogLevel->3]; 
        (*TODO:_Tale pick je problematièen*)
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
        
        reps={FaceForm@fillStyle,EdgeForm@plotStyle}~Join~reps~Join~
            {FaceForm@OptionValue["NonNormalFillStyle"],EdgeForm@OptionValue["NonNormalOutlineStyle"]}~Join~nonNormalReps;

        If[OptionValue@"MarkSelectedBin", (*then*)
          (*Wrap bin marker in Dynamic if the graph is clickable if "MarkedBinDynamic" True *)
          markedBinDynamic=If[ # === Automatic, OptionValue@"Clickable", #]&@OptionValue@"MarkedBinDynamic";
          
          (*There was some problem with dynamic if bins were not injected*)
          With[{ibins=bins},
             br=If[markedBinDynamic,Dynamic@DiffBinRectangle[ ibins[[binIndex]] ], DiffBinRectangle[ ibins[[binIndex]] ]];
          ];
          reps=reps~Join~{EdgeForm@OptionValue@"MarkedBinOutlineStyle",FaceForm@OptionValue@"MarkedBinFillStyle", br};  
        ];    
        
        If[OptionValue@"ShowMinBins" && MemberQ[diffInfos,"xMinWidth",Infinity], (*then*)
            
            minbins=GetValue[{{"x", "y"}, {"xMinWidth", "yMinWidth"}},diffInfos[[binIndex]]];
            minbinsOutlineStyle=If[#===Automatic,plotStyle,#]&@OptionValue["MinBinsOutlineStyle"];
            minbinsFillStyle=If[#===Automatic,fillStyle,#]&@OptionValue["MinBinsFillStyle"]; 
            reps=reps~Join~{EdgeForm@minbinsOutlineStyle,FaceForm[minbinsFillStyle], GetBinsAsRectangles@GetBinsFromBinsOrSpec@{minbins}};  
        ];
        
        reps=Graphics[reps
        	,PlotRange->plotRange,AspectRatio->aspectRatio,FilterRules[{opts}~Join~Options@DrawDiffusionTensorRepresentations,Options@Graphics]];
        
        If[ OptionValue@"Clickable",
            reps = WrapBinClickEventHandler[binIndex,bins,reps];
        ];          	
        reps	
    ]
]
  
DrawDiffusionTensorRepresentations[diffInfos:listOfListOfRules,binIndex_,opts:OptionsPattern[]] :=
Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1, i},
    Module[{plotStyles,fillStyles,plots,bins},
        Puts["********DrawDiffusionTensorRepresentations (multipe lists)********"];
        PutsOptions[DrawDiffusionTensorRepresentations,{opts},LogLevel->2];
        
        plotStyles=If[#===Automatic,
            Table[Directive[Opacity@1,ColorData[1][i]],{i,Length@diffInfos}]
            ,#]&@OptionValue[PlotStyle];
        fillStyles=If[#===Automatic,
            Table[None,{i,Length@diffInfos}]
            ,#]&@OptionValue["FillStyle"];            
        Puts["plotStyles: ",plotStyles];
        Puts["fillStyles: ",fillStyles];
        
        plots=Show@@Table[DrawDiffusionTensorRepresentations[diffInfos[[i]], binIndex,
                PlotStyle->plotStyles[[i]],"FillStyle"->fillStyles[[i]],"Clickable"->False,
                (*Mark bin of only first graph if bins should be marked. Also make it dynamic if it is clickable*)
                "MarkSelectedBin"-> (OptionValue@"MarkSelectedBin" && i==1 ),
                "MarkedBinDynamic"->(OptionValue@"MarkSelectedBin" && i==1 && OptionValue@"Clickable"), opts]
            ,{i,Length@diffInfos}];
        
        
        If[ OptionValue@"Clickable",
            bins=GetValues[{{"x", "y"}, {"xWidth", "yWidth"}},First@diffInfos];
            plots = WrapBinClickEventHandler[binIndex,bins,plots];
        ];              
        plots    
             
    ]
]


ClearAll@RowLegend;
Options@RowLegend = {PlotStyle -> Automatic,
                     TextStyle -> Bold,
                     Tooltip->None, (*Array of tooltips to be displayed*)
                     PopupWindow->None,(*Array of data to be displayed in popupwindows*)
                     "VisibilityList"->None(*If given it must be a held list with 1,1,1.. Will change to refelct visibility of each diff*) };

RowLegend[legends_, opts : OptionsPattern[]] :=
Block[ {plotStyles,i,l,tooltips, metadata},
        plotStyles = 
         If[ # === Automatic,
             Table[Directive[Thick, Opacity@1, ColorData[1][i]], {i, Length@legends}],#
           ] &@OptionValue[PlotStyle];
        Assert[Length@legends === Length@plotStyles, "PlotStyles and legends must be of the same length"];

        l=Table[
            Column[{
	            Style[ legends[[i]], OptionValue[TextStyle],plotStyles[[i]]],
	            Graphics[{plotStyles[[i]],Line[{{0,0},{1,0}}]},AspectRatio->1/10]
            }, Center, Spacings->0],
        {i,Length@legends}];
        
        If[OptionValue@Tooltip =!= None, 
            tooltips=OptionValue@Tooltip;
            Assert[Length[legends] === Length[tooltips], "legends and tooltips must be of the same length"];
            l=Table[ Tooltip[l[[i]],tooltips[[i]] ] ,{i,Length@legends}];
        ];
        If[OptionValue@PopupWindow =!= None, 
            (*TODO Make popupwindow na dvojni click in resizable*)
            metadata=OptionValue@PopupWindow;
            Assert[Length[legends] === Length[metadata], "legends and metadata must be of the same length"];
            l=Table[ PopupWindow[l[[i]], metadata[[i]],WindowTitle->legends[[i]] ] ,{i,Length@legends}];
        ];
        l    
]

GetHistogramListFrom2DPDF[dist_,{xmin_,xmax_,xn_},{ymin_,ymax_,yn_}]:=
Block[{xbins,ybins,data,x,y},
(*    xbins=FindDivisions[{xmin,xmax},xn];
    ybins=FindDivisions[{ymin,ymax},yn];*)
    xbins=Range[xmin,xmax,(xmax-xmin)/xn];
    ybins=Range[ymin,ymax,(ymax-ymin)/yn];
    data=Table[PDF[dist,{x,y}],{x,xbins},{y,ybins}];
    {{xbins,ybins},data}	
];  
Options@GetStepsHistogramDiffusion={"binNum"->{30,30}}

GetStepsHistogramDiffusion[{Dx_,Dy_,Da_},stride_:1,opts:OptionsPattern[]]:=
Block[{D, rD, dist, maxX,maxY, binN, $VerboseIndentLevel=$VerboseIndentLevel+1},
    Puts["***GetStepsHistogramDiffusion***"];
    PutsF["Dx: ``; Dy: ``; Da: ``; stride: ``",Dx,Dy,Da,stride,LogLevel->2];
    PutsOptions[GetStepsHistogramDiffusion,{opts}];
    
    D=Get2DCovarianceFromDiff[Dx*stride,Dy*stride,Da];
    binN=OptionValue@"binNum";
    
	    (*Get the plotting range*)
	    rD=RotationMatrix[Da*Degree].{{3*Sqrt[stride*Dx], 0}, {0,3*Sqrt[stride*Dy]}}.RotationMatrix[-Da*Degree];
	    (*Get the max X and maxY from the rotated diff vectors *)
	    {maxX,maxY}=Max/@(Transpose@rD);
     
    Puts["D:\n",D];
    dist=MultinormalDistribution[{0,0},D];
    Puts["D:\n",dist];
    GetHistogramListFrom2DPDF[dist,{maxX,-maxX,First@binN},{maxY,-maxY,Last@binN}]
]



ClearAll@GetMeanAndErrFromBins;
GetMeanAndErrFromBins[name_, diffs_] :=
    Module[ {strides, vals, means, errs, data, stridesNum},
        Puts["***GetMeanAndErrFromBins***"];
        strides = GetValue["Stride", diffs[[1]]];
        stridesNum = Length@strides;
        vals = GetValue[name, diffs[[All, #]]] & /@ Range@stridesNum;
        vals = DeleteCases[vals, Missing[___], 2];
        means = Mean /@ vals;
        errs = StandardDeviation /@ vals;
        data = 
        Table[{{strides[[i]], means[[i]]}, ErrorBar[errs[[i]] ]}, 
            {i, stridesNum}]
    ];

ClearAll@DrawErrDiagram;
Options[DrawErrDiagram] :=
    {"LastIsJoined" -> True,     
    PlotLegends -> Automatic, Frame -> True, Axes -> False, 
    LabelStyle -> Medium, ImageSize -> Medium}~Join~Options[ErrorListPlot];
    
DrawErrDiagram[data_, type_, xplotRange_, opts : OptionsPattern[]] :=
    Block[ {tdata = data, joined, injectedOptions, strides},
        Puts["***DrawErrDiagram***"];
        PutsF["Getting type ``", type];
        strides = tdata[[1, All, 1, 1]];
        PutsE[strides];
        tdata[[All, All, 1, 1]] = Log2@tdata[[All, All, 1, 1]];
        (*Do a manual increment*)
        Do[tdata[[i, All, 1, 1]] = 
        tdata[[i, All, 1, 
           1]] + (i - 1)*(Log2@Last@xplotRange - Log2@First@xplotRange)/
           40, {i, Length@tdata - 1}];
        injectedOptions = 
        FilterRules[{opts}~Join~Options@DrawErrDiagram, 
        Options@ErrorListPlot];
        joined = ConstantArray[False, Length@data - 1];
        AppendTo[joined, OptionValue@"LastIsJoined"];
        PutsE[joined];
        ErrorListPlot[tdata,
            PlotRange -> {xplotRange, All},
            FrameLabel -> {"Log2[Stride]", type},
            Joined -> joined, 
        FrameTicks -> {Table[{Log2@s, s}, {s, strides}], Automatic},
            PlotLegends -> Placed[LineLegend@OptionValue@PlotLegends, Above],
        injectedOptions]
    ]

End[] (* End Private Context *)

EndPackage[]