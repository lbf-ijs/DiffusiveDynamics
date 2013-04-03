(* Mathematica Package *)

BeginPackage["DiffusiveDynamics`Manipulate2D`",{"DiffusiveDynamics`Utils`","DiffusiveDynamics`Visualize2D`","Developer`"}]
(* Exported symbols added here with SymbolName::usage *)  

ClearAll[DrawStridePlotsFromBinInfo, ViewDiffsWithStridePlots];
DrawStridePlotsFromBinInfo::usage="TODO"

ViewDiffsWithStridePlots::usage="TODO";

DrawDiffsWithStridePlots::usage="TODO";

Begin["`Private`"] (* Begin Private Context *) 

ClearAll[PointInBinQ];  
PointInBinQ[pt_,{center_,w_}] :=
    (With[ {px=pt[[1]],py=pt[[2]],x = center[[1]],y = center[[2]],xw = w[[1]]/2.,yw = w[[2]]/2.},
         (x-xw<px<=x+xw)&&(y-yw<py<=y+yw)
     ]);

(*PointInBinQ[{-545.8579881656805`,418.19526627218914`},#,w]&/@bins*)


    
    
ClearAll[GetIndexFromList];
(*find the index of the closest element of sorted list to val*)
Attributes[GetIndexFromList]=HoldAll;
GetIndexFromList[val_,list_] :=
    Block[ {},
        (*This could be imporved, but for now I calculate the absolute distance to all baundries and find the minimum*)
        (*Returns the position in list in which the min value appears*)
        First@Ordering[Abs[val - list], 1]
    ];
        
    
(*Block[{pt, bins},
GetBinIndexFromPointC=
    Compile[{{pt,_Real,1},{bins,_Real,2}},
        Block[ {i},
           (*Puts[StringForm["GetBinIndexFromPoint: pt `1` bins `2`",pt,bins]];*)
            For[i = 1,i<=Length[bins],i++,
            If[ PointInBinQC[pt,bins[[i]]],
                Return[i]
            ]];
            (*If we get here return null*)                                        
            Return[-1];
        ]    
    
   ,CompilationOptions->{"ExpressionOptimization"->True,"InlineCompiledFunctions"->True,"InlineExternalDefinitions"->True}
   ,"RuntimeOptions"->"Speed"];
]*)
 


(*Wrap plots in an event handler, so that they respond to clicks*)
ClearAll[WrapStrideClickEventHandler];
(*Must be HoldFirst, so strideIndex emulates. HoldAll does not work becuase strideList does not get expanded!*)
Attributes@WrapStrideClickEventHandler=HoldFirst; 
WrapStrideClickEventHandler[strideIndex_,strideList_,plot_]:=
    MouseAppearance[EventHandler[plot,
    (*Change the stride on clicks*)
          {"MouseClicked", 1}:>Block[{pt},
            pt=First@MousePosition["Graphics"];
(*                  Print[Unevaluated@strideIndex, ": ", strideIndex];
                    Print[strideList];
                    Print["pt: ",pt, " si: ",GetIndexFromList[pt,strideList]];*)
            strideIndex=GetIndexFromList[pt, strideList];
            ]
    ,PassEventsDown->True],"LinkHand"];                    

 
crossPlotMarker=Graphics[{Line[{{-1, 1}, {1, -1}}], Line[{{1, 1}, {-1, -1}}]}];
Options[DrawStridePlotsFromBinInfo] = { "Verbose":>$VerbosePrint,  (*Log output*)
                                        "VerboseLevel":>$VerboseLevel, (*The amount of details to log. Higher number means more details*)
                                        (*"StrideIndex"->1, (*The stride index for the displayed histogram*)
                                        "BinIndex"->1,     (*The bin index for the plots*)*)
                                        "HistogramPresentation"->Draw2DHistogram,
                                        "MarkNonNormal" -> True,(*Should non normal distributions be marked?*)
                                        "Clickable" -> True,(*Wrap the plots in an EventHandler so that stride can be changed*)
                                        PlotStyle -> Automatic,
                                        PlotMarkers->{Graphics@{Disk[]},0.03},
                                        ImageSize -> Medium
                                      };


Attributes[DrawStridePlotsFromBinInfo]={HoldRest};
(*Hold all emulates pass-by-refrence schematics, so the value of stride and bin can be changed from inside here. 
binIndex ,strideIndex must be symbols *)
DrawStridePlotsFromBinInfo[binInfos:diffInfosWithStride,binIndex_,strideIndex_,opts:OptionsPattern[]] := 
Block[{$VerbosePrint=OptionValue["Verbose"], $VerboseLevel=OptionValue["VerboseLevel"],$VerboseIndentLevel=$VerboseIndentLevel+1}, 
    Block[ {strides,stridex,stridexPlot,stridey,strideN, strideA, strideyPlot,strideplotopts,strideHistogram, plotStyle,
            bin,strideAPlot,strideNPlot,normals,nonNormalX,nonNormalY, nonNormalN, nonNormalA, injectedOptions, normalstr,Dx,Dy,Da,histDraw,histIs3D,allStrideNEmpty},
       Puts["***DrawStridePlotsFromBinInfo****"];
       PutsOptions[DrawStridePlotsFromBinInfo, {opts}, LogLevel->2];

       plotStyle=If[#===Automatic,Directive[Thick,ColorData[1][1]],#]&@OptionValue[PlotStyle];

       bin = GetValue[{"x","y"},binInfos[[binIndex,strideIndex]]];
       
       normals = GetValues["IsNormal",binInfos[[binIndex]] ];       
       strides = GetValues["Stride",binInfos[[binIndex]]];
       stridex = GetValues[{"Stride","Dx"},binInfos[[binIndex]]];

       (*If all stridex are missing just return empty graphics*)
       If[And@@(MatchQ[#,Missing[___]]&/@stridex[[All,2]]), 
           Return[
	           {"SelectedBinCenter"->bin,"SelectedStride"->stridex[[strideIndex,1]], "SlectedHistogram"->Graphics[],
	            "StridePlotX"->Graphics[],"StridePlotY"->Graphics[],
	            "StridePlotA"->Graphics[],"StridePlotN"->Graphics[]}
	            ];     
       ];
       stridey = GetValues[{"Stride","Dy"},binInfos[[binIndex]]];
       strideN = GetValues[{"Stride","StepsInBin"},binInfos[[binIndex]]];
       (*carefull. Null devided by anything will not match this*)
       allStrideNEmpty=And@@(MatchQ[#,Null]&/@strideN[[All,2]]);
       strideN[[All,2]] = strideN[[All,2]]/1000.;
       strideA= GetValues[{"Stride","Da"},binInfos[[binIndex]]];
       

       strideplotopts = Sequence[
                          Frame->True,ImageSize->Medium,LabelStyle->Medium, Axes->False,
                          PlotRange->{All,{0,All}},Joined->True,GridLines->{{stridex[[strideIndex,1]]},{}},
                          GridLinesStyle->{Directive[Dashed],Automatic},
                          PlotStyle->plotStyle, ImagePadding -> {{40, 10}, {40, 0}},
                          PlotMarkers->OptionValue@PlotMarkers
                        ];
  
       stridexPlot = ListPlot[stridex,FrameLabel->{"Stride","Dx"},strideplotopts]; 
       strideyPlot = ListPlot[stridey,FrameLabel->{"Stride","Dy"},strideplotopts];        
       strideAPlot = ListPlot[strideA,FrameLabel->{"Stride","Da"},PlotRange->{Automatic,{0,180}},strideplotopts];
       strideNPlot = If[allStrideNEmpty, Graphics[],
                     ListPlot[strideN,FrameLabel->{"Stride","StepsInBin (x 1000)"},strideplotopts]];       
       
       If[OptionValue@"MarkNonNormal", (*then*)
           nonNormalX=Pick[stridex,normals,False];
           nonNormalY=Pick[stridey,normals,False];
           nonNormalA=Pick[strideA,normals,False];
           nonNormalN=Pick[strideN,normals,False];     
           If[nonNormalX=!={},
           stridexPlot=Show[{stridexPlot, ListPlot[nonNormalX,PlotMarkers -> {crossPlotMarker, .04}, Joined->False, GridLines->{},strideplotopts]}]];
           If[nonNormalY=!={},
           strideyPlot=Show[{strideyPlot, ListPlot[nonNormalY,PlotMarkers -> {crossPlotMarker, .04}, Joined->False, GridLines->{},strideplotopts]}]];
           If[nonNormalA=!={},
           strideAPlot=Show[{strideAPlot, ListPlot[nonNormalA,PlotMarkers -> {crossPlotMarker, .04}, Joined->False, GridLines->{},strideplotopts]}]];
           If[nonNormalN=!={},
           strideNPlot=Show[{strideNPlot, ListPlot[nonNormalN,PlotMarkers -> {crossPlotMarker, .04}, Joined->False, GridLines->{},strideplotopts]}]];

       ];

       If[OptionValue@"Clickable", (*then*)
           stridexPlot = WrapStrideClickEventHandler[strideIndex,strides,stridexPlot];             
           strideyPlot = WrapStrideClickEventHandler[strideIndex,strides,strideyPlot];              
           strideAPlot = WrapStrideClickEventHandler[strideIndex,strides,strideAPlot];  
           strideNPlot = WrapStrideClickEventHandler[strideIndex,strides,strideNPlot];  
       ];
          
       strideHistogram = "StepsHistogram"/.(binInfos[[binIndex,strideIndex]]);
       normalstr = ToString@If[normals[[strideIndex]],"Normal distribution","Is NOT Normal dist"]; 

       histDraw=OptionValue["HistogramPresentation"];
       histIs3D=StringMatchQ[SymbolName[histDraw],___~~"3D"~~___];
       injectedOptions=FilterRules[{PlotRange->All}~Join~{opts}~Join~Options@DrawStridePlotsFromBinInfo,Options@histDraw];
 
       (*Draw the histogram or show the normalstr  *)
       strideHistogram = If[ !(MatchQ[strideHistogram,Missing[___]] || strideHistogram===Null || OptionValue["HistogramPresentation"] === Null ||  OptionValue["HistogramPresentation"] === None),
                          (*then*)
                            If[histIs3D, (*then*)                             
                                Show[histDraw[strideHistogram,"FillStyle"->plotStyle,"OutlineStyle"->plotStyle,injectedOptions], AxesLabel-> {"Steps x","Steps y","P(x,y)"}, PlotLabel-> normalstr, RotationAction -> "Clip",ImageSize->{300,300},LabelStyle->Medium]
                            ,(*else*) 
                                Show[histDraw[strideHistogram,injectedOptions], FrameLabel->{{"Steps y",""},{"Steps x",normalstr}} ,ImageSize->{300,300},LabelStyle->Medium]
                            ] 
                          ,(*else*)
                            (*SpanFromLeft*)
                             (*Graphics[Style[Text@"No histogram",Large],ImageSize->{300,300}]*)
                             If[histIs3D,Graphics3D[],Graphics[]]
                         ];
       
       
       {"SelectedBinCenter"->bin,"SelectedStride"->stridex[[strideIndex,1]], "SelectedHistogram"->strideHistogram,
       "StridePlotX"->stridexPlot,"StridePlotY"->strideyPlot,
       "StridePlotA"->strideAPlot,"StridePlotN"->strideNPlot}
    ]
   ];

DrawStridePlotsFromBinInfo[diffInfos:listOfDiffInfosWithStide,binIndex_,strideIndex_,opts:OptionsPattern[]] := 
Block[{$VerbosePrint=OptionValue["Verbose"], $VerboseLevel=OptionValue["VerboseLevel"],$VerboseIndentLevel=$VerboseIndentLevel+1,i}, 
    Module[{plotStyles,plots,stridexPlot,strideyPlot,strideAPlot,strideNPlot,strides,histograms},
        Puts["***DrawStridePlotsFromBinInfo (multiple lists)****"];

        PutsOptions[DrawStridePlotsFromBinInfo, {opts}, LogLevel->2];
        
        plotStyles = If[ #===Automatic,
                         Table[Directive[Thick,Opacity@1,ColorData[1][i]],{i,Length@diffInfos}]
                        ,#]&@OptionValue[PlotStyle];
        Puts["plotStyles: ",plotStyles];
         
        plots=Table[DrawStridePlotsFromBinInfo[diffInfos[[i]],binIndex,strideIndex,
                PlotStyle->plotStyles[[i]],"Clickable"->False,(*"HistogramPresentation" -> DrawSmooth3DHistogram,*) opts]
            ,{i,Length@diffInfos}];
       
       histograms= GetValues["SelectedHistogram",plots];
       (*Some bins may not exist, be missing. So Delete the "SelectedHistogram" text before showing*) 
       histograms=DeleteCases[histograms,"SelectedHistogram"];
       histograms=Show@@histograms;   
            
       stridexPlot = Show@@DeleteCases[GetValues["StridePlotX",plots],"StridePlotX"];
       strideyPlot = Show@@DeleteCases[GetValues["StridePlotY",plots],"StridePlotY"];
       strideAPlot = Show@@DeleteCases[GetValues["StridePlotA",plots],"StridePlotA"];
       strideNPlot = Show@@DeleteCases[GetValues["StridePlotN",plots],"StridePlotN"];
       (*Only the first binInfo is made clickable*)
       If[OptionValue@"Clickable", (*then*)
           strides = GetValues["Stride",diffInfos[[1,binIndex]]];
           stridexPlot = WrapStrideClickEventHandler[strideIndex,strides,stridexPlot];             
           strideyPlot = WrapStrideClickEventHandler[strideIndex,strides,strideyPlot];              
           strideAPlot = WrapStrideClickEventHandler[strideIndex,strides,strideAPlot];  
           strideNPlot = WrapStrideClickEventHandler[strideIndex,strides,strideNPlot];  
       ];
      
   
        (*rules=plots[[1,All,1]];(*all the rules names. Could also be DeleteDuplicates@plots[[All,All,1]]*);  *)
        {"SelectedBinCenter"->GetValue["SelectedBinCenter",First@plots],
         "SelectedStride"->GetValue["SelectedStride",First@plots],
         "SelectedHistogram"->histograms,
         "StridePlotX"->stridexPlot,"StridePlotY"->strideyPlot,
         "StridePlotA"->strideAPlot,"StridePlotN"->strideNPlot}
                  

    ]
]


ClearAll[DrawDiffsWithStridePlots];

(*binindex_,strideIndex_ can be passed by value, that is why HoldRest is needed *)
SetAttributes[DrawDiffsWithStridePlots,HoldAll];

Options[DrawDiffsWithStridePlots] = {
                      "CellRange"->Automatic,
                      "Verbose":>$VerbosePrint,  (*Log output*)
                      "VerboseLevel":>$VerboseLevel,
                      "MarkNonNormal" -> True,(*Should non normal distributions be marked?*)
                      PlotStyle->Automatic,
                      Frame->True,
                      "Clickable"->False,
                      "Scale"->3,
                      "HistogramPresentation" -> DrawSmooth3DHistogram,
                      "MarkSelectedBin" -> True,
                      "LegendCaptions"->None
                      }~Join~
                      Options[DrawDiffusionTensorRepresentations]~Join~
                      Options[DrawStridePlotsFromBinInfo];

DrawDiffsWithStridePlots[diffs_,binIndex_,strideIndex_,opts:OptionsPattern[]] :=
Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1},
    Module[ {diffsDim,bins,cellRange,tensors, binfo,tensorRepOpts,stridePlotOpts, selectedStr, legendRow},
        Puts["****DrawDiffsWithStridePlots****"];
        PutsOptions[DrawDiffsWithStridePlots,{opts},LogLevel->2];
        diffsDim = Dimensions@diffs;
        Assert[(Length@diffsDim==3) || (Length@diffsDim==4),"diffs must be either a list of {bins, stride, rules} or {diff, bins, stride, rules}"];
        bins=GetValues[{{"x", "y"}, {"xWidth", "yWidth"}}, diffs[[All, 1]]];
        cellRange = If[ #===Automatic,GetCellRangeFromBins@bins,#]&@OptionValue@"CellRange";
        
        tensorRepOpts=FilterRules[{opts}~Join~Options[DrawDiffsWithStridePlots],Options@DrawDiffusionTensorRepresentations];
        
        If[ Length@diffsDim==3, (*One set of diffusions*)
            tensors = DrawDiffusionTensorRepresentations[diffs[[All,strideIndex]],binIndex, Evaluate@tensorRepOpts];
            (*else multiple sets of diffusion*),
            tensors = DrawDiffusionTensorRepresentations[diffs[[All,All,strideIndex]],binIndex, Evaluate@tensorRepOpts];
        ];
        
        stridePlotOpts=FilterRules[{opts}~Join~Options[DrawDiffsWithStridePlots],Options@DrawStridePlotsFromBinInfo];
     
        binfo = DrawStridePlotsFromBinInfo[diffs,binIndex,strideIndex,Evaluate@stridePlotOpts];
        selectedStr=Style[StringForm["Slected bin `1` Stride `2`",GetValue["SelectedBinCenter",binfo],GetValue["SelectedStride",binfo]],Medium];
        legendRow=If[OptionValue["LegendCaptions"]=!=None && Length@diffsDim==4,
        legendRow=Row@RowLegend[OptionValue@"LegendCaptions",PlotStyle->OptionValue@PlotStyle]
        (*else*), Style["",0]];
        
        Column[{
            legendRow,
            Grid@Partition[{
               Labeled[tensors,selectedStr,Top], If[#===None,SpanFromLeft,#]&@GetValue["SelectedHistogram",binfo],
               GetValue["StridePlotX",binfo],    GetValue["StridePlotY",binfo],
               GetValue["StridePlotA",binfo],    GetValue["StridePlotN",binfo]
            },2]
        },Center]
    ]
]    

ClearAll[ViewDiffsWithStridePlots];
SetAttributes[ViewDiffsWithStridePlots,HoldFirst];
Options[ViewDiffsWithStridePlots] = {
                      "SaveDefinitions"->False,
                      "ShowNotebook"->True, 
                      "FileName"->Null, 
                      "CellRange"->Automatic,
                      "Verbose":>$VerbosePrint,  (*Log output*)
                      "VerboseLevel":>$VerboseLevel,
                      "Scale"-> 1,
                      "MarkNonNormal" -> True,(*Should non normal distributions be marked?*)
                      PlotStyle->Automatic,
                      Frame->True
                      }~Join~
                      Options[DrawDiffsWithStridePlots];
ViewDiffsWithStridePlots[diffs_,title_,opts:OptionsPattern[]] :=
Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1,
        binIndex,scale, strideIndex,histogramPresentation,markNonNormal,showPlotMarkers,showNormalizedDiff,showMinBins(*These are just for syntax higlighting inside WB*)},
     Module[ {man,nb,diffsDim,histogramButtons, stepdeltaLength,drawDiffsOpts,strideFactor},
         Puts["****ViewDiffsWithStridePlots****"];
         PutsOptions[ViewDiffsWithStridePlots,{opts},LogLevel->2];
         diffsDim=Dimensions@diffs;
         Assert[(Length@diffsDim==3) || (Length@diffsDim==4),"diffs must be either a list of {bins, stride, rules} or {diff, bins, stride, rules}"];
         (*Not all histogram presentations work well when presenting multiple data sets*)
         If[Length@diffsDim==3,
             histogramButtons = {Draw2DHistogram->"2D Histogram",DrawSmooth3DHistogram->"Smooth 3D Histogram", (*Draw3DHistogram->"3D Histogram",*)Null->"None"};
             stepdeltaLength = Length@First@diffs; (*All the bines have the same strides*)
         ,(*else*)
             histogramButtons = {DrawSmooth3DHistogram->"Smooth 3D Histogram", Null->"None"};
             stepdeltaLength = Length@diffs[[1,1]];
         ]; 
         

         
         drawDiffsOpts=FilterRules[{opts}~Join~Options[ViewDiffsWithStridePlots],Options@DrawDiffsWithStridePlots];           
         man = DynamicModule[{},
	         Manipulate[
	             Puts["Manipulate evaluate: ViewDiffsWithStridePlots"];    
                 
                          If[ showNormalizedDiff, (*then*)
                              strideFactor = 1,
                          (*else get the curernt stride*)
                              If[ Length@diffsDim==3,
                                  strideFactor = ("Stride"/.diffs[[binIndex,strideIndex]]),(*else*)
                                  strideFactor = ("Stride"/.diffs[[1,binIndex,strideIndex]])
                              ];
                          ];
                 DrawDiffsWithStridePlots[diffs,binIndex,strideIndex,"HistogramPresentation"->histogramPresentation, 
                     "MarkNonNormal"->markNonNormal, "Clickable"->True, "Scale"->scale*strideFactor,
                     "ShowMinBins"->showMinBins,
                     PlotMarkers->If[showPlotMarkers,{Graphics@{Disk[]},0.03},None], Evaluate@drawDiffsOpts]	             
	         ,Style[title,Large]
			    ,Row[{
			       Control@{{scale,OptionValue@"Scale","Disc Scale"},0,10,Appearance->"Labeled"}
			      ,Control@{{strideIndex,1,"Stride"},1,stepdeltaLength,1,Appearance->"Labeled"}
			       }]                         
	         ,Row[{
	               Control@{{histogramPresentation,DrawSmooth3DHistogram,"Histogram presentation"},histogramButtons}," "
	              ,Column[{
	                  Control@{{markNonNormal,OptionValue@"MarkNonNormal","Mark nonormal"},{True,False}}
	                 ,Control@{{showPlotMarkers,True,"Show markers"},{True,False}}
	                      }]," "
                  ,Column[{
                      Control@{{showNormalizedDiff,True,"Show time normalized diff"},{True,False}}
                     ,Control@{{showMinBins,False,"Show min bins"},{True,False}}
                          }]	                      
	             }]
	         ,{{binIndex ,1},None}
	         ,{{strideIndex, 1}, None},
	         SaveDefinitions->OptionValue["SaveDefinitions"],TrackedSymbols:>True(*{strideIndex,binIndex,scale,histogramPresentation}*),AppearanceElements->All,Alignment->Center
	         ]];
	         
	         
        nb = CreateDocument[{man},
                WindowTitle->title,
                WindowMargins->Automatic,
                WindowSize->{All,Automatic},
                ShowCellBracket->False,
                Deployed->True,
                Visible->OptionValue["ShowNotebook"]
            ];
            (*SelectionMove[nb,Before,Notebook];*)
        If[ OptionValue["FileName"]=!=Null,
            Export[OptionValue["FileName"],nb,"CDF"]
        ];
        If[ (OptionValue["FileName"]=!=Null) && (!OptionValue["ShowNotebook"]),
            NotebookClose[nb]
        ];
    ];
    ];

ClearAll[ExportDiffs];
ExportDiffs[title_,datafile_,dir_:"finished"] :=
    ViewDiffs[title,datafile, "ShowNotebook"->False,"SaveDefinitions"->True,"FileName"->FileNameJoin[{dir,title<>".cdf"}]];


End[] (* End Private Context *)

EndPackage[]
