(* Mathematica Package *)

(* Created by the Wolfram Workbench 14.1.2013 *)

BeginPackage["DiffusiveDynamics`Analyze2D`",{"DiffusiveDynamics`Utils`","DiffusiveDynamics`Visualize2D`"}]
(* Exported symbols added here with SymbolName::usage *) 

$Analyze2DCompilationTarget="C";

ClearAll[GetDiffusionInBin];
GetDiffusionInBin::usage="TODO"

ClearAll[GetDiffusionInBins]
GetDiffusionInBins::usage="TODO"

ClearAll[compiledSelectBin];
compiledSelectBin::usage="compiledSelectBin[points, min, max] 
Takes a list of 2D points and the boundaries of the bin (min, max). 
Returns all the points that are in the bin (between min and max)";

ClearAll[CompileFunctions,CompileFunctionsIfNecessary];
CompileFunctions::usage="CompileFunctions[] recompiles all CompiledFunctions in package. Use $Analyze2DCompilationTarge to set the compilation target to \"C\" or \"VWM\"";
CompileFunctionsIfNecessary::usage="CompileFunctionsIfNecessary[] only compiles functions if they have not been compiled yet";

ClearAll[GetDiffusionInBinsBySelect];
GetDiffusionInBinsBySelect::usage="TODO";

ClearAll[GetDiffusionInfoFromParameters];
GetDiffusionInfoFromParameters::usage="Returns a list of rules from the DiffX, DiffY, DAlpha functions."

Begin["`Private`"]
(* Implementation of the package *)

 
(*SetAttributes[GetDiffusionInBin,HoldFirst];*)

Options[GetDiffusionInBin] = {"Verbose":>$VerbosePrint,  (*Log output*)
						   "VerboseLevel":>$VerboseLevel,(*The amount of details to log. Higher number means more details*)
                           "Strides"->1.,     (*How many points between what is considered to be a step- 1 means consecutive points in the raw list*)
                           "PadSteps"->True,     (*Take points that are outside of the bin on order to improve the statistics *)
                           "ReturnRules"->True,  (*Return the results as a list of rules*)
                           "ReturnStepsHistogram"->False, (*Returns the density histogram of steps. Slows down the calcualtion considerably. Useful for debuging and visualization.*)
                           "WarnIfEmpty"->False (*Prints a warning if too litle points in bin*)
           };

GetDiffusionInBin[rwData_,dt_,{min_,max_},{cellMin_,cellMax_},opts:OptionsPattern[]] :=
Block[{$VerbosePrint=OptionValue["Verbose"], $VerboseLevel=OptionValue["VerboseLevel"],$VerboseIndentLevel=$VerboseIndentLevel+1}, 
    Module[ {data,t,diffs,bincenter,binwidth,sd},
            Puts["***GetDiffusionInBin****"];
			Assert[Last@Dimensions@rwData==3,"Must be a list of triplets in the form {t,x,y}"];
			Assert[Length@Dimensions@rwData==3,"rwData must be a list of lists of triplets!"];
            Puts[Row@{"\ndt: ",dt,"\nmin: ",min,"\nmax: ",max,"\ncellMin: ",cellMin,"\ncellMax: ",cellMax},LogLevel->2];
            PutsOptions[GetDiffusionInBin, {opts}, LogLevel->2];
			CompileFunctionsIfNecessary[];
            PutsE["original data:\n",rwData,LogLevel->5];
            Puts["Is packed original data? ",And@@Developer`PackedArrayQ[#]&/@rwData, LogLevel->5];

            bincenter = (max+min)/2.;
            binwidth = (max-min);
            (*select the steps in the bin*)
            (*Take just the step's indexes*)
            t = AbsoluteTiming[   
                   data = Map[Part[compiledSelectBin[#,min,max],All,1]&,rwData];
             ];

            Puts["select data took: ", First@t,LogLevel->3];
            PutsE["after select:\n",data, LogLevel->3];

            (*if OptionValue["Strides"] only one number and not a list convert it to list {x}*)
            sd = If[ NumericQ[OptionValue["Strides"]],
                     {OptionValue["Strides"]},
                     OptionValue["Strides"]
                 ];
            
            If[OptionValue@"WarnIfEmpty",On[GetStepsFromBinnedPoints::zerodata],
                                         Off[GetStepsFromBinnedPoints::zerodata]];
            
            diffs = Map[With[{stepdelta = #},
                            GetDiffsFromBinnedPoints[data,rwData,dt,{min,max},{cellMin,cellMax}
                            ,"Stride"->stepdelta
                            ,"PadSteps"->OptionValue["PadSteps"],"ReturnStepsHistogram"->OptionValue["ReturnStepsHistogram"]]
                        ]&,sd];
            diffs
        ]
]; 

ClearAll[compiledSelectBinFunc];
compiledSelectBinFunc = Compile[{{point,_Real, 1},{min,_Real, 1},{max,_Real, 1}},
  ((point[[2]]>=min[[1]] )&&(point[[2]]<=max[[1]])&&(point[[3]]>=min[[2]] )&&(point[[3]]<=max[[2]]))
,  
   CompilationOptions->{"ExpressionOptimization"->True,"InlineExternalDefinitions"->True},
   "RuntimeOptions"->"Speed"];
  
(*The delayed definition := is just a trick to compile functions on first use. makes packages load quicker*)	
ClearAll[compiledSelectBinUncompiled];
compiledSelectBinUncompiled[]:= Block[{points,min,max},   (*This block is just for syntax coloring in WB*) 
compiledSelectBin = Compile[{{points,_Real, 2},{min,_Real, 1},{max,_Real, 1}},
  Select[points,compiledSelectBinFunc[#,min,max]&]
,  CompilationTarget->$Analyze2DCompilationTarget,
   CompilationOptions->{"ExpressionOptimization"->True,"InlineCompiledFunctions"->True,"InlineExternalDefinitions"->True},
   "RuntimeOptions"->"Speed"];
] 

ClearAll[GetDiffsFromBinnedPoints];
(*SetAttributes[GetDiffsFromBinnedPoints,{HoldAll}];*)
Options[GetDiffsFromBinnedPoints] = {"Verbose":>$VerbosePrint,  (*Log output*)
						        "VerboseLevel":>$VerboseLevel,(*The amount of details to log. Higher number means more details*)
                                "Stride"->1., (*kdaj sta dva koraka zaporedna*)
                                "PadSteps"->True, (*if NextStepNum steps should be added to each continous traj*)
                                "ReturnStepsHistogram"->True};
GetDiffsFromBinnedPoints[binnedInd_,rwData_,dt_,{min_,max_},{cellMin_,cellMax_},opts:OptionsPattern[]] :=
    Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1},
        Module[ {steps, bincenter = (min+max)/2,binwidth = (max-min)},
            Puts["***GetDiffsFromBinnedPoints***"];
            PutsOptions[GetDiffsFromBinnedPoints,{opts},LogLevel->2];
            
            steps = Developer`ToPackedArray@Flatten[
                    Table[GetStepsFromBinnedPoints[binnedInd[[i]],rwData[[i]],dt,{min,max},{cellMin,cellMax}, 
                            "Stride"->OptionValue["Stride"], "Verbose"->OptionValue["Verbose"],"PadSteps"->OptionValue["PadSteps"]]
                          ,{i,Length[rwData]}]
                  ,1];

            PutsE["Steps:\n",steps,LogLevel->5];

            GetDiffsFromSteps[steps,dt, OptionValue["Stride"]]~Join~{"StepDelta"->OptionValue["Stride"], "x"->bincenter[[1]], "y"->bincenter[[2]],
                      "xWidth"->binwidth[[1]], "yWidth"->binwidth[[2]], "StepsInBin"->Length[steps],
                      "StepsHistogram"->If[ OptionValue["ReturnStepsHistogram"],
                                            QucikDensityHistogram[steps,30]
                                        ]}
        ]
    ];


ClearAll[GetStepsFromBinnedPoints];
Options[GetStepsFromBinnedPoints] = {"Verbose":>$VerbosePrint,  (*Log output*)
						        "VerboseLevel":>$VerboseLevel,  (*The amount of details to log. Higher number means more details*)
                                "Stride"->1.,                   (*Number of points between a step*)
                                "PadSteps"->True                (*if Stride steps should be added to each continous segment of the trayectory in bbin*)};
(*GetStepsFromBinnedPoints::few =                 "Few steps (`1`) when constructing histogram in bin {`2`,`3`}!";
GetStepsFromBinnedPoints::tofewAbort =          "To few steps (`1`) To construct histogram in bin {`2`,`3`}! Returning Missing value";
*)
GetStepsFromBinnedPoints::zerodata =            "Zero data (empty list) in bin {`1`,`2`}! Returning empty list";
GetStepsFromBinnedPoints[binnedInd_,rwData_,dt_,{min_,max_},{cellMin_,cellMax_},opts:OptionsPattern[]] :=
Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1},
    Module[ {data,indpaths,cellWidth,bincenter,ds,BeforeAppendLenghts,AfterAppendLengths},
            Puts["********GetStepsFromBinnedPoints********"];
            PutsOptions[GetStepsFromBinnedPoints,{opts},LogLevel->2];
            Puts[Row@{"min: ", min," max: ", max},LogLevel->2];
            Puts[Row@{"cellMin: ", cellMin," cellMax: ", cellMax},LogLevel->2];
            PutsE["rwData:\n",rwData,LogLevel->5];
            PutsE["binnedInd:\n",binnedInd,LogLevel->5];
            Puts["Length rwData: ",Length[rwData],LogLevel->3];
            If[ Length@binnedInd>0, (*then*)
                Puts["binnedInd min: ",Round@binnedInd[[1]]," max: ", Round@binnedInd[[-1]],LogLevel->2];
            ];

            ds = OptionValue["Stride"];
            If[ Length[binnedInd]==0,(*then*)
                Message[GetStepsFromBinnedPoints::zerodata,min,max];
                Return[{}];
            ];
            cellWidth = (cellMax-cellMin);
            bincenter = (min+max)/2;


            (*Split indices into continuous segments. 
            One sub list is inside the bin the whole time. Works by splitting the list if the difference in steps is not 1.*)
            indpaths = Split[binnedInd,#2-#1==1.&];

            PutsE["Indpaths lengths:\n",BeforeAppendLenghts=Length/@indpaths,LogLevel->3];
            Puts["Mean: ",N@Mean[Length/@indpaths],LogLevel->2];
            Puts[Histogram[Length/@indpaths,{5},PlotRange->{{0,100},Automatic}],LogLevel->5];
            (*Add ds steps to the beggining and end, so that we get at least 2 steps for each point in bin at largest ds*)
            If[ OptionValue["PadSteps"], (*then*)
                indpaths = Map[AppendLeftRight[#,ds,Length[rwData]]&, indpaths];
                PutsE["Indpaths lengths (after padding):\n",AfterAppendLengths=Length/@indpaths,LogLevel->3];
                Puts["Mean (after padding): ",N@Mean[Length/@indpaths],LogLevel->2];
                PutsE["Indpaths lengths differences:\n",AfterAppendLengths-BeforeAppendLenghts];
            ];
            
            data = rwData[[Round@#]]&/@indpaths;
            PutsE["All data:\n",data,LogLevel->5];
            Puts["data is packed: ", And@@(Developer`PackedArrayQ[#]&/@indpaths),LogLevel->5];
            Puts[
             Module[ {dp},
                 dp = StrideData[Flatten[data,1],5000];
                 (*dp=Tooltip[#[[{2,3}]],#[[1]]]&/@dp;*)
                 dp = dp[[All,{2,3}]];
                 ListPlot[dp,Frame->True,PlotStyle->Opacity[.4],PlotRange->Transpose[{cellMin,cellMax}],
                        Epilog->{Opacity[0],EdgeForm[{Red,Thick}],Rectangle[min,max]},
                        ImageSize->Medium]
             ],LogLevel->3];



           (*calculate the steps (diferences between consecutive points*)
            data = GetDifferences[#,ds]&/@data;
            (*Delete empty lists*)
            data = DeleteCases[data,{{}}];
            PutsE["Steps:\n",data, LogLevel->5];
            data = Developer`ToPackedArray[Flatten[data,1]];

            PutsE["Consecutive steps:\n",data,LogLevel->5];
            Puts["Steps are is packed: ", Developer`PackedArrayQ[data], LogLevel->5];
            Puts["STEP LENGTH: ",Length[data],LogLevel->2];

            If[ Length[data]==0,(*then*)
                Message[GetStepsFromBinnedPoints::zerodata,min,max];
                Return[{}];
            ];
            (*If[Length[data]<500,Message[GetDiffFromBinnedData::tofewAbort,Length[data],min,max];Return[Missing[StringForm["To few ``",Length[data]]]]];
            If[Length[data]<1000,Message[GetDiffFromBinnedData::few,Length[data],min,max]];*)
            data = data[[All,{2,3}]];

            (*Pick the shortest of the steps. This is needed if the step was over the periodic boundary limit. 
            Used for each ccordinate seperatly.
            MakeInPeriodicCell is listable.*)
            data[[All,1]] = MakeInPeriodicCell[data[[All,1]], cellWidth[[1]]];
            data[[All,2]] = MakeInPeriodicCell[data[[All,2]], cellWidth[[2]]];
            Puts["data is packed: ", Developer`PackedArrayQ[data]];
            (*Puts[Module[{g,plr},
              g=DensityHistogram[data,30(*,PerformanceGoal->"Speed"*)];
              plr={Min@#1,Max@#2}&@@Transpose[PlotRange/.Options[g]];
              Show[g,PlotRange->{plr,plr}]

            ]];*)
            (*alternativno iz drugih momentov*)
            Return[data];
        ]
    ];
(************************************************************************)
ClearAll[GetDifferences,GetDifferencesUncompiled];
GetDifferencesUncompiled[] := Block[{l,ds}, 
GetDifferences = Compile[{{l,_Real, 2},{ds,_Integer,0}},
  Block[ {lr},
      lr = {{}};
      (*If[ds==1,(*then*)lr=Differences[l];];*)
      If[ Length[l]-ds>0,(*if list is long enoug then*)
          lr = Drop[l,ds]-Drop[l,-ds]
      ];
      lr
  ],
 CompilationTarget->$Analyze2DCompilationTarget, 
   CompilationOptions->{"ExpressionOptimization"->True,"InlineExternalDefinitions"->True},
   "RuntimeOptions"->{"Speed","CompareWithTolerance"->True}];
];


ClearAll[GetDiffsFromSteps];
(*Takes a list of steps {{dx,dy}, ...} and calculates the diffusion constants.
data -- list of steps
dt   -- the timestep in units of time
ds   -- the stride of timeteps (how many steps are skipped. Unitless. 

Returns a list of replacement rules for higher flexibility. 
*)
(*TODO: here it would be possible to get parameters using FindDistribution and MultinormalDistribution. 
Or add a test for normality in any other way*) 
GetDiffsFromSteps[data_,dt_,ds_] :=  Block[{$VerboseIndentLevel = $VerboseIndentLevel+1},
    Module[ {ux,uy,rDx,rDy,rDa,pVal=Null,isNormal=Null},
        Puts["***GetDiffsFromSteps***"];
        If[ Length[data]==0, (*If no steps then*)
            {ux,uy,rDx,rDy,rDa} = ConstantArray[Missing[],5];,(*else*)
            (*TODO: Perhaps add option to choose if we want the normality test and which test is wanted *)
            (*{ux,uy,rDx,rDy,rDa} = GetTensorFromMoments[data];*)
            {ux,uy,rDx,rDy,rDa,pVal,isNormal}=GetTensorFromMomentsWithNormalityTest[data];
            (*We fitted the principal sigmas... transform into diffusion and devide by StepsDelta*)
            {rDx,rDy} = ({rDx,rDy}^2)/(2 dt)/ds;
        ];
        {"Dx"->rDx,"Dy"->rDy,"Da"->rDa,"ux"->ux,"uy"->uy,"PValue"->pVal,"IsNormal"->isNormal}
    ]
];

Clear[GetTensorFromMoments];
(*Takes a list of steps {{dx,dy}, ...} and calculates the principal central second moment (standard deviation)
Principal here means that the tensor is diagonal. The central second moment is the Sum[(x-ux)^2]/N

Returns {ux,uy,sx,sy,alpha}
mx, my -- first moment
sx, sy -- standard deviation Sqtr[Sum[(x-ux)^2]/N] 

alpha is in Degrees from 0 to 180*)
Options[GetTensorFromMoments]={HoldFirst};
GetTensorFromMoments[data_] :=
    Module[ {ux,uy,a,b,c,eval,evec,sx,sy,alpha},
        ux = N[Mean[data[[All,1]]]];
        uy = N[Mean[data[[All,2]]]];
        (*Sum[(x-ux)^2]/N as a dot product, becasue it's faster*)
        a = 1/Length[data]*(data[[All,1]]-ux).(data[[All,1]]-ux);
        b = 1/Length[data]*(data[[All,2]]-uy).(data[[All,2]]-uy);
        c = 1/Length[data]*(data[[All,1]]-ux).(data[[All,2]]-uy);
		(*The tensor is symmetric*)
        {eval,evec} = Eigensystem[{{a,c},{c,b}}];
        (*return standard deviation of principle components*)
        {sx,sy} = Sqrt[eval];
        (*alpha is in Degrees*)
        alpha = ArcCos[ evec[[1,1]] ]/Degree*Sign[ArcSin[ evec[[1,2]] ]];
        (*Interval from 0 to 180*)
        alpha = Mod[alpha,180];
        Return[{ux,uy,sx,sy,alpha}]
    ];

(*Takes a list of steps {{dx,dy}, ...} and calculates the principal central second moment (standard deviation)
Principal here means that the tensor is diagonal. The central second moment is the Sum[(x-ux)^2]/N

Returns {ux,uy,sx,sy,alpha, pVal,isNormal}
mx, my -- first moment
sx, sy -- standard deviation Sqtr[Sum[(x-ux)^2]/N] 
alpha -- is in Degrees from 0 to 180
pVal -- PValue from AndersonDarlingTest
isNormal -- Is this a normal distribution according to AndersonDarlingTest
*)

Options[GetTensorFromMomentsWithNormalityTest]={HoldFirst};
GetTensorFromMomentsWithNormalityTest[data_] :=
    Module[ {ux,uy,a,b,c,eval,evec,sx,sy,alpha,dist,hypothesis,params,pVal, isNormal},
		dist = MultinormalDistribution[{ux, uy}, {{a, c}, {c, b}}];
		hypothesis=AndersonDarlingTest[data, dist, "HypothesisTestData"];
		
		(*Is normal distribution?*)
		pVal=hypothesis["PValue"];
		isNormal=hypothesis["ShortTestConclusion"] == "Do not reject";
		(*Retrive the parameters*)
		params=hypothesis["FittedDistributionParameters"];
		{ux,uy,a,b,c}={ux,uy,a,b,c}/.params;
		
		(*The tensor is symmetric*)
        {eval,evec} = Eigensystem[{{a,c},{c,b}}];
        (*return standard deviation of principle components*)
        {sx,sy} = Sqrt[eval];
        (*alpha is in Degrees*)
        alpha = ArcCos[ evec[[1,1]] ]/Degree*Sign[ArcSin[ evec[[1,2]] ]];
        (*Interval from 0 to 180*)
        alpha = Mod[alpha,180];
        Return[{ux,uy,sx,sy,alpha,pVal,isNormal}]
    ];


ClearAll[MinAbs];
MinAbs[L_] :=
    First@Sort[L,Abs[#1]<Abs[#2]&];

ClearAll[MakeInPeriodicCell,MakeInPeriodicCellUncompiled];
MakeInPeriodicCellUncompiled[]:=Block[{x,cellwidth},  (*This block is just for syntax coloring in WB*)
(*
Steps through the whole cell are very improbable, so large steps are due to periodic conditions.
Takes a coordinate and chooses the smallest of the three options due to periodic conditions.
*)
MakeInPeriodicCell = Compile[{{x,_Real},{cellwidth,_Real}},
  If[ x<-(cellwidth/2.),(*then*)
      x+cellwidth,
   (*else*)
      If[ x>cellwidth/2., (*then*)
          x-cellwidth,
        (*else*)
          x
      ]
  ]
,CompilationOptions->{"ExpressionOptimization"->True,"InlineCompiledFunctions"->True,"InlineExternalDefinitions"->True},
 RuntimeAttributes->{Listable},
 CompilationTarget->$Analyze2DCompilationTarget,
 "RuntimeOptions"->"Speed"];
];


ClearAll[AppendLeftRight,AppendLeftRightUncompiled];
AppendLeftRightUncompiled[] := Block[{l,ds,max}, (*This block is just for syntax coloring in WB*)
(*Takes a list of indexes and tries to append ds consecutive numbers to the beginning and end 

l   --  list of array indexes in rwData. (sorted) 
ds  --  stride step
max --  length of rwData
TODO: If ds steps can't be added perhaps some smaller number can?*)
AppendLeftRight = Compile[{{l,_Real, 1},{ds,_Real,0},{max,_Real,0}},
  Block[ {lr}, 
      lr = l; (*Have to make a copy that can be modified*)
      If[ (First[lr]-ds)>0, (*if there are some points at the begining then*)
          lr = Range[First[lr]-ds,First[lr]-1]~Join~lr (*add the indexes*)
      ];
      If[ (Last[lr]+ds)<max, (*then*)
          lr = lr~Join~Range[Last[lr]+1,Last[lr]+ds]
      ];
      Return[lr]
  ],
 CompilationTarget->$Analyze2DCompilationTarget,
   CompilationOptions->{"ExpressionOptimization"->True,"InlineExternalDefinitions"->True},
   "RuntimeOptions"->{"Speed","CompareWithTolerance"->True}];
];  
 
   
$HaveFunctionsBeenCompiled=False;
(*recompiles teh functions by calling the set delayed definitions*)
CompileFunctions[] :=
    (Puts["Compiling functions in Anayze2D.m to ", $Analyze2DCompilationTarget,LogLevel->2];
     AppendLeftRightUncompiled[];
     compiledSelectBinUncompiled[];
     GetDifferencesUncompiled[];
     MakeInPeriodicCellUncompiled[];
     $HaveFunctionsBeenCompiled = True; 
    );

CompileFunctionsIfNecessary[]:= If[! $HaveFunctionsBeenCompiled, CompileFunctions[]];


   
(*GetDiffusionInBinsBySelect*)
(*Calculates the diffusion coeficients in all bins of the cell. Bining is re-done for each bin by select.  
 rwData --  Data of the diffusion process. A list of points {{t,x,y},...}
 dt     --  the timstep between two points in the units of time 
 binSpec -- specfication of the bins {min,max,dstep}
 cellMinMax The periodic cell limits {{minx,minY},{maxX, maxY}. If Null then taken form binSpec}*)

Options[GetDiffusionInBinsBySelect] = {"Parallel"->True,(*Use parallel functions*)
    								   "CellRange"->Automatic (*cell range in {{MinX,MinY},{MaxX,MaxY}}. If automatic, then calculated from binsOrBinSpec*)
							          }~Join~Options[GetDiffusionInBin];
Attributes[GetDiffusionInBinsBySelect]={HoldFirst};
GetDiffusionInBinsBySelect[rwData_,dt_,binsOrBinSpec_,opts:OptionsPattern[]] :=
    Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1},
        Module[ {bins, cellMin,cellMax},
            Puts["********GetDiffusionInBinsBySelect********"];
            PutsE["rwData: ",rwData, LogLevel->2];
            PutsE["binsOrBinSpec: ",binsOrBinSpec, LogLevel->2];
            Puts[$VerboseIndentString,"dimensions: ",Dimensions@binsOrBinSpec];
            PutsOptions[GetDiffusionInBinsBySelect, {opts}, LogLevel->2];
             
			bins=GetBinsFromBinsOrSpec@binsOrBinSpec;
            (*get cellMin/max from the bins if the option for cellRange is automatic*)
            {cellMin,cellMax}=If[#===Automatic,GetCellRangeFromBins@bins,#]&@OptionValue@"CellRange";
            PutsE["used cellRange: ",{cellMin,cellMax}, LogLevel->2];
            
            
			If[ OptionValue@"Parallel", (*then*)
            	(*I think distribute definitions also works. Actually it's more approapriate, but I'm not sure if it makes a copy or not...*)
                SetSharedVariable@rwData;
                With[ {IcellRange = {cellMin,cellMax},Idt = dt,injectedOptions = FilterRules[{opts},Options@GetDiffusionInBin]},
                    PutsE["injectedOptions:\n",injectedOptions, LogLevel->1];
                    ParallelMap[
                        GetDiffusionInBin[rwData, Idt, {#[[1]]-#[[2]]/2,#[[1]]+#[[2]]/2},IcellRange,injectedOptions]&
                        ,bins]
                ],(*else*)
                With[ {injectedOptions = FilterRules[{opts},Options@GetDiffusionInBin]},
                    PutsE["injectedOptions:\n",{injectedOptions}, LogLevel->3];
                    Map[
                         GetDiffusionInBin[rwData, dt, {#[[1]]-#[[2]]/2,#[[1]]+#[[2]]/2},{cellMin,cellMax},injectedOptions]&
                         ,bins]
                ]
            ] (*end if -- no semicolon here! as  the result of the Map / ParallelMap gets returned*)

        ]
    ];

Options[GetDiffusionInBinsByGatherBy] = {"Parallel"->True}~Join~Options[GetStepsFromBinnedPoints];
Attributes[GetDiffusionInBinsByGatherBy]={HoldFirst};
GetDiffusionInBinsByGatherBy[rwData_,dt_,binSpec_,cellMinMax_:Null,opts:OptionsPattern[]] :=
Block[{$VerbosePrint=OptionValue["Verbose"], $VerboseLevel=OptionValue["VerboseLevel"],$VerboseIndentLevel=$VerboseIndentLevel+1}, 
    Module[ {data,binnum,bincenters,bincentersWithSteps,MapFunction,
            binwidth,cellMin,cellMax,halfcellwidth,passopts,pointcount,t},

            Puts["********GetDiffusionInBinsByGatherBy********"];
            PutsOptions[GetDiffusionInBinsByGatherBy,{opts},LogLevel->2];
            (*Generate all the bin centers. The width is contant*)
            bincenters = Developer`ToPackedArray[
            	Tuples[{
	            	MovingAverage[Range@@binSpec[[1]],2],
		            MovingAverage[Range@@binSpec[[2]],2]
            }],Real];
            binwidth = Transpose[binSpec][[3]];
            (*If cellMinMax is not given, assume it's the same as the bin minmax*)
            If[ cellMinMax===Null,(*then*)
                cellMin = Transpose[binSpec][[1]];
                cellMax = Transpose[binSpec][[2]];(*else*),
                cellMin = cellMinMax[[1]];
                cellMax = cellMinMax[[2]];
            ]; 
            halfcellwidth = (cellMax-cellMin)/2;
            binnum = Length[bincenters];
            Puts[Row@{"cellMin: ", cellMin," cellMax: ", cellMax}];
            Puts["BinCenters: \n",bincenters];
            Puts["binwidth: ",binwidth];
            Puts["halfcellwidth: ",halfcellwidth];
            Puts["Data: ",data];
            (*Only bin the points once! This is the main point as we can avoid unecessary selects!*)
            (*Add a dummy bin step number -100.*)
            bincentersWithSteps = Transpose[Transpose[bincenters]~Prepend~ConstantArray[-100.,binnum]];
            Puts["bincentersWithSteps:\n",bincentersWithSteps,DisplayFunction->Identity];
            (*gather by vrne bine v vrstenm redu v katerem najde prvo toèko za tisti bin. Tako podatkom dodamo toèke za bine povrsti. Vzamemo samo toliko podlistov kot smo dodalitoèk (kot imamo binov), saj so ostali podlisti izven obmoèja. Tako se ognemo tudi še enemu selectu. Prvi element vsakega podlista zavržemo, saj je to naša dodana toèka, to naredi Take*)
            t = AbsoluteTiming[
            data = GatherBy[bincentersWithSteps~Join~data,Floor[(#[[2;;3]]+halfcellwidth)/binwidth]&];
                          (*Take just binums bins, the rest are out of binrange. The first element of each bin is our dummy bin, so we take the second to last points of each bin*)
            data = Take[data,binnum,{2,-1}];
            ];

              (*narišemo vse binne, ampak samo vsako n toèko v binu*)
            Puts["max pointcount per bin: ",pointcount = Max[(Length[#]&/@data)]];
            Puts[ListPlot[DeleteCases[data[[All,1;;-1;;Ceiling[pointcount/10000*binnum],{2,3}]],{}],
                 PlotRange->{{-CELLRANGE[[1]],CELLRANGE[[1]]},{-CELLRANGE[[2]],CELLRANGE[[2]]}}]];
            Puts["Po binningu:\n",data];
            Puts["Binning took: ",First@t];
            data = Developer`ToPackedArray[#[[All,1]]]&/@data;
            Puts["Indeksi:\n",data];
            Puts["Is packed first bin data? ",Developer`PackedArrayQ[First@data]];



            (*dodamo še sredino bina*)
            data = Transpose[{data,bincenters}];
            Puts["Po dodani sredini:\n",data];
            Puts["Is packed  data? ",Developer`PackedArrayQ[data]];


            (*sedaj pa naredimo map na teh podlistih*)
            MapFunction = If[ OptionValue["Parallel"],
                              ParallelMap,
                              Map
                          ];
            Puts["Using "<>ToString[MapFunction]];
            passopts = Evaluate[FilterRules[{opts}, Options[GetStepsFromBinnedData]]];
            data = MapFunction[(
                    Puts["bincenter:",#[[1]]];
                    GetStepsFromBinnedData[#[[1]],rwData,dt,{#[[2]]-binwidth/2,#[[2]]+binwidth/2},{cellMin,cellMax}, passopts]
                   )&,data];
            Return[data];
        ]
    ];


(*GetDiffusionInBins*)
(*Calculates the diffusion coeficients in all bins of the cell.
 rwData --  Data of the diffusion process. A list of points {{t,x,y},...}
 dt     --  the timstep between two points in the units of time 
 binSpec -- specfication of the bins {{minX,maxX,dstepX},{minY,maxY,dstepY}}
 cellRange The periodic cell limits {{minx,minY},{maxX, maxY}. If Null then taken form binSpec}*)

Options[GetDiffusionInBins] = {"Parallel"->True,(*Use parallel functions*)
							   "Method"->Select, (*The method of binning. For now select is redone for each bin. GatherBy would bin the data only once, but is not yet implemented*) 
							   "CellRange"->Automatic (*cell range in {{MinX,MinY},{MaxX,MaxY}}. If automatic, then calculated from binsOrBinSpec*)
							   }~Join~Options[GetDiffusionInBin];
Attributes[GetDiffusionInBins]={HoldFirst};
GetDiffusionInBins[rwData_,dt_,binSpec_,opts:OptionsPattern[]] :=
Block[{$VerbosePrint=OptionValue["Verbose"], $VerboseLevel=OptionValue["VerboseLevel"],$VerboseIndentLevel=$VerboseIndentLevel+1}, 
    Module[ {},
            Puts["********GetDiffusionInBins********"];
            PutsOptions[GetDiffusionInBins,{opts},LogLevel->2];
            
            Switch[OptionValue@"Method",
            Select,(*then*)
            	GetDiffusionInBinsBySelect[rwData,dt,binSpec, FilterRules[{opts},Options@GetDiffusionInBinsBySelect]],
            GatherBy, (*then*)
            	Assert[False, "GatherBy not yet implemented as method in GetDiffusionInBins"],
            _,(*else*)
            	Assert[False,"Wrong method "<>ToString@OptionValue@"Method"<>"in GetDiffusionInBins"]
            ]
        ]
    ];

ClearAll@GetDiffusionInfoForBinFromParameters;
GetDiffusionInfoForBinFromParameters[{{x_,y_},{dx_,dy_}},DiffX_,DiffY_,DiffA_]:=
 {"Dx"->DiffX[x,y],"Dy"->DiffY[x,y],"Da"->DiffA[x,y],
  "x"->x,"y"->y,"xWidth"->dx,"yWidth"->dy, "ux"->0,"uy"->0,
  "StepsHistogram"->Null,"StepsInBin"->0,"StepDelta"->0};
(*TODO: Perhaps I could fill a Gaussian StepsHistogram. StepDelta could be passed in as an option.  *)
Options@GetDiffusionInfoFromParameters = {
               "Verbose":>$VerbosePrint,  (*Log output*)
			   "VerboseLevel":>$VerboseLevel(*The amount of details to log. Higher number means more details*)
}

GetDiffusionInfoFromParameters[binsOrBinspec_, DiffX_,DiffY_,DiffA_]:=
Block[{$VerboseIndentLevel=$VerboseIndentLevel+1}, 
    Module[{bins},
        Puts["********GetDiffusionInfoFromParameters********"];
        (*PutsOptions[GetDiffusionInfoFromParameters,{opts},LogLevel->2];  *)      
        bins=GetBinsFromBinsOrSpec@binsOrBinspec;
        GetDiffusionInfoForBinFromParameters[#,DiffX,DiffY,DiffA]&/@bins     
    ]
]     

End[]

EndPackage[]

 