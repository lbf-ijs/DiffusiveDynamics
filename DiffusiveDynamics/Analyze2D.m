(* Mathematica Package *)

(* Created by the Wolfram Workbench 14.1.2013 *)

BeginPackage["DiffusiveDynamics`Analyze2D`",{"DiffusiveDynamics`Utils`","DiffusiveDynamics`Visualize2D`"}]
(* Exported symbols added here with SymbolName::usage *) 

$Analyze2DCompilationTarget="C";

ClearAll[GetDiffusionInBin];
GetDiffusionInBin::usage="TODO"

ClearAll[compiledSelectBin];
compiledSelectBin::usage="compiledSelectBin[points, min, max] 
Takes a list of 2D points and the boundaries of the bin (min, max). 
Returns all the points that are in the bin (between min and max)";

ClearAll[CompileFunctions,CompileFunctionsIfNecessary];
CompileFunctions::usage="TODO";
CompileFunctionsIfNecessary::usage="TODO";


Begin["`Private`"]
(* Implementation of the package *)

 
(*SetAttributes[GetDiffusionInBin,HoldFirst];*)

Options[GetDiffusionInBin] = {"Verbose":>$VerbosePrint,  (*Log output*)
						   "VerboseLevel":>$VerboseLevel,(*The amount of details to log. Higher number means more details*)
                           "StepDeltas"->1.,     (*How many points between what is considered to be a step- 1 means xonsecutive points in the raw list*)
                           "PadSteps"->True,     (*Take points that are outside of the bin on order to improve the statistics *)
                           "Parallel"->False,    (*Use parallel functions*)
                           "ReturnRules"->True,  (*Return the results as a list of rules*)
                           "ReturnStepsHistogram"->False (*Returns the density histogram of steps. Slows down the calcualtion considerably. Useful for debuging and visualization.*)
           };

GetDiffusionInBin[rwData_,dt_,{min_,max_},{cellMin_,cellMax_},opts:OptionsPattern[]] :=
Block[{$VerbosePrint=OptionValue["Verbose"], $VerboseLevel=OptionValue["VerboseLevel"],$VerboseIndentLevel=$VerboseIndentLevel+1}, 
    Module[ {data,t,diffs,bincenter,binwidth,sd},
        Block[ {map = If[ OptionValue["Parallel"],
                          ParallelMap,
                          Map
                      ],
               table = If[ OptionValue["Parallel"],
                           ParallelTable,
                           Table
                       ]},
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

            (*if OptionValue["StepDeltas"] only one number and not a list convert it to list {x}*)
            sd = If[ NumericQ[OptionValue["StepDeltas"]],
                     {OptionValue["StepDeltas"]},
                     OptionValue["StepDeltas"]
                 ];
            diffs = Map[With[{stepdelta = #},
                            GetDiffsFromBinnedData[data,rwData,dt,{min,max},{cellMin,cellMax}
                            ,"NextStepNum"->stepdelta
                            ,"PadSteps"->OptionValue["PadSteps"],"ReturnStepsHistogram"->OptionValue["ReturnStepsHistogram"]]
                        ]&,sd];
            diffs
        ]
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

ClearAll[GetDiffsFromBinnedData];
(*SetAttributes[GetDiffsFromBinnedData,{HoldAll}];*)
Options[GetDiffsFromBinnedData] = {"Verbose":>$VerbosePrint,  (*Log output*)
						        "VerboseLevel":>$VerboseLevel,(*The amount of details to log. Higher number means more details*)
                                "NextStepNum"->1., (*kdaj sta dva koraka zaporedna*)
                                "PadSteps"->True, (*if NextStepNum steps should be added to each continous traj*)
                                "ReturnStepsHistogram"->True};
GetDiffsFromBinnedData[binnedInd_,rwData_,dt_,{min_,max_},{cellMin_,cellMax_},opts:OptionsPattern[]] :=
    Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1},
        Module[ {steps, bincenter = (min+max)/2,binwidth = (max-min)},
            Puts["***GetDiffsFromBinnedData***"];
            PutsOptions[GetDiffsFromBinnedData,{opts},LogLevel->2];
            
            steps = Developer`ToPackedArray@Flatten[
                    Table[GetStepsFromBinnedData[binnedInd[[i]],rwData[[i]],dt,{min,max},{cellMin,cellMax}, 
                            "NextStepNum"->OptionValue["NextStepNum"], "Verbose"->OptionValue["Verbose"],"PadSteps"->OptionValue["PadSteps"]]
                          ,{i,Length[rwData]}]
                  ,1];

            PutsE["Steps:\n",steps,LogLevel->5];

            GetDiffsFromSteps[steps,dt, OptionValue["NextStepNum"]]~Join~{"StepDelta"->OptionValue["NextStepNum"], "x"->bincenter[[1]], "y"->bincenter[[2]],
                      "xWidth"->binwidth[[1]], "yWidth"->binwidth[[2]], "StepsInBin"->Length[steps],
                      "StepsHistogram"->If[ OptionValue["ReturnStepsHistogram"],
                                            QucikDensityHistogram[steps,30]
                                        ]}
        ]
    ];


ClearAll[GetStepsFromBinnedData];
Options[GetStepsFromBinnedData] = {"Verbose":>$VerbosePrint,  (*Log output*)
						        "VerboseLevel":>$VerboseLevel,(*The amount of details to log. Higher number means more details*)
                                "NextStepNum"->1., (*kdaj sta dva koraka zaporedna*)
                                "PadSteps"->True (*if NextStepNum steps should be added to each continous traj*)};
GetStepsFromBinnedData::few =                 "Few steps (`1`) when constructing histogram in bin {`2`,`3`}!";
GetStepsFromBinnedData::tofewAbort =          "To few steps (`1`) To construct histogram in bin {`2`,`3`}! Returning Missing value";
GetStepsFromBinnedData::zerodata =            "Zero data (empty list) in bin {`1`,`2`}! Returning empty list";
GetStepsFromBinnedData::nogoodR2 =            "R2 of fit (`1`) is smaller than the treshold `2` in bin {`3`,`4`}";
GetStepsFromBinnedData[binnedInd_,rwData_,dt_,{min_,max_},{cellMin_,cellMax_},opts:OptionsPattern[]] :=
Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1},
    Module[ {data,indpaths,ind,sredinaKorakov,cellWidth,fit,bincenter,rDx,rDy,r\[Alpha],rint,R2,t,pl,\[Mu]x,\[Mu]y,ds},
            Puts["********GetStepsFromBinnedData********"];
            PutsOptions[GetStepsFromBinnedData,{opts},LogLevel->2];
            Puts[Row@{"min: ", min," max: ", max},LogLevel->2];
            Puts[Row@{"cellMin: ", cellMin," cellMax: ", cellMax},LogLevel->2];
            PutsE["rwData:\n",rwData,LogLevel->5];
            PutsE["binnedInd:\n",binnedInd,LogLevel->5];
            Puts["Length rwData: ",Length[rwData],LogLevel->3];
            If[ Length@binnedInd>0, (*then*)
                Puts["binnedInd min: ",Round@binnedInd[[1]]," max: ", Round@binnedInd[[-1]],LogLevel->2];
            ];

            ds = OptionValue["NextStepNum"];
            If[ Length[binnedInd]==0,(*then*)
                Message[GetStepsFromBinnedData::zerodata,min,max];
                Return[{}];
            ];
            cellWidth = (cellMax-cellMin);
            bincenter = (min+max)/2;


            (*Split indices into continous segments*)
            indpaths = Split[binnedInd,#2-#1==1.&];


            PutsE["Indpaths lengths:\n",Length/@indpaths,LogLevel->3];
            Puts["Mean: ",N@Mean[Length/@indpaths],LogLevel->2];
            Puts[Histogram[Length/@indpaths,{5},PlotRange->{{0,100},Automatic}],LogLevel->5];
            (*Add ds steps to the beggining and end, so that we get at least 2 steps in each bin for each point in bin*)
            If[ OptionValue["PadSteps"], (*then*)
                indpaths = Map[AppendLeftRight[#,ds,Length[rwData]]&, indpaths]
            ];
            data = rwData[[Round@#]]&/@indpaths;
            PutsE["All data:\n",data,LogLevel->5];
            Puts["data is packed: ", And@@(Developer`PackedArrayQ[#]&/@indpaths),LogLevel->5];
            Puts[
             Module[ {dp},
                 dp = StrideData[Flatten[data,1],5000];
                 (*dp=Tooltip[#[[{2,3}]],#[[1]]]&/@dp;*)
                 dp = dp[[All,{2,3}]];
                 ListPlot[dp,Frame->True,PlotRange->Transpose[{cellMin,cellMax}],
                        Epilog->{Opacity[0],EdgeForm[{Red,Thick}],Rectangle[min,max]},
                        ImageSize->Medium]
             ],LogLevel->3];



           (*dobi razlike, se pravi velikost koraka*)
            data = GetDifferences[#,ds]&/@data;
            (*Vržemo ven prazne liste*)
            data = DeleteCases[data,{{}}];
            PutsE["Razlike:\n",data];
            data = Flatten[data,1];
            PutsE["Razlike:\n",data];
            (*vzamemo samo zaporedne korake to bi morali biti vsi
            data=Developer`ToPackedArray[Select[data,#[[1]]==ds&]];*)
            data = Developer`ToPackedArray[data];
            PutsE["Zaporedni koraki:\n",data];
            Puts["data is packed: ", Developer`PackedArrayQ[data]];
            Puts["ŠTEVILO KORAKOV: ",Length[data]];
            If[ Length[data]==0,(*then*)
                Message[GetStepsFromBinnedData::zerodata,min,max];
                Return[{}];
            ];
            (*If[Length[data]<500,Message[GetDiffFromBinnedData::tofewAbort,Length[data],min,max];Return[Missing[StringForm["To few ``",Length[data]]]]];
            If[Length[data]<1000,Message[GetDiffFromBinnedData::few,Length[data],min,max]];*)
            data = data[[All,{2,3}]];

            (*Izberemo korak, ki je najbližje 0. MakeInPeriodicCell is listable*)
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
      If[ Length[l]-ds>0,(*then*)
          lr = Drop[l,ds]-Drop[l,-ds]
      ];
      lr
  ],
 CompilationTarget->$Analyze2DCompilationTarget, 
   CompilationOptions->{"ExpressionOptimization"->True,"InlineExternalDefinitions"->True},
   "RuntimeOptions"->{"Speed","CompareWithTolerance"->True}];
];


ClearAll[GetDiffsFromSteps];
GetDiffsFromSteps[data_,dt_,ds_] :=  Block[{$VerboseIndentLevel = $VerboseIndentLevel+1},
    Module[ {ux,uy,rDx,rDy,rDa},
        Puts["***GetDiffsFromSteps***"];
        If[ Length[data]===0, (*then*)
            {ux,uy,rDx,rDy,rDa} = ConstantArray[Missing[],5];,(*else*)
            {ux,uy,rDx,rDy,rDa} = GetTensorFromMoments[data];
             (*We fitted the principal sigmas... transform into diffusion and devide by StepsDelta*)
            {rDx,rDy} = ({rDx,rDy}^2)/(2 dt)/ds;
        ];
        {"Dx"->rDx,"Dy"->rDy,"D\[Alpha]"->rDa,"\[Mu]x"->ux,"\[Mu]y"->uy}
    ]
];

Clear[GetTensorFromMoments];
GetTensorFromMoments[data_] :=
    Module[ {ux,uy,a,b,c,eval,evec,sx,sy,alpha},
        ux = N[Mean[data[[All,1]]]];
        uy = N[Mean[data[[All,2]]]];
        a = 1/Length[data]*(data[[All,1]]-ux).(data[[All,1]]-ux);
        b = 1/Length[data]*(data[[All,2]]-uy).(data[[All,2]]-uy);
        c = 1/Length[data]*(data[[All,1]]-ux).(data[[All,2]]-uy);
        {eval,evec} = Eigensystem[{{a,c},{c,b}}];
        {sx,sy} = Sqrt[eval];
        alpha = ArcCos[evec[[1,1]]]/Degree*Sign[ArcSin[evec[[1,2]]]];
        alpha = Mod[alpha,180];
        Return[{ux,uy,sx,sy,alpha}]
    ];





ClearAll[MinAbs];
MinAbs[L_] :=
    First@Sort[L,Abs[#1]<Abs[#2]&];

ClearAll[MakeInPeriodicCell,MakeInPeriodicCellUncompiled];
MakeInPeriodicCellUncompiled[]:=Block[{x,cellwidth},  (*This block is just for syntax coloring in WB*)
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

(*Adds the previous and last index into a sorted list. *)
ClearAll[AppendLeftRight,AppendLeftRightUncompiled];
AppendLeftRightUncompiled[] := Block[{l,ds,max}, (*This block is just for syntax coloring in WB*)
AppendLeftRight = Compile[{{l,_Real, 1},{ds,_Real,0},{max,_Real,0}},
  Block[ {lr}, 
      lr = l;
      If[ (First[lr]-ds)>0, (*then*)
          lr = Range[First[lr]-ds,First[lr]-1]~Join~lr
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


End[]

EndPackage[]

 