(* Mathematica Package *)

(* Created by the Wolfram Workbench 14.1.2013 *)

BeginPackage["DiffusiveDynamics`Analyze2D`",{"DiffusiveDynamics`Utils`","DiffusiveDynamics`Visualize2D`"}]
(* Exported symbols added here with SymbolName::usage *) 

$Analyze2DCompilationTarget="C";

ClearAll[GetDiffusionInBin];
GetDiffusionInBin::usage="GetDiffusionInBin
Diffusion info contains:
x,y,   -- the positiopn of the bin 
ux,uy  -- the mean of the distribution
Dx,Dy  -- the diffusion in the principal x and y directions.
Da     -- the rotation of Dx to the x axis  
Stride -- how many steps are steps are joined into one step.
xWidth, 
yWidth -- the widths of the bin
StepsInBin     -- how many steps in are in this bin
StepsHistogram -- draw the histogram of steps
PValue,IsNormal-- The PValue and the outcome of the normality test.
" 


ClearAll[GetDiffusionInBins]
GetDiffusionInBins::usage="TODO"

ClearAll[compiledSelectBin];
compiledSelectBin::usage="compiledSelectBin[points, min, max] 
Takes a list of 2D points and the boundaries of the bin (min, max). 
Returns all the points that are in the bin (between min and max)";

ClearAll[compiledGetContigIntervals]
compiledGetContigIntervals::usage="Returns the intervals where the difference between the sorted integers is not 1

Params:
   ind -- a floating point list of integers eg {1., 2., 3. ,6. 7. 8.}

Result:
   a list of intervals as a packed (integer) array. For example:
   {{1, 3}, {6,8}}";

ClearAll[CompileFunctions,CompileFunctionsIfNecessary];
CompileFunctions::usage="CompileFunctions[] recompiles all CompiledFunctions in package. Use $Analyze2DCompilationTarge to set the compilation target to \"C\" or \"VWM\"";
CompileFunctionsIfNecessary::usage="CompileFunctionsIfNecessary[] only compiles functions if they have not been compiled yet";

ClearAll[GetDiffusionInBinsBySelect];
GetDiffusionInBinsBySelect::usage="TODO";

ClearAll[GetDiffusionInfoFromParameters, GetDiffusionInfoFromParametersWithStride];
GetDiffusionInfoFromParameters::usage="GetDiffusionInfoFromParameters[binsOrBinspec, DiffX,DiffY,DiffA] 
Returns a list of rules from the DiffX, DiffY, DAlpha functions."

GetDiffusionInfoFromParametersWithStride::usage="GetDiffusionInfoFromParametersWithStride[binsOrBinspec, strides, DiffX,DiffY,DiffA] 
Returns a list of rules from the DiffX, DiffY, DAlpha functions for each stride."

ClearAll[DiffusionNormalForm];
DiffusionNormalForm::usage="DiffusionNormalForm[diffInfo] Makes the difusion tensor in a normal form: Dx>Dy and rotate Da 90 Deg. Da is always between 0 and 180"

Begin["`Private`"]
(* Implementation of the package *)
$SignificanceLevel = 0.05;
 
(*SetAttributes[GetDiffusionInBin,HoldFirst];*)



Options[GetDiffusionInBin] = {"Verbose":>$VerbosePrint,  (*Log output*)
						   "VerboseLevel":>$VerboseLevel,(*The amount of details to log. Higher number means more details*)
                           "Strides"->1.,     (*How many points between what is considered to be a step- 1 means consecutive points in the raw list*)
                           "PadSteps"->True,     (*Take points that are outside of the bin on order to improve the statistics *)
                           "ReturnRules"->True,  (*Return the results as a list of rules*)
                           "ReturnStepsHistogram"->False, (*Returns the density histogram of steps. Slows down the calcualtion considerably. Useful for debuging and visualization.*)
                           "WarnIfEmpty"->False, (*Prints a warning if too litle points in bin*)
                           "NormalitySignificanceLevel"-> .05 (*Significance level for the normality test*)
           };

GetDiffusionInBin[rwData_,dt_,{min_,max_},{cellMin_,cellMax_},opts:OptionsPattern[]] :=
Block[{$VerbosePrint=OptionValue["Verbose"], $VerboseLevel=OptionValue["VerboseLevel"],$VerboseIndentLevel=$VerboseIndentLevel+1,
       $SignificanceLevel=OptionValue@"NormalitySignificanceLevel"}, 
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

            Puts["select data took: ", First@t,LogLevel->2];

            t = AbsoluteTiming[   
                   data = Map[(Interval@@compiledGetContigIntervals[#])&,data];
             ];
            Puts["Finding contigous inds took: ", First@t, LogLevel->2];
            PutsE["after select and contig:\n",data, LogLevel->5];
            
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

(*Returns the intervals where the difference between the sorted integers is not 1

Params:
   ind -- a floating point list of integers eg {1., 2., 3. ,6. 7. 8.}

Result:
   a list of intervals as a packed (integer) array. For example:
   {{1, 3}, {6,8}}
*)

ClearAll[compiledGetContigIntervals, compiledGetContigIntervalsUncompiled];
compiledGetContigIntervalsUncompiled[]:=Block[{ind},   (*This block is just for syntax coloring in WB*) 
	compiledGetContigIntervals = Compile[{{ind,_Real, 1}},
	    Block[{i, openInterval = 0, result = Internal`Bag[Most@{0}]},
	        openInterval = Round@ind[[1]];(*the first oppening interval*)
	        
	        (*loop through all the indices and check for diffrences <> 1. If that is the case stuff the interval *)
	        (*Floating points implicitly compared with tolerances*)
	        Do[With[{curr=Round@ind[[i]], prev=Round@ind[[i-1]]},
	            If[ (curr-prev)!=1,
	             Internal`StuffBag[result, openInterval];
	             Internal`StuffBag[result, prev ];
	             openInterval=curr;      
	             ];
	        ], {i,2,Length@ind}];

           Internal`StuffBag[result, openInterval];    
           Internal`StuffBag[result, Round@ind[[-1]]];
	       (*return the intervals*) 
	       Partition[Internal`BagPart[result, All], 2]
	    ]  
	];
];
ClearAll[GetDiffsFromBinnedPoints];
Attributes[GetDiffsFromBinnedPoints]={HoldAll};
(*SetAttributes[GetDiffsFromBinnedPoints,{HoldAll}];*)
Options[GetDiffsFromBinnedPoints] = {"Verbose":>$VerbosePrint,  (*Log output*)
						        "VerboseLevel":>$VerboseLevel,(*The amount of details to log. Higher number means more details*)
                                "Stride"->1., (*kdaj sta dva koraka zaporedna*)
                                "PadSteps"->True, (*if NextStepNum steps should be added to each continous traj*)
                                "ReturnStepsHistogram"->True};
GetDiffsFromBinnedPoints[binnedIndInterval_,rwData_,dt_,{min_,max_},{cellMin_,cellMax_},opts:OptionsPattern[]] :=
    Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1},
        Module[ {steps, bincenter = (min+max)/2,binwidth = (max-min)},
            Puts["***GetDiffsFromBinnedPoints***"];
            PutsOptions[GetDiffsFromBinnedPoints,{opts},LogLevel->2];
            
            steps = Developer`ToPackedArray@Flatten[
                    Table[GetStepsFromBinnedPoints[binnedIndInterval[[i]],rwData[[i]],dt,{min,max},{cellMin,cellMax}, 
                            "Stride"->OptionValue["Stride"], "Verbose"->OptionValue["Verbose"],"PadSteps"->OptionValue["PadSteps"]]
                          ,{i,Length[rwData]}]
                  ,1];

            PutsE["Steps:\n",steps,LogLevel->5];

            GetDiffsFromSteps[steps,dt, OptionValue["Stride"]]~Join~{"Stride"->OptionValue["Stride"], "x"->bincenter[[1]], "y"->bincenter[[2]],
                      "xWidth"->binwidth[[1]], "yWidth"->binwidth[[2]], "StepsInBin"->Length[steps], "dt"->dt,
                      "StepsHistogram"->If[ OptionValue["ReturnStepsHistogram"],
                                            N@HistogramList[steps,"Sturges","PDF"]
                                        ]}
        ]
    ];


ClearAll[GetStepsFromBinnedPoints];
Attributes[GetStepsFromBinnedPoints]={HoldAll};

Options[GetStepsFromBinnedPoints] = {"Verbose":>$VerbosePrint,  (*Log output*)
						        "VerboseLevel":>$VerboseLevel,  (*The amount of details to log. Higher number means more details*)
                                "Stride"->1.,                   (*Number of points between a step*)
                                "PadSteps"->True                (*if Stride steps should be added to each continous segment of the trayectory in bbin*)};
(*GetStepsFromBinnedPoints::few =                 "Few steps (`1`) when constructing histogram in bin {`2`,`3`}!";
GetStepsFromBinnedPoints::tofewAbort =          "To few steps (`1`) To construct histogram in bin {`2`,`3`}! Returning Missing value";
*)
GetStepsFromBinnedPoints::zerodata =            "Zero data (empty list) in bin {`1`,`2`}! Returning empty list";
GetStepsFromBinnedPoints[binnedIndInterval_,rwData_,dt_,{min_,max_},{cellMin_,cellMax_},opts:OptionsPattern[]] :=
Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1},
    Module[ {data,indpaths,cellWidth,bincenter,ds,indLengths},
            Puts["********GetStepsFromBinnedPoints********"];
            PutsOptions[GetStepsFromBinnedPoints,{opts},LogLevel->2];
            Puts[Row@{"min: ", min," max: ", max},LogLevel->2];
            Puts[Row@{"cellMin: ", cellMin," cellMax: ", cellMax},LogLevel->2];
            PutsE["rwData:\n",rwData,LogLevel->5];
            PutsE["binnedInd:\n",binnedIndInterval,LogLevel->5];
            Puts["Length rwData: ",Length[rwData],LogLevel->3];
            If[ Length@binnedIndInterval>0, (*then*)
                Puts["binnedInd min: ",Min@binnedIndInterval," max: ", Max@binnedIndInterval,LogLevel->2];
            ];

            ds = OptionValue["Stride"];
            If[ Length[binnedIndInterval]==0,(*then*)
                Message[GetStepsFromBinnedPoints::zerodata,min,max];
                Return[{}];
            ];
            cellWidth = (cellMax-cellMin);
            bincenter = (min+max)/2;


            indpaths = binnedIndInterval;
            PutsE["Indpaths: ", indpaths,LogLevel->5];
            PutsE["Indpaths lengths:\n",indLengths=(Last@#-First@#+1)&/@(List@@indpaths),LogLevel->2];
            PutsF["Indpaths count `` mean ``:\n",Length@indLengths, N@Mean@indLengths, LogLevel->2];
            Puts[Histogram[indLengths,{5},PlotRange->{{0,All},{0,All}}],LogLevel->5];
            (*Add ds steps to the begining and end, so that we get at least 2 steps for each point in bin at largest ds*)
            (*Do this only if length of path is less or equal to  ds *)
            If[ OptionValue["PadSteps"], (*then*)
                (*Expand the intervals. Unions are automagically preformed*)
                indpaths = indpaths + Interval[{-ds, ds}];
                (*Stay within the limits*)
                indpaths = IntervalIntersection[indpaths, Interval[{1, Length@rwData}]];
                
                PutsF["After padding (with `1`):",ds,LogLevel->2];
                PutsE["Indpaths: ", indpaths,LogLevel->5];
                PutsE["Indpaths lengths:\n",indLengths=(Last@#-First@#+1)&/@(List@@indpaths),LogLevel->2];
                PutsF["Indpaths count `` mean `` (after padding ``):\n",Length@indLengths, N@Mean@indLengths,ds, LogLevel->2];
                Puts[Histogram[indLengths,{5},PlotRange->{{0,All},{0,All}}],LogLevel->5];
                (*Puts["Mean (after padding): ",N@Mean[(Differences[#]+1)&/@indpaths],LogLevel->0];
                Puts[Histogram[(Differences[#]+1)&/@indpaths,{5},PlotRange->{{0,All},{0,All}}],LogLevel->3];*)
                
            ];
            
            
            (*get the contigs x,y*)
            data = rwData[[First@#;;Last@#,{2,3}]]&/@(List@@indpaths);
           
            PutsE["All data:\n",data,LogLevel->5];
            
            Puts["data is packed: ", And@@(Developer`PackedArrayQ[#]&/@data),LogLevel->2];
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
            data = Differences[#,1,ds]&/@data;
            (*Delete empty lists (*TODO: Perhaps this is still neede?*)
            data = DeleteCases[data,{{}}];*)
            
            If[ Length[data]==0,(*then*)
                Message[GetStepsFromBinnedPoints::zerodata,min,max];
                Return[{}];
            ];
            
            data = Developer`ToPackedArray[Flatten[data,1]];
            (*Pick the shortest of the steps. This is needed if the step was over the periodic boundary limit. 
            Used for each ccordinate seperatly.
            MakeInPeriodicCell is listable.*)
            
            data[[All,1]] = MakeInPeriodicCell[data[[All,1]], cellWidth[[1]]];
            data[[All,2]] = MakeInPeriodicCell[data[[All,2]], cellWidth[[2]]];
            
            
            
            

            
            PutsE["Steps: \n",data,LogLevel->5];
            
            Puts["Steps are packed: ", Developer`PackedArrayQ[data], LogLevel->2];
            PutsF["STEPS: num ``; average length `` ",Length[data],Mean[Norm/@data],LogLevel->2];
               



            
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
      If[ Length[l]>ds,(*if list is long enough then*)
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
Attributes[GetDiffsFromSteps]={HoldFirst};
GetDiffsFromSteps[data_,dt_,ds_] :=  Block[{$VerboseIndentLevel = $VerboseIndentLevel+1},
    Module[ {ux,uy,sx,sy,rDx,rDy,rDa,pVal=Null,isNormal=Null},
        Puts["***GetDiffsFromSteps***"];
        If[ Length[data]==0, (*If no steps then*)
            {ux,uy,rDx,rDy,rDa} = ConstantArray[Missing[],5];,(*else*)
            (*TODO: Perhaps add option to choose if we want the normality test and which test is wanted *)
            (*{ux,uy,rDx,rDy,rDa} = GetTensorFromMoments[data];*)
            {ux,uy,sx,sy,rDa,pVal,isNormal}=GetTensorFromMomentsWithNormalityTest[data];
            
            (*We fitted the principal sigmas... transform into diffusion and devide by StepsDelta*)
            {rDx,rDy} = ({sx,sy}^2)/(2*dt*ds);
        ];
        {"Dx"->rDx,"Dy"->rDy,"Da"->rDa,"ux"->ux,"uy"->uy,"sx"->sx,"sy"->sy,
         "PValue"->pVal,"IsNormal"->isNormal,"xMinWidth"->Max@Abs@{6*sx*Cos[rDa*Degree],6*sy*Cos[(rDa+90)*Degree]},
                                             "yMinWidth"->Max@Abs@{6*sy*Sin[rDa*Degree],6*sx*Sin[(rDa+90)*Degree]} (*99.7 steps are inside the bin*)}
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

		hypothesis=AndersonDarlingTest[data, dist, "HypothesisTestData",SignificanceLevel->$SignificanceLevel]; 
		(*Is normal distribution?*)
		pVal=hypothesis["PValue"];
		isNormal=hypothesis["ShortTestConclusion"] == "Do not reject";
		(*Retrive the parameters*)
		params=hypothesis["FittedDistributionParameters"];
		{ux,uy,a,b,c}={ux,uy,a,b,c}/.params;
		{a,b,c}={a,b,c}/.params;
		
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
     compiledGetContigIntervalsUncompiled[];
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
GetDiffusionInfoForBinFromParameters[{{x_,y_},{dx_,dy_}}, timestep_,stride_,DiffX_,DiffY_,DiffA_,ReturnHistograms_:False]:=
With[{Dx=DiffX[x,y],Dy=DiffY[x,y],Da=DiffA[x,y]},
	 {"Dx"->Dx,"Dy"->Dy,"Da"->Da, "sx"->Sqrt[2*Dx*stride*timestep], "sy"->Sqrt[2*Dy*stride*timestep],
	  "x"->x,"y"->y,"xWidth"->dx,"yWidth"->dy, "ux"->0,"uy"->0, "dt"->timestep,
	  "StepsHistogram"->If[ReturnHistograms, GetStepsHistogramDiffusion[{DiffX[x,y], DiffY[x,y], DiffA[x,y]}, stride]],  
	  "StepsInBin"->Null,"Stride"->stride, "PValue"->1, "IsNormal"->True,"xMinWidth"->dx,"yMinWidth"->dy}
];
(*TODO: Perhaps I could fill a Gaussian StepsHistogram. StepDelta could be passed in as an option.  *)

Options@GetDiffusionInfoFromParameters = {
               "Verbose":>$VerbosePrint,  (*Log output*)
			   "VerboseLevel":>$VerboseLevel,(*The amount of details to log. Higher number means more details*)
               "ReturnStepsHistogram"->False, (*Returns The PDF plot.*)
               "Parallel" -> True
}

GetDiffusionInfoFromParameters[binsOrBinspec_, timestep_?NumericQ, stride_?NumericQ, DiffX_,DiffY_,DiffA_,opts:OptionsPattern[]]:=
Block[{$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"], $VerboseIndentLevel = $VerboseIndentLevel+1}, 
    Module[{bins},
        Puts["********GetDiffusionInfoFromParameters********"];
        (*PutsOptions[GetDiffusionInfoFromParameters,{opts},LogLevel->2];  *)      
        bins=GetBinsFromBinsOrSpec@binsOrBinspec;
        GetDiffusionInfoForBinFromParameters[#, timestep, stride,DiffX,DiffY,DiffA,OptionValue@"ReturnStepsHistogram"]&/@bins     
    ]
]     


GetDiffusionInfoFromParameters[binsOrBinspec_,timestep_?NumericQ, strides_List, DiffX_,DiffY_,DiffA_,opts:OptionsPattern[]]:=
Block[{$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],  $VerboseIndentLevel=$VerboseIndentLevel+1}, 
    Block[{bins, bin, stride (*just for syntax higlighting in WB*), tabelF = If[OptionValue@"Parallel", ParallelTable, Table]},
        Puts["********GetDiffusionInfoFromParameters********"];
        (*PutsOptions[GetDiffusionInfoFromParameters,{opts},LogLevel->2];  *)
        bins=GetBinsFromBinsOrSpec@binsOrBinspec;
        With[{iDiffX=DiffX,iDiffY=DiffY,iDiffA=DiffA,returnHistograms=OptionValue@"ReturnStepsHistogram"},
        tabelF[
            GetDiffusionInfoForBinFromParameters[bin, timestep, stride,iDiffX,iDiffY,iDiffA,returnHistograms],
        {bin,bins},{stride,strides}]
        ]      
    ]
]  



DiffusionNormalForm[Dx_?NumericOrSymbolQ, Dy_?NumericOrSymbolQ, Da_?NumericOrSymbolQ]:=
Block[{dx,dy,da},
    If[Dy>Dx,(*then*)
        (*If we switch x and y then we get the same if we turn the tensor 90 deg *)
        dx=Dy;dy=Dx;da=Da+90;
    ,(*else*)
        dx=Dx;dy=Dy;da=Da;
    ];
    da=Mod[da,180];
    {dx,dy,da}
]

DiffusionNormalForm[{Dx_?NumericOrSymbolQ, Dy_?NumericOrSymbolQ, Da_?NumericOrSymbolQ}]:=DiffusionNormalForm[Dx, Dy, Da];

(*Accept a list of rules*)
DiffusionNormalForm[diffInfo:{__Rule}]:=
Block[{dx,dy,da},
    (*Get sorted values*)
    {dx,dy,da}=DiffusionNormalForm@GetValue[{"Dx","Dy","Da"},diffInfo];
    

	(*Modify the original lis*)
	diffInfo /.   {Rule["Dx", _] -> Rule["Dx", dx],
			       Rule["Dy", _] -> Rule["Dy", dy],
			       Rule["Da", _] -> Rule["Da", da]}
]

DiffusionNormalForm[diffInfos:{{__Rule}..}]:=
  DiffusionNormalForm/@diffInfos;

DiffusionNormalForm[diffInfosWithStrides:{{{__Rule}..}..}]:=
  DiffusionNormalForm/@diffInfosWithStrides;

DiffusionNormalForm::wrongargs="Wrong arguments for DiffusionNormalForm `1` ";  
DiffusionNormalForm[args___]:=(Message[DiffusionNormalForm::wrongargs,Shallow@args];$Failed);  

End[]

EndPackage[]

 