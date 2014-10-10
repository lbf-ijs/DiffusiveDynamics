(* Mathematica Package *)

(* Created by the Wolfram Workbench 14.1.2013 *)

BeginPackage["DiffusiveDynamics`Analyze2D`",{"DiffusiveDynamics`Utils`","DiffusiveDynamics`Visualize2D`","CCompilerDriver`"}]
(* Exported symbols added here with SymbolName::usage *) 

(*Fallback gracefully if no C compiler is present*)
If[Length@CCompilers[]>0,
    $Analyze2DCompilationTarget="C"
,(*else*)    
    $Analyze2DCompilationTarget="WVM"];

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
ClearAll[GetDiffusionInBinByMiddlePoint];
GetDiffusionInBinByMiddlePoint::usage="See implementation section"

ClearAll[GetDiffusionInBinsByMiddlePoint];
GetDiffusionInBinsByMiddlePoint::usage="See implementation section"

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

TransposeDiffusion::usage="DiffusionNormalForm[diffInfo] changes the x and y variables. The angle is also adjusted, as well as all the bin statistics.";

ClearAll[LoadDiffusions, SaveDiffusions];
LoadDiffusions::usage="LoadDiffusions[name, opts]
Loads the diffusions from the directory name. Returns a list of:

  {\"Metadata\" -> metadata,
   \"Diffusions\" -> diffs,
   \"RawSteps\" -> rawSteps}
";


SaveDiffusions::usage="SaveDiffusions[name, diffs, opts]. \n Saves the diffusions diffs to a directory specified by name. Metadata can be given as na option.";

ClearAll[EstimateDiffusionError];
EstimateDiffusionError::usage="";

EstimateDiffusionErrorAndSave::usage="";
GetDiffusionsRMSD::usage="";

MoveDiffusionBins::usage="MoveDiffusionBins[diff_, x_, y_]
Moves the bin centers by x and y." 
JoinDiffusionBins::usage="JoinDiffusionBins[{diff1_,diff2_, diff_n}] 
diff1 and diff2 are lists of list of difusion rules [[bins, strides, rules]]"

Begin["`Private`"]
(* Implementation of the package *)
$SignificanceLevel = 0.05;
 
(*SetAttributes[GetDiffusionInBin,HoldFirst];*)



Options[GetDiffusionInBin] := {"Verbose":>$VerbosePrint,  (*Log output*)
						   "VerboseLevel":>$VerboseLevel,(*The amount of details to log. Higher number means more details*)
                           "Strides"->1.,     (*How many points between what is considered to be a step- 1 means consecutive points in the raw list*)
                           "PadSteps"->True,     (*Take points that are outside of the bin on order to improve the statistics *)
                           "ReturnRules"->True,  (*Return the results as a list of rules*)
                           "ReturnStepsHistogram"->False, (*Returns the density histogram of steps. Slows down the calcualtion considerably. Useful for debuging and visualization.*)
                           "WarnIfEmpty"->False, (*Prints a warning if too litle points in bin*)
                           "NormalitySignificanceLevel"-> .05 (*Significance level for the normality test*)
           }~Join~Options[GetDiffsFromBinnedPoints];
 
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
            
            (*if OptionValue["Strides"] only one number and not a list convert it to list {x}*)
            sd = If[ NumericQ[OptionValue["Strides"]],
                     {OptionValue["Strides"]},
                     OptionValue["Strides"]
                 ];            
       
            If[OptionValue@"WarnIfEmpty",On[GetStepsFromBinnedPoints::zerodata],
                                         Off[GetStepsFromBinnedPoints::zerodata]];
                        
            (*select the points that have the origin in the bin*)
            (*Take just the step's indexes*)
            t = AbsoluteTiming[   
                   data = Map[Part[compiledSelectBin[#,min,max],All,1]&,rwData];
             ];
      
            Puts["select data took: ", First@t,LogLevel->2];

            
            t = AbsoluteTiming[   
                   data = Map[(If[Length[#] != 0, 
                                   Interval@@compiledGetContigIntervals[#], (*UNPACKING here*)
                                   Interval[]]
                               )&,data];
             ];
            Puts["Finding contigous inds took: ", First@t, LogLevel->2];
            PutsE["after select and contig:\n",data, LogLevel->5];
            

            

           
            diffs = Map[With[{stepdelta = #},
                            GetDiffsFromBinnedPoints[data,rwData,dt,{min,max},{cellMin,cellMax}
                            ,"Stride"->stepdelta,"Verbose"->OptionValue["Verbose"]
                            ,"PadSteps"->OptionValue["PadSteps"],"PadOnlyShort"->OptionValue@"PadOnlyShort"
                            ,"StepsOverlap"->OptionValue@"StepsOverlap"
                            ]
            ]&,sd];
            diffs
        ]
]; 

Options[GetDiffusionInBinByMiddlePoint] := {"Verbose":>$VerbosePrint,  (*Log output*)
                           "VerboseLevel":>$VerboseLevel,(*The amount of details to log. Higher number means more details*)
                           "Strides"->1.,     (*How many points between what is considered to be a step- 1 means consecutive points in the raw list*)
                           "ReturnRules"->True,  (*Return the results as a list of rules*)
                           "ReturnStepsHistogram"->False, (*Returns the density histogram of steps. Slows down the calcualtion considerably. Useful for debuging and visualization.*)
                           "WarnIfEmpty"->False, (*Prints a warning if too litle points in bin*)
                           "NormalitySignificanceLevel"-> .05 (*Significance level for the normality test*)
           }~Join~Options[GetDiffsFromBinnedPoints];
GetDiffusionInBinByMiddlePoint::usage="
Gets diffusion in bin given by {min_,max_}.

rwData --- a list of list of points {t,x,y}
dt --- the step
min --- min values of the bin (lower left corner)
max --- max values of the bin
" 
GetDiffusionInBinByMiddlePoint[rwData_,dt_,{min_,max_},{cellMin_,cellMax_},opts:OptionsPattern[]] :=
Block[{$VerbosePrint=OptionValue["Verbose"], $VerboseLevel=OptionValue["VerboseLevel"],$VerboseIndentLevel=$VerboseIndentLevel+1,
       $SignificanceLevel=OptionValue@"NormalitySignificanceLevel"}, 
    Module[ {steps,t,m,mm,bincenter,binwidth,stride,cellwidth},
            Puts["***GetDiffusionInBin****"];
            (*TODO: These checks are not good enough. Because of these checks all the trajectory lenghts must be of the same size.*)
            Assert[Last@Dimensions@rwData==3,"Must be a list of triplets in the form {t,x,y}"];
            Assert[Length@Dimensions@rwData==3,"rwData must be a list of lists of triplets!"];
            Puts[Row@{"\ndt: ",dt,"\nmin: ",min,"\nmax: ",max,"\ncellMin: ",cellMin,"\ncellMax: ",cellMax},LogLevel->2];
            PutsOptions[GetDiffusionInBinByMiddlePoint, {opts}, LogLevel->2];
            PutsE["original data:\n",rwData,LogLevel->5];
            Puts["Is packed original data? ",And@@Developer`PackedArrayQ[#]&/@rwData, LogLevel->5];
            CompileFunctionsIfNecessary[];
            bincenter=(min+max)*0.5; 
            binwidth=N@(max-min);
            cellwidth=cellMax-cellMin;
            Table[
	            (*Get the vectors in bin*)
	            m = MemoryInUse[]; mm=MaxMemoryUsed[];
	            t = AbsoluteTiming[   
	                   If[OptionValue@"StepsOverlap",	                  
	                       steps = Map[compiledSelectByMiddlePointWithOverlap[#,min,max,cellwidth,stride]&,rwData];
	                   ,(*else*)
	                       steps = Map[compiledSelectByMiddlePointNoOverlap[#,min,max,cellwidth,stride]&,rwData];
	                   ];
	                   steps = Join@@steps; (*Flatten[steps,1] unpacks the array!*)
	             ];
	            (*TMP`$steps[stride]=steps;*)
	            Puts["select steps took: ", First@t, " s and ", (MemoryInUse[]-m)/1048576. ," MB (MAX: ",(MaxMemoryUsed[]-mm)/1048576., " MB)",LogLevel->2];
	            Puts["Data dimensions: ", Dimensions@steps,LogLevel->2];
	            Puts["Data packed? ", Developer`PackedArrayQ@steps,LogLevel->2];
	            
	            Puts["Histogram of sizes:", Histogram["Sturges"],LogLevel->5];
	           
	            GetDiffsFromSteps[steps, dt, stride, OptionValue@"StepsOverlap"]~Join~{"Stride"->stride, "x"->bincenter[[1]], "y"->bincenter[[2]],
                      "xWidth"->binwidth[[1]], "yWidth"->binwidth[[2]], "StepsInBin"->Length[steps], "dt"->dt,
                      "StepsHistogram"->If[ OptionValue["ReturnStepsHistogram"],
                                            N@HistogramList[steps,"Sturges","PDF"]
                                        ]}
	            
	            
            ,{stride,OptionValue@"Strides"}]
    ]
]


$DebugCompiledFunctions=False; 
stripPrint = If[Not@$DebugCompiledFunctions, 
     HoldPattern[h_[pre___, __Print, post___]] :> h[pre, post],
   (*else*)   
     HoldPattern[h_[pre___, Print[diags___], post___]] :> h[pre, Print[diags], post]];
   
ClearAll[MakeInPeriodicCell,MakeInPeriodicCellUncompiled];
MakeInPeriodicCellUncompiled[]:=Block[{x,cellwidth},  (*This block is just for syntax coloring in WB*)
(*
Steps through the whole cell are very improbable, so large steps are due to periodic conditions.
Takes a coordinate and chooses the smallest of the three options due to periodic conditions.
Hmmm, whats the rigth answer for MakeInPeriodicCell[2,3]
*)
MakeInPeriodicCell = Compile[{{x,_Real,0},{cellwidth,_Real,0}},
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


ClearAll[compiledSelectBinFunc];
compiledSelectBinFunc = Compile[{{point,_Real, 1},{min,_Real, 1},{max,_Real, 1}},
  ((point[[2]]>=min[[1]])&&(point[[2]]<=max[[1]])&&(point[[3]]>=min[[2]] )&&(point[[3]]<=max[[2]]))
,  
   CompilationOptions->{"ExpressionOptimization"->True,"InlineExternalDefinitions"->True},
   "RuntimeOptions"->"Speed"];

   
ClearAll[compiledSelectByMiddlePoint, compiledSelectByMiddlePointUncompiled];
compiledSelectByMiddlePoint::usage="Selects steps whose middle point lies in the bin given by min/max. data is given as a list of points {t,x,y}";
(*This is a bit of a mess, because all the other functions expect {t,x,y}, but here the t (point index) is redundant. 
For compatibility with other functions (both upstream and downstram) it is however kept and accepts a list of {t,x,y}*)
compiledSelectByMiddlePointUncompiled[]:=Block[{points,min,max,result,i,stride,cellWidth}, (*These are just for WB syntax highlighting*)
    compiledSelectByMiddlePointWithOverlap=Compile@@(Hold[{{points,_Real, 2},{min,_Real, 1},{max,_Real, 1}, {cellWidth,_Real, 1} ,{stride,_Integer, 0}},
        Module[{resultBag=Internal`Bag[](*Empty real bag *), middlePoint={0.,0.}, step={0.,0.}, len=0},
        Print["cellWidth: ", cellWidth];
        (*loop over points*)  
        Do[          
          Print["iteration: ", i];
          step=points[[i+stride]]-points[[i]]; Print["Step: ", step];   
          (*Take care of steps that went over periodic conditions (out of the unit cell)*)
          
          step[[2]]=MakeInPeriodicCell[step[[2]], cellWidth[[1]]];
          step[[3]]=MakeInPeriodicCell[step[[3]], cellWidth[[2]]];  
          Print["Step after periodic:", step];  
          
          middlePoint=points[[i]]+0.5*step; Print["middlePoint: ",middlePoint];      

          If[compiledSelectBinFunc[middlePoint,min,max],
             Print["Is in!"];
             Internal`StuffBag[resultBag, Internal`Bag[ step[[{2,3}]] ]];
             len=len+1;(*Internal`BagLength is not compilable. Must track manually*)
                      
            ];   
        ,{i,1,Length@points-stride}];
        
        (*Return stuffed vectors*)
        Table[Internal`BagPart[Internal`BagPart[resultBag, i], All], {i, 1, len}]
        ]
        ,   CompilationTarget->$Analyze2DCompilationTarget,
            CompilationOptions->{"ExpressionOptimization"->True,"InlineCompiledFunctions"->True,"InlineExternalDefinitions"->True},
            "RuntimeOptions"->"Speed"] //.stripPrint(*Compile*));
            
    compiledSelectByMiddlePointNoOverlap=Compile@@(Hold[{{points,_Real, 2},{min,_Real, 1},{max,_Real, 1}, {cellWidth,_Real, 1} ,{stride,_Integer, 0}},
        Module[{resultBag=Internal`Bag[](*Empty real bag *), middlePoint={0.,0.}, step={0.,0.}, len=0},
        Print["cellWidth: ", cellWidth];
        (*loop over points*)  
        Do[          
          Print["iteration: ", i];
          step=points[[i+stride]]-points[[i]]; Print["Step: ", step];   
          (*Take care of steps that went over periodic conditions (out of the unit cell)*)
          
          step[[2]]=MakeInPeriodicCell[step[[2]], cellWidth[[1]]];
          step[[3]]=MakeInPeriodicCell[step[[3]], cellWidth[[2]]];  
          Print["Step after periodic:", step];  
          
          middlePoint=points[[i]]+0.5*step; Print["middlePoint: ",middlePoint];      

          If[compiledSelectBinFunc[middlePoint,min,max],
             Print["Is in!"];
             Internal`StuffBag[resultBag, Internal`Bag[ step[[{2,3}]] ]];
             len=len+1;(*Internal`BagLength is not compilable. Must track manually*)
                      
            ];   
        ,{i,1,Length@points-stride,stride}];
        
        (*Return stuffed vectors*)
        Table[Internal`BagPart[Internal`BagPart[resultBag, i], All], {i, 1, len}]
        ]
        ,   CompilationTarget->$Analyze2DCompilationTarget,
            CompilationOptions->{"ExpressionOptimization"->True,"InlineCompiledFunctions"->True,"InlineExternalDefinitions"->True},
            "RuntimeOptions"->"Speed"] //.stripPrint(*Compile*))            
];

  
(*ClearAll[compiledSelectByMiddlePoint, compiledSelectByMiddlePointUncompiled];
compiledSelectByMiddlePoint::usage="Selects steps whose middle point lies in the bin given by min/max. data is given as a list of points {t,x,y}";
(*This is a bit of a mess, because all the other functions expect {t,x,y}, but here the t (point index) is redundant. 
For compatibility with other functions (both upstream and downstram) it is however kept and accepts a list of {t,x,y}*)
compiledSelectByMiddlePointUncompiled[]:=Block[{points,min,max,result,i,stride,cellWidth}, (*These are just for WB syntax highlighting*)
    compiledSelectByMiddlePoint=Compile@@(Hold[{{points,_Real, 2},{min,_Real, 1},{max,_Real, 1}, {cellWidth,_Real, 1} ,{stride,_Integer, 0}},
        Module[{resultBag=Internal`Bag[](*Empty real bag *), middlePoint={0.,0.}, step={0.,0.}, len=0},
        Print["cellWidth: ", cellWidth];
        (*loop over points*)  
        Do[          
          Print["iteration: ", i];
          step=points[[i+stride]]-points[[i]]; Print["Step: ", step];   
          (*Take care of steps that went over periodic conditions (out of the unit cell)*)
          
          step[[2]]=MakeInPeriodicCell[step[[2]], cellWidth[[1]]];
          step[[3]]=MakeInPeriodicCell[step[[3]], cellWidth[[2]]];  
          Print["Step after periodic:", step];  
          
          middlePoint=points[[i]]+0.5*step; Print["middlePoint: ",middlePoint];      

          If[compiledSelectBinFunc[middlePoint,min,max],
             Print["Is in!"];
             Internal`StuffBag[resultBag, Internal`Bag[ step[[{2,3}]] ]];
             len=len+1;(*Internal`BagLength is not compilable. Must track manually*)
                      
            ];   
        ,{i,1,Length@points-stride,stride}];
        
        (*Return stuffed vectors*)
        Table[Internal`BagPart[Internal`BagPart[resultBag, i], All], {i, 1, len}]
        ]
        ,   CompilationTarget->$Analyze2DCompilationTarget,
            CompilationOptions->{"ExpressionOptimization"->True,"InlineCompiledFunctions"->True,"InlineExternalDefinitions"->True},
            "RuntimeOptions"->"Speed"] //.stripPrint(*Compile*))
];*)
  
  
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
	,CompilationTarget->$Analyze2DCompilationTarget, 
   CompilationOptions->{"ExpressionOptimization"->True,"InlineExternalDefinitions"->True},
   "RuntimeOptions"->{"Speed","CompareWithTolerance"->False}];
];
ClearAll[GetDiffsFromBinnedPoints];
Attributes[GetDiffsFromBinnedPoints]={HoldAll};
(*SetAttributes[GetDiffsFromBinnedPoints,{HoldAll}];*)
Options[GetDiffsFromBinnedPoints] := {"Verbose":>$VerbosePrint,  (*Log output*)
						        "VerboseLevel":>$VerboseLevel,(*The amount of details to log. Higher number means more details*)
                                "Stride"->1., (*kdaj sta dva koraka zaporedna*)
                                "ReturnStepsHistogram"->False}~Join~Options[GetStepsFromBinnedPoints];
GetDiffsFromBinnedPoints[binnedIndInterval_,rwData_,dt_,{min_,max_},{cellMin_,cellMax_},opts:OptionsPattern[]] :=
    Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1},
        Module[ {steps, bincenter = (min+max)/2,binwidth = (max-min)},
            Puts["***GetDiffsFromBinnedPoints***"];
            PutsOptions[GetDiffsFromBinnedPoints,{opts},LogLevel->2];
            
            steps = Developer`ToPackedArray@Flatten[
                    Table[GetStepsFromBinnedPoints[binnedIndInterval[[i]],rwData[[i]],dt,{min,max},{cellMin,cellMax} 
                            ,"Stride"->OptionValue["Stride"], "Verbose"->OptionValue["Verbose"]
                            ,"PadSteps"->OptionValue["PadSteps"],"PadOnlyShort"->OptionValue@"PadOnlyShort"
                            ,"StepsOverlap"->OptionValue@"StepsOverlap"]
                          ,{i,Length[rwData]}]
                  ,1];

            PutsE["Steps:\n",steps,LogLevel->5];

            GetDiffsFromSteps[steps,dt, OptionValue["Stride"],OptionValue["StepsOverlap"]]~Join~{"Stride"->OptionValue["Stride"], "x"->bincenter[[1]], "y"->bincenter[[2]],
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
                                "PadSteps"->True,               (*if Stride steps should be added to each continous segment of the trayectory in bbin*)
                                "PadOnlyShort"->True,           (*Pad only intervals smaller or equal than ds (If PadSteps is true) *)
                                "StepsOverlap"->False          (*If true steps are calcualted as [i+stride]-[i], where i is incremented by 1. If False, i is incremented by stride*)};

GetStepsFromBinnedPoints::zerodata =            "Zero data (empty list) in bin {`1`,`2`}! Returning empty list";
GetStepsFromBinnedPoints[binnedIndInterval_,rwData_,dt_,{min_,max_},{cellMin_,cellMax_},opts:OptionsPattern[]] :=
Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1},
    Module[ {data,indpaths,cellWidth,bincenter,ds,indLengths},
            Puts["********GetStepsFromBinnedPoints********",LogLevel->1];
            PutsOptions[GetStepsFromBinnedPoints,{opts},LogLevel->2];
            Puts[Row@{"min: ", min," max: ", max},LogLevel->2];
            Puts[Row@{"cellMin: ", cellMin," cellMax: ", cellMax},LogLevel->2];
            PutsE["rwData:\n",rwData,LogLevel->5];
            PutsE["binnedInd:\n",binnedIndInterval,LogLevel->3];
            Puts["Length rwData: ",Length[rwData],LogLevel->2];
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
            PutsE["Indpaths lengths:\n",indLengths=(Last@#-First@#+1)&/@(List@@indpaths),LogLevel->3];
            PutsF["Indpaths count `` mean ``:\n",Length@indLengths, N@Mean@indLengths, LogLevel->3];
            Puts[Histogram[indLengths,{5},PlotRange->{{0,All},{0,All}}],LogLevel->5];
            (*Add ds steps to the begining and end, so that we get at least 2 steps for each point in bin at largest ds*)
            (*Do this only if length of path is less or equal to  ds *)
            If[ OptionValue["PadSteps"], (*then*)
                (*Expand the intervals. Unions are automagically preformed*)
                
                If[ OptionValue@"PadOnlyShort", 
                    indpaths=If[(Last@# - First@#) <= ds, {First@# - ds, Last@# + ds}, #] & /@indpaths;
                     Puts["PaddingOnlyShort",LogLevel->5];
                ,(*else*)
                    indpaths = indpaths + Interval[{-ds, ds}];
                    Puts["PaddingAll",LogLevel->5];
                ]; 

                (*Stay within the limits*)
                indpaths = IntervalIntersection[indpaths, Interval[{1, Length@rwData}]];
                 
                PutsF["After padding (with `1`):",ds,LogLevel->2]; 
                PutsE["Indpaths: ", indpaths,LogLevel->5];
                PutsE["Indpaths lengths:\n",indLengths=(Last@#-First@#+1)&/@(List@@indpaths),LogLevel->3];
                PutsF["Indpaths count `` mean `` (after padding ``):\n",Length@indLengths, N@Mean@indLengths,ds, LogLevel->3];
                Puts[Histogram[indLengths,{5},PlotRange->{{0,All},{0,All}}],LogLevel->5];
                (*Puts["Mean (after padding): ",N@Mean[(Differences[#]+1)&/@indpaths],LogLevel->0];
                Puts[Histogram[(Differences[#]+1)&/@indpaths,{5},PlotRange->{{0,All},{0,All}}],LogLevel->3];*)
                
            ];
            
            Puts["Before getting data",LogLevel->5];
            (*get the contigs x,y*)
            data = rwData[[First@#;;Last@#,{2,3}]]&/@(List@@indpaths);(*UNPACKING here? *)
            Puts["After getting data",LogLevel->5];
            PutsE["All data:\n",data,LogLevel->5];
            Puts["After getting data is it packed? ",Developer`PackedArrayQ@data,LogLevel->5];
            Puts["data is packed: ", And@@(Developer`PackedArrayQ[#]&/@data),LogLevel->2];
            Puts[ 
             Module[ {dp},
                 dp = StrideData[Flatten[data,1],5000];
                 (*dp=Tooltip[#[[{2,3}]],#[[1]]]&/@dp;*)
                 
                 ListPlot[dp,Frame->True,PlotStyle->Opacity[.4],PlotRange->Transpose[{cellMin,cellMax}],
                        Epilog->{Opacity[0],EdgeForm[{Red,Thick}],Rectangle[min,max]},
                        ImageSize->Medium]
             ],LogLevel->3];


            
           (*calculate the steps (diferences between consecutive points*)
            Puts["Before getting Diffrences",LogLevel->5];
           
                      
            If[OptionValue["StepsOverlap"],
                data = (If[Length[#]<=ds,{},(*If less than ds steps, then return empty list*)
                        Drop[#,ds]-Drop[#,-ds]])&/@data; (*UNPACKING here? *)
            ,(*else*)
                data = (If[Length[#]<=ds,{},(*If less than ds steps, then return empty list*)
                           With[{d = #[[1;; Length@# ;; ds]]}, Rest@d - Most@d]])&/@data;
            ];           
            (*This is not supported in mma 8.
            BUG IN Differences, just crashes, while running out of memory
            If[$VersionNumber>=9,
                data = Differences[#,1,ds]&/@data;
            ,(*else*)
                data = (Drop[#,ds]-Drop[#,-ds])&/@data;
            ];*)
            Puts["After getting Diffrences",LogLevel->5];

            (*Delete empty lists (*TODO: Perhaps this is still needed?*)
            data = DeleteCases[data,{{}}];*)
            

            
(*            data = Developer`ToPackedArray[Flatten[data,1]]; (*UNPACKING here? *)*)
            data = Developer`ToPackedArray[Join@@data];
            If[ Length[data]===0,(*then*)
                Message[GetStepsFromBinnedPoints::zerodata,min,max];
                Return[{}];
            ];

            (*Pick the shortest of the steps. This is needed if the step was over the periodic boundary limit. 
            Used for each coordinate seperatly.
            MakeInPeriodicCell is listable.*)
            Puts["Before Calling MakeInPeriodicCell",LogLevel->4];
            data[[All,1]] = MakeInPeriodicCell[data[[All,1]], cellWidth[[1]]];
            data[[All,2]] = MakeInPeriodicCell[data[[All,2]], cellWidth[[2]]];
            Puts["After Calling MakeInPeriodicCell",LogLevel->4];	            
            
            
            

            
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
GetDifferencesWithOverlap = Compile[{{l,_Real, 2},{ds,_Integer,0}},
  Block[ {lr= {{}}},      
      (*If[ds==1,(*then*)lr=Differences[l];]; Diffrences seem to have some problems...*)
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
GetDiffsFromSteps[data_,dt_,ds_, StepsOverlap_] :=  Block[{$VerboseIndentLevel = $VerboseIndentLevel+1},
    Module[ {ux,uy,sx,sy,rDx,rDy,rDa,pVal,isNormal,
             xMinWidth,yMinWidth,allMissing,result},
        allMissing={"Dx"->Missing[],"Dy"->Missing[],"Da"->Missing[],"ux"->Missing[],"uy"->Missing[]y,"sx"->Missing[],"sy"->Missing[],
                    "PValue"->Null,"IsNormal"->Null (*Must be null here, otherwise it breaks a Pick later on*),
                    "xMinWidth"->Missing[],"yMinWidth"->Missing[]};
        Puts["***GetDiffsFromSteps***"];
        If[ Length[data]>10, (*If enough steps then*)
            (*TODO: Perhaps add option to choose if we want the normality test and which test is wanted *)
            (*{ux,uy,sx,sy,rDa} = GetTensorFromMoments[data];*)
            {ux,uy,sx,sy,rDa,pVal,isNormal}=GetTensorFromMomentsWithNormalityTest[data, ds, StepsOverlap];
            If[Not[NumericQ@ux && NumericQ@uy && NumericQ@sx && NumericQ@sy && NumericQ@rDa], 
                Print["Some of the returned values are not numeric: ",{ux,uy,sx,sy,rDa}]; 
                result=allMissing;
                ];
             
            (*We fitted the principal sigmas... transform into diffusion and devide by StepsDelta*)
            {rDx,rDy} = ({sx,sy}^2)/(2*dt*ds);
            (*99.7 steps are inside the bin. 3*sigma to each side*)
            xMinWidth=Max@Abs@{6*sx*Cos[rDa*Degree],6*sy*Cos[(rDa+90)*Degree]};
            yMinWidth=Max@Abs@{6*sy*Sin[rDa*Degree],6*sx*Sin[(rDa+90)*Degree]};
            
            result= {"Dx"->rDx,"Dy"->rDy,"Da"->rDa,"ux"->ux,"uy"->uy,"sx"->sx,"sy"->sy,
                     "PValue"->pVal,"IsNormal"->isNormal,"xMinWidth"->xMinWidth,
                     "yMinWidth"->yMinWidth};
        ,(*else*)
        result=allMissing;        
        ];
        result

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
GetTensorFromMomentsWithNormalityTest[data_, ds_, stepsOverlap_] :=
    Module[ {ux,uy,a,b,c,eval,evec,sx,sy,alpha,dist,pVal, isNormal, params},
        
        dist = MultinormalDistribution[{ux, uy}, {{a, c}, {c, b}}];
        
        params=EstimatedDistribution[data, dist] /. MultinormalDistribution[{ux_, uy_}, {{a_, c_}, {c_, b_}}] :> {ux, uy, a, b, c};
        If[And[stepsOverlap,ds>1], (*The steps are correlated and so the number of independent observations is in fact lower! 
                                    But always take at least 10 samples *)
            pVal=AndersonDarlingTest[RandomSample[data,Max[Round[Length[data]/ds],10]]];
         ,(*else*)
            pVal=AndersonDarlingTest[data];
        ];
        (*Hack to clear memory wasted by AndersonDarlingTest *) 
        CleariHypothesisTestFunctionMemory[];         
        
        
        If[Not@VectorQ[params, NumberQ],
            Print["Could not fit distribution parameters",data];
            Return@ConstantArray[Missing[],7];
          ];
        {ux,uy,a,b,c}=params;

        
        (*Is normal distribution?*)
        isNormal=pVal>$SignificanceLevel;
        
        
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
     MakeInPeriodicCellUncompiled[];
     compiledSelectBinUncompiled[];
     GetDifferencesUncompiled[];     
     compiledGetContigIntervalsUncompiled[];
     compiledSelectByMiddlePointUncompiled[];
     $HaveFunctionsBeenCompiled = True; 
    );

CompileFunctionsIfNecessary[]:= If[! $HaveFunctionsBeenCompiled, CompileFunctions[]];


   
(*GetDiffusionInBinsBySelect*)
(*Calculates the diffusion coeficients in all bins of the cell. Bining is re-done for each bin by select.  
 rwData --  Data of the diffusion process. A list of lists of points {{t,x,y},...}
 dt     --  the timstep between two points in the units of time 
 binSpec -- specfication of the bins {min,max,dstep}
 cellMinMax The periodic cell limits {{minx,minY},{maxX, maxY}. If Null then taken form binSpec}*)

Options[GetDiffusionInBinsBySelect] := {"Parallel"->True,(*Use parallel functions*)
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
                (*DistributeDefinitions@rwData;*)
                With[ {IrwData=rwData,IcellRange = {cellMin,cellMax},Idt = dt,injectedOptions = FilterRules[{opts},Options@GetDiffusionInBin]},
                    PutsE["injectedOptions:\n",injectedOptions, LogLevel->1];
                    ParallelMap[
                        GetDiffusionInBin[IrwData, Idt, {#[[1]]-#[[2]]/2,#[[1]]+#[[2]]/2},IcellRange,injectedOptions]&
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

(*GetDiffusionInBinsByMiddlePoint*)
GetDiffusionInBinsByMiddlePoint::usage="
Calculates the diffusion by assigning steps (vectors) to bins by the steps's middle point.
 rwData --  Data of the diffusion process. A list of lists of points {{t,x,y},...}
 dt     --  the timstep between two points in the units of time 
 binSpec -- specfication of the bins {min,max,dstep}
 cellMinMax The periodic cell limits {{minx,minY},{maxX, maxY}. If Null then taken form binSpec}
";
Options[GetDiffusionInBinsByMiddlePoint] := {"Parallel"->True,(*Use parallel functions*)
                                       "CellRange"->Automatic (*cell range in {{MinX,MinY},{MaxX,MaxY}}. If automatic, then calculated from binsOrBinSpec*)
                                      }~Join~Options[GetDiffusionInBin];
Attributes[GetDiffusionInBinsByMiddlePoint]={HoldFirst};
GetDiffusionInBinsByMiddlePoint[rwData_,dt_,binsOrBinSpec_,opts:OptionsPattern[]] :=
    Block[ {$VerbosePrint = OptionValue["Verbose"], $VerboseLevel = OptionValue["VerboseLevel"],$VerboseIndentLevel = $VerboseIndentLevel+1},
        Module[ {bins, cellMin,cellMax},
            Puts["********GetDiffusionInBinsByMiddlePoint********"];
            PutsE["rwData: ",rwData, LogLevel->2];
            PutsE["binsOrBinSpec: ",binsOrBinSpec, LogLevel->2];
            Puts[$VerboseIndentString,"dimensions: ",Dimensions@binsOrBinSpec];
            PutsOptions[GetDiffusionInBinsBySelect, {opts}, LogLevel->2];
             
            bins=GetBinsFromBinsOrSpec@binsOrBinSpec;
            (*get cellMin/max from the bins if the option for cellRange is automatic*)
            {cellMin,cellMax}=If[#===Automatic,GetCellRangeFromBins@bins,#]&@OptionValue@"CellRange";
            PutsE["used cellRange: ",{cellMin,cellMax}, LogLevel->2];
            
            If[ OptionValue@"Parallel", (*then*)
                (*DistributeDefinitions@rwData;*)
                With[ {IrwData=rwData,IcellRange = {cellMin,cellMax},Idt = dt,injectedOptions = FilterRules[{opts},Options@GetDiffusionInBin]},
                    PutsE["injectedOptions:\n",injectedOptions, LogLevel->1];
                    ParallelMap[
                        GetDiffusionInBinByMiddlePoint[IrwData, Idt, {#[[1]]-#[[2]]/2,#[[1]]+#[[2]]/2}, IcellRange, injectedOptions]&
                        ,bins,DistributedContexts->None]
                ],(*else*)
                With[ {injectedOptions = FilterRules[{opts},Options@GetDiffusionInBin]},
                    PutsE["injectedOptions:\n",{injectedOptions}, LogLevel->3];
                    Map[
                         GetDiffusionInBinByMiddlePoint[rwData, dt, {#[[1]]-#[[2]]/2,#[[1]]+#[[2]]/2}, {cellMin,cellMax}, injectedOptions]&
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

Options[GetDiffusionInBins] := {"Parallel"->True,(*Use parallel functions*)
							   "StepsToBinMethod"->"Pad", (*Pad, NoPad, MiddlePoint*) 
							   "CellRange"->Automatic (*cell range in {{MinX,MinY},{MaxX,MaxY}}. If automatic, then calculated from binsOrBinSpec*)
							   }~Join~Options[GetDiffusionInBin];
Attributes[GetDiffusionInBins]={HoldFirst};
GetDiffusionInBins[rwData_,dt_,binSpec_,opts:OptionsPattern[]] :=
Block[{$VerbosePrint=OptionValue["Verbose"], $VerboseLevel=OptionValue["VerboseLevel"],$VerboseIndentLevel=$VerboseIndentLevel+1}, 
    Module[ {},
            Puts["********GetDiffusionInBins********"];
            PutsOptions[GetDiffusionInBins,{opts},LogLevel->2];
            Switch[OptionValue@"StepsToBinMethod",
            "Pad",(*then*)
            	GetDiffusionInBinsBySelect[rwData,dt,binSpec, FilterRules[{"PadSteps" -> True}~Join~{opts},Options@GetDiffusionInBinsBySelect]],
            "NoPad",(*then*)
                GetDiffusionInBinsBySelect[rwData,dt,binSpec, FilterRules[{"PadSteps" -> False}~Join~{opts},Options@GetDiffusionInBinsBySelect]],
            "MiddlePoint", (*then*)
                GetDiffusionInBinsByMiddlePoint[rwData,dt,binSpec, FilterRules[{"PadSteps" -> False}~Join~{opts},Options@GetDiffusionInBinsBySelect]],
            "GatherBy", (*then*)
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
	  "StepsInBin"->Null,"Stride"->stride, "PValue"->1, "IsNormal"->True,"xMinWidth"->dx,"yMinWidth"->dy,
	  
	  "DxError"->0,"DyError"->0,"DaErorr"->0, "sxError"->0, "syError"->0, "StepsInBinError"->0,
      "xMinWidthError"->0,"yMinWidthError"->0, "uxError"->0,"uyError"->0,"PValueError"->0}
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
    {dx,dy,da}=GetValue[{"Dx","Dy","Da"},diffInfo];
    (*If any of the values are missing just return the non-modified*)
    If[MemberQ[{dx,dy,da},Missing[___]],Return[diffInfo]];
    (*Get sorted values*)    
    {dx,dy,da}=DiffusionNormalForm@{dx,dy,da};
    

	(*Modify the original lis*)
	diffInfo /.   {Rule["Dx", _] -> Rule["Dx", dx],
			       Rule["Dy", _] -> Rule["Dy", dy],
			       Rule["Da", _] -> Rule["Da", da]}
]

DiffusionNormalForm[diffInfos:{{__Rule}..}]:= DiffusionNormalForm/@diffInfos;

DiffusionNormalForm[diffInfosWithStrides:{{{__Rule}..}..}]:= DiffusionNormalForm/@diffInfosWithStrides;

DiffusionNormalForm::wrongargs="Wrong arguments for DiffusionNormalForm `1` ";  
DiffusionNormalForm[args___]:=(Message[DiffusionNormalForm::wrongargs,Shallow@args];$Failed);  

ClearAll@SwitchValueRule;
SwitchValueRule[listOfRules_, key1_, key2_] :=
 With[{val1 = key1 /. listOfRules, val2 = key2 /. listOfRules},
  {
   Rule[key1, _] -> Rule[key1, val2],
   Rule[key2, _] -> Rule[key2, val1]
   } 
  ];

ClearAll@TransposeDiffusion;

TransposeDiffusion[diffInfo : {__Rule}] :=
  Block[{dx, dy, da},
   da = GetValue["Da", diffInfo];
   (*If any of the values are missing just return the non-modified*)
   If[MatchQ[da,Missing[___]], Return[diffInfo]];
   diffInfo /.Flatten@{
      Rule["Da", _] -> Rule["Da", Mod[90-da,180]],
      SwitchValueRule[diffInfo, "x", "y"],
      SwitchValueRule[diffInfo, "ux", "uy"],
      SwitchValueRule[diffInfo, "sx", "sy"],
      SwitchValueRule[diffInfo, "xWidth", "yWidth"],
      SwitchValueRule[diffInfo, "xMinWidth", "yMinWidth"],
      SwitchValueRule[diffInfo, "uxError", "uyError"],
      SwitchValueRule[diffInfo, "sxError", "syError"],
      SwitchValueRule[diffInfo, "xMinWidthError", "yMinWidthError"](*,
      SwitchValueRule[diffInfo, "Dxx", "Dyy"], (*What about Dxy*)
      SwitchValueRule[diffInfo, "DxxError", "DyyError"]*)
      }
      ];


TransposeDiffusion[diffInfos:{{__Rule}..}]:= TransposeDiffusion/@diffInfos;
TransposeDiffusion[diffInfosWithStrides:{{{__Rule}..}..}]:= TransposeDiffusion/@diffInfosWithStrides;

TransposeDiffusion::wrongargs="Wrong arguments for TransposeDiffusion `1` ";  
TransposeDiffusion[args___]:=(Message[TransposeDiffusion::wrongargs,Shallow@args];$Failed);  



Options[SaveDiffusions] = {"Metadata" -> Automatic, 
   "Description" -> "", "RawSteps" -> None, "OverrideExisting" -> False,
                        "DiffusionsFileName" -> "diffusions.mx.gz",
                        "MetadataFileName" -> "metadata.m",
                        "RawStepsFileName" -> "rawSteps.mx.gz",
                        "Title"->"",
                        "Description" -> ""};
SaveDiffusions[name_String, diffs:diffInfosWithStride, opts : OptionsPattern[]] :=
    Block[ {dir, metadataTMP, diffusionsFN, metadataFN, rawStepsFN, 
      existingFileNames},
        Puts["***SaveDiffusions****"];
        PutsOptions[SaveDiffusions, {opts}, LogLevel -> 2];
        Quiet[dir = CreateDirectory@name;];
        (*So that we dont get diffrent files out of sync *)
        existingFileNames = FileNames@FileNameJoin@{dir, "*"};
        PutsE[existingFileNames, LogLevel -> 3];
        If[ OptionValue@"OverrideExisting",          
            DeleteFile@existingFileNames
        ,(*else*)
            If[Length[existingFileNames] > 0,Print["Warning: Files allready exist in " <> name]];
        ];
        If[ dir === $Failed,
            Throw["Directory " <> name <> " could not be created"]
        ];
        diffusionsFN = FileNameJoin@{dir, OptionValue@"DiffusionsFileName"};
        metadataFN   = FileNameJoin@{dir, OptionValue@"MetadataFileName"};
        rawStepsFN   = FileNameJoin@{dir, OptionValue@"RawStepsFileName"};
        
        (*Retrive the metadata if set to automatic*)
        metadataTMP = (If[ # === Automatic,
                           Throw["Metadata must be specified manually"],
                           #
                       ]) &@
         OptionValue@"Metadata";
        {"Metadata" -> Export[metadataFN, metadataTMP],
            "Diffusions" -> 
         Export[diffusionsFN, diffs, "CompressionLevel" -> .3],
            "RawSteps" -> (If[ Length[#] < 1,
                               None,
                               Export[rawStepsFN, #, "CompressionLevel" -> .1]
                           ] &@
           OptionValue["RawSteps"])}
    ];

SaveDiffusions[name_String, diffs:{_Rule..}, opts : OptionsPattern[]] :=   
Module[ {diffdata,metadata, rawsteps,iopts=opts},
        Puts["***SaveDiffusions (in List)****"];
        PutsOptions[SaveDiffusions, {opts}, LogLevel -> 2]; 
        
        diffdata="Diffusions"/.diffs;
        metadata="Metadata"/.diffs;
        rawsteps="RawSteps"/.diffs;
        
        SaveDiffusions[name,diffdata, {"Metadata"->metadata,"RawSteps"->rawsteps}~Join~iopts]
  (*diffs must contain rule for metadata and for diffusions*)
]/; (And[MemberQ[#, "Metadata"], MemberQ[#, "Diffusions"]]&[diffs[[All,1]]]);

SaveDiffusions[else___]:=Throw["Wrong arguments for SaveDiffusions",ToString@Short@{else}];
 

Options[LoadDiffusions] = {"LoadMetadata" -> True, 
   "LoadDiffusions" -> True, "LoadRawSteps" -> False,
                        "DiffusionsFileName" -> "diffusions.mx.gz",
                        "MetadataFileName" ->   "metadata.m",
                        "RawStepsFileName" ->   "rawSteps.mx.gz"};
LoadDiffusions[name_, opts : OptionsPattern[]] :=
  Block[{metadata = None, diffs = None, rawSteps = None, diffusionsFN,
     metadataFN, rawStepsFN},
       Puts["***LoadDiffusions****"];
       PutsOptions[LoadDiffusions, {opts}, LogLevel -> 2];  
    If[Not@FileExistsQ@name, 
    Throw["Diffusions directory with name " <> name <> 
      " does not exists!"]];
   
    diffusionsFN = FileNameJoin@{name, OptionValue@"DiffusionsFileName"};
    metadataFN   = FileNameJoin@{name, OptionValue@"MetadataFileName"};
    rawStepsFN   = FileNameJoin@{name, OptionValue@"RawStepsFileName"};
    
    (*Todo check that the files exist?*)
    metadata = If[OptionValue@"LoadMetadata",   Puts["Loading: ",metadataFN];  Import[metadataFN]];
    diffs    = If[OptionValue@"LoadDiffusions", Puts["Loading: ",diffusionsFN];Import[diffusionsFN]];
    rawSteps = If[OptionValue@"LoadRawSteps",   Puts["Loading: ",rawStepsFN];  Import[rawStepsFN]];
      { "Metadata" -> metadata,
        "Diffusions" -> diffs,
        "RawSteps" -> rawSteps}
    
   ];
   

ClearAll@AverageOneDiffBin;
(*Given a list of diff bins, returns the averages and std errors of Quantities*)

ClearAll[meanAngle];
meanAngle[data_List] := Arg[Mean[Exp[I N[data Degree]]]]/Degree;

ClearAll[stdevAngle];
stdevAngle[data_List] := 
  With[{ddata = N@data, meanAngle = meanAngle@data},
   Sqrt[Total[Mod[ddata - meanAngle, 360, -180]^2]/(Length[data] - 1)]
   ];

AverageOneDiffBin[listOfDiffBins_,Quantities_]:=
Module[{result, quantity, quantityErr, tmpvals, mean, stderr,i, das},
   (*result=First@listOfDiffBins;*)
   result=DeleteDuplicates[Flatten[listOfDiffBins],First[#1] === First[#2]&];
   Do[
      quantityErr=quantity<>"Error";
      tmpvals=GetValues[Evaluate@quantity,listOfDiffBins];
      (*Take only numeric values*)
      tmpvals=Cases[tmpvals, _?NumericQ]; 
      If[Length@tmpvals<=1
      ,(*then*)
          mean=stderr=Missing["To few points in bin"];
      ,(*else*)
          Switch[quantity,
              "Da",mean=meanAngle@tmpvals;stderr=stdevAngle@tmpvals;,
              _,mean=N@Mean@tmpvals; stderr=N@StandardDeviation@tmpvals;];
      ];

      result=result/.(Rule[quantity,_]->Rule[quantity,mean]);
      (*delete it just in case, because not all diffs allready have a diff error*)
      result=DeleteCases[result, Rule[quantityErr,_]];
      AppendTo[result,  Rule[quantityErr,stderr]];
      
        ,{quantity,Quantities}];
         
    (*If we request Dx, Dy, and Da then add the components of the tensor and their errors*)
	If[And @@ (MemberQ[Quantities, #] & /@ {"Dx", "Dy", "Da"}),
	 tmpvals = GetValues[{"Dx", "Dy", "Da"}, listOfDiffBins];
	 (*Get the diff tensors componets Dxx, Dxy and Dyy*)
	 tmpvals = 
	  Flatten[Get2DCovarianceFromDiff[#]][[{1, 2, 4}]] & /@ tmpvals;
	 (*Take only numeric values*)
	 tmpvals = Select[tmpvals, VectorQ[#, NumberQ] &];
	 
	 If[Length@tmpvals <= 1,(*then*)
	  mean = stderr = ConstantArray[Missing["To few points in bin"], 3];
	  ,(*else*)
	  mean = Mean@tmpvals; stderr = StandardDeviation@tmpvals;];
	 
	 Do[
	  AppendTo[result, Rule[{"Dxx", "Dxy", "Dyy"}[[i]], mean[[i]] ] ];
	  AppendTo[result, Rule[{"DxxError", "DxyError", "DyyError"}[[i]], stderr[[i]] ] ];
	  , {i, 3}];
	  
	 das=GetDiffFrom2DCovariance[{{mean[[1]],mean[[2]]},{mean[[2]],mean[[3]]}}];
	 (*Put tensot back into Dx Dy Da -- This is a hack! What to do about errors.*)
	 Do[
      result=DeleteCases[result, Rule[{"Dx", "Dy", "Da"}[[i]],_]];
      AppendTo[result, Rule[{"Dx", "Dy", "Da"}[[i]], das[[i]] ] ];
     , {i, 3}];
	];
   result
];

ClearAll@EstimateDiffusionError   
Options@EstimateDiffusionError={
            "Quantities"-> 
            {"Dx", "Dy", "Da", "ux", "uy", "sx", "sy", "PValue",  
             "xMinWidth", "yMinWidth"},
             "DeleteCases"->"actual"
            };

EstimateDiffusionError::usage="EstimateDiffusionError[listOfDiffs] take a list of diffusions (must have same bins) and 
calculates the averages and standard deviations for the given list of quantities (for example Dx, Dy...). 
Returns a diffusions list, where Dx->Avg[{Dx...}] and DxError->StDev[{Dx...}]

EstimateDiffusionError[listOfDiffsWithMetadata] returns diffusion errors with metadata.

EstimateDiffusionError[listOfDirNames] Loads the files and returns diffusion errors with metadata.

EstimateDiffusionError[parentDirName] Loads the files in the parent directory and returns diffusion errors with metadata.

"


EstimateDiffusionError[diffs:listOfdiffInfosWithStride,opts:OptionsPattern[]]:=
Block[ {$VerboseIndentLevel = $VerboseIndentLevel+1, binIndex, q, strideIndex,
        binNum (*Number of bins in one diffusion set*),strideNum (*number of strides*),
        quan, 
        result (*Diff set for results*)},
    Puts["***EstimateDiffusionError****"];
    PutsOptions[EstimateDiffusionError, {opts}, LogLevel -> 2];   
    
    quan = OptionValue@"Quantities";
    
    
    (*the dimensions describe : {set, bins, stride, diffRules}. diffRules is given only if all diff sets have the same umber of rules "Dy"->_ etc...*)
    binNum=Dimensions[diffs][[2]];
    strideNum=Dimensions[diffs][[3]];

    Table[AverageOneDiffBin[diffs[[All,binIndex,strideIndex]],quan],{binIndex,binNum},{strideIndex,strideNum}]
   
];


EstimateDiffusionError[diffDataWithMeta_List,opts:OptionsPattern[]]:=
Block[ {$VerboseIndentLevel = $VerboseIndentLevel+1},
Module[{diffData, diffErrorsMeta, diffErrors},     
    Puts["***EstimateDiffusionError (Diffs with metadata)****"];
    PutsOptions[EstimateDiffusionError, {opts}, LogLevel -> 2];   
    
    diffData ="Diffusions"/.diffDataWithMeta;
	
	diffErrors=EstimateDiffusionError[diffData];
	(*Get the metadata of the first diff. Just change the titles. *)
	(*TODO: Should do some error testing to ensure all diffs are the compatible*)
	diffErrorsMeta = ("Metadata"/.First[diffDataWithMeta])
	      (*remove possible serial number at the end*)
	      /. Rule["Title", t_] :> Rule["Title", StringReplace[ToString@t, RegularExpression["(_|\\s)\\d+$"] -> ""]]
	      (*Append description*)
	      /. Rule["Description", d_] :> Rule["Description", ToString@d <> " Generated from " <> ToString@Length@diffDataWithMeta <> " runs"];
    diffErrorsMeta=diffErrorsMeta~Join~{"NumberOfRuns"->Length@diffDataWithMeta};      
	     
   {"Diffusions"->diffErrors,"Metadata"->diffErrorsMeta}
  (*Test that all diffusions have metadata and diffusion rules*)
]]/; And @@ (And[MemberQ[#, "Metadata"], MemberQ[#, "Diffusions"]] & /@ diffDataWithMeta[[All, All, 1]]);

EstimateDiffusionError[diffDirNames:{_String..},opts:OptionsPattern[]]:=
Block[ {$VerboseIndentLevel = $VerboseIndentLevel+1},
Module[{},     
    Puts["***EstimateDiffusionError (List of dir names)****"];
    PutsOptions[EstimateDiffusionError, {opts}, LogLevel -> 3];   
    
    (*Ensure all dirs exist*)
    Assert[And@@(FileExistsQ/@diffDirNames)];
    
    EstimateDiffusionError[LoadDiffusions/@diffDirNames,opts]
]]

EstimateDiffusionError[parentDir_String,opts:OptionsPattern[]]:=
Block[ {$VerboseIndentLevel = $VerboseIndentLevel+1},
Module[{dirs},     
    Puts["***EstimateDiffusionError (Parent Dir name)****"];
    PutsOptions[EstimateDiffusionError, {opts}, LogLevel -> 3];   
    dirs=Select[FileNames@FileNameJoin@{parentDir,"*"},DirectoryQ];
    dirs=DeleteCases[dirs,OptionValue["DeleteCases"]];
    EstimateDiffusionError[dirs,opts]  
]]


EstimateDiffusionError[else___]:=Throw["Wrong arguments for EstimateDiffusionError",ToString@Short@else];


ClearAll@EstimateDiffusionErrorAndSave;
Options[EstimateDiffusionErrorAndSave]={"SkipExisting"->True,"FakeRun"->False}~Join~Options@EstimateDiffusionError;
EstimateDiffusionErrorAndSave::usage="EstimateDiffusionErrorAndSave[parentDirName, targetDirName:Null] Gets diffusion error and saves it to target dir.";
EstimateDiffusionErrorAndSave[parentDir_String,targetDir_String,opts:OptionsPattern[]]:=
Module[{diffFile, iOpts},
   diffFile=FileNameJoin@{targetDir,"diffusions.mx.gz"};
   If[FileExistsQ@diffFile,
       If[OptionValue["SkipExisting"],
            Print["Skipping: "<>diffFile];Return[True];,
            Print["Overwritng:"<>diffFile]
       ];
   ];
  
   Print["Getting diffusion for: ", targetDir, " from: \n", parentDir];
   If[Not@OptionValue@"FakeRun",  
     iOpts=FilterRules[{opts},Options[EstimateDiffusionError]];
     SaveDiffusions[targetDir,EstimateDiffusionError[parentDir,iOpts]]
   ]    
];

EstimateDiffusionErrorAndSave[parentDir_String,opts:OptionsPattern[]]:=EstimateDiffusionErrorAndSave[parentDir,parentDir,opts];

EstimateDiffusionErrorAndSave[diffDirNames:{_String..},targetDir_String,opts:OptionsPattern[]]:=
Module[{diffFile,iOpts},
   diffFile=FileNameJoin@{targetDir,"diffusions.mx.gz"};
   If[FileExistsQ@diffFile,
       If[OptionValue["SkipExisting"],
            Print["Skipping: "<>diffFile];Return[True];,
            Print["Overwritng:"<>diffFile]
       ];
   ];
   
   Print["Getting diffusion for: ", targetDir, " from: \n", diffDirNames];
   If[Not@OptionValue@"FakeRun",
      iOpts=FilterRules[{opts},Options[EstimateDiffusionError]];
      SaveDiffusions[targetDir,EstimateDiffusionError[diffDirNames,iOpts]]
   ];       
];

EstimateDiffusionErrorAndSave[else___]:=Throw["Wrong arguments for EstimateDiffusionErrorAndSave",ToString@Short@else];


ClearAll@GetDiffusionsRMSD   
Options@GetDiffusionsRMSD={
                DistanceFunction->EuclideanDistance,
                "ReturnErrors"->False
            };

GetDiffusionsRMSD::usage="GetDiffusionsRMSD[diffs1, diffs2] takew two sets of diffusions (must have same bins) and 
calculates the RMSD (Root mean square displacement) between all the bins. The distance function used on the covariance tensors can be given as an option. Defaults to EuclidianDistance . 

Returns the RMSD."

GetDiffusionsRMSD[diffs1:diffInfos,diffs2:diffInfos,opts:OptionsPattern[]]:=
Block[ {$VerboseIndentLevel = $VerboseIndentLevel+1,i,covars1,covars2,rmsdList,df=OptionValue@DistanceFunction},
    Puts["***GetDiffusionsRMSD****"];
    PutsOptions[GetDiffusionsRMSD, {opts}, LogLevel -> 2];       
    If[Length@diffs1=!=Length@diffs2,
       Throw@StringForm["Number of bins in first and second diffusion set must be the same ``=!=``",Length@diffs1,Length@diffs2]
      ];
    
     
     (*devided by two since it has 2*sigma in the definition of the CovarianceTensor. devided by 3 to get per component deviation*)
     covars1=(Get2DCovarianceFromDiff/@GetValues[{"Dx","Dy","Da"},diffs1])/2;
     covars2=(Get2DCovarianceFromDiff/@GetValues[{"Dx","Dy","Da"},diffs2])/2;
     
        
     (*Handle missing casses*)
     (*For now just replace with 0
     covars1=covars1/.Missing[___]->0;
     covars2=covars2/.Missing[___]->0;*)
     rmsdList=Transpose@{covars1,covars2};        
     
     (*Filter missing*)
     (*Select only tensors that both have only numeric values*)
     rmsdList=Select[rmsdList,ArrayQ[#, _, NumericQ]&];
     If[Length@rmsdList===0, Return[Missing["To little points with valid diffusions"]]];
     rmsdList=df[#[[1]] , #[[2]] ]&/@rmsdList; 
     If[OptionValue["ReturnErrors"],
         With[{mean=Mean@rmsdList},
            {mean, (* Percentiles corresponding to +-sigma*)
                {mean-Quantile[rmsdList, 25/100, {{1/2, 0}, {0, 1}}], mean-Quantile[rmsdList, 75/100, {{1/2, 0}, {0, 1}}]}}
         ]
     ,(*else*)        
        Mean@rmsdList
     ]
];

Clear[MoveDiffusionBins];
MoveDiffusionBins::usage="MoveDiffusionBins[diff_, x_, y_]
Moves the bin centers by x and y." 
MoveDiffusionBins[diff_, x_, y_]:=Module[{result},
    result=diff/.("x"->ax_):>("x"->ax+x);
    result=result/.("y"->ay_):>("y"->ay+y);
    result
]
Clear[JoinDiffusionBins];
JoinDiffusionBins::usage="JoinDiffusionBins[{diff1_,diff2_, diffn_}] 
a list of  diffusion structures {\"Diffusions\"->, \"Metadata\"->} "
JoinDiffusionBins[diffs_] := Module[{rawdiffs, metas, steps},
  rawdiffs = "Diffusions" /. diffs;
  metas = "Metadata" /. diffs;
  steps = "RawSteps" /. diffs;
  rawdiffs = Join @@ rawdiffs;
  {"Diffusions" -> rawdiffs, "Metadata" -> metas, 
   "RawSteps" -> steps}]

(*Distribution Fit Test leaks memory. This fixes it.*)
ClearAll@CleariHypothesisTestFunctionMemory;
CleariHypothesisTestFunctionMemory[] :=
  DownValues@
    Statistics`GoodnessOfFitTestingDump`iHypothesisTestFunction = 
   Uncompress@
    "1:eJztWktv4zYQtrOvPjZFu4cCfVwK9I1edlGgi/\
bkxnESYJ3Ekbd76SG0RNlEaFElqWT978sRrZclOxyt7UWBXgiTGc4M5/\
HNkMo3E3EVPux0OgqGV0zp8ABmH5vhKuG0TzlZ0CDsZounggeXRGsqI0v5oRmORBQwzcRy\
6Q8zeJpow4356vpEiCCiSl2EA6bH1CxG034yj6/Z6SIWekYVU7A8SCK/\
YJLp4z0zP8xGTYO+\
mUo2SYAoldtLtJgbOb7d8sQMS93UD446zHQQPoDNIKaqT59oUmf8syPjI8I5lWEH9j8yw5\
+cRDf3TFNhj80wJNqfjez8q6rAV2wiiVxcn477fxGeUMxJPTDqeBFT7ylsIvOY07E4Tzi3\
osAQHtXvy4sopx0UphP+TXhYlqZ+dGQUYIhjIpX6yZE4TFm7UgPxC/\
WdIzVxppw4mzQg2pk2pnP1vSPtXexMSqbOpDF1J3Xnqm7DxxkUHIl5LJIoOH4bS0ObB/\
QyRVARZgFmm3kMDE8uBt4HkM8AVTlwZ/qhIs9u/Qgwn8ac+LTHOe6ElWQvZlBCUkD7+/\
fBy+e/uUf4OhYv3UO/\
ag9EHuzOXQBXUFWeWxGg4FlY6NmLlrAGw4W0RffXZj2Mx31JNR0mXLNbIhnRtIysI5z/\
UlEvNot6HW1LEBz2XOhavetmTj+lJMCB+KdL05ZVax8B6UbXFGIx8Ze1+\
6mVdSmpzwAzUPALyXwWhSxielGZ7PMcePbbTBRoB3Lj1ZME+\
rDXkSJzOmqHxlDCbZT90qz1CdXlCLok0ggzfZ/CRXmnAoePXIsKogDvDqS+\
AB9k5z42NNCbiWhI9UyU8Mn4ZFMuo87yMMudHoce22y6NaTVDr9q0m59Bh1g2kZ79J+\
ERj7Ny4f9e95mF+\
sH9zkGAm5oFsiUFvei5cK5sRDyNpAWgJDpEFldu5ltr6hOzB3kMAWHABw0Z8ZctJ4qyxKS\
d8keJA2qw2u4YLVitOK5Vl1K2k3b5mylmctC3sQ7M5BETQ1dxqx79AP44jr1qm269QPmnQ\
vUpTdC3pj9BbDtFjeRQZnqOpYJDZ+\
44hSioW7oLJvWvE8sSwl77flRMbbBBU2sG2K7DQbnkNCn3CTjEVFIte9TtPGBAOuFu7jRC\
w8y5S9iwHcr8MsOXNOjYE0RxODH+sjPk6PBDbvrJlrZjkx3bqe07vl+Iom/\
OBGEt1M0pjtX9FlDYyBkS3V3b9fDsvfrhv2vlh4E9ja8626sS2hEybmtxjAmv6qcal7DhF\
SVVXO8Yt6Y2hbScpO8oRvDvSVCdf52QBjPnuOzzjudrKhaurvneXSeG2PU+OCDi0Nc/\
rRPvfK5UwsXz9UurQpOzU45Ywppte4a7khnUXpX4cbW5oeJgEVvomik38Wv76D6nj3acAd\
Bfjj6/ytRy69E3UzYkJmAj6beoQWCXhxz5pMJf8+u+LrRFaUqvkWnHOzZKbVHBvu4A+\
2Bd8e0P9vnN7syLKXO/dwMjTYf7ekTRA0oVx93MRLWpHQNeNZBERjuDdOzpsvh/\
h4O92T5/IloPJOUBCutEOqYuYGGJLZ8QVI12SGtjt9q0+/pbTm2eKUbMJn5CSRfCmX/\
uWCPlgTLeVxols7wUbIZnlMo+SyDEsjlHE62Cu7dbYD7emXLrcu/n5/PiQ==";

End[]

EndPackage[]

