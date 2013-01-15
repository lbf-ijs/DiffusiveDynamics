(* Mathematica Package *)

(* Created by the Wolfram Workbench 14.1.2013 *)

BeginPackage["DiffusiveDynamics`Utils`"]
(* Exported symbols added here with SymbolName::usage *) 

$VerboseLevel = 3;
$VerbosePrint = False;
$VerbosePrintFunction = Print;
$VerbosePrintMemoryUsage = False;
$PrintMemoryUsageBaseLine = 0;

Puts::usage = "Puts[msg, data, opt] Prints msg with a string representation of data.
Puts[msg] prints a string ";

ClearAll[StrideData];
StrideData::usage="StrideData[data, maxPoints] Takes every n-th point from data so that the length of the result is less then maxPoints";
Begin["`Private`"]
(* Implementation of the package *)

(*Util functions*)
 

(*Verbose not quite working*)

SetAttributes[Puts,HoldAll];
Options[Puts] = {DisplayFunction->Shallow};
Puts[msg_,data_,n_Integer:1,opt:OptionsPattern[]] :=
    (If[ $VerbosePrint &&($VerboseLevel>=n), (*then*)
         $VerbosePrintFunction[msg,ToString[OptionValue[DisplayFunction][data]]]
     ];
     If[ $VerbosePrintMemoryUsage,
         PrintMem[]
     ]);
Puts[msg__,n_Integer:1,opt:OptionsPattern[]] :=
    (If[ $VerbosePrint &&($VerboseLevel>=n),
         $VerbosePrintFunction[msg]
     ];
     If[ $VerbosePrintMemoryUsage,
         PrintMem[]
     ]);


ClearAll[Putsf]
Putsf[str_,args___,n_Integer:1,opt:OptionsPattern[]] :=
    Puts[ToString@StringForm[str,args],n,opt];



StrideData[data_List,maxPoints_?NumericQ] :=
    data[[1;;-1;;Ceiling[Length[data]/maxPoints]]];



PrintMem[] :=
    Print[StringForm["Memory in use: `1`",N[(MemoryInUse[]-$memoryBaseLine)/1024^2,2],MaxMemoryUsed[]/1024.^2]];

ClearAll[getEnv]
Options[getEnv] = {"ToExpression"->False}
getEnv[name_,default_, opts:OptionsPattern[]] :=
    Module[ {t},
        t = Environment[name];
        If[ t===$Failed, (*then*)
            t = default
        ];
        If[ OptionValue["ToExpression"],(*then*)
            t = ToExpression@t
        ];
        Return@t;
    ];


ClearAll[SymbolNamesToString];
SetAttributes[SymbolNamesToString,{HoldFirst,Listable}];
SymbolNamesToString[x_] :=
    ToString[Unevaluated[x]];

ClearAll[GetValues,GetValue];
SetAttributes[GetValues,HoldAll];
GetValues[values_,list_] :=
    With[ {strval = SymbolNamesToString[values]},
        (strval/.#)&/@list
    ];
SetAttributes[GetValue,HoldAll];
GetValue[value_,list_] :=
    First@GetValues[value,{list}];


On[Assert];
$AssertFunction = Throw[{##}]&;

ClearAll[Unshallow];
Unshallow[sh_] :=
    If[ Head[sh]===Shallow,(*then*)
        sh[[1]],(*else*)
        sh
    ];

ClearAll[CompiledFunctionNames];
CompiledFunctionNames[pattern_] :=
    Select[Names["Global`*"], 
     MatchQ[Evaluate[Symbol@#], _CompiledFunction] &];

ClearAll[DistributeCompiledFunction]
DistributeCompiledFunction[name_] :=
    With[ {copy = Symbol[name]},
        ParallelEvaluate[
         If[ MatchQ[Evaluate[Symbol@name], _Symbol],(*Print["Setting ",
          name];*)
             Evaluate[Symbol[name]] = copy
         ]]
    ];

ClearAll[DistributeCompiledFunctions];
DistributeCompiledFunctions[pattern_] :=
    DistributeCompiledFunction /@ CompiledFunctionNames[pattern];


ClearAll[SetKernelsDirectory];
SetKernelsDirectory[dir_] :=
    ParallelEvaluate[SetDirectory[dir]];


End[]

EndPackage[]







