(* Mathematica Package *)

(* Created by the Wolfram Workbench 14.1.2013 *)

BeginPackage["DiffusiveDynamics`"]
(* Exported symbols added here with SymbolName::usage *) 

Begin["`Private`"]
(* Implementation of the package *)

(*Util functions*)
ClearAll[StrideData];
StrideData[data_,maxPoints_] :=
    data[[1;;-1;;Ceiling[Length[data]/maxPoints]]];


ClearAll[PrintMem,$PrintMemoryUsageBaseLine,$PrintMemoryUsage];
$PrintMemoryUsageBaseLine = 0;
$PrintMemoryUsage = False;

PrintMem[] :=
    Print[StringForm["Memory in use: `1`",N[(MemoryInUse[]-$memoryBaseLine)/1024^2,2],MaxMemoryUsed[]/1024.^2]];

ClearAll[Puts,$VerbosePrint,$PrintFunction]
SetAttributes[Puts,HoldAll];
Options[Puts] = {DisplayFunction->Shallow};
$VerbosePrint = False;
$VerbosePrintFunction = PrintToConsole;
Puts[msg_,data_,opt:OptionsPattern[]] :=
    (If[ $VerbosePrint,
         $VerbosePrintFunction[msg,ToString[OptionValue[DisplayFunction][data]]]
     ];
     If[ $PrintMemoryUsage,
         PrintMem[]
     ]);
Puts[msg__,opt:OptionsPattern[]] :=
    (If[ $VerbosePrint,
         $VerbosePrintFunction[msg]
     ];
     If[ $PrintMemoryUsage,
         PrintMem[]
     ]);


ClearAll[Putsf]
Putsf[str_,args___] :=
    Puts@ToString@StringForm[str,args];
(*System`Puts[msg_List]:=If[$VerbosePrint, PrintToConsole[MapShowIt[msg]]];
System`Puts[msg_]:=If[$VerbosePrint, PrintToConsole[msg]];
System`PutsIt[code_]:=With[{y=code},If[$VerbosePrint,PrintToConsole[Defer[code=y]]];y];*)

(*$VerbosePrint =True;
Puts["test:\n",Print["THIS!"]]
$VerbosePrint =False;
Puts["test:\n",Print["THIS!"]];*)



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







