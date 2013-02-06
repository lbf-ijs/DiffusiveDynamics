(* Mathematica Package *)

(* Created by the Wolfram Workbench 14.1.2013 *)

BeginPackage["DiffusiveDynamics`Utils`"]
(* Exported symbols added here with SymbolName::usage *) 

$VerboseLevel = 3;
$VerbosePrint = False;
$VerbosePrintFunction = Print;

$VerbosePrintMemoryUsage = False;
$PrintMemoryUsageBaseLine = 0;

$VerboseIndentLevel = -1;
$VerboseIndentString = "> ";
ClearAll[GetVerboseIndentString];
GetVerboseIndentString::usage="Get the $VerboseIndentString multiplied $VerboseIndentLevel times"; 

LogLevel::usage="LogLevel Option";
ClearAll[Puts];
Puts::usage = "Puts[msg..., opt] Prints msg with a string representation of data.
Mesage is only printed if $VerbosePrint is true and LogLevel >= $VerboseLevel.";

ClearAll[PutsF];
PutsF::usage="PutsF[str_, args, opt] Puts a string with StringForm str and args are passed to StringForm";

ClearAll[PutsE];
PutsE::usage="PutsE[data, sparator, opts]. 
Puts the symbol data in the form
NameOf[data]+separator+ValueOf[data]. Also converts the data to string and makes sure it is not too long.";

ClearAll[StrideData];
StrideData::usage="StrideData[data, maxPoints] Takes every n-th point from data so that the length of the result is less then maxPoints";

ClearAll[PutsOptions];
PutsOptions::usage="PutsOptions[opts] Prints a readable list of options";


ClearAll[DistributeCompiledFunction, DistributeCompiledFunctions, CompiledFunctionNames]

DistributeCompiledFunction::usage="TODO";
DistributeCompiledFunctions::usage="TODO";
CompiledFunctionNames::usage="TODO";
SetKernelsDirectory::usage="TODO";

ClearAll[GetValues,GetValue];
GetValues::usage="TODO";
GetValue::usage="TODO";


ClearAll[LetL];
LetL::usage="LetL[const, body] implements nested With blocks for each constant. 
So it is possible to use previous constants in the definition of next constants."

Begin["`Private`"]
(* Implementation of the package *)

(*Util functions*)
 ClearAll[GetVerboseIndentString];
 (* The stuff with If[] just retruns 0 if $VerboseIndentLevel happens to be negative*)
 GetVerboseIndentString[]:=StringJoin@ConstantArray[$VerboseIndentString, (If[# < 0, 0, #])&@$VerboseIndentLevel]

(*Verbose not quite working*)

SetAttributes[Puts,HoldAll];
Options[Puts] = {LogLevel->1};

Puts[Shortest[msgs__], opt:OptionsPattern[]] :=
    (If[ $VerbosePrint &&($VerboseLevel>=OptionValue["LogLevel"]),
         (*Print["msg puts"];*)
         $VerbosePrintFunction[GetVerboseIndentString[],msgs]
     ];
     If[ $VerbosePrintMemoryUsage,
         PrintMem[]
     ]);



SetAttributes[PutsE,HoldAllComplete];
Options[PutsE] = {LogLevel->1,DisplayFunction->Short};
PutsE[data_, sparator_String:": ", opt:OptionsPattern[]] :=
    ((*Print[opt];*)
    Puts[ToString[Unevaluated[data]],sparator,ToString[data,FormatType->InputForm,TotalWidth->300]
    	,Evaluate@FilterRules[{opt},Options[Puts]]]);


PutsE[name_String, data_, sparator_String:"", opt:OptionsPattern[]] :=
    ((*Print[opt];*)
    Puts[name,sparator,ToString[data,FormatType->InputForm,TotalWidth->300],
    	Evaluate@FilterRules[{opt},Options[Puts]]]);


SetAttributes[PutsOptions,HoldAll];
Options[PutsOptions]={LogLevel->2, DisplayFunction->Shallow,"OptionIndentString"->"   "}~Join~Options@Puts;
(*TODO set a limit options to print*)
PutsOptions[symbol_Symbol, actualOpts_List, opts : OptionsPattern[]] := 
  (*Iterates all the options that are defined for symbol. Then gets the actual opton using option value. 
  This must be done, because default options are not put into actualOpts of OptionsPattern[]. 
  First is needed, because are interested in only option names and not the default values.*)
  With[{df=OptionValue@DisplayFunction,is=OptionValue@"OptionIndentString"},
  Puts["***"<>ToString@Unevaluated@symbol<>"*** Options: \n",
  	   StringJoin[(is<>
  	   	           ToString[First@#]<>" -> "<> 
  	   	           ToString[df@OptionValue[symbol,actualOpts,First@#],OutputForm]<>
  	   	           "\n")&
  	   	            /@Options[symbol]
  	   	          ]	(*end string join*)   
  	   ,Evaluate@FilterRules[{opts}, Options[Puts]]]
];
  
SetAttributes[PutsF,HoldAll];
Options[PutsF] = {LogLevel->1};
PutsF[str_String,Shortest[args__], opt:OptionsPattern[]] :=
    Puts[ToString@StringForm[str,args], Evaluate@opt];



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


CompiledFunctionNames[pattern_] :=
    Select[Names[pattern], 
     MatchQ[Evaluate[Symbol@#], _CompiledFunction] &];


DistributeCompiledFunction[name_] :=
    With[ {copy = Symbol[name]},
        ParallelEvaluate[
         If[ MatchQ[Evaluate[Symbol@name], _Symbol],(*Print["Setting ", name];*)
             Evaluate[Symbol[name]] = copy
         ]]
    ];


DistributeCompiledFunctions[pattern_] :=
    DistributeCompiledFunction /@ CompiledFunctionNames[pattern];


ClearAll[SetKernelsDirectory];
SetKernelsDirectory[dir_] :=
    ParallelEvaluate[SetDirectory[dir]];



SetAttributes[LetL, HoldAll];
SyntaxInformation[LetL] = {
   "ArgumentsPattern" -> {_, _}, 
   "LocalVariables" -> {"Solve", {1, Infinity}}
};
LetL /: Verbatim[SetDelayed][lhs_, rhs : HoldPattern[LetL[{__}, _]]] :=
   Block[{With}, Attributes[With] = {HoldAll};
     lhs := Evaluate[rhs]];
LetL[{}, expr_] := expr;
LetL[{head_}, expr_] := With[{head}, expr];
LetL[{head_, tail__}, expr_] := 
  Block[{With}, Attributes[With] = {HoldAll};
    With[{head}, Evaluate[LetL[{tail}, expr]]]];

End[]

EndPackage[]







