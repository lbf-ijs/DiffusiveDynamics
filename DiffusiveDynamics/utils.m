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
GetSubValue::usage="";

ClearAll[LetL];
LetL::usage="LetL[const, body] implements nested With blocks for each constant. 
So it is possible to use previous constants in the definition of next constants."

valueQ::usage="Check if the symbol has a defined value.";
(**)
withoutIndeterminate /: (f : Min | Max)[withoutIndeterminate[args___]] :=
  Block[{Indeterminate = f[]}, f[args]];

Clear[LoadData, LoadHeader, TakeData];
LoadData::usage="TODO";
LoadHeader::usage="TODO";
TakeData::usafe="TakeData[data,header,take] given a table and a header, extracts the coresponfing columns in in take-";

PermanentSet::usage="PermanentSet[symb, expr] saves the expr in the tagging rules";
PermanentSetCell::usage="PermanentSet[symb, expr] saves the symb in a seperate cell";

(*Taken from http://mathematica.stackexchange.com/questions/2886/transferring-a-large-amount-of-data-in-parallel-calculations*)
withUnpackingMemberQ[expr_] :=
 Module[{doneQ, unmatchable},
  Internal`InheritedBlock[{MemberQ},
   Unprotect[MemberQ];
   (* Can uncomment this if we want to print out the MemberQ calls:
   mq:MemberQ[args___]/;(Print@HoldForm[mq];True):=mq;
   *)
   MemberQ[list_, patt_Symbol, args___] /; !TrueQ[doneQ] :=
    Block[{doneQ = True},
     MemberQ[
      Unevaluated[list] /. _List?Developer`PackedArrayQ -> {unmatchable},
      Unevaluated[patt],
      args
     ]
    ];
   Protect[MemberQ];
   expr
  ]
 ];
SetAttributes[withUnpackingMemberQ, HoldAllComplete];

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
    (If[ ($VerbosePrint &&($VerboseLevel>=OptionValue["LogLevel"]) || 
         (OptionValue["LogLevel"]===0) (*For quick hacks to force printing*)),
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
    
ClearAll@RulesToList;
RulesToList[rules_] :=
  Flatten[rules //. Rule[a__, b__] :> List[a, b]];    

ClearAll[GetSubValue]
GetSubValue::usage =  "GetSubValue[r_Rule,list_List]\n given a a list of rules a->b->c returns the first  a->b->c element";
GetSubValue[r_Rule, list_List] := Block[{tmplist = list},
   Do[tmplist = GetValue[Evaluate@val, tmplist];, {val, RulesToList[r]}];
   tmplist
];

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


(*From here: http://mathematica.stackexchange.com/questions/5959/saving-data-inside-a-notebook-so-that-i-dont-have-to-run-it-again*)
SetAttributes[PermanentSetCell, HoldAll];
PermanentSetCell[var_Symbol, 
  value_] := (If[OwnValues[var] === {}, 
   Module[{nb = EvaluationNotebook[]}, 
    SelectionMove[nb, Before, EvaluationCell];
    NotebookWrite[nb, 
     Cell[ToString[Unevaluated[var]] <> " = Uncompress[\"" <> 
       Compress[var = value] <> "\"]", "Input", Editable -> False]]]];
  var)
  
SetAttributes[PermanentSet,HoldAll];
PermanentSet[var_Symbol, value_] :=
  Module[{nb=EvaluationNotebook[],
          name=ToString[Unevaluated[var]],
          expr=Compress[Unevaluated[value]]},
    If[TrueQ[CurrentValue[nb, {TaggingRules, "Storage",
                               name <> "expression"}] == expr],
      var = Uncompress@CurrentValue[nb, {TaggingRules, "Storage", name<>"value"}],
      CurrentValue[nb, {TaggingRules, "Storage", name<>"expression"}] = expr;
      CurrentValue[nb,{TaggingRules, "Storage", name<>"value"}] =
        Compress[var=value]];
    var];  
  

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


ClearAll[symbolicHead];
SetAttributes[symbolicHead, HoldAllComplete];
symbolicHead[f_Symbol[___]] := f;
symbolicHead[f_[___]] := symbolicHead[f];
symbolicHead[f_] := Head[Unevaluated[f]];

ClearAll[valueQ];
SetAttributes[valueQ, HoldAllComplete];
valueQ[a_Symbol] /; OwnValues[a] =!= {} := 
  With[{result = (# =!= (# /. OwnValues[a])) &[HoldComplete[a]]}, 
   result /; result];

valueQ[a : f_Symbol[___]] /; DownValues[f] =!= {} := 
  With[{result = (# =!= (# /. DownValues[f])) &@HoldComplete[a]}, 
   result /; result];

valueQ[a_] := 
  With[{sub = SubValues[Evaluate[symbolicHead[a]]]}, 
   With[{result = (# =!= (# /. sub)) &[HoldComplete[a]]}, 
     result /; result] /; sub =!= {}];

valueQ[a_] := 
  With[{upsyms = 
     Flatten@Cases[Unevaluated[a], s_Symbol :> UpValues[s], 1, 
       Heads -> True]}, 
   With[{result = (# =!= (# /. upsyms)) &[HoldComplete[a]]}, 
     result /; result] /; upsyms =!= {}];

valueQ[_] := False;

LoadData::nofile="Filename `1` does not exist";

Options[LoadData] = {"Type"->"Table",
                     "Stride"->1, 
                     "FirstElement"->1, 
                     "LastElement"->-1,
                     "PackArray"->True, (*Should try ro return packed array*)
                     "AutoStrideIfMoreThan"->Null,  
                     "DataHeader"->Null,
                     "Verbose":>$VerbosePrint,  (*Log output*)
                     "VerboseLevel":>$VerboseLevel};
   
LoadData[afilename_,opt:OptionsPattern[]] :=
    Module[{filename=afilename,data,stride},
        
        Puts["Loading data from file ",filename];
        
        (* If file does not exist try using / as \\ *)
        If[!FileExistsQ@filename,
            filename=StringReplace[filename,"/"->"\\"];
            Puts["Trying with name by replacing / to \\ ",filename,LogLevel->2];
           ];

        If[!FileExistsQ@filename, 
            Message[LoadData::nofile,filename];
            Return[$Failed];
            ]; 
        
        If[ StringMatchQ[OptionValue@"Type","Real*"],(*then*)
            data = Import[filename,OptionValue@"Type"]; 
            (*Partition the data according to the header, since binary data is not partitioned*)
            If[ OptionValue@"DataHeader"=!=Null, (*then*)
                data = Partition[data,Length[OptionValue@"DataHeader"]]
            ];
         ,(*else*)
            
            data = N@Import[filename,OptionValue@"Type", "IgnoreEmptyLines"->True, "Numeric"->True];
            
            (*Take everything that has a numeric first part*)
            
            data = Select[data,NumericQ[#[[1]]]&];
            
            
        ];
        
        If[OptionValue@"PackArray",
            data = Developer`ToPackedArray@data;
          ];
        (*Print["First ", OptionValue[FirstElement]," Last ", OptionValue[LastElement]," Stride ", OptionValue[Stride]];*)
        If[ OptionValue@"AutoStrideIfMoreThan"===Null, (*then*)
            (*If there are default firt,last,stride, do nothing*)
            If[!(OptionValue@"FirstElement"==1 && OptionValue@"LastElement"==-1 && OptionValue@"Stride"==1),
                data = data[[OptionValue@"FirstElement" ;; OptionValue@"LastElement" ;; OptionValue@"Stride"]];];
        ,(*else*)        
        (*Make sure we return approx AutoStrideIfMoreThan points*)
            data = data[[OptionValue@"FirstElement"  ;; OptionValue@"LastElement"]];
            If[ Length[data]>OptionValue"AutoStrideIfMoreThan",(*then*)
                stride = Round[Length[data]/OptionValue"AutoStrideIfMoreThan"];
                Puts["Autostride is ",stride,LogLevel->2];
                data = data[[1;;-1;;stride]];
            ];
        ];
        Puts["Loaded ",Length[data]," from ", filename];
        Return[data];
    ];
 
Clear[LoadMultipleData];
Options[LoadMultipleData] = {"Type"->"Table","Stride"->1};

LoadMultipleData[filenames_,opt:OptionsPattern[]] :=
    Module[ {},
        Flatten[LoadData[#,Type->OptionValue[Type]]&/@files,1][[;; ;; OptionValue[Stride]]]
    ];


Options[LoadHeader] = {"CommentChar"->"#","Separator"->WhitespaceCharacter..};

LoadHeader[filename_,opt:OptionsPattern[]] :=
    (*Loads the first line, ignoring any present Comments Char *)
        StringTrim[
         DeleteCases[
            StringSplit[
              Import[filename,{"Lines",1}],
              OptionValue@"Separator"
            ]
          ,OptionValue@"CommentChar"]  
        ];

Clear[TakeData];
Options[TakeData] = {};

TakeData[data_,header_,take_List,opt:OptionsPattern[]] :=
    Module[ {ind,takestr},
        takestr = ToString[#]&/@take;
        (*Print[FullForm[takestr]];*)
        ind = Flatten[(Position[header,#])&/@takestr];
        (*Print[FullForm[ind]];
        Print[data[[;;10,ind]]];*)
        Return[data[[All,ind]]]
    ];


End[]

EndPackage[]






