(* Mathematica Package *)
(* Author: Ajasja Ljubetic (ajasja.ljubetic@gmail.com)
   
   A package for generating position dependent 2D diffusive processes *)

BeginPackage["DiffusiveDynamics`"]
(* Exported symbols added here with SymbolName::usage *) 



Begin["`Private`"]
(* Implementation of the package *)

ClearAll[RandomWalk2DAnizo];
Options[RandomWalk2DAnizo] = {"InitialPosition"->{0.,0.}};
RandomWalk2DAnizo[steps_Integer,DiffX_,DiffY_,\[Alpha]_,F_,kT_?NumberQ,dt_?NumberQ,{min_,max_},opt:OptionsPattern[]] :=
    Module[ {data,g,width,halfwidth,gradD,gradF,stepfunction,compOpts,DD,DIFF,RM,stepfunctionexp,x,y,g1,g2},
        Off[CompiledFunction::cfsa];
        Puts["Initial Position: ",OptionValue["InitialPosition"]];

        (*generate random numbers*)
        width = (max-min);
        halfwidth = width/2;
        compOpts = {CompilationOptions->{"ExpressionOptimization"->True,"InlineCompiledFunctions"->True,"InlineExternalDefinitions"->True},
        "RuntimeOptions"->"Speed"};
        DD = Compile[{x,y}, Evaluate[
          {{DiffX[x,y],0},{0,DiffY[x,y]}}
        ]];
        Puts["\nDD:\n",DD,DisplayFunction->Identity];
        Puts[DD[0,0]];
        RM = Compile[{x,y} ,
             RotMatrix2D[\[Alpha][x,y]]   
           ];
        DIFF = Compile[{x,y}, Evaluate[
         RM[x,y].DD[x,y].Transpose[RM[x,y]]
        ]];
        Puts["\nDIFF:\n",DIFF,DisplayFunction->Identity];
        Puts[DIFF[0,0]];
        g = RandomVariate[NormalDistribution[0,1],{steps,2}];
        Quiet[
          gradD = Grad2DMatrix[DIFF[x,y],x,y];
          Puts["gradD not compiled:\n",gradD,DisplayFunction->Identity];
          gradD = Compile[{x,y},Evaluate[
            Grad2DMatrix[DIFF[x,y],x,y]
          ]];
          Puts["\ngradD:\n",gradD,DisplayFunction->Identity];
          Puts[gradD[0.,0.]]; 

        (*  gradF=FullSimplify[D[F[x,y],{{x,y}}]];
          
          Puts["gradF not compiled:\n",gradF,DisplayFunction->Identity];*)
          gradF = Compile[{x,y},Evaluate[
            D[F[x,y],{{x,y}}]
          ]];
          Puts["\ngradF:\n",gradF,DisplayFunction->Identity];
          Puts[gradF[0.,0.]];
        
        ,{CompiledFunction::cfsa}];
        stepfunctionexp = Simplify[
           ({x,y} + (gradD[x,y]-DIFF[x,y].gradF[x,y]/kT) dt +
              RM[x,y].Sqrt[2 DD[x,y] dt].Transpose[RM[x,y]].{g1,g2})
        ,TimeConstraint->1];
        (*Ensure periodic conditions after every step *)
        (*x*)
        stepfunctionexp[[1]] = Mod[stepfunctionexp[[1]]+halfwidth[[1]] ,width[[1]]]-halfwidth[[1]];
   		(*y*)
        stepfunctionexp[[2]] = Mod[stepfunctionexp[[2]]+halfwidth[[2]] ,width[[2]]]-halfwidth[[2]];

   		(*stepfunctionexp=Experimental`OptimizeExpression[#]&/@stepfunctionexp;*)
        stepfunction = Compile[{{xv,_Real,1},{rv,_Real,1}}, Block[ {x = xv[[1]],y = xv[[2]],g1 = rv[[1]],g2 = rv[[2]]},
                                                                stepfunctionexp
                                                            ],
            CompilationOptions->{"ExpressionOptimization"->True,"InlineCompiledFunctions"->True,"InlineExternalDefinitions"->True},
            "RuntimeOptions"->"Speed"];

(*        <<CompiledFunctionTools`;
        Print["stepfunction:\n",CompiledFunctionTools`CompilePrint[stepfunction]];*)

        (*stepfunction=Compile[{{xv,_Real,1},{rv,_Real,1}}, With[{x=xv[[1]],y=xv[[2]]},
          (xv + (gradD[x,y]-DIFF[x,y].gradF[x,y]/kT) dt +
              RM[x,y].Sqrt[2 DD[x,y] dt].Transpose[RM[x,y]].rv)
        ]];*)


        (*stepfunction=Compile[{{xv,_Real,1},{rv,_Real,1}}, Evaluate[
         
          (xv + (gradD[xv[[1]],xv[[2]]]-DIFF[xv[[1]],xv[[2]]].gradF[xv[[1]],xv[[2]]]/kT) dt +
              RM[xv[[1]],xv[[2]]].Sqrt[2 DD[xv[[1]],xv[[2]]] dt].Transpose[RM[xv[[1]],xv[[2]]]].rv)
        ]];*)
        Puts["\nstepfunction:\n",stepfunction,DisplayFunction->Identity];
        Puts[stepfunction[{0.,0.},{10.,1.}]];

        (*Puts["InitialPosition ",OptionValue["InitialPosition"]];*)
        data = FoldList[(stepfunction[#1,#2])&, OptionValue["InitialPosition"], g];




        (*Periodic conditions
        (*x*)data[[All,1]]=Mod[data[[All,1]]+halfwidth[[1]] ,width[[1]]]-halfwidth[[1]];
        (*y*)data[[All,2]]=Mod[data[[All,2]]+halfwidth[[2]] ,width[[2]]]-halfwidth[[2]];*)

        (*Add steps number*)
        (*data=ToPackedArray[Transpose[{Range[Length[data]],data}],Real];*)
        data =(*ToPackedArray[*)
        Transpose[Prepend[Transpose[data],Range[Length[data]] ]](*,Real]*);
        Return[data]
    ];
ClearAll[ParallelRandomWalk2DAnizo];
Options[ParallelRandomWalk2DAnizo] = {"ThreadCount"-> 2$ProcessorCount,
                                      "InitialPositions"->Random  (*takes a list of points or the default, Random, which generates uniformly randommly distributed points*)};
ParallelRandomWalk2DAnizo[steps_Integer,DiffX_,DiffY_,\[Alpha]_,F_,kT_?NumberQ,dt_?NumberQ,{min_,max_},opt:OptionsPattern[]] :=
    Module[ {rw, initialPoints,numPoints,stepsPerPoint},
(*Generate intial points*)
        numPoints = OptionValue["ThreadCount"];
        stepsPerPoint = Round[steps/numPoints];
        initialPoints = If[ OptionValue["InitialPositions"]===Random, (*then*)
                            Transpose[{RandomReal[{min[[1]],max[[1]]},numPoints],RandomReal[{min[[2]],max[[2]]},numPoints]}],
                          (*else*)
                            OptionValue["InitialPositions"]
                        ];
        Puts["InitialPoints:\n",initialPoints];
        DistributeDefinitions[RandomWalk2DAnizo,RotMatrix2D,Grad2DMatrix];
        rw = ParallelMap[
          (Developer`ToPackedArray[RandomWalk2DAnizo[stepsPerPoint,DiffX,DiffY,\[Alpha],F,kT,dt,{min,max}, "InitialPosition"->#],Real])&,
        initialPoints];
        Return[rw];
    ];

End[]

EndPackage[]

