(* Mathematica Package *)
(* Author: Ajasja Ljubetic (ajasja.ljubetic@gmail.com)
   
   A package for generating position dependent 2D diffusive processes *)

BeginPackage["DiffusiveDynamics`Generate2D`",{"DiffusiveDynamics`Utils`"}]
(* Exported symbols added here with SymbolName::usage *) 

ClearAll[GenerateDiffusionTrajectory2D];
GenerateDiffusionTrajectory2D::usage="GenerateDiffusionTrajectory2D[steps, DiffX, DiffY, DiffA, F, kT, dt, {min,max},opt]
Generates a 2D diffusive process where the difusion tensor and energy optionally depend on the location of the particle.

steps      -- number of steps to perform
DiffX[x,y] -- a Function or CompiledFunction giving the difusion in the X and Y principal direction
DiffY[x,y] -- as above for the Y direction
DiffA[x,y] -- a Function or CompiledFunction giving the rotation of the principal direction
F[x,y]     -- Free Energy as a function of position (must be either a Function or CompiledFunction)
kT         -- set the temparature in units of kT (thermal energy = boltzman konstant* temperature
dt         -- the time size of one step
min        -- the minimum sizes of the periodic cell {minX, minY}
max        -- the maximum sizes of the periodic cell {maxX, maxY}

The equation used is: TODO
";

ClearAll[ParallelGenerateDiffusionTrajectory2D];
ParallelGenerateDiffusionTrajectory2D::usage="USE"; 


Begin["`Private`"]
(* Implementation of the package *)



Options[GenerateDiffusionTrajectory2D] = {"InitialPosition"->{0.,0.} (*The starting point for the trajectory*),
	                                      "Verbose"->False (*Log output*)};

GenerateDiffusionTrajectory2D[steps_Integer, DiffX_, DiffY_, DiffA_, F_, kT_?NumberQ, dt_?NumberQ, 
	                         {min_,max_},opt:OptionsPattern[]] :=
Block[{$VerbosePrint=$VerbosePrint||OptionValue["Verbose"]},   
    Module[ {data,g,width,halfwidth,gradD,gradF,stepfunction,compOpts,DD,DIFF,RM,stepfunctionexp,x,y,g1,g2},
        Off[CompiledFunction::cfsa];
        Puts["*****GenerateDiffusionTrajectory2D*****"];
        Puts["Initial Position: ",OptionValue["InitialPosition"]];
        
        width = (max-min);
        halfwidth = width/2;
        compOpts = {CompilationOptions->{"ExpressionOptimization"->True,"InlineCompiledFunctions"->True,"InlineExternalDefinitions"->True}};
        DD = Compile[{x,y}, Evaluate[
          {{DiffX[x,y],0},{0,DiffY[x,y]}}
        ]];
        Puts["\nDD:\n",DD,DisplayFunction->Identity];
        Puts[DD[0,0]];
        RM = Compile[{x,y} ,
             RotMatrix2D[DiffA[x,y]]   
           ];
        DIFF = Compile[{x,y}, Evaluate[
         RM[x,y].DD[x,y].Transpose[RM[x,y]]
        ]];
        Puts["\nDIFF:\n",DIFF,DisplayFunction->Identity];
        Puts[DIFF[0,0]];
        
        Quiet[
          gradD = GradMatrix2D[DIFF[x,y],x,y];
          Puts["gradD not compiled:\n",gradD,DisplayFunction->Identity];
          gradD = Compile[{x,y},Evaluate[
            GradMatrix2D[DIFF[x,y],x,y]
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
        stepfunctionexp = (*Simplify[*)
           ({x,y} + (gradD[x,y]-DIFF[x,y].gradF[x,y]/kT) dt +
              RM[x,y].Sqrt[2 DD[x,y] dt].Transpose[RM[x,y]].{g1,g2});
        (*,TimeConstraint->1];*)
        (*Ensure periodic conditions after every step *)
        (*x*)
        stepfunctionexp[[1]] = Mod[stepfunctionexp[[1]]+halfwidth[[1]] ,width[[1]]]-halfwidth[[1]];
   		(*y*)
        stepfunctionexp[[2]] = Mod[stepfunctionexp[[2]]+halfwidth[[2]] ,width[[2]]]-halfwidth[[2]];

   		
        g = RandomVariate[NormalDistribution[0,1],{steps,2}];
        stepfunction = Compile[{{xv,_Real,1},{rv,_Real,1}}, Block[ {x = xv[[1]],y = xv[[2]],g1 = rv[[1]],g2 = rv[[2]]},
                                                                stepfunctionexp
                                                            ],
            CompilationOptions->{"ExpressionOptimization"->True,"InlineCompiledFunctions"->True,"InlineExternalDefinitions"->True},
            "RuntimeOptions" -> "Speed"
            ];

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
        data = FoldList[(stepfunction[#1,#2])&, N@OptionValue["InitialPosition"], g];




        (*Periodic conditions
        (*x*)data[[All,1]]=Mod[data[[All,1]]+halfwidth[[1]] ,width[[1]]]-halfwidth[[1]];
        (*y*)data[[All,2]]=Mod[data[[All,2]]+halfwidth[[2]] ,width[[2]]]-halfwidth[[2]];*)

        (*Add steps number*)
        (*data=ToPackedArray[Transpose[{Range[Length[data]],data}],Real];*)
        data =(*ToPackedArray[*)
        Transpose[Prepend[Transpose[data],N@Range[Length[data]] ]](*,Real]*);
        Return[data]
    ]
];

ClearAll[ParallelGenerateDiffusionTrajectory2D];
Options[ParallelGenerateDiffusionTrajectory2D] = {"ThreadCount"-> 2$ProcessorCount,
                                      "InitialPositions"->Random  (*takes a list of points or the default, Random, which generates uniformly randommly distributed points*)};
ParallelGenerateDiffusionTrajectory2D[steps_Integer,DiffX_,DiffY_,\[Alpha]_,F_,kT_?NumberQ,dt_?NumberQ,{min_,max_},opt:OptionsPattern[]] :=
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
        DistributeDefinitions[GenerateDiffusionTrajectory2D,RotMatrix2D,GradMatrix2D];
        rw = ParallelMap[
          (Developer`ToPackedArray[GenerateDiffusionTrajectory2D[stepsPerPoint,DiffX,DiffY,\[Alpha],F,kT,dt,{min,max}, "InitialPosition"->#],Real])&,
        initialPoints];
        Return[rw];
    ];

Clear[RotMatrix2D];
(*Returns a 2D rotation matrix. Angle given in degrees*) 
RotMatrix2D = Compile[{\[Alpha]},{{Cos[\[Alpha]*\[Degree]],-Sin[\[Alpha]*\[Degree]]},{Sin[\[Alpha]*\[Degree]],Cos[\[Alpha]*\[Degree]]}}];
Clear[GradMatrix2D];
GradMatrix2D::usage="GradMatrix2D[mat_,x_,y_] returns the gradient derivative of the matrix 

mat -- the matrix (a function of x and y)
x,y -- the symbolic coordiantes
"
GradMatrix2D[mat_,x_,y_] :=
	{D[mat[[1,1]],x]+D[mat[[1,2]],y],D[mat[[2,2]],y]+D[mat[[2,1]],x]};


End[]

EndPackage[]

