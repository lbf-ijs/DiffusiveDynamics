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

OPTIONS:
\"InitialPosition\"->{0.,0.}  -- The starting point for the trajectory,
\"Verbose\"->False            -- Log output
\"VerboseLevel\"->3           -- Verbosity of the output  
";

ClearAll[ParallelGenerateDiffusionTrajectory2D];
ParallelGenerateDiffusionTrajectory2D::usage="ParallelGenerateDiffusionTrajectory2D[steps, DiffX, DiffY, DiffA, F, kT, dt, {min,max},opt]
Generates a 2D diffusive process where the difusion tensor and energy optionally depend on the location of the particle. 
Uses \"ProcessCount\" threads to generate  \"ProcessCount\" independent trajectories.

See GenerateDiffusionTrajectory2D for further info.

OPTIONS:
\"ProcessCount\"-> 2$ProcessorCount -- Number of independent difussion processe that get started in seperate threads
\"InitialPositions\"->Automatic        -- takes a list of points or the default, Automatic, which generates uniformly randommly distributed points in the cell
"; 


Begin["`Private`"]
(* Implementation of the package *)



Options[GenerateDiffusionTrajectory2D] = {"InitialPosition"->{0.,0.} (*The starting point for the trajectory*),
	                                      "Verbose":>$VerbosePrint, (*Log output*)
	                                      "VerboseLevel":>$VerboseLevel  (*Verbosity of the output. Automatic is $VerboseLevel*)
	                                      };

GenerateDiffusionTrajectory2D[steps_Integer, DiffX_, DiffY_, DiffA_, F_, kT_?NumberQ, dt_?NumberQ, 
	                         {min_,max_},opt:OptionsPattern[]] :=
Block[{$VerbosePrint=OptionValue["Verbose"], $VerboseLevel=OptionValue["VerboseLevel"],$VerboseIndentLevel=$VerboseIndentLevel+1},   
	Module[ {data(*data for the diffusive process trajectory *),
    	     g (*steps*2 array of gausian random values*), width,halfwidth (*cell info*),
    	     DD (*2D diagonal difusion tensor (DiffX, DiffY)*),
    	     RM (*rotation matrix*),
    	     RotatedDD (*DD rotated by DiffA*) ,
    	     gradD (*gradient of the rotated difusion tensor*),
    	     gradF (*gradient of the free energy*), 
    	     stepfunctionexp(*the step function expression*),
    	     stepfunction (*the compiled step function*),
    	     x,y,g1,g2 (*temporary variables used in compilation of the step function*)},
     
        Puts["***GenerateDiffusionTrajectory2D***"];
        PutsOptions[GenerateDiffusionTrajectory2D,{opt}];
        
        width = (max-min);
        halfwidth = width/2;
        Quiet[(*The message CompiledFunction::cfsa is generated, because we put  
               symbolic arguuments into compile, when inlining.
               But it is harmeless, so supress it.*)
        DD = Compile[{x,y}, Evaluate[
           {{DiffX[x,y],0},{0,DiffY[x,y]}}
          ]];
        Puts["\nDD:\n",DD, LogLevel->5];
        Puts["Nonrotated difusion tensor at {0,0}: ",DD[0,0],LogLevel->4];

        RM = Compile[{x,y} , RotMatrix2D[DiffA[x,y]]];
        RotatedDD = Compile[{x,y}, Evaluate[
           RM[x,y].DD[x,y].Transpose[RM[x,y]]
           ]];
        Puts["\nRotatedDD:\n",RotatedDD,LogLevel->5];
        Puts["Difusion tensor at {0,0}: ",RotatedDD[0,0],LogLevel->4];
        
		gradD = Compile[{x,y},Evaluate[
			GradMatrix2D[RotatedDD[x,y],x,y]
			]];
		Puts["\ngradD:\n",gradD,LogLevel->5];
		Puts["gradient of D tensor at {0,0}: ",gradD[0.,0.],LogLevel->4];
		
        gradF = Compile[{x,y},Evaluate[
        	D[F[x,y],{{x,y}}]
        	]];
		Puts["\ngradF:\n",gradF,LogLevel->5];
		Puts["gradient of free energy at {0,0}: ",gradF[0.,0.],LogLevel->4];
  
        (*This is the main equation used to generate the trajectory for the diffusive process*)
        (*Refrence: http://iopscience.iop.org/1367-2630/7/1/034 page 7. I did the generalization to 2D myself*)
        stepfunctionexp = 
           ({x,y} + (gradD[x,y]-RotatedDD[x,y].gradF[x,y]/kT) dt +
              RM[x,y].Sqrt[2 DD[x,y] dt].Transpose[RM[x,y]].{g1,g2});
        (*Danger!! Simplifinng the above expression (in order to get better performance) can be dangerous. As a concrete example, in some combination of difussion tensor it introdudes Csc[], which has an infinity at 0.*)        
        (*Ensure periodic conditions after every step *)
        stepfunctionexp[[1]] = Mod[stepfunctionexp[[1]]+halfwidth[[1]] ,width[[1]]]-halfwidth[[1]];
        stepfunctionexp[[2]] = Mod[stepfunctionexp[[2]]+halfwidth[[2]] ,width[[2]]]-halfwidth[[2]];
   		
   		(*Block is used to inject the xv and rv parameters into x,y and g1mg2 of stepfunctionexp espression*) 
        (* the outer block is just for syntax highlighting in WB*)
        stepfunction = Block[{xv,rv}, 
        	           Compile[{{xv,_Real,1},{rv,_Real,1}}, Block[ {x = xv[[1]],y = xv[[2]],g1 = rv[[1]],g2 = rv[[2]]},
                                     stepfunctionexp
                       ],         
            (*CompilationTarget->C, Does not give more tha 2x speed imporvment for 10^7 points, so compilation to C does not pay off*)
            CompilationOptions->{"ExpressionOptimization"->True,"InlineCompiledFunctions"->True,"InlineExternalDefinitions"->True},
            "RuntimeOptions" -> "Speed"
            ]];
            
        Puts["\nstepfunction:\n",stepfunction, LogLevel->5];
        Puts["stepfunction at {0,0} and g {1.,10.}: ",stepfunction[{0.,0.},{10.,1.}],LogLevel->4];
            
	   ,{CompiledFunction::cfsa}];
		
		(*generate an stepsx2 list of normally distributed random values*)
		g = RandomVariate[NormalDistribution[0,1],{steps,2}];        
		(*Calculate the actual diffusive process. 
		 FoldList as #1 passes the previous point and as #2 the correct value from the list of random numbers *)
        data = FoldList[(stepfunction[#1,#2])&, N@OptionValue["InitialPosition"], g];

        (*Add steps number. So the final form will be {{1.,x1,y1}, {2.,x1,y1}, ...}*)
        (*TODO: Perhaps make this optional?*)                 
        Transpose[Prepend[Transpose[data],N@Range[Length[data]] ]]
        (*The last expression is automatically returned -- notice no ; at the end*)
    ](*Module*)
](*Block*);

ClearAll[ParallelGenerateDiffusionTrajectory2D];
Options[ParallelGenerateDiffusionTrajectory2D] = {"ProcessCount":> 2$ProcessorCount, (*Number of independent difussion processe that get started*)
                                                  "InitialPositions"->Automatic  (*takes a list of points or the default, Random, which generates uniformly randommly distributed points*)
                                                  }~Join~Options[GenerateDiffusionTrajectory2D] ;

ParallelGenerateDiffusionTrajectory2D[steps_Integer,DiffX_,DiffY_,DiffA_,F_,kT_?NumberQ,dt_?NumberQ,{min_,max_},opt:OptionsPattern[]] :=
Block[{$VerbosePrint=OptionValue["Verbose"], $VerboseLevel=OptionValue["VerboseLevel"],$VerboseIndentLevel=$VerboseIndentLevel+1},   
    Module[ {initialPoints,numPoints,stepsPerPoint},
        Puts["***ParallelGenerateDiffusionTrajectory2D***"];
        PutsOptions[ParallelGenerateDiffusionTrajectory2D,{opt}];
        
        numPoints = OptionValue["ProcessCount"];
        stepsPerPoint = Round[steps/numPoints];
        initialPoints=OptionValue["InitialPositions"];
        (*Generate initial points*)
        If[ initialPoints===Automatic, (*then*)
           initialPoints = Transpose[{RandomReal[{min[[1]],max[[1]]},numPoints],
           	                          RandomReal[{min[[2]],max[[2]]},numPoints]}];,
           (*else*)
           (*if there are to few points add them*)
           If[ Length@initialPoints<numPoints,
               initialPoints=initialPoints~Join~
                     Transpose[{RandomReal[{min[[1]],max[[1]]},numPoints-Length@initialPoints],
                     	        RandomReal[{min[[2]],max[[2]]},numPoints-Length@initialPoints]}];
               ];
          (*if there are too many truncate. TODO: add warnnings? add tests for this*)
           If[ Length@initialPoints>numPoints,
               initialPoints = initialPoints[[;;numPoints]]
           ];
       ];
        PutsE["InitialPoints (recalc):\n",initialPoints,LogLevel->2];


		(*Inject the values into ParallelMap using With, so they don't have to be distribuated*)
		With[ {istepsPerPoint = stepsPerPoint, iDiffX = DiffX, iDiffY = DiffY ,iDiffA = DiffA, iF = F,
		       ikT = kT,idt = dt,imin = min, imax = max, iOptions=FilterRules[{opt},Options@GenerateDiffusionTrajectory2D]},
            ParallelMap[
              GenerateDiffusionTrajectory2D[istepsPerPoint,iDiffX,iDiffY,iDiffA,iF,ikT,idt,{imin,imax}, 
                                         "InitialPosition"->#,iOptions]&,
            initialPoints]]
    ] 
];
Clear[RotMatrix2D];
(*Returns a 2D rotation matrix. Angle given in degrees*) 
RotMatrix2D = Compile[{a},{{Cos[a*Degree],-Sin[a*Degree]},{Sin[a*Degree],Cos[a*Degree]}}];
Clear[GradMatrix2D];
GradMatrix2D::usage="GradMatrix2D[mat_,x_,y_] returns the gradient derivative of the matrix 

mat -- the matrix (a function of x and y)
x,y -- the symbolic coordiantes
"
GradMatrix2D[mat_,x_,y_] :=
	{D[mat[[1,1]],x]+D[mat[[1,2]],y],D[mat[[2,2]],y]+D[mat[[2,1]],x]};


End[]

EndPackage[]

