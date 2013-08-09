(* Mathematica Package *)
(* Author: Ajasja Ljubetic (ajasja.ljubetic@gmail.com)
   
   A package for generating position dependent 1D diffusive (Brownian) processes *)

BeginPackage["DiffusiveDynamics`Generate1D`",{"DiffusiveDynamics`Utils`"}]
(* Exported symbols added here with SymbolName::usage *) 

ClearAll[GenerateDiffusionTrajectory1D];
GenerateDiffusionTrajectory1D::usage="GenerateDiffusionTrajectory1D[steps, DiffX, DiffY, DiffA, F, kT, dt, {min,max},opt]
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

ClearAll[ParallelGenerateDiffusionTrajectory1D];
ParallelGenerateDiffusionTrajectory1D::usage="ParallelGenerateDiffusionTrajectory1D[steps, DiffX, DiffY, DiffA, F, kT, dt, {min,max},opt]
Generates a 2D diffusive process where the difusion tensor and energy optionally depend on the location of the particle. 
Uses \"ProcessCount\" threads to generate  \"ProcessCount\" independent trajectories.

See GenerateDiffusionTrajectory1D for further info.

OPTIONS:
\"ProcessCount\"-> 2$ProcessorCount -- Number of independent difussion processe that get started in seperate threads
\"InitialPositions\"->Automatic     -- takes a list of points or the default, Automatic, which generates uniformly randommly distributed points in the cell
\"RandomSeeds\"->Automatic          -- takes a list of random seeds used to generate each trajectory.
"; 



Begin["`Private`"]
(* Implementation of the package *)



Options[GenerateDiffusionTrajectory1D] = {"RandomSeed"->Automatic,
                                          "InitialPosition"->0. (*The starting point for the trajectory*),
	                                      "Verbose":>$VerbosePrint, (*Log output*)
	                                      "VerboseLevel":>$VerboseLevel  (*Verbosity of the output. Automatic is $VerboseLevel*)
	                                      };

GenerateDiffusionTrajectory1D[steps_Integer, Diff_, F_, kT_?NumberQ, dt_?NumberQ, 
	                         {min_?NumberQ,max_?NumberQ},opt:OptionsPattern[]] :=
Block[{$VerbosePrint=OptionValue["Verbose"], $VerboseLevel=OptionValue["VerboseLevel"],$VerboseIndentLevel=$VerboseIndentLevel+1},   
    Module[{data (*the trajectory data*) 
           ,g (*random numbers array*) 
           ,halfwidth,width (*periodic cell half width and width*) 
           ,randomSeed
           ,stepfunctionexp (*Symbolic step function expression*)
           ,stepfunction (*compiled step function*)
           ,xv,rv (*step function dummy arguments*)},
    	     
     
        Puts["***GenerateDiffusionTrajectory1D***", LogLevel->1];
        PutsOptions[GenerateDiffusionTrajectory1D,{opt},LogLevel->2];
        randomSeed=If[#===Automatic,RandomInteger[1000000],#]&@OptionValue@"RandomSeed";
        
        width = (max-min);
        halfwidth = width/2.;
        
       Quiet[ (*Because we are doing symboilc derivatves on compiled functions we get some warnings.*) 
        
        (*This is the main equation used to generate the trajectory for the diffusive process*)
        (*Refrence: http://iopscience.iop.org/1367-2630/7/1/034 page 7 and the original refrence by McCannon. *)
        stepfunctionexp = (xv + (Diff'[xv] - F'[xv] Diff[xv]/kT) dt + rv*Sqrt[2 Diff[xv] dt]);
        (*Danger!! Simplifinng the above expression (in order to get better performance) can be dangerous. As a concrete example, in some combination of difussion tensor it introdudes Csc[], which has an infinity at 0.*)        
        (*Ensure periodic conditions after every step *)
        stepfunctionexp = Mod[stepfunctionexp+halfwidth ,width]-halfwidth;
        Puts["Symbolic stepfunction:\n",stepfunctionexp, LogLevel->5];
        
        
        

        stepfunction =  Compile[{{xv,_Real},{rv,_Real}},
                         stepfunctionexp                                           
            (*CompilationTarget->C, Does not give more tha 2x speed imporvment for 10^7 points, so compilation to C does not pay off*)
            ,CompilationOptions->{"ExpressionOptimization"->True,"InlineCompiledFunctions"->True,"InlineExternalDefinitions"->True}
            ,"RuntimeOptions" -> "Speed"
            ];
        ,{CompiledFunction::cfsa}];
        Puts["stepfunction:\n",stepfunction, LogLevel->5];
        Puts["stepfunction at 0 and g 1: ",stepfunction[0,1],LogLevel->4];

        
        (*Get block of random data*)
        BlockRandom[
            SeedRandom[randomSeed,Method -> "MersenneTwister"];             
            g = RandomVariate[NormalDistribution[0, 1], steps]; 
            Puts["Random Seed:", randomSeed, "(FirstStep ",First@g, ")", LogLevel->3];   
        ];(*BlockRandom*)      
        
          data = FoldList[stepfunction, OptionValue["InitialPosition"], g];
        (*Periodic conditions*)
        data = Mod[data + halfwidth, max - min] - halfwidth;
        (*Add steps number*)
        data = Transpose[{N@Range[Length[data]], data}];
        Return[data];
    ](*Module*)
](*Block*);



ClearAll[ParallelGenerateDiffusionTrajectory1D];
Options[ParallelGenerateDiffusionTrajectory1D] = {"ProcessCount":> 2$ProcessorCount, (*Number of independent difussion processe that get started*)
                                                  "InitialPositions"->Automatic,  (*takes a list of points or the default, Random, which generates uniformly randommly distributed points*)
                                                  "RandomSeeds"->Automatic (*A list of random seeds for each trajectory*)
                                                  }~Join~Options[GenerateDiffusionTrajectory1D] ;

ParallelGenerateDiffusionTrajectory1D[steps_Integer, Diff_, F_, kT_?NumberQ, dt_?NumberQ, 
                             {min_?NumberQ,max_?NumberQ},opt:OptionsPattern[]] :=
Block[{$VerbosePrint=OptionValue["Verbose"], $VerboseLevel=OptionValue["VerboseLevel"],$VerboseIndentLevel=$VerboseIndentLevel+1},   
    Module[ {initialPoints,numPoints,stepsPerPoint,randomSeeds},
        Puts["***ParallelGenerateDiffusionTrajectory1D***",LogLevel->1];
        PutsOptions[ParallelGenerateDiffusionTrajectory1D,{opt},LogLevel->2];
        
        numPoints = OptionValue["ProcessCount"];
        stepsPerPoint = Round[steps/numPoints];
        initialPoints=OptionValue["InitialPositions"];
        (*Generate initial points*)
        If[ initialPoints===Automatic, (*then*)
           initialPoints = RandomReal[{min,max},numPoints];
           	                          
           ,(*else*)
           (*if there are to few points add them*)
           If[ Length@initialPoints<numPoints,
               initialPoints=initialPoints~Join~
                    RandomReal[{min,max},numPoints-Length@initialPoints];
               ];
          (*if there are too many truncate. TODO: add warnnings? add tests for this*)
           If[ Length@initialPoints>numPoints,
               initialPoints = initialPoints[[;;numPoints]]
           ];
       ];
       
        PutsE["InitialPoints (recalc):\n",initialPoints,LogLevel->2];
        randomSeeds=If[#===Automatic,
                        RandomInteger[10000000,Length@initialPoints]
                    ,(*else*)    
                        #]&
                    @OptionValue@"RandomSeeds";
        PutsE["randomSeeds:\n",randomSeeds,LogLevel->2];                    
        Assert[Length@randomSeeds==Length@initialPoints,"Initial points and random seeds must be of the same length"];
		(*Inject the values into ParallelMap using With, so they don't have to be distributed*)
		With[ {istepsPerPoint = stepsPerPoint, iDiff = Diff, iF = F,
		       ikT = kT,idt = dt,imin = min, imax = max, iOptions=FilterRules[{opt},Options@GenerateDiffusionTrajectory1D]},
            Map[
              GenerateDiffusionTrajectory1D[istepsPerPoint,iDiff,iF,ikT,idt,{imin,imax}, 
                                         "InitialPosition"->#[[1]],"RandomSeed"->#[[2]],iOptions]&,
            Transpose@{initialPoints,randomSeeds}]]
    ] 
];

End[]

EndPackage[]

