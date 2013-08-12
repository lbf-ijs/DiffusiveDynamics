(* Mathematica Init File *)
 
(*While developing it's convinietn to force packages to reload *)
Get["DiffusiveDynamics`Utils`"];
Get["DiffusiveDynamics`Generate2D`"];
Get["DiffusiveDynamics`Analyze2D`"];
Get["DiffusiveDynamics`Visualize2D`"];
Get["DiffusiveDynamics`Manipulate2D`"];

Get["DiffusiveDynamics`Generate1D`"];
Get["DiffusiveDynamics`Analyze1D`"];
Print@"Reloaded DiffusiveDynamics`!";
If[ ($KernelID==0) && ($KernelCount>0), (*if this is the master kernel and some subkernels have been lauched*)
ParallelEvaluate[    
    Get["DiffusiveDynamics`Utils`"];
    Get["DiffusiveDynamics`Generate2D`"];
    Get["DiffusiveDynamics`Analyze2D`"];
    Get["DiffusiveDynamics`Visualize2D`"];
    Get["DiffusiveDynamics`Manipulate2D`"];
    
    Get["DiffusiveDynamics`Generate1D`"];
    Get["DiffusiveDynamics`Analyze1D`"];    
    Print["Reloaded DiffusiveDynamics`! on kernel"<>ToString@$KernelID];
]];  

BeginPackage["DiffusiveDynamics`",{"DiffusiveDynamics`Utils`","DiffusiveDynamics`Generate2D`","DiffusiveDynamics`Analyze2D`","DiffusiveDynamics`Visualize2D`"}];

 
EndPackage[];
