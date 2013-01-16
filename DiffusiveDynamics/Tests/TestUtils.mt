(* Mathematica Test File *)
Needs["DiffusiveDynamics`Utils`"];


Test[(*test if stride data reduces the first list dimension to 20*)
	data=RandomReal[{-1,1},{100,2}];
	sd=StrideData[data,20];	
	Length@sd
	,
	20
	,
	TestID->"Utils->StrideData test lengths"
]


ClearAll[data,sd];