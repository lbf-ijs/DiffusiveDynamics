(* Mathematica Test File *)

ClearAll[DIFFX, DIFFY, ALPHA,CELLRANGE,ENERGY,KT,rw];
    STEPS = 9;
    STEP = 1;
    CELLRANGE = {1000., 500.};
    DIFFX = Compile[{x, y}, 5 Sin[x/ CELLRANGE[[1]] \[Pi]] + 10];
    DIFFY = Compile[{x, y}, 5 Sin[y/ CELLRANGE[[2]] \[Pi]] + 10];
    ALPHA = Compile[{x, y}, (((x/2)/CELLRANGE[[1]])^2 + ((y/2)/ CELLRANGE[[2]])^2)*180];
    ENERGY = Compile[{x, y}, 1];
    KT = 1.;


Test[ (*Checks if GenerateDiffusionTrajectory2D result change. TODO: This is a very brittle test. Make more robust*)
    BlockRandom[SeedRandom[1];
                rw = GenerateDiffusionTrajectory2D[STEPS, DIFFX, DIFFY, ALPHA, ENERGY, KT, STEP, 
                                  {-CELLRANGE, CELLRANGE}, InitialPosition -> {0., 0.}]
    ];
    (*target = {{1., 0., 0.}, {2., 2.18773, 1.86114}, {3., 3.37662, 4.74361}, {4., 10.3169, 7.82425}, {5., 10.9218, 12.8288}, {6., 9.05783, 6.94991}, {7., 9.00656, -0.983006}, {8., 9.11527, 4.90628}, {9., 9.5713, 4.07522}, {10., 8.02057, 3.27114}};*)
     target={{1.,0.,0.},{2.,-4.69635,-2.68286},{3.,-6.1222,-0.37686},{4.,-11.1117,1.84641},{5.,-9.61719,-5.44074},{6.,-7.31684,-13.6857},{7.,-10.7295,-16.1392},{8.,-3.44012,-21.8715},{9.,-2.96321,-26.374},{10.,-7.35223,-33.3529}};
(*  
    Print[rw];
    Print[target];
    Print[Chop[rw-target,0.0001]];*)
    result = Total@Total@Chop[rw-target,0.0001]==0;
    If[ ! result,
        Print["Generate2DTest->GenerateDiffusionTrajectory2D rezultati ponovljivi..."];
        Print[rw];
        Print[target];
        Print[Chop[rw-target,0.0001]];
    ];
    result
    ,
    True
    ,
    TestID->"Generate2DTest->GenerateDiffusionTrajectory2D results are working..."
]


Test[ (**)
    ips={{0, 0},{1,1}};
    BlockRandom[SeedRandom[1];
                rw = ParallelGenerateDiffusionTrajectory2D[STEPS, DIFFX, DIFFY, ALPHA, ENERGY, KT, STEP, 
                                  {-CELLRANGE, CELLRANGE}, "InitialPositions" -> ips,"ProcessCount" -> 2]
    ];
	rw[[All,1,{2,3}]]
    ,
    N@ips
    ,
    TestID->"Generate2DTest->GenerateDiffusionTrajectory2D initial points work..."
]

Test[ (**)
    ips={{0, 0}};
    BlockRandom[SeedRandom[1];
                rw = ParallelGenerateDiffusionTrajectory2D[STEPS, DIFFX, DIFFY, ALPHA, ENERGY, KT, STEP, 
                                  {-CELLRANGE, CELLRANGE}, "InitialPositions" -> ips,"ProcessCount" -> 2]
    ];
	Length@rw
    ,
    2
    ,
    TestID->"Generate2DTest->GenerateDiffusionTrajectory2D initial points get added if less then ProcessCount..."
]

Test[ (**)
    ips={{0, 0},{0, 0},{0, 0},{0, 0}};
    BlockRandom[SeedRandom[1];
                rw = ParallelGenerateDiffusionTrajectory2D[STEPS, DIFFX, DIFFY, ALPHA, ENERGY, KT, STEP, 
                                  {-CELLRANGE, CELLRANGE}, "InitialPositions" -> ips,"ProcessCount" -> 2]
    ];
	Length@rw
    ,
    2
    ,
    TestID->"Generate2DTest->GenerateDiffusionTrajectory2D initial points get truncated if more than..."
]


ClearAll[DIFFX, DIFFY, ALPHA,CELLRANGE,ENERGY,KT,rw];


(*ClearAll[DIFFX, DIFFY, ALPHA,CELLRANGE,ENERGY,KT,rw];
Test[ (*Checks if GenerateDiffusionTrajectory2D result change. TODO: This is a very brittle test. Maek more robust*)
    STEPS = 10;
    STEP = 1;
    CELLRANGE = {1000., 500.};
    DIFFX = Compile[{x, y}, 5 Sin[x/ CELLRANGE[[1]] \[Pi]] + 10];
    DIFFY = Compile[{x, y}, 5 Sin[y/ CELLRANGE[[2]] \[Pi]] + 10];
    ALPHA = Compile[{x, y}, (((x/2)/CELLRANGE[[1]])^2 + ((y/2)/ CELLRANGE[[2]])^2)*180];
    ENERGY = Compile[{x, y}, 1];
    KT = 1.;
    BlockRandom[SeedRandom[1];
                rw=GenerateDiffusionTrajectory2D[STEPS = 10, DIFFX, DIFFY, ALPHA, ENERGY, KT, STEP, 
                                  {-CELLRANGE, CELLRANGE}, InitialPosition -> {0., 0.}]
    ]
    ,
    {{1., 0., 0.}, {2., 2.18773, 1.86114}, {3., 3.37662, 4.74361}, {4., 10.3169, 7.82425}, {5., 10.9218, 12.8288}, {6., 9.05783, 6.94991}, {7., 9.00656, -0.983006}, {8., 9.11527, 4.90628}, {9., 9.5713, 4.07522}, {10., 8.02057, 3.27114}}
    ,
    EquivalenceFunction->(Total@Total@Chop[#1-#2,0.001]==0;True)&
    ,
    TestID->"Generate2DTest->GenerateDiffusionTrajectory2D rezultati ponovljivi..."
    
]
ClearAll[DIFFX, DIFFY, ALPHA,CELLRANGE,ENERGY,KT,rw];*)