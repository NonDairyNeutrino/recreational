(* ::Package:: *)

(*Question: https://mathematica.stackexchange.com/q/182275/46490*)
(*For a given matrix, randomly replace n values with x but none of the replaced values are adjacent*)
argpat = PatternSequence[mat_?SquareMatrixQ, n_?Positive (*Number of values to replace*), repval_ (*value upon replacement*), metric_:ChessboardDistance];
myReplacePart[mat_, reps_, dim_, repval_] := mat - SparseArray[reps -> Extract[mat,reps], {dim, dim}] + SparseArray[reps -> repval,{dim,dim}]


(*Loop Method*)
nonAdjRepLoop[argpat] := Block[
    {dim = Length@mat, reps = DeleteDuplicates@RandomInteger[{1, Length@mat}, {Length@mat - 1, 2}]},
    While[
        Length@reps < n,
        AppendTo[reps, RandomInteger[{1, dim}, 2]];
        reps = Cases[
            {#, Nearest[Complement[reps, {#}], #, {All, 1}, DistanceFunction -> metric]} & /@ reps,
            {pt_, {}} :> pt
            ]
    ];
	myReplacePart[mat, reps, dim, repval]
]


(*Recursive Method*)
nonAdjRepRecInds[{inds_, n_, dim_}] := If[
	    Length@inds < n,
		nonAdjRepRecInds@{
			With[
				{rando = Append[inds, RandomInteger[{1, dim}, 2]]},
				Cases[
					{#, Nearest[Complement[rando, {#}], #, {All, 1}]} & /@ rando,
					{pt_, {}} :> pt
				]
			], n, dim},
		{inds, n, dim}
	]

nonAdjRepRec[argpat] := With[
	{dim = Length@mat, reps = First@nonAdjRepRecInds@{DeleteDuplicates@RandomInteger[{1, Length@mat}, {Length@mat - 1, 2}], n, Length@mat}},
	myReplacePart[mat, reps, dim, repval]
]


(*General Recursive Method*)
nonAdjRepGenRecInds[dim_Integer, n_Integer, rad_?NumericQ, reps_: {}] /; 0 < n <= If[EvenQ@dim, dim^2/4, (dim + 1)^2/4] := If[
	Length@reps < n,
	nonAdjRepGenRecInds[dim, n, rad, #] &@With[
		{rando = DeleteCases[Union[reps, {RandomInteger[{1, dim}, 2]}], {}]},
		Cases[
			{#, Nearest[rando, #, {All, rad}, DistanceFunction -> ChessboardDistance]} & /@ rando,
			{x_, {x_}} :> x
		]
	],
	reps
]

nonAdjRepGenRec[argpat, rad_] := myReplacePart[mat, nonAdjRepGenRecInds[Length@mat, n, rad], Length@mat, repval]
