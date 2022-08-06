(* ::Package:: *)

(*Question: https://mathematica.stackexchange.com/q/212572/46490*)


monosub[poly_, lhs_ -> rhs_, vars_] := With[
  {dims = (*Effective dimensions of CoefficientList array*) Exponent[poly, vars]},
  If[
    Or @@ Thread[Exponent[lhs, vars] > dims] (*If the power of either variable is not in poly, no sub is done*),
    poly,
    (*Reconstruct the polynomial*) 
	Dot[ 
		vars[[1]]^Range[0, dims[[1]]] (*Variable 1 monomial-vector e.g. {1, x, x^2, ...}*),
		(*Apply conversion factors to the right coefficients using component-wise multiplication*)
		CoefficientList[poly, vars] SparseArray[
			Band[Exponent[lhs, vars] + 1] -> ConstantArray[rhs/lhs, dims - Exponent[lhs, vars] + 1], 
			dims + 1,
			1
		],
      vars[[2]]^Range[0, dims[[2]]] (*Variable 2 monomial-vector*)
    ]
  ] // Expand
]
