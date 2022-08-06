(* ::Package:: *)

(* ::Section:: *)
(*Code*)


(*indexRotation gives the new position of the index {j,k} after the nxn matrix has been rotated clockwise 90 degrees*)
indexRotation[n_,{j_,k_}?VectorQ]:={k,n+1-j}
indexRotation[n_,indices:{{_,_}?VectorQ..}]:= indices.{{0,-1},{1,0}} // TranslationTransform[{0,n+1}]
indexRotation[n_][indices_] := indexRotation[n, indices]


(*For an nxn matrix, perimeterIndices gives the ordered indices of the perimeter on the given radius/level.
e.g. level 1 contains the first and last rows and columns, and level 2 contains 2nd and 2nd to last rows and columns*)
perimeterIndices[n_,level_]/;level<=n-level:=Catenate@NestList[indexRotation[n],Thread@{level,Range[level,n-level]} (*Top edge*),3]
perimeterIndices[n_,level_]/;level==Ceiling[n/2]:={{level,level}}
perimeterIndices[n_][level_] := perimeterIndices[n,level]


snake[n_Integer]:=Catenate@Array[perimeterIndices[n],Ceiling[n/2]]
snake[n_Integer,level_Integer]/;level<=Ceiling[n/2]:=Catenate@Array[perimeterIndices[n],level]
snake[n_Integer,{level_Integer}]:=perimeterIndices[n,level]


(* ::Section:: *)
(*Applications*)


(* ::Input:: *)
(*(*(*Snake Animation*)*)
(*Module[*)
(*{n=100,parts},*)
(*parts=snake[n];*)
(*Animate[ArrayPlot@SparseArray[parts[[;;k]]->1,{n,n}],{k,Range[n^2]}]*)
(*]*)*)


(*Discrete Integration around the perimeters of a square domain*)
perimeterIntegrationList[f_, dom : {{x1_, x2_, dx_ : 1}, {y1_, y2_, dy_ : 1}}] := Block[
	{data = Outer[f, ##]&@@Range@@Transpose@dom (*Such that the domain is square*), perimeterValues},
	perimeterValues = Extract[data, snake@Length@data](*This also gives how fast the sum changes at each step by TFC2*);
	Accumulate[perimeterValues dx dy]
]
perimeterIntegrationList[f_, interval : {lb_?NumericQ, ub_?NumericQ, d_ : 1}] := perimeterIntegrationList[f,{interval, interval}]
perimeterIntegration[f_, dom_] := perimeterIntegrationList[f, dom][[-1]]

(*Example*)
perimeterIntegration[Exp[-Norm[{##}]]&, {-1., 1, 0.005}] - NIntegrate[Exp[-Norm[{##}]]&[x,y],{x,-1,1},{y,-1,1}] // Abs
