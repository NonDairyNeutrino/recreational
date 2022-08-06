(* ::Package:: *)

(*Question: https://mathematica.stackexchange.com/q/212162/46490*)
(*Which operation combines {f,g,h,\[Ellipsis]} and y[u,v,w,\[Ellipsis]] to get y[f[u],g[v],h[w],\[Ellipsis]]?*)


innerMap[funcs_, expr_] := Head[expr] @@ MapThread[#1[#2] &, {funcs, List @@ expr}]
