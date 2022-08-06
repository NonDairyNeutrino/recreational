(* ::Package:: *)

(*Question: https://mathematica.stackexchange.com/q/209367/46490*)
(*Goal: Find the smallest subset of the codomain that has the range as a subset and can be represented as a product of intervals*)


nMinMaxValue[dom : {lb_?NumericQ, ub_?NumericQ}][y_] := With[{t = First@Variables@y}, {NMinValue[{y, lb < t < ub}, t], NMaxValue[{y, lb < t < ub}, t]}]


rangeBounds[listofvecs_?ArrayQ, dom_] := Map[nMinMaxValue[dom], listofvecs, {-1}]
rangeBounds[dom_][vec_] := rangeBounds[vec, dom]
(*Gives the lower and upper bounds of the range in each dimension*)
