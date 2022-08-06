(* ::Package:: *)

(* Question: https://mathematica.stackexchange.com/q/182882/46490*)
(*Given a list of integers, find the largest sum of a contiguous sub-sequence and print the sub-array.*)


largestContiguousSum[nums_List] := With[
    {sums = Array[Sum[nums[[k]], {k, ##}] &, Length@nums {1, 1}]},
    {Max@sums, nums[[Span @ ##]]} & @@@ Position[sums, Max@sums]
]


largestContiguousSum2[nums_List] := Block[
	{sums = Table[Total@nums[[j ;; k]], {j, #}, {k, j, #}] &@Length@nums, maxsums},
	maxsums = Max@sums;
	{maxsums, nums[[Span@##]]} & @@@ Replace[Position[sums, maxsums], {a_, b_} :> {a, b + a - 1}, {-2}]
]
