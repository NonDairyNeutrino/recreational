(* ::Package:: *)

(*Write a function to find which numbers can be generated by combining n 1s with Plus and Times in all possible ways.*)
Clear@numberFromOnes
numberFromOnes[n_Integer /; 1 < n] := Block[
	{ones = ConstantArray[1, n]},
	
]


{((1 + 1) + 1) + 1, 1 + ((1 + 1) + 1), (1 + (1 + 1)) + 1, 1 + (1 + (1 + 1)), (1 + 1) + (1 + 1), (1 1 + 1) + 1, 1 + (1 1 + 1), (1 + 1 1) + 1, 1 + (1 + 1 1), (1 + 1) + 1 1, 1 1 + (1 + 1), (1 + 1) 1 + 1, 1 + (1 + 1) 1, 1 (1 + 1) + 1, 1 + 1 (1 + 1), ((1 + 1) + 1) 1, 1 ((1 + 1) + 1), (1 + (1 + 1)) 1, 1 (1 + (1 + 1)), (1 + 1) (1 + 1), (1 1) 1 + 1, 1 + (1 1) 1, 1 (1 1) + 1, 1 + 1 (1 1), 1 1 + 1 1, (1 1 + 1) 1, 1 (1 1 + 1), (1 + 1 1) 1, 1 (1 + 1 1), (1 + 1) (1 1), (1 1) (1 + 1), ((1 + 1) 1) 1, 1 ((1 + 1) 1), (1 (1 + 1)) 1, 1 (1 (1 + 1)), ((1 1) 1) 1, 1 ((1 1) 1), (1 (1 1)) 1, 1 (1 (1 1)), (1 1) (1 1)}


Block[
	{ones = ConstantArray[1, 4]},
	HoldForm@*Plus@@@Subsets[ones, {2}]
]