(* ::Package:: *)

(*Write a function that stacks successively longer lists of integers on top of each other.*)
Clear@numberTriangle
numberTriangle[n_] := Column[(*Arranges the list of elements in a vertical column*)
	Range[(*Since Range is listable, it returns a list of Range evaluated for each element of the list*)
		Range[(*Gives a list of integers from 1 to n*)
			n
		]
	]
]


(* ::Input:: *)
(*numberTriangle[5]*)
