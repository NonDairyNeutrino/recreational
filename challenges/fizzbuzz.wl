(* ::Package:: *)

(*
Given a list of integers from 1 to n,
write a function that replaces multiples of 3 with "fizz", multiples of 5 with "buzz",
multiples of both 3 and 5 with "fizzbuzz"
and leaves other numbers fixed.
*)

Clear@fizzBuzzMath
fizzBuzzMath[n_] := Switch[
	Denominator@#,    (*Look at the denominator of each fraction*)
	5, "fizz",        (*If it's 5, return "fizz"*)
	3, "buzz",        (*If it's 3, return "buzz"*)
	1, "fizzbuzz",    (*If it's 1, return "fizzbuzz"*)
	15, 15#           (*If it's 15, return the original number*)
]& /@ (Range[n]/15) (*Divide by 15 so multiples of 3 have a 5 in the denominator, 5 has 3, and 15 has 1.)*)
