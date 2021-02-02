(* ::Package:: *)

(* ::Title:: *)
(*Weekly Challenges*)


(* ::Section:: *)
(*10/18/20 Butterflied Strings*)


(* ::Text:: *)
(*Butterflied Strings: Join a string with its reversal*)


Clear@butterfly
butterfly[str_String]:=str<>StringReverse@str


(* ::Input:: *)
(*butterfly["Mathematica"]*)
(*butterfly["racecar"]*)
(*butterfly["kayak"]*)


(* ::Section:: *)
(*10/25/20 Multiples of 3 and 5*)


(* ::Text:: *)
(*Multiples of 3 and 5: Write a function that takes a positive integer n and returns the number of multiples of both 3 and 5 up to n.*)


Clear@threefive
threefive[n_Integer]:=Floor[n/15]


(* ::Input:: *)
(*DiscretePlot[threefive[n],{n,100},Ticks->{Range[0,100,15],Automatic}]*)


(* ::Section:: *)
(*11/1/20 Words Beginning and Ending with a Given Letter*)


(* ::Text:: *)
(*Find all words that begin and end with a given letter. Link*)


Clear@wordsandwich
wordsandwich[letter_String]:=Select[WordList[],StringTake[#,1]===StringTake[#,-1]===letter&]


(* ::Section:: *)
(*Number Triangles*)


(* ::Text:: *)
(*Write a function that stacks successively longer lists of integers on top of each other.*)


Clear@numberTriangle
numberTriangle[n_]:=Column@Range@Range@n


(* ::Input:: *)
(*numberTriangle[5]*)


(* ::Section:: *)
(*FizzBuzz*)


(* ::Text:: *)
(*Given a list of integers from 1 to n, write a function that replaces multiples of 3 with "fizz", multiples of 5 with "buzz", multiples of both 3 and 5 with "fizzbuzz" and leaves other numbers fixed.*)


(* ::Subsection:: *)
(*Math*)


Clear@fizzBuzzMath
fizzBuzzMath[n_]:=Range[n]/.MapThread[
	k_/;Denominator[k/15]==#1:>#2&,
	{
		{5,3,1},
		{"fizz","buzz","fizzbuzz"}
	}
]


(* ::Input:: *)
(*fizzBuzzMath@30*)


(* ::Subsection:: *)
(*Divisors*)


Clear@fizzBuzzDivisors
fizzBuzzDivisors[k_Integer?Positive]:=Range[k]/.n_/;Divisible[n,15]:>"fizzbuzz"/.{n_/;Divisible[n,3]:>"fizz",n_/;Divisible[n,5]:>"buzz"}


(* ::Input:: *)
(*fizzBuzzDivisors@30*)


(* ::Subsection:: *)
(*Base 15*)


(* ::Input:: *)
(*Range[30]*)
(*IntegerDigits[#,15]&/@%*)
(*MemberQ[{3,6,9,12},Last@#]&/@%*)


(* ::Section:: *)
(*Getting a Basketball Score*)


(* ::Text:: *)
(*Write a function that takes an integer n and outputs all the possible ways to represent n as sums of 2s and 3s.*)


Clear@twoAndThreePointers
twoAndThreePointers[n_Integer?Positive] := IntegerPartitions[n, Infinity, {3, 2}]


(* ::Input:: *)
(*twoAndThreePointers/@{5,14,3,6,19}//Column*)
