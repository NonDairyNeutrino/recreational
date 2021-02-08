(* ::Package:: *)

(*11/1/20 Find all words that begin and end with a given letter.*)
Clear@wordsandwich
wordsandwich[letter_String] := Select[ (*picks out the elements of the given list such that the function evaluates to True*)
	WordList[] (*gives a list of common words*),
	StringTake[#, 1]  (*returns the first character of the input string*) ===
	StringTake[#, -1] (*returns the last character of the input string*) ===
	letter & (*check if the first, last, and given character are syntactically/manifestlly equal*)
]
