(* ::Package:: *)

(* ::Title:: *)
(*Wordle Helper*)


(* ::Text:: *)
(*Things that need to be done*)
(*- Incorporate Wordle returned color/inclusional/positional data after each step*)
(*- Add interactivity for Wordle data*)


(* ::Section::Closed:: *)
(*Creating the domain of words*)


(* ::Text:: *)
(*Start with all 5 letter words in the English language.*)


Clear@wordDomain;
wordDomain=Sort@DeleteDuplicates@(*Pick the words with 5 letters*)Select[
	Catenate@Select[
		(*Pick out only the words made of letters*)StringCases[
			(*Get all "words" Mathematica knows*)DeleteDuplicates@ToLowerCase@WordData[],
			LetterCharacter..
		],
		Length@# == 1 &
	],
	StringLength@# == 5 &
];


(* ::Section:: *)
(*Finding the "Best" Guess at Each Step*)


(* ::Text:: *)
(*First we need to define what we mean by "best".  The "best" word should contain the most common letters while still being a valid word.*)


(* ::Text:: *)
(*So then we find the most common letters in our pool.*)


Clear@mostCommonLetters
mostCommonLetters[wordList_]:=Reverse@Sort@Counts@Catenate@Characters@wordList//Keys


(*The main idea is to whittle down the pool/domain of words by progressively only considering words that contain the most common letters (i.e. All the words that contain the most common letter, then out of those words, pick out all the words that contain the second most common letter, etc.).*)
Clear@bestWords
bestWords[wordList_]:= Block[
	{pool = wordList, temp},
	FoldList[
		(*If none of the words  in the current pool contain the n-th most common letter, move on to the (n+1)th letter with the same pool*)
		If[
			(*Pick out all the words from the current pool that contain the n-th most common letter in the pool*)
			(temp = Select[#1, StringContainsQ[#2]]) == {},
			pool,
			pool = temp
		] &,
		wordList,
		mostCommonLetters@wordList
	] // DeleteCases[{}] // Last
]


(* ::Subsection:: *)
(*Best Opener/First Guess*)


bestWords@wordDomain


(* ::Text:: *)
(*Out of these, "orate" is the word whose remaining letter is the more common than that of the other words.  Therefore, orate is the "best" word to open with in Wordle.*)


(* ::Text:: *)
(*These should be the most "efficient" words to choose first/open with because they have most of the most common letters.*)


(* ::Section:: *)
(*Using Wordle Data*)


(* ::Text:: *)
(*So upon entering "orate" as our first guess, Wordle will return some data to us.  This data comes in the form of tiles being colored either yellow, green, or grey.  Yellow means that letter is in the word, but not in that position.  Green means that letter is in the word and in the correct position.  Grey means that letter is not in the word.*)


Clear@"stringContains*"
stringContainsAll[str_String,chars:{__String}]:=And@@(StringContainsQ[str,#]&/@chars)
stringContainsAll[chars:{__String}]@str_String:=stringContainsAll[str,chars]

stringContainsNone[str_String,chars:{__String}]:=And@@(Not@StringContainsQ[str,#]&/@chars)
stringContainsNone[chars:{__String}]@str_String:=stringContainsNone[str,chars]


Select[wordDomain,(*yellow*)stringContainsAll[#,{"o","r","g","e"}]&&StringTake[#,{2,{3,4},1}]!={"sn","or"}&&(*gray*)stringContainsNone[#,{"a","t","s","n","f"}]&&(*green*)StringTake[#,{2,5}]=="orge"&]
bestWords[%,mostCommonLetters@%]
