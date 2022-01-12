(* ::Package:: *)

(* ::Title:: *)
(*Wordle Helper*)


(* ::Text:: *)
(*Things that need to be done*)
(*- Add interactivity for Wordle data i.e. after each step, the program prompts the user to enter the "color data" returned by Wordle.*)
(*- [Optional] Add "visualization" for the whittler.  Show sets of words at each step of whittling so you can see the progress and the pool get smaller.*)


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


(* ::Section::Closed:: *)
(*Using Wordle Data*)


(* ::Text:: *)
(*So upon entering "orate" as our first guess, Wordle will return some data to us.  This data comes in the form of tiles being colored either yellow, green, or grey.  Yellow means that letter is in the word, but not in that position.  Green means that letter is in the word and in the correct position.  Grey means that letter is not in the word.*)


(* ::Subsection::Closed:: *)
(*Logistic Functions and Definitions*)


Clear@"stringContains*"
stringContainsAll[str_String, chars : {__String}] := SubsetQ[Characters@str, chars]
stringContainsAll[chars : {__String}]@str_String := stringContainsAll[str, chars]

stringContainsNone[str_String, chars : {__String}] := DisjointQ[Characters@str, chars]
stringContainsNone[chars : {__String}]@str_String := stringContainsNone[str, chars]


Clear@letAndPosPattern
letAndPosPattern = {{_?LetterQ, {__Integer} | _Integer}...};


(* ::Subsection::Closed:: *)
(*Color Validation*)


Clear@greenMatchQ
(*No greens*)greenMatchQ[str_, {}] = True;
greenMatchQ[str_, greens : letAndPosPattern] := StringTake[str, Replace[greens[[;;, 2]], pos_Integer :> {pos}, {1}]] === greens[[;;, 1]]


(* ::Input:: *)
(*(*Test*)greenMatchQ["orate", {{"r", 2}, {"te", {4, 5}}}]*)


Clear@yellowMatchQ
yellowMatchQ[str_, {}] = True;
yellowMatchQ[str_, yellows : letAndPosPattern /; (*Makes sure each string is a single letter*)MatchQ[StringLength /@ yellows[[;;, 1]], {1 ..}]] := 
	stringContainsAll[str, yellows[[;;, 1]]] && 
	StringTake[str, List /@ yellows[[;;, 2]]] ~List~ yellows[[;;, 1]] // Transpose // Apply[UnsameQ, #, {1}] & // Apply@And


(* ::Input:: *)
(*(*Test*)yellowMatchQ["orate", {{"r", 1}, {"t", 3}, {"e", 4}}]*)


(*Yes this is technically redundant but is redfined for the sake of consistency, understanding, readability, and to allow for input validation*)
Clear@grayMatchQ
grayMatchQ[str_, ""] = True;
grayMatchQ[str_, grays_?LetterQ] := stringContainsNone[str, Characters@grays]


(* ::Input:: *)
(*(*Test*)grayMatchQ["orate", "qlms"]*)


Clear@colorMatchPattern
colorMatchPattern = {
	"greens" -> greens : letAndPosPattern,
	"yellows" -> yellows : letAndPosPattern,
	"grays" -> grays_?LetterQ
};


Clear@colorMatchQ
colorMatchQ[str_, colorMatchPattern] := greenMatchQ[str, greens] && yellowMatchQ[str, yellows] && grayMatchQ[str, grays]
(*Operator Form*)colorMatchQ[colorData_]@str_ := colorMatchQ[str, colorData]


(* ::Input:: *)
(*(*Test*)colorMatchQ[{"greens" -> {{"o", 1}}, "yellows" -> {{"e", 2}, {"r", 3},{"a", 4}}, "grays" -> "qms"}]@"orate"*)


(* ::Subsection::Closed:: *)
(*Valid Word Pool*)


Clear@validPool
validPool[wordList_, colorData_] := Select[wordList, colorMatchQ[colorData]]


(* ::Input:: *)
(*validPool[wordDomain, {"greens" -> {{"b", 1}}, "yellows" -> {{"i", 3}, {"e", 4}}, "grays" -> "aow"}]*)
(*bestGuess@%*)


(* ::Input:: *)
(*validPool[wordDomain, {"greens" -> {}, "yellows" -> {}, "grays" -> ""}]===wordDomain*)


(* ::Section::Closed:: *)
(*Finding the "Best" Guess at Each Step*)


(* ::Text:: *)
(*First we need to define what we mean by "best".  The "best" word should contain the most common letters while still being a valid word.*)


(* ::Text:: *)
(*So then we find the most common letters in our pool.*)


Clear@mostCommonLetters
mostCommonLetters[wordList_] := Reverse@Sort@Counts@Catenate@Characters@wordList//Keys


(*The main idea is to whittle down the pool/domain of words by progressively only considering words that contain the most common letters (i.e. All the words that contain the most common letter, then out of those words, pick out all the words that contain the second most common letter, etc.).*)
Clear@bestGuess
bestGuess[colorData : colorMatchPattern]:= Block[
	{wordList = validPool[(*Could configure to reference the previous validPool to be more efficient, but it's decently quick already*)wordDomain, colorData], pool = wordList, temp},
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


(* ::Subsection::Closed:: *)
(*Best Opener/First Guess*)


bestGuess@wordDomain


(* ::Text:: *)
(*Out of these, "orate" is the word whose remaining letter is the more common than that of the other words.  Therefore, orate is the "best" word to open with in Wordle.*)


(* ::Text:: *)
(*These should be the most "efficient" words to choose first/open with because they have most of the most common letters.*)


(* ::Section:: *)
(*Interactivity*)


InputField[Dynamic[colorData], Expression, FieldSize -> 45, FieldHint -> "Enter color data 
e.g. {\"greens\" \[Rule] {{\"r\", 2}, {\"it\", {3, 4}}}, \"yellows\" \[Rule] {{\"k\", 1}, {\"i\", 5}}, \"grays\" \[Rule] \"oatebcp\"}"]
Dynamic@colorData


(* ::Section:: *)
(*Main*)


(* ::Input:: *)
(* bestGuess[{"greens" -> {{"r", 2}, {"i", 3}, {"k", 5}, {"n", 4}}, "yellows" -> {{"k", 1}, {"i", 5}}, "grays" -> "oatebcp"}]*)
