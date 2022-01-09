(* ::Package:: *)

(* ::Title:: *)
(*Wordle Helper*)


(* ::Text:: *)
(*Some of these ideas may have been inspired by the game Mastermind.*)


(* ::Section:: *)
(*Creating the domain of words*)


(* ::Text:: *)
(*Start with all 5 letter words in the English language.*)


Clear@wordDomain;
wordDomain=Sort@DeleteDuplicates@Select[
Catenate@Select[
StringCases[
DeleteDuplicates@ToLowerCase@WordData[],
LetterCharacter..
],
Length@#==1&
],
StringLength@#==5&
];


(* ::Section:: *)
(*Finding the "Best" Guess at Each Step (Needs position consideration)*)


(* ::Text:: *)
(*First we need to define what we mean by "best".  The "best" word should contain the most common letters while still being a valid word.*)


(* ::Text:: *)
(*So then we find the most common letters in our pool.*)


(*Percent total that has that letter in it*)
Clear@mostCommonLetters
mostCommonLetters[wordList_]:=Reverse@Sort@Counts@Catenate@Characters@wordList//Keys


Clear@bestWords
bestWords[wordList_,mostCommonLetters_]:=FoldList[
(*If this one returns NULL, return the previous iteration*)
If[
Select[#1,StringContainsQ[#2]]=={},
Select[#1,StringContainsQ[#2]]
]&,
wordList,
mostCommonLetters
]//DeleteCases[{}]//Last


(* ::Subsection:: *)
(*Best Opener/First Guess*)


mostCommonLetters@wordDomain


bestWords[wordDomain,mostCommonLetters@wordDomain]
(*Reverse@Sort@Counts@Catenate@Characters@%*)


(* ::Text:: *)
(*Out of these, "orate" is the word whose remaining letter is the more common than that of the other words.  Therefore, orate is the "best" word to open with in Wordle.*)


(* ::Text:: *)
(*These should be the most "efficient" words to choose first/open with because they have most of the most common letters.*)


(* ::Section:: *)
(*Second Guess (*second breakfast pun here*)*)


(* ::Text:: *)
(*So upon entering "orate" as our first guess, Wordle will return some data to us.  This *)
