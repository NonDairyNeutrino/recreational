(* ::Package:: *)

(* ::Title:: *)
(*Wordle Helper*)


(* ::Text:: *)
(*Things that need to be done:*)
(*- Add word cloud*)
(*	- Needs bestGuessList that contains all valid words in decreasing likelihood, with option to give subset of list like the first most likely through the n-th, or n-th through m-th.*)
(*		- Would like to run bestGuess, then delete the ouput form the pool of considered words, then run bestGuess on that pool to get the 2nd most likely word, repeat.*)
(*- Add consideration for "vowel optimization" as a metric (In addition to most common letters, also consider words that have the most vowels in them because you can throw out a lot of words that have some vowel in it). From thewanderebard_ "Finding words that fit your current rules with the most vowels"*)
(*- Restructure into proper package for release*)
(*	- Add engine functionality? (Maybe just manually loop bestGuess with Wordle data)*)


(* ::Section::Closed:: *)
(*Creating the Domain of Words*)


(* ::Text:: *)
(*Start with all 5 letter words in the English language.*)


Clear@wordDomain;
wordDomain = Sort@DeleteDuplicates@(*Pick the words with 5 letters*)Select[
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
(*Using Wordle Data/Color Data to get a Valid Pool*)


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


(* ::Subsubsection::Closed:: *)
(*Green*)


Clear@greenMatchQ
(*No greens*)greenMatchQ[str_, {}] = True;
greenMatchQ[str_, greens : letAndPosPattern] := StringTake[str, Replace[greens[[;;, 2]], pos_Integer :> {pos}, {1}]] === greens[[;;, 1]]


(* ::Input:: *)
(*(*Test*)greenMatchQ["orate", {{"r", 2}, {"te", {4, 5}}}]*)


(* ::Subsubsection::Closed:: *)
(*Yellow*)


Clear@yellowMatchQ
yellowMatchQ[str_, {}] = True;
yellowMatchQ[str_, yellows : letAndPosPattern /; (*Makes sure each string is a single letter*)MatchQ[StringLength /@ yellows[[;;, 1]], {1 ..}]] := 
	stringContainsAll[str, yellows[[;;, 1]]] && 
	StringTake[str, List /@ yellows[[;;, 2]]] ~List~ yellows[[;;, 1]] // Transpose // Apply[UnsameQ, #, {1}] & // Apply@And


(* ::Input:: *)
(*(*Test*)yellowMatchQ["orate", {{"r", 1}, {"t", 3}, {"e", 4}}]*)


(* ::Subsubsection::Closed:: *)
(*Gray*)


(*Yes this is technically redundant but is redfined for the sake of consistency, understanding, readability, and to allow for input validation*)
Clear@grayMatchQ
grayMatchQ[str_, ""] = True;
grayMatchQ[str_, grays : letAndPosPattern] := stringContainsNone[str, grays[[;;, 1]]]
grayMatchQ[str_, grays_?LetterQ] := stringContainsNone[str, Characters@grays]


(* ::Input:: *)
(*(*Test*)grayMatchQ["orate", "qlms"]*)
(*grayMatchQ["orate", {{"q", 2}, {"l", 3},{"m", 4}}]*)


(* ::Subsubsection::Closed:: *)
(*All Combined*)


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
(*bestGuess@{"greens" -> {{"b", 1}}, "yellows" -> {{"i", 3}, {"e", 4}}, "grays" -> "aow"}*)


(* ::Input:: *)
(*validPool[wordDomain, {"greens" -> {}, "yellows" -> {}, "grays" -> ""}]===wordDomain*)


(* ::Section:: *)
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


Clear@bestGuessList
bestGuessList[colorData : colorMatchPattern] := bestGuess[colorData]


(* ::Subsection::Closed:: *)
(*Best Opener/First Guess*)


bestGuess[{"greens" -> {}, "yellows" -> {}, "grays" -> ""}]


(* ::Text:: *)
(*Out of these, "orate" is the word whose remaining letter is the more common than that of the other words.  Therefore, orate is the "best" word to open with in Wordle.*)


(* ::Text:: *)
(*These should be the most "efficient" words to choose first/open with because they have most of the most common letters.*)


(* ::Section::Closed:: *)
(*Interactivity*)


Clear@inputPrompt
inputPrompt = "Enter each color data as a list of the letter and its position. e.g. {\"greens\" -> {{\"a\", 2}}, \"yellows\" -> {{\"o\", 1}, {\"r\", 2}}, \"grays\" -> \"temnls\"}

In case of emergency, enter \"Break[]\" or \"Abort[]\".
In case of a bigger emergency, enter \"Quit[]\" or \"Exit[]\".

";


Clear@prevSug
prevSug[prevOutputList_] := Row["Previous suggestions:\n" <> StringRiffle[prevOutputList, "\n" ], WordCloud[]]


Clear@inputWindow
inputWindow[prevInput_, prevOutputList_] :=  [inputPrompt <> prevSug[prevOutputList], prevInput]


(* ::Section:: *)
(*wordleHelper/main*)


echo[expr_] := (Print@expr; expr)


Clear@wordleHelper
wordleHelper[] := Block[
	{prevInput = {"greens" -> {}, "yellows" -> {}, "grays" -> ""}, prevInputTemp, prevOutputList = {}, tempGuess},
	While[
		True,
		tempGuess = bestGuess[prevInputTemp = inputWindow[prevInput, prevOutputList]];
		Which[
			prevInputTemp === $Canceled,
			Break[],
			Head@tempGuess =!= bestGuess,
			(prevInput = prevInputTemp); AppendTo[prevOutputList, tempGuess],
			Head@tempGuess === bestGuess,
			AppendTo[prevOutputList, "That didn't work. Check for errors and try again."]
		]
	]
]
