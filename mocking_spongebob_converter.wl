(* ::Package:: *)

(*Original Method*)
spongemock[str_String, percent_: 0.5] := Block[
  {lowerstr = ToLowerCase@str, letpos = StringPosition[str, _?LetterQ], spongepos},
  spongepos = RandomSample[letpos, percent Length@letpos // Round];
  StringReplacePart[lowerstr, ToUpperCase@StringTake[lowerstr, spongepos], spongepos]
]


(*Much faster method*)
spongemock[str_String, percent_: 0.5] := Block[
  {lowerstr = ToLowerCase@str, letpos = StringPosition[str, _?LetterQ], spongepos},
  spongepos = List /@ RandomSample[letpos, percent Length@letpos // Round][[;; , 1]];
  StringJoin @@ MapAt[ToUpperCase, lowerstr, spongepos]
]


(*Much much faster one-line method, thanks to /u/Imanton1 and /u/PeterLissajous on reddit*)
spongeMock[str_String, p_: 0.5] := RandomChoice[{p, 1 - p} -> {ToUpperCase, ToLowerCase}]@#& /@ Characters@str // StringJoin


(* ::Input:: *)
(*spongeMock@"Hello World!"*)
(*spongeMock@" +-1234567890aBc"*)
(*spongeMock@ExampleData[{"Text","ToBeOrNotToBe"}]*)
(*spongeMock@ExampleData[{"Text","GettysburgAddress"}]*)


(* ::Input:: *)
(*{#2,StringLength@ExampleData@{##}}&@@@ExampleData["Text"]//SortBy[#,#[[2]]&]&*)
