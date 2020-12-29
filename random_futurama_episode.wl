(* ::Package:: *)

(* ::Title:: *)
(*Random Futurama Episode Selector*)


(* ::Section:: *)
(*Version 1*)


(* ::Input:: *)
(*Clear[Futurama,epnames]*)
(*epnames[S_]:=epnames[S]=WolframAlpha["Futurama season"<>ToString@S,{{"SeasonEpisodes:TelevisionProgramData",1},"ComputableData"},PodStates->{"SeasonEpisodes:TelevisionProgramData__More","SeasonEpisodes:TelevisionProgramData__More"}][[2;;,1]]*)
(*Futurama:=Block[*)
(*(*This uses this sequencing http://epguides.com/Futurama/*)*)
(*{S=RandomInteger[{1,7}],ep},*)
(*ep=RandomInteger[{1,{9,20,15,12,16,26,26}[[S]]}];*)
(*"Season: "<>ToString@S<>", Episode: "<>ToString@ep<>", Name: "<>epnames[S][[ep]]*)
(*]*)
(*CreateDialog[*)
(*Button["Give me a random Futurama episode to watch",Print@Futurama,Alignment->Center],*)
(*WindowTitle->"Which episode of Futurama you should watch"*)
(*]*)


(* ::Section:: *)
(*Version 2*)


Clear@fulleps
fulleps=Cases[
	Import["https://en.wikipedia.org/wiki/List_of_Futurama_episodes","Data"],
	Alternatives@@({_Integer}~Join~ConstantArray[_,#]&/@{7,8}),
	\[Infinity]
];


Clear@eps
eps=ToExpression[
	StringSplit[#[[If[Length@#==8,-2,-3]]],_?LetterQ..]
]~Join~{
	StringTrim[#[[3]],("\" "|" \"")]
}&/@fulleps;


futurama[]:=StringForm["Season: `` Epsiode: `` Name: ``",Sequence@@RandomChoice[eps]]


CreateDialog[
	Button["Give me a random Futurama episode to watch",Print@futurama[],Alignment->Center],
	WindowTitle->"Which episode of Futurama you should watch"
]
