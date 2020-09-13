(* ::Package:: *)

BeginPackage["taylorPath`"];
(*Usage*)
taylorSeq::usage =
	"taylorSeq[{f, z, n}] gives the sequence of partial sums, up to n, of the Maclaurin series of f, evaluated at the complex number z.\n"<>
	"taylorSeq[{f, z, \[Infinity], \[Epsilon]}] gives the sequence of partial sums of the Maclaurin series of f, evaluated at the complex number z to within a tolerance of \[Epsilon].";

taylorPath::usage =
	"taylorPath[{f, z, n}] gives the list of pairs of points that trace out the path defined by the Maclaurin series, up to n terms, of f at z.\n"<>
	"taylorPath[{f, z, \[Infinity], \[Epsilon]}] gives the list of pairs of points that trace out the path defined by the Maclaurin series of f at z up to a tolerance \[Epsilon].";

pathToTerms::usage = "pathToTerms[path] gives the list of the terms of the Maclaurin series that traces out path.";

labelPos::usage = "labelPos[path] gives the list of pairs of positions that yield aesthetic labels for the vectors in taylorPath.";

Begin["funcs`"];
(*Sequence of partial Taylor sums*)
Clear@taylorSeq
taylorSeq[{f_,z_/;z\[Element]Complexes,maxterms_/;maxterms\[Element]Integers&&maxterms>=0}]:=SeriesCoefficient[f[x],{x,0,n}]x^n/.{x->z}/.({n->#}&/@Range[0,maxterms])//ReIm//Accumulate
taylorSeq[{f_,z_/;z\[Element]Complexes,\[Infinity],Optional[\[Epsilon]_,0.05]}]:=Block[
	{maxterms=0,partsumseq={},partsum={0,0},error=Function[n,
		EuclideanDistance[
			partsum+=(SeriesCoefficient[f[x],{x,0,n}]x^n/.x->z//ReIm)//(AppendTo[partsumseq,#];#)&,
			ReIm@f[z]
		]
	]
	},
	While[error@maxterms>\[Epsilon],maxterms++];
	partsumseq
]

(*Sequence of arrows tracing the path*)
Clear@taylorPath
taylorPath[args:({f_,z_/;z\[Element]Complexes,maxterms_/;maxterms\[Element]Integers&&maxterms>=0}|{f_,z_/;z\[Element]Complexes,\[Infinity],Optional[\[Epsilon]_,0.05]})]:=Partition[Prepend[taylorSeq@args,{0,0}],2,1]

(*path to terms*)
Clear@pathToTerms
pathToTerms[path:{{{_,_},{_,_}}..}]:=#1+I #2&@@@path[[;;,2]]//Prepend[Differences@#,#[[1]]]&

(*Label positions*)
Clear@labelPos
labelPos[path:{{{_,_},{_,_}}..}]:=MapThread[RotationTransform[-\[Pi]/8,#1][#2]&,{path[[;;,1]],#1+(#2-#1)/2&@@@path}]

End[];
EndPackage[];
