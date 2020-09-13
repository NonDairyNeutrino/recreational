(* ::Package:: *)

BeginPackage["taylorPath`"];
(*Usage*)
taylorSeq::usage =
	"taylorSeq[{f, z, n}] gives the sequence of partial sums, up to n, of the real and imaginary parts of the Maclaurin series of f, evaluated at the complex number z.\n"<>
	"taylorSeq[{f, z, \[Infinity], \[Epsilon]}] gives the sequence of partial sums of the real and imaginary parts of the Maclaurin series of f, evaluated at the complex number z to within a tolerance of \[Epsilon].";

taylorPath::usage =
	"taylorPath[{f, z, n}] gives the list of pairs of points that trace out the path defined by the Maclaurin series, up to n terms, of f at z.\n"<>
	"taylorPath[{f, z, \[Infinity], \[Epsilon]}] gives the list of pairs of points that trace out the path defined by the Maclaurin series of f at z up to a tolerance \[Epsilon].";

pathToTerms::usage = "pathToTerms[path] gives the list of the terms of the Maclaurin series that traces out path.";

labelPos::usage = "labelPos[path] gives the list of pairs of positions that yield aesthetic labels for the vectors in taylorPath.";

taylorPathAnim::usage = "taylorPathAnim[{f, z, n}] gives an animation of the path traced out by the complex Maclaurin series of f up to n as it, effectively, moves through the complex plane approaching z.\n"<>
						"taylorPathAnim[{f, z, \[Infinity], \[Epsilon]}] gives an animation of the path traced out by the complex Maclaurin series of f to within a tolerance of \[Epsilon] as it, effectively, moves through the complex plane approaching z.";

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

(*Path animation*)
Clear@taylorPathAnim
taylorPathAnim[args:({f_,z_/;z\[Element]Complexes,maxterms_/;maxterms\[Element]Integers&&maxterms>=0}|{f_,z_/;z\[Element]Complexes,\[Infinity],Optional[\[Epsilon]_,0.05]})]:=ListAnimate@Block[
	{path=taylorPath@args,labels=pathToTerms@path,labelplaces=labelPos@path},
	Table[
		Graphics[
			{Thick,
				{Darker[Orange,1/5],Circle[]},
				{Arrow@path[[;;n]]},
				##&@@MapThread[Inset[Style[TraditionalForm@Chop@N@#1,12,Black],#2]&,{labels,labelplaces}[[;;,;;n]]],
				{Red,Arrow@{{0,0},ReIm[args[[1]][args[[2]]]]}}
			},
			Axes->True,
			PlotLabel->Style["Vector series of"<>ToString[args[[1]][args[[2]]],TraditionalForm],15,Black],
			PlotRange->Max@Abs@Accumulate@labels
		],
	{n,Length@path}
	]
]

End[];
EndPackage[];
