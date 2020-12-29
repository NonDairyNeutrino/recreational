(* ::Package:: *)

(* ::Title:: *)
(*Rotating Square in its Circumcircle*)


(* ::Section:: *)
(*Revising 1.0*)


(* ::Text:: *)
(*Make the original recreation code better.*)


(* ::Subsection:: *)
(*Code*)


(*Points that make up the cirlce*) circpts={Cos[#],Sin[#]}&@Range[0,2\[Pi],\[Pi]/16]//Transpose;


(*Let's make a square*)
(*Left side of the square*)leftpts=Thread@{-0.5,Range[-0.5,0.5,.125]};
sqrpts=Catenate@(*Rotate most the left side of the square to form the rest of the sides*)Table[RotationMatrix[k \[Pi]/2].#&/@Most@leftpts,{k,0,3}];
rotsqr[t_]=RotationMatrix[t].#&/@sqrpts;


(* ::Subsection:: *)
(*Visualizations*)


(* ::Text:: *)
(*Static picture of the system.*)


Graphics[
	{
		PointSize[0.025],
		Thickness[.007],
		(*Circle*) {Orange,Point[circpts]},
		(*Square*) {Yellow,Point@sqrpts},
		(*Lines*) {Thickness[.007],RGBColor[0,.5,.5],Line@Transpose@{N@circpts,Join@@@Nearest[sqrpts,circpts]}}
	},
	Background->Darker[RGBColor[0,.25,.35],3/5],
	PlotRangePadding->0.1
]


(* ::Text:: *)
(*Interactive version exploring if the rotations have different frequencies.*)


Manipulate[
	If[
		spiro,
		Graphics[
			{
				PointSize[0.025],
				Thickness[.007],
				(*Circle*) {Orange,Point[RotationMatrix[\[Omega]circ t].#&/@circpts]},
				(*Square*) {Yellow,Point@rotsqr[-\[Omega]sqr t]},
				(*Lines*) {Thickness[.007],RGBColor[0,.5,.5],Line@Transpose[{#,Join@@@Nearest[rotsqr[-\[Omega]sqr t],#]}&[RotationMatrix[\[Omega]circ t].#&/@circpts]]}
			},
			Background->Darker[RGBColor[0,.25,.35],3/5],
			PlotRangePadding->0.1
		],
		Graphics[
			{
				PointSize[0.025],
				Thickness[.007],
				(*Square*) {Yellow,Point@rotsqr[-\[Omega]sqr t]},
				(*Lines*) {Thickness[.007],RGBColor[0,.5,.5],Line@Transpose[{circpts,Join@@@Nearest[rotsqr[-\[Omega]sqr t],#]}&[RotationMatrix[\[Omega]circ t].#&/@circpts]]}
			},
			Background->Darker[RGBColor[0,.25,.35],3/5],
			PlotRangePadding->0.1
		]
	],
	{spiro,{True,False}},
	{t,0,2\[Pi],Animator},
	{{\[Omega]circ,1},-1,1},
	{{\[Omega]sqr,1},-1,1}
]


(* ::Text:: *)
(*Gif code*)


(* ::Input:: *)
(*With[*)
(*	{\[Omega]circ=1,\[Omega]sqr=-1},*)
(*	Table[*)
(*		Graphics[*)
(*			{*)
(*				PointSize[0.025],*)
(*				Thickness[.007],*)
(*				(*Square*) {Yellow,Point@rotsqr[-\[Omega]sqr t]},*)
(*				(*Lines*) {Thickness[.007],RGBColor[0,.5,.5],Line@Transpose[{circpts,Join@@@Nearest[rotsqr[-\[Omega]sqr t],#]}&[RotationMatrix[\[Omega]circ t].#&/@circpts]]}*)
(*			},*)
(*			Background->Darker[RGBColor[0,.25,.35],3/5],*)
(*			PlotRangePadding->0.1*)
(*		],*)
(*		{t,0.,2\[Pi],2\[Pi]/100}*)
(*	]*)
(*];*)
(*ListAnimate@%*)
(*(*SystemOpen@Export["C:\\Users\\Nathan Chapman\\Desktop\\Aperture.gif",%,"DisplayDurations"\[Rule]3./100]*)*)


(* ::Section::Closed:: *)
(*Accurate Recreation*)


(* ::Text:: *)
(*Actually make the inner dots move in circles*)
