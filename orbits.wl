(* ::Package:: *)

pointA[\[Omega]A_,t_]:={Cos[\[Omega]A t],Sin[\[Omega]A t]};
pointB[\[Omega]B_,t_]:=2 {Cos[\[Omega]B t],Sin[\[Omega]B t]};
Orbit[\[Omega]A_,\[Omega]B_,tt_] := ListAnimate[
	ParallelTable[
		Graphics[
			{
				Text["The Sun", {0, 0}],
				(*Circle A*) {Circle[{0, 0}, 1]},
				(*Circle B*) {Circle[{0, 0}, 2]},
				(*Points*) {
					PointSize[.07],
					Point[
						{
							(*Point A*) pointA[\[Omega]A, t],
							(*Point B*) pointB[\[Omega]B, t]
						}
					],
					(*AB*) Line@{pointA[\[Omega]A, t], pointB[\[Omega]B, t]},
					Line@Table[{pointA[\[Omega]A, k], pointB[\[Omega]B, k]}, {k, 0, t, 1/2}]
				}
			},
			PlotRange->2.25,
			ImageSize->Scaled[1/4]
		],
		{t, 0, tt, tt/400}
	],
	10
]


(* ::Section:: *)
(*Examples*)


(* ::Text:: *)
(*A good "third argument" is 100.*)


(* ::Input:: *)
(*Orbit[1/2,1,100]*)


(* ::Input:: *)
(*Orbit[1,1/3,100]*)


(* ::Input:: *)
(*Orbit[1,\[Pi]/2,100]*)


(* ::Input:: *)
(*Orbit[\[Pi]/2,1,100]*)


(* ::Input:: *)
(*Orbit[1,1.01,100]*)


(* ::Input:: *)
(*Orbit[-1,1,100]*)


(* ::Input:: *)
(*Orbit[-2,1,100]*)


(* ::Input:: *)
(*Orbit[-3,1,100]*)


(* ::Input:: *)
(*Orbit[-2.5,1,100]*)


(* ::Input:: *)
(*Orbit[-1.5,1,100]*)
