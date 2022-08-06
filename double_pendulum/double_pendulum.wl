(* ::Package:: *)

BeginPackage["doublePendulum`"]

(*Usage*)
tf::usage = "tf[expr] gives the form of expr collected in powers of m and g.";
notation::usage = "notation[expr] gives expr formatted with the canonical notation found in most physics texts.";
height::usage = "height[k][t] gives the height of mass k at time t relative to the topmost anchor point.";
pe::usage = "pe[k][t] gives the potential energy of mass k at time t.";
petot::usage = "petot[t] gives the total potential energy of the system at time t.";
velocity::usage = "velocity[k][t] gives the velocity of mass k at time t with respect to the topmost anchor point.";
ke::usage = "ke[k][t] gives the kinetic energy of mass k at time t with respect to the topmost anchor point.";
ketot::usage = "ketot[t] gives the total kinetic energy of the system at time t.";
lagrangian::usage = "lagrangian[t] gives the Lagrangian of the system at time t.";
lagapp::usage = "lagapp[t] gives the Lagrangian of the system at time t under the small-angle approximation .";
condsToAngs::usage = "condsToAngs[initconds : {\[Phi]10, \[Omega]10, \[Phi]20, \[Omega]20}, params : {grav, len1, m1, len2, m2}] gives the angles that each rod makes with the vertical as a function of time given initial conditions conds and parameters params.";
pen::usage = "pen[angvec : {\[Phi]1t, \[Phi]2t}][time] gives the positions of the masses.";
penplot::usage = "penplot[penvec : {pen1pos, pen2pos}] gives a diagram of the double pendulum for specific positions of the masses.";
traceplot::usage = "traceplot[pnts, {ind}] gives a plot of the path traced out by the bottom mass.";
tracepenplot::usage = "tracepenplot[{pen1pos, pen2pos}, pnts, {ind}] gives a diagram of the pendulum with the bottom mass path traced out.";
discretePen::usage = "discretePen[initconds : {\[Phi]10, \[Omega]10, \[Phi]20, \[Omega]20}, params : {grav, len1, m1, len2, m2}, times] is an all-in-one function that goes from initial conditions and parameters of the double pendulum to the analytic solution evaluated at given times."

Begin["funcs`"]

Needs["VariationalMethods`"]
$Assumptions={(\[Phi]vec[1][t]|\[Phi]vec[2][t])\[Element]Vectors[2]};

Clear[tf,notation]
tf[expr_]:=Collect[Expand@expr,{_m,g},Simplify]
notation[expr_]:=expr/.{\[Phi][x_][t]:>Subscript[\[Phi], x],\[Phi][x_]'[t]:>Subscript[
\!\(\*OverscriptBox[\(\[Phi]\), \(.\)]\), x],\[Phi][x_]''[t]:>Subscript[
\!\(\*OverscriptBox[\(\[Phi]\), \("\<..\>"\)]\), x],len[x_]:>Subscript[L, x],m[x_]:>Subscript[m, x]}//TraditionalForm

(*Heights*)
Clear@height
height[1][t_]=len[1]-len[1]Cos[\[Phi][1][t]];
height[2][t_]=len[1]+len[2]-(len[1]Cos[\[Phi][1][t]]+len[2]Cos[\[Phi][2][t]]);

(*Potential energies*)
Clear[pe,petot]
pe[k_][t_]:= m[k] g height[k][t]
petot[t_]=pe[1][t]+pe[2][t]//tf (*2 Sin[t/2]^2 \[Equal] 1-Cos[t]*);

(*Angle vector relation*)
ClearAll@\[Phi]vec
\[Phi]vec[k_][t_].\[Phi]vec[j_][t_]^:=Cos[\[Phi][k][t]-\[Phi][j][t]]

(*Velocities*)
Clear@velocity
velocity[k_][t]:=len[k]\[Phi][k]'[t]\[Phi]vec[k][t]
velocity[2][t]=v[1][t]+v[2][t];

(*Kinetic Energies*)
Clear[ke,ketot]
ke[k_][t_]:=1/2 m[k](#.#&@v[k][t])//TensorExpand
ketot[t_]=ke[1]+ke[2];

(*Lagrangians*)
Clear[lagrangian,lagapp]
lagrangian[t_]=ketot[t]-petot[t];
lagapp[t_]=lagrangian[t]//.{(*(*Trig Identity*)Sin[x_/2]^2\[RuleDelayed](1-Cos[x])/2,*)(*Small Angle Approximation*)Cos[x_]:>1-x^2/2,\[Phi][1][t]-\[Phi][2][t]->0}

(*Approximate equations of motion*)
eom=EulerEquations[lagapp,{\[Phi][1][t],\[Phi][2][t]},t]

(*Solution and Animation*)

(*Solution from intiial conditions and parameters*)
condsToAngs[
  initconds : {\[Phi]10_, \[Omega]10_, \[Phi]20_, \[Omega]20_},
  params : {grav_, len1_, m1_, len2_, m2_}
  ] := Block[
  {\[Phi]},
  DSolveValue[
    eom~Join~Thread[{\[Phi][1][0], \[Phi][1]'[0], \[Phi][2][0], \[Phi][2]'[0]} == initconds],
    {\[Phi][1][t], \[Phi][2][t]},
    t
  ] /. Thread[{g, len[1], m[1], len[2], m[2]} -> params]
]

(*Positions of the masses*)
pen[angvec : {\[Phi]1t_, \[Phi]2t_}][time_] := {
  {Cos[#1], Sin[#1]},
  {Cos[#2], Sin[#2]}
}.RotationMatrix[\[Pi]/2] &[\[Phi]1 t, \[Phi]2 t] /. t -> time // Accumulate // Chop

(*Diagram of the double pendulum for specific positions of the masses*)
penplot[penvec : {pen1pos_, pen2pos_}] := Graphics[
  {
   (*ceiling*){Thick, White, Line@{{-2, 0}, {2, 0}}},
   (*rods*){Thick, White, Line@{{{0, 0}, pen1pos}, {pen1pos, pen2pos}}},
   (*points*){PointSize[0.03], Red, Point@{pen1pos, pen2pos}}
  },
  PlotRange -> {{-2, 2}, {0.1, -2}},
  Background -> Black
]

(*Plot of the path traced out by the bottom mass*)
Clear@traceplot
traceplot[pnts_, {ind_}] := Graphics[{Thickness[0.0025], Darker[Green, 3/5], Line@pnts[[;; ind, 2]]}]

(*Diagram of the pendulum with the bottom mass path traced out*)
Clear@tracepenplot
tracepenplot[{pen1pos_, pen2pos_}, pnts_, {ind_}] := Show[
  penplot@{pen1pos, pen2pos},
  traceplot[pnts, {ind}]
]

(*"All in one" function that goes from initial conditions and parameters of the double pendulum to the analytic solution evaluated at given times*)
Clear@discretePen
discretePen[
  initconds : {\[Phi]10_, \[Omega]10_, \[Phi]20_, \[Omega]20_},
  params : {grav_, len1_, m1_, len2_, m2_}, times_] :=
 Evaluate[pen[condsToAngs[initconds, params]]] /@ times

(*Create the frames of the animation and animate it*)
SetSharedVariable@k; k = 0;
Monitor[
 ListAnimate[
  plots = Parallelize[
    MapIndexed[
     (k++; Rasterize@tracepenplot[#1, pnts, #2]) &,
     pnts
     ],
    Method -> "CoarsestGrained"
    ],
  DefaultDuration -> 5
  ],
 ProgressIndicator[k, {0, Length@pnts}]
 ]

 (*Still image of just the traced path*)
 With[
  {initconds = {\[Pi]/4, -11.5, -\[Pi]/6, -5}, params = {9.8`, 1, 2, 1, 1},
  times = Range[0.01, 20 \[Pi], 20 \[Pi]/(60(*fps*)*60(*length*))]},
  Show[
    traceplot[discretePen[initconds, params, times], {All}],
    Background -> Black
  ] // Rasterize[#, RasterSize -> 1000, ImageSize -> 72*10] &
]

End[];
EndPackage[];
