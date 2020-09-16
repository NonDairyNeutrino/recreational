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

(*Animation*)


End[];
EndPackage[];
