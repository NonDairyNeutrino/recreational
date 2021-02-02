(* ::Package:: *)

(* ::Title:: *)
(*Visualizing Complex Mappings*)


(* ::Subtitle:: *)
(*By Nathan Chapman*)


(* ::Text:: *)
(*In this notebook is an inteactive and dynamic visualization tool to help see how complex mappings change a set.  It allows for the input of a function of your choosing, and the domain to be parametrized either in Cartesian or Polar coordinates.  *)
(*For Cartesian coordinates, there are sliders that allow the control of the upper and lower bounds for the real and imaginary axes.  The biggest domain is the square with side length \[Pi].*)
(*For Polar coordinates, there are sliders that allow the control of the upper and lower bounds for the radius and the angle.  The biggest domain is the disk of radius 2.*)
(*As the boundary of the domain changes, the title of the visualization also dynamically changes to show the traditional form of the function applied to either the Cartesian form of the complex number, or the polar form, and the corresponding set of the domain and range.*)
(**)
(*Another key and helpful product of this visualization is the mesh that is overlayed onto each of the plots shows the contours and the mappings of such contours.  This allows for a more in-depth understanding of how these functions map such contours.  For example vertical and horizontal lines, as well as contours of angle and radius.*)
(**)
(*Note: The function that is input needs to either be a function that is already in Mathematica (i.e. a symbolic function) (e.g. Cos), or in pure function form (e.g. #^2 &).*)
(*Note: The graphic is automatically programmed to output and dynamically change to be one half of the width of the Mathematica window.*)
(*Note: Once the function cell has been run, to overwrite the plots to a new function simply press Enter not Shift+Enter.  If Shift+Enter is pressed a new new cell will be created with the new plot.*)
(*Note: If you see your plot has a darker color in some spots, that indicates an "overlap" of the function. i.e. the function is no longer one-to-one.*)


(* ::Section:: *)
(*Plot Options*)


SetOptions[ParametricPlot, AspectRatio->1, Mesh->True, MeshStyle->{Darker[Green,3/5],Red}];
SetOptions[GraphicsRow, ImageSize->Scaled[1/2]];


(* ::Section:: *)
(*Cartesian*)


cartesianPlots[f_, xbounds : {xl_, xu_} /; xl <= xu, ybounds : {yl_, yu_} /; yl <= yu] := ParametricPlot[
	#1,
	{x, xl, xu},
	{y, yl ,yu},
	PlotLabel->#2,
	PlotRange->#3
]&@@@{{{x, y}, "Domain", {-\[Pi],\[Pi]}}, {ReIm@f[x + I y], "Range", All}} // GraphicsRow


label[f_, xbounds : {xl_, xu_} /; xl <= xu, ybounds : {yl_, yu_} /; yl <= yu] := StringJoin[
	ToString[f[x+I y],TraditionalForm],
	":{(x,y):",
	ToString[xl,TraditionalForm],
	"< x <",
	ToString[xu,TraditionalForm],
	",",
	ToString[yl,TraditionalForm],
	"< y <",
	ToString[yu,TraditionalForm],
	"}"
] \[RightTeeArrow] StringJoin[
	"{(u,v):",
	ToString[Evaluate[Re[f[xl]]],TraditionalForm],
	"< u <",
	ToString[Evaluate[Re[f[xu]]],TraditionalForm],
	",",
	ToString[Evaluate[Im[f[yl]]],TraditionalForm],
	"< v <",
	ToString[Evaluate[Im[f[yu]]],TraditionalForm],
	"}"
]


(*Good*)
Show[cartesianPlots@##, PlotLabel->label@##]&@@{Sqrt, {-\[Pi],\[Pi]}, {-\[Pi],\[Pi]}}


(*Not good*)
Manipulate[
	Show[cartesianPlots@##, PlotLabel->label@##]&@@{Sqrt, {-xl,xu}, {-yl,yu}}//Evaluate,
	{{xl,-\[Pi],"Domain x Lower Bound"},-\[Pi],-0.01},
	{{xu,0.01,"Domain x Upper Bound"},0.01,\[Pi]},
	{{yl,-\[Pi],"Domain y Lower Bound"},-\[Pi],-0.01},
	{{yu,0.01,"Domain y Upper Bound"},0.01,\[Pi]}
]


(* ::Input:: *)
(*Manipulate[*)
(*If[Chart=="Cartesian",*)
(*(*Cartesian*)*)
(*Manipulate[*)
(*GraphicsRow[*)
(*{*)
(*(*Domain*)*)
(*ParametricPlot[*)
(*{x,y},*)
(*{x,dxl,dxu},*)
(*{y,dyl,dyu},*)
(*PlotLabel->"Domain",*)
(*PlotRange->{{-\[Pi],\[Pi]},{-\[Pi],\[Pi]}},*)
(*AspectRatio->1,*)
(*Mesh->True,*)
(*MeshStyle->{Darker[Green,3/5],Red}*)
(*],*)
(*(*Range*)*)
(*ParametricPlot[*)
(*{Re[#],Im[#]}&@f[x+I y],*)
(*{x,dxl,dxu},*)
(*{y,dyl,dyu},*)
(*PlotRange->All,*)
(*PlotLabel->"Range",*)
(*AspectRatio->1,*)
(*Mesh->True,*)
(*MeshStyle->{Darker[Green,3/5],Red}*)
(*]*)
(*},*)
(*ImageSize->Scaled[1/2],*)
(*PlotLabel->ToString[f[x+I y],TraditionalForm]<>":{(x,y):"<>ToString[dxl]<>"< x <"<>ToString[dxu]<>","<>ToString[dyl]<>"< y <"<>ToString[dyu]<>"}"\[RightTeeArrow]"{(u,v):"<>ToString[Evaluate[Re[f[dxl]]],TraditionalForm]<>"< u <"<>ToString[Evaluate[Re[f[dxu]]],TraditionalForm]<>","<>ToString[Evaluate[Im[f[dyl]]]]<>"< v <"<>ToString[Evaluate[Im[f[dyu]]]]<>"}"*)
(*],*)
(*{{dxl,-\[Pi],"Domain x Lower Bound"},-\[Pi],-0.01},*)
(*{{dxu,0.01,"Domain x Upper Bound"},0.01,\[Pi]},*)
(*{{dyl,-\[Pi],"Domain y Lower Bound"},-\[Pi],-0.01},*)
(*{{dyu,0.01,"Domain y Upper Bound"},0.01,\[Pi]}*)
(*],*)
(*(*Polar*)*)
(*Manipulate[*)
(*GraphicsRow[*)
(*{*)
(*(*Domain*)*)
(*ParametricPlot[*)
(*r{Cos[\[Theta]],Sin[\[Theta]]},*)
(*{r,rl,ru},*)
(*{\[Theta],\[Theta]l,\[Theta]u},*)
(*PlotRange->{{-2,2},{-2,2}},*)
(*PlotLabel->"Domain",*)
(*AspectRatio->1,*)
(*Mesh->True,*)
(*MeshStyle->{Darker[Green,3/5],Red}*)
(*],*)
(*(*Range*)*)
(*ParametricPlot[*)
(*{Re[#],Im[#]}&@f[r Exp[I \[Theta]]],*)
(*{r,rl,ru},*)
(*{\[Theta],\[Theta]l,\[Theta]u},*)
(*PlotRange->All,*)
(*PlotLabel->"Range",*)
(*AspectRatio->1,*)
(*Mesh->True,*)
(*MeshStyle->{Darker[Green,3/5],Red}*)
(*]*)
(*},*)
(*ImageSize->Scaled[1/2],*)
(*PlotLabel->ToString[f[r Exp[I \[Theta]]],TraditionalForm]<>":{(r,\[Theta]):"<>ToString[rl]<>"< r <"<>ToString[ru]<>","<>ToString[\[Theta]l]<>"< \[Theta] <"<>ToString[\[Theta]u]<>"}"\[RightTeeArrow]"{(u,v):"<>ToString[Evaluate[Re[f[rl]]],TraditionalForm]<>"< u <"<>ToString[Evaluate[Re[f[ru]]],TraditionalForm]<>","<>ToString[Evaluate[Im[f[\[Theta]l]]]]<>"< v <"<>ToString[Evaluate[Im[f[\[Theta]u]]]]<>"}"*)
(*],*)
(*{{rl,0.01,"Radius Lower Bound"},0.01,.99},*)
(*{{ru,1,"Radius Upper Bound"},1,2},*)
(*{{\[Theta]l,0.01,"\[Theta] Lower Bound"},0.01,\[Pi]},*)
(*{{\[Theta]u,\[Pi],"\[Theta] Upper Bound"},\[Pi],2\[Pi]}*)
(*]*)
(*],*)
(*{f,Exp},*)
(*{Chart,{"Cartesian","Polar"}}*)
(*]*)
