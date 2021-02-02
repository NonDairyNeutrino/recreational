(* ::Package:: *)

(* ::Title:: *)
(*Random Walk/Drunken Pirate*)


(*Random Step Fucntion*)
Clear@step;
(*Can move, WLOG, left then right*) step[p_,leftright,dirs:{right_,up_,down_}/;Total[dirs]==1&&Positive[dirs]==True {1,1,1}]:=Piecewise[{{{1,0},0<=p<right},{{0,1},right<=p<right+up},{{0,-1},right+up<=p<right+up+down}}];
(*Every step is a step forward*) step[p_,forward]=Piecewise[Reverse/@{{0<=p<.25,{1,-1}},{0.25<=p<0.75,{1,0}},{0.75<=p<1.,{1,1}}}];
(*Every direction is viable*) step[p_,backward]=Piecewise[Reverse/@{{0<=p<.1,{-1,0}},{0.1<=p<0.3,{0,1}},{0.3<=p<0.8,{1,0}},{0.8<=p<1.,{0,-1}}}];


path[length_, width_, method_, dirs_] := {{length, width, method, dirs}, NestWhileList[# + step[RandomReal[], method, dirs] &, {0,0}, 0 <= #[[1]] < length && Abs@#[[2]] <= width &]}


pathPlot[pathOutput : {{length_, width_, method_, dirs_}, path_}] := ListLinePlot[
	path,
	PlotRange->{{0,length},width{-1,1}},
	Axes->False,
	Frame->True,
	FrameTicks->{{#1,None},{#2,None}}&@@Range[{-width,0},{width,length},{width/2,length/10}],
	AspectRatio->width/length,
	ImageSize->10*72,
	PlotLabel->If[path[[-1, 1]] == length, "They made it!", "They fell off"]
]


walk[length_, width_, method_, dirs_] := ListAnimate@With[
	{thispath = path[length, width, method, dirs]},
	Show[
		pathPlot@thispath,
		Epilog->{PointSize[.015], Red, Point@#}
	]&/@thispath[[2]]
]


(* ::Input:: *)
(*walk[100, 10, leftright, {0.5,0.25,0.25}]*)
