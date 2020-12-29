(* ::Package:: *)

(* ::Title:: *)
(*Hot Ones Sauces: An Analysis*)


SetDirectory@NotebookDirectory[]


SetOptions[ListPlot,BaseStyle->{FontSize->11},PlotStyle->{PointSize[.025]},PlotRange->All,Frame->True,FrameTicksStyle->Black,FrameLabel->Evaluate[Style[#,11,Black]&/@{"Board position","Spiciness (kSHU)"}]];


(* ::Section:: *)
(*Getting the sauce data*)


Clear@data
data=Import["https://en.wikipedia.org/wiki/Hot_Ones","Data"];


Cases[data,{{"#","Sauce","Scoville units"},sauces__}:>{sauces},\[Infinity]]/.{pos_Integer,name_String,scolville_}:>{pos,name,ToExpression[StringReplace[ToString@scolville,","|"+"->""]]/1000.};
Clear@sauces;sauces=Transpose[{Array["Season "<>ToString@#&,9],%}]


(* ::Section:: *)
(*Sauce heat per season*)


(* ::Subsection:: *)
(*Individual Seasons*)


(* ::Subsubsection:: *)
(*Sauce Lineup*)


(*Export[
StringReplace[#\[LeftDoubleBracket]1\[RightDoubleBracket]," "\[Rule]"_"]<>".png",*)
ListPlot[
	List/@#[[2,;;,{1,3}]],
	PlotLabel->Style[#[[1]],11,Black],
	PlotLegends->(#1<>" ("<>ToString[#2]<>" kSHU)"&@@@#[[2,;;,2;;]]),
	ImageMargins->20
]~Rasterize~{ImageSize->72*15,RasterSize->2500}(*,
"Image",
ImageSize\[Rule]72*15
]*)&/@sauces


(* ::Subsubsection:: *)
(*Average Heat per Season*)


ListPlot[
	List/@#,
	PlotLabel->Style["Average heat per season",12,Black],
	FrameTicks->{{Automatic,Automatic},{Range[9],Automatic}},
	FrameLabel->Evaluate[Style[#,11,Black]&/@{"Season","Spiciness (kSHU)"}],
	PlotLegends->("Season "<>ToString[#1]<>" ("<>ToString[#2]<>" kSHU)"&@@@#),
	ImageMargins->20
]~Rasterize~{ImageSize->72*12,RasterSize->2500}&@Transpose@{Range[9],Mean/@sauces[[;;,2,;;,3]]}
(*Export["Average_heat_season.png",%]*)
(*SystemOpen@%*)


(* ::Subsection:: *)
(*Sauce Lineup - All Seasons (Poster)*)


ListPlot[
	List/@#[[2,;;,{1,3}]],
	BaseStyle->{FontSize->11},
	PlotStyle->{PointSize[.025]},
	PlotRange->All,
	Frame->True,
	FrameTicksStyle->Black,
	FrameLabel->Evaluate[Style[#,11,Black]&/@{"Board position","Spiciness (kSHU)"}],
	PlotLabel->Style[#[[1]],11,Black],
	PlotLegends->(#1<>" ("<>ToString[#2]<>" kSHU)"&@@@#[[2,;;,2;;]]),
	ImageMargins->20
]~Rasterize~{RasterSize->1000}&~ParallelMap~sauces//Partition[#,3]&//GraphicsGrid[#,Spacings->0,Alignment->Left]&//Rasterize[#,RasterSize->3000,ImageSize->72*36]&
(*Export["Hot_Ones_Sauces.png",%,ImageSize->72*36]*)
(*SystemOpen@%*)


(* ::Section:: *)
(*Heat per indexed sauce*)


(* ::Subsection:: *)
(*Average of indexed sauces over all seasons*)


Clear@stats;stats=Mean/@Transpose@sauces[[;;,2,;;,3]]


ListPlot[
	List/@#,
	PlotLabel->Style["Average heat of each sauce",12,Black],
	FrameTicks->{{Automatic,Automatic},{Range[10],Automatic}},
	FrameLabel->Evaluate[Style[#,11,Black]&/@{"Sauce","Spiciness (kSHU)"}],
	PlotLegends->("Sauce "<>ToString[#1]<>" ("<>ToString[#2]<>" kSHU)"&@@@#),
	ImageMargins->20
]~Rasterize~{ImageSize->72*12,RasterSize->2500}&@Transpose@{Range[10],stats}
(*Export["Average_heat_sauce.png",%]*)
(*SystemOpen@%*)
