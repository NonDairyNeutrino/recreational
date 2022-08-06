(* ::Package:: *)

(* ::Title:: *)
(*Disjoint Subcollections*)


(* ::Section:: *)
(*Setup*)


SetOptions[Graph,VertexLabels->"Name",ImagePadding->Full];


(* ::Text:: *)
(*Inspired by this question*)


(* ::Text:: *)
(*Alternatively could be phrased as "For a given collection of sets, what are the subcollections such that each set is disjoint?"*)


Clear[lists,goal]
lists={{1,2},{1,6},{2,3},{2,5},{3,4},{3,6},{4,5},{5,6}};
goal={{{1,2},{3,4},{5,6}},{{1,2},{3,6},{4,5}},{{1,6},{2,3},{4,5}},{{1,6},{2,5},{3,4}}};


(* ::Section:: *)
(*Answer*)


Clear[djgraph,intgraph]
{djgraph,intgraph}=Graph[UndirectedEdge@@@Select[Subsets[lists,{2}],Apply@#],PlotLabel->StringDrop[ToString@#,-1]]&/@{DisjointQ,IntersectingQ};
Rasterize[GraphicsRow@%,ImageSize->72*10]
maximumDisjointSubcollections[collection_]:=With[
{graph=UndirectedEdge@@@Select[Subsets[collection,{2}],Apply@DisjointQ]//Graph},FindClique[graph,{Length@First@FindClique@graph},All]
]


(*Verbose version*)
(*maximumDisjointSubcollections[collection_]:=*)With[
{collection=lists},
Block[
{
vertexPairs=Subsets[collection,{2}],
disjointPairs,
graph,
cliqueNumber
},
disjointPairs=Select[vertexPairs,Apply@DisjointQ];
graph=UndirectedEdge@@@disjointPairs//Graph;
cliqueNumber=Length@First@FindClique@graph;
FindClique[graph,{cliqueNumber},All]
]
]
goal==(Sort/@%//Sort)


(* ::Subsection:: *)
(*Finding collections of disjoint sets*)


Sort/@maximumDisjointSubcollections@lists//Sort
%==goal


(* ::Subsection:: *)
(*Avoiding intersects*)


(* ::Text:: *)
(*Inspired by this answer*)


Sort/@FindIndependentVertexSet[intgraph,{3,Infinity},All]//Sort
%==goal
