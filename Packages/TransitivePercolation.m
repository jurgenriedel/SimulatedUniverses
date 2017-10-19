(* ::Package:: *)

BeginPackage["TransitivePercolation`"]

GenerateTranisitivePercolationCausalSets::usage="GenerateTranisitivePercolationCausalSets[n, p] : n = iteration, p = probability."

Begin["`Private`"]

AddTransivity[edges_]:=Module[{edgelist},edgelist=List@@@edges;If[Length[edgelist]>1,Union[Rule@@@Flatten[Union[Table[Table[Table[Flatten[{h,i}][[{1,4}]],{i,Select[edgelist,#[[1]]==k &]}],{h,Select[edgelist,#[[2]]==k &]}],{k,Union[edgelist[[All,1]]]}]],2],edges],edges]]

GenerateTransitivePercolationCausalSet[g_,newvertex_,p_]:=Module[{vertices,edges,edges2,pairs,selection,poslist},
vertices=VertexList[g];
edges=EdgeList[g];
pairs=Tuples[{vertices,newvertex}];
poslist=Flatten[Position[RandomVariate[BernoulliDistribution[p],Length[pairs]],1]];
edges=Union[Rule@@@(pairs[[#]]&/@poslist),edges];
vertices=Union[vertices,newvertex];
TransitiveReductionGraph[Graph[vertices,edges]]
]

GenerateTranisitivePercolationCausalSets[iter_,p_]:=Module[{g, glist},
g=Graph[{1},{}];glist={};For[i=2,i<iter+2,i++,g=GenerateTransitivePercolationCausalSet[g,{i},p];AppendTo[glist,AdjacencyMatrix[g]]]; glist]


End[]

EndPackage[]
