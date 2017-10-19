(* ::Package:: *)

BeginPackage["OrigatoryPercolation`"]

GenerateOriginayPercolationCausalSets::usage="GenerateOriginaryPercolationCausalSets[n, p] : n = iteration, p = probability."

Begin["`Private`"]

GenerateOriginayPercolationCausalSet[g_,newvertex_,p_]:=Module[{vertices,edges,pairs,selection,poslist},
vertices=VertexList[g];
edges=EdgeList[g];
pairs=Tuples[{vertices,newvertex}];
poslist=Flatten[Position[RandomVariate[BernoulliDistribution[p],Length[pairs]],1]];
While[Length[poslist]==0,poslist=Flatten[Position[RandomVariate[BernoulliDistribution[p],Length[pairs]],1]]];
edges=Union[Rule@@@(pairs[[#]]&/@poslist),edges];
vertices=Union[vertices,newvertex];
TransitiveReductionGraph[Graph[vertices,edges]]
]

GenerateOriginayPercolationCausalSets[iter_,p_]:=Module[{g, glist},
g=Graph[{1},{}];glist={};For[i=2,i<iter+2,i++,g=GenerateOriginayPercolationCausalSet[g,{i},p];AppendTo[glist,AdjacencyMatrix[g]]]; glist]


End[]

EndPackage[]
