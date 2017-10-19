(* ::Package:: *)

BeginPackage["DeSitterModel`"]

CreateCSSeriesDeSitterTransReduction::usage="CreateCSSeriesDeSitterTransReduction[\[Lambda],\[Tau],n,d]"

Begin["`Private`"]

CreateCSSeriesDeSitter[lambda_,tau_,n_,d_]:=Module[{R,t,r,limits,pts,pairs,graphs,metric,reg,omegalist,freg,preg,Region},
metric=Flatten[{1,ConstantArray[-1,d]}];
R=Sqrt[3/lambda];
t=Table[RandomVariate[UniformDistribution[{-R Sinh[tau/R],R  Sinh[tau/R]}]],{i,n}];
r=Sqrt[t^2+1];
limits=Flatten[{{{-1,1}},Table[{0,\[Pi]},{i,d-2}],{{0,2\[Pi]}}},1];
pts=SortBy[RandomPoint[Sphere[d],n],First];
pts=Table[Flatten[{t[[i]],Sqrt[t[[i]]^2+1] pts[[i]]}],{i,n}];pairs=Select[ Tuples[pts,{2}],#[[All,1]][[1]]<#[[All,1]][[2]]&];
graphs=Graph[Rule@@@pairs[[Flatten[Position[Total[(#[[2]]-#[[1]])^2.metric]&/@pairs,_?(#>= 0&)]]]]];
graphs=Table[Subgraph[graphs,Subsets[Take[pts,i],{2}]],{i,1,Length[pts]}];
AdjacencyMatrix[#]&/@Select[graphs,EmptyGraphQ[#]==False&]
]

CreateCSSeriesDeSitterTransReduction[lambda_,tau_,n_,d_]:=Module[{R,t,r,limits,pts,pairs,graphs,metric,reg,omegalist,freg,preg,Region},
metric=Flatten[{1,ConstantArray[-1,d]}];
R=Sqrt[3/lambda];
t=Table[RandomVariate[UniformDistribution[{-R Sinh[tau/R],R  Sinh[tau/R]}]],{i,n}];
r=Sqrt[t^2+1];
limits=Flatten[{{{-1,1}},Table[{0,\[Pi]},{i,d-2}],{{0,2\[Pi]}}},1];
pts=SortBy[RandomPoint[Sphere[d],n],First];
pts=Table[Flatten[{t[[i]],Sqrt[t[[i]]^2+1] pts[[i]]}],{i,n}];pairs=Select[ Tuples[pts,{2}],#[[All,1]][[1]]<#[[All,1]][[2]]&];
graphs=Graph[Rule@@@pairs[[Flatten[Position[Total[(#[[2]]-#[[1]])^2.metric]&/@pairs,_?(#>= 0&)]]]]];
graphs=Table[Subgraph[graphs,Subsets[Take[pts,i],{2}]],{i,1,Length[pts]}];
AdjacencyMatrix[TransitiveReductionGraph[#]]&/@Select[graphs,EmptyGraphQ[#]==False&]
]

End[]

EndPackage[]
