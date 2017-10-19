(* ::Package:: *)

BeginPackage["MinkowskiSprinklingModel`"]

CreateCSSeriesMinkowskiSeriesTransReduction::usage = "Create Minkowski Model Series."

Begin["`Private`"]

CreateCSSeriesMinkowskiSeries[n_,d_]:=Module[{s,R,metric,pts,pairs,graphs},
metric=Flatten[{1,ConstantArray[-1,d-1]}];
s= IntegerPart[n^(1/d)];
R=Cuboid[ConstantArray[0,d],ConstantArray[s,d]];
pts=SortBy[RandomPoint[R,n],First];
pairs=Select[ Tuples[pts,{2}],#[[All,1]][[1]]<#[[All,1]][[2]]&];
graphs=Graph[Rule@@@pairs[[Flatten[Position[Total[(#[[2]]-#[[1]])^2.metric]&/@pairs,_?(#>= 0&)]]]]];
graphs=Table[Subgraph[graphs,Subsets[Take[pts,i],{2}]],{i,1,Length[pts]}];
AdjacencyMatrix[#]&/@Select[graphs,EmptyGraphQ[#]==False&]];

CreateCSSeriesMinkowskiSeriesTransReduction[n_,d_]:=Module[{s,R,metric,pts,pairs,graphs},
metric=Flatten[{1,ConstantArray[-1,d-1]}];
s= IntegerPart[n^(1/d)];
R=Cuboid[ConstantArray[0,d],ConstantArray[s,d]];
pts=SortBy[RandomPoint[R,n],First];
pairs=Select[ Tuples[pts,{2}],#[[All,1]][[1]]<#[[All,1]][[2]]&];
graphs=Graph[Rule@@@pairs[[Flatten[Position[Total[(#[[2]]-#[[1]])^2.metric]&/@pairs,_?(#>= 0&)]]]]];
graphs=Table[Subgraph[graphs,Subsets[Take[pts,i],{2}]],{i,1,Length[pts]}];
AdjacencyMatrix[TransitiveReductionGraph[#]]&/@Select[graphs,EmptyGraphQ[#]==False&]];

CreateMinkowskiInterval[n_,d_,pts_]:=Module[{s,R,metric,pairs,start,source,sink,ptsfut,ptspast,ptsall,graphs},
metric=Flatten[{1,ConstantArray[-1,d-1]}];
start=RandomInteger[Length[pts]];
source=pts[[start]];
sink=pts[[RandomInteger[{start,Length[pts]}]]];
ptsfut=Select[pts,#[[1]]>source[[1]]&&#[[1]]<sink[[1]]&];
pairs=Select[ Table[{source,i},{i,ptsfut}],#[[All,1]][[1]]<#[[All,1]][[2]]&];
pairs=pairs[[Flatten[Position[Total[(#[[2]]-#[[1]])^2.metric]&/@pairs,_?(#>= 0&)]]]];
ptsfut=Union[Flatten[pairs,1]];
ptspast=Select[pts,#[[1]]<sink[[1]]&&#[[1]]>source[[1]]&];
pairs=Select[ Table[{i,sink},{i,ptspast}],#[[All,1]][[1]]<#[[All,1]][[2]]&];
pairs=pairs[[Flatten[Position[Total[(#[[2]]-#[[1]])^2.metric]&/@pairs,_?(#>= 0&)]]]];
ptspast=Union[Flatten[pairs,1]];
ptsall=Union[Intersection[ptspast,ptsfut],{source},{sink}];
pairs=Select[ Tuples[ptsall,{2}],#[[All,1]][[1]]<#[[All,1]][[2]]&];
graphs=Graph[Rule@@@pairs[[Flatten[Position[Total[(#[[2]]-#[[1]])^2.metric]&/@pairs,_?(#>= 0&)]]]]]
]

CreateMinkowskiCS[n_,d_]:=Module[{s,R,t,r,limits,pts,pairs,graphs,metric,reg,omegalist,freg,preg,Region},
metric=Flatten[{1,ConstantArray[-1,d-1]}];
s= IntegerPart[n^(1/d)];
R=Cuboid[ConstantArray[0,d],ConstantArray[s,d]];
pts=RandomPoint[R,n]
]

CreateMinkowskiIntervals[n_,d_,iter_]:=Module[{pts},
pts=CreateMinkowskiCS[n,d];
pts=SortBy[pts,First];
Select[Table[CreateMinkowskiInterval[n,d,pts],{i,iter}],!EmptyGraphQ[#]&]
]

End[]

EndPackage[]
