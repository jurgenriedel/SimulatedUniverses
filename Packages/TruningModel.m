(* ::Package:: *)

BeginPackage["TruningModel`"]

CreateTruningCSTimeSeries::usage="Create Turing model time series."

Begin["`Private`"]

CreateTruningCS[rule_,state_,color_, inits_, t_]:=With[{e=TuringMachine[{rule,state,color},{1,{IntegerDigits[inits[[10]]/.List->{},3],IntegerDigits[inits[[2000]],3]}},t]},Graph[With[{rules=MapIndexed[{First[#2],#[[1,2]],#[[2,#[[1,2]]]]}&,e]},Join[Rule@@@Partition[rules,2,1],Flatten[Rule@@@Partition[#,2,1]&/@Last@Reap[Sow[#,#[[2]]]&/@rules],1]]],DirectedEdges->True,AspectRatio->Automatic,ImageSize->{450,450}]]

CreateTruningCSTimeSeries[rule_,state_,color_,iter_]:=
Table[AdjacencyMatrix[CreateTruningCS[rule,state,color,Join[{0},Select[Range[0,6506],Mod[#,3]!=0&]],i]],{i,1,iter}]

End[]

EndPackage[]
