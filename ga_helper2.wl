(* ::Package:: *)

BeginPackage["GA`"];

GenerateNullStrategy::usage="";
MultiPopulationGAStep::usage="";
MultiPopulationSequentialGAStep::usage="";
SelectPhenotypes::usage="";
EvaluateStrategy::usage="";
GAStep::usage="";
AddElitism::usage="";
WipeFitness::usage="";
MixedMethod::usage="";
ProbabilisticOccurance::usage="";
RouletteSelection::usage="";
RouletteSelectionZeroed::usage="";
RouletteSelectionRank::usage="";
RouletteSelectionExponentialRank::usage="";
KeepBestSurvival::usage="";
GenerationalSurvival::usage="";
TournamentSelection::usage="";

Begin["`Private`"];

GenerateNullStrategy[]=
   <|"Genotype"->Null,
     "Phenotype"->Null,
     "Fitness"->Null,
     "Age"->0|>;

MultiPopulationGAStep[StrategySelection_,Fitness_][individualGAs_List]:=MultiPopulationGAStep[individualGAs,StrategySelection,Fitness];
MultiPopulationGAStep[individualGAs_List,StrategySelection_,GroupOutcome_]:=
   Module[{newIndividualGAs,selectedPhenotypes,newStrats,outcome,IndividualEvaluateStrategy},
      selectedPhenotypes=SelectPhenotypes[individualGAs,StrategySelection];
      newIndividualGAs=UpdateGAs[individualGAs,GroupOutcome,selectedPhenotypes];
      outcome=MultiPhenotypeGroupOutcome[GroupOutcome,selectedPhenotypes];
      {outcome,newIndividualGAs,selectedPhenotypes}
   ];
MultiPopulationSequentialGAStep[gaSets_List,StrategySelection_,PartialGroupOutcome_]:=
   Module[{newIndividualGA,GroupOutcome,selectedPhenotypes,newStrats,outcome,IndividualEvaluateStrategy,newGASets},
      selectedPhenotypes=SelectPhenotypes[#,StrategySelection]&/@gaSets;
      newGASets=Table[
         GroupOutcome=PartialGroupOutcome[If[Length[gaSets]>2,Join,Identity]@@Delete[selectedPhenotypes,gaNum]];
         newIndividualGA=UpdateGAs[gaSets[[gaNum]],GroupOutcome,selectedPhenotypes[[gaNum]]];
         selectedPhenotypes[[gaNum]]=SelectPhenotypes[newIndividualGA,StrategySelection];
         newIndividualGA
         ,{gaNum,1,Length[gaSets]}
      ];
      outcome=MultiPhenotypeGroupOutcome[GroupOutcome,Last@selectedPhenotypes];
      {outcome,newGASets,selectedPhenotypes}
   ];

UpdateGAs[gas_,GroupOutcome_,selectedPhenotypes_]:=
  Module[{newStrats,IndividualEvaluateStrategy},
    ParallelTable[
      IndividualEvaluateStrategy=EvaluateStrategy[participant["GenotypeExpression"],IndividualPhenotypeFitness[GroupOutcome,selectedPhenotypes,participant["ID"]]];
      newStrats=participant["Strategies"];
      newStrats[[All,"Fitness"]]=Null;
      newStrats=participant["NextPopSansFitness"][newStrats,IndividualEvaluateStrategy];
      Append[participant,"Strategies"->newStrats]
      ,{participant,gas}
    ]
  ];

IndividualPhenotypeFitness[GroupOutcome_,selectedPhenotypes_,id_][phenotype_]:=
  Module[{replacedPhenotypes,phenotypeCombinations},
    replacedPhenotypes=Append[selectedPhenotypes,id->{phenotype}];
    MultiPhenotypeGroupOutcome[GroupOutcome,replacedPhenotypes][id]
  ];

MultiPhenotypeGroupOutcome[GroupOutcome_,selectedPhenotypes_]:=
  Module[{phenotypeCombinations,outcomes},
    phenotypeCombinations=AssociationThread[Keys[selectedPhenotypes]->#]&/@Transpose[CycleMatrixPad[Values[selectedPhenotypes]]];
    outcomes=GroupOutcome/@phenotypeCombinations;
    CombineNestedAssoc[outcomes]
  ];

CycleList[list_,n_]:=(Join@@Table[list,{Ceiling[n/Length[list]]}])[[;;n]];
CycleMatrixPad[mat_]:=
  Module[{max=Max[Length/@mat]},
    CycleList[#,max]&/@mat
  ];

CombineNestedAssoc[assoc_List]:=
  Module[{},
    Merge[assoc,If[Head[#[[1]]]===Association,CombineNestedAssoc[#],Mean[#]]&]
  ];

SelectPhenotypes[individualGAs_,StrategySelection_]:=Association[SelectPhenotype[#,StrategySelection]&/@individualGAs];
SelectPhenotype[individualGA__,StrategySelection_]:=
   With[{selectedStrategies=StrategySelection[individualGA["Strategies"]]},
      (individualGA["ID"]->selectedStrategies[[All,"Phenotype"]])
   ];

GAStep[Crossover_,Mutation_,ParentSelection_,SurvivalSelection_,nChildren_][strategies_,EvaluateStrategy_]:=GAStep[strategies,EvaluateStrategy,Crossover,Mutation,ParentSelection,SurvivalSelection,nChildren];
GAStep[Crossover_,Mutation_,ParentSelection_,SurvivalSelection_,nChildren_,EvaluateStrategy_][strategies_]:=GAStep[strategies,EvaluateStrategy,Crossover,Mutation,ParentSelection,SurvivalSelection,nChildren];
GAStep[strategies_,EvaluateStrategy_,Crossover_,Mutation_,ParentSelection_,SurvivalSelection_,nChildren_]:=
   Module[{newPopulation},
      newPopulation=EvaluateStrategy/@strategies;
      newPopulation=AddChildren[newPopulation,nChildren,Crossover,Mutation,ParentSelection,EvaluateStrategy];
      newPopulation=SurvivalSelection[newPopulation,Length[strategies]];
      newPopulation[[All,"Age"]]+=1;
      newPopulation
   ];

AddChildren[strategies_,nChildren_,Crossover_,Mutation_,ParentSelection_,EvaluateStrategy_]:=
   Module[{parents,children},
      parents=ParentSelection[strategies,Ceiling[nChildren/2]*2];
      children=Map[Reproduction[#,Crossover,Mutation,EvaluateStrategy]&,Partition[parents,2]];
      Join[strategies,Take[Flatten[children],nChildren]]
   ];

EvaluateStrategy[GenotypeExpression_,PhenotypeFitness_][strategy_]:=EvaluateStrategy[strategy,GenotypeExpression,PhenotypeFitness];
EvaluateStrategy[strategy_,GenotypeExpression_,PhenotypeFitness_]/;strategy["Fitness"]==Null&&strategy["Phenotype"]==Null:=
   With[{phenotype=GenotypeExpression[strategy["Genotype"]]},
      Join[strategy,<|"Phenotype"->phenotype,"Fitness"->PhenotypeFitness[phenotype]|>]
   ];
EvaluateStrategy[strategy_,GenotypeExpression_,PhenotypeFitness_]/;strategy["Fitness"]==Null:=
   Append[strategy,"Fitness"->PhenotypeFitness[strategy["Phenotype"]]]
EvaluateStrategy[strategy_,_,_]:=strategy;

Reproduction[parents_List,Crossover_,Mutation_,EvaluateStrategy_]:=
   Module[{children},
      children=Table[GenerateNullStrategy[],{Length[parents]}];
      children[[All,"Genotype"]]=Crossover[parents[[All,"Genotype"]]];
      children[[All,"Genotype"]]=Mutation[#]&/@children[[All,"Genotype"]];
      children=EvaluateStrategy/@children
   ];

(*AddElitism[SurvivalFunction_][strats_,nRemaining_]:=Join[SurvivalFunction[strats,nRemaining-1],MaximalBy[strats,#Fitness&,1]];*)
AddElitism[nElites_Integer:1,SurvivalFunction_][strats_,nRemaining_]:=Join[SurvivalFunction[strats,nRemaining-nElites],MaximalBy[strats,#Fitness&,nElites]];
WipeFitness[SurvivalFunction_][strats_,nRemaining_]:=
   Module[{newStrats=SurvivalFunction[strats,nRemaining]},
      Append[#,"Fitness"->Null]&/@newStrats
   ];
MixedMethod[methods_List][input__]:=RandomChoice[methods][input];
MixedMethod[probs_List->methods_List][input__]:=RandomChoice[probs->methods][input];
ProbabilisticOccurance[prob_,function_][input_]:=
   If[RandomReal[]<prob,
      function[input],
      input
   ];

RouletteSelection[strats_,nSelections_]:=RandomChoice[strats[[All,"Fitness"]]->strats,nSelections];
RouletteSelectionZeroed[strats_,nSelections_]:=
   Module[{minFit=Min[strats[[All,"Fitness"]]]},
      RandomChoice[(strats[[All,"Fitness"]]-minFit)->strats,nSelections]
   ];
RouletteSelectionRank[weight_][strats_,nSelections_]:=RouletteSelectionRank[strats,nSelections,weight];
RouletteSelectionRank[strats_,nSelections_,alpha_:1]:=
   Module[{ranks=Ordering[Ordering[strats[[All,"Fitness"]]]]-1,nStrats=Length[strats],weights,totalRanks},
      totalRanks=nStrats*(nStrats-1)/2;
      weights=alpha*ranks/totalRanks + (1-alpha)*1/nStrats;
      RandomChoice[weights->strats,nSelections]
   ];
RouletteSelectionExponentialRank[strats_,nSelections_]:=
   Module[{ranks=Ordering[Ordering[strats[[All,"Fitness"]]]]-1},
      RandomChoice[(1-E^(-ranks))->strats,nSelections]
   ];

KeepBestSurvival[strats_,nRemaining_]:=MaximalBy[strats,#Fitness&,nRemaining];
GenerationalSurvival[strats_,nRemaining_]:=MinimalBy[strats,#Age&,nRemaining];

TournamentSelection[tourneySize_][strats_,nSelections_]:=TournamentSelection[strats,nSelections,tourneySize]
TournamentSelection[strats_,nSelections_,tourneySize_:2]:=
   Module[{randPairs},
      randPairs=RandomChoice[strats,{nSelections,tourneySize}];
      KeepBestSurvival[#,1][[1]]&/@randPairs
   ];

End[];

EndPackage[];
