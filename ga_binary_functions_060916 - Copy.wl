(* ::Package:: *)

BeginPackage["BinaryGA`"];

BinaryOnePointCrossover::usage="";
BinaryMutation::usage="";
BinaryToDecimalList::usage=""
GrayToBinary::usage="";
ScaleNum::usage="";
GenerateBinaryGenotypes::usage="";
GenerateBinaryStrategies::usage="";

Begin["`Private`"];

GenerateNullStrategy[]=
   <|"Genotype"->Null,
     "Phenotype"->Null,
     "Fitness"->Null,
     "Age"->0|>;

BinaryOnePointCrossover[geneLength_][parents_]:=BinaryOnePointCrossover[parents,geneLength];
BinaryOnePointCrossover[{val1_,val2_},geneLength_]:=
   Module[{mask1,mask2,child1,child2},
      mask1=2^(RandomInteger[{1,geneLength}])-1;
      mask2=(2^geneLength-1)-mask1;
      child1=BitOr[BitAnd[val1,mask1],BitAnd[val2,mask2]];
      child2=BitOr[BitAnd[val2,mask1],BitAnd[val1,mask2]];
      {child1,child2}
   ];

BinaryMutation[geneLength_,mutRate_][strategy_]:=BinaryMutation[strategy,geneLength,mutRate];
BinaryMutation[strat_,geneLength_,mutRate_]:=
   Module[{rand,probMutation=mutRate/geneLength},
      rand=RandomChoice[{1-probMutation,probMutation}->{0,1},{geneLength}];
      BitXor[strat,FromDigits[rand,2]]
   ];

GrayToBinary[binList_List]:=GrayToBinary[FromDigits[binList,2]];
GrayToBinary[n_Integer]:=
   Module[{res=n,mask=BitShiftRight[n]},
      While[mask!=0,
         res=BitXor[res,mask];
         mask=BitShiftRight[mask];
      ];
      res
   ];

BinaryToDecimalList[bitsPerNumber_,bounds_][num_]:=BinaryToDecimalList[num,bitsPerNumber,bounds];
BinaryToDecimalList[num_,numbersPerStrategy_,bitsPerNumber_,bounds_]:=BinaryToDecimalList[num,Table[bitsPerNumber,{numbersPerStrategy}],bounds];
BinaryToDecimalList[num_,bitsPerNumber_List,bounds:{_,_}]:=BinaryToDecimalList[num,bitsPerNumber,Table[bounds,{Length[bitsPerNumber]}]];
BinaryToDecimalList[num_,bitsPerNumber_List,bounds:{{_,_}..}]:=
   Module[{coeffs,bitsPerStrategy=Total[bitsPerNumber]},
      coeffs=PadLeft[IntegerDigits[num,2],bitsPerStrategy];
      coeffs=Internal`PartitionRagged[coeffs,bitsPerNumber];
      coeffs=GrayToBinary/@coeffs;
      coeffs=MapThread[ScaleNum,{coeffs,bitsPerNumber,bounds}]
   ];

ScaleNum[n_,geneLength_,{min_,max_}]:=N@(max-min)/(2^geneLength-1)*n+min;
GenerateBinaryGenotypes[len_,n_]:=RandomInteger[{0,2^len-1},{n}];

GenerateBinaryStrategies[geneLength_Integer,GenotypeExpression_:(Null&)][nStrategies_Integer]:=GenerateBinaryStrategies[geneLength,nStrategies,GenotypeExpression];
GenerateBinaryStrategies[geneLength_Integer,nStrategies_Integer,GenotypeExpression_:(Null&)]:=
   With[{strats=MapThread[Append[#1,"Genotype"->#2]&,{ConstantArray[GenerateNullStrategy[],nStrategies],GenerateBinaryGenotypes[geneLength,nStrategies]}]},
      Append[#,"Phenotype"->GenotypeExpression[#["Genotype"]]]&/@strats
   ];

End[];

EndPackage[];
