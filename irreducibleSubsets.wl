(* ::Package:: *)

(*Question: https://mathematica.stackexchange.com/q/201947/46490*)
(*Given a set of numbers, get two lists with one being the set of numbers than can be represented as the sum of some of the original numbers, and the other being the rest of the numbers*)


(*If we take a look at FrobeniusSolve we get the description

The Frobenius equation is the Diophantine equation a1x1+\[Ellipsis]+anxn=b where the ai are positive integers, b is an integer, and a solution x1,\[Ellipsis],xn must consist of non-negative integers. For negative b there are no solutions.

Now consider FrobeniusSolve[{a1, a2, ...}, ak] yields a solution x that consists of only zeros and at least two ones (We do this check on each produced solution with the FreeQ[#, x_ /; x > 1] && Count[#, 1] > 1 & /@). The existence of this solution means that there is a subset {ai}, and i \[NotEqual] k for all i (this is where the "at least two ones" comes in), of the original list that total the number you want.

Then we have a list of Trues and Falses, but we only care if there exists at least one True. Then we use good ol' Or to test this existence since Or

evaluates its arguments in order, giving True immediately if any of them are True, and False if they are all False.

So then we can use this method to assign each number in the list a truth value on whether or not it can be represented as a sum of other numbers in the list, and gather all the Trues and Falses together to get what you want.*)


(*After shifting down, is everything no greater than zero*) greaterThanOneQ[n_] := And@@NonPositive[n-1]
(*Tests if list is {0,0,0,...,1,0,0,0...}; True is good*)notJustOne1[list:{__?NonNegative}] := Total@list>1 (*The only way to have a total of 1 is to have one 1 and the rest are 0s*)
justZerosAndOnesQ[list_] := greaterThanOneQ@list && notJustOne1@list
sortIrreducibleFirst[pairs_] := Reverse[Sort@GatherBy[pairs, First](*Collect in groups of True and False and sort by*)][[;;,;;,2]]


irreducibleSubsets[nums : {__Integer?Positive}] := {justZerosAndOnesQ /@ FrobeniusSolve[nums, #] // Apply[Or], #} & /@ nums //sortIrreducibleFirst
(*
This gives a result of the form {{irreducible nums}, {reducible nums}}.  
This algorithm is strict in that the reducible numbers can be reduced via each other; 
e.g. irreducibleSubsets@{4,1,7,5,10} returns {{4,1,7},{5,10}} where 10 is deemed reducible because 10 = 1 + 4 + 5
*)
