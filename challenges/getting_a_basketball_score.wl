(* ::Package:: *)

(*Write a function that takes an integer n and outputs all the possible ways to represent n as sums of 2s and 3s.*)

(*Mathemaica version*)
Clear@twoAndThreePointers
twoAndThreePointers[n_Integer?Positive] := IntegerPartitions[n, Infinity, {3, 2}] (*Automatically gives all the possible ways to represent n as sums of twos and threes*)
(* ::Input:: *)
(*twoAndThreePointers/@{5,14,3,6,19}//Column*)

(*Longer version*)
(*Overall idea: find the number of threes that fit into n, thake them out and put them in a list, and make the rest of n a list of twos*)
Clear@basketball
basketball[n_Integer?Positive] := Join[ (*Concatenate the given lists*)
  ConstantArray[
    2,
    (n - 3 # (*Take out all the threes from n to leave only the twos*)) /
    2        (*Since there are only twos, divide by 2 to get the number of twos*)
  ]          (*list of twos*),
  ConstantArray[3, #] (*list of threes*)
] & /@ Range[
  Mod[n, 2]      (*Start the range at either 1 or 2*),
  Quotient[n, 3] (*== (n - Mod[n,3])/3 *),
  2              (*Go in steps of 2*)
]

(* ::Input:: *)
(*basketball/@{5,14,3,6,19}//Column*)

(*One line version*)
ConstantArray[2, (n - 3 #)/2]~Join~ConstantArray[3, #] & /@ Range[Mod[n, 2], Quotient[n, 3], 2]
