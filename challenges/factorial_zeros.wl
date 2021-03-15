(* ::Package:: *)

(* ::Text:: *)
(*Factorial Zeros : Write a function that finds the number of zeros at the end of the decimal expansion of n!.*)


Clear@fz
fz[n_Integer?NonNegative/;n<=4]=0;
fz[n_Integer?NonNegative/;4<n]:=n 5^-Range[Floor@Log[5,n]]//Total@*Floor


(* ::Input:: *)
(*Clear@explicitcheck*)
(*explicitcheck[n_Integer?NonNegative/;4<n]:=Length@Last@SplitBy[IntegerDigits[n!],#==0&]*)


(* ::Input:: *)
(*{#,Factorial@#}&@Range[0,100,5]//Transpose//TableForm*)


(* ::Input:: *)
(*Array[fz@#-explicitcheck@#&,100][[5;;]]*)
