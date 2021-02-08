(* ::Package:: *)

(*
10/25/20 Multiples of 3 and 5:
Write a function that takes a positive integer n and returns the number of multiples of both 3 and 5 up to n.
*)
Clear@threefive
(*
Every multiple of both 3 and 5 is a multiple of 15.
So the question is really "How many multiples of 15 that are smaller than n are there?",
which is really the same as the the integer part of n/15.
*)
threefive[n_Integer] := Floor[n/15]


(* ::Input:: *)
(*DiscretePlot[threefive[n],{n,100},Ticks->{Range[0,100,15],Automatic}]*)
