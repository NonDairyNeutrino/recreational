(* ::Package:: *)

(*10/18/20 Butterflied Strings: Join a string with its reversal*)
Clear@butterfly
butterfly[str_String] := str<>StringReverse@str (*Take the original string str and concatenate it with the reversal of the original string*)


(* ::Input:: *)
(*butterfly["Mathematica"]*)
(*butterfly["racecar"]*)
(*butterfly["kayak"]*)
