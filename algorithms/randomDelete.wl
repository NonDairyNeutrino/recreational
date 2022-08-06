(* ::Package:: *)

(*Question: https://mathematica.stackexchange.com/q/181636/46490*)
(*Function to delete a random part of an expression*)
randomDelete[expr_] := Delete[expr, RandomChoice@Position[expr, _Symbol | _Real, {-1}, Heads -> False]]
