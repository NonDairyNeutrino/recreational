(* ::Package:: *)

(*Question: https://mathematica.stackexchange.com/q/209362/46490*)
(*Goal: For a given set of regions, color the regions of intersection*)


regionPairs[regs : {__}] := Subsets[regs, {2}]
nonEmptyIntersections[pairs : {{_, _}..}] := DeleteCases[RegionIntersection @@@ pairs, EmptyRegion[2]]
paintIntersections[color_][poly_] := poly /. reg_Polygon :> {EdgeForm[color], color, reg}
drawIntersections[regs : {__}, color_ : White] := MeshPrimitives[DiscretizeRegion@#, 2] & /@ nonEmptyIntersections@regionPairs@regs // paintIntersections[color] // Catenate // Graphics
drawRegions[regs : {__}, colors : {intersectionColor_?ColorQ, xorColor_?ColorQ} : {White, Black}] := Show[Graphics[{xorColor, regs}], drawIntersections[regs, intersectionColor], PlotRange -> All]
