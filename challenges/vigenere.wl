(* ::Package:: *)

Clear@vig
With[
  {notUpperCaseLetters=Except[Alternatives@@ToUpperCase@Alphabet[]]},
  vig[
    text_String,
    key_String/;StringFreeQ[ToUpperCase@key,notUpperCaseLetters] (*make sure key is the right form*)
  ] := Block[
    {
    parsed=StringReplace[ToUpperCase@text,notUpperCaseLetters->""] (*parse the input string to be in the right form*),
    cipher,shiftedcharcodes
  },
  cipher=ToUpperCase@StringPadRight[key,StringLength@parsed,key] (*make the repeated cipher*);
  shiftedcharcodes=Total@ToCharacterCode@{parsed,cipher}+1//Mod[#,26,1]&(*get ciphertext character codes*);
  StringJoin@@ToUpperCase@Alphabet[][[shiftedcharcodes]] (*pick out letters and parse*)
  ]
]


(* ::Input:: *)
(*vig["Plaintext English!","key"]*)
(*vig["Computation Meets Knowledge","wolfram"]*)
(*vig["The quick brown fox jumped over the lazy dog.","leap"]*)
(*vig["abcdefghijklmnopqrstuvwxyz","zyxwvutsrqponmlkjihgfedcba"]*)
(*vig["message","thekeycanbelonger"]*)
