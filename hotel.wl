(* ::Package:: *)

(* ::Title:: *)
(*The Hotel*)


(* ::Text:: *)
(*The original problem was posed as such:*)
(**)
(*"We have 4 rooms in a hotel, and each room has 2 person capacity (i.e. each room can have 0, 1, or 2 people occupying it). A group of 4 friends comes to this hotel and need a place to stay.  In how many different ways can they accommodate?"*)
(**)
(*After only a moment or two I had the overwhelming feeling that this could be approached, or at least mathematically formulated in terms of Linear Algebra.  A little while later I settled on this reformulation:*)
(**)
(*Let $H$ be the sequence of $N$-dimensional, diagonal operators $H = { R_k }_k^C$ where $R_j = \sum_{i,k}^N \lambda_i \delta_{i,k}$ and $\lambda_i \in { 0, 1 }$ and $\mathrm{tr}(R_j) \leq 2$ and $\sum_k^C R_k = I$. How many configurations of $H$ are there?*)
(**)
(*The idea being that, for a vector of people |peeps> = ( peep1, peep2, ..., peepN), we have the room R_j being used as R_j |peeps> giving a certain state of the people staying in that room (including zero/no people!), and then the sum of all of those gets us back the whole group of people |peeps>.*)


Clear@h
h[c_, n_] := h[c, n] = Table[\[Lambda][j, i] KroneckerDelta[i, k], {j, c}, {i, n}, {k, n}]


(* ::Section::Closed:: *)
(*Example 1: 2 rooms, 2 people, 2 beds*)


(* ::Text:: *)
(*To start off, let's consider 2 rooms that can have up to 2 trying to accomodate a group of 2 people.*)
(**)
(*If we first consider a the vector |peeps> = (Alice, Bob), then each room can be in state {0, 0}, {Alice, 0}, {0, Bob}, or {Alice, Bob} representing the people assigned to that room.  Each of these can be represented as applying diagonal operators (whose elements are either 0 or 1) to |peeps> as*)


(* ::Input:: *)
(*With[*)
(*{peeps={"Alice","Bob"}},*)
(*{*)
(*{{0,0},{0,0}}.peeps,*)
(*{{1,0},{0,0}}.peeps,*)
(*{{0,0},{0,1}}.peeps,*)
(*{{1,0},{0,1}}.peeps*)
(*}*)
(*]*)


(* ::Text:: *)
(*So we can represent the occupancy configuration of each room as a diagonal square matrix whose elements are either 0 or 1.  For this example we have*)


(* ::Input:: *)
(*MatrixForm/@h[2,2]*)


(* ::Text:: *)
(*Since people don't dissapear, the the total of the all the occupancy states (the vectors that result from applying the room operator to |peeps>) needs to equal the original vector of people |peeps>.  In other words*)


(* ::Input:: *)
(*With[*)
(*{hotel=h[2,2],peeps={"Alice","Bob"}},*)
(*Sum[MatrixForm@hotel[[k]].MatrixForm@peeps,{k,2}]==MatrixForm@peeps*)
(*]*)


(* ::Text:: *)
(*Now we can see that we can neglect considering the actual people and just worry about the room configurations as *)


(* ::Input:: *)
(*With[*)
(*{hotel=h[2,2]},*)
(*Sum[MatrixForm@hotel[[k]],{k,2}]==MatrixForm@IdentityMatrix[2]*)
(*]*)


(* ::Text:: *)
(*Which we can thread together as*)


(* ::Input:: *)
(*Diagonal@Total@h[2,2]==1//Thread//Column*)


(* ::Text:: *)
(*Which gives us a system of two equations in four unknowns, BUT we also know that each room has a maximum occupancy.  In this case, 2  So with this in mind, and since each room is diagonal, we also have the stipulation that the trace of each room is less than or equal to the maximum occupancy, which we will call the number of beds.*)


(* ::Input:: *)
(*Tr/@h[2,2]<=2//Thread//Column*)


(* ::Text:: *)
(*And for consistency we also have the conditions, rewritten in terms better understood by Mathematica,*)


(* ::Input:: *)
(*{0<=#<=1,#\[Element]Integers}&/@Flatten@Array[\[Lambda],{2,2}]//Grid*)


(* ::Text:: *)
(*to give us that each variable is either 0 or 1.*)


(* ::Text:: *)
(*With all of these equations, inequalities, and domain restrictions, we have enough information to let Mathematica do the rest of the work and give us the possible accomodations.*)


(* ::Input:: *)
(*Block[*)
(*{capacity=2,groupsize=2,beds=2,hotel,vars,eqs,conds},*)
(*hotel=h[capacity,groupsize];*)
(*vars=Flatten@Array[\[Lambda],{capacity,groupsize}];*)
(*eqs=Diagonal@Total@hotel==1//Thread(*Total@hotel\[Equal]IdentityMatrix[groupsize]//Map[Thread]@*Thread//Diagonal*);*)
(*conds=Join[*)
(*0<=Tr/@hotel<=beds//Thread,*)
(*0<=vars<=1//Thread*)
(*];*)
(*hotel/.Solve[eqs~Join~conds,vars,Integers]*)
(*];*)
(*Map[MatrixForm,%,2]*)
(*Map[Total[#.{"Alice","Bob"}]&,%%,{2}]*)
(*Length@%*)


(* ::Text:: *)
(*And we can see there are 4 configurations/accomodations that satisfy our conditions.*)


(* ::Section::Closed:: *)
(*Example 2: 3 rooms, 2 people, 2 beds*)


(* ::Text:: *)
(*Now we repeat our procedure above for a slightly more interesting example.*)


(* ::Text:: *)
(*Let's consider 3 rooms that can have up to 2 trying to accomodate a group of 2 people.*)


(* ::Text:: *)
(*We can represent the occupancy configuration of each room as a diagonal square matrix whose elements are either 0 or 1.  For this example we have*)


(* ::Input:: *)
(*MatrixForm/@h[3,2]*)


(* ::Text:: *)
(*Which we can thread together as*)


(* ::Input:: *)
(*Diagonal@Total@h[3,2]==1//Thread//Column*)


(* ::Text:: *)
(*Which gives us a system of 2 equations in 6 unknowns, BUT we also know that each room has a maximum occupancy.  In this case, 2  So with this in mind, and since each room is diagonal, we also have the stipulation that the trace of each room is less than or equal to the maximum occupancy, which we will call the number of beds.*)


(* ::Input:: *)
(*Tr/@h[3,2]<=2//Thread//Column*)


(* ::Text:: *)
(*And for consistency we also have the conditions, rewritten in terms better understood by Mathematica,*)


(* ::Input:: *)
(*{0<=#<=1,#\[Element]Integers}&/@Flatten@Array[\[Lambda],{3,2}]//Grid*)


(* ::Text:: *)
(*to give us that each variable is either 0 or 1.*)


(* ::Text:: *)
(*With all of these equations, inequalities, and domain restrictions, we have enough information to let Mathematica do the rest of the work and give us the possible accomodations.*)


(* ::Input:: *)
(*Block[*)
(*{capacity=3,groupsize=2,beds=2,hotel,vars,eqs,conds},*)
(*hotel=h[capacity,groupsize];*)
(*vars=Flatten@Array[\[Lambda],{capacity,groupsize}];*)
(*eqs=Diagonal@Total@hotel==1//Thread(*Total@hotel\[Equal]IdentityMatrix[groupsize]//Map[Thread]@*Thread//Diagonal*);*)
(*conds=Join[*)
(*0<=Tr/@hotel<=beds//Thread,*)
(*0<=vars<=1//Thread*)
(*];*)
(*hotel/.Solve[eqs~Join~conds,vars,Integers]*)
(*];*)
(*Map[MatrixForm,%,2]*)
(*Map[Total[#.{"Alice","Bob"}]&,%%,{2}]*)
(*Length@%*)


(* ::Text:: *)
(*Now we have solutions that contain empty rooms, which makes sense!*)


(* ::Section:: *)
(*Interesting Situations*)


(* ::Text:: *)
(*Now that we have a grasp on the overall construction of the situation, we can skip to the last step so we can more easily explore some interesting situations; but first let's make a function to do this all in one step.*)


Clear[accomodations]
accomodations[capacity_, groupsize_, beds_] := Block[
  {hotel, vars, eqs, conds},
  hotel = h[capacity, groupsize];
  vars = Flatten@Array[\[Lambda], {capacity, groupsize}];
  eqs = Diagonal@Total@hotel == 1 // Thread; 
  conds = Join[Thread[0 <= Tr /@ hotel <= Min[beds, groupsize]], Thread[0 <= vars <= 1]];
  hotel /. Solve[Join[eqs, conds], vars, Integers]
]


Clear[states]
states[accs_, peeps_] := accs.peeps.ConstantArray[1, Length@peeps]
states[accs_] := accs.Array[peep, Last@Dimensions@accs](*.ConstantArray[1,Length@peeps]*)


(* ::Subsection:: *)
(*More people than rooms?*)


(* ::Text:: *)
(*What if there are more people than rooms with enough beds? *)


(* ::Input:: *)
(*accomodations[2,3,2];*)
(*states[%,{"Moe","Larry","Curly"}]*)


(* ::Text:: *)
(*Not a problem!*)


(* ::Text:: *)
(*What if there are more people than rooms with not enough beds? *)


(* ::Input:: *)
(*accomodations[2,3,1]*)


(* ::Text:: *)
(*In this example, no actual accommodation was returned because there does not exist a situation in which 3 people can occupy two rooms that can only hold 1 person each.*)


(* ::Subsection:: *)
(*More beds than people?*)


(* ::Text:: *)
(*What if there are more beds than people?*)


(* ::Input:: *)
(*Length@accomodations[2,2,2]*)
(*Length@accomodations[2,2,3]*)


(* ::Text:: *)
(*If there are more beds than people, then the number of accomodations is the same as if there were the same numer of beds as people.*)


(* ::Section:: *)
(*General: C rooms, N people, B beds*)


(* ::Input:: *)
(*states[accomodations[2,4,2],{"Samantha","Jack","Daniel","Vala"}]*)
(*Length@%*)


(* ::Input:: *)
(*accomodations[4,4,2].{"Samantha","Jack","Daniel","Vala"}*)
