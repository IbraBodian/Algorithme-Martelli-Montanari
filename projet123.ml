type terme = X of int | A | B | F of terme | G of terme * terme | H of terme * terme * terme ;;
type equation = E of terme * terme ;;
type systeme = S of equation list ;;
exception Undefined;;

(**************************************************************************************)


let rec cas1 s1=
	match s1 with 
	[]->true
	|(e::s)->match e with
		E(t1,t2)-> if t1=t2 then true else cas1 s;;
let rec  supprimer s1=
     match s1  with
       []->[]
       |(e::s)->match e with
		E(t1,t2)-> if t1=t2 then supprimer s else (E(t1,t2)::(supprimer s));;
  


(*****************************************************************************************) 
let verification t = match t with
  |X(i) -> false
  |A -> true
  |B -> true
  |F(t1)-> true
  |G(t1,t2) -> true
  |H(t1,t2,t3)-> true;;
		
let rec cas2 s1=
	match s1 with 
	[]-> true
	|(e::s)->match e with
		   E(t1,t2)->if ((verification t1)=true) && ((verification t2)=false) then false else cas2 s;;

		   
let rec echanger s1=
	match s1 with 
	[]->[]
	|(e::s)->match e with
	  E(t1,t2)-> if verification t1=true &&  verification t2=false then (E(t2,t1)::(echanger s)) else (E(t1,t2)::(echanger s));;


(************************************************************************************************)
										      
let appart t= match t with
  |X(i) -> false
  |A -> false
  |B -> false
  |F(t1)-> true
  |G(t1,t2) -> true
  |H(t1,t2,t3)-> true;;											

let length t=match t with
  |X(i) -> 0
  |A -> 0
  |B -> 0
  |F(t1)-> 1
  |G(t1,t2) -> 2
  |H(t1,t2,t3)-> 3;;


			     
let rec deconstruire s1=
    match s1 with 
    []->[]
    |(e::s)->match e with 
      E(t1,t2)->if appart t1=true && appart t2=true && length t1!=length t2 then raise Undefined
		else								    
		  if appart t1=false || appart t2=false then (E(t1,t2)::(deconstruire s))				   else let (a,b) = (t1,t2) in
					    match (a,b) with
		                    |(F(a),F(b))-> (E(a,b)::(deconstruire s))
					    |(G(a,b),G(x,y))-> (E(a,x)::E(b,y)::(deconstruire s))                                                  |(H(a,b,c),H(x,y,z))->(E(a,x)::E(b,y)::E(c,z)::(deconstruire s));;


let rec cas3 s1=
	match s1 with 
	[]->true
	|(e::s)->match e with 
		   E(t1,t2)->if (( appart t1)=true) && ((appart t2)=true) then false else cas3 s;;
let rec test_occurence x1 t1=match t1 with
    |X(i) -> if x1=X(i) then true else false
    |A -> false
    |B -> false 
    |F(t1)-> test_occurence x1 t1
    |G(t1,t2) ->(test_occurence x1 t1) || (test_occurence x1 t2)
    |H(t1,t2,t3)->(test_occurence x1 t1) || (test_occurence x1 t2) || (test_occurence x1 t3);;
let rec substitution x t1 t2= match t2 with
    |X(i)-> if X(i)=x then t1 else t2
    |A-> t2  
    |B-> t2
    |F(t3)-> F(substitution x t1 t3)
    |G(t3,t4)-> G((substitution x t1 t3),(substitution x t1 t4))
    |H(t3,t4,t5)-> H((substitution x t1 t3),(substitution x t1 t4),(substitution x t1 t5));;
let rec subequation x t s1=match s1 with
  []->[]
 (*|(e::s)->match e with
	     E(t1,t2)->if test_occurence x t2=true then (E(t1,substitution x t t2)::(subequation x t s)) else
			 if  test_occurence x t1=true then (E(substitution x t t1, t2)::(subequation x t s)) else
			   (E(t1,t2)::(subequation x t s));;*)
|(e::s)->match e with  E(t1,t2)-> ([E(substitution x t t1,substitution x t t2)]@ (subequation x t s));;
  
(**************************************************************************************************)
   let rec remove e1  s1 = match s1 with
    []->[]
   |(e::s)->match e with E(t1,t2)->if E(t1,t2)=e1 then remove e1 s else (e::(remove e1 s));;
             



let rec appartient x1 s1=
  match s1 with
   []->false
   |(e::s)->match e with
	      E(t1,t2)-> if test_occurence x1 t1=true ||test_occurence x1 t2=true then true else appartient x1 s;;   

let rec substitution_systeme (X i) t1 eq = match eq with
	[]->[]
  | (t2,t3)::eqq -> [((substitution (X i) t1 t2),(substitution (X i) t1 t3))]@(substitution_systeme (X i) t1 eqq) 


;;	      
let rec testsub s2 = match s2 with            substitution_systeme (X i) t ([X(i),t]@reste)	
  |[] -> []
  |(E(X(i),t)::s)-> if test_occurence (X i) t  then raise Undefined else if (appartient t s)=true then substitution_systeme (X i) t ([E(X(i),t)]@s)
  |(E(t1,t2)::s) ->testsub (E(t1,t2)@s)
;;



	                                                                     

let rec cas4 s1 s2 =
  match s1 with
  []->true 
  |(e::s)->match e with 
   E(t1,t2)->if verification t1=false && appartient t1 (remove e s2)=true then false else cas4 s s2 ;;

let cas44 s = cas4 s s;;

  
(*********************************************************************************)
 let forme_resolue s = if (cas1 s) && (cas2 s) && (cas3 s) && (cas4 s s) 
                       then true
                       else false;; 

 let algo_unifie s = if cas1 s=false then supprimer s else
		       if cas2 s=false then echanger s else
			 if cas3 s=false then deconstruire s else
			   if (cas4 s s=false) then testsub s s else s;;
                      
 let rec projet s = match s with
   |[] -> []
   |(e::ss) -> if (forme_resolue s)=true
              then s
              else  projet(supprimer(echanger(deconstruire(testsub ([e]@ss) ([e]@ss)))));;
