type terme = X of int | A | B | F of terme | G of terme * terme | H of terme * terme * terme ;;
type equation = E of terme * terme ;;
type systeme = S of equation list ;;
exception ECHEC;;

(**************************************************************************************)


let rec cas1 s1=
	match s1 with 
	[]->true
	|(e::s)->match e with
		E(t1,t2)-> if t1=t2 then false else cas1 s;;

let rec  supprimer s1=
     match s1  with
       []->[]
       |(e::s)->match e with
		E(t1,t2)-> if t1=t2 then supprimer s else (E(t1,t2)::(supprimer s));;
  


(*****************************************************************************************) 
let variable t = match t with
  |(X i) -> true
  |A -> false
  |B -> false
  |F(t1)-> false
  |G(t1,t2) -> false
  |H(t1,t2,t3)-> false;;
		
let rec cas2 s1=
	match s1 with 
	[]-> true
	|(e::s)->match e with
		   E(t1,t2)->if ((variable t1)=false) && ((variable t2)=true) then false else cas2 s;;

		   
let rec echanger s1=
	match s1 with 
	[]->[]
	|(e::s)->match e with
	  E(t1,t2)-> if variable t1=false &&  variable t2=true then (E(t2,t1)::(echanger s)) else (E(t1,t2)::(echanger s));;


(************************************************************************************************)
										      
let fonction t= match t with
  |(X i) -> false
  |A -> false
  |B -> false
  |F(t1)-> true
  |G(t1,t2) -> true
  |H(t1,t2,t3)-> true;;											

let length t=match t with
  |(X i) -> 0
  |A -> 0
  |B -> 0
  |F(t1)-> 1
  |G(t1,t2) -> 2
  |H(t1,t2,t3)-> 3;;


			     
let rec deconstruire s1=
    match s1 with 
    []->[]
    |(e::s)->match e with 
      E(t1,t2)->if fonction t1=true && fonction t2=true && length t1!=length t2 then raise ECHEC
		else								    
		  if fonction t1=false || fonction t2=false then (E(t1,t2)::(deconstruire s)) 
		   else let (a,b) = (t1,t2) in
			match (a,b) with
		        |(F(a),F(b))-> (E(a,b)::(deconstruire s))
		        |(G(a,b),G(x,y))-> (E(a,x)::E(b,y)::(deconstruire s)) 
			|(H(a,b,c),H(x,y,z))->(E(a,x)::E(b,y)::E(c,z)::(deconstruire s));;


let rec cas3 s1=
	match s1 with 
	[]->true
	|(e::s)->match e with 
		   E(t1,t2)->if (( fonction t1)=true) && ((fonction t2)=true) then false else cas3 s;;
(**************************************************************************************************)
let rec test_occurence (X i) t1=match t1 with
     X(a) -> if a=i then true else false
    |A -> false
    |B -> false 
    |F(t1)-> test_occurence (X i) t1
    |G(t1,t2) ->(test_occurence (X i) t1) || (test_occurence (X i) t2)
    |H(t1,t2,t3)->(test_occurence (X i) t1) || (test_occurence (X i) t2) || (test_occurence (X i) t3);;

let rec substitution (X i) t1 t2= match t2 with
    (X a)-> if a=i then t1 else t2
    |A-> t2  
    |B-> t2
    |F(t3)-> F(substitution (X i) t1 t3)
    |G(t3,t4)-> G((substitution (X i) t1 t3),(substitution (X i) t1 t4))
    |H(t3,t4,t5)-> H((substitution (X i) t1 t3),(substitution (X i) t1 t4),(substitution (X i) t1 t5));;
  
             
let rec autre_occurence (X i) s1=match s1 with
   []->false
   |(e::s)->match e with
	      E(t1,t2)-> if test_occurence (X i) t1=true ||test_occurence (X i) t2=true then true else autre_occurence (X i) s;;   

let rec substitution_systeme  (X i) t s1 = match s1 with
	[]->[]
  	|(e::s)-> match e with 
		E(t1,t2)-> (E(substitution (X i) t t1,substitution (X i) t t2)::(substitution_systeme (X i) t s));;
	      
let rec remplacer s1=match s1 with 
	[]->[]
	|(e::s)->match e with
		E(t1,t2)-> if ((variable t1)&&(test_occurence t1 t2)) then raise ECHEC 
			      else
				  if ((variable t1)&& (autre_occurence t1 s)) then ([e]@substitution_systeme t1 t2 s) else (E(t1,t2)::(remplacer s));;

let rec reverse s1=
  match s1 with
  []->[]
  |(e::s)->((reverse s)@[e]);;
	                                                                   
let rec cas4 s1=
  match s1 with
  []->true
  |(e::s)->match e with 
   E(t1,t2)->if ((variable t1)&& (autre_occurence t1 s))then false else cas4 s;;

  
(*********************************************************************************)
let forme_resolue s = if (cas1 s) && (cas2 s) && (cas3 s) && (cas4 s) && (cas4 (reverse s)) then true else false;;
                       
let unification s=
	remplacer(reverse(remplacer(deconstruire(echanger(supprimer(s))))));;
	

                      
let rec projet s = match s with
   |[] -> []
   |(e::ss) -> if (forme_resolue s)=true then s else  projet(unification s);;
