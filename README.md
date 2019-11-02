# Algorithme-Martelli-Montanari
Algo d'unification qui permet de mettre un systeme d'equation sous forme résolue en OCaml

En 1982, Martelli et Montanari décrivent un algorithme sous la forme d'un ensemble de règles qui transforment un ensemble 
d'équations21. L'algorithme est présenté dans des ouvrages pédagogiques27,28,29. Il est similaire à l'algorithme proposé par 
Herbrand dans sa thèse30. Selon Baader et Snyder31, un algorithme sous la forme d'un ensemble de règles dégage les concepts 
essentiels et permet de démontrer la correction de plusieurs algorithmes pratiques en même temps.

On se donne un ensemble fini d'équations G = { s1 ≐ t1, ..., sn ≐ tn } où les si et ti sont des termes du premier ordre. 
L'objectif est de calculer une substitution la plus générale. On applique alors les règles suivantes à l'ensemble G jusqu'à 
épuisement :
G ∪ { t ≐ t } 	⇒ 	G 		supprimer
G ∪ { f(s1,...,sk) ≐ f(t1,...,tk) } 	⇒ 	G ∪ { s1 ≐ t1, ..., sk ≐ tk } 		décomposer
G ∪ { f(t⃗) ≐ x } 	⇒ 	G ∪ { x ≐ f(t⃗) } 		échanger
G ∪ { x ≐ t } 	⇒ 	G{x↦t} ∪ { x ≐ t } 	si x ∉ vars(t) et x ∈ vars(G) 	éliminer

La règle supprimer supprime une équation t ≐ t, c'est-à-dire où les termes de la partie gauche et de la partie droite sont les
mêmes. La règle décomposer supprime une équation de la forme f(s1,...,sk) ≐ f(t1,...,tk) et la remplace par les équations s1 ≐ t1, 
..., sk ≐ tk. La règle échanger oriente les équations pour que la variable x soit en partie gauche. En présence d'une équation 
x ≐ t où la variable x n'apparaît pas dans le terme t, la règle éliminer remplace les occurrences de x par t dans les autres 
équations.

Les règles suivantes sont également ajoutées en guise d'optimisation29 :
G ∪ { f(s⃗) ≐ g(t⃗) } 	⇒ 	⊥ 	si f ≠ g or k ≠ m 	conflit
G ∪ { x ≐ f(s⃗) } 	⇒ 	⊥ 	si x ∈ vars(f(s⃗)) 	vérifier (occurs-check)

Si l'ensemble contient une équation de la forme f(s⃗) ≐ g(t⃗) où f et g ne sont pas les mêmes symboles de fonctions ou alors si 
le nombre d'arguments n'est pas le même la règle conflit fait échouer le processus d'unification. 
La règle vérifier (occurs-check), quant à elle, fait échouer l'unification si l'ensemble contient une équation x ≐ f(s⃗) où 
x apparaît dans f(s⃗).

L'algorithme est en temps exponentiel et demande un espace mémoire au plus exponentiel si l'on représente les termes par 
leurs arbres syntaxiques. Néanmoins, on peut n'utiliser qu'un espace mémoire linéaire si on représente les termes par des graphes.

Source Wikipedia
