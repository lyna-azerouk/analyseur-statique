# Projet d'analyseur statique du cours TAS - Rapport de projet

  

*Nom et numéro d'étudiant des auteurs du projet.*

  

Lina Azerouk


21206889

  ##  Domaine des constantes
- [x]  [constant_domain.ml](src/domains/constant_domain.ml) , implémentation des fonctions manquante pour gérer les constantes.
- [x] Produit réduit tests/04_constant

## Domaine des Intervalles

-  **Gestion des Intervalles**: Implémentation du domaine des intervalles pour manipuler des ensembles d'entiers avec des bornes pouvant être ±∞.

- [x] les tests tests/0_interval passent

- [x] Comparaison test/10_interval_cmp

## Analyse des Boucles et Élargissement

-  **Traitement Spécifique des Boucles**:

- [x] Loop sur les intervales tests/12_interval_loop
- [x]  delay sur les intervales tests/13_interval_loop_delay 
- [ ]  unroll sur les intervales tests/14_interval_loop_delay_unroll, uniquement 8 tests sur 16 passent concernant le unroll.   


## Produit Réduit

-  **Domaine des Parités**: Intégration du domaine des parités.
	- [x]  [parity_domain.ml](src/domains/parity_domain.ml) 

-   **Produit Réduit**  :  Produit réduit entre le domaine des intervalles et le domaine des parités

- [x]  Définition de la fonction reduce [parity_interval_reduction.ml](src/domains/parity_interval_reduction.ml) 
- [x] Produit réduit tests/20_reduced

  
  

## Extension: Domaine Disjonctive (l’ensemble des parties finies)

  Implémentation du domaine disjonctive en prenant en compte le domaine des intervalles définie précédemment.
Pour cela, j'ai défini un nouveau qui peut un être un ensemble (ENS), ou un ensemble d'ensemble (type récursif), ou un ensemble vide. 
```ocaml 
type t = | ENS of D.t | Union of t * t | BOT
```
Redéfinition des fonctions de base, **add_var**,  **remove_var**,  **subset**, **join**,  **meet**,  et le **print** (le **widen** n'est pas traité). 

**Exemple**: 
```c
int x;
int y;
x = rand(10,20);
y = rand(0,1);
if(y>0){ x = -x; }
print_all;
```
- La branche then du test donne:  ``` x in [-20;-10], y in [1;1]  ```
- La branche else donne:  ``` x in [10;20], y in [0;0] ```
- L'union des deux renvoie: ```{ [ x in [-20;-10], y in [1;1] ] }, { [ x in [10;20], y in [0;0] ] } ```

- [x] Domaine disjonctive  [src/domaine/disjonctive_domain.ml](src/domains/disjonctive_domain.ml) 
- [x]  Tests  sur le domaine disjonction [tests/disjonction](tests/disjonction) 
-  [x]   [main.ml](src/main.ml): option  `-disjonction`  pour l'ensemble des parties finies. 
(l'exemple de la boucle n'est pas fonctionnel, car le widen n'a pas été redéfini)