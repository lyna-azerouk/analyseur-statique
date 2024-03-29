# Projet d'analyseur statique du cours TAS - Rapport de projet

*Nom et numéro d'étudiant des auteurs du projet.*

Lina Azerouk 

21206889

# Analyse du Domaine des Intervalles


- **Gestion des Intervalles**: Implémentation du domaine des intervalles pour manipuler des ensembles d'entiers avec des bornes pouvant être ±∞.
- [x]  les tests  tests/0_interval passent 
- [x]  Comparaison  test/10_interval_cmp

## Analyse des Boucles et Élargissement

- **Traitement Spécifique des Boucles**: 
- [x]  Loop sur les intervales tests/12_interval_loop
- [x]  Loop sur les intervales tests/12_interval_loop_delay
- [ ]  Loop sur les intervales tests/12_interval_loop_delay_unroll

## Produit Réduit
- **Domaine des Parités et Réduction**: Intégration du domaine des parités et création d'un produit réduit avec le domaine des intervalles.
- **Foncteur Générique de Produit Réduit**: Définition d'un foncteur adaptable à deux domaines abstraits arbitraires.
- [x]  Produit réduit tests/20_reduced


## Extension et Personnalisation: Domaine Disjonctive

Implémentation du domaine disjonctive en prenant en compte le demaine des intervalle  définie précédament.
Redifinition des fonction de base, add_var, remove_var, subset, join, meet, et le print (le widen n'est pas traité)

- [x]  Disjonction tests/disjonction (l'xemple de la boucle n'est pas fonctionelle, car le widen n'a pas été redéfini)

