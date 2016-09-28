# SimulatR

[![DOI](https://zenodo.org/badge/69283925.svg)](https://zenodo.org/badge/latestdoi/69283925)

## Description

Outil interactif permettant de simuler les situations d'inférence.

## License

GNU AFFERO GENERAL PUBLIC LICENSE v3

## Citation

Commenges H. (2016) **SimulatR : simuler les situations d'inférence avec R**, UMR 8504 Géographie-cités, DOI: 10.5281/zenodo.157162


## Utilisation

### Données

Le jeu de données d'exemple est une population de 1000 individus caractérisés par trois variables indépendantes les unes des autres :

- *forme :* variable qualitative à deux modalités également réparties (rond, triangle)
- *couleur :* variable qualitative à deux modalités également réparties (orange, vert)
- *taille :* variable quantitative de distribution normale, de moyenne 100 et de variance paramétrable

### Principe général

Dans l'enseignement des statistiques en SHS, il n'est pas possible de faire la démonstration analytique des propriétés des estimateurs et des statistiques de test. L'idée de SimulatR est donc de simuler une situation d'échantillonage et de la répéter pour visualiser une distribution d'échantillonage de la moyenne ou une distribution d'une statistique de test (ici le t de Student et le chi2). Trois situations peuvent être simulées :

1. *estimation d'un intervalle de confiance :* distribution d'échantillonage de la moyenne
2. *comparaison de moyennes :* distribution de la statistique t de Student
3. *tableau de contingence :* distribution de la statistique chi2
