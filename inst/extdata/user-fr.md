Cette application web estime la distribution de la sensibilité des espèces en fonction des données de concentration. L'application est conçue à partir ddu progiciel R [ssdtools](https://github.com/bcgov/ssdtools), et partage les mêmes fonctions. 
De plus amples informations sur les méthodes de ssdtools sont disponibles aux liens suivants [articles du site web] (https://bcgov.github.io/ssdtools/articles/).

*Astuce: Clic sur les icônes info pour obtenir plus d'informations.*

### Étape 1: Fournir les données

* Les données doivent être fournies pour **une substance chimique** à la fois.
* Une seule valeur doit être entrée par espèce.
* L'ensemble de données doit contenir **au moins une colonne** comprenant au **minimum six valeurs de concentration positives et non manquantes**.
* Des colonnes pour **les espèces et les groupes** taxonomiques peuvent également être ajoutées. Des étiquettes et des couleurs sont alors disponibles pour permettre leur identification dans le graphique.
* Des colonnes additionnelles sont possibles mais elles ne sont reliées à aucune fonction. 

<center>

Concentration&nbsp;&nbsp; | Espèce&nbsp;&nbsp; | Groupe &nbsp;
--- | --- | ---
2.1 | Oncorhynchus mykiss &nbsp; | Poisson
2.4 | Ictalurus punctatus &nbsp;| Poisson 
4.1 | Micropterus salmoides &nbsp;| Poisson
10  | Brachydanio rerio &nbsp;| Poisson
15.6 | Carassius auratus &nbsp;| Poisson
18.3 | Pimephales promelas &nbsp;| Poisson
6 | Daphnia magna &nbsp;| Invertébré
10 | Opercularia bimarginata &nbsp;| Invertébré

</center>

Trois options sont disponibles pour déposer les données dans l'application:

1. **Utiliser les données démo du Bore.**
    - Permet de connaître rapidement les fonctions de l'application avec un ensemble «fonctionnel» de données.
    - Citation: [Canadian Council of Ministers of the Environment. 2009. Canadian water quality guidelines for the protection of aquatic life: Boron. In: Canadian  environmental  quality guidelines, 2009, Canadian Council of  Ministers of the Environment, Winnipeg.](https://ccme.ca/en/res/boron-en-canadian-water-quality-guidelines-for-the-protection-of-aquatic-life.pdf)
2. **Télécharger un fichier .csv**.
    - Le format Excel n'est pas accepté. Si vous avez un fichier Excel, il faut l'exporter dans une feuille de calcul .csv.
3. **Remplir le tableau interactif.**
    - Cliquer sur une cellule pour commencer à entrer les données. Faire un clic-droit pour ajouter ou supprimer des colonnes ou des rangées. Le nom des colonnes ne peut pas être modifiés. 

Vous pouvez également entrer le **nom du toxique**, qui sera utilisé comme titre par défaut du graphique.

Enfin, visualisez les données dans le tableau à droite de l'onglet. L'ensemble de données peut être téléchargé aux formats CSV ou XLSX.

### Étape 2: Ajustement des distributions 

1. Spécifier **quelle est la colonne qui contient les valeurs de concentrations**. 
L'application tente de deviner quelle est la colonne contenant les valeurs de concentrations à l'aide des noms des colonnes. 
Cela peut toutefois nécessiter une correction. 
2. **Sélectionner (ou désélectionner) les distributions à ajuster aux données.** Le graphique des distributions ajustées comprend les estimations moyennes du modèle. 
Il est à noter que s'il y a un chevauchement dans l'ajustement de deux ou plusieurs fonctions de distribution, il y a aura alors une exagération de la forme de cet ajustement dans l'inférence multimodèle. Consultez [cet article](https://bcgov.github.io/ssdtools/articles/distributions.html) pour plus d'information.  
Cliquez sur « Mettre à jour l'ajustement » pour actualiser les résultats après avoir modifié les distributions sélectionnées ou l'option de redimensionnement des données.
3. **Sélectionner si les données doivent être redimensionnées**
Indique si les données doivent être redimensionnées. Lorsque cette option est désactivée, les valeurs de concentration restent inchangées. Lorsqu'elle est activée, les valeurs sont redimensionnées en les divisant par la moyenne géométrique des valeurs positives finies minimale et maximale. Le redimensionnement améliore la stabilité numérique lorsque certaines distributions ne parviennent pas à s'ajuster. Les estimations et les statistiques de qualité d'ajustement ne sont pas modifiées.
4. Utilisez les options de la barre latérale pour mettre en forme le graphique, puis **téléchargez le graphique** au format PNG ou RDS et **le tableau de qualité d'ajustement** aux formats CSV ou XLSX. Sélectionnez également les unités à afficher sur l'axe x du graphique.

Information additionnelle sur **le tableau de l'évaluation de la qualité de l'ajustement des courbes de distribution**:
Les colonnes du tableau de qualité d'ajustement sont : la distribution (dist), le nombre de paramètres (npars), le nombre d'observations (nobs), la log-vraisemblance (log_lik), le critère d'information d'Akaike (aic), le critère d'information d'Akaike corrigé pour la taille de l'échantillon (aicc), les différences de critère d'information (delta), les poids des critères d'information (wt), le critère d'information bayésien (bic), la statistique d'Anderson-Darling (ad), la statistique de Kolmogorov-Smirnov (ks) et la statistique de Cramér-von Mises (cvm). 
L'estimation de la fonction de distribution finale  est basée sur l'inférence multimodèle (à partir de l'AICc). 
La concentration présentant un risque est la concentration estimée d'une substance affectant le pourcentage sélectionné de l'ensemble des espèces.

### Étape 3: Estimation de la concentration présentant un risque ou du pourcentage d'espèces affectées
1. Deux options sont possibles : Estimer la **concentration** affectant/protégeant une fraction sélectionnée d'espèces (%) OU estimer la fraction d'espèces (%) affectée par une concentration sélectionnée. 
Cela affecte le tracé du graphique (ligne pointillée), le texte sous le graphique et le calcul des bornes de l'intervalle de confiance.

2. Sélectionner le **nombre de simulations bootstrap pour le calcul des bornes de l'intervalle de confiance.** Il est recommandé d'utiliser 10 000 simulations. Le calcul peut prendre du temps.  
Sélectionner un nombre inférieur de simulations bootstrap pour réduire le temps de calcul. 
Les distributions sont considérés comme constituant une distribution de mélange unique (plutôt que la prise de la moyenne) pour le calcul des estimations de la moyenne du modèle. 
Les distributions ne sont pas traitées comme constituant une distribution unique pour le calcul des intervalles de confiance, car cela augmente considérablement le temps de traitement.

3. Les intervalles de confiance ne sont pas calculés automatiquement car cela prend un certain temps. 
Il faut cliquer sur le bouton `Obtenir bornes`.
4. Utilisez les options de la barre latérale pour mettre en forme le graphique, puis **téléchargez le graphique** au format PNG ou RDS ainsi que **le tableau des limites de confiance** aux formats CSV ou XLSX.

### Étape 4: Obtenir le rapport BCANZ
Générer un rapport au format HTML ou PDF comprenant le tracé de la distribution ajustée, le tableau de qualité de l'ajustement, le tracé de l'ajustement moyen du modèle et le tableau des concentrations dangereuses/protectrices estimées. 
Toutes les options sélectionnées dans l'application seront incorporées dans le rapport.

### Étape 5: Obtenir le code R

Copiez le code R pour reproduire les résultats par programmation. Le code est généré dynamiquement en fonction des entrées de l'utilisateur et des fonctions exécutées dans l'application (par exemple, le code permettant de générer les limites de confiance apparaît après avoir cliqué sur le bouton « Obtenir les bornes »).  
 


