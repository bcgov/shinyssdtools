Cette application web **estime les fonctions de distribution de la sensibilité des espèces grâce aux données de concentration.** L’application est faite à partir de l’application R [ssdtools](https://github.com/bcgov/ssdtools), et partage les mêmes fonctions. 

*Astuce: Clic sur les icônes info pour obtenir plus d’informations.*

### Étape 1: Fournir les données

* Les données doivent être fournies pour **une substance chimique** à la fois.
* Une seule valeur doit être entrée par espèce. 
* L’ensemble des données doit contenir **au moins une colonne** comportant au **minimum 8 concentrations distinctes,positives et sans valeur manquante**.
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

Trois options sont disponibles pour déposer les données dans l’application:

1. **Utiliser les données démo du Bore.**
    - Permet de connaître rapidement les fonctions de l’application avec un ensemble «fonctionnel» de données.
    - Citation: [Canadian Council of Ministers of the Environment. 2009. Canadian water quality guidelines for the protection of aquatic life: Boron. In: Canadian  environmental  quality guidelines, 2009, Canadian Council of  Ministers of the Environment, Winnipeg.](http://ceqg-rcqe.ccme.ca/download/en/324/)
2. **Télécharger un fichier .csv**.
    - Le format Excel n’est pas accepté. Si vous avez un fichier Excel, il faut l’exporter dans une feuille de calcul .csv.
3. **Remplir le tableau interactif.**
    - Cliquer sur une cellule pour commencer à entrer les données. Faire un clic-droit pour ajouter ou supprimer des colonnes ou des rangées. Le nom des colonnes ne peut pas être modifiés. 

Enfin, visualiser les données fournies dans le tableau à droite de l'onglet.

<center>
![Provide data in tab `1. Data`](https://media.giphy.com/media/fjyG7Wbgp1RKV8sS9s/giphy.gif)
</center>

### Étape 2: Ajustement des distributions 

1. Spécifier **quelle est la colonne qui contient les valeurs de concentrations**. L’application tente de deviner quelle est la colonne contenant les valeurs de concentrations à l’aide des noms des colonnes. Cela peut toutefois nécessiter une correction. 
2. **Sélectionner (ou désélectionner) les distributions à ajuster aux données.** À noter que s’il y a un chevauchement dans l’ajustement de deux ou plusieurs fonctions de distribution, il y a aura alors une exagération de la forme de cet ajustement dans l’inférence multimodèle. Consultez cet article pour plus d’information.  La fonction peut prendre quelques secondes pour se mettre à jour. 
3. Mettre en forme le graphique à l’aide des entrées de la barre latérale et **télécharger le graphique et le tableau de l'évaluation de la qualité de l’ajustement des courbes de distribution** respectivement dans des fichiers .png et .csv. 

<center>
![Fit distributions in tab `2. Fit`](https://media.giphy.com/media/yIjxGcFKt0zK2vj38j/giphy.gif)
</center>

Information additionnelle sur **le tableau de l'évaluation de la qualité de l’ajustement des courbes de distribution**:
Les colonnes du tableau sont la distribution (dist), la statistique d’Anderson-Darling (ad), la statistique de Kolmogorov-Smirnov (ks), la statistique de Cramer-von-Mises (cmv), le critère d’information Akaike (aic), le critère d’information Akaike corrigé pour la taille de l’échantillon (aicc), le critère d’information Bayésien (bic), la différence AICc (delta) et le coefficient AICc basé sur le poids Akaike (weight). L’estimation de la fonction de distribution finale  est basée sur l’inférence multimodèle (à partir de l’AICc). La concentration présentant un risque est la concentration estimée d’une substance affectant le pourcentage sélectionné de l’ensemble des espèces.

### Étape 3: Estimation de la concentration présentant un risque ou du pourcentage d’espèces affectées
1. Sélectionner le **seuil (%) des espèces affectées** pour calculer **l’estimation de la concentration présentant un risque** OU  sélectionner **concentration** pour calculer le pourcentage d’espèces affecté par la concentration sélectionnée d’un substance. Cela affecte le graphique (ligne pointillée), le texte sous le graphique et le calcul des bornes de l’intervalle de confiance.
2. Sélectionner le nombre de **simulations bootstrap pour le calcul des bornes de l’intervalle de confiance.** Il est recommandé d’utiliser 10 000 simulations. Le calcul prendra environ 3 minutes.  Sélectionner un nombre moindre de simulations bootstrap pour réduire le temps de calcul. 

<center>

Nombre de simulation Bootstrap &nbsp;&nbsp; | Temps de calcul estimé
--- | ---
10 000 &nbsp; | 5 secondes
5 000 &nbsp;| 10 secondes
1 000 &nbsp;| 20 secondes
500 &nbsp;| 45 secondes

</center>

3. Les intervalles de confiance ne sont pas calculés automatiquement car cela prend un certain temps. Il faut cliquer sur le bouton `Obtenir bornes`.
4. **Le graphique peut être mis en forme** à l’aide des entrées disponibles dans la barre latérale et **le graphique et le tableau peuvent être téléchargés** respectivement dans des fichiers .png et .csv.
    - Si l’étiquette des espèces n’est pas entièrement visible sur le graphique, ajuster l’axe des X et la position des étiquettes en utilisant la fonction étiquette 'X-axis max' et 'Adjust label'.
    - L’éventail de couleurs provient de [ColorBrewer](http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3).
    - Si vous utilisez la même variable pour la couleur et la forme, fournir le même titre de légende permettra de combiner les deux en une seule légende. 
    
<center>
![Get hazard concentration estimates and confidence limits in tab `3. Predict`](https://media.giphy.com/media/xKb9nQsPFlqTCGzUgF/giphy.gif)
</center>

### Étape 4: obtenir le code R

Copier le code R pour reproduire la programmation. Le code est ajouté après chaque exécution dans l’application (par exemple : le code qui génère l’estimation des bornes de l’intervalle de confiance apparaitra après que `Obtenir bornes` aura été cliqué.  

<center>
![Get R code to reproduce results programmatically in tab `R Code`](https://media.giphy.com/media/XIgsL03rRnEfn8nNas/giphy.gif)
</center>

Pour générer un graphique avec l’intervalle de confiance, copier le code R et le coller dans R. Par la suite, régler ci = TRUE dans les fonctions d’estimation et de ssd_plot (predict and ssd_plot functions). 
 


