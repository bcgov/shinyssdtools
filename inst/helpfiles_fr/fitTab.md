## Ajustement des distributions


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
