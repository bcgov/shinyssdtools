## Estimation de la concentration présentant un risque

1. Deux options sont possibles : Estimer la **concentration** affectant/protégeant une fraction sélectionnée d'espèces (%) OU estimer la fraction d'espèces (%) affectée par une concentration sélectionnée. 
Cela affecte le tracé du graphique (ligne pointillée), le texte sous le graphique et le calcul des bornes de l'intervalle de confiance.

2. Sélectionner le **nombre de simulations bootstrap pour le calcul des bornes de l'intervalle de confiance.** Il est recommandé d'utiliser 10 000 simulations. Le calcul peut prendre du temps.  
Sélectionner un nombre inférieur de simulations bootstrap pour réduire le temps de calcul. 
Les distributions sont considérés comme constituant une distribution de mélange unique (plutôt que la prise de la moyenne) pour le calcul des estimations de la moyenne du modèle. 
Les distributions ne sont pas traitées comme constituant une distribution unique pour le calcul des intervalles de confiance, car cela augmente considérablement le temps de traitement.

3. Les intervalles de confiance ne sont pas calculés automatiquement car cela prend un certain temps. 
Il faut cliquer sur le bouton `Obtenir bornes`.
4. Utilisez les options de la barre latérale pour mettre en forme le graphique, puis **téléchargez le graphique** au format PNG ou RDS ainsi que **le tableau des limites de confiance** aux formats CSV ou XLSX.
