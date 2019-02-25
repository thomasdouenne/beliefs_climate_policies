### Statistiques descriptives ###
decrit(s$schiste_approbation)
decrit(s$schiste_approbation, miss=T)
decrit(s$schiste_traite)
decrit(s$schiste_avantage)
decrit(s$schiste_CC)


### Simples OLS ###
summary(lm((schiste_approbation=='Oui') ~ (schiste_traite==0), data=s, weights = s$weight)) # Encore rien de trÃ¨s significatif
summary(lm((schiste_avantage=='Cela permettrait de créer des emplois et dynamiser les départements concernés') ~ (schiste_traite==1), data=s, subset=schiste_approbation=='Oui' & schiste_avantage !='Aucune de ces deux raisons', weights = s$weight))
summary(lm((schiste_CC=='Elle est valable : toute baisse des émissions va dans la bonne direction') ~ (schiste_traite==1), data=s, subset=schiste_approbation=='Oui' & schiste_CC !='NSP (Ne sait pas, ne se prononce pas)', weights = s$weight))


### Logit ###
logit_schiste_approbation <- glm((schiste_approbation=='Oui') ~ (schiste_traite==0) + revenu + (taxe_approbation != 'Non'), data=s, family=binomial(link="logit"))
summary(logit_schiste_approbation) # To do : ajouter les variables de contrôle pertinentes pour avoir une bonne spécification

logit_schiste_avantage <- glm((schiste_avantage=='Cela permettrait de créer des emplois et dynamiser les départements concernés') ~ (schiste_traite==0) + revenu + (taxe_approbation != 'Non'), data=s, family=binomial(link="logit"))
summary(logit_schiste_avantage) # To do : ajouter les variables de contrôle pertinentes pour avoir une bonne spécification

logit_schiste_CC <- glm((schiste_CC=='Elle est valable : toute baisse des émissions va dans la bonne direction') ~ (schiste_traite==0) + revenu + (taxe_approbation != 'Non'), data=s, family=binomial(link="logit"))
summary(logit_schiste_CC) # To do : ajouter les variables de contrôle pertinentes pour avoir une bonne spécification
