source("packages_functions.R")
# To trim white edges on figures: mogrify -trim +repage *.png https://askubuntu.com/questions/351767/how-to-crop-borders-white-spaces-from-image


##### Distributions de revenus #####
decrit(s$revenu, weights = s$weight)
round(deciles_erfs_inflates_weighted)
distribution_revenu <- wtd.Ecdf(s$revenu, weights = s$weight)
plot(distribution_revenu_erfs_weighted$x/12, distribution_revenu_erfs_weighted$ecdf, xlim=c(0,5000), type='l', lwd=2, col="blue", xlab = "Net individual income (in €/month)", ylab="Distribution of incomes")
lines(distribution_revenu$x, distribution_revenu$ecdf, col="orange", type='l', lwd=2) + grid()
legend("bottomright", lwd=2, col=c("blue", "orange"), title = "Income distribution from:", title.col = "black", text.col = c("blue", "orange"), legend=c("ERFS (true)", "BCP (our survey)"))

decrit(s$rev_tot, weights = s$weight)
round(deciles_menage_erfs_inflates_weighted)
distribution_rev_tot <- wtd.Ecdf(s$rev_tot, weights = s$weight)
plot(distribution_rev_tot_erfs_weighted$x/12, distribution_rev_tot_erfs_weighted$ecdf, xlim=c(0,5000), type='l', lwd=2, col="blue", xlab = "Net household income (in €/month)", ylab="Distribution of incomes")
lines(distribution_rev_tot$x, distribution_rev_tot$ecdf, col="orange", type='l', lwd=2) + grid()
legend("bottomright", lwd=2, col=c("blue", "orange"), title = "Income distribution from:", title.col = "black", text.col = c("blue", "orange"), legend=c("ERFS (true)", "BCP (our survey)"))


##### Durées et qualité #####
decrit(s$duree/60) # 18 min (moyenne 24)
decrit(s$duree_depenses/60) # 2 min (moyenne 3.5)
decrit(s$duree_champ_libre/60) # 0.1 min (moyenne 1)
decrit(s$duree_info_CC_PM/60) # 0.4 min (moyenne 1)
decrit(s$duree_info_CC/60) # 0.3 min (moyenne 1.7)
decrit(s$duree_info_PM/60) # 0.2 min (moyenne 0.5)
decrit(s$duree_no_info/60) # 0.05 min (moyenne 0.1)
length(which(!is.na(s$depenses_confiant)))/nrow(s) # 32%
plot(density(sa$duree/60), xlim=c(0,30))
plot(Ecdf(sa$duree/60)$x, Ecdf(sa$duree/60)$ecdf)
sa$duree_min <- sa$duree/60
Ecdf(sa$duree_min) 
# significant correlation:
summary(lm(age ~ I(duree/60), data=s, weights = s$weight)) # -.002**
summary(lm(revenu ~ I(duree/60), data=s, weights = s$weight)) # -.15*
summary(lm(sexe=='Masculin' ~ I(duree/60), data=s, weights = s$weight)) # -.00004*
# Uncorrelated:
summary(lm(Diplome ~ duree, data=s, weights = s$weight))
summary(lm(taille_agglo ~ duree, data=s, weights = s$weight))
# Uncorrelated with preferences:
summary(lm(taxe_approbation!='Non' ~ duree, data=s, weights = s$weight))
summary(lm(gagnant_categorie!='Perdant' ~ duree, data=s, weights = s$weight))
summary(lm(gain ~ duree, data=s, weights = s$weight))
summary(lm(taxe_approbation!='Non' ~ duree + I(duree^2), data=s, weights = s$weight)) # .
summary(lm(gagnant_categorie!='Perdant' ~ duree + I(duree^2), data=s, weights = s$weight))
summary(lm(gain ~ duree + I(duree^2), data=s, weights = s$weight))

# mauvaise_qualite uncorrelated with preferences
summary(lm(taxe_approbation!='Non' ~ mauvaise_qualite, data=s, weights = s$weight))
summary(lm(gagnant_categorie!='Perdant' ~ mauvaise_qualite, data=s, weights = s$weight))
summary(lm(gain ~ mauvaise_qualite, data=s, weights = s$weight))

# Weak instrument for the IV acceptance ~ taxe_efficace but gives similar 58 p.p.
mean(s$duree_info > 180) # 3%
summary(lm(pmin(180, pmax(duree_info_PM, duree_info_CC_PM, na.rm=T)) ~ (info_CC==1), data=s, weights=s$weight)) # 22'' reading PM when CC before, 31'' otherwise
summary(lm(pmin(180, pmax(duree_info_CC, duree_info_CC_PM, na.rm=T)) ~ (info_PM==1), data=s, weights=s$weight)) # 32'' reading CC
tsls1_ee1bis <- lm(taxe_efficace!='Non' ~ apres_modifs + pmin(180, duree_info) * info_CC * info_PM, data=s, weights=s$weight) # I(info_CC==1 | info_PM==1) + I(info_PM==0 & info_CC==1) + I(info_PM==1 & info_CC==0)
summary(tsls1_ee1bis)
s$taxe_efficace.hat <- tsls1_ee1bis$fitted.values
tsls2_ee1bis <- lm(tax_acceptance ~ taxe_efficace.hat, data=s, weights=s$weight)
summary(tsls2_ee1bis)
summary(lm(tax_acceptance ~ pmin(180, duree_info) * info_CC * info_PM, data=s, weights=s$weight))


##### Caractéristiques énergétiques #####
decrit(s$surface, weights = s$weight) # 120
decrit(s$chauffage, weights = s$weight) # 50% gaz ou fioul
decrit(s$km, weights = s$weight) # 11k (moyenne 15k)
decrit(s$conso, weights = s$weight) # 6
decrit(s$nb_vehicules, weights = s$weight)


##### Gain questions générales (TVA, transports, logement) #####
decrit(s$perte_relative_tva, weights = s$weight)
decrit(s$perte_relative_fuel, weights = s$weight)
decrit(s$perte_relative_chauffage, weights = s$weight) # proportions similaires pour les 3, environ 60% pensent perdre plus que la moyenne
summary(lm((perte_relative_partielle==-2) ~ variante_partielle, data=s, weights = s$weight))
cor(s$perte_relative_partielle, s$gain)
cor(s$perte_relative_partielle, 1*(s$gagnant_categorie=='Gagnant')-1*(s$gagnant_categorie=='Perdant'))
decrit(s$gagnant_categorie[s$perte_relative_partielle<0], weights = s$weight[s$perte_relative_partielle<0])
decrit(s$gagnant_categorie[s$perte_relative_partielle>0], weights = s$weight[s$perte_relative_partielle>0])
decrit(s$gagnant_categorie, weights = s$weight)
temp1 <- temp2 <- s[,c("perte_relative_tva", "perte_relative_fuel", "perte_relative_chauffage", "perte_relative_partielle", "variante_partielle", "weight")]
temp1$perte <- temp1$perte_relative_tva
temp1$variante_perte <- "tva"
temp2$perte <- temp2$perte_relative_partielle
temp2$variante_perte <- temp2$variante_partielle
s_perte <- merge(temp1, temp2, all=T)
# s_perte$variante_perte <- relevel(as.factor(s_perte$variante_perte), "tva")
summary(lm(perte ~ variante_perte, data = s_perte, weights = s_perte$weight)) # *** -0.31 chauffage, -0.18 fuel: les gens s'estiment plus perdants avec hausse TVA
# TODO: restreindre aux seuls gagnants/perdants

decrit(s$gain_chauffage, weights = s$weight)
decrit(s$gain_chauffage[(s$fioul | s$gaz) == FALSE], weights = s$weight[(s$fioul | s$gaz) == FALSE])
wtd.mean((s$gain_chauffage<0)[(s$fioul | s$gaz) == FALSE], weights = s$weight[(s$fioul | s$gaz) == FALSE]) # 32% think they lose although they don't use hydrocarbon!
wtd.mean((s$gain_chauffage<50)[(s$fioul | s$gaz) == FALSE], weights = s$weight[(s$fioul | s$gaz) == FALSE]) # 82% think they win less than 50 although they don't use hydrocarbon!
wtd.mean((s$gain_chauffage<0)[(s$fioul | s$gaz) == T], weights = s$weight[(s$fioul | s$gaz) == T]) # 79% of hydrocarbon users they think lose 
wtd.mean((s$gain_chauffage<50)[(s$fioul | s$gaz) == T], weights = s$weight[(s$fioul | s$gaz) == T]) # 100% of hydrocarbon users they think lose 
decrit(s$chauffage[(s$fioul | s$gaz) == FALSE & s$gain_chauffage < 0], weights = s$weight[(s$fioul | s$gaz) == FALSE & s$gain_chauffage < 0]) # 60% à l'électricité
wtd.mean((s$gain_chauffage<0)[(s$fioul | s$gaz) == FALSE & s$mode_chauffage=='individuel' & s$chauffage!='NSP'], 
         weights = s$weight[(s$fioul | s$gaz) == FALSE & s$mode_chauffage=='individuel' & s$chauffage!='NSP']) # 30%: ce n'est pas dû au mode de chauffage
wtd.mean((s$gain_chauffage<50)[(s$fioul | s$gaz) == FALSE & s$mode_chauffage=='individuel' & s$chauffage!='NSP'], 
         weights = s$weight[(s$fioul | s$gaz) == FALSE & s$mode_chauffage=='individuel' & s$chauffage!='NSP']) # 87%
decrit(s$gain_chauffage[(s$fioul | s$gaz) == FALSE & s$mode_chauffage=='individuel' & s$chauffage!='NSP']) # 684 personnes
decrit(s$gain_chauffage[(s$fioul | s$gaz) == FALSE]) # 831 personnes 
decrit((s$gain_fuel<0)[s$km <= 1000], weights = s$weight[s$km <= 1000]) # 32% sur 245 personnes (271 en weighted)
decrit((s$gain_fuel<40)[s$km <= 1000], weights = s$weight[s$km <= 1000]) # 92%. 40€ de hausse pour 1000 km * 30 L/100 km * 0.13€/L d'essence
formula_perte_absurde <- as.formula(paste("gain_partielle < 0 ~ ", paste(variables_demo, collapse=' + ')))
summary(lm(formula_perte_absurde, subset=km <= 1000 | (fioul != T & gaz != T), data=s, weights=s$weight)) # adjusted R^2: 0.01, R^2: 0.04

decrit(s$gain_chauffage[s$perte_relative_chauffage=='= Moyenne'], weights=s$weight[s$perte_relative_chauffage=='= Moyenne']) # médiane à 0, moyenne à -30
decrit((s$gain_chauffage==0)[s$perte_relative_chauffage=='= Moyenne'], weights=s$weight[s$perte_relative_chauffage=='= Moyenne']) # 40% à 0, 
decrit(s$gain_fuel[s$perte_relative_fuel=='= Moyenne'], weights=s$weight[s$perte_relative_fuel=='= Moyenne']) # 40% à 0, moyenne à -44
decrit(s$gain_partielle[s$perte_relative_partielle==0], weights=s$weight[s$perte_relative_partielle==0]) # médiane à 0, 35% à 0, moyenne à -38
decrit((s$gain_partielle>0)[s$perte_relative_partielle==0], weights=s$weight[s$perte_relative_partielle==0]) # médiane à 0, 16% > 0

decrit(s$perte_relative_chauffage, weights = s$weight) # médiane: = Moyenne, 40% + dont 25% bcp +
decrit(s$perte_relative_chauffage[(s$fioul | s$gaz) == FALSE], weights = s$weight[(s$fioul | s$gaz) == FALSE]) # médiane: = Moyenne, 40% + dont 25% bcp +
decrit(s$perte_relative_fuel[s$km <= 1000], weights = s$weight[s$km <= 1000]) # comme ci-dessus
decrit(s$perte_relative_chauffage[(s$fioul | s$gaz) == FALSE & s$gain_chauffage<0], weights = s$weight[(s$fioul | s$gaz) == FALSE & s$gain_chauffage<0]) # 46%: bcp +
decrit(s$perte_relative_fuel[s$km <= 1000 & s$gain_fuel<0], weights = s$weight[s$km <= 1000 & s$gain_fuel<0]) # comme ci-dessus
decrit(s$perte_relative_partielle[(s$km <= 1000 & s$gain_fuel<0) | ((s$fioul | s$gaz) == FALSE & s$gain_chauffage<0)], weights = s$weight[(s$km <= 1000 & s$gain_fuel<0) | ((s$fioul | s$gaz) == FALSE & s$gain_chauffage<0)]) # comme ci-dessus

decrit((s$gain<0)[s$simule_gain > 90], weights = s$weight[s$simule_gain > 90]) # 50% sur 1100 personnes 
decrit((s$gain<0)[s$simule_gain >= 110], weights = s$weight[s$simule_gain >= 110]) # 47% sur 700 personnes 


##### Correlates of the bias #####
# taille_agglo, composition du ménage et sexe corrélés, mais ça reste très idiosyncratique
wtd.mean(s$simule_gain - s$gain > 50, weights = s$weight) # 75%
decrit(s$simule_gain - s$gain, weights = s$weight)
summary(lm(as.formula(paste("(simule_gain - gain > 110) ~ as.factor(taille_agglo) + diplome +", paste(variables_demo, collapse=' + '))), data=s, weights=s$weight)) # nb_adultes: 0.11, Masculin: -0.05, R^2: 0.04 (la moitié due à la composition du ménage)
summary(lm(as.formula(paste("(simule_gain - gain > 50) ~ as.factor(taille_agglo) + diplome +", paste(variables_demo, collapse=' + '))), data=s, weights=s$weight)) # nb_adultes: 0.11, Masculin: -0.05, R^2: 0.04 (la moitié due à la composition du ménage)
summary(lm(as.formula(paste("(simule_gain - gain) / simule_gain ~ as.factor(taille_agglo) + diplome +", paste(variables_demo, collapse=' + '))), data=s, weights=s$weight)) # uc, nb_adultes: +, nb>14, taille_menage: -, R^2: 0.03 (la moitié due à la composition du ménage)
summary(lm(as.formula(paste("(simule_gain - gain > 50) ~ ", paste(variables_demo[!(variables_demo %in% c('taille_menage', 'nb_14_et_plus', 'nb_adultes', 'uc'))], collapse=' + '))), data=s, weights=s$weight))
summary(lm(as.formula(paste("(simule_gain - gain) ~ ", paste(variables_demo[!(variables_demo %in% c('taille_menage', 'nb_14_et_plus', 'nb_adultes', 'uc'))], collapse=' + '))), data=s, weights=s$weight)) # taille_agglo: -4
summary(lm(as.formula(paste("gain ~ ", paste(variables_demo, collapse=' + '))), data=s, weights=s$weight)) # revenu: -4.4 / k€, taille_agglo: +10, age 18-34: +30, R^2: 0.04
summary(lm(as.formula(paste("gain ~ as.factor(taille_agglo) + diplome +", paste(variables_demo, collapse=' + '))), data=s, weights=s$weight)) # revenu: -4.4 / k€, taille_agglo: +10, age 18-34: +30, R^2: 0.04

plot(s$simule_gain, jitter(s$gain, 10), xlim=c(-400,300), type='p', col='blue', cex=0.1, xlab='Simulated gain', ylab='Subjective gain')
abline(lm(gain ~ simule_gain, data=ss), col='blue', lwd=2)
lines(seq(-500, 500, by=10), seq(-500, 500, by=10), type='l', col='black') + grid()
abline(h = 0, v=0)

summary(lm(gain ~ simule_gain, data=s, weights=s$weight))
summary(lm(gain ~ simule_gain + I(simule_gain^2), data=s, weights=s$weight))
summary(lm(as.formula(paste("gain ~ simule_gain + ", paste(variables_demo, collapse=' + '))), data=s, weights=s$weight))
summary(lm(as.formula(paste("gain ~ simule_gain + I(simule_gain^2)", paste(variables_demo, collapse=' + '))), data=s, weights=s$weight))
summary(lm(as.formula(paste("simule_gain  gain ~ ", paste(variables_demo, collapse=' + '))), data=s, weights=s$weight))


##### Gain (dividende - hausse dépenses énergétiques) #####
decrit(110 * pmin(2, s$nb_adultes) - s$hausse_depenses, weights = s$weight) # Ok !
decrit(s$gain, weights = s$weight) 
decrit(s$gagnant_categorie, weights = s$weight)
decrit(s$gagnant_feedback_categorie, weights = s$weight)
decrit(s$gagnant_feedback_categorie[s$hausse_depenses < 110 * s$nb_adultes], weights = s$weight[s$hausse_depenses < 110 * s$nb_adultes]) 
# On ne les convainc pas !!!
length(which(s$gagnant_feedback_categorie=='Gagnant' & s$hausse_depenses < 110 * s$nb_adultes))/length(which(!is.na(s$gagnant_feedback_categorie) & s$hausse_depenses < 110 * s$nb_adultes))
length(which(s$gagnant_feedback_categorie=='Gagnant' & s$gagnant_categorie != 'Gagnant' & s$hausse_depenses < 110 * s$nb_adultes))/length(which(!is.na(s$gagnant_feedback_categorie) & s$hausse_depenses < 110 * s$nb_adultes & s$gagnant_categorie != 'Gagnant'))
decrit(s$gagnant_progressif_categorie, weights = s$weight)

## Plot CDF
cdf_gain <- wtd.Ecdf(s$gain, weights = s$weight)
plot(cdf_gain$x, cdf_gain$ecdf, type='s', lwd=2, col='orange', xlab="Category of subjective gain", ylab="Distribution of answers") + grid()

decrit(s$simule_gagnant_interaction != s$simule_gagnant, weights = s$weight)
decrit(1*(s$simule_gain_inelastique > 0) != s$simule_gagnant, weights = s$weight)
decrit(s$gain > s$simule_gain, weights = s$weight)
decrit(s$gain > s$simule_gain_interaction, weights = s$weight)
decrit(s$gain > s$simule_gain_inelastique, weights = s$weight)
decrit(s$gain > s$simule_gain_elast_perso, weights = s$weight)
# TODO: pourquoi si peu d'erreur pour les gains négatifs ? parce qu'on utilise la spécification (1): on a la bonne courbe en prenant la spécification (2)
ggplot(data=fit_housing, aes(x=vrai_gain_chauffage)) + 
  geom_smooth(method = "auto", se=F, aes(y=1*(estimation_gain_chauffage > 0))) + ylim(c(0,1)) + xlab("Objective gain without fuel (density in black)") + ylab("P(gain - (hausse_carburants-60) > 0) i.e. proba gain") + xlim(c(-200, 120)) + geom_density(aes(y=..scaled..)) + geom_vline(xintercept=0, col='red')
ggplot(data=fit_housing, aes(x=gain)) + 
  geom_smooth(method = "auto", se=F, aes(y=1*(predicted_gain > 0))) + ylim(c(0,1)) + xlab("Objective gain per consumption unit, without fuel (density in black)") + ylab("P(gain - (hausse_carburants-60) > 0) i.e. proba gain") + xlim(c(-150, 100)) + geom_density(aes(y=..scaled..)) + geom_vline(xintercept=0, col='red')
# ggplot(data=fit_housing, aes(x=obj)) + geom_smooth(aes(y=1*(fit < 110)), method = "glm", method.args = list(family = "binomial")) + ylim(c(0,1)) + xlab("Objective housing expenditure increase (density in black)") + ylab("P(hausse_chauffage_interaction < 110) i.e. proba gain") + xlim(c(0, 500)) + geom_density(aes(y=..scaled..)) + geom_vline(xintercept=110, col='red')
# length(which(fit_housing$obj > 50 & fit_housing$obj < 200))/length(fit_housing$obj) # 25%
# length(which(fit_housing$obj > 75 & fit_housing$obj < 170))/length(fit_housing$obj) # 15%
length(which(fit_housing$vrai_gain_chauffage > -50 & fit_housing$vrai_gain_chauffage < 50))/length(fit_housing$vrai_gain_chauffage) # 20% de chances d'avoir une proba entre 0.1 et 0.9
length(which(fit_housing$estimation_gain_chauffage > 0 & fit_housing$vrai_gain_chauffage==50))/length(which(fit_housing$vrai_gain_chauffage==50))
sum(s$weight[s$simule_gain_interaction > -50 & s$simule_gain_interaction < 50])/sum(s$weight) # 21%
ggplot(data=s, aes(x=hausse_carburants)) + geom_density() + xlim(c(0, 300))
ggplot(data=s, aes(x=simule_gain_interaction)) + geom_density() + xlim(c(-200, 300))
ggplot(data=s, aes(x=60 * pmin(2, nb_adultes) - hausse_carburants)) + geom_density() + xlim(c(-100, 200))
mean(fit_housing_2$estimation_gain_chauffage[fit_housing_2$vrai_gain_chauffage < 0] > 0) # 7%
fit_housing_2$vrai_gain_chauffage <- 50 * pmin(2, fit_housing_2$nb_adultes) - fit_housing_2$obj
fit_housing_2$estimation_gain_chauffage <- 50 * pmin(2, fit_housing_2$nb_adultes) - fit_housing_2$fit
ggplot(data=fit_housing_2, aes(x=vrai_gain_chauffage)) + 
  geom_smooth(method = "auto", aes(y=1*(estimation_gain_chauffage > 0))) + ylim(c(0,1)) + xlab("Objective gain without fuel (density in black)") + ylab("P(gain - (hausse_carburants-60) > 0) i.e. proba gain") + xlim(c(-200, 120)) + geom_density(aes(y=..scaled..)) + geom_vline(xintercept=0, col='red')
ggplot(data=fit, aes(x=gain)) + 
  geom_smooth(method = "auto", aes(y=predicted_winner), se=F) + ylim(c(0,1)) + xlab("Objective gain (density in black)") + ylab("Probability of predicting gain") + xlim(c(-400, 240)) + geom_density(aes(y=..scaled..)) + geom_vline(xintercept=0, col='red')
mean(fit$gain > -100 & fit$gain < 100) # 53% de chances d'avoir une proba entre 0.1 et 0.9
mean(fit$gain > -30 & fit$gain < 50) # 22% de chances d'avoir une proba entre 0.25 et 0.75
sum(s$weight[s$simule_gain_interaction > -50 & s$simule_gain_interaction < 50])/sum(s$weight) # 21%
sum(s$weight[s$simule_gain_interaction > -30 & s$simule_gain_interaction < 50])/sum(s$weight) # 21%
mean(fit$predicted_winner[fit$winner==0] == 1)
mean(fit$predicted_winner[fit$winner==1] == 0)
mean(fit$predicted_winner[fit$gain < -50]) # 10%
mean(fit$predicted_winner[fit$gain > 50]==0) # 10%
mean(fit$predicted_winner[fit$gain < -45 & fit$gain > -55]) # 18%
mean(fit$predicted_winner[fit$gain < 55 & fit$gain > 45]==0) # 25%
mean(fit$gain < -45 & fit$gain > -55) # 2%
mean(fit$gain < 55 & fit$gain > 45) # 4%
mean(fit$gain < -50 | fit$gain > 50) # 74%
mean(fit$predicted_winner[fit$gain < -60 & fit$gain > -70]) # 16.7%
mean(fit$gain < -60 & fit$gain > -70) # 2%
mean(fit$predicted_winner[fit$gain < 95 & fit$gain > 85]==0) # 7.3%
mean(fit$gain < 95 & fit$gain > 85) # 3.4%
mean(fit$predicted_winner[fit$gain < -85 & fit$gain > -95]) # 
mean(fit$predicted_winner[fit$gain < -90]) # 
mean(fit$predicted_winner[fit$gain < -65]) # 9%
mean(fit$predicted_winner[fit$gain > 90]) #
mean(fit$predicted_winner[fit$gain > 90]==0) # 5
mean(fit$gain < -65 | fit$gain > 90) # 56%
fit_2$gain <- 110 * fit_2$nb_beneficiaries - fit_2$total_expenditures_increase
ggplot(data=fit_2, aes(x=gain)) + 
  geom_smooth(method = "auto", aes(y=predicted_winner), se=F) + ylim(c(0,1)) + xlab("Objective gain (density in black)") + ylab("P(gain > 0) i.e. proba gain") + xlim(c(-400, 240)) + geom_density(aes(y=..scaled..)) + geom_vline(xintercept=0, col='red')
prediction_gain <- lm(gain ~ predicted_gain, data=fit)
summary(prediction_gain)
predicted_gain <- predict(prediction_gain, interval='predict', level=0.9)
mean(predicted_gain[,3] - predicted_gain[,2]) / 2 # 130: demi-largeur de l'intervalle de confiance à 90%
predicted_gain <- predict(prediction_gain, interval='predict', level=0.834)
mean(predicted_gain[,3] - predicted_gain[,2]) / 2 # 110: demi-largeur de l'intervalle de confiance à 83.4%
plot(fit$predicted_gain, fit$gain, cex=0.1, xlim=c(-400,240), ylim=c(-400, 240), col='blue')
lines(c(-400, 240), c(-400, 240), col='green', type='l', lwd=2)
lines(fit$predicted_gain[1:100], predicted_gain[1:100,1], xlim=c(-400,240), ylim=c(-400, 240), type='l', col='red', lwd=2)
lines(fit$predicted_gain[1:100], predicted_gain[1:100,2], type='l', lwd=2)
lines(fit$predicted_gain[1:100], predicted_gain[1:100,3], type='l', lwd=2)

sd(fit_2$predicted_gain - fit_2$gain) # 55
prediction_gain <- lm(gain ~ predicted_gain, data=fit_2) # Good specification: (2) interaction
summary(prediction_gain)
predicted_gain <- predict(prediction_gain, interval='predict', level=0.9)
mean(predicted_gain[,3] - predicted_gain[,2]) / 2 # 87: demi-largeur de l'intervalle de confiance à 90%
predicted_gain <- predict(prediction_gain, interval='predict', level=0.75)
mean(predicted_gain[,3] - predicted_gain[,2]) / 2 # 60: demi-largeur de l'intervalle de confiance à 63%
predicted_gain <- predict(prediction_gain, interval='predict', level=0.6667)
mean(predicted_gain[,3] - predicted_gain[,2]) / 2 # 51: demi-largeur de l'intervalle de confiance à 83.4%

prediction_gain <- lm(gain ~ predicted_gain, data=fit_housing)
summary(prediction_gain)
predicted_gain_housing <- predict(prediction_gain, interval='predict', level=0.9)
mean(predicted_gain_housing[,3] - predicted_gain_housing[,2]) / 2 # 92: demi-largeur de l'intervalle de confiance à 90%
predicted_gain_housing <- predict(prediction_gain, interval='predict', level=0.63)
mean(predicted_gain_housing[,3] - predicted_gain_housing[,2]) / 2 # 50: demi-largeur de l'intervalle de confiance à 63%
predicted_gain_housing <- predict(prediction_gain, interval='predict', level=0.834)
mean(predicted_gain_housing[,3] - predicted_gain_housing[,2]) / 2 # 77: demi-largeur de l'intervalle de confiance à 83.4%
plot(fit_housing$predicted_gain, fit_housing$gain, cex=0.1, xlim=c(-200,110), ylim=c(-400, 60), col='blue')
lines(c(-400, 240), c(-400, 240), col='green', type='l', lwd=2)
lines(fit_housing$predicted_gain[c(1,nrow(fit_housing))], predicted_gain_housing[c(1,nrow(fit_housing)),1], xlim=c(-200,60), ylim=c(-400, 110), type='l', col='red', lwd=2)
lines(fit_housing$predicted_gain[c(1,nrow(fit_housing))], predicted_gain_housing[c(1,nrow(fit_housing)),2], type='l', lwd=2)
lines(fit_housing$predicted_gain[c(1,nrow(fit_housing))], predicted_gain_housing[c(1,nrow(fit_housing)),3], type='l', lwd=2)
grid() + abline(h=0, v=0)

prediction_gain <- lm(gain ~ predicted_gain, data=fit_housing_2)
summary(prediction_gain)
predicted_gain_housing <- predict(prediction_gain, interval='predict', level=0.9)
mean(predicted_gain_housing[,3] - predicted_gain_housing[,2]) / 2 # 80: demi-largeur de l'intervalle de confiance à 90%
predicted_gain_housing <- predict(prediction_gain, interval='predict', level=0.834)
mean(predicted_gain_housing[,3] - predicted_gain_housing[,2]) / 2 # 68: demi-largeur de l'intervalle de confiance à 83.4%
plot(fit_housing_2$predicted_gain, fit_housing_2$gain, cex=0.1, xlim=c(-200,110), ylim=c(-400, 60), col='blue')
lines(c(-400, 240), c(-400, 240), col='green', type='l', lwd=2)
lines(fit_housing_2$predicted_gain[c(1,nrow(fit_housing_2))], predicted_gain_housing[c(1,nrow(fit_housing_2)),1], xlim=c(-200,60), ylim=c(-400, 110), type='l', col='red', lwd=2)
lines(fit_housing_2$predicted_gain[c(1,nrow(fit_housing_2))], predicted_gain_housing[c(1,nrow(fit_housing_2)),2], type='l', lwd=2)
lines(fit_housing_2$predicted_gain[c(1,nrow(fit_housing_2))], predicted_gain_housing[c(1,nrow(fit_housing_2)),3], type='l', lwd=2)
grid() + abline(h=0, v=0)

prediction_gain_chauffage <- lm(vrai_gain_chauffage ~ estimation_gain_chauffage, data=fit_housing)
summary(prediction_gain_chauffage)
predicted_gain_chauffage <- predict(prediction_gain_chauffage, interval='predict', level=0.9)
mean(predicted_gain_chauffage[,3] - predicted_gain_chauffage[,2]) / 2 # 133: demi-largeur de l'intervalle de confiance à 90%
predicted_gain_chauffage <- predict(prediction_gain_chauffage, interval='predict', level=0.834)
mean(predicted_gain_chauffage[,3] - predicted_gain_chauffage[,2]) / 2 # 112: demi-largeur de l'intervalle de confiance à 83.4%
plot(fit_housing$vrai_gain_chauffage, fit_housing$estimation_gain_chauffage, cex=0.1, xlim=c(-400, 110), ylim=c(-250,110), col='blue')
lines(c(-400, 240), c(-400, 240), col='green', type='l', lwd=2)
lines(predicted_gain_chauffage[c(1,nrow(fit_housing)),1], fit_housing$estimation_gain_chauffage[c(1,nrow(fit_housing))], xlim=c(-400, 110), ylim=c(-250,110), type='l', col='red', lwd=2)
lines(16.1 + predicted_gain_chauffage[c(1,nrow(fit_housing)),1], fit_housing$estimation_gain_chauffage[c(1,nrow(fit_housing))], xlim=c(-400, 110), ylim=c(-250,110), type='l', col='red', lwd=2, lty=2)
lines(predicted_gain_chauffage[c(1,nrow(fit_housing)),2], fit_housing$estimation_gain_chauffage[c(1,nrow(fit_housing))], type='l', lwd=2)
lines(predicted_gain_chauffage[c(1,nrow(fit_housing)),3], fit_housing$estimation_gain_chauffage[c(1,nrow(fit_housing))], type='l', lwd=2)
grid() + abline(h=0, v=0)

fit_housing_2 <- fit_housing_2[order(fit_housing_2$estimation_gain_chauffage, decreasing=T),]
prediction_gain_chauffage <- lm(vrai_gain_chauffage ~ estimation_gain_chauffage, data=fit_housing_2)
summary(prediction_gain_chauffage)
predicted_gain_chauffage <- predict(prediction_gain_chauffage, interval='predict', level=0.9)
mean(predicted_gain_chauffage[,3] - predicted_gain_chauffage[,2]) / 2 # 124: demi-largeur de l'intervalle de confiance à 90%
predicted_gain_chauffage <- predict(prediction_gain_chauffage, interval='predict', level=0.834)
mean(predicted_gain_chauffage[,3] - predicted_gain_chauffage[,2]) / 2 # 105: demi-largeur de l'intervalle de confiance à 83.4%
plot(fit_housing_2$vrai_gain_chauffage, fit_housing_2$estimation_gain_chauffage, cex=0.1, xlim=c(-400, 110), ylim=c(-250,110), col='blue')
lines(c(-400, 240), c(-400, 240), col='green', type='l', lwd=2)
lines(predicted_gain_chauffage[c(1,nrow(fit_housing_2)),1], fit_housing_2$estimation_gain_chauffage[c(1,nrow(fit_housing_2))], xlim=c(-400, 110), ylim=c(-250,110), type='l', col='red', lwd=2)
lines(predicted_gain_chauffage[c(1,nrow(fit_housing_2)),2], fit_housing_2$estimation_gain_chauffage[c(1,nrow(fit_housing_2))], type='l', lwd=2)
lines(predicted_gain_chauffage[c(1,nrow(fit_housing_2)),3], fit_housing_2$estimation_gain_chauffage[c(1,nrow(fit_housing_2))], type='l', lwd=2)
grid() + abline(h=0, v=0)

mean(fit$mistake[fit$gain > 50]) # 7%
mean(fit$mistake[fit$gain > 65]) # 5%
mean(fit$mistake[fit$gain > 100]) # 1.8%
mean(fit$mistake[fit$gain > 110]) # 1%
mean(fit$mistake[fit$gain > 105 & fit$gain < 115]) # 1.2%
mean(fit$gain > 105 & fit$gain < 115) # 1%
mean(fit$mistake[fit$gain > 60 & fit$gain < 70]) # 14%
mean(fit$gain > 60 & fit$gain < 70) # 6%
mean(fit$mistake[fit$gain > 97 & fit$gain < 103]) # 5%
mean(fit$gain > 97 & fit$gain < 103) # 2.4%
mean(fit$mistake[fit$gain > 45 & fit$gain < 55]) # 16.4%
mean(fit$mistake[fit$predicted_gain - fit$gain > 60 & fit$predicted_gain - fit$gain < 70]) # 25%
mean(fit$mistake[fit$predicted_gain - fit$gain > 60 & fit$predicted_gain - fit$gain < 70]) # 25%
mean(fit_housing$error[fit_housing$predicted_gain - fit_housing$gain > 50]) # 6%
mean(fit_housing$error[fit_housing$predicted_gain - fit_housing$gain > 0]) # 4%
mean(fit_housing$error[fit_housing$gain > 50]) # 5%
mean(fit$mistake[fit$gain > 50]) # 7%
mean(fit_housing$error[fit_housing$predicted_gain > 50]) # 0
mean(fit$mistake[fit$predicted_gain > 50]) # 1%
mean(fit$mistake[fit$predicted_gain < -50]) # 1%
mean(fit$mistake)
wtd.mean(s$gagnant_categorie=='Gagnant' & s$simule_gagnant==0) # 1.8%
wtd.mean(s$gagnant_categorie=='Gagnant' & s$simule_gain < -50) # 0.8%
wtd.mean(s$simule_gain - s$gain > 50) # 75%
wtd.mean(s$simule_gain - s$gain > 65) # 70%
wtd.mean(s$simule_gain - s$gain > 110) # 53%
wtd.mean(s$simule_gain - s$gain > 160) # 33%
decrit(s$simule_gain - s$gain, weights = s$weight)

##### Approbation #####
decrit(s$taxe_approbation, miss=T, weights=s$weight) # 10% Dur !!!
decrit(s$taxe_approbation[s$gilets_jaunes_soutien==TRUE])
decrit(s$taxe_approbation[s$gilets_jaunes_oppose==TRUE])
decrit(s$taxe_feedback_approbation) # 17%
decrit(s$taxe_efficace) # 18%
decrit(s$taxe_progressif_approbation) # 19%
summary(lm((taxe_feedback_approbation=='Oui') ~ (gagnant_categorie!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_progressif_approbation=='Oui') ~ (gagnant_categorie!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_approbation=='Oui') ~ (taxe_efficace!='Non') + (gagnant_categorie!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_approbation=='Oui') ~ score_climate_call + score_polluants + (gagnant_categorie!='Perdant'), data=s, weights = s$weight))

decrit(s$taxe_approbation[s$gagnant_categorie=='Gagnant'], miss=T) # 32% de ceux qui s'estiment gagnant approuvent
decrit(s$taxe_feedback_approbation[s$gagnant_categorie=='Gagnant'], miss=T) # 32% de ceux qui s'estiment gagnant approuvent après info
decrit(s$taxe_feedback_approbation[s$gagnant_feedback_categorie=='Gagnant'], miss=T) # 52% de ceux qui s'estiment gagnant après info approuvent après info
decrit(s$taxe_feedback_approbation[s$gagnant_categorie!='Gagnant' & s$gagnant_feedback_categorie=='Gagnant'], miss=T) 
# 48% de ceux qu'on fait changer d'avis approuvent

decrit(s$taxe_approbation[s$simule_gagnant==1], miss=T) # 68% des gagnants rejettent
decrit(s$taxe_approbation[s$simule_gagnant==1 & s$gagnant_feedback_categorie=='Gagnant' & s$gagnant_categorie!='Gagnant'], miss=T) # 51% des gagnants qu'on fait changer d'avis rejettent
decrit(s$taxe_feedback_approbation[s$simule_gagnant==1 & s$gagnant_categorie!='Gagnant' & s$gagnant_feedback_categorie=='Gagnant'], miss=T) 
decrit(s$taxe_approbation[s$simule_gagnant==1 & s$gagnant_feedback_categorie=='Gagnant'], miss=T) # 41% des gagnants qui nous croient rejettent
decrit(s$taxe_feedback_approbation[s$simule_gagnant==1 & s$gagnant_feedback_categorie=='Gagnant'], miss=T) # 21% de ceux qui croient à l'info qu'ils sont gagnants rejettent après info
decrit(s$taxe_feedback_approbation[s$simule_gagnant==1 & s$gagnant_feedback_categorie!='Gagnant'], miss=T) # 71% de ceux qui ne croient pas à l'info qu'ils sont gagnants rejettent après info

decrit(s$taxe_approbation, weights=s$weight, miss=T) # Oui/Non 10/ 70
decrit(s$taxe_info_approbation, weights=s$weight, miss=T) # 16 / 64
decrit(s$taxe_feedback_approbation[(s$gagnant_info_categorie=='Gagnant' & s$simule_gagnant==1) | (s$gagnant_info_categorie=='Perdant' & s$simule_gagnant==0)], weights=s$weight[(s$gagnant_info_categorie=='Gagnant' & s$simule_gagnant==1) | (s$gagnant_info_categorie=='Perdant' & s$simule_gagnant==0)], miss=T) # 29 / 54
decrit(s$taxe_feedback_approbation[(s$gagnant_info_categorie=='Gagnant' & s$simule_gagnant==1)], weights=s$weight[(s$gagnant_info_categorie=='Gagnant' & s$simule_gagnant==1)], miss=T) # 53 / 21
decrit(s$taxe_feedback_approbation[s$gagnant_info_categorie=='Gagnant'], weights=s$weight[s$gagnant_info_categorie=='Gagnant'], miss=T) # 53 / 21


# Approbation des répondants biaisés
summary(lm(taxe_approbation != 'Non'~ (simule_gain - gain > 110) + simule_gagnant + simule_gain, data=s, subset=(gagnant_categorie == 'Perdant'), weights = s$weight))
summary(lm(taxe_approbation != 'Non'~ I((simule_gain - gain)/100) + simule_gagnant + simule_gain, data=s, subset=(gagnant_categorie == 'Perdant'), weights = s$weight))

# Gain selon l'approbation parmi les gagnants pessimistes
decrit(s$gain[s$taxe_approbation=='Oui' & s$simule_gagnant==1 & s$gagnant_categorie=='Perdant'])
decrit(s$gain[s$taxe_approbation=='Non' & s$simule_gagnant==1 & s$gagnant_categorie=='Perdant'])
decrit(s$gain[s$taxe_approbation=='NSP' & s$simule_gagnant==1 & s$gagnant_categorie=='Perdant'])
# same effect of taxe_approbation when we restrict to simule_gagnant==1
summary(lm(as.formula(paste("update_correct ~ ", paste(variables_update_bis, collapse=' + '))), subset = feedback_infirme_large==T & simule_gagnant==1, data=s, weights = s$weight))
# effect of gain in accordance with new interpretation
summary(lm(as.formula(paste("update_correct ~ gain + (gain==0) + ", paste(variables_update_bis, collapse=' + '))), subset = feedback_infirme_large==T, data=s, weights = s$weight))
# effect of gain in accordance with new interpretation (restricted to simule_gagnant==1)
summary(lm(as.formula(paste("update_correct ~ gain + (gain==0) + ", paste(variables_update_bis, collapse=' + '))), subset = feedback_infirme_large==T & simule_gagnant==1, data=s, weights = s$weight))
# # no effect of gain (even interacted) on simule_gagnant==1
# summary(lm(as.formula(paste("update_correct ~ gain*taxe_approbation + ", paste(variables_update_bis, collapse=' + '))), subset = feedback_infirme_large==T & simule_gagnant==1, data=s, weights = s$weight))
# # no effect of gain (even interacted) nor of taxe_approbation on simule_gagnant==0
# summary(lm(as.formula(paste("update_correct ~ gain*taxe_approbation + ", paste(variables_update_bis[variables_update_bis!="conservateur"], collapse=' + '))), subset = feedback_infirme_large==T & simule_gagnant==0, data=s, weights = s$weight))
# # many interacted effects but driven by the fact that taxe_approbation has an effect only for simule_gagnant==1
# summary(lm(as.formula(paste("update_correct ~ gain*taxe_approbation*simule_gagnant + ", paste(variables_update_bis, collapse=' + '))), subset = feedback_infirme_large==T, data=s, weights = s$weight))
# # for (v in variables_update_bis) print(decrit(s[[v]][s$feedback_infirme_large==T & s$simule_gagnant==0]))


##### Approbation: Model Selection #####
# Other method of model selection exists (like forward/backward stepwise selection), but seem less reliable as they do not
#   explore exhaustively all possible models nor do they minimize anything. Best-subset selection is exhaustive, but I am
#   afraid that it be too computationally intensive. TODO: test them (cf. medium.com/cracking-the-data-science-interview/1ef6dbd531f7)
# (as far as I understood) There are two ways to use LASSO: (cf. web.stanford.edu/~hastie/glmnet/glmnet_alpha.html, wikipedia for lasso and CV)
#   - glmnet (without cross-validation) decreases lambda (i.e. increases the norm of beta and the number of regressors)
#     until the minimum of (null) deviance explained (~ max adj-R^2) is reached, "lassoing" variables one after the other
#   - cv.glmnet (with cross-validation (CV)) uses 10-fold CV (cf. wikipedia>CV) to select the lambda that minimizes
#     "out-of-the-sample" deviance* (avoiding too low lambda that induce overfitting): lambda.min. lambda.1se is an
#     alternative choice for lambda that allow less regressors but stays with 1 s.e. of deviance relative to lambda.min
#       * actually type.measure = class/mae/deviance/auc minimizes misclassification/mean absolute error/deviance/area under ROC curve
# Elastic net is a compromise between Ridge regression (L^2 norm penalty) and lasso (L^1 penalty), with a penalty
#   alpha*lasso + (1-alpha)*Ridge. Ridge bias all estimates to 0 instead of removing some of them (and biasing some others)
#   as lasso does. But Ridge allows to keep variables that are correlated (while lasso would then to keep only of one them). 
#   How to choose alpha? Define a 10-fold, use it to run cv.glmnet on different values for alpha, and select the one 
#   whose lambda.min minimizes deviance (cf. Zou & Hastie, 2005). 
# The Composite Absolute Penalties allows to group categorical variables: all in or all out of the model for a given variable 
#   (introduced by Zhao, Rocha &  Yu (2009) who only coded it in matlab; R package quadrupen provides a related method cran.r-project.org/web/packages/quadrupen/quadrupen.pdf)
# Multinomial regression is an alternative to logit, as we have 3 categories in our dependant variable (Oui/Non/NSP)
#   This is family="multinomial". type.multinomial="grouped" forces that the same variables be taken for our categories (Oui/Non/NSP)

variables_toutes_sauf_approbation <- setdiff(variables_toutes, variables_approbation)
variables_wo_missing <- variables_toutes_sauf_approbation
for (v in variables_toutes_sauf_approbation) { # display and remove variables with missing values
  # na_v <- length(which(is.na(s[[v]]) | is.nan(s[[v]]) | is.infinite(s[[v]])))
  if (length(intersect(c("labelled", "character.item", "double.item"), class(s[[v]])))) na_v <- length(which(is.na(s[[v]])))
  else na_v <- length(which(is.missing(s[[v]])))
  if (na_v>0) {
    # print(paste(v, na_v))
    variables_wo_missing <- variables_wo_missing[variables_wo_missing!=v] }
}
# summary(lm(as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_wo_missing, collapse=' + '))), data=ss))
x <- model.matrix(as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_wo_missing, collapse=' + '))),  data=ss)
y <- ifelse(s$taxe_approbation=="Oui", 1, 0)
# cexplore other type.measure options

# fit <- glmnet(x, y, alpha=1, family="binomial", weights = s$weight)
lasso_dev <- cv.glmnet(x, y, alpha=1, family="binomial", weights = s$weight) # run parallel computing: parallel = T;  residual deviance: 1705, AIC: 1834
lasso_class <- cv.glmnet(x, y, alpha=1, type.measure = "class", family="binomial", weights = s$weight) # min missclassification error is 0.064 ! (very small) residual deviance: 1600, AIC: 1764
lasso_mae <- cv.glmnet(x, y, alpha=1, type.measure = "mae", family="binomial", weights = s$weight) # residual deviance: 1390, AIC: 1626

# smaller alpha yield more selected variables, a decrease of AIC and residual variance, but no decrease of minimum binomial variance
foldid=sample(1:10, size=length(y), replace=TRUE) # TODO: difference residual variance (output glm) and binomial variance (output cross validation)?
cv1 <- cv.glmnet(x, y, foldid=foldid, alpha=1, family="binomial", weights = s$weight) # lasso (residual deviance: 1695, AIC: 1836)
cv.5 <- cv.glmnet(x, y, foldid=foldid, alpha=.5, family="binomial", weights = s$weight) # elastic net (residual deviance: 1646, AIC: 1791)
cv0 <- cv.glmnet(x, y, foldid=foldid, alpha=0, family="binomial", weights = s$weight) # Ridge: all variables selected (residual deviance: 1152, AIC: 1509)
cv0_mae <- cv.glmnet(x, y, alpha=0, type.measure = "mae", family="binomial", weights = s$weight) # all variables selected. residual deviance: 1152, AIC: 1509

lasso <- lasso_dev
plot(lasso)
min(lasso$cvm) # min deviance. alpha = 1: 0.3399301 / 0.5: 0.3393227 / 0: 0.3456004 ; alpha = 1 and class: 0.064 / mae: 0.18
coefs_lasso <- coef(lasso, s="lambda.1se") # lambda.1se contains less variables than lambda.min
coefs_lasso <- coef(lasso, s="lambda.min")
data.frame(name = coefs_lasso@Dimnames[[1]][coefs_lasso@i + 1], coefficient = coefs_lasso@x) # doesn't work for multinomial
selected_variables <- coefs_lasso@i - 1 
selected_variables <- selected_variables[selected_variables > 0 & selected_variables <= length(variables_wo_missing)]
selected_variables <- variables_wo_missing[selected_variables] # length(selected_variables) (.min) when alpha = 1: 58 / 0.5: 62 / 0: 142 (all) ; alpha = 1 and class: 67 / mae: 97
selected_variables[duplicated(selected_variables)]
summary(glm(as.formula(paste("(taxe_approbation!='Non') ~", paste(selected_variables, collapse=' + '))), binomial, data=s, weights=s$weight))


significatifs_lasso_dev_oui <- as.formula((taxe_approbation=='Oui') ~ age + fume + niveau_vie + hausse_diesel + (transports_loisirs=="La voiture") + gilets_jaunes_oppose 
                                          + perte_relative_partielle + taxe_perdant_pauvres + benefices_CC + benefices_aucun + problemes_aucun
                                          + si_compensee + peages_urbains + ges_O2 + ges_avion + changer_si_tous)
summary(glm(significatifs_lasso, binomial, data=s, weights=s$weight))


##### OLS Approbation ####
summary(lm(as.formula(paste("(taxe_approbation!='Non') ~", paste(selected_variables, collapse=' + '))), data=s, weights=s$weight))
summary(lm(significatifs_lasso, data=s, weights=s$weight))
summary(lm(as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_big_regression, collapse=' + '))), data=ss))

s_prog <- s[!is.na(s$info_progressivite),]
s_prog <- s[s$info_progressivite == 1,]
summary(lm(taxe_progressif_approbation ~ (progressivite != 'Non') + (gagnant_progressif_categorie != 'Non') + taxe_efficace, data=s_prog))

###### Approbation: ANOVA #####
# ANalysis Of VAriance partitions sequentially the error terms of a regression where regressors are categorical variables. 
#   This gives the Sum of squares (SS) of each variable, which depends on the rank of the variable in the list (I think that variables 
#   have higher sum of squares when they are treated first). (SS are rank-independent when the sample is balanced, i.e. when there are the same number of observations in each category)
#   This generalizes from OLS to logit (and other) using deviance; to continuous regressors (TODO: how? if we don't know, we could still make them categorical)
# Is there a canonical way to decompose the variance for unbalanced samples? Like averaging SS from random draws on the ordering of the variables?
#   I haven't found. en.wikipedia.org/wiki/Repeated_measures is something else. The more I dig, the more I realize ANOVA is fit for another
#   purpose: when we can decompose the sample into several groups (as if there were only one categorical variable, or different but with all interaction terms)

variables_big_regression <- c("revenu", "rev_tot", "hausse_carburants", "hausse_chauffage", "score_climate_call", "score_ges", "Gauche_droite",
                              "emission_cible", "effets_CC", "ecologiste", "conservateur", "liberal", "humaniste", "patriote", "apolitique",
                              "sexe", "age", "diplome4", "statut_emploi", "csp", "region", "fume")
# 
# ols <- summary(lm(as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_big_regression, collapse=' + '))), data=ss))
# ols
# sum(ols$residuals^2)
# anova <- summary(aov(as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_big_regression, collapse=' + '))), data=ss))
# anova
# sum_sq <- anova[[1]]$'Sum Sq'
# explained_variance <- sum(sum_sq[-length(sum_sq)])
# print(paste("Share of explained variance: ", round(explained_variance/sum(sum_sq), 2), ". Among which, share of explained variance explained by each variable:", sep=""))
# for (i in 1:length(variables_big_regression)) print(paste(variables_big_regression[order(sum_sq[-length(sum_sq)], decreasing = T)][i], round(sort(sum_sq[-length(sum_sq)], decreasing = T)[i]/explained_variance,3)))
# summary(lm((taxe_approbation=='Oui') ~ ecologiste + conservateur, data=ss))
# 
# anovaVCA(as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_big_regression, collapse=' + '))), Data=ss)
# st <- s[,c("revenu", "Gauche_droite", "humaniste")]
# st$revenu <- n(st$revenu)
# st$humaniste <- factor(st$humaniste)
# anovaMM(revenu ~ Gauche_droite + humaniste, Data=st)
# data(dataEP05A2_2)
# dt <- dataEP05A2_2[-1,]
# st <- cbind(st[1:79,], dt)
# anovaMM(revenu ~ day, st)

# TODO: non linear ANOVA
variables_big_regression <- selected_variables 
nb_draws <- 100
sum_sq_all <- matrix(nrow = length(variables_big_regression), ncol = nb_draws, dimnames = list(variables_big_regression))
for (i in 1:nb_draws) {
  variables_i <- variables_big_regression[shuffle(length(variables_big_regression))]
  anova_i <- summary(aov(as.formula(paste("(taxe_approbation!='Non') ~ ", paste(variables_i, collapse=' + '))), data=s, weights = s$weight))
  sum_sq_i <- anova_i[[1]]$'Sum Sq'
  explained_error <- sum(sum_sq_i[-length(sum_sq_i)]) # independent of i
  for (v in variables_big_regression) sum_sq_all[v,i] <- sum_sq_i[which(variables_i==v)]/explained_error
}
sum_sq_average <- rowMeans(sum_sq_all)
sum_sq_sd <- apply(sum_sq_all, 1, sd)
explained_error/(var(s$taxe_approbation!='Non')*nrow(s)) # share of explained variance (0.08 with arbitrary pick, 0.31 with lasso)
sort(sum_sq_average, decreasing = T) # average share of explained variance explained by each variable
sum_sq_sd[order(sum_sq_average, decreasing = T)] # standard deviation of the share of explained variance explained by each variable
sort(sum_sq_average, decreasing = T)/sum_sq_sd[order(sum_sq_average, decreasing = T)] # "T-stats"
sum(sum_sq_average) # should = 1 (if it's not, probably because one variable is missing in anova_i; to find which one, run next line (I found ges_pm))
# for (v in variables_big_regression) if (!(v %in% broom::tidy(aov(as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_i, collapse=' + '))), data=s, weights = s$weight))$term )) print(v) 


##### Logit Approbation #####
# to weight correctly, use svyglm, cf. https://stats.stackexchange.com/questions/57107/use-of-weights-in-svyglm-vs-glm
# All variables we can think of (TODO: complete the list)
logit_all <- glm((taxe_approbation!='Non') ~ revenu + rev_tot + hausse_carburants + hausse_chauffage + score_climate_call + score_ges
                 + Gauche_droite + emission_cible + effets_CC + ecologiste + conservateur + liberal + humaniste + patriote + apolitique
                 + sexe + age + diplome4 + statut_emploi + csp + region, 
                 family = "binomial", data=ss)
summary(logit_all)
PseudoR2(logit_all)

# Only significant variables
summary(glm((taxe_approbation!='Non') ~ hausse_chauffage + score_climate_call + ecologiste + conservateur + humaniste + sexe , 
            binomial, data=ss))

# Only demographics
summary(glm((taxe_approbation!='Non') ~ revenu + rev_tot + sexe + age + region, 
            family = "binomial", data=ss)) # *: -age, +revenu, -rev_tot, +Homme, qq regions
summary(glm((taxe_approbation!='Non') ~ revenu + rev_tot + sexe + age + diplome4 + statut_emploi + csp + region, 
            family = "binomial", data=ss)) # *: +revenu, -rev_tot, +Homme, qq regions
summary(glm((taxe_approbation!='Non') ~ revenu + rev_tot + sexe + Age + Diplome + region, 
            family = "binomial", data=ss)) # *: +revenu, -rev_tot, +Homme, qq regions

# Demographics + politics
summary(glm((taxe_approbation!='Non') ~ revenu + rev_tot + sexe + age + diplome4 + statut_emploi + csp + region
            + Gauche_droite + ecologiste + conservateur + liberal + humaniste + patriote + apolitique, 
            family = "binomial", data=ss)) # *: apolitique, ecologiste, conservateur, +revenu, -rev_tot, +Homme, qq regions
summary(glm((taxe_approbation!='Non') ~ revenu + rev_tot + sexe + age + diplome4 + statut_emploi + csp + region
            + Gauche_droite + ecologiste + conservateur + liberal + humaniste + patriote + apolitique, 
            family = "binomial", data=ss)) # *: apolitique, ecologiste, conservateur, +revenu, -rev_tot, +Homme, qq regions


##### Probit Approbation #####
summary(glm(, family = binomial(link = "probit", data=ss)))


##### Elasticites #####
decrit(s$Elasticite_fuel, weights = s$weight) # -0.43
decrit(s$Elasticite_fuel_perso, weights = s$weight) #
decrit(s$Elasticite_chauffage, weights = s$weight) # -0.41
decrit(s$Elasticite_chauffage_perso, weights = s$weight) #
decrit(s$Elasticite_fuel_perso, weights = s$weight * s$depense_carburants) # -0.41
decrit(s$Elasticite_chauffage_perso, weights = s$weight * s$depense_chauffage) # -0.33

summary(lm(Elasticite_fuel ~ (taxe_efficace=='Oui'), data=s, weights = s$weight))
summary(lm(Elasticite_chauffage ~ (taxe_efficace=='Oui'), data=s, weights = s$weight)) # Aucun lien évident élasticité / efficacité environnementale

cor(s$Elasticite_fuel[!is.na(s$Elasticite_fuel)], s$Elasticite_fuel_perso[!is.na(s$Elasticite_fuel_perso)])
cor(s$Elasticite_chauffage[!is.na(s$Elasticite_chauffage)], s$Elasticite_chauffage_perso[!is.na(s$Elasticite_chauffage_perso)])
# Correlation positive entre �lasticit� perso et �lasticit� globale

# 71% (resp. 80%) think they are strictly more contrained than average for fuel (resp. chauffage)
# TODO: weight by their conso to see if it's consistent, exploit perte_relative for this issue
wtd.mean(s$Elasticite_fuel_perso > s$Elasticite_fuel, weights=s$weight, na.rm = T) # 71%
wtd.mean(s$Elasticite_chauffage_perso > s$Elasticite_chauffage, weights=s$weight, na.rm = T) # 80%
# Actually it's more 45% and 53% because categories were not directly comparable
s$Elasticite_fuel_max <- s$Elasticite_fuel + 0.05 * (s$Elasticite_fuel %in% c(-0.22, -0.05))
s$Elasticite_chauffage_max <- s$Elasticite_chauffage + 0.05 * (s$Elasticite_chauffage %in% c(-0.22, -0.05))
wtd.mean(s$Elasticite_fuel_perso > s$Elasticite_fuel_max, weights=s$weight, na.rm = T) # 45%
wtd.mean(s$Elasticite_chauffage_perso > s$Elasticite_chauffage_max, weights=s$weight, na.rm = T) # 53%

cor(s$km, s$elasticite_fuel_perso=='0% - Je suis contraint sur tous mes déplacements', use='complete.obs') # 0.22
cor(s$depense_carburants, s$Elasticite_fuel_perso-s$Elasticite_fuel_max, use='complete.obs') # -0.03
cor(s$depense_carburants, s$Elasticite_fuel_perso-s$Elasticite_fuel_max>0, use='complete.obs') # -0.02

summary(lm(Elasticite_fuel_perso ~ (taille_agglo == 'rural') + (taille_agglo == '-20k') + (taille_agglo == '20-100k') + (taille_agglo == '+100k'), data=s, subset=variante_partielle=='f', weights = s$weight))
summary(lm(Elasticite_fuel_perso ~ Revenu, data=s, subset=variante_partielle=='f', weights = s$weight))

# Link with taxe_efficace - Logit:
formula_c <- taxe_efficace!='Non' ~ Elasticite_chauffage
logit_elas_c <- glm(formula_c, family = binomial(link='logit'), data=ss)
summary(logit_elas_c)
logit_elas_c_margins <- logitmfx(formula_c, s, atmean=FALSE)$mfxest
logit_elas_c_margins

formula_f <- taxe_efficace!='Non' ~ Elasticite_fuel
logit_elas_f <- glm(formula_f, family = binomial(link='logit'), data=ss)
summary(logit_elas_f)
logit_elas_f_margins <- logitmfx(formula_f, s, atmean=FALSE)$mfxest
logit_elas_f_margins

formula_c_controls <- as.formula(paste("taxe_efficace!='Non' ~ Elasticite_chauffage + ", paste(variables_reg_elast, collapse=' + ')))
logit_elas_c_controls <- glm(formula_c_controls, family = binomial(link='logit'), data=ss)
summary(logit_elas_c_controls)
logit_elas_c_margins_controls <- logitmfx(formula_c_controls, s, atmean=FALSE)$mfxest
logit_elas_c_margins_controls

formula_f_controls <- as.formula(paste("taxe_efficace!='Non' ~ Elasticite_fuel + ", paste(variables_reg_elast, collapse=' + ')))
logit_elas_f_controls <- glm(formula_f_controls, family = binomial(link='logit'), data=ss)
summary(logit_elas_f_controls)
logit_elas_f_margins_controls <- logitmfx(formula_f_controls, s, atmean=FALSE)$mfxest
logit_elas_f_margins_controls

summary(lm(Elasticite_partielle ~ benefices_CC + benefices_circulation + benefices_sante + problemes_inefficace + problemes_alternatives + variante_partielle, data=s, weights=s$weight))
summary(lm(Elasticite_partielle ~ problemes_inefficace + problemes_alternatives, data=s, weights=s$weight))


##### Ciblage #####
decrit(s$categorie_cible, weights = s$weight)
decrit(s$taxe_cible_approbation, weights = s$weight)
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==20), data=s, subset=categorie_cible=='20_30', weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==30), data=s, subset=categorie_cible=='30_40', weights = s$weight)) # *
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==40), data=s, subset=categorie_cible=='40_50', weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==20), data=s, subset=(categorie_cible=='20_30' & taxe_cible_approbation!='NSP'), weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==30), data=s, subset=(categorie_cible=='30_40' & taxe_cible_approbation!='NSP'), weights = s$weight)) # *
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==40), data=s, subset=(categorie_cible=='40_50' & taxe_cible_approbation!='NSP'), weights = s$weight))

summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==20, weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==30, weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==40, weights = s$weight)) # *
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==50, weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==20 & taxe_cible_approbation!='NSP', weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==30 & taxe_cible_approbation!='NSP', weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==40 & taxe_cible_approbation!='NSP', weights = s$weight)) # *
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==50 & taxe_cible_approbation!='NSP', weights = s$weight))


##### Gaz de schiste #####
decrit(s$schiste_approbation, weights = s$weight)
decrit(s$schiste_approbation, miss=T, weights = s$weight)
decrit(s$schiste_traite, weights = s$weight)
decrit(s$schiste_avantage, weights = s$weight)
decrit(s$schiste_CC, weights = s$weight)
summary(lm((schiste_approbation=='Oui') ~ (schiste_traite==0), data=s, weights = s$weight)) # Encore rien de très significatif
summary(lm((schiste_avantage=='Cela permettrait de créer des emplois et dynamiser les départements concernés') ~ (schiste_traite==1), 
           data=s, subset=schiste_approbation=='Oui' & schiste_avantage !='Aucune de ces deux raisons', weights = s$weight))
summary(lm((as.character(schiste_CC)=='valable') ~ (schiste_traite==1), data=s, 
           subset=schiste_approbation=='Oui' & as.character(schiste_CC) !='NSP', weights = s$weight))
# Shale gas (cf. Appendix for regression)
decrit(s$schiste_approbation, miss=T, weights=s$weight) # 59% non, 16% oui
decrit(s$schiste_avantage, miss=T, weights=s$weight) # 56% aucun, 26% emplois, 18% CC
decrit(s$schiste_CC, miss=T, weights=s$weight) # 43% malvenue, 25% valable
barres(file="shale_val_nolegend", dataKN(c("schiste_approbation")), nsp=TRUE, legend=c("Yes", "No", "PNR"), labels = c(" "))
## Shale gas
reg_shale_1 <- lm((schiste_approbation!='Non') ~ (schiste_traite==1), data=s, weights = s$weight)
summary(reg_shale_1) # - 3.9 p.p. acceptance when treated
variables_reg_schiste <- c("Revenu", "score_ges", "score_climate_call", variables_demo) # 
variables_reg_schiste <- variables_reg_schiste[!(variables_reg_schiste %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
formula_schiste_approbation <- as.formula(paste("schiste_approbation!='Non' ~ (schiste_traite==1) + ",
                                                paste(variables_reg_schiste, collapse = ' + ')))
reg_shale_2 <- lm(formula_schiste_approbation, data=s, weights = s$weight)
summary(reg_shale_2) # - 5.1 p.p. acceptance when treated / Scores, Sex and Education matter
logit_shale_3 <- glm(formula_schiste_approbation, family = binomial(link='logit'), data=s)
summary(logit_shale_3)
logit_shale_3_margins <- logitmfx(formula_schiste_approbation, s, atmean=FALSE)$mfxest
logit_shale_3_margins # -5.7 p.p. with logit
summary(lm((schiste_approbation=='Oui') ~ (schiste_traite==1), data=s, weights = s$weight)) # Not significant for approval

decrit(s$schiste_approbation)
Table_shale_gas <- stargazer(reg_shale_1, reg_shale_2, logit_shale_3, 
                             title="Effect of being treated on acceptance of shale gas exploitation", model.names = TRUE, #star.cutoffs = c(0.1, 1e-5, 1e-30),
                             covariate.labels = c("Treated"), 
                             dep.var.labels = c("Shale gas exploitation: not ``No''"),# dep.var.caption = "", header = FALSE,
                             keep = c("schiste_traite"),
                             coef = list(NULL, NULL, logit_shale_3_margins[,1]), 
                             se = list(NULL, NULL, logit_shale_3_margins[,2]),
                             column.labels = c("(1)", "(2)", "(3)"), model.numbers = FALSE,
                             add.lines = list(c("Controls: Socio-demographics, scores", "", "\\checkmark", "\\checkmark")),
                             no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="table:shale_gas")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', 
                                                       '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_shale_gas, fixed=TRUE), fixed=TRUE), collapse=' ')


##### Engagement personnel ######
decrit(s$mode_vie_ecolo, weights = s$weight) # 79% !
decrit(s$mode_vie_ecolo, miss=T, weights = s$weight) # 65%
decrit(s$enfant_CC, weights = s$weight)
decrit(s$enfant_CC_pour_CC[s$enfant_CC=='Oui'], weights = s$weight[s$enfant_CC=='Oui'])
decrit(s$enfant_CC_pour_lui[s$enfant_CC=='Oui'], weights = s$weight[s$enfant_CC=='Oui'])
summary(lm((enfant_CC=='Oui') ~ sexe + age, data=s))
CrossTable(s$enfant_CC, s$age, prop.r = FALSE, prop.t = FALSE, prop.chisq = FALSE, )


##### Connaissances et opinions CC #####
decrit(s$ges_avion, weights = s$weight) # 59%
decrit(s$ges_boeuf, weights = s$weight) # 46%
decrit(s$ges_nucleaire, weights = s$weight) # 50%
decrit(s$score_climate_call, weights = s$weight) # médiane à 2/3
s$nb_coches_climate_call <- (s$ges_avion==T) + (s$ges_boeuf==T) + (s$ges_nucleaire==T)
decrit(s$nb_coches_climate_call, weights = s$weight) # médiane à 1/3
decrit(s$ges_CO2, weights = s$weight) # 77%
decrit(s$ges_CH4, weights = s$weight) # 48%
decrit(s$ges_O2, weights = s$weight) # 4%
decrit(s$ges_pm, weights = s$weight) # 61%
decrit(s$score_ges, weights = s$weight) # médiane à 3/4
s$nb_coches_ges <- (s$ges_CO2==T) + (s$ges_CH4==T) + (s$ges_O2==T) + (s$ges_pm==T)
decrit(s$nb_coches_ges, weights = s$weight) # médiane à 2/4
decrit(s$emission_cible, weights = s$weight) # 5
decrit(s$cause_CC, miss=T, weights = s$weight) # 72% anthropic, 20% natural, 3% doesn't exist
decrit(s$parle_CC, weights = s$weight) # 3 tiers
decrit(s$effets_CC, weights = s$weight) # 20% cataclysmiques; 31% désastreux, 38% graves
decrit(s$region_CC, weights = s$weight) # 65% autant, 29% Inde, 6% UE

# Graphs correct/false
ges_climate_call <- rev(paste("ges_correct", c("avion", "nucleaire", "boeuf", "O2", "CO2", "CH4", "pm"), sep="_")) # names(s)[which(grepl("ges_correct", names(s)))]
labels_ges_climate_call <- rev(c("Plane", "Nuclear", "Beaf", "Oxygen", "CO<sub>2</sub>", "Methane", "Particulates")) # labels_ges_climate_call <- c("Plane", "Beaf", "Nuclear", "CO<sub>2</sub>", "CH<sub>4</sub>", "O<sub>2</sub>", "PM") 
oui_non(margin_l=20, ges_climate_call, NSP=FALSE, en=c("Correct", "Wrong"), labels = labels_ges_climate_call, sort=FALSE)
oui_non(ges_climate_call, NSP=FALSE, en=c("Correct", "Faux"), labels = rev(c("Avion", "Nucléaire", "Bœuf", "Oxygène", "CO<sub>2</sub>", "Méthane", "Particules fines")), sort=FALSE)

barres(file="CC_responsable", title="", data=data1(names(s)[which(grepl("responsable_CC", names(s)))]), sort=T, showLegend=FALSE, labels=c("Chacun d'entre nous", "Les gouvernements", "Certains pays étrangers", "Les plus riches", "Des causes naturelles", "Les générations passées"), hover=labels_responsable)
barres(file="CC_responsible", title="", data=data1(names(s)[which(grepl("responsable_CC", names(s)))]), sort=T, showLegend=FALSE, labels=c("Each one of us", "Governments", "Certain foreign countries", "The richest", "Natural causes", "Past generations"), hover=labels_responsable)
barres(file="CC_effects", title="", thin=T, data=dataN("effets_CC"), nsp=T, sort=T, legend = c("Insignificant", "Small", "Serious", "Disastrous", "Cataclysmic", "NSP"), labels=c("Consequences of CC"))
barres(file="CC_effets", title="", data=dataN("effets_CC"), nsp=T, sort=T, legend = dataN("effets_CC", return="levels"), labels=c("Conséquences du CC"))
barres(file="CC_cause", title="", data=dataN("cause_CC"), nsp=T, sort=T, legend = c("Anthropic", "Natural", "Does not exist", "NSP"), labels=c("Cause of CC"))
barres(file="CC_cause_fr", title="", data=dataN("cause_CC"), nsp=T, sort=T, legend = dataN("cause_CC", return="levels"), labels=c("Cause du CC"))
s$region_CC <- as.factor(s$region_CC)
s$region_CC <- relevel(relevel(s$region_CC, "Autant dans les deux"), "L'Inde")
barres(file="CC_region", title="", data=dataN("region_CC", miss=FALSE), nsp=FALSE, sort=T, legend = c("India", "As much in both", "European Union", "NSP"), labels=c("Region with biggest consequences of CC"))
barres(file="CC_région", title="", data=dataN("region_CC", miss=FALSE), nsp=FALSE, sort=T, legend = dataN("region_CC", return="levels"), labels=c("Région aux plus grandes conséquences du CC"))
barres(file="CC_target_emission", title="", data=dataN("emission_cible", miss=FALSE), nsp=FALSE, sort=T, color = rev(brewer.pal(11, "RdBu")), legend = dataN("emission_cible", return="levels"), labels=c("Emission compatible with +2°C (tCO<sub>2</sub>e/yr p.c.)")) 
barres(file="CC_émission_cible", title="", data=dataN("emission_cible", miss=FALSE), nsp=FALSE, sort=T, color = rev(brewer.pal(11, "RdBu")), legend = dataN("emission_cible", return="levels"), labels=c("Émission compatible avec +2°C (tCO<sub>2</sub>e/an p.c.)")) 
s$parle_CC <- as.factor(s$parle_CC)
s$parle_CC <- relevel(relevel(s$parle_CC, "Plusieurs fois par an"), "Plusieurs fois par mois")
barres(file="CC_talks", title="", data=dataN("parle_CC"), nsp=T, sort=T, legend = c("Several times per month", "Several times per year", "Almost never", "PNR"), labels=c("Talks about CC...")) 
barres(file="CC_parle", title="", data=dataN("parle_CC"), nsp=T, sort=T, legend = dataN("parle_CC", return="levels"), labels=c("Parle du CC...")) 
barres(file="CC_generation_min", title="", rev_color = T, data=dataN("generation_CC_min"), nsp=T, sort=T, legend = c(dataN("generation_CC_min", return="levels")[1:4], "PNR"), labels=c("First generation of French severely affected by CC (born in...)"))
barres(file="CC_génération_min", title="", rev_color = T, data=dataN("generation_CC_min"), nsp=T, sort=T, legend = c(dataN("generation_CC_min", return="levels")[1:4], "PNR"), labels=c("Première génération de Français <br>gravement affectée par le CC (née en...)"))


##### Orientation politiques #####
decrit(s$gauche_droite, weights = s$weight, miss=T)
decrit(s$gauche_droite)
decrit(s$apolitique, weights = s$weight) # 21%
decrit(s$ecologiste, weights = s$weight) # 15%
decrit(s$humaniste, weights = s$weight) # 11%
decrit(s$patriote, weights = s$weight) # 8%
decrit(s$liberal, weights = s$weight) # 5%
decrit(s$conservateur, weights = s$weight) # 2%


##### Transferts inter #####
summary(lm((transferts_inter=='Oui') ~ aide_2p, data = s, weights = s$weight)) # -.16 !! aide de 2% moins approuvée que de 5%: sûrement parce qu'à partir de la moitié la formulation à changer de "transferts des pays riches" à "aide de la France", ptet aussi faire attention que les socio-démos sont un peu différents dans le 3è tiers de l'échantillon...
summary(lm((transferts_inter=='Oui') ~ aide_2p*transferts_inter_info*apres_modifs, data = s, weights = s$weight))
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info*apres_modifs, data = s, weights = s$weight)) # 0 !
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info*apres_modifs, data = s, subset = transferts_inter!='NSP', weights = s$weight)) # 0 !
decrit(s$variation_aide, weights = s$weight)
load('p_data.RData')
t <- merge(s, t_transferts_inter_a, all=T) # /!\ Problème avec cette analyse: transferts_inter inclut des réponses où la question est 2% et non 5% du PIB (aide_2p) 
t$transferts_inter[!is.na(t$taille_foyer)] <- t$transferts_inter_a[!is.na(t$taille_foyer)] 
decrit(t$transferts_inter, weights = t$weight)
decrit(t$transferts_inter, weights = t$weight, miss=T)
decrit(t$transferts_inter[t$apres_modifs==T], weights = t$weight[t$apres_modifs==T], miss=T)
decrit(t$transferts_inter[t$apres_modifs==FALSE], weights = t$weight[t$apres_modifs==FALSE], miss=T)
decrit(t$transferts_inter[t$transferts_inter_info==T], weights = t$weight[t$transferts_inter_info==T], miss=T)
decrit(t$transferts_inter[t$transferts_inter_info==FALSE], weights = t$weight[t$transferts_inter_info==FALSE], miss=T)
decrit(t$transferts_inter[t$apres_modifs==T & t$transferts_inter_info==T], weights = t$weight[t$apres_modifs==T & t$transferts_inter_info==T], miss=T)
decrit(t$transferts_inter[t$apres_modifs==T & t$transferts_inter_info==FALSE], weights = t$weight[t$apres_modifs==T & t$transferts_inter_info==FALSE], miss=T)
decrit(t$transferts_inter[t$apres_modifs==FALSE & t$transferts_inter_info==T], weights = t$weight[t$apres_modifs==FALSE & t$transferts_inter_info==T], miss=T)
decrit(t$transferts_inter[t$apres_modifs==FALSE & t$transferts_inter_info==FALSE], weights = t$weight[t$apres_modifs==FALSE & t$transferts_inter_info==FALSE], miss=T)
decrit(t$transferts_inter, miss=T)
binconf(sum(t$weight[!is.na(t$transferts_inter) & t$transferts_inter=='Oui']), sum(t$weight[!is.missing(t$transferts_inter)]))
binconf(sum(t$weight[!is.na(t$transferts_inter) & t$transferts_inter=='Oui']), sum(t$weight[!is.na(t$transferts_inter)]))
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info*apres_modifs, data = t)) # 0
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info*apres_modifs, data = t, subset = transferts_inter!='NSP')) # 0
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info*apres_modifs, data = t, weights = t$weight)) # 0
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info*apres_modifs, data = t, subset = transferts_inter!='NSP', weights = t$weight)) # 0

# use m_global (in enquete/codes) to redo graph with new data for transferts_inter
decrit(s$aide_2p, miss = T, weights = s$weight) # Dernier tiers de l'échantillon: 2% du PIB au lieu de 5, et on demande pourquoi s'ils répondent pas Oui
decrit(s$transferts_inter[s$aide_2p==T], miss = T, weights = s$weight[s$aide_2p==T]) # Désapprobation d'aideà 2%: 51%
decrit(s$transferts_inter[s$aide_2p==F], miss = T, weights = s$weight[s$aide_2p==F]) # Désapprobation d'aideà 5%: 34%
decrit(s$aide_non_etats, weights = s$weight)
decrit(s$aide_non_priorite, weights = s$weight)
decrit(s$aide_non_global, weights = s$weight)
decrit(s$aide_non_trop, weights = s$weight)
decrit(s$aide_non_autonomie, weights = s$weight)


##### Dépenses publiques #####
categories_depenses <- c("sante", "education", "retraites", "securite", "recherche", "justice", "armee", "protection", "infrastructures", "loisirs", "aide")
i <- 0
for (v in categories_depenses) {
  print(summary(lm(s[[paste('variation', v, sep='_')]] ~ s[[paste('dep', i, 'en_position', sep='_')]], weights=s$weight)))
  i <- i+1 }
# *** pour justice, loisirs, éducation
t_depenses$aleatoire <- FALSE
s$aleatoire <- TRUE
d <- merge(s, t_depenses, all=T)
for (v in categories_depenses) {  print(summary(lm(d[[paste('variation', v, sep='_')]] ~ d$aleatoire))) } # * -: armee, securite, aide, 
summary(lm(d$variation_aide ~ d$aleatoire, weights = d$weight))
summary(rq(d$variation_aide ~ d$aleatoire, weights = d$weight))
decrit(d$variation_aide, weights = d$weight)
decrit(s$variation_aide)
decrit(s$depenses_confiant)
decrit(s$depenses_confiant[is.na(s$variation_aide)])
decrit(t_depenses$variation_aide)
for (v in categories_depenses) { # why not use tidyverse's gather?
  temp <- d[, c(paste('variation', v, sep='_'), 'weight')]
  temp$categorie <- v
  temp$variation <- temp[[paste('variation', v, sep='_')]]
  if (exists('dep')) dep <- merge(dep, temp, all=T)
  else dep <- temp
}
dep$categorie <- relevel(as.factor(dep$categorie), "infrastructures")
summary(lm(variation ~ categorie, data=dep)) # answers are not random, i.e. average depends significantly on category


##### Rattrapage diesel #####
decrit(s$rattrapage_diesel, miss = T, weights = s$weight)
decrit(s$rattrapage_diesel[s$diesel==T], miss = T, weights = s$weight[s$diesel==T])
decrit(s$rattrapage_diesel[s$diesel!=T], miss = T, weights = s$weight[s$diesel!=T])
decrit(s$rattrapage_diesel[s$essence==T], miss = T, weights = s$weight[s$essence==T])
decrit(s$rattrapage_diesel[s$diesel!=T & s$essence!=T], miss = T, weights = s$weight[s$diesel!=T & s$essence!=T])
decrit(s$hausse_diesel, weights = s$weight) 
summary(lm((rattrapage_diesel!='Non') ~ diesel + essence, data=s, weights = s$weight))
for (j in names(s)) if (grepl('gilets_jaunes', j)) print(decrit(s[[j]], weights=s$weight))
oui_non(c("rattrapage_diesel"), NSP=TRUE, en=TRUE, labels = rev(c("Favorable to catch-up diesel taxes")), sort=FALSE)
barres(file="diesel_catch_up_val", dataKN(c("rattrapage_diesel")), nsp=TRUE, legend=c("Yes", "No", "PNR"), color=, labels = c("Favorable to catch-up diesel taxes"))


##### Approbation politiques environnementales #####
for (j in names(s)) if (grepl('si_', j)) print(decrit(s[[j]], weights=s$weight))
for (j in 102:109) print(decrit(s[[j]], weights=s$weight))
labels_politiques_env <- c()
for (j in 102:109) labels_politiques_env <- c(labels_politiques_env, gsub(" - Q74", "", gsub(".*) à ", "", Label(s[[j]]))))  
labels_taxe_condition <- c()
for (j in 92:100) labels_taxe_condition <- c(labels_taxe_condition, gsub(" - .*", "", gsub(".*: ", "", Label(s[[j]])))) 
labels_taxe_condition[8] <- "des énergies renouvelables<br> (éoliennes, solaire, etc.)"
labels_taxe_condition[1] <- "un versement pour les 50% de Français les plus<br> modestes (ceux gagnant moins de 1670€/mois)"
labels_taxe_condition[3] <- "une compensation pour les ménages contraints<br> dans leur consommation de produits pétroliers"
labels_environmental_policies <- c("a tax on kerosene (aviation)", "a tax on red meat", "stricter insulation standards for new buildings", "stricter standards on pollution from new vehicles", "stricter standards on pollution during roadworthiness tests", "the prohibition of polluting vehicles in city centres", "the introduction of urban tolls", "a contribution to a global climate fund")
labels_tax_condition <- c("a payment for the 50% poorest French people<br> (those earning less than 1670€/month)", "a payment to all French people", "compensation for households forced to consume petroleum products", "a reduction in social contributions", "a VAT cut", "a reduction in the public deficit", "the thermal renovation of buildings", "renewable energies (wind, solar, etc.)", "clean transport")
labels_tax_condition[3] <- "compensation for households constrained<br> to consume petroleum products"
labels_politiques_env[3] <- "des normes plus strictes <br>sur l'isolation pour les nouveaux bâtiments"
labels_politiques_env[4] <- "des normes plus strictes <br>sur la pollution des nouveaux véhicules"
labels_politiques_env[5] <- "des normes plus strictes <br>sur la pollution lors du contrôle technique"
labels_politiques_env[8] <- "une contribution pour <br>un fond mondial pour le climat"
labels_politiques_env[6] <- "l'interdiction des véhicules polluants <br>dans les centre-villes"
barres(file="politiques_environnementales", title="<b>Seriez-vous favorable aux politiques environnementales suivantes ?</b>", 
       data=data5(names(s)[141:148], miss=FALSE), nsp=FALSE, sort=T, color=(color5), legend = c(oui_non5), labels=labels_politiques_env)
barres(file="environmental_policies", title="<b>Would you agree with the following environnemental policies?</b>", 
       data=data5(names(s)[141:148], miss=FALSE), nsp=FALSE, sort=T, color=color5, legend = c(yes_no5), labels=labels_environmental_policies)
barres(file="taxe_condition", title="Dans quels cas seriez-vous favorable à l'augmentation de la taxe carbone ?<br><b>Je serais favorable si les recettes de la taxe étaient utilisées pour financer ...</b>", 
       data=data5(names(s)[131:139], miss=FALSE), nsp=FALSE, margin_l = 270, sort=T, color=color5, legend = c(oui_non5), labels=labels_taxe_condition)
barres(file="tax_condition", title="In what cases would you be in favour of increasing the carbon tax? <br><b>I would be in favour if the tax revenues were used to finance...</b>", 
       data=data5(names(s)[131:139], miss=FALSE), nsp=FALSE, margin_l = 205, sort=T, color=color5, legend = c(yes_no5), labels=labels_tax_condition)


##### Bénéfices/problèmes taxe carbone #####
variables_benefices <- names(s)[which(grepl("benefice", names(s)))[which(grepl("benefice", names(s)))>300]]
variables_problemes <- names(s)[which(grepl("problemes", names(s)))[which(grepl("problemes", names(s)))>300]]
labels_benefices <- c()
values_benefices <- c()
for (v in variables_benefices[2:(length(variables_benefices)-2)]) {
  labels_benefices <- c(labels_benefices, gsub(" - .*", "", gsub(".*: ", "", Label(s[[v]]))))
  values_benefices <- c(values_benefices, sum(s$weight[which(s[[v]]==T)])/sum(s$weight)) }
labels_problemes <- c()
values_problemes <- c()
for (v in variables_problemes[2:(length(variables_problemes)-2)]) {
  labels_problemes <- c(labels_problemes, gsub(" - .*", "", gsub(".*: ", "", Label(s[[v]]))))
  values_problemes <- c(values_problemes, sum(s$weight[which(s[[v]]==T)])/sum(s$weight)) }

# oui_non(margin_l=430, variables_benefices[1:(length(variables_benefices)-2)], "barres_benefices", labels_benefices)
# oui_non(margin_l=430, variables_problemes[1:(length(variables_problemes)-2)], "barres_problemes", labels_problemes)

# TODO: utiliser nb_bénéfices / problèmes cochés, voire l'influence des traitements

barres(file="benefices", title="<b>Bénéfices d'une taxe carbone compensée</b><br>(choix multiples)", data=matrix(values_benefices, ncol=length(values_benefices)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_benefices, hover=labels_benefices, legend="empty")
barres(file="problemes", title="<b>Problèmes d'une taxe carbone compensée</b><br>(choix multiples)", data=matrix(values_problemes, ncol=length(values_problemes)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_problemes, hover=labels_problemes, legend="empty")
# orca(barres_problemes, "../images/problemes.png")
# orca(barres_benefices, "../images/benefices.png")

# plot(1:3,1:3)
# dev.copy(png, filename="../images/test.png") # plot from R (not from plotly)
# dev.off()
labels_benefices <- c("Fights CC", "Reduces negative impact of pollution on health", "Reduces congestion", "Increases my purchasing power", "Increases purchasing power of the poorest",
                      "Increases France's independence toward fossils", "Prepares the economy for tomorrow", "None of these reasons", "Other reasons")
barres(file="CC_benefits", title="", data=data1(variables_benefices), sort=T, showLegend=FALSE, labels=labels_benefices, hover=labels_benefices)

variables_benefits <- names(s)[which(grepl("benefice", names(s)))[which(grepl("problemes", names(s)))>300]]
variables_benefits <- variables_benefits[!(variables_benefits %in% c("nb_benefices", "benefices_autre"))]
labels_benefits <- c("Lutte contre le changement climatique", "Améliore la qualité de l'air et la santé", "Réduit les embouteillages", "Augmente mon pouvoir d'achat", 
                     "Augmente le pouvoir d'achat des plus modestes",
                     "Réduit la dépendance aux importations de pétrole", "Prépare l'économie aux technologies de demain", "Pour aucune de ces raisons", "Autres raisons")
barres(file="CC_benefits_synchro", title="", data=data1(variables_benefits), sort=T, showLegend=FALSE, labels=labels_benefits, hover=labels_benefits, xrange=c(0, 0.47), margin_l=280) # pb 35% NSP
variables_problems <- names(s)[which(grepl("problemes", names(s)))]
variables_problems <- variables_problems[!(variables_problems %in% c("nb_problemes", "problemes_autre"))]
labels_problems <- c("Est inefficace pour réduire la pollution", "Les alternatives sont insuffisantes ou trop chères", "Pénalise les milieux ruraux", "Réduit mon pouvoir d'achat",
                     "Réduit le pouvoir d'achat de ménages modestes", "Nuit à l'économie et à l'emploi", "Est un prétexte pour augmenter les impôts", "Pour aucune de ces raisons", "Autres raisons")
barres(file="CC_problems_synchro", title="", data=data1(variables_problems), sort=T, showLegend=FALSE, labels=labels_problems, hover=labels_problems, xrange=c(0, 0.47), margin_l=280)


##### Perdants/Gagnants #####
labels_gagnant_perdant <- c()
values_gagnant <- c()
for (v in variables_taxe_gagnant) {
  labels_gagnant_perdant <- c(labels_gagnant_perdant, gsub(" - .*", "", gsub(".*: ", "", Label(s[[v]]))))
  values_gagnant <- c(values_gagnant, sum(s$weight[which(s[[v]]==T)])/sum(s$weight)) }
values_perdant <- c()
for (v in variables_taxe_perdant) {
  values_perdant <- c(values_perdant, sum(s$weight[which(s[[v]]==T)])/sum(s$weight)) }
labels_gagnant_perdant[7] <- "Certains Français, mais <br>pas une catégorie de revenus particulière"
labels_gagnant <- labels_perdant <- labels_gagnant_perdant
labels_perdant[6] <- "Les ruraux"

barres_gagnants <- barres(file="gagnants", title="<b>Selon vous, quels seraient les gagnants avec une taxe carbone compensée ?</b><br>(choix multiples)", margin_l = 200, data=matrix(values_gagnant, ncol=length(values_gagnant)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_gagnant, hover=labels_gagnant, legend="empty")
barres_perdants <- barres(file="perdants", title="<b>Selon vous, quels seraient les perdants avec une taxe carbone compensée ?</b><br>(choix multiples)", margin_l = 200, data=matrix(values_perdant, ncol=length(values_perdant)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_perdant, hover=labels_perdant, legend="empty")
barres_gagnants
barres_perdants

for (v in c(variables_taxe_gagnant, variables_taxe_perdant)) {
  reg_v <- summary(glm(as.formula(paste(v, "==T ~ variante_monetaire")), data=s, weights=s$weight))
  if (reg_v$coefficients[2,4]<0.05) print(paste(v, ": coef: ", reg_v$coefficients[2], "; p-value: ", reg_v$coefficients[2,4]))
}
test <- glm(as.formula(paste(variables_taxe_gagnant[1], "==T ~ variante_monetaire")), data=s, weights=s$weight)
test <- summary(glm(as.formula(paste(variables_taxe_gagnant[1], "==T ~ variante_monetaire")), data=s, weights=s$weight))
length(c(variables_taxe_gagnant, variables_taxe_perdant))

variables_winners <- names(s)[which(grepl("taxe_gagnant_", names(s)))]
labels_winners <- c("Personne", "Les plus pauvres", "Les classes moyennes", "Les plus riches", "Tous les Français", "Les citadins", "Certains Français, mais pas une <br>catégorie de revenus particulière", "NSP <br>(Ne sais pas, ne se prononce pas)")
barres(file="tax_winners_synchro", title="", data=data1(variables_winners), sort=T, showLegend=FALSE, labels=labels_winners, hover=labels_winners, xrange=c(0, 0.58), margin_l=200)
variables_losers <- names(s)[which(grepl("taxe_perdant_", names(s)))]
labels_losers <- c("Personne", "Les plus pauvres", "Les classes moyennes", "Les plus riches", "Tous les Français", "Les ruraux ou péri-urbains", "Certains Français, mais pas une <br>catégorie de revenus particulière", "NSP <br>(Ne sais pas, ne se prononce pas)")
barres(file="tax_losers_synchro", title="", data=data1(variables_losers), sort=T, showLegend=FALSE, labels=labels_losers, hover=labels_losers, xrange=c(0, 0.58), margin_l=200)


##### Gilets jaunes #####
decrit(s$gilets_jaunes_dedans, weights = s$weight)
decrit(s$gilets_jaunes_soutien, weights = s$weight)
decrit(s$gilets_jaunes_compris, weights = s$weight)
decrit(s$gilets_jaunes_oppose, weights = s$weight)
decrit(s$gilets_jaunes_NSP, weights = s$weight)
decrit((s$gilets_jaunes_oppose==T & (s$gilets_jaunes_dedans==T | s$gilets_jaunes_soutien==T)))

values_benefices_GJ_approuve <- c()
values_benefices_GJ_oppose <- c()
for (v in variables_benefices[1:(length(variables_benefices)-2)]) {
  values_benefices_GJ_approuve <- c(values_benefices_GJ_approuve, sum(s$weight[which(s[[v]]==T & (s$gilets_jaunes_dedans==T | s$gilets_jaunes_compris==T))])/sum(s$weight[s$gilets_jaunes_dedans==T | s$gilets_jaunes_compris==T]))
  values_benefices_GJ_oppose <- c(values_benefices_GJ_oppose, sum(s$weight[which(s[[v]]==T & s$gilets_jaunes_oppose==T)])/sum(s$weight[s$gilets_jaunes_oppose==T])) }
values_problemes_GJ_approuve <- c()
values_problemes_GJ_oppose <- c()
for (v in variables_problemes[1:(length(variables_problemes)-2)]) {
  values_problemes_GJ_approuve <- c(values_problemes_GJ_approuve, sum(s$weight[which(s[[v]]==T & (s$gilets_jaunes_dedans==T | s$gilets_jaunes_compris==T))])/sum(s$weight[s$gilets_jaunes_dedans==T | s$gilets_jaunes_compris==T]))
  values_problemes_GJ_oppose <- c(values_problemes_GJ_oppose, sum(s$weight[which(s[[v]]==T & s$gilets_jaunes_oppose==T)])/sum(s$weight[s$gilets_jaunes_oppose==T])) }

decrit((s$gilets_jaunes_dedans==T | s$gilets_jaunes_soutien==T), weights = s$weight)
decrit((s$gilets_jaunes_oppose==T), weights = s$weight)
barres(file="politiques_environnementales_GJ_approuve", title="Seriez-vous favorable aux politiques environnementales suivantes ?<br><b>Réponses parmi les 36% de soutiens aux gilets jaunes</b>", 
       data=data5(names(s)[141:148], miss=FALSE, data=s[s$gilets_jaunes_dedans==T | s$gilets_jaunes_compris==T,]), nsp=FALSE, sort=T, color=(color5), legend = c(oui_non5), labels=labels_politiques_env)
barres(file="politiques_environnementales_GJ_oppose", title="Seriez-vous favorable aux politiques environnementales suivantes ?<br><b>Réponses parmi les 25% d'opposants aux gilets jaunes</b>", 
       data=data5(names(s)[141:148], miss=FALSE, data=s[s$gilets_jaunes_oppose==T,]), nsp=FALSE, sort=T, color=(color5), legend = c(oui_non5), labels=labels_politiques_env)
barres(file="taxe_condition_GJ_approuve", title="Je serais favorable à la hausse de la taxe carbone si les recettes étaient utilisées pour financer ...<br><b>Réponses parmi les 36% de soutiens aux gilets jaunes</b>", 
       data=data5(names(s)[131:139], miss=FALSE, data=s[s$gilets_jaunes_dedans==T | s$gilets_jaunes_compris==T,]), nsp=FALSE, margin_l = 270, sort=T, color=color5, legend = c(oui_non5), labels=labels_taxe_condition)
barres(file="taxe_condition_GJ_oppose", title="Je serais favorable à la hausse de la taxe carbone si les recettes étaient utilisées pour financer ...<br><b>Réponses parmi les 25% d'opposants aux gilets jaunes</b>", 
       data=data5(names(s)[131:139], miss=FALSE, data=s[s$gilets_jaunes_oppose==T,]), nsp=FALSE, margin_l = 270, sort=T, color=color5, legend = c(oui_non5), labels=labels_taxe_condition)

barres_benefices_GJ_approuve <- barres(file="benefices_GJ_approuve", title="<b>Bénéfices d'une taxe carbone compensée</b><br>(choix multiples)", data=matrix(values_benefices_GJ_approuve, ncol=length(values_benefices)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_benefices, hover=labels_benefices, legend="empty")
barres_problemes_GJ_approuve <- barres(file="problemes_GJ_approuve", title="<b>Problèmes d'une taxe carbone compensée</b><br>(choix multiples)", data=matrix(values_problemes_GJ_approuve, ncol=length(values_problemes)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_problemes, hover=labels_problemes, legend="empty")
barres_benefices_GJ_oppose <- barres(file="benefices_GJ_oppose", title="<b>Bénéfices d'une taxe carbone compensée</b><br>(choix multiples)", data=matrix(values_benefices_GJ_oppose, ncol=length(values_benefices)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_benefices, hover=labels_benefices, legend="empty")
barres_problemes_GJ_oppose <- barres(file="problemes_GJ_oppose", title="<b>Problèmes d'une taxe carbone compensée</b><br>(choix multiples)", data=matrix(values_problemes_GJ_oppose, ncol=length(values_problemes)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_problemes, hover=labels_problemes, legend="empty")
barres_benefices_GJ_approuve
barres_problemes_GJ_approuve
barres_benefices_GJ_oppose
barres_problemes_GJ_oppose

values_gagnant_GJ_approuve <- c()
values_gagnant_GJ_oppose <- c()
for (v in variables_taxe_gagnant) {
  values_gagnant_GJ_approuve <- c(values_gagnant_GJ_approuve, sum(s$weight[which(s[[v]]==T & (s$gilets_jaunes_dedans==T | s$gilets_jaunes_compris==T))])/sum(s$weight[s$gilets_jaunes_dedans==T | s$gilets_jaunes_compris==T]))
  values_gagnant_GJ_oppose <- c(values_gagnant_GJ_oppose, sum(s$weight[which(s[[v]]==T & s$gilets_jaunes_oppose==T)])/sum(s$weight[s$gilets_jaunes_oppose==T])) }
values_perdant_GJ_approuve <- c()
values_perdant_GJ_oppose <- c()
for (v in variables_taxe_perdant) {
  values_perdant_GJ_approuve <- c(values_perdant_GJ_approuve, sum(s$weight[which(s[[v]]==T & (s$gilets_jaunes_dedans==T | s$gilets_jaunes_compris==T))])/sum(s$weight[s$gilets_jaunes_dedans==T | s$gilets_jaunes_compris==T])) 
  values_perdant_GJ_oppose <- c(values_perdant_GJ_oppose, sum(s$weight[which(s[[v]]==T & s$gilets_jaunes_oppose==T)])/sum(s$weight[s$gilets_jaunes_oppose==T])) }

barres_gagnants_GJ_approuve <- barres(file="gagnants_GJ_approuve", title="Selon vous, quels seraient <b>les gagnants</b> avec une taxe carbone compensée ?<br><b>Réponses parmi les 36% de soutiens aux gilets jaunes</b>", margin_l = 200, data=matrix(values_gagnant_GJ_approuve, ncol=length(values_gagnant)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_gagnant, hover=labels_gagnant, legend="empty")
barres_gagnants_GJ_oppose <- barres(file="gagnants_GJ_oppose", title="Selon vous, quels seraient <b>les gagnants</b> avec une taxe carbone compensée ?<br><b>Réponses parmi les 25% d'opposants aux gilets jaunes</b>", margin_l = 200, data=matrix(values_gagnant_GJ_oppose, ncol=length(values_gagnant)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_gagnant, hover=labels_gagnant, legend="empty")
barres_perdants_GJ_approuve <- barres(file="perdants_GJ_approuve", title="Selon vous, quels seraient <b>les perdants</b> avec une taxe carbone compensée ?<br><b>Réponses parmi les 36% de soutiens aux gilets jaunes</b>", margin_l = 200, data=matrix(values_perdant_GJ_approuve, ncol=length(values_perdant)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_perdant, hover=labels_perdant, legend="empty")
barres_perdants_GJ_oppose <- barres(file="perdants_GJ_oppose", title="Selon vous, quels seraient <b>les perdants</b> avec une taxe carbone compensée ?<br><b>Réponses parmi les 25% d'opposants aux gilets jaunes</b>", margin_l = 200, data=matrix(values_perdant_GJ_oppose, ncol=length(values_perdant)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_perdant, hover=labels_perdant, legend="empty")
barres_gagnants_GJ_approuve
barres_gagnants_GJ_oppose
barres_perdants_GJ_approuve
barres_perdants_GJ_oppose
# TODO: combiner les deux graphes opposants/soutiens, pourcentages


# Socio demo gilets jaunes
table_taille_agglo_GJ <- round(Crosstab(s[s$gilets_jaunes!='NSP',], row.vars="taille_agglo", col.vars="gilets_jaunes", type="j", dec.places = 0)$Crosstab)
table_taille_agglo_GJ[1:5, ] <- round(Crosstab(s[s$gilets_jaunes!='NSP',], row.vars="taille_agglo", col.vars="gilets_jaunes", type="r", dec.places = 0)$Crosstab)
table_taille_agglo_GJ
xtable(table_taille_agglo_GJ, digits=0)

table_categorie_cible_GJ <- round(Crosstab(s[s$gilets_jaunes!='NSP',], row.vars="revenu_decile", col.vars="gilets_jaunes", type="j", dec.places = 0)$Crosstab)
table_categorie_cible_GJ[1:10, ] <- round(Crosstab(s[s$gilets_jaunes!='NSP',], row.vars="revenu_decile", col.vars="gilets_jaunes", type="r", dec.places = 0)$Crosstab)
table_categorie_cible_GJ
xtable(table_categorie_cible_GJ, digits=0)


##### Régressions: 2.1 OLS gagnant_cible_categorie ######
# cible_approbation_by_group <- as.data.table(s)[, .(mean=wtd.mean(taxe_cible_approbation!='Non', weight)), by=list(gagnant_cible_categorie!='Perdant', taxe_approbation!='Non')]
# cible_approbation_by_group
# xtable(cible_approbation_by_group)
# # print(xtable(cible_approbation_by_group), file="cible_approbation_by_group.tex")
# with(s, tapply(X=taxe_cible_approbation!='Non', INDEX=list(gagnant_cible_categorie!='Perdant', taxe_approbation!='Non'), FUN=mean)) # cannot put weights
# xtabs((taxe_cible_approbation!='Non') ~ (gagnant_cible_categorie!='Perdant') + (taxe_approbation!='Non'), s)/xtabs(~ (gagnant_cible_categorie!='Perdant') + (taxe_approbation!='Non'), s) # cannot put weights
# s %>%  group_by((gagnant_cible_categorie!='Perdant'), tax_acceptance) %>%  summarise(mean = mean(taxe_cible_approbation!='Non')) %>%  spread(tax_acceptance, mean, sep = '') # cannot put weights
# cf. aussi crosstab, e.g. crosstab( s$gagnant_categorie[s$simule_gagnant==0], s$gagnant_feedback_categorie[s$simule_gagnant==0],s$weight[s$simule_gagnant==0], dnn=c('Winning category, Before', 'Winning category, After'), prop.r=T, dir=c("h", "v")) # , inv.x=T, inv.y=T
dcast(s, win_cible_category ~ tax_acceptance, value.var="tax_cible_acceptance", fun.aggregate = mean, margins=T)

# 45%*** more chance of acceptance when thinking to not loose
summary(lm((taxe_cible_approbation!='Non') ~ (gagnant_cible_categorie!='Perdant')*(taxe_approbation!='Non'), data=s, weights = s$weight))
# same thing among < 70_
summary(lm((taxe_cible_approbation!='Non') ~ (gagnant_cible_categorie!='Perdant')*(taxe_approbation!='Non'), data=s, subset=categorie_cible!='70_', weights = s$weight))
# 52%*** less chance of approval when thinking to not win, among < 70_
summary(lm((taxe_cible_approbation=='Oui') ~ (gagnant_cible_categorie=='Gagnant')*(taxe_approbation=='Oui'), data=s, subset=categorie_cible!='70_', weights = s$weight))
cor(s$gagnant_cible_categorie!='Perdant', s$traite_cible==1) # 21%
cor(s$gagnant_cible_categorie!='Perdant', s$traite_cible_conjoint==1) # 10%
cor(s$gagnant_cible_categorie=='Gagnant', s$traite_cible==1) # 23%
cor(s$gagnant_cible_categorie=='Gagnant', s$traite_cible_conjoint==1) # 10%


##### Régressions: 2.2 traite_cible ######
# 20%*** (conjoint 10%) for 20_30
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible * traite_cible_conjoint, subset = categorie_cible == '20_30', data=s, weights = s$weight))
# 15%* (15%*) for 30_40
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible * traite_cible_conjoint, subset = categorie_cible == '30_40', data=s, weights = s$weight))
# 12% (17%*) for 40_50
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible * traite_cible_conjoint, subset = categorie_cible == '40_50', data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible + traite_cible_conjoint, subset = categorie_cible == '20_30', data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible + traite_cible_conjoint, subset = categorie_cible == '30_40', data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible + traite_cible_conjoint, subset = categorie_cible == '40_50', data=s, weights = s$weight))
# 13%*** (resp. 8%***) more chance of acceptance with traite_cible (resp. traite_cible_conjoint), with -7%. if both are treated
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible * traite_cible_conjoint + cible + taxe_approbation + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible * traite_cible_conjoint + cible + taxe_approbation + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), subset=is.element(categorie_cible, c('20_30', '30_40', '40_50')), data=s, weights = s$weight))
# Effect driven by cible=30 for respondent and cible=20 for conjoint
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible * traite_cible_conjoint * cible + taxe_approbation + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible * traite_cible_conjoint * cible + taxe_approbation + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ versement_cible*cible + simule_gain_cible + taxe_approbation + revenu + revenu_conjoint, data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible, subset = revenu >= 780 & revenu < 1140, data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible, subset = revenu >= 1140 & revenu < 1430, data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible, subset = revenu >= 1430 & revenu < 1670, data=s, weights = s$weight))


##### Régressions: 2.3 2SLS gagnant_cible_categorie ######
# traite_cible: 19%*** (conjoint 10%**) 
tsls1 <- lm(gagnant_cible_categorie !='Perdant'~ traite_cible + traite_cible_conjoint + tax_acceptance, subset=is.element(categorie_cible, c('20_30', '30_40', '40_50')), data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1)
gagnant.hat <- fitted.values(tsls1)
# gagnant.hat: 69% MAIS manquent des contrôles
summary(lm((taxe_cible_approbation!='Non') ~ gagnant.hat + tax_acceptance, data=s[is.element(s$categorie_cible, c('20_30', '30_40', '40_50')),], weights = s$weight[is.element(s$categorie_cible, c('20_30', '30_40', '40_50'))]))
# cf. commentaires de 3. dans private_benefits

# tsls_rdd_1 <- lm((gagnant_cible_categorie!='Perdant') ~ traite_cible * traite_cible_conjoint + taxe_approbation + hausse_depenses + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, subset=cible!='70_', weights = s$weight)
tsls_rdd_1 <- lm((gagnant_cible_categorie!='Perdant') ~ traite_cible * traite_cible_conjoint + taxe_approbation + simule_gain_cible + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, subset=cible!='70_', weights = s$weight)
summary(tsls_rdd_1) # exclure les >70 ou pas ? Non
gagnant.hat <- fitted.values(tsls_rdd_1)
# summary(lm(dummy_approbation_cible ~ gagnant.hat + taxe_approbation + hausse_depenses + revenu + revenu_conjoint, data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ gagnant.hat + taxe_approbation + simule_gain_cible + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, subset=cible!='70_', weights = s$weight))

# On estime un TOT : ceteris paribus, se considérer comme gagnant augmente la probabilité d'approbation de 47 p.p.
# Note : je ne suis pas sûr que d_rdd.hat exprime ce que l'on souhaite : quel rôle des variables de contrôle dans le 1er et 2e stage ? Revoir la théorie

# Avec effet hétérogène par seuil
# s$cible <- relevel(as.factor(s$cible), '50')
summary(lm((taxe_cible_approbation!='Non') ~ gagnant.hat*cible + taxe_approbation + simule_gain_cible + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, subset=cible!='70_', weights = s$weight))
# Les résultats ne sont pas significatifs pour les effets par seuil. L'effet d_rdd.hat varie pour cible=30, et perd en significativité (enfin on reste à 4e-5).


##### Régressions: Feedback #####
# OLS - s'estimer gagnant: +31%*** chance d'approuver / non perdant: +33%*** chance d'accepter
summary(lm((taxe_feedback_approbation == 'Oui') ~ (gagnant_feedback_categorie == 'Gagnant') + taxe_approbation, data=s, weights = s$weight))
summary(lm((taxe_feedback_approbation != 'Non') ~ (gagnant_feedback_categorie != 'Perdant') + taxe_approbation, data=s, weights = s$weight))

# 2.4 RDD simple - effet d'être (simulé) gagnant: +9%*** approbation / +15%*** acceptation
summary(lm((taxe_feedback_approbation == 'Oui') ~ simule_gagnant + simule_gain + I(simule_gain^2) + taxe_approbation, data=s, weights = s$weight))
summary(lm((taxe_feedback_approbation != 'Non') ~ simule_gagnant + simule_gain + I(simule_gain^2) + taxe_approbation, data=s, weights = s$weight))

# 2.5 2SLS avec 1st stage RDD - effet de se considérer gagnant
cor(s$gagnant_feedback_categorie == 'Gagnant', s$simule_gagnant==1, use = "complete.obs") # 0.24
cor(s$gagnant_feedback_categorie != 'Perdant', s$simule_gagnant==1, use = "complete.obs") # 0.33

# 19%***
tsls_rdd_feedback_1 <- lm(gagnant_feedback_categorie == 'Gagnant' ~ simule_gagnant + taxe_approbation + simule_gain + I(simule_gain^2), data=s, weights = s$weight, na.action="na.exclude")
summary(tsls_rdd_feedback_1)
gagnant_f.hat <- fitted.values(tsls_rdd_feedback_1) 
# +49%***
summary(lm((taxe_feedback_approbation == 'Oui') ~ gagnant_f.hat + taxe_approbation + simule_gain + I(simule_gain^2),  data=s, weights = s$weight))

# +32%***
tsls_rdd_feedback_2 <- lm(gagnant_feedback_categorie != 'Perdant' ~ simule_gagnant + taxe_approbation + simule_gain + I(simule_gain^2), data=s, weights = s$weight, na.action="na.exclude")
summary(tsls_rdd_feedback_2)
gagnant_f.hat <- fitted.values(tsls_rdd_feedback_2)
# +47%***
summary(lm((taxe_feedback_approbation != 'Non') ~ gagnant_f.hat + taxe_approbation + simule_gain + I(simule_gain^2), data=s, weights = s$weight))

# 2.6
tsls_rdd_feedback_D <- lm((gagnant_feedback_categorie != 'Perdant') - (gagnant_categorie != 'Perdant') ~ simule_gagnant + taxe_approbation + simule_gain + I(simule_gain^2), data=s, weights = s$weight, na.action="na.exclude")
summary(tsls_rdd_feedback_D)
gagnant_f.hat <- fitted.values(tsls_rdd_feedback_D)
# +47%***
summary(lm((taxe_feedback_approbation != 'Non') ~ gagnant_f.hat + taxe_approbation + simule_gain + I(simule_gain^2), data=s, weights = s$weight))

# OLS with controls
feed_ols <- as.formula(paste("taxe_feedback_approbation!='Non' ~ (gagnant_info_categorie!='Perdant') + gagnant_categorie + tax_acceptance + (taxe_approbation=='NSP') + Simule_gain + Simule_gain2 + taxe_efficace +", 
                             paste(variables_reg_self_interest, collapse = ' + ')))
summary(lm(feed_ols, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f']))


##### Régressions: persistance des croyances #####
# TODO!: Etudier les gens gagnant en pouvoir d'achat mais perdant en utilit? (en prenant une ?lasticit? nulle)
# 3.1 apprendre qu'on est (simulé) gagnant augmente la croyance de ne pas perdre de 23%***
croyances_1 <- lm(((gagnant_feedback_categorie!='Perdant') - (gagnant_categorie!='Perdant')) ~ simule_gagnant + simule_gain + I(simule_gain^2), data=s, weights = s$weight, na.action="na.exclude")
summary(croyances_1)
Dgagnant.hat <- fitted.values(croyances_1)
# 2.4 apprendre qu'on est (simulé) gagnant augmente l'acceptation de 12%***
summary(lm(((taxe_feedback_approbation!='Non') - (taxe_approbation!='Non')) ~ simule_gagnant + simule_gain + I(simule_gain^2), data=s, weights = s$weight)) # TODO: rajouter contrôles
# 3.2 comprendre qu'on est non perdant augmente l'approbation de 51%*** = 11/0.23 (0.5313=0.1182/0.2304)
summary(lm(((taxe_feedback_approbation!='Non') - (taxe_approbation!='Non')) ~ Dgagnant.hat + simule_gain + I(simule_gain^2), data=s, weights = s$weight))


##### Adaptation Bayesienne biaisée des croyances (aka. "on se fait Bayeser") #####
summary(lm((gain*uc) ~ versement + hausse_depenses, subset = s$hausse_depenses<200, data=s, weights = s$weight)) 
summary(lm((gain - versement/uc) ~ 1, data=s, weights = s$weight)) 
summary(lm(gain ~ I(versement/uc) + I(hausse_depenses/uc) + I(hausse_depenses^2/uc) + taille_menage + diplome + age + csp + region + revenu + rev_tot + I(revenu^2) + I(rev_tot^2), data=s, weights = s$weight))

cor(s$hausse_depenses, s$gain*s$uc)
cor(s$hausse_depenses[s$nb_adultes==1], (s$gain*s$uc)[s$nb_adultes==1])
cor(s$perte_relative_partielle, s$gain)
cor(s$perte_relative_chauffage, s$hausse_chauffage, use="complete.obs")
cor(s$perte_relative_fuel, s$hausse_carburants, use="complete.obs")
plot(s$hausse_carburants, jitter(s$perte_relative_fuel), xlim=c(0, 500))
plot(s$hausse_chauffage, jitter(s$perte_relative_chauffage), xlim=c(0, 500))
length(which(s$gain <= 0 & s$km < 1000 & s$gaz==FALSE & s$fioul==FALSE))

decrit(s$update_correct)
decrit(s$update_correct_large) 
# Les gens pensant perdre légèrement updatent plus correctement que la moyenne et plus ils se trompent, plus ils updatent correctement.
# Les autres catégories ne sont pas significatives: notamment le fait de se tromper davantage n'est pas associé à updater plus correctement que la moyenne, même si le signe est +.
summary(lm(update_correct_large ~ I(simule_gain - gain)*as.factor(gain), data=s, weights = s$weight))

# Biais à la perte : (Tout est significatif) Ceux qui gagnent (simulé) updatent moins correctement, ceux qui croient perdrent aussi. 
summary(lm(update_correct_large ~ simule_gagnant*gagnant_categorie, data=s, weights = s$weight))

# summary(lm(((taxe_feedback_approbation=='Oui') - (taxe_approbation=='Oui')) ~ I(feedback_confirme - feedback_infirme)*(s$gagnant_categorie=='Perdant'), data=s, weights=s$weight))
# summary(lm(((taxe_feedback_approbation=='Oui') - (taxe_approbation=='Oui')) ~ simule_gagnant*(gagnant_categorie=='Gagnant'), data=s, weights=s$weight))
decrit(s$feedback_infirme, weights = s$weight) # 48%
decrit(s$feedback_infirme_large, weights = s$weight) # 70%
decrit(s$update_correct[s$feedback_infirme==T], weights = s$weight[s$feedback_infirme==T]) # 15%
decrit(s$update_correct[s$feedback_infirme_large==T], weights = s$weight[s$feedback_infirme_large==T]) # 18%
decrit(s$update_correct[s$feedback_infirme==T & s$simule_gagnant==1], weights = s$weight[s$feedback_infirme==T & s$simule_gagnant==1])
decrit(s$update_correct[s$feedback_infirme==T & s$simule_gagnant==0], weights = s$weight[s$feedback_infirme==T & s$simule_gagnant==0])
sum(s$weight[s$feedback_infirme & s$simule_gagnant==1])/3002 # 46%
sum(s$weight[!is.na(s$update_correct) & s$update_correct==1 & s$feedback_infirme & s$simule_gagnant==1])/sum(s$weight[!is.na(s$update_correct) & s$feedback_infirme & s$simule_gagnant==1]) # 12%

# 3.3 Les gens qui se croient gagnants updatent plus correctement que les autres lorsqu'ils doivent le faire
summary(lm(update_correct_large ~ gagnant_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight))
summary(lm(update_correct ~ gagnant_categorie=='Gagnant', subset = feedback_infirme==T, data=s, weights = s$weight)) # Non affectés exclus par feedback_infirme

# 3.4 Biais d'update à la perte
summary(lm(update_correct ~ gagnant_feedback_categorie=='Gagnant', subset = feedback_infirme==T, data=s, weights = s$weight)) # Non affectés exclus par feedback_infirme

# 3.5 Biais à la perte: des gens pensent perdre après feedback confirmant qu'ils gagnent 5 fois sur 6. 
# (Sont-ce des gens qui ont répondu au pif perdre après un feedback confirmant leur gain ? Non, cf. les deux lignes d'après)
summary(lm(update_correct_large ~ gagnant_categorie=='Gagnant', data=s, weights = s$weight)) 
decrit(s$update_correct[s$gagnant_categorie=='Gagnant' & s$simule_gagnant==1])
decrit(s$update_correct[s$gagnant_categorie=='Perdant' & s$simule_gagnant==0])

# 3.6 Indication de non-biais vers la perte: parmi les non affectés, approbation augmente de 7%* (resp. 5%) quand on leur dit qu'ils gagnent (resp. perdent)
summary(lm(I((taxe_feedback_approbation=='Oui') - (taxe_approbation=='Oui')) ~ simule_gagnant, subset=gain=='0', data=s, weights=s$weight))
summary(lm(I((taxe_feedback_approbation=='Oui') - (taxe_approbation=='Oui')) ~ 0 + simule_gagnant, subset=gain=='0', data=s, weights=s$weight))

# 3.7 Signe > 0 indique biais de confirmation: ~ 0.03 dans les deux 
summary(lm(((taxe_feedback_approbation=='Oui') - (taxe_approbation=='Oui')) ~ gagnant_categorie=='Gagnant', subset = simule_gagnant==1, data=s, weights=s$weight))
summary(lm(((taxe_feedback_approbation=='Oui') - (taxe_approbation=='Oui')) ~ gagnant_categorie!='Perdant', subset = simule_gagnant==0, data=s, weights=s$weight))
decrit(s$feedback_confirme)

# 3.3 et 3.4, all regressions possibles:
base_winner_all <- lm(update_correct ~ gagnant_categorie=='Gagnant', data=s, weights = s$weight) #
base_feedback_winner_all <- lm(update_correct ~ gagnant_feedback_categorie=='Gagnant',  data=s, weights = s$weight) 
base_winner_validated <- lm(update_correct ~ gagnant_categorie=='Gagnant', subset = feedback_infirme_large==FALSE, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
base_feedback_winner_validated <- lm(update_correct ~ gagnant_feedback_categorie=='Gagnant', subset = feedback_infirme_large==FALSE, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
asymmetric <- stargazer(base_winner, base_feedback_winner, base_winner_validated, base_feedback_winner_validated,
                        title="Asymmetric updating of winning category", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                        covariate.labels = c("Constant", "Winner, before feedback ($\\dot{G}$)", "Winner, after feedback ($\\dot{G}^F$)"),
                        dep.var.labels = "Correct updating ($U$)", dep.var.caption = "",
                        add.lines = c("Among: invalidated & \\checkmark & \\checkmark &  &  ", "Among: validated & & & \\checkmark & \\checkmark  "),
                        no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="asymmetric")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', asymmetric, fixed=TRUE), fixed=TRUE), collapse=' ')


base_winner <- lm(update_correct ~ gagnant_categorie=='Gagnant', subset = feedback_infirme==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
large_winner <- lm(update_correct_large ~ gagnant_categorie=='Gagnant', subset = feedback_infirme==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
base_not_loser <- lm(update_correct ~ gagnant_categorie!='Perdant', subset = feedback_infirme==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
large_not_loser <- lm(update_correct_large ~ gagnant_categorie!='Perdant', subset = feedback_infirme==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
base_winner_large <- lm(update_correct ~ gagnant_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
large_winner_large <- lm(update_correct_large ~ gagnant_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
base_not_loser_large <- lm(update_correct ~ gagnant_categorie!='Perdant', subset = feedback_infirme_large==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
large_not_loser_large <- lm(update_correct_large ~ gagnant_categorie!='Perdant', subset = feedback_infirme_large==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
summary(base_winner)
summary(large_winner)
summary(base_not_loser)
summary(large_not_loser)
summary(base_winner_large)
summary(large_winner_large)
summary(base_not_loser_large)
summary(large_not_loser_large)

stargazer(base_winner, base_winner_large, base_not_loser_large, large_winner, large_winner_large, large_not_loser_large, 
          title="Asymmetric updating of winning category", star.cutoffs = c(0.1, 1e-5, 1e-30),
          covariate.labels = c("Constant", "Winner, before feedback ($\\dot{G}$)", "Not loser, before feedback ($G$)"),
          # covariate.labels = c("Constant", "Winner, before feedback", "Not loser, before feedback", "Winner, after feedback", "Not loser, after feedback"),
          # omit = c(),  dep.var.caption = "", 
          dep.var.labels = c("Correct updating", "Correct updating direction"), dep.var.caption = "",
          add.lines = c("Among: invalidated & \\checkmark & \\checkmark &  &  & \\checkmark & \\checkmark &  & ",
                        "Among: loosely invalidated &  &  & \\checkmark & \\checkmark &  &  & \\checkmark & \\checkmark"),
          no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="asymmetric_all")

base_feedback_winner <- lm(update_correct ~ gagnant_feedback_categorie=='Gagnant', subset = feedback_infirme==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
large_feedback_winner <- lm(update_correct_large ~ gagnant_feedback_categorie=='Gagnant', subset = feedback_infirme==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
base_feedback_not_loser <- lm(update_correct ~ gagnant_feedback_categorie!='Perdant', subset = feedback_infirme==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
large_feedback_not_loser <- lm(update_correct_large ~ gagnant_feedback_categorie!='Perdant', subset = feedback_infirme==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
base_feedback_winner_large <- lm(update_correct ~ gagnant_feedback_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
large_feedback_winner_large <- lm(update_correct_large ~ gagnant_feedback_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
base_feedback_not_loser_large <- lm(update_correct ~ gagnant_feedback_categorie!='Perdant', subset = feedback_infirme_large==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
large_feedback_not_loser_large <- lm(update_correct_large ~ gagnant_feedback_categorie!='Perdant', subset = feedback_infirme_large==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
summary(base_feedback_winner)
summary(large_feedback_winner)
summary(base_feedback_not_loser)
summary(large_feedback_not_loser)
summary(base_feedback_winner_large)
summary(large_feedback_winner_large)
summary(base_feedback_not_loser_large)
summary(large_feedback_not_loser_large)

stargazer(base_feedback_winner, base_feedback_not_loser, base_feedback_winner_large, base_feedback_not_loser_large, large_feedback_winner, large_feedback_not_loser, large_feedback_winner_large, large_feedback_not_loser_large, 
          title="Asymmetric updating of winning category", star.cutoffs = c(0.1, 1e-5, 1e-30),
          covariate.labels = c("Constant", "Winner, after feedback ($\\dot{G^F}$)", "Not loser, after feedback ($G^F$)"),
          # covariate.labels = c("Constant", "Winner, before feedback", "Not loser, before feedback", "Winner, after feedback", "Not loser, after feedback"),
          # omit = c(),  dep.var.caption = "", 
          dep.var.labels = c("Correct updating", "Correct updating direction"), dep.var.caption = "",
          add.lines = c("Among: invalidated & \\checkmark & \\checkmark &  &  & \\checkmark & \\checkmark &  & ",
                        "Among: loosely invalidated &  &  & \\checkmark & \\checkmark &  &  & \\checkmark & \\checkmark"),
          no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="asymmetric_after_all")

base_winner_interact <- lm(update_correct ~ (gagnant_categorie=='Gagnant')*(taxe_approbation=='Oui'), subset = feedback_infirme_large==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
base_feedback_winner_interact <- lm(update_correct ~ (gagnant_feedback_categorie=='Gagnant')*(taxe_approbation=='Oui'), subset = feedback_infirme_large==T, data=s, weights = s$weight) # Non affectés exclus par feedback_infirme
summary(base_winner_interact)
summary(base_feedback_winner_interact)



##### Adaptation Bayesienne: nouveau modèle #####
plot(s$simule_gain, jitter(s$gain, 10), xlim=c(-400,300), type='p', col='blue', cex=0.1, xlab='Simulated gain', ylab='Subjective gain')
abline(lm(gain ~ simule_gain, data=ss), col='blue', lwd=2)
lines(seq(-500, 500, by=10), seq(-500, 500, by=10), type='l', col='black') + grid()
abline(h = 0, v=0)
cor(s$gain, s$simule_gain) # 0.05
summary(lm(gain ~ simule_gain, data=ss))
sum(s$weight[s$simule_gain < s$gain])/sum(s$weight) # 11% surestiment leurs gains

b <- wtd.mean(s$simule_gain - s$gain, weights = s$weight) # 136
# b <- median(s$simule_gain - s$gain) # 161

loess_gains <- loess((gagnant_categorie!='Perdant') ~ I(simule_gain-b), data=ss)
# plot(s$simule_gain, (s$gagnant_categorie!='Perdant'), col = "red", xlim = c(-500, 300))
plot(sort(s$simule_gain), predict(loess_gains)[order(s$simule_gain)], xlim = c(-500, 300), ylim=c(0,1), type='l', col = "blue", lwd=2)
lines(seq(-500, 300, by=10), predict(loess_gains, newdata = seq(-500, 300, by=10)), type='l', col='red')
grid() # blue: y -> f(y-b) / red: y-b -> f(y-b) (the good one)

f <- function(x, xmin=-500, xmax=300, by=10) approxfun(seq(xmin, xmax, by=by), predict(loess_gains, newdata = seq(xmin, xmax, by=by)), rule=2)(x)
f_inv <- function(x, xmin=-500, xmax=300, by=10) approxfun(predict(loess_gains, newdata = seq(xmin, xmax, by=by)), seq(xmin, xmax, by=by), rule=2)(x) # imputes extremal values when outside bonds

ggplot(data=s, aes(simule_gain, 1*(s$gagnant_categorie!='Perdant'))) + ylim(0,1) + xlim(-500, 300) + # xlim(-500, 400) +
  xlab("Simulated gain") + ylab("Probability of thinking not to loose") + geom_smooth(method='loess') #  + geom_point()
# why method='auto' yields such a different result?
length(which(s$simule_gain< -500))
length(which(s$simule_gain > 400))
# length(which(s$simule_gain< -500 | s$simule_gain>400)) # 76

decrit(s$gain_echelle)
G_F_by_gain_echelle <- c()
gain_by_gain_echelle <- c()
imax <- 1
for (i in -6:imax) { # différent de confirmation_bias parce que repose sur les catégories de l'échelle, pas sur les quantiles
  G_F_by_gain_echelle <- c(G_F_by_gain_echelle, sum(s$weight[s$variante_taxe_info=='f' & s$gagnant_feedback_categorie!='Perdant' & pmin(s$gain_echelle, imax)==i])/sum(s$weight[s$variante_taxe_info=='f' & pmin(s$gain_echelle, imax)==i]))
  gain_by_gain_echelle <- c(gain_by_gain_echelle, wtd.mean(s$gain[s$variante_taxe_info=='f' & pmin(s$gain_echelle, imax)==i], weights = s$weight[s$variante_taxe_info=='f' & pmin(s$gain_echelle, imax)==i])) }
plot(-6:(-7+length(G_F_by_gain_echelle)), G_F_by_gain_echelle, type='l', xlab='min(gain_echelle, 1)', ylab="Probability that G^F ≠ 'Perdant'") + grid()
hat_g_F_by_gain_echelle <- f_inv(G_F_by_gain_echelle)
hat_alpha_i_by_gain_echelle <- 1 + (gain_by_gain_echelle - hat_g_F_by_gain_echelle)/b
decrit(hat_alpha_i_by_gain_echelle) 

confirmation_bias <- function(by_variable = 'gain', nb_bin = 8, local_b = TRUE, return='all', method='median') { # return = c('all', '', 'alpha', 'hat_alpha_i', 'b_i')
  nb_bins <- length(levels(binning(s[[by_variable]], bins=nb_bin, method="wtd.quantile", ordered=FALSE, weights=s$weight)))
  bins <- binning(s[[by_variable]], bins=nb_bin, method="wtd.quantile", labels=c(1:nb_bins), ordered=FALSE, weights=s$weight)
  gain_i <- G_F_i <- b_i <- G_i <- p_i <- variable_i <- c()
  for (i in 1:nb_bins) {
    p_i <- c(p_i, sum(s$weight[s$variante_taxe_info=='f' & bins==i])/sum(s$weight[s$variante_taxe_info=='f']))
    G_i <- c(G_i, sum(s$weight[s$variante_taxe_info=='f' & s$gagnant_categorie!='Perdant' & bins==i])/sum(s$weight[s$variante_taxe_info=='f' & bins==i]))
    G_F_i <- c(G_F_i, sum(s$weight[s$variante_taxe_info=='f' & s$gagnant_feedback_categorie!='Perdant' & bins==i])/sum(s$weight[s$variante_taxe_info=='f' & bins==i]))
    if (method=='median') {
      variable_i <- c(variable_i, wtd.median(s[[by_variable]][s$variante_taxe_info=='f' & bins==i], weight=s$weight[s$variante_taxe_info=='f' & bins==i]))
      gain_i <- c(gain_i, wtd.median(s$gain[s$variante_taxe_info=='f' & bins==i], weight = s$weight[s$variante_taxe_info=='f' & bins==i]))
      b_i <- c(b_i, wtd.median((s$simule_gain - s$gain)[s$variante_taxe_info=='f' & bins==i], weight = s$weight[s$variante_taxe_info=='f' & bins==i]))
    } else if (method=='mean') {
      variable_i <- c(variable_i, wtd.mean(s[[by_variable]][s$variante_taxe_info=='f' & bins==i], weights=s$weight[s$variante_taxe_info=='f' & bins==i]))
      gain_i <- c(gain_i, wtd.mean(s$gain[s$variante_taxe_info=='f' & bins==i], weights = s$weight[s$variante_taxe_info=='f' & bins==i]))
      b_i <- c(b_i, wtd.mean((s$simule_gain - s$gain)[s$variante_taxe_info=='f' & bins==i], weights = s$weight[s$variante_taxe_info=='f' & bins==i]))
    }
    s$b_i[s$variante_taxe_info=='f' & bins==i] <<- b_i[i] }
  plot(1:nb_bins, G_F_i, type='l', xlab=paste('bins of', by_variable, '(lowest to highest)'), ylab="Probability that G^F ≠ 'Perdant'") + grid()
  
  lowess_gains <- loess((gagnant_categorie!='Perdant') ~ I(simule_gain - b_i), data=ss)
  f__1 <- function(x, xmin=-500, xmax=300, by=10) approxfun(predict(lowess_gains, newdata = seq(xmin, xmax, by=by)), seq(xmin, xmax, by=by), rule=2)(x) # imputes extremal values when outside bonds
  plot(seq(-500, 300, by=10), predict(lowess_gains, newdata = seq(-500, 300, by=10)), xlab=paste('simule_gain - biais_bin(i), où bin vient de', by_variable), ylab='Proba that G != Perdant', type='l', col='red') + grid()
  
  hat_g_F_i <- f__1(G_F_i)
  hat_alpha_i <- 1 + (gain_i - hat_g_F_i)/(b_i*local_b + (!local_b)*wtd.mean(s$simule_gain - s$gain, weights = s$weight))
  
  alphas_i <- mean_alphas_i <- median_alphas_i <- c()
  for (i in 1:nb_bins) {
    alphas_i[s$variante_taxe_info=='f' & bins==i] <- 1 + (s$gain[s$variante_taxe_info=='f' & bins==i] - hat_g_F_i[i])/(s$simule_gain[s$variante_taxe_info=='f' & bins==i] - s$gain[s$variante_taxe_info=='f' & bins==i])
    mean_alphas_i <- c(mean_alphas_i, wtd.mean(alphas_i[s$variante_taxe_info=='f' & bins==i], weights = s$weight[s$variante_taxe_info=='f' & bins==i]))
    median_alphas_i <- c(median_alphas_i, wtd.median(alphas_i[s$variante_taxe_info=='f' & bins==i], weight = s$weight[s$variante_taxe_info=='f' & bins==i]))
  }
  
  name_by_var <- paste(by_variable, 'i', sep='_')
  if (return=='alpha') return(median(hat_alpha_i))
  else if (return=='all') return(list('alpha'=median(hat_alpha_i),  name_by_var=variable_i, 'G_i'=G_i, 'gain_i'=gain_i, 
                                      'b_i'=b_i ,'G_F_i'=G_F_i, 'hat_g_F_i'=hat_g_F_i, 'hat_alpha_i'=hat_alpha_i,
                                      'share_alphas_i>0'=sum(s$weight[alphas_i>0 & s$variante_taxe_info=='f'])/sum(s$weight[s$variante_taxe_info=='f']), 'share_alphas_i>1'=sum(s$weight[alphas_i>1 & s$variante_taxe_info=='f'])/sum(s$weight[s$variante_taxe_info=='f']),
                                      'mean(alphas_i)'=wtd.mean(alphas_i, weights = s$weight, na.rm=T),
                                      'median(alphas_i)'=wtd.median(alphas_i, weight = s$weight, na.rm=T), 'mean_alphas_i'=mean_alphas_i, 'median_alphas_i'=median_alphas_i)) #
  else if (return=='hat_alpha_i') return(hat_alpha_i)
  else if (return=='b_i') return(b_i)
  else return(list('alpha'=median(hat_alpha_i), 'hat_alpha_i'=hat_alpha_i, 'b_i'=b_i))
}

# preferred specification:
confirmation_bias('simule_gain', 7, TRUE, 'all', 'median')

# me semble le plus pertinent a priori car repose sur une variable objective, mais donne des résultats aberrants
confirmation_bias('simule_gain', 8, TRUE, 'all', 'mean') 
# gain marche bien (pb: prendre gain rend les bin endogènes)
confirmation_bias('gain', 7, TRUE, 'all', 'median') 
confirmation_bias('gain', 7, TRUE, 'all', 'mean') # higher values yield non monotonic functions
confirmation_bias('gain_echelle', 8, TRUE, 'all', 'mean') 

confirmation_bias(nb_bin = 1, method='mean')

# Bizarre: les gain = non affectés sont en moyenne perdants selon simule_gain ! (valable selon quand on pondère) => TODO: clean numbers simule_gain, also hausse_carburants
decrit(s$simule_gain[s$gain==0 & s$variante_taxe_info=='f'], weights= s$weight[s$gain==0 & s$variante_taxe_info=='f'])
max(s$simule_gain[bins==4])


##### Adaptation bayésienne : tout nouveau modèle ######
s$gagnant_feedback_pas_faux <- (s$simule_gagnant==1 & s$gagnant_feedback_categorie!='Perdant') | (s$simule_gagnant==0 & s$gagnant_feedback_categorie!='Gagnant')
s$gagnant_feedback_correct <- (s$simule_gagnant==1 & s$gagnant_feedback_categorie=='Gagnant') | (s$simule_gagnant==0 & s$gagnant_feedback_categorie=='Perdant')
decrit(s$gagnant_feedback_pas_faux)
decrit(s$gagnant_feedback_correct)
nb_bin <- 8
nb_bins <- length(levels(binning(s$simule_gain, bins=nb_bin, method="wtd.quantile", ordered=FALSE, weights=s$weight)))
s$bins_simule_gain <- binning(s$simule_gain, bins=nb_bin, method="wtd.quantile", labels=c(1:nb_bins), ordered=FALSE, weights=s$weight)
sigma_i <- feedback_pas_faux_i <- feedback_correct_i <- c()
for (i in 1:nb_bins) { # TODO: make bins of same size (?)
  if (i==3) { # bin that contains both positive and negative simule_gain
    s$bins_simule_gain[s$simule_gain<0 & s$bins_simule_gain==i] <- 2
    # s$bins_simule_gain[s$simule_gain>0 & s$bins_simule_gain==i] <- 4
  } 
  sigma_i <- c(sigma_i, sqrt(wtd.var(s$gain[s$variante_taxe_info=='f' & s$bins_simule_gain==i], s$weight[s$variante_taxe_info=='f' & s$bins_simule_gain==i])))
  feedback_pas_faux_i <- c(feedback_pas_faux_i, sum(s$weight[s$variante_taxe_info=='f' & s$gagnant_feedback_pas_faux==T & s$bins_simule_gain==i])/sum(s$weight[s$variante_taxe_info=='f' & s$bins_simule_gain==i]))
  feedback_correct_i <- c(feedback_correct_i, sum(s$weight[s$variante_taxe_info=='f' & s$gagnant_feedback_correct==T & s$bins_simule_gain==i])/sum(s$weight[s$variante_taxe_info=='f' & s$bins_simule_gain==i]))
  if (i<3) {
    s$phi_g_sigma_gamma_large[s$variante_taxe_info=='f' & s$bins_simule_gain==i] <- 1-(5/6)*(1-pnorm(-s$gain[s$variante_taxe_info=='f' & s$bins_simule_gain==i]/sigma_i[i], lower.tail=T))/feedback_pas_faux_i[i]
    s$phi_g_sigma_gamma[s$variante_taxe_info=='f' & s$bins_simule_gain==i] <- 1-(5/6)*(1-pnorm(-s$gain[s$variante_taxe_info=='f' & s$bins_simule_gain==i]/sigma_i[i], lower.tail=T))/feedback_correct_i[i]
  } else {
    s$phi_g_sigma_gamma_large[s$variante_taxe_info=='f' & s$bins_simule_gain==i] <- (5/6)*(pnorm(-s$gain[s$variante_taxe_info=='f' & s$bins_simule_gain==i]/sigma_i[i], lower.tail=T))/feedback_pas_faux_i[i]
    s$phi_g_sigma_gamma[s$variante_taxe_info=='f' & s$bins_simule_gain==i] <- (5/6)*(pnorm(-s$gain[s$variante_taxe_info=='f' & s$bins_simule_gain==i]/sigma_i[i], lower.tail=T))/feedback_correct_i[i]
  }
  # print(mean(s$simule_gain[s$bins_simule_gain==i]))
}
# decrit(s$bins_simule_gain)
s$non_bayesien_large <- s$phi_g_sigma_gamma_large < 0 | s$phi_g_sigma_gamma_large > 1
s$non_bayesien <- s$phi_g_sigma_gamma < 0 | s$phi_g_sigma_gamma > 1
s$sigma_gamma_large <- - s$gain/qnorm(s$phi_g_sigma_gamma_large)
s$sigma_gamma <- - s$gain/qnorm(s$phi_g_sigma_gamma)

decrit(s$non_bayesien_large, weights = s$weight) # 37%
decrit(s$non_bayesien, weights = s$weight) # 73%
decrit(s$phi_g_sigma_gamma_large[s$non_bayesien_large==FALSE], weights = s$weight[s$non_bayesien_large==FALSE])
decrit(s$phi_g_sigma_gamma[s$non_bayesien==FALSE], weights = s$weight[s$non_bayesien==FALSE])
decrit(s$sigma_gamma_large[s$non_bayesien_large==FALSE], weights = s$weight[s$non_bayesien_large==FALSE])
decrit(s$sigma_gamma[s$non_bayesien==FALSE], weights = s$weight[s$non_bayesien==FALSE])
decrit((s$sigma_gamma_large<0)[s$non_bayesien_large==FALSE], weights = s$weight[s$non_bayesien_large==FALSE]) # 14%
decrit((s$sigma_gamma<0)[s$non_bayesien==FALSE], weights = s$weight[s$non_bayesien==FALSE]) # 10%
sort(sigma_i) # 7 in 110-125, 148
sqrt(wtd.var(s$gain ,weights = s$weight)) # 124
feedback_pas_faux_i
feedback_correct_i
decrit(s$non_bayesien_large | s$sigma_gamma_large < 0, weights = s$weight) # 46%
decrit(s$non_bayesien | s$sigma_gamma < 0, weights = s$weight) # 76%
decrit(s$sigma_gamma_large[s$non_bayesien_large==FALSE & s$sigma_gamma_large > 0], weights = s$weight[s$non_bayesien_large==FALSE & s$sigma_gamma_large > 0]) # mean 181
decrit(s$sigma_gamma[s$non_bayesien==FALSE & s$sigma_gamma > 0], weights = s$weight[s$non_bayesien==FALSE & s$sigma_gamma > 0]) # mean 183
decrit(s$non_bayesien_large[s$simule_gain>0], weights = s$weight[s$simule_gain>0]) # 48%
decrit(s$non_bayesien[s$simule_gain>0], weights = s$weight[s$simule_gain>0]) # 94%
decrit(s$non_bayesien_large[s$simule_gain<0], weights = s$weight[s$simule_gain<0]) # 0%
decrit(s$non_bayesien[s$simule_gain<0], weights = s$weight[s$simule_gain<0]) # 0%

decrit(s$update_correct | s$gagnant_feedback_correct, weights = s$weight) # 43%
decrit(s$update_correct_large | s$gagnant_feedback_pas_faux, weights = s$weight) # 67%


##### Adaptation bayésienne : tout nouveau modèle ######
s$gagnant_feedback_pas_faux <- (s$simule_gagnant==1 & s$gagnant_feedback_categorie!='Perdant') | (s$simule_gagnant==0 & s$gagnant_feedback_categorie!='Gagnant')
s$gagnant_feedback_correct <- (s$simule_gagnant==1 & s$gagnant_feedback_categorie=='Gagnant') | (s$simule_gagnant==0 & s$gagnant_feedback_categorie=='Perdant')
decrit(s$gagnant_feedback_pas_faux)
decrit(s$gagnant_feedback_correct)
nb_bin <- 8
nb_bins <- length(levels(binning(s$simule_gain, bins=nb_bin, method="wtd.quantile", ordered=FALSE, weights=s$weight)))
s$bins_simule_gain <- binning(s$simule_gain, bins=nb_bin, method="wtd.quantile", labels=c(1:nb_bins), ordered=FALSE, weights=s$weight)
sigma_i <- feedback_pas_faux_i <- feedback_correct_i <- c()
for (i in 1:nb_bins) { # TODO: make bins of same size (?)
  if (i==3) { # bin that contains both positive and negative simule_gain
    s$bins_simule_gain[s$simule_gain<0 & s$bins_simule_gain==i] <- 2
    # s$bins_simule_gain[s$simule_gain>0 & s$bins_simule_gain==i] <- 4
  } 
  sigma_i <- c(sigma_i, sqrt(wtd.var(s$gain[s$variante_taxe_info=='f' & s$bins_simule_gain==i], s$weight[s$variante_taxe_info=='f' & s$bins_simule_gain==i])))
  feedback_pas_faux_i <- c(feedback_pas_faux_i, sum(s$weight[s$variante_taxe_info=='f' & s$gagnant_feedback_pas_faux==T & s$bins_simule_gain==i])/sum(s$weight[s$variante_taxe_info=='f' & s$bins_simule_gain==i]))
  feedback_correct_i <- c(feedback_correct_i, sum(s$weight[s$variante_taxe_info=='f' & s$gagnant_feedback_correct==T & s$bins_simule_gain==i])/sum(s$weight[s$variante_taxe_info=='f' & s$bins_simule_gain==i]))
  if (i<3) {
    s$phi_g_sigma_gamma_large[s$variante_taxe_info=='f' & s$bins_simule_gain==i] <- 1-(5/6)*(1-pnorm(-s$gain[s$variante_taxe_info=='f' & s$bins_simule_gain==i]/sigma_i[i], lower.tail=T))/feedback_pas_faux_i[i]
    s$phi_g_sigma_gamma[s$variante_taxe_info=='f' & s$bins_simule_gain==i] <- 1-(5/6)*(1-pnorm(-s$gain[s$variante_taxe_info=='f' & s$bins_simule_gain==i]/sigma_i[i], lower.tail=T))/feedback_correct_i[i]
  } else {
    s$phi_g_sigma_gamma_large[s$variante_taxe_info=='f' & s$bins_simule_gain==i] <- (5/6)*(pnorm(-s$gain[s$variante_taxe_info=='f' & s$bins_simule_gain==i]/sigma_i[i], lower.tail=T))/feedback_pas_faux_i[i]
    s$phi_g_sigma_gamma[s$variante_taxe_info=='f' & s$bins_simule_gain==i] <- (5/6)*(pnorm(-s$gain[s$variante_taxe_info=='f' & s$bins_simule_gain==i]/sigma_i[i], lower.tail=T))/feedback_correct_i[i]
  }
  # print(mean(s$simule_gain[s$bins_simule_gain==i]))
}
# decrit(s$bins_simule_gain)
s$non_bayesien_large <- s$phi_g_sigma_gamma_large < 0 | s$phi_g_sigma_gamma_large > 1
s$non_bayesien <- s$phi_g_sigma_gamma < 0 | s$phi_g_sigma_gamma > 1
s$sigma_gamma_large <- - s$gain/qnorm(s$phi_g_sigma_gamma_large)
s$sigma_gamma <- - s$gain/qnorm(s$phi_g_sigma_gamma)

decrit(s$non_bayesien_large, weights = s$weight) # 37%
decrit(s$non_bayesien, weights = s$weight) # 73%
decrit(s$phi_g_sigma_gamma_large[s$non_bayesien_large==FALSE], weights = s$weight[s$non_bayesien_large==FALSE])
decrit(s$phi_g_sigma_gamma[s$non_bayesien==FALSE], weights = s$weight[s$non_bayesien==FALSE])
decrit(s$sigma_gamma_large[s$non_bayesien_large==FALSE], weights = s$weight[s$non_bayesien_large==FALSE])
decrit(s$sigma_gamma[s$non_bayesien==FALSE], weights = s$weight[s$non_bayesien==FALSE])
decrit((s$sigma_gamma_large<0)[s$non_bayesien_large==FALSE], weights = s$weight[s$non_bayesien_large==FALSE]) # 14%
decrit((s$sigma_gamma<0)[s$non_bayesien==FALSE], weights = s$weight[s$non_bayesien==FALSE]) # 10%
sort(sigma_i) # 7 in 110-125, 148
sqrt(wtd.var(s$gain ,weights = s$weight)) # 124
feedback_pas_faux_i
feedback_correct_i
decrit(s$non_bayesien_large | s$sigma_gamma_large < 0, weights = s$weight) # 46%
decrit(s$non_bayesien | s$sigma_gamma < 0, weights = s$weight) # 76%
decrit(s$sigma_gamma_large[s$non_bayesien_large==FALSE & s$sigma_gamma_large > 0], weights = s$weight[s$non_bayesien_large==FALSE & s$sigma_gamma_large > 0]) # mean 181
decrit(s$sigma_gamma[s$non_bayesien==FALSE & s$sigma_gamma > 0], weights = s$weight[s$non_bayesien==FALSE & s$sigma_gamma > 0]) # mean 183
decrit(s$non_bayesien_large[s$simule_gain>0], weights = s$weight[s$simule_gain>0]) # 48%
decrit(s$non_bayesien[s$simule_gain>0], weights = s$weight[s$simule_gain>0]) # 94%
decrit(s$non_bayesien_large[s$simule_gain<0], weights = s$weight[s$simule_gain<0]) # 0%
decrit(s$non_bayesien[s$simule_gain<0], weights = s$weight[s$simule_gain<0]) # 0%

decrit(s$update_correct | s$gagnant_feedback_correct, weights = s$weight) # 43%
decrit(s$update_correct_large | s$gagnant_feedback_pas_faux, weights = s$weight) # 67%


##### Graph distributions subjective/objective gains #####
par(mar = c(2.1, 4.1, 1.1, 0.1), cex=1.5)
# (a) transport
cdf_transport <- Ecdf(objective_gains$transport)
cdf_transport_inelastic <- Ecdf(objective_gains_inelastic$transport)
cdf_transport_estimated <- Ecdf((60 * pmin(2, s$nb_adultes) - s$hausse_carburants)/ s$uc)
plot(Ecdf(s$gain_fuel), type="s", lwd=2, col="red", xlim=c(-250, 70), xlab="", main="", ylab=expression("Proportion "<=" x")) + grid()
plot(Ecdf(s$gain_fuel), type="s", lwd=2, col="red", xlab="", main="", ylab=expression("Proportion "<=" x")) + grid()
lines(cdf_transport$x, cdf_transport$y, lwd=2, col="blue")
lines(cdf_transport_inelastic$x, cdf_transport_inelastic$y, lwd=2, lty=2, col="blue")
lines(cdf_transport_estimated$x, cdf_transport_estimated$y, lwd=2, col="purple")
abline(v = c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), lty=3, col=rgb(1,0,0,0.7))
axis(3, at=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), tck=0.0, lwd=0, lwd.ticks = 0, padj=1.5, col.axis="red", cex.axis=0.9)
# (b) housing
cdf_housing <- Ecdf(objective_gains$housing)
cdf_housing_inelastic <- Ecdf(objective_gains_inelastic$housing)
cdf_housing_estimated_interaction <- Ecdf((50 * pmin(2, s$nb_adultes) - s$hausse_chauffage_interaction_inelastique/0.8) / s$uc)
cdf_housing_estimated <- Ecdf((50 * pmin(2, s$nb_adultes) - s$hausse_chauffage) / s$uc)
plot(Ecdf(s$gain_chauffage), type="s", lwd=2, col="red", xlim=c(-250, 90), xlab="", main="", ylab=expression("Proportion "<=" x")) + grid()
# plot(Ecdf(s$gain_chauffage), type="s", lwd=2, col="red", xlab="", main="", ylab=expression("Proportion "<=" x")) + grid()
lines(cdf_housing$x, cdf_housing$y, lwd=2, col="blue")
lines(cdf_housing_inelastic$x, cdf_housing_inelastic$y, lwd=2, lty=2, col="blue")
lines(cdf_housing_estimated$x, cdf_housing_estimated$y, lwd=2, col="purple")
lines(cdf_housing_estimated_interaction$x, cdf_housing_estimated_interaction$y, lwd=2, lty=2, col="purple")
abline(v=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), lty=3, col=rgb(1,0,0,0.7))
axis(3, at=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), tck=0.0, lwd=0, lwd.ticks = 0, padj=1.5, col.axis="red", cex.axis=0.9)
# (c) both combined 
cdf_all <- Ecdf(objective_gains$all)
cdf_all_inelastic <- Ecdf(objective_gains_inelastic$all)
cdf_all_estimated <- Ecdf(s$simule_gain)
cdf_all_estimated_interaction <- Ecdf(s$simule_gain_interaction)
plot(Ecdf(s$gain), type="s", lwd=2, col="red", xlim=c(-400, 180), xlab="", main="", ylab=expression("Proportion "<=" x")) + grid()
# plot(Ecdf(s$gain), type="s", lwd=2, col="red", xlab="", main="", ylab=expression("Proportion "<=" x")) + grid()
lines(cdf_all$x, cdf_all$y, lwd=2, col="blue")
lines(cdf_all_inelastic$x, cdf_all_inelastic$y, lwd=2, lty=2, col="blue")
lines(cdf_all_estimated$x, cdf_all_estimated$y, lwd=2, col="purple")
lines(cdf_all_estimated_interaction$x, cdf_all_estimated_interaction$y, lwd=2, lty=2, col="purple")
abline(v=c(-280, -190, -120, -70, -30, 0, 20, 40, 60, 80), lty=3, col=rgb(1,0,0,0.7))
axis(3, at=c(-280, -190, -120, -70, -30, 0, 20, 40, 60, 80), tck=0.0, lwd=0, lwd.ticks = 0, padj=1.5, col.axis="red", cex.axis=0.9)
# restore graphical parameters
par(mar = mar_old, cex = cex_old)

#### old:
mar_old <- par()$mar
cex_old <- par()$cex
par(mar = c(2.1, 4.1, 1.1, 0.1), cex=1.5)
plot(density(objective_gains$transport, bw=30), xlim=c(-400, 200), lwd=2, col="blue", xlab="", main="") + grid()
lines(density(subjective_gains$transport, bw=30), xlim=c(-400, 200), lwd=2, col="red")

plot(density(objective_gains$housing, bw=30), xlim=c(-400, 200), lwd=2, col="blue", xlab="", main="") + grid()
lines(density(subjective_gains$housing, bw=30), xlim=c(-400, 200), lwd=2, col="red")

plot(density(objective_gains$all, bw=30), xlim=c(-400, 200), lwd=2, col="blue", xlab="", main="") + grid()
lines(density(subjective_gains$all, bw=30), xlim=c(-400, 200), lwd=2, col="red")

par(mar = c(2.1, 4.1, 1.1, 0.1), cex=1.5)
cdf_transport <- Ecdf(objective_gains$transport)
cdf_transport_inelastic <- Ecdf(objective_gains_inelastic$transport)
cdf_transport_min <- Ecdf(s$gain_fuel_min)
cdf_transport_max <- Ecdf(s$gain_fuel_max)
plot(Ecdf(s$gain_fuel), type="s", lwd=2, col="red", xlab="", main="", ylab=expression("Proportion "<=" x")) + grid()
lines(cdf_transport$x, cdf_transport$y, lwd=2, col="blue")
lines(cdf_transport_inelastic$x, cdf_transport_inelastic$y, lwd=2, lty=2, col="blue")
# lines(cdf_transport_min$x, cdf_transport_min$y, type="s", lty=2, col="red")
# lines(cdf_transport_max$x, cdf_transport_max$y, type="s", lty=2, col="red")
# axis(1, at=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), tck=0.04, lwd=0, lwd.ticks = 1, col="red", labels=rep("", 10))
abline(v = c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), lty=3, col=rgb(1,0,0,0.7))
axis(3, at=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), tck=0.0, lwd=0, lwd.ticks = 0, padj=1.5, col.axis="red", cex.axis=0.9)

cdf_housing <- Ecdf(objective_gains$housing)
cdf_housing_inelastic <- Ecdf(objective_gains_inelastic$housing)
cdf_housing_min <- Ecdf(s$gain_chauffage_min)
cdf_housing_max <- Ecdf(s$gain_chauffage_max)
plot(Ecdf(s$gain_chauffage), type="s", lwd=2, col="red", xlab="", main="", ylab=expression("Proportion "<=" x")) + grid()
lines(cdf_housing$x, cdf_housing$y, lwd=2, col="blue")
lines(cdf_housing_inelastic$x, cdf_housing_inelastic$y, lwd=2, lty=2, col="blue")
# lines(cdf_housing_min$x, cdf_housing_min$y, type="s", lty=2, col="red")
# lines(cdf_housing_max$x, cdf_housing_max$y, type="s", lty=2, col="red")
abline(v=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), lty=3, col=rgb(1,0,0,0.7))
axis(3, at=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), tck=0.0, lwd=0, lwd.ticks = 0, padj=1.5, col.axis="red", cex.axis=0.9)

cdf_all <- Ecdf(objective_gains$all)
cdf_all_inelastic <- Ecdf(objective_gains_inelastic$all)
cdf_min <- Ecdf(s$gain_min)
cdf_max <- Ecdf(s$gain_max)
plot(Ecdf(s$gain), type="s", lwd=2, col="red", xlab="", main="", ylab=expression("Proportion "<=" x")) + grid()
lines(cdf_all$x, cdf_all$y, lwd=2, col="blue")
lines(cdf_all_inelastic$x, cdf_all_inelastic$y, lwd=2, lty=2, col="blue")
# lines(cdf_min$x, cdf_min$y, type="s", lty=2, col="red")
# lines(cdf_max$x, cdf_max$y, type="s", lty=2, col="red")
# axis(1, at=c(-280, -190, -120, -70, -30, 0, 20, 40, 60, 80), tck=0.04, lwd=0, lwd.ticks = 1, col="red", labels=rep("", 10))
abline(v=c(-280, -190, -120, -70, -30, 0, 20, 40, 60, 80), lty=3, col=rgb(1,0,0,0.7))
axis(3, at=c(-280, -190, -120, -70, -30, 0, 20, 40, 60, 80), tck=0.0, lwd=0, lwd.ticks = 0, padj=1.5, col.axis="red", cex.axis=0.9)


par(mar = mar_old, cex = cex_old)

##### Transports en commun #####
decrit(s$transports_avis, weights = s$weight, miss=T)

table_taille_agglo_transports <- round(Crosstab(s[s$transports_avis!='NSP',], row.vars="taille_agglo", col.vars="transports_avis", type="j", dec.places = 0)$Crosstab)
table_taille_agglo_transports[1:5, ] <- round(Crosstab(s[s$transports_avis!='NSP',], row.vars="taille_agglo", col.vars="transports_avis", type="r", dec.places = 0)$Crosstab)
table_taille_agglo_transports
xtable(table_taille_agglo_transports, digits=0)
# print(xtable(table_taille_agglo_transports, digits=0), file="table_taille_agglo_transports.tex")

decrit(s$transports_distance, weights = s$weight)
length(which(s$transports_distance <= 5))/length(which(!is.na(s$transports_distance))) # 49%
length(which(s$transports_distance <= 15))/length(which(!is.na(s$transports_distance))) # 77%
decrit(s$transports_frequence, weights = s$weight)
decrit(s$transports_courses, weights = s$weight)
decrit(s$transports_loisirs, weights = s$weight)
decrit(s$transports_travail, weights = s$weight)
decrit(s$transports_travail[s$transports_travail!='Non concerné·e'], weights = s$weight[s$transports_travail!='Non concerné·e'])
decrit(s$transports_travail_commun, weights = s$weight) # TODO: preparation
decrit(s$transports_travail_actif, weights = s$weight)
decrit(s$transports_travail_actif=='Non' & s$transports_travail_commun=='Non', weights = s$weight)
decrit(s$transports_travail_actif=='Oui, ça ne me poserait pas de grande difficulté' | s$transports_travail_commun=='Oui, ça ne me poserait pas de grande difficulté', weights = s$weight)


##### Transition matrix: gagnant avant/après feedback #####
# # non weighted:
# decrit(s$simule_gagnant)
# 
# decrit(s$gagnant_categorie[s$simule_gagnant==1])
# decrit(s$gagnant_feedback_categorie[s$simule_gagnant==1])
# round(Crosstab(s[s$simule_gagnant==1,], row.vars="gagnant_categorie", col.vars="gagnant_feedback_categorie", type="r", dec.places = 0)$Crosstab)
# 
# decrit(s$gagnant_categorie[s$simule_gagnant==0])
# decrit(s$gagnant_feedback_categorie[s$simule_gagnant==0])
# round(Crosstab(s[s$simule_gagnant==0,], row.vars="gagnant_categorie", col.vars="gagnant_feedback_categorie", type="r", dec.places = 0)$Crosstab)

# weighted:

mar_old <- par()$mar
cex_old <- par()$cex
par(mar = c(0.1, 3.1, 2.1, 0), cex.lab=1.2)

decrit(s$simule_gagnant, weights = s$weight)

decrit(s$gagnant_categorie[s$simule_gagnant==1], weights = s$weight[s$simule_gagnant==1])
decrit(s$gagnant_feedback_categorie[s$simule_gagnant==1], weights = s$weight[s$simule_gagnant==1])
crosstab_simule_gagnant <- crosstab(s$winning_category[s$simule_gagnant==1], s$winning_feedback_category[s$simule_gagnant==1], 
                                    s$weight[s$simule_gagnant==1], # dnn=c(expression('Winning category,'~bold(Before)~feedback), ''),
                                    prop.r=T, sort=2:1, cex.axis=0.9) # sort=2:1, dir=c("h", "v"), inv.x=T, inv.y=T, color = FALSE # see mosaicplot
crosstab_simule_gagnant
plot(crosstab_simule_gagnant, sort=2:1, cex.axis=0.9, ylab = expression('Winning category,'~bold(Before)~feedback), xlab=NA)
mtext(side=3, expression('Winning category,'~bold(After)~feedback), line=0.8, cex = 1.2)

decrit(s$gagnant_categorie[s$simule_gagnant==0], weights = s$weight[s$simule_gagnant==0])
decrit(s$gagnant_feedback_categorie[s$simule_gagnant==0], weights = s$weight[s$simule_gagnant==0])
crosstab_simule_gagnant <- crosstab(s$winning_category[s$simule_gagnant==0], s$winning_feedback_category[s$simule_gagnant==0], 
                                    s$weight[s$simule_gagnant==0], # dnn=c(expression('Winning category,'~bold(Before)~feedback), ''),
                                    prop.r=T, sort=2:1, cex.axis=0.9) # sort=2:1, dir=c("h", "v"), inv.x=T, inv.y=T, color = FALSE # see mosaicplot
crosstab_simule_gagnant
plot(crosstab_simule_gagnant, sort=2:1, cex.axis=0.9, ylab = expression('Winning category,'~bold(Before)~feedback), xlab=NA)
mtext(side=3, expression('Winning category,'~bold(After)~feedback), line=0.8, cex = 1.2)

par(mar = mar_old, cex = cex_old)

# mosaicplot(crosstab_simule_gagnant$tab, # dnn=c(expression('Winning category,'~bold(Before)~feedback), ''),
#          sort=2:1, cex.axis=0.9, labeling=c(1:9))
# mosaic(crosstab_simule_gagnant$tab, shade=FALSE, labeling=c(1:9))
# labeling_cells(text = round(100*crosstab_simule_gagnant$prop.r), margin = 0)(as.table(crosstab_simule_gagnant$prop.r))

#TODO: feedback robustesse sans gagnant !=, sans les +/-50


##### Corrélats du LATE #####
# Overall, no socio-demo difference between convinced and the others, but differences in energetic characteristics and attitudes to CC.
sum(s$tax_cible_acceptance - s$tax_acceptance > 0)
length(which(s$taxe_cible_approbation!='Non' & s$taxe_approbation=='Non' & (s$traite_cible==T | s$traite_cible_conjoint==T)))
length(which(s$taxe_feedback_approbation!='Non' & s$tax_acceptance==0))
length(which(s$taxe_feedback_approbation!='Non' & s$taxe_approbation=='Non' & s$simule_gagnant==1))

variables_exogenes <- c(variables_qualite, variables_demo, variables_transport, variables_politiques, variables_energie,
                        variables_gilets_jaunes, variables_Elasticite, variables_elasticite, 
                        variables_politiques_environnementales, variables_connaissances_CC, variables_avis_CC)
# Rien de significatif à 5% avec la 1ere specification, negligible Adjusted R-squared
summary(lm(as.formula(paste("convaincu_feedback==T ~ ", paste(variables_demo, collapse=' + '))), data=s, weights = s$weight))
# CSP: Inactif, Retraité +25p.p.*, Région Ouest -8 p.p.*, responsee_CC_riches -5p.p.*. But Negative Adjusted R-squared
summary(lm(as.formula(paste("convaincu_feedback==T ~ ", paste(variables_exogenes, collapse=' + '))), data=s, weights = s$weight))
# Null deviance: 1155, Residual deviance: 1091, nothing significant at 5%
summary(glm(as.formula(paste("convaincu_feedback==T ~ ", paste(variables_demo, collapse=' + '))), binomial, data=s, weights = s$weight))
# Null deviance: 1037, Residual deviance: 871, Significant at 5%: Région Ouest, humaniste, nb_vechicules, ges_avion, responsable_CC_riches
summary(glm(as.formula(paste("convaincu_feedback==T ~ ", paste(variables_exogenes, collapse=' + '))), binomial, data=s, weights = s$weight))
# AIC of this model (1208) lower than the two above (1230), indicating that nothing explains convaincu_feedback
summary(glm(convaincu_feedback==T ~ info_PM, binomial, data=s, weights = s$weight))

# Masculin -4 p.p.*, negligible Adjusted R-squared
summary(lm(as.formula(paste("convaincu_cible==T ~ ", paste(variables_demo, collapse=' + '))), data=s, weights = s$weight))
# Significant at 5%: opinions_CC, elasticite, essence, hausse_diesel, surface, ecologiste, humaniste, duree_info. Adjusted R-squared of 0.06
summary(lm(as.formula(paste("convaincu_cible==T ~ ", paste(variables_exogenes, collapse=' + '))), data=s, weights = s$weight))
# Null deviance: 814, Residual deviance: 750, nothing significant at 5%. AIC: 867
summary(glm(as.formula(paste("convaincu_cible==T ~ ", paste(variables_demo, collapse=' + '))), binomial, data=s, weights = s$weight))
# Null deviance: 739, Residual deviance: 482, Many significant at 5%. AIC: 818
summary(glm(as.formula(paste("convaincu_cible==T ~ ", paste(variables_exogenes, collapse=' + '))), binomial, data=s, weights = s$weight))
# AIC: 850, indicating that socio-demo explain nothing but other variables do.
summary(glm(convaincu_cible==T ~ info_PM, binomial, data=s, weights = s$weight))


##### Persistance of beliefs on effectiveness and progressivity #####
# L'acceptation que la taxe n'est pas inefficace passe de 32% à 36% après l'ancrage : "Les scientifiques s'accordent à dire qu'une taxe carbone serait efficace pour diminuer la pollution."
decrit(s$taxe_efficace, weights = s$weight, miss=T)
summary(lm(taxe_efficace!='Non' ~ apres_modifs, data=s, weights = s$weight)) # 0.04*
summary(lm(taxe_efficace!='Non' ~ apres_modifs + info_CC * info_PM, data=s, weights = s$weight)) # 0.04*  
summary(lm(taxe_efficace=='Oui' ~ apres_modifs, data=s, weights = s$weight)) # 0.04** 
summary(lm(taxe_efficace=='NSP' ~ apres_modifs, data=s, weights = s$weight)) # 0
summary(lm(problemes_inefficace==T ~ apres_modifs, data=s, weights = s$weight)) # -0.03 p-value: 0.1
summary(lm(benefices_sante==T ~ apres_modifs, data=s, weights = s$weight)) # 0
summary(lm(benefices_circulation==T ~ apres_modifs, data=s, weights = s$weight)) # 0

# Pas d'effet de l'ancrage : "En moyenne, cette mesure augmenterait le pouvoir d'achat des ménages les plus modestes, et diminuerait celui des plus riches, qui consomment plus d'énergie."
decrit(s$progressivite, weights = s$weight)
summary(lm(progressivite!='Non' ~ info_progressivite, data=s, weights = s$weight)) # 0
summary(lm(progressivite!='Non' ~ info_progressivite + info_CC * info_PM, data=s, weights = s$weight))
summary(lm((progressivite!='Non') ~ info_progressivite + (taxe_approbation != 'Non') + (gagnant_feedback_categorie!='Perdant') + (taxe_efficace!='Non'), data=s, weights = s$weight, na.action="na.exclude"))
summary(lm(progressivite=='Oui' ~ info_progressivite, data=s, weights = s$weight)) # 0
summary(lm(progressivite=='NSP' ~ info_progressivite, data=s, weights = s$weight)) # 0
summary(lm(problemes_pauvres==T ~ info_progressivite, data=s, weights = s$weight)) # 0
summary(lm(problemes_revenu==T ~ info_progressivite, data=s, weights = s$weight)) # 0
summary(lm(problemes_ruraux==T ~ info_progressivite, data=s, weights = s$weight)) # 0
summary(lm(benefices_pauvres==T ~ info_progressivite, data=s, weights = s$weight)) # 0
summary(lm(benefices_revenu==T ~ info_progressivite, data=s, weights = s$weight)) # 0


##### 5.1 Self-Interest and Acceptance #####
# To pool all RDD together, it is better to control for the reform, i.e. for cible. 
# Otherwise the effect of being treated might be confounded with the effect of a more targeted reform.
# TODO: graphs
# Main identification strategy
tsls1_si1 <- lm(gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + I(traite_cible*traite_cible_conjoint) + cible + Revenu + I(Revenu^2) + Revenu_conjoint + I(Revenu_conjoint^2), data=s, weights = s$weight)
summary(tsls1_si1)
s$non_perdant <- tsls1_si1$fitted.values
# 50 p.p.***
tsls2_si1 <- lm(taxe_cible_approbation!='Non' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2, data=s, weights = s$weight)
summary(tsls2_si1)

# Alternative specifications for robustness checks
# 'bis' try to reproduce Thomas' specifications
# (2) With controls: taxe_approbation + Simule_gain_cible + taxe_efficace
tsls1_si2 <- lm(gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + I(traite_cible*traite_cible_conjoint) + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taxe_approbation + Simule_gain_cible + taxe_efficace, data=s, weights = s$weight)
summary(tsls1_si2)
s$non_perdant <- tsls1_si2$fitted.values
# 52 p.p.*** 
tsls2_si2 <- lm(taxe_cible_approbation!='Non' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + I(Revenu_conjoint^2) + taxe_approbation + Simule_gain_cible + taxe_efficace, data=s, weights = s$weight)
summary(tsls2_si2)

# (2bis) With controls: (taxe_approbation!='Non') + Simule_gain_cible + (taxe_efficace=='Oui') (same result for taxe_efficace!='Non')
tsls1_si2bis <- lm(gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + I(traite_cible*traite_cible_conjoint) + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + (taxe_approbation!='Non') + Simule_gain_cible + (taxe_efficace=='Oui'), data=s, weights = s$weight)
summary(tsls1_si2bis)
s$non_perdant <- tsls1_si2bis$fitted.values
# 52 p.p.*** 
tsls2_si2bis <- lm(taxe_cible_approbation!='Non' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + (taxe_approbation!='Non') + Simule_gain_cible + (taxe_efficace=='Oui'), data=s, weights = s$weight)
summary(tsls2_si2bis)

# (2ter) With many controls
formula_tsls1_si2ter <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + I(traite_cible*traite_cible_conjoint) + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taxe_approbation + Simule_gain_cible + taxe_efficace +", 
                                         paste(c(variables_demo, variables_politiques), collapse = ' + ')))
tsls1_si2ter <- lm(formula_tsls1_si2ter, data=s, weights = s$weight)
summary(tsls1_si2ter)
s$non_perdant <- tsls1_si2ter$fitted.values
# 52 p.p.*** 
formula_tsls2_si2ter <- as.formula(paste("taxe_cible_approbation!='Non' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taxe_approbation + Simule_gain_cible + taxe_efficace +", 
                                         paste(c(variables_demo, variables_politiques), collapse = ' + ')))
tsls2_si2ter <- lm(formula_tsls2_si2ter, data=s, weights = s$weight)
summary(tsls2_si2ter)

# (3) Simple OLS (same results and same distinction as before for 'bis' or not)
s$non_perdant <- n(s$gagnant_cible_categorie!='Perdant')
tsls1_si3 <- lm(taxe_cible_approbation!='Non' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taxe_approbation + Simule_gain_cible + taxe_efficace, data=s, weights = s$weight)
summary(tsls1_si3)
# 45 p.p.*** 
tsls2_si3bis <- lm(taxe_cible_approbation!='Non' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + (taxe_approbation!='Non') + Simule_gain_cible + (taxe_efficace=='Oui'), data=s, weights = s$weight)
summary(tsls2_si3bis)

# (4) Simple Probit
# 53 p.p.***
s$non_perdant <- s$gagnant_cible_categorie!='Perdant'
# Warning when weighting: it relates to number of trials and not to survey weights. TODO: use svyglm to weight correctly cf. https://stats.stackexchange.com/questions/57107/use-of-weights-in-svyglm-vs-glm
probit_si4 <- glm(taxe_cible_approbation!='Non' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + tax_acceptance, family = binomial(link='probit'), data=ss)
summary(probit_si4)
probit_si4_margins <- summary(margins(data=s, model=probit_si4))
probit_si4_margins
probitmfx(taxe_cible_approbation!='Non' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + tax_acceptance, data=s, atmean =FALSE)
probit_si4_margins <- probitmfx(taxe_cible_approbation!='Non' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + tax_acceptance, data=s, atmean = FALSE)
probit_si4_margins
probit_si4_margins$mfxest[1,1]
probit_si4_margins$mfxest[1,2]
probit_si4_margins$mfxest[,1]
probit_si4_margins$mfxest

test <- glm(tax_acceptance ~traite_cible, data=s, family = binomial(link='probit'))
summary(margins(data=s, model=test))
probitmfx(tax_acceptance ~ traite_cible, data=ss)
probitmfx(tax_acceptance ~ traite_cible, data=s, atmean=FALSE)

# # (4 deprecated) Probit for second stage
# # tsls1_si4 <- lm(gagnant_cible_categorie!='Perdant' ~ traite_cible * traite_cible_conjoint + cible + Revenu + I(Revenu^2) + Revenu_conjoint + I(Revenu_conjoint^2), data=s, weights = s$weight)
# # summary(tsls1_si4)
# # non_perdant.hat4 <- tsls1_si1$fitted.values
# # 53 p.p. *** problème mfx: no weighting
# tsls2_si4 <- probitmfx(taxe_cible_approbation!='Non' ~ non_perdant.hat1 + cible + Revenu + I(Revenu^2) + Revenu_conjoint + I(Revenu_conjoint^2), data=s, atmean = TRUE)
# tsls2_si4
# 
# s$non_perdant.hat1 <- non_perdant.hat1
# # 50 p.p.*** probit avec weighting:
# tsls2_si4bis <- glm(taxe_cible_approbation!='Non' ~ non_perdant.hat1 + cible + Revenu + I(Revenu^2) + Revenu_conjoint + I(Revenu_conjoint^2), family = binomial(link = 'probit'), data=s, weights = s$weight)
# summary(margins(data=s, model=tsls2_si4bis))
# probitmfx(taxe_feedback_approbation!='Non' ~ (gagnant_feedback_categorie!='Perdant') + Revenu + I(Revenu^2) + Revenu_conjoint + I(Revenu_conjoint^2), data=s, atmean = TRUE)

# # (5) Biprobit
# tsls1_si5 <- glm(gagnant_cible_categorie!='Perdant' ~ traite_cible * traite_cible_conjoint + cible + Revenu + I(Revenu^2) + Revenu_conjoint + I(Revenu_conjoint^2), data=s, family = binomial(link = 'probit'), weights = s$weight)
# summary(tsls1_si5)
# s$non_perdant.hat5 <- tsls1_si5$fitted.values
# # 51 p.p.*** 
# tsls2_si5 <- glm(taxe_cible_approbation!='Non' ~ non_perdant.hat5 + cible + Revenu + I(Revenu^2) + Revenu_conjoint + I(Revenu_conjoint^2), family = binomial(link = 'probit'), data=s, weights = s$weight)
# summary(margins(data=s, model=tsls2_si5))

# (6) IV Feedback
tsls1_si6 <- lm(gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taxe_approbation + Simule_gain + Simule_gain2 + taxe_efficace, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_si6)
s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_si6$fitted.values
# 43 p.p. ***
tsls2_si6 <- lm(taxe_feedback_approbation!='Non' ~ non_perdant + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taxe_approbation + Simule_gain + Simule_gain2 + taxe_efficace, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
summary(tsls2_si6)

# Controles binaires au lieu de ternaires, 
tsls1_si6bis <- lm(gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + Revenu + Revenu2 + (taxe_approbation!='Non') + Simule_gain + Simule_gain2 + (taxe_efficace=='Oui'), data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_si6bis)
s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_si6bis$fitted.values
# 44 p.p. ***
tsls2_si6bis <- lm(taxe_feedback_approbation!='Non' ~ non_perdant + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + (taxe_approbation!='Non') + Simule_gain + Simule_gain2 + (taxe_efficace=='Oui'), data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
summary(tsls2_si6bis)

# Results
# à la Thomas
TableV <- stargazer(tsls2_si1, tsls2_si2bis, tsls2_si3bis, probit_si4, tsls2_si6bis, # tsls2_si4: Unrecognized object type
                    title="Effect of self-interest on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                    covariate.labels = c("Constant", "Believes not to lose ($\\widehat{G^C},\\,\\widehat{G^C},\\,G^C,\\,\\widehat{G^F}$)", "Income ($I$, k\\euro{}/m)", "Income$^2$ ($I^2$)", "Spouse income ($I_2$, k\\euro{}/m)", "Spouse income$^2$ ($I_2^2$)",
                                         "Initial tax Acceptance ($A^I$)", "Estimated gain for targeted tax ($\\widehat{\\gamma^T}$)", "Estimated gain ($\\widehat{\\gamma}$)", "Estimated gain$^2$ ($\\widehat{\\gamma^2}$)",
                                         "Environmentally effective: `Yes'"),
                    dep.var.labels = c("Targeted Acceptance ($A^T$)", "Feedback Acceptance ($A^F$)"), dep.var.caption = "", header = FALSE,
                    coef = list(NULL, NULL, NULL, probit_si4_margins$AME, NULL),
                    se = list(NULL, NULL, NULL, probit_si4_margins$SE %>% set_names(probit_si4_margins$factor), NULL),
                    omit = "cible.+",
                    # add.lines = c("Method: 2SLS & \\checkmark & \\checkmark & \\checkmark &  & \\checkmark"),
                    no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="results_private_benefits")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableV, fixed=TRUE), fixed=TRUE), collapse=' ')

# à la Adrien
TableV <- stargazer(tsls2_si1, tsls2_si2bis, tsls2_si3bis, probit_si4, tsls2_si6bis, # tsls2_si4: Unrecognized object type
                    title="Effect of self-interest on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                    covariate.labels = c("Constant", "Believes not to lose ($\\widehat{G^C},\\,\\widehat{G^C},\\,G^C,\\,\\widehat{G^F}$)",
                                         "Initial tax Acceptance ($A^I$)",
                                         # "Estimated gain for targeted tax ($\\widehat{\\gamma^T}$)", "Estimated gain ($\\widehat{\\gamma}$)", "Estimated gain$^2$ ($\\widehat{\\gamma^2}$)",
                                         "Environmentally effective: `Yes'"),
                    dep.var.labels = c("Targeted Acceptance ($A^T$)", "Feedback Acceptance ($A^F$)"), dep.var.caption = "", header = FALSE,
                    omit = c("cible.+", "Revenu", "gain"),
                    coef = list(NULL, NULL, NULL, probit_si4_margins$AME, NULL), 
                    se = list(NULL, NULL, NULL, probit_si4_margins$SE %>% set_names(probit_si4_margins$factor), NULL),
                    add.lines = c(
                      # "Method: 2SLS & \\checkmark & \\checkmark &  & \\checkmark",
                      "Controls: Incomes & \\checkmark & \\checkmark & \\checkmark  & \\checkmark & \\checkmark",
                      "Controls: Estimated gain & & \\checkmark & \\checkmark &  & \\checkmark",
                      "Controls: Target of the tax & \\checkmark & \\checkmark & \\checkmark & \\checkmark  & "),
                    no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="results_private_benefits")
write_clip(sub("\\multicolumn{3}{c}{\\textit{OLS}} & \\textit{probit} & \\textit{OLS}", "\\multicolumn{2}{c}{\\textit{IV}} & \\textit{OLS} & \\textit{probit} & \\textit{IV}", 
               gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableV, fixed=TRUE), fixed=TRUE),
               fixed=T), collapse=' ')

# Results First Stage
# à la Thomas
TableX <- stargazer(tsls1_si1, tsls1_si2bis, tsls1_si6bis,
                    title="First stage regressions results for self-interest", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                    covariate.labels = c("Constant", "Transfer to respondent ($T_1$)", "Transfer to spouse ($T_2$)",
                                         "$T_1 \\times T_2$", "Simulated winner ($\\widehat{\\Gamma}$)", "Income ($I$)", "Income$^2$ ($I^2$)", "Spouse income ($I_2$)", "Spouse income$^2$ ($I_2^2$)",
                                         "Initial tax Acceptance ($A^I$)", "Estimated gain for targeted tax ($\\widehat{\\gamma^T}$)", "Estimated gain ($\\widehat{\\gamma}$)", "Estimated gain$^2$ ($\\widehat{\\gamma^2}$)",
                                         "Environmentally effective: `Yes'"),
                    dep.var.labels = c("Targeted tax ($G^T$)", "After feedback ($G^F$)"), dep.var.caption = "Believes not to lose", header = FALSE,
                    omit = "^cible.+",
                    add.lines = c("Controls: Target of the tax & \\checkmark & \\checkmark & "),
                    no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="first_stage_private_benefits")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableX, fixed=TRUE), fixed=TRUE), collapse=' ')

# à la Adrien
TableX <- stargazer(tsls1_si1, tsls1_si2bis, tsls1_si6bis,
                    title="First stage regressions results for self-interest", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                    covariate.labels = c("Constant", "Transfer to respondent ($T_1$)", "Transfer to spouse ($T_2$)",
                                         "$T_1 \\times T_2$", "Simulated winner ($\\widehat{\\Gamma}$)",
                                         # "Income ($I$)", "Income$^2$ ($I^2$)", "Spouse income ($I_2$)", "Spouse income$^2$ ($I_2^2$)",
                                         "Initial tax Acceptance ($A^I$)",
                                         # "Estimated gain for targeted tax ($\\widehat{\\gamma^T}$)", "Estimated gain ($\\widehat{\\gamma}$)", "Estimated gain$^2$ ($\\widehat{\\gamma^2}$)",
                                         "Environmentally effective: `Yes'"),
                    dep.var.labels = c("Targeted tax ($G^T$)", "After feedback ($G^F$)"), dep.var.caption = "Believes not to lose", header = FALSE,
                    omit = c("^cible.+", "Revenu", "gain"),
                    add.lines = c("Controls: Incomes & \\checkmark & \\checkmark & \\checkmark",
                                  "Controls: Estimated gain & & \\checkmark  & \\checkmark",
                                  "Controls: Target of the tax & \\checkmark & \\checkmark & "),
                    no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="first_stage_private_benefits")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableX, fixed=TRUE), fixed=TRUE), collapse=' ')

# Différence entre IV et la stat desc: l'IV (0.52) mesure l'effet de se savoir gagnant par rapport à se savoir perdant. 
# La stat desc (0.39) mesure l'effet de se savoir gagnant par rapport à se croire perdant mais sans trop savoir.
ssi5 <- lm(gagnant_feedback_categorie!='Perdant' ~ simule_gagnant, data=s, subset=variante_taxe_info=='f')
summary(ssi5)
s$temp[s$variante_taxe_info=='f'] <- round(ssi5$fitted.values, 4)
summary(lm(taxe_feedback_approbation!='Non' ~ temp, data=s, subset=variante_taxe_info=='f')) # 0.52***

decrit(s$taxe_approbation, miss=T) # 0.7 - 0.41 = 0.39
decrit(s$taxe_approbation[s$gagnant_categorie=="Perdant" & s$gagnant_info_categorie!="Perdant" & s$simule_gagnant==1 & s$variante_taxe_info=='f'], miss=T)
decrit(s$taxe_feedback_approbation[s$gagnant_categorie=="Perdant" & s$gagnant_info_categorie!="Perdant" & s$simule_gagnant==1], miss=T)
# 0.54 - 0.69 = -0.15
decrit(s$taxe_approbation[s$gagnant_categorie!="Perdant" & s$gagnant_info_categorie=="Perdant" & s$simule_gagnant==0 & s$variante_taxe_info=='f'], miss=T)
decrit(s$taxe_feedback_approbation[s$gagnant_categorie!="Perdant" & s$gagnant_info_categorie=="Perdant" & s$simule_gagnant==0], miss=T)

decrit(s$taxe_approbation[s$gagnant_categorie!="Perdant"], miss=T) # 0.85 - 0.45 = 0.4
decrit(s$taxe_approbation[s$gagnant_categorie=="Perdant"], miss=T)
decrit(s$taxe_feedback_approbation[s$gagnant_info_categorie!="Perdant"], miss=T) # 0.87 - 0.33 = 0.54: plus forte différence quand la croyance vague 
decrit(s$taxe_feedback_approbation[s$gagnant_info_categorie=="Perdant"], miss=T) # est devenue certitude


##### 5.2 Environmental effectiveness #####
s$non_perdant <- n(s$gagnant_categorie!='Perdant')
# Main identification strategy
# (1) 2SLS both instruments: 60 p.p.*
tsls1_ee1 <- lm((taxe_efficace=='Oui') ~ info_CC + info_PM + I((info_CC==1)*(info_PM==1)) + apres_modifs + non_perdant + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taille_agglo + Simule_gain + Simule_gain2, data=s, weights=s$weight)
summary(tsls1_ee1)
s$taxe_efficace.hat <- tsls1_ee1$fitted.values
tsls2_ee1 <- lm(tax_acceptance ~ taxe_efficace.hat + non_perdant + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taille_agglo + Simule_gain + Simule_gain2, data=s, weights=s$weight)
summary(tsls2_ee1)
summary(lm(tax_acceptance ~ info_CC * info_PM, data=s, weights=s$weight)) # info_CC is a good instrument

# (1bis) change to approval and tax not ineffective, remove controls because they are not needed
tsls1_ee1bis <- lm((taxe_efficace!='Non') ~ apres_modifs + I(info_CC==1 | info_PM==1) + I(info_PM==0 & info_CC==1) + I(info_PM==1 & info_CC==0), data=s, weights=s$weight)
summary(tsls1_ee1bis)
s$taxe_inefficace.hat <- tsls1_ee1bis$fitted.values
# 42 p.p. *
tsls2_ee1bis <- lm(tax_approval ~ taxe_inefficace.hat, data=s, weights=s$weight)
summary(tsls2_ee1bis)

# # (1ter) change the interaction terms to show that the instrument is not weak, remove controls because they are not needed (except non_perdant for significance)
# # 64 p.p. * (56 p.p. ., p-value: 0.07 without non_perdant)
# tsls1_ee1ter <- lm((taxe_efficace=='Oui') ~ I(info_CC==1 | info_PM==1) + I(info_PM==0 & info_CC==1) + I(info_PM==1 & info_CC==0) + apres_modifs + non_perdant, data=s, weights=s$weight)
# summary(tsls1_ee1ter)
# s$taxe_efficace.hat <- tsls1_ee1ter$fitted.values
# tsls2_ee1ter <- lm(tax_acceptance ~ taxe_efficace.hat + non_perdant, data=s, weights=s$weight)
# summary(tsls2_ee1ter)

# (2) 2SLS info_CC*info_PM: 141 p.p. **
tsls1_ee2 <- lm((taxe_efficace=='Oui') ~ info_CC + info_PM + I((info_CC==1)*(info_PM==1)) + non_perdant + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taille_agglo + Simule_gain + Simule_gain2, data=s, weights=s$weight)
summary(tsls1_ee2)
s$taxe_efficace.hat <- tsls1_ee2$fitted.values
tsls2_ee2 <- lm(tax_acceptance ~ taxe_efficace.hat + non_perdant + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taille_agglo + Simule_gain + Simule_gain2, data=s, weights=s$weight)
summary(tsls2_ee2)

# (2bis) 57 p.p. . change to approval and tax not ineffective, remove controls
tsls1_ee2bis <- lm((taxe_efficace!='Non') ~ info_CC + info_PM + I((info_CC==1)*(info_PM==1)), data=s, weights=s$weight)
summary(tsls1_ee2bis)
s$taxe_inefficace.hat <- tsls1_ee2bis$fitted.values
tsls2_ee2bis <- lm(tax_approval ~ taxe_inefficace.hat, data=s, weights=s$weight)
summary(tsls2_ee2bis)

# (3) 2SLS info_ee (apres_modifs): 8 p.p. **
tsls1_ee3 <- lm((taxe_efficace=='Oui') ~ apres_modifs + non_perdant + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taille_agglo + Simule_gain + Simule_gain2, data=s, weights=s$weight)
summary(tsls1_ee3)
s$taxe_efficace.hat <- tsls1_ee3$fitted.values
tsls2_ee3 <- lm(tax_acceptance ~ taxe_efficace.hat + non_perdant + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taille_agglo + Simule_gain + Simule_gain2, data=s, weights=s$weight)
summary(tsls2_ee3)

# (3bis) 31 p.p. change to approval and tax not ineffective, remove controls
tsls1_ee3bis <- lm((taxe_efficace!='Non') ~ apres_modifs, data=s, weights=s$weight)
summary(tsls1_ee3bis)
s$taxe_inefficace.hat <- tsls1_ee3bis$fitted.values
tsls2_ee3bis <- lm(tax_approval ~ taxe_inefficace.hat, data=s, weights=s$weight)
summary(tsls2_ee3bis)

# Alternative specifications for robustness checks
# (4) OLS simple: 38 p.p. **
s$taxe_efficace.hat <- n(s$taxe_efficace=='Oui')
ols_ee4 <- lm(tax_acceptance ~ taxe_efficace.hat + non_perdant + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taille_agglo + Simule_gain + Simule_gain2, data=s, weights=s$weight)
summary(ols_ee4)

# (4bis) 20 p.p. *** change to approval (this is what reduces coef) and tax not ineffective, complete controls
s$taxe_inefficace.hat <- n(s$taxe_efficace!='Non')
formula_ols_ee4bis <- as.formula(paste("tax_approval ~ taxe_inefficace.hat +", paste(c(variables_demo, variables_politiques, variables_energie), collapse = ' + ')))
ols_ee4bis <- lm(formula_ols_ee4bis, data=s, weights=s$weight)
# ols_ee4 <- lm(as.formula(paste("tax_acceptance ~ taxe_efficace.hat +", paste(c(variables_demo, variables_politiques, variables_energie), collapse = ' + '))), data=s, weights=s$weight)
# ols_ee4 <- lm(tax_approval ~ taxe_efficace.hat + non_perdant + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taille_agglo + Simule_gain + Simule_gain2, data=s, weights=s$weight)
summary(ols_ee4bis)

# (5) probit: 29 p.p. ** change to approval (this is what reduces coef) and tax not ineffective, complete controls
s$taxe_efficace.hat <- n(s$taxe_efficace=='Oui')
probit_ee5 <- glm(tax_acceptance ~ taxe_efficace.hat + non_perdant + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taille_agglo + Simule_gain + Simule_gain2, binomial(link='probit'), data=ss)
summary(probit_ee5)
probit_ee5_margins <- summary(margins(data=s, model=probit_ee5))
probit_ee5_margins

# (5bis) 20 p.p. *** change to approval (this is what reduces coef) and tax not ineffective, complete controls
s$taxe_inefficace.hat <- n(s$taxe_efficace!='Non')
variables_socio_demo <- c(variables_demo, variables_politiques, variables_energie)
# probit_ee5bis <- glm(formula_ols_ee4bis, binomial(link='probit'), data=ss)
formula_probit_ee5bis <- as.formula(paste("tax_approval ~ taxe_inefficace.hat +", paste(variables_socio_demo[!(variables_socio_demo %in% 
                                                                                                                 c("age_50_64", "age_65_plus", "gaz", "fioul", "hausse_chauffage", "hausse_depenses", "simule_gain"))], collapse = ' + ')))
probit_ee5bis <- glm(formula_probit_ee5bis, binomial(link='probit'), data=ss)
summary(probit_ee5bis)
probit_ee5bis_margins <- summary(margins(data=s, model=probit_ee5bis, variable = "taxe_inefficace.hat")) # 
probit_ee5bis_margins

# (6) biprobit: TODO

# (4ter) TSLS Approval ~ efficace == Oui
otsls1_ee1bis <- lm((taxe_efficace=='Oui') ~ apres_modifs + I(info_CC==1 | info_PM==1) + I(info_PM==0 & info_CC==1) + I(info_PM==1 & info_CC==0), data=s, weights=s$weight)
summary(otsls1_ee1bis)
s$taxe_efficace.hat <- otsls1_ee1bis$fitted.values
otsls2_ee1bis <- lm(tax_approval ~ taxe_efficace.hat, data=s, weights=s$weight)
summary(otsls2_ee1bis)

# (5ter) TSLS Acceptance ~ efficace != Non
atsls1_ee1bis <- lm((taxe_efficace!='Non') ~ apres_modifs + I(info_CC==1 | info_PM==1) + I(info_PM==0 & info_CC==1) + I(info_PM==1 & info_CC==0), data=s, weights=s$weight)
summary(atsls1_ee1bis)
s$taxe_inefficace.hat <- atsls1_ee1bis$fitted.values
atsls2_ee1bis <- lm(tax_acceptance ~ taxe_inefficace.hat, data=s, weights=s$weight)
summary(atsls2_ee1bis)

# (6ter) TSLS Acceptance ~ efficace == Oui. Adding non_perdant as control entail 5% significance
aotsls1_ee1bis <- lm((taxe_efficace=='Oui') ~ apres_modifs + I(info_CC==1 | info_PM==1) + I(info_PM==0 & info_CC==1) + I(info_PM==1 & info_CC==0), data=s, weights=s$weight)
summary(aotsls1_ee1bis)
s$taxe_efficace.hat <- aotsls1_ee1bis$fitted.values
aotsls2_ee1bis <- lm(tax_acceptance ~ taxe_efficace.hat, data=s, weights=s$weight)
summary(aotsls2_ee1bis)

# Results
# à la Thomas
table_ee <- stargazer(tsls2_ee1, tsls2_ee2, tsls2_ee3, ols_ee4, probit_ee5, 
                      title="Effect of believing in environmental effectiveness on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                      covariate.labels = c("Constant", "Environmental effectiveness: `Yes'", "Believes not to lose ($G$)",
                                           "Income ($I$)", "Income$^2$ ($I^2$)", "Spouse income ($I_2$)", "Spouse income$^2$ ($I_2^2$)",
                                           "Size of town", "Estimated gain ($\\widehat{\\gamma}$)", "Estimated gain$^2$ ($\\widehat{\\gamma^2}$)"),
                      dep.var.labels = "Tax Acceptance ($A^I$)", dep.var.caption = "", header = FALSE,
                      # keep = c("taxe_efficace.hat"), # "Constant", 
                      coef = list(NULL, NULL, NULL, NULL, probit_ee5_margins$AME), 
                      se = list(NULL, NULL, NULL, NULL, probit_ee5_margins$SE %>% set_names(probit_ee5_margins$factor)),
                      add.lines = c( # "Method: Probit &  &  &  & \\checkmark & \\checkmark & \\checkmark",
                        "Instrument: info C.C. \\& P.M. & \\checkmark & \\checkmark &  &  &",
                        "Instrument: info E.E. & \\checkmark &  & \\checkmark &  & "), #  (energy and transport characteristics, attitudes on policies and climate change)
                      no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="tab:results_environmental_effectivenes")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', table_ee, fixed=TRUE), fixed=TRUE), collapse=' ')

# Thomas (specification) à la Adrien (presentation)
table_ee <- stargazer(tsls2_ee1, tsls2_ee2, tsls2_ee3, ols_ee4, probit_ee5, 
                      title="Effect of believing in environmental effectiveness on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                      covariate.labels = c("Environmental effectiveness: `Yes'"), # "Constant", 
                      dep.var.labels = "Tax Acceptance ($A^I$)", dep.var.caption = "", header = FALSE,
                      keep = c("taxe_efficace.hat"), # "Constant", 
                      coef = list(NULL, NULL, NULL, NULL, probit_ee5_margins$AME), 
                      se = list(NULL, NULL, NULL, NULL, probit_ee5_margins$SE %>% set_names(probit_ee5_margins$factor)),
                      add.lines = c( # "Method: Probit &  &  &  & \\checkmark & \\checkmark & \\checkmark",
                        "Instrument: info C.C. \\& P.M. & \\checkmark & \\checkmark &  &  &",
                        "Instrument: info E.E. & \\checkmark &  & \\checkmark &  & ",
                        "Controls & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark"), #  (energy and transport characteristics, attitudes on policies and climate change)
                      no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="tab:results_environmental_effectivenes")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', table_ee, fixed=TRUE), fixed=TRUE), collapse=' ')

# à la Adrien (specifications bis)
table_ee <- stargazer(tsls2_ee1bis, tsls2_ee2bis, tsls2_ee3bis, ols_ee4bis, probit_ee5bis, 
                      title="Effect of believing in environmental effectiveness on approval", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                      covariate.labels = c("Environmental effectiveness: not `No'"), # "Constant",
                      dep.var.labels = "Tax Approval ($\\dot{A^I}$)", dep.var.caption = "", header = FALSE,
                      keep = c("taxe_inefficace.hat"), # "Constant", 
                      coef = list(NULL, NULL, NULL, NULL, probit_ee5bis_margins$AME), 
                      se = list(NULL, NULL, NULL, NULL, probit_ee5bis_margins$SE %>% set_names(probit_ee5bis_margins$factor)),
                      add.lines = c( # "Method: Probit &  &  &  & \\checkmark & \\checkmark & \\checkmark",
                        "Instrument: info C.C. \\& P.M. & \\checkmark & \\checkmark &  &  &",
                        "Instrument: info E.E. & \\checkmark &  & \\checkmark &  & ",
                        "Controls: Socio-demo, energy, politics & & & & \\checkmark & \\checkmark"), #  (energy and transport characteristics, attitudes on policies and climate change)
                      no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="tab:ee")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', table_ee, fixed=TRUE), fixed=TRUE), collapse=' ')

# à la ouakbar. Adding non_perdant as a control in the three last would add them a *
table_ee <- stargazer(tsls2_ee1bis, ols_ee4bis, probit_ee5bis, otsls2_ee1bis, atsls2_ee1bis, aotsls2_ee1bis,
                      title="Effect of believing in environmental effectiveness on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                      covariate.labels = c("Environmental effectiveness: not `No'", "Environmental effectiveness: `Yes'"), # "Constant",
                      dep.var.labels = c("Tax Approval ($\\dot{A^I}$)", "Tax Acceptance ($A^I$)"), dep.var.caption = "", header = FALSE,
                      keep = c("efficace.hat"), # "Constant",
                      coef = list(NULL, NULL, probit_ee5bis_margins$AME, NULL, NULL, NULL), 
                      se = list(NULL, NULL, probit_ee5bis_margins$SE %>% set_names(probit_ee5bis_margins$factor), NULL, NULL, NULL),
                      add.lines = c( # "Method: Probit &  &  &  & \\checkmark & \\checkmark & \\checkmark",
                        "Instruments: info E.E., C.C. \\& P.M. & \\checkmark & & & \\checkmark & \\checkmark & \\checkmark",
                        "Controls: Socio-demo, energy, politics & & \\checkmark & \\checkmark  & & &"), #  (energy and transport characteristics, attitudes on policies and climate change)
                      no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="tab:ee")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', table_ee, fixed=TRUE), fixed=TRUE), collapse=' ')

# First Stage Results
# à la Thomas
table_ee <- stargazer(tsls1_ee1, tsls1_ee2, tsls1_ee3,
                      title="First stage regressions results for environmental effectiveness", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                      covariate.labels = c("Constant", "Info on Climate Change ($Z_{CC}$)", "Info on Particulates ($Z_{PM}$)", "$Z_{CC} \\times Z_{PM}$", "Info on Environmental Effectiveness ($Z_{EE}$)",
                                           "Believes not to lose ($G$)", "Income ($I$)", "Income$^2$ ($I^2$)", "Spouse income ($I_2$)", "Spouse income$^2$ ($I_2^2$)",
                                           "Size of town", "Estimated gain ($\\widehat{\\gamma}$)", "Estimated gain$^2$ ($\\widehat{\\gamma^2}$)"),
                      dep.var.labels = "Environmental effectiveness: `Yes'", dep.var.caption = "", header = FALSE,
                      # keep = c("taxe_efficace.hat"), # "Constant", 
                      coef = list(NULL, NULL, NULL, NULL, probit_ee5_margins$AME), 
                      se = list(NULL, NULL, NULL, NULL, probit_ee5_margins$SE %>% set_names(probit_ee5_margins$factor)),
                      # add.lines = c(), #  (energy and transport characteristics, attitudes on policies and climate change)
                      no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="first_stage_environmental_effectiveness")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', table_ee, fixed=TRUE), fixed=TRUE), collapse=' ')

# à la Adrien
table_ee <- stargazer(tsls1_ee1, tsls1_ee2, tsls1_ee3,
                      title="First stage regressions results for environmental effectiveness", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                      covariate.labels = c("Constant", "Info on Climate Change ($Z_{CC}$)", "Info on Particulates ($Z_{PM}$)", "$Z_{CC} \\times Z_{PM}$", "Info on Environmental Effectiveness ($Z_{EE}$)"),
                      dep.var.labels = "Environmental effectiveness: `Yes'", dep.var.caption = "", header = FALSE,
                      keep = c("Constant", "info", "apres_modifs"), # 
                      coef = list(NULL, NULL, NULL, NULL, probit_ee5_margins$AME), 
                      se = list(NULL, NULL, NULL, NULL, probit_ee5_margins$SE %>% set_names(probit_ee5_margins$factor)),
                      add.lines = c("Controls & \\checkmark & \\checkmark & \\checkmark"), #  (energy and transport characteristics, attitudes on policies and climate change)
                      no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="first_stage_environmental_effectiveness")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', table_ee, fixed=TRUE), fixed=TRUE), collapse=' ')

# à la ouakbar
table_ee <- stargazer(tsls1_ee1bis, otsls1_ee1bis, #atsls1_ee1bis, aotsls1_ee1bis,
                      title="First stage regressions results for environmental effectiveness", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                      covariate.labels = c("Constant", "Info on Environmental Effectiveness ($Z_{EE}$)", "Info on Climate Change or on Particulates ($Z_{CC} \\; | \\; Z_{PM}$)", "Info on Climate Change only ($Z_{CC} \\; \\& \\; \\neg Z_{PM}$)", "Info on Particulates only ($Z_{PM} \\; \\& \\; \\neg Z_{CC}$)"),
                      dep.var.labels = c("not `No'", "`Yes'"), dep.var.caption = "Environmental effectiveness", header = FALSE,
                      keep = c("Constant", "info", "apres_modifs"), 
                      column.labels = c("(1, 5)", "(4, 6)"), model.numbers = FALSE,
                      # add.lines = c("Controls &  &  &  &"), #  (energy and transport characteristics, attitudes on policies and climate change)
                      no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="first_stage_environmental_effectiveness")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', table_ee, fixed=TRUE), fixed=TRUE), collapse=' ')


##### 5.3 Progressivity #####
# IV doesn't work because instrument is weak:
prog1 <- lm((progressivite!='Non') ~ info_progressivite, weights=s$weight, data=s, na.action='na.exclude')
summary(prog1)
summary(lm(tax_acceptance ~ fitted.values(prog1), weights=s$weight, data=ss))

s$progressif <- n(s$progressivite=='Oui') 
# (1) OLS simple: 22 p.p.***
ols_prog0 <- lm(tax_acceptance ~ progressif, weights=s$weight, data=ss)
summary(ols_prog0)

# (2) OLS: 20 p.p.***
formula_ols_prog1 <- as.formula(paste("tax_acceptance ~ progressif + ", paste(variables_demo, collapse=' + ')))
ols_prog1 <- lm(formula_ols_prog1, weights=s$weight, data=ss)
summary(ols_prog1)

# (3) OLS tous contrôles: 12 p.p.***
variables_exogenes <- c(variables_qualite, variables_demo, variables_transport, variables_politiques, variables_energie,
                        variables_Elasticite, variables_elasticite, variables_gilets_jaunes, 
                        variables_politiques_environnementales, variables_connaissances_CC, variables_avis_CC)
formula_ols_prog2 <- as.formula(paste("tax_acceptance ~ progressif + ", paste(variables_exogenes, collapse=' + ')))
ols_prog2 <- lm(formula_ols_prog2, weights=s$weight, data=ss)
summary(ols_prog2)

# (4) probit: 21 p.p.***
logit_prog0 <- glm(tax_acceptance ~ progressif, binomial(link="logit"), data=ss)
summary(logit_prog0)
logit_prog0_margins <- summary(margins(data=s, model=logit_prog0, variable="progressif")) # 
logit_prog0_margins

# (5) probit: 19 p.p.***
formula_probit_prog1 <- as.formula(paste("tax_acceptance ~ progressif + ", paste(variables_demo[1:(length(variables_demo)-2)], collapse=' + ')))
probit_prog1 <- glm(formula_probit_prog1, binomial(link="probit"), data=s[s$region!='autre',])
summary(probit_prog1)
probit_prog1_margins <- summary(margins(data=s[s$region!='autre',], model=probit_prog1, variable="progressif")) # 
probit_prog1_margins

# (6) probit tous contrôles: 10 p.p.***
variables_exogenes <- c(variables_qualite, variables_demo, variables_transport, variables_politiques, variables_energie,
                        variables_Elasticite, variables_elasticite, variables_gilets_jaunes,
                        variables_politiques_environnementales, variables_connaissances_CC) #, variables_avis_CC)
formula_probit_prog2 <- as.formula(paste("tax_acceptance ~ progressif + ", paste(variables_exogenes[!(variables_exogenes %in% 
                                                                                                        c("age_50_64", "age_65_plus", "Transports_travail_commun", "Transports_travail_actif", "gaz", "fioul", "hausse_chauffage", "hausse_depenses", 
                                                                                                          "Simule_gain", "simule_gain", "elasticite_partielle", "elasticite_partielle_perso", "score_ges", "score_climate_call", "enfant_CC_pour_lui", "enfant_CC_pour_CC"))], 
                                                                                 collapse=' + ')))
probit_prog2 <- glm(formula_probit_prog2, binomial(link="probit"), data=s[s$region!='autre',])
summary(probit_prog2)
probit_prog2_margins <- summary(margins(data=s[s$region!='autre',], model=probit_prog2, variable="progressif")) # 
probit_prog2_margins
# probit_prog3 <- glm(as.formula(paste("tax_acceptance ~ progressif + ", paste(variables_exogenes, collapse=' + '))), binomial(link="probit"), data=s[s$region!='autre',])
# summary(probit_prog3)
# summary(margins(data=s[s$region!='autre',], model=probit_prog3, variable="progressif"))

s$non_progressif <- n(s$progressivite!='Non') 
# (1) OLS simple: 22 p.p.***
nols_prog0 <- lm(tax_acceptance ~ non_progressif, weights=s$weight, data=ss)
summary(nols_prog0)

# (2) OLS: 20 p.p.***
nformula_ols_prog1 <- as.formula(paste("tax_acceptance ~ non_progressif + ", paste(variables_demo, collapse=' + ')))
nols_prog1 <- lm(nformula_ols_prog1, weights=s$weight, data=ss)
summary(nols_prog1)

# (3) OLS tous contrôles: 12 p.p.***
nformula_ols_prog2 <- as.formula(paste("tax_acceptance ~ non_progressif + ", paste(variables_exogenes, collapse=' + ')))
nols_prog2 <- lm(nformula_ols_prog2, weights=s$weight, data=ss)
summary(nols_prog2)

# (4) probit: 21 p.p.***
nprobit_prog0 <- glm(tax_acceptance ~ non_progressif, binomial(link="probit"), data=ss)
summary(nprobit_prog0)
nprobit_prog0_margins <- summary(margins(data=s, model=nprobit_prog0, variable="non_progressif")) # 
nprobit_prog0_margins

# (5) probit: 19 p.p.***
nformula_probit_prog1 <- as.formula(paste("tax_acceptance ~ non_progressif + ", paste(variables_demo[1:(length(variables_demo)-2)], collapse=' + ')))
nprobit_prog1 <- glm(nformula_probit_prog1, binomial(link="probit"), data=s[s$region!='autre',])
summary(nprobit_prog1)
nprobit_prog1_margins <- summary(margins(data=s[s$region!='autre',], model=nprobit_prog1, variable="non_progressif")) # 
nprobit_prog1_margins

# (6) probit tous contrôles: 10 p.p.***
nformula_probit_prog2 <- as.formula(paste("tax_acceptance ~ non_progressif + ", paste(variables_exogenes[!(variables_exogenes %in% 
                                                                                                             c("age_50_64", "age_65_plus", "Transports_travail_commun", "Transports_travail_actif", "gaz", "fioul", "hausse_chauffage", "hausse_depenses", 
                                                                                                               "Simule_gain", "simule_gain", "elasticite_partielle", "elasticite_partielle_perso", "score_ges", "score_climate_call", "enfant_CC_pour_lui", "enfant_CC_pour_CC"))], 
                                                                                      collapse=' + ')))
nprobit_prog2 <- glm(nformula_probit_prog2, binomial(link="probit"), data=s[s$region!='autre',])
summary(nprobit_prog2)
nprobit_prog2_margins <- summary(margins(data=s[s$region!='autre',], model=nprobit_prog2, variable="non_progressif")) # 
nprobit_prog2_margins

# (5bis) OLS Approval ~ progressivite == Oui, tous contrôles
formula_pols_prog2 <- as.formula(paste("tax_approval ~ progressif + ", paste(variables_exogenes, collapse=' + ')))
pols_prog2 <- lm(formula_pols_prog2, weights=s$weight, data=ss)
summary(pols_prog2)

# (6bis) OLS Approval ~ progressivite != Non, tous contrôles
formula_pnols_prog2 <- as.formula(paste("tax_approval ~ non_progressif + ", paste(variables_exogenes, collapse=' + ')))
pnols_prog2 <- lm(formula_pnols_prog2, weights=s$weight, data=ss)
summary(pnols_prog2)

# Acceptance ~ progressivite=='Oui', depending on different controls and OLS/probit
table_prog <- stargazer(ols_prog0, ols_prog1, ols_prog2, probit_prog0, probit_prog1, probit_prog2,
                        title="Effect of believing in progressivity on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                        covariate.labels = c("Progressivity: `Yes'"), # "Constant", 
                        dep.var.labels = "Tax Acceptance ($A^I$)", dep.var.caption = "", header = FALSE,
                        keep = c("progressif"), # "Constant", 
                        coef = list(NULL, NULL, NULL, probit_prog0_margins$AME, probit_prog1_margins$AME, probit_prog2_margins$AME),
                        se = list(NULL, NULL, NULL, probit_prog0_margins$SE %>% set_names("progressif"), probit_prog1_margins$SE %>% set_names("progressif"), probit_prog2_margins$SE %>% set_names("progressif")),
                        add.lines = c( # "Method: Probit &  &  &  & \\checkmark & \\checkmark & \\checkmark",
                          "Controls: Socio-demographics &  & \\checkmark & \\checkmark &  & \\checkmark & \\checkmark",
                          "Controls: All (101 variables) &  &  & \\checkmark &  &  & \\checkmark"), #  (energy and transport characteristics, attitudes on policies and climate change)
                        no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="tab:prog")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', table_prog, fixed=TRUE), fixed=TRUE), collapse=' ')

length(variables_exogenes)

# Acceptance ~ progressivite!='Non', depending on different controls and OLS/probit
table_prog_non <- stargazer(nols_prog0, nols_prog1, nols_prog2, nprobit_prog0, nprobit_prog1, nprobit_prog2,
                            title="Effect of not believing in regressivity on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                            covariate.labels = c("Progressivity: not `No'"), # "Constant", 
                            dep.var.labels = "Tax Acceptance ($A^I$)", dep.var.caption = "", header = FALSE,
                            keep = c("progressif"), # "Constant", 
                            coef = list(NULL, NULL, NULL, nprobit_prog0_margins$AME, nprobit_prog1_margins$AME, nprobit_prog2_margins$AME),
                            se = list(NULL, NULL, NULL, nprobit_prog0_margins$SE %>% set_names("non_progressif"), nprobit_prog1_margins$SE %>% set_names("non_progressif"), nprobit_prog2_margins$SE %>% set_names("non_progressif")),
                            add.lines = c( # "Method: Probit &  &  &  & \\checkmark & \\checkmark & \\checkmark",
                              "Controls: Socio-demographics &  & \\checkmark & \\checkmark &  & \\checkmark & \\checkmark",
                              "Controls: All (101 variables) &  &  & \\checkmark &  &  & \\checkmark"), #  (energy and transport characteristics, attitudes on policies and climate change)
                            no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="tab:prog_non")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', table_prog_non, fixed=TRUE), fixed=TRUE), collapse=' ')

# Acceptance/Approval ~ progressivite ==Oui/!=Non, + 1 probit, + 1 sans contrôles
table_prog_non <- stargazer(ols_prog2, probit_prog2, ols_prog0, nols_prog2, pols_prog2, pnols_prog2,
                            title="Effect of not believing in regressivity on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                            covariate.labels = c("Progressivity: `Yes'", "Progressivity: not `No'"), # "Constant", 
                            dep.var.labels = c("Tax Acceptance ($A^I$)", "Tax Approval ($\\dot{A^I}$)"), dep.var.caption = "", header = FALSE,
                            keep = c("progressif"), # "Constant", 
                            coef = list(NULL, probit_prog2_margins$AME, NULL, NULL, NULL, NULL),
                            se = list(NULL, probit_prog2_margins$SE %>% set_names("progressif"), NULL, NULL, NULL, NULL),
                            add.lines = c( # "Method: Probit &  &  &  & \\checkmark & \\checkmark & \\checkmark",
                              "Controls (101 variables) & \\checkmark & \\checkmark &  & \\checkmark & \\checkmark & \\checkmark"), #  (energy and transport characteristics, attitudes on policies and climate change)
                            no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="tab:prog_non")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', table_prog_non, fixed=TRUE), fixed=TRUE), collapse=' ')

# (4) OLS simple: progressivite is yes
ols_prog4 <- lm(tax_acceptance ~ (progressivite == 'Oui'), weights=s$weight, data=ss)
summary(ols_prog4)

# (5) OLS simples: approve
ols_prog5 <- lm(tax_approval ~ progressif, weights=s$weight, data=ss)
summary(ols_prog5)

# (4) OLS with controls: progressivite is yes
formula_ols_prog4 <- as.formula(paste("tax_acceptance ~ (progressivite == 'Oui') + ", paste(variables_reg_prog, collapse=' + ')))
ols_prog4 <- lm(formula_ols_prog4, weights=s$weight, data=ss)
summary(ols_prog4)

# (5) OLS with controls: approve
formula_ols_prog5 <- as.formula(paste("tax_approval ~ progressif + ", paste(variables_reg_prog, collapse=' + ')))
ols_prog5 <- lm(formula_ols_prog5, weights=s$weight, data=ss)
summary(ols_prog5)

# (6) OLS with controls: progressivite is yes / approve
formula_ols_prog6 <- as.formula(paste("tax_approval ~ (progressivite == 'Oui') + ", paste(variables_reg_prog, collapse=' + ')))
ols_prog6 <- lm(formula_ols_prog6, weights=s$weight, data=ss)
summary(ols_prog6)

variables_reg_prog <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "Simule_gain", "Simule_gain2", variables_demo, variables_energie)
variables_reg_prog <- variables_reg_prog[!(variables_reg_prog %in% 
                                             c("revenu", "rev_tot", "age", "age_65_plus", "fioul", "gaz", "hausse_chauffage", "hausse_essence", "hausse_diesel", "hausse_depenses", "simule_gain"))]

summary(lm(as.formula(paste("progressif ~ ", paste(variables_reg_prog, collapse=' + '))), weights=s$weight, data=s))
variables_correlees_prog <- c("Revenu", "Revenu2", "gagnant_categorie", "taxe_efficace", "sexe", "diplome4", "surface")

s$progressif <- s$progressivite!='Non'
# (1) OLS with controls and interactions: effect only when interacted (with 101 control variables and no interaction: 27 p.p.***)
formula_ols_prog1 <- as.formula(paste("tax_acceptance ~ progressif + ", paste(paste(variables_reg_prog, collapse=' + '), 
                                                                              " + (gagnant_categorie!='Perdant') * (taxe_efficace!='Non') * progressif")))
ols_prog1 <- lm(formula_ols_prog1, weights=s$weight, data=s)
summary(ols_prog1)

# (1bis) OLS with controls and interactions: effect only when interacted (with 101 control variables and no interaction: 27 p.p.***)
formula_ols_prog1bis <- as.formula(paste("taxe_info_approbation!='Non' ~ progressif + ", paste(paste(variables_reg_prog, collapse=' + '), 
                                                                                               " + (gagnant_info_categorie!='Perdant') * (taxe_efficace!='Non') * progressif + progressif*revenu"))) #  + taxe_approbation: no dramatic difference /  + (gagnant_info_categorie!='Perdant') * revenu * progressif: no effect
ols_prog1bis <- lm(formula_ols_prog1bis, weights=s$weight, data=s)
summary(ols_prog1bis) # sum of all effects True: 0.826. P+G: 0.675; P+EE: 0.611 ; G+EE: 0.494.

# (2) OLS simple: 38 p.p.***
ols_prog2 <- lm(tax_acceptance ~ progressif, weights=s$weight, data=s)
summary(ols_prog2)

# (3) Logit with controls and interaction: pareil
logit_prog3 <- glm(tax_acceptance ~ progressif, family = binomial(link='logit'), data=s)
summary(logit_prog3)
logit_prog3_margins <- logitmfx(logit_prog3, data=s, atmean=FALSE)$mfxest
logit_prog3_margins

# (4) OLS simple: progressivite is yes / approve
ols_prog4 <- lm(tax_approval ~ (progressivite == 'Oui'), weights=s$weight, data=s)
summary(ols_prog4)
TableVII <- stargazer(ols_prog1, ols_prog2, logit_prog3, ols_prog4,
                      title="Effect of beliefs over progressivity on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                      covariate.labels = c("Progressivity: not `No' $(P>0)$", "Believes does not lose $(G>0)$", "Effective: not No $(EE>0)$", "$(G>0) \\times (EE>0)$",
                                           "Interaction: not lose $(P>0) \\times (G>0)$", "Interaction: effective $(P>0) \\times (EE>0)$", "$(P>0) \\times (G>0) \\times (EE>0)$", "Progressivity: `Yes' ($\\dot{P}>0$)"), # "Constant",
                      dep.var.labels = c("Tax Acceptance ($A^I$)", "Tax Approval ($\\dot{A^I}$)"), dep.var.caption = "", header = FALSE,
                      keep = c("progressi", "gagnant_categorie", 'taxe_efficace'), # "Constant"
                      coef = list(NULL, NULL, logit_prog3_margins[,1], NULL),
                      se = list(NULL, NULL, logit_prog3_margins[,2], NULL),
                      add.lines = list(c("Controls: Socio-demo, energy", "\\checkmark ", " ", "", "")),
                      no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:progressivity")
write_clip(gsub('\\end{table}', '} {\\footnotesize \\\\ \\quad \\\\ \\textsc{Note:} Standard errors are reported in parentheses. For logit, average marginal effects are reported and not coefficients. } \\end{table} ',
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableVII, fixed=TRUE), fixed=TRUE), collapse=' ')

s$pauvre_gagne_moins_perd <- (s$benefices_pauvres==T) - (s$problemes_pauvres==T)
summary(ivreg((taxe_info_approbation!='Non') ~ pauvre_gagne_moins_perd | info_progressivite, data=s))

##### 5.4 All effects #####
variables_reg_all <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "Simule_gain", "Simule_gain2", variables_demo)
variables_reg_all <- variables_reg_all[!(variables_reg_all %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
# With all exogenous variables, we obtain the same result
# variables_reg_all <- c("Revenu2", "Revenu_conjoint2", "Simule_gain2", variables_exogenes[!(variables_exogenes %in% c("age_50_64", "age_65_plus", "Transports_travail_commun", "Transports_travail_actif", "gaz", "fioul", "hausse_chauffage", 
#       "hausse_depenses", "Simule_gain", "simule_gain", "elasticite_partielle", "elasticite_partielle_perso", "score_ges", "score_climate_call", "enfant_CC_pour_lui", "enfant_CC_pour_CC"))])
s$gagnant <- s$gagnant_categorie=='Gagnant'
s$gagnant_NSP <- s$gagnant_categorie=='Non affecté'
s$taxe_efficace_oui <- s$taxe_efficace=='Oui'
s$taxe_efficace_NSP <- s$taxe_efficace=='NSP'
s$progressif <- s$progressivite=='Oui'
s$progressif_NSP <- s$progressivite=='NSP'
formula_all_acc <- as.formula(paste("tax_acceptance ~ gagnant + taxe_efficace_oui + progressif + gagnant_NSP + taxe_efficace_NSP + progressif_NSP +",paste(variables_reg_all, collapse = ' + '))) 
formula_all_app <- as.formula(paste("tax_approval ~ gagnant + taxe_efficace_oui + progressif + gagnant_NSP + taxe_efficace_NSP + progressif_NSP +", paste(variables_reg_all, collapse = ' + '))) 

# (1) OLS Acceptance with controls
ols_all1 <- lm(formula_all_acc, data=s, weights = s$weight)
summary(ols_all1)

# (2) OLS Acceptance without controls
ols_all2 <- lm(tax_acceptance ~ gagnant + taxe_efficace_oui + progressif + gagnant_NSP + taxe_efficace_NSP + progressif_NSP, data=s, weights = s$weight)
summary(ols_all2)

# (3) Logit Acceptance with controls
logit_all3 <- glm(formula_all_acc, family = binomial(link='logit'), data=s[s$region!='autre',])
summary(logit_all3)
logit_all3_margins <- logitmfx(data=s, formula=logit_all3, atmean=FALSE)$mfxest
logit_all3_margins

# (4) OLS Approval with controls
ols_all4 <- lm(formula_all_app, data=s, weights = s$weight)
summary(ols_all4)

# (5) OLS Approval without controls
ols_all5 <- lm(tax_approval ~ gagnant + taxe_efficace_oui + progressif + gagnant_NSP + taxe_efficace_NSP + progressif_NSP, data=s, weights = s$weight)
summary(ols_all5)

# (6) Logit Approval with controls
logit_all6 <- glm(formula_all_app, family = binomial(link='logit'), data=s[s$region!='autre',])
summary(logit_all6)
logit_all6_margins <- logitmfx(data=s, formula=logit_all6, atmean=FALSE)$mfxest 
logit_all6_margins

# (6bis) OLS Acceptance with all controls
formula_ols_all6 <- as.formula(paste("tax_acceptance ~ non_perdant + taxe_efficace_not_no + progressif_not_no + non_perdant * taxe_efficace_not_no * progressif_not_no +",
                                     paste(c(variables_reg_all, variables_energie, variables_politiques, variables_gilets_jaunes), collapse = ' + '))) 
ols_all6 <- lm(formula_ols_all6, data=s, weights = s$weight)
summary(ols_all6)

# Results
TableXIII <- stargazer(ols_all1, ols_all2, logit_all3, ols_all4, ols_all5, logit_all6,
                       title="Effects of three motives on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                       covariate.labels = c("Believes wins ($\\dot{G}>0$)", "Environmental effectiveness: `Yes'", "Progressivity: `Yes'",
                                            "Believes unaffected ($\\dot{G}=0$)", "Environmental effectiveness: `PNR'", "Progressivity: `PNR'"),
                       dep.var.labels = c("Acceptance", "Approval"), 
                       dep.var.caption = c("Tax and dividend"), header = FALSE,
                       keep = c("gagnant", "efficace", "progressif"),
                       coef = list(NULL, NULL, logit_all3_margins[,1], NULL, NULL, logit_all6_margins[,1]), 
                       se = list(NULL, NULL, logit_all3_margins[,2], NULL, NULL, logit_all6_margins[,2]),
                       add.lines = list(c("Controls: Incomes, socio-demographics", "\\checkmark ", "", "\\checkmark  ", "\\checkmark", "", "\\checkmark")),
                       no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:all_effects")
write_clip(gsub("logistic", "logit", gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableXIII, fixed=TRUE), fixed=TRUE)), collapse=' ')
variables_reg_all <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "Simule_gain", "Simule_gain2", variables_demo)
variables_reg_all <- variables_reg_all[!(variables_reg_all %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
# With all exogenous variables, we obtain the same result
# variables_reg_all <- c("Revenu2", "Revenu_conjoint2", "Simule_gain2", variables_exogenes[!(variables_exogenes %in% c("age_50_64", "age_65_plus", "Transports_travail_commun", "Transports_travail_actif", "gaz", "fioul", "hausse_chauffage", 
#       "hausse_depenses", "Simule_gain", "simule_gain", "elasticite_partielle", "elasticite_partielle_perso", "score_ges", "score_climate_call", "enfant_CC_pour_lui", "enfant_CC_pour_CC"))])

s$gagnant <- s$gagnant_categorie=='Gagnant'
s$gagnant_NSP <- s$gagnant_categorie=='Non affecté'
s$non_perdant <- s$gagnant_categorie!='Perdant'
s$taxe_efficace_oui <- s$taxe_efficace=='Oui'
s$taxe_efficace_NSP <- s$taxe_efficace=='NSP'
s$taxe_efficace_not_no <- s$taxe_efficace!='Non'
s$progressif <- s$progressivite=='Oui'
s$progressif_NSP <- s$progressivite=='NSP'
s$progressif_not_no <- s$progressivite!='Non'
formula_all_acc_wo_PNR <- as.formula(paste("tax_acceptance ~ non_perdant + taxe_efficace_not_no + progressif_not_no + non_perdant * taxe_efficace_not_no * progressif_not_no +", paste(variables_reg_all, collapse = ' + '))) 
formula_all_acc_wo_interaction <- as.formula(paste("tax_acceptance ~ non_perdant + taxe_efficace_not_no + progressif_not_no + gagnant_NSP + taxe_efficace_NSP + progressif_NSP +",paste(variables_reg_all, collapse = ' + '))) 
formula_all_acc <- as.formula(paste("tax_acceptance ~ non_perdant + taxe_efficace_not_no + progressif_not_no + gagnant_NSP + taxe_efficace_NSP + progressif_NSP + non_perdant * taxe_efficace_not_no * progressif_not_no +",paste(variables_reg_all, collapse = ' + '))) 
formula_all_app <- as.formula(paste("tax_approval ~ non_perdant + taxe_efficace_not_no + progressif_not_no + gagnant_NSP + taxe_efficace_NSP + progressif_NSP + non_perdant * taxe_efficace_not_no * progressif_not_no +", paste(variables_reg_all, collapse = ' + '))) 

# (1) OLS Acceptance with controls and without PNR
ols_all1 <- lm(formula_all_acc_wo_PNR, data=s, weights = s$weight)
summary(ols_all1)

# (2) OLS Acceptance with controls
ols_all2 <- lm(formula_all_acc, data=s, weights = s$weight)
summary(ols_all2)

# (3) OLS Acceptance without controls
ols_all3 <- lm(tax_acceptance ~ non_perdant + taxe_efficace_not_no + progressif_not_no + gagnant_NSP + taxe_efficace_NSP + progressif_NSP + non_perdant * taxe_efficace_not_no * progressif_not_no, data=s, weights = s$weight)
summary(ols_all3)

# (4) Logit Acceptance with controls
logit_all4 <- glm(formula_all_acc, family = binomial(link='logit'), data=s[s$region!='autre',])
summary(logit_all4)
logit_all4_margins <- logitmfx(data=s, formula=logit_all4, atmean=FALSE)$mfxest
logit_all4_margins

# (5) OLS Approval with controls
ols_all5 <- lm(formula_all_app, data=s, weights = s$weight)
summary(ols_all5)

# (6) OLS Approval without interaction
ols_all6 <- lm(formula_all_acc_wo_interaction, data=s, weights = s$weight)
summary(ols_all6)

# Results
all_effects <- stargazer(ols_all1, ols_all2, ols_all3, logit_all4, ols_all5,
                         title="Effects of three motives on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                         covariate.labels = c("Constant", "Believes does not lose ($G>0$)", "Environmental effectiveness: `Not no' ($EE>0$)", "Progressivity: `Not no' ($P>0$)",
                                              "Believes unaffected ($\\dot{G}=0$)", "Environmental effectiveness: `PNR' ($\\dot{EE}=0$)", "Progressivity: `PNR' ($\\dot{P}=0$)",
                                              "$(G>0) \\times (EE>0)$", "$(G>0) \\times (P>0)$", "$(EE>0) \\times (P>0)$", "$(G>0) \\times (EE>0) \\times (P>0)$"),
                         dep.var.labels = c("Acceptance", "Approval"), 
                         dep.var.caption = c("Tax and dividend"), header = FALSE,
                         keep = c("Constant", "gagnant", "efficace", "progressif", "perdant"),
                         coef = list(NULL, NULL, NULL, logit_all4_margins[,1], NULL), 
                         se = list(NULL, NULL, NULL, logit_all4_margins[,2], NULL),
                         add.lines = list(c("Controls: Incomes, socio-demographics", "\\checkmark ", "\\checkmark  ", "", "\\checkmark  ", "\\checkmark")),
                         no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:all_effects")
write_clip(gsub("logistic", "logit", gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', all_effects, fixed=TRUE), fixed=TRUE)), collapse=' ')

summary(lm(progressivite!='Non' ~ info_progressivite, data = s, weights = s$weight))

summary(lm(progressivite!='Non' ~ info_CC * simule_gagnant * info_progressivite + simule_gain + taxe_approbation, data = s, weights = s$weight))
summary(lm(taxe_info_approbation!='Non' ~ info_CC * simule_gagnant * info_progressivite + simule_gain + taxe_approbation, data = s, weights = s$weight))

summary(lm(progressivite!='Non' ~ info_CC * simule_gagnant * info_progressivite * biais_sur + simule_gain + taxe_approbation, data = s, weights = s$weight, subset = apres_modifs == T))
summary(lm(taxe_feedback_approbation!='Non' ~ info_CC * simule_gagnant * info_progressivite * biais_sur + simule_gain + taxe_approbation, data = s, weights = s$weight, subset = apres_modifs == T))

summary(lm(progressivite!='Non' ~ info_CC * simule_gagnant * info_progressivite + simule_gain + taxe_approbation, data = s, weights = s$weight, subset = biais_sur == F))
summary(lm(taxe_info_approbation!='Non' ~ info_CC * simule_gagnant * info_progressivite + simule_gain + taxe_approbation, data = s, weights = s$weight, subset = biais_sur == F))

summary(ivreg(taxe_info_approbation!='Non' ~ simule_gagnant + simule_gain + taxe_approbation + (progressivite!='Non') |
    simule_gagnant + simule_gain + taxe_approbation + info_progressivite, data = s, weights = s$weight), diagnostics = TRUE)

summary(ivreg(taxe_info_approbation!='Non' ~ taxe_approbation + (progressivite!='Non') | taxe_approbation + info_progressivite, data = s, weights = s$weight), diagnostics = TRUE)
summary(ivreg(taxe_info_approbation!='Non' ~ (progressivite!='Non') |  info_progressivite, data = s, weights = s$weight), diagnostics = TRUE)
summary(ivreg(taxe_cible_approbation!='Non' ~ (progressivite!='Non') | info_progressivite, data = s, weights = s$weight), diagnostics = TRUE)
summary(ivreg(taxe_cible_approbation!='Non' ~ cible + percentile_revenu + percentile_revenu_conjoint + (progressivite!='Non') |  
                info_progressivite + cible + percentile_revenu + percentile_revenu_conjoint, data = s, weights = s$weight), diagnostics = TRUE)

##### Graphiques RDD #####
# Seul le revenu du répondant est pris en compte, sont exclus de l'analyse les répondants au revenu > 2220
deciles <- c(780, 1140, 1430, 1670, 2220)
arguments_rdd <- matrix(c(-Inf, deciles[1:3], -Inf, deciles[2:4], deciles[c(1:4,2:5)], rep(T, 4), rep(FALSE, 4)), nrow=8)
ci_mean_cible_acceptance <- apply(arguments_rdd, 1, function(row) { low <- row[1];  up <- row[2];  traite <- row[3]; return(binconf(sum(s$weight[s$taxe_cible_approbation!='Non' & s$revenu >= low & s$revenu < up & s$traite_cible==traite]), sum(s$weight[s$revenu >= low & s$revenu < up & s$traite_cible==traite]), 0.1))})

mar_old <- par()$mar
par(mar=c(4.1, 4.1, 1.1, 1.1))

plot(c(0, 780), rep(ci_mean_cible_acceptance[1,1], 2), type='l', col = 'cyan', lwd=2,
     xlab="Income of respondent (€/month)", ylab="Average Targeted Tax Acceptance", xlim=c(500, 2000), ylim=c(0.2, 0.55), xaxt='n')
lines(c(780, 1140), rep(ci_mean_cible_acceptance[1,5], 2), type='l', col = 'cyan', lwd=2, lty=2)
lines(c(780, 1140), rep(ci_mean_cible_acceptance[1,2], 2), type='l', col = 'deepskyblue3', lwd=2)
lines(c(1140, 1430), rep(ci_mean_cible_acceptance[1,6], 2), type='l', col = 'deepskyblue3', lwd=2, lty=2)
lines(c(1140, 1430), rep(ci_mean_cible_acceptance[1,3], 2), type='l', col = 'blue', lwd=2)
lines(c(1430, 1670), rep(ci_mean_cible_acceptance[1,7], 2), type='l', col = 'blue', lwd=2, lty=2)
lines(c(1430, 1670), rep(ci_mean_cible_acceptance[1,4], 2), type='l', col = 'black', lwd=2)
lines(c(1670, 2220), rep(ci_mean_cible_acceptance[1,8], 2), type='l', col = 'black', lwd=2, lty=2)
plotCI(x=c((c(500, deciles[c(1:3,1:4)]) + c(deciles[c(1:4,2:4)], 2000))/2) + c(rep(-30,4), rep(30,4)), 
       y=ci_mean_cible_acceptance[1,], li=ci_mean_cible_acceptance[2,], ui=ci_mean_cible_acceptance[3,], add=T,
       col=c('cyan', 'deepskyblue3', 'blue', 'black', 'cyan', 'deepskyblue3', 'blue', 'black'), lwd=0.7, pch=NA)
grid() + abline(v = c(780), lwd=0.5) + axis(1, at=c(780, 1140))
axis(1, at=c(1430)) +  abline(v = c(1140), lwd=0.5) 
axis(1, at=c(1670)) +  abline(v = c(1430), lwd=0.5) 
axis(1, at=c(500, 2000)) +  abline(v = c(1670), lwd=0.5) 
axis(1, at=c(500, 780, 1140, 1430, 1670, 2000))
grid() + abline(v = c(780, 1140, 1430, 1670), lwd=0.5) 
legend("topright", lwd=2, lty=c(1,2), col=c("black"), title.col = "black", legend=c("Eligible to payment", "Not eligible")) # , text.col = c("blue")

# All stacked
plot(c(0, 1), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu < 780 & s$traite_cible==T], weights=s$weight[s$revenu < 780 & s$traite_cible==T]), 2), type='l', col = 'cyan', lwd=2,
     xlab="Decile of respondent (€/month)", ylab="Average Targeted Tax Acceptance", xlim=c(0, 1), ylim=c(0.2, 0.6), xaxt='n')
lines(c(0, 1), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 780 & s$revenu < 1140 & s$traite_cible==T], weights=s$weight[s$revenu >= 780 & s$revenu < 1140 & s$traite_cible==T]), 2), type='l', col = 'deepskyblue3', lwd=2)
lines(c(0, 1), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible==T], weights=s$weight[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible==T]), 2), type='l', col = 'blue', lwd=2)
lines(c(0, 1), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible==T], weights=s$weight[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible==T]), 2), type='l', col = 'black', lwd=2)
lines(c(0, 1), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu < 1140 & s$traite_cible!=T], weights=s$weight[s$revenu < 1140 & s$traite_cible!=T]), 2), type='l', col = 'cyan', lwd=2, lty=2)
lines(c(0, 1), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible!=T], weights=s$weight[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible!=T]), 2), type='l', col = 'deepskyblue3', lwd=2, lty=2)
lines(c(0, 1), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible!=T], weights=s$weight[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible!=T]), 2), type='l', col = 'blue', lwd=2, lty=2)
lines(c(0, 1), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 1670 & s$revenu < 2220 & s$traite_cible!=T], weights=s$weight[s$revenu >= 1670 & s$revenu < 2220 & s$traite_cible!=T]), 2), type='l', col = 'black', lwd=2, lty=2)
axis(1, at=c(0, 1), labels = c("Lower", "Upper"))
grid() + abline(v = c(0, 1), lwd=0.5)
legend("topright", lwd=2, lty=c(1,2), col=c("blue"), title.col = "black", legend=c("Transfer to respondent", "No transfer")) # , text.col = c("blue")

# Before/After threshold
plot(c(-1, 0), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu < 780 & s$traite_cible==T], weights=s$weight[s$revenu < 780 & s$traite_cible==T]), 2), type='l', col = 'cyan', lwd=2,
     xlab="Income of respondent (€/month)", ylab="Average Targeted Tax Acceptance", xlim=c(-1, 1), ylim=c(0.2, 0.55), xaxt='n')
lines(c(-1, 0), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 780 & s$revenu < 1140 & s$traite_cible==T], weights=s$weight[s$revenu >= 780 & s$revenu < 1140 & s$traite_cible==T]), 2), type='l', col = 'deepskyblue3', lwd=2)
lines(c(-1, 0), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible==T], weights=s$weight[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible==T]), 2), type='l', col = 'blue', lwd=2)
lines(c(-1, 0), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible==T], weights=s$weight[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible==T]), 2), type='l', col = 'black', lwd=2)
lines(c(0, 1), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu < 1140 & s$traite_cible!=T], weights=s$weight[s$revenu < 1140 & s$traite_cible!=T]), 2), type='l', col = 'cyan', lwd=2, lty=2)
lines(c(0, 1), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible!=T], weights=s$weight[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible!=T]), 2), type='l', col = 'deepskyblue3', lwd=2, lty=2)
lines(c(0, 1), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible!=T], weights=s$weight[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible!=T]), 2), type='l', col = 'blue', lwd=2, lty=2)
lines(c(0, 1), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 1670 & s$revenu < 2220 & s$traite_cible!=T], weights=s$weight[s$revenu >= 1670 & s$revenu < 2220 & s$traite_cible!=T]), 2), type='l', col = 'black', lwd=2, lty=2)
axis(1, at=0, labels = c("Target Threshold"))
grid() + abline(v = 0, lwd=0.5)
legend("topright", lwd=2, lty=c(1,2,0,1,1,1,1), col=c("blue", "blue", NA, "cyan", "deepskyblue3", "blue", "black"), title.col = "black", legend=c("Transfer to respondent", "No transfer", "Target: bottom...", "20%", "30%", "40%", "50%")) # , text.col = c("blue")

# Placebo: on tax_acceptance
plot(c(0, 780), rep(wtd.mean(s$tax_acceptance[s$revenu < 780 & s$traite_cible==T], weights=s$weight[s$revenu < 780 & s$traite_cible==T]), 2), type='l', col = 'blue', lwd=2,
     xlab="Income of respondent (€/month)", ylab="Average Tax Acceptance", xlim=c(500, 2000), ylim=c(0.2, 0.4), xaxt='n')
lines(c(780, 1140), rep(wtd.mean(s$tax_acceptance[s$revenu >= 780 & s$revenu < 1140 & s$traite_cible==T], weights=s$weight[s$revenu >= 780 & s$revenu < 1140 & s$traite_cible==T]), 2), type='l', col = 'blue', lwd=2)
lines(c(1140, 1430), rep(wtd.mean(s$tax_acceptance[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible==T], weights=s$weight[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible==T]), 2), type='l', col = 'blue', lwd=2)
lines(c(1430, 1670), rep(wtd.mean(s$tax_acceptance[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible==T], weights=s$weight[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible==T]), 2), type='l', col = 'blue', lwd=2)
lines(c(780, 1140), rep(wtd.mean(s$tax_acceptance[s$revenu < 1140 & s$traite_cible!=T], weights=s$weight[s$revenu < 1140 & s$traite_cible!=T]), 2), type='l', col = 'blue', lwd=2, lty=2)
lines(c(1140, 1430), rep(wtd.mean(s$tax_acceptance[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible!=T], weights=s$weight[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible!=T]), 2), type='l', col = 'blue', lwd=2, lty=2)
lines(c(1430, 1670), rep(wtd.mean(s$tax_acceptance[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible!=T], weights=s$weight[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible!=T]), 2), type='l', col = 'blue', lwd=2, lty=2)
lines(c(1670, 2220), rep(wtd.mean(s$tax_acceptance[s$revenu >= 1670 & s$revenu < 2220 & s$traite_cible!=T], weights=s$weight[s$revenu >= 1670 & s$revenu < 2220 & s$traite_cible!=T]), 2), type='l', col = 'blue', lwd=2, lty=2)
axis(1, at=c(500, 780, 1140, 1430, 1670, 2000))
grid() + abline(v = c(780, 1140, 1430, 1670), lwd=0.5)
legend("topright", lwd=2, lty=c(1,2), col=c("blue"), title.col = "black", text.col = c("blue"), legend=c("Transfer to respondent", "No transfer"))

par(mar=mar_old)

# plot(c(0, 780), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu < 780 & s$traite_cible==T], weights=s$weight[s$revenu < 780 & s$traite_cible==T]), 2), type='l', col = 'cyan', lwd=2,
#      xlab="Income of respondent (€/month)", ylab="Average Targeted Tax Acceptance", xlim=c(500, 2000), ylim=c(0.2, 0.5), xaxt='n')
# lines(c(780, 1140), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 780 & s$revenu < 1140 & s$traite_cible==T], weights=s$weight[s$revenu >= 780 & s$revenu < 1140 & s$traite_cible==T]), 2), type='l', col = 'deepskyblue3', lwd=2)
# lines(c(1140, 1430), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible==T], weights=s$weight[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible==T]), 2), type='l', col = 'blue', lwd=2)
# lines(c(1430, 1670), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible==T], weights=s$weight[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible==T]), 2), type='l', col = 'black', lwd=2)
# lines(c(780, 1140), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu < 1140 & s$traite_cible!=T], weights=s$weight[s$revenu < 1140 & s$traite_cible!=T]), 2), type='l', col = 'cyan', lwd=2, lty=2)
# lines(c(1140, 1430), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible!=T], weights=s$weight[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible!=T]), 2), type='l', col = 'deepskyblue3', lwd=2, lty=2)
# lines(c(1430, 1670), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible!=T], weights=s$weight[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible!=T]), 2), type='l', col = 'blue', lwd=2, lty=2)
# lines(c(1670, 2220), rep(wtd.mean((s$taxe_cible_approbation!='Non')[s$revenu >= 1670 & s$revenu < 2220 & s$traite_cible!=T], weights=s$weight[s$revenu >= 1670 & s$revenu < 2220 & s$traite_cible!=T]), 2), type='l', col = 'black', lwd=2, lty=2)

# rdd_20 <- lm(taxe_cible_approbation!='Non' ~ revenu + traite_cible, subset=categorie_cible %in% c('_20', '20_30') & revenu < 2220, data=s, weights=s$weight)
# rdd_30 <- lm(taxe_cible_approbation!='Non' ~ revenu + traite_cible, subset=categorie_cible %in% c('30_40', '20_30') & revenu < 2220, data=s, weights=s$weight)
# rdd_40 <- lm(taxe_cible_approbation!='Non' ~ revenu + traite_cible, subset=categorie_cible %in% c('30_40', '40_50') & revenu < 2220, data=s, weights=s$weight)
# rdd_50 <- lm(taxe_cible_approbation!='Non' ~ revenu + traite_cible, subset=categorie_cible %in% c('40_50', '50_70') & revenu < 2220, data=s, weights=s$weight)
# summary(rdd_20)
# plot(s$revenu[s$categorie_cible %in% c('_20', '20_30') & s$revenu < 2220][order(rdd_20$fitted.values)], sort(rdd_20$fitted.values), type='l', col = 'blue', lwd=2,
#      xlab="Income of respondent", ylab="Fit of Tax Acceptance ~ Income + Transfer")
# 
# rdd_20_T <- lm(taxe_cible_approbation!='Non' ~ revenu, subset=revenu < 780 & traite_cible==T, data=s, weights=s$weight)
# rdd_30_T <- lm(taxe_cible_approbation!='Non' ~ revenu, subset=revenu >= 780 & revenu < 1140 & traite_cible==T, data=s, weights=s$weight)
# rdd_40_T <- lm(taxe_cible_approbation!='Non' ~ revenu, subset=revenu >= 1140 & revenu < 1430 & traite_cible==T, data=s, weights=s$weight)
# rdd_50_T <- lm(taxe_cible_approbation!='Non' ~ revenu, subset=revenu >= 1430 & revenu < 1670 & traite_cible==T, data=s, weights=s$weight)
# rdd_20_nT <- lm(taxe_cible_approbation!='Non' ~ revenu, subset=revenu < 1140 & traite_cible!=T, data=s, weights=s$weight)
# rdd_30_nT <- lm(taxe_cible_approbation!='Non' ~ revenu, subset=revenu >= 1140 & revenu < 1430 & traite_cible!=T, data=s, weights=s$weight)
# rdd_40_nT <- lm(taxe_cible_approbation!='Non' ~ revenu, subset=revenu >= 1430 & revenu < 1670 & traite_cible!=T, data=s, weights=s$weight)
# rdd_50_nT <- lm(taxe_cible_approbation!='Non' ~ revenu, subset=revenu >= 1670 & revenu < 2220 & traite_cible!=T, data=s, weights=s$weight)
# plot(s$revenu[s$revenu < 780 & s$traite_cible==T][order(rdd_20_T$fitted.values)], sort(rdd_20_T$fitted.values), type='l', col = 'blue', lwd=2,
#      xlab="Income of respondent", ylab="Fit of Tax Acceptance ~ Income + Transfer", xlim=c(0, 2220), ylim=c(0.2, 0.5))
# lines(s$revenu[s$revenu >= 780 & s$revenu < 1140 & s$traite_cible==T][order(rdd_30_T$fitted.values)], sort(rdd_30_T$fitted.values), type='l', col = 'blue', lwd=2)
# lines(s$revenu[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible==T][order(rdd_40_T$fitted.values)], sort(rdd_40_T$fitted.values), type='l', col = 'blue', lwd=2)
# lines(s$revenu[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible==T][order(rdd_50_T$fitted.values)], sort(rdd_50_T$fitted.values), type='l', col = 'blue', lwd=2)
# lines(s$revenu[s$revenu < 1140 & s$traite_cible!=T][order(rdd_20_nT$fitted.values)], sort(rdd_20_nT$fitted.values), type='l', col = 'blue', lwd=2, lty=2)
# lines(s$revenu[s$revenu >= 1140 & s$revenu < 1430 & s$traite_cible!=T][order(rdd_30_nT$fitted.values)], sort(rdd_30_nT$fitted.values), type='l', col = 'blue', lwd=2, lty=2)
# lines(s$revenu[s$revenu >= 1430 & s$revenu < 1670 & s$traite_cible!=T][order(rdd_40_nT$fitted.values)], sort(rdd_40_nT$fitted.values), type='l', col = 'blue', lwd=2, lty=2)
# lines(s$revenu[s$revenu >= 1670 & s$revenu < 2220 & s$traite_cible!=T][order(rdd_50_nT$fitted.values)], sort(rdd_50_nT$fitted.values), type='l', col = 'blue', lwd=2, lty=2)

###### Effets des infos environnement #####
# rien sur le CC
summary(lm((cause_CC=='anthropique') ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight))
summary(lm(n(effets_CC) ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight))
summary(lm(n(effets_CC)>1 ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight)) # seul spécification significative pour effets_CC
summary(lm(generation_CC_min > 1990 ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight))
summary(lm(generation_CC_max > 1990 ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight))
summary(lm(changer_essaie==T ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight))
summary(lm(n(parle_CC) ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight))
summary(lm(responsable_CC_chacun==T ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight))
summary(lm(enfant_CC!='Non' ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight))
summary(lm(emission_cible ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight))
summary(lm(region_CC=='Autant dans les deux' ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight))
# quelques interactions entre le fait d'être convaincu et des socio-démos
summary(lm(taxe_efficace!='Non' ~ apres_modifs * Gilets_jaunes + info_CC * Gilets_jaunes + info_PM * Gilets_jaunes, data=s, weights = s$weight)) # soutiens moins sensibles à PM
summary(lm(taxe_efficace!='Non' ~ apres_modifs * sexe + info_CC * sexe + info_PM * sexe, data=s, weights = s$weight)) # femme plus sensibles à apres_modifs
summary(lm(taxe_efficace!='Non' ~ apres_modifs * statut_emploi + info_CC * statut_emploi + info_PM * statut_emploi, data=s, weights = s$weight)) # étudiants + sensibles à apres_modifs, fonctionnaires + à PM
summary(lm(taxe_efficace!='Non' ~ apres_modifs * diplome4 + info_CC * diplome4 + info_PM * diplome4, data=s, weights = s$weight)) # diplômés plus sensibles à apres_modifs
summary(lm(taxe_efficace!='Non' ~ apres_modifs * (Diplome>4) + info_CC * (Diplome>4) + info_PM * (Diplome>4), data=s, weights = s$weight))  # licenciés plus sensibles à apres_modifs
summary(lm(taxe_efficace!='Non' ~ apres_modifs * Gauche_droite + info_CC * Gauche_droite + info_PM * Gauche_droite, data=s, weights = s$weight)) # extrême-droite et indéterminés moins sensibles à apres_modifs

summary(lm(taxe_efficace!='Non' ~ apres_modifs * (Diplome>4) + apres_modifs * sexe, data=s, weights = s$weight))

summary(lm(progressivite!='Non' ~ info_CC * info_PM, data=s, weights = s$weight))
summary(lm(taxe_approbation!='Non' ~ apres_modifs + info_CC * info_PM, data=s, weights = s$weight))
summary(lm(taxe_approbation!='Non' ~ info_CC, data=s, weights = s$weight))
# pb avec l'exclusion restriction!
summary(lm(gagnant_categorie!='Perdant' ~ apres_modifs + info_CC * info_PM, data=s, weights = s$weight))
summary(lm(gagnant_categorie!='Perdant' ~ info_PM, data=s, weights = s$weight))
summary(lm(gagnant_categorie=='Gagnant' ~ apres_modifs + info_CC * info_PM, data=s, weights = s$weight))
summary(lm(benefices_CC==T ~ apres_modifs + info_CC * info_PM, data=s, weights = s$weight))
summary(lm(problemes_inefficace==T ~ apres_modifs + info_CC * info_PM, data=s, weights = s$weight))


##### Effets de l'info progressivité #####
summary(lm(gagnant_categorie=='Gagnant' ~ info_progressivite * biais_sur, data=s, weights = s$weight))
summary(lm(gagnant_categorie!='Perdant' ~ info_progressivite * biais_sur, data=s, weights = s$weight))
summary(lm(taxe_gagnant_pauvres==T ~ info_progressivite * biais_sur, data=s, weights = s$weight))
summary(lm(taxe_gagnant_riches==T ~ info_progressivite * biais_sur, data=s, weights = s$weight))
summary(lm(taxe_gagnant_personne==T ~ info_progressivite * biais_sur, data=s, weights = s$weight))
summary(lm(taxe_gagnant_citadins==T ~ info_progressivite * biais_sur, data=s, weights = s$weight))
summary(lm(taxe_gagnant_moyennes==T ~ info_progressivite * biais_sur, data=s, weights = s$weight))
summary(lm(taxe_gagnant_certains==T ~ info_progressivite * biais_sur, data=s, weights = s$weight))
summary(lm(taxe_perdant_pauvres==T ~ info_progressivite * biais_sur, data=s, weights = s$weight))
summary(lm(taxe_perdant_riches==T ~ info_progressivite * biais_sur, data=s, weights = s$weight))
summary(lm(taxe_perdant_personne==T ~ info_progressivite * biais_sur, data=s, weights = s$weight)) # -0.01 * prog + 0.02 . interaction
summary(lm(taxe_perdant_personne==T ~ info_progressivite * biais_sur * apres_modifs, data=s, weights = s$weight)) # seul prog:biais n'a pas d'effet
summary(lm(taxe_perdant_ruraux==T ~ info_progressivite * biais_sur, data=s, weights = s$weight))
summary(lm(taxe_perdant_moyennes==T ~ info_progressivite * biais_sur, data=s, weights = s$weight)) # 0.07** prog - 0.06 . interaction
summary(lm(taxe_perdant_moyennes==T ~ info_progressivite * biais_sur * apres_modifs, data=s, weights = s$weight)) # ...
summary(lm(taxe_perdant_certains==T ~ info_progressivite * biais_sur, data=s, weights = s$weight))
# benefices/problems présenté avant info : les corrélations sont l'effet de la seconde moitié de l'échantillon
summary(lm(benefices_pauvres==T ~ info_progressivite * biais_sur, data=s, weights = s$weight))
summary(lm(benefices_revenu==T ~ info_progressivite * revenu, data=s, weights = s$weight)) 
summary(lm(benefices_revenu==T ~ info_progressivite * biais_sur, data=s, weights = s$weight)) # 0.03 . interaction
summary(lm(benefices_revenu==T ~ info_progressivite * biais_sur * apres_modifs, data=s, weights = s$weight)) # 0.05 . interaction BIZARRE
summary(lm(problemes_ruraux==T ~ info_progressivite * biais_sur, data=s, weights = s$weight)) # .05 .
summary(lm(problemes_revenu==T ~ info_progressivite * biais_sur, data=s, weights = s$weight))
summary(lm(problemes_pauvres==T ~ info_progressivite * biais_sur, data=s, weights = s$weight)) # -0.04 . prog + 0.08 * interaction
summary(lm(problemes_pauvres==T ~ info_progressivite * biais_sur * apres_modifs, data=s, weights = s$weight)) # -0.04 . prog + 0.08 * interaction
summary(lm(progressivite!='Non' ~ info_progressivite * biais_sur, data=s, weights=s$weight)) # -0.11 * prog:biais
summary(lm(progressivite!='Non' ~ info_progressivite * Gauche_droite + info_progressivite * Gilets_jaunes, data=s, weights=s$weight)) 
summary(lm(progressivite!='Non' ~ info_progressivite * gauche_droite + info_progressivite * gilets_jaunes, data=s, weights=s$weight)) # -.003 . prog:gilets_jaunes
summary(lm(progressivite!='Non' ~ info_progressivite * gauche_droite + info_progressivite * gilets_jaunes + info_progressivite * biais_sur, data=s, weights=s$weight)) # some corr but is it meaningful?
summary(lm(gagnant_info_categorie!='Perdant' ~ info_progressivite, data=s, weights=s$weight))
summary(lm(gagnant_info_categorie!='Perdant' ~ info_progressivite + apres_modifs, data=s, weights=s$weight))
summary(lm(gagnant_info_categorie!='Perdant' ~ info_progressivite * revenu, data=s, weights=s$weight))
summary(lm(gagnant_info_categorie!='Perdant' ~ info_progressivite * revenu + info_progressivite * rev_tot + info_progressivite * I(rev_tot^2), data=s, weights=s$weight))
# summary(lm(gagnant_info_categorie!='Perdant' ~ (progressivite!='Non') * Revenu + (progressivite!='Non') * Revenu_conjoint + (progressivite!='Non') * Revenu2, data=s, weights=s$weight))
# summary(lm(gagnant_info_categorie!='Perdant' ~ (progressivite!='Non') * Revenu + (progressivite!='Non') * Revenu_conjoint, data=s, weights=s$weight))
summary(lm(gagnant_info_categorie!='Perdant' ~ (progressivite!='Non') * Revenu, data=s, weights=s$weight))
# summary(lm(gagnant_info_categorie!='Perdant' ~ (progressivite!='Non'), data=s, weights=s$weight))
summary(lm(gagnant_info_categorie!='Perdant' ~ (progressivite=='Oui') * Revenu, data=s, weights=s$weight))
summary(lm(gagnant_info_categorie=='Gagnant' ~ (progressivite!='Non') * Revenu, data=s, weights=s$weight)) # interaction -.03*
summary(lm(gagnant_info_categorie=='Gagnant' ~ (progressivite=='Oui') * Revenu, data=s, weights=s$weight)) # interaction -.07***
summary(lm(gagnant_info_categorie=='Gagnant' ~ progressivite * Revenu, data=s, weights=s$weight)) # interaction Oui -.07***
summary(lm(gagnant_info_categorie!='Perdant' ~ progressivite * Revenu, data=s, weights=s$weight)) # interaction NSP .09***


##### ON SUBSAMPLE OF BIAS > 110 #####
# Those who are very biased present very similar results than whole sample, except for progressivity: they turn to think the tax is regressive when we tell them it's not
# Those who have large absolute gains have similar results, though smaller coef for SI-RDD because the compliers of LATE for are more biased and have lower acceptance anyway
ss <- s[s$simule_gain - s$gain > 110,]
ss <- s[abs(s$simule_gain - s$gain) > 110,]
ss <- s[abs(s$simule_gain) > 90,]
wtd.mean(ss$simule_gain_inelastique < ss$gain, ss$weight) # Less than 1% of those with a bias > 110 have a higher subjective gain with inelastic good specification
wtd.mean(abs(s$simule_gain) > 90, s$weight) # 37% have absolute gain higher than 90, meaning that there is 95% our prediction is correct (93% at the margin)

##### 3 Perceptions: Same Results #####
## 3.1 Impact on purchasing power
# Over-estimation of policy costs TODO: correct figures in paper
# Subjective losses
decrit(ss$gain_fuel, weights = ss$weight) # mean -61 instead of +18
decrit(ss$gain_chauffage, weights = ss$weight) # -43 instead of +6
decrit(ss$gain, weights = ss$weight) # -89 instead of +24
# Objective winning category: cf. consistency_belief_losses.py for weighted results
decrit(objective_gains$transport > 0)
decrit(objective_gains$housing > 0)
decrit(objective_gains$all > 0, weights = objective_gains$weight)
# Subjective winning category
decrit(ss$gagnant_categorie, weights = ss$weight) # 14.0% think they win (21.7% unaffected)
decrit(ss$gagnant_fuel_categorie, weights = ss$weight) # 15.5% think they win (21.8% unaffected)
decrit(ss$gagnant_chauffage_categorie, weights = ss$weight) # 17.0% think they win (30.0% unaffected)

decrit(ss$gagnant_categorie, weights = ss$weight) # 64/22/14 +1/0/-1
decrit(ss$simule_gagnant, weights = ss$weight)
decrit(n(ss$gain) - ss$simule_gain, weights = ss$weight) # mean -126, median -116
decrit(ss$simule_gain > ss$gain, weights = ss$weight) # 89%
# decrit(ss$simule_gain - ss$gain > 50, weights = ss$weight) # 75%
decrit(ss$simule_gain - ss$gain > 110, weights = ss$weight) # 53%
decrit(ss$simule_gain_inelastique - ss$gain > 0, weights = ss$weight) # 77%
# decrit(ss$simule_gain_inelastique - ss$gain > 50, weights = ss$weight) # 61%
decrit(ss$simule_gain_inelastique - ss$gain > 110, weights = ss$weight) # 37%
decrit(ss$simule_gain_inelastique - n(ss$gain), weights = ss$weight) # mean 75, median 80

# Figure 1: PDF of subjective vs. objective gain
mar_old <- par()$mar
cex_old <- par()$cex
par(mar = c(2.1, 4.1, 1.1, 0.1), cex=1.5)
# (a) transport
plot(density(objective_gains$transport, bw=30), xlim=c(-400, 200), lwd=2, col="blue", xlab="", main="") + grid()
lines(density(subjective_gains$transport, bw=30), xlim=c(-400, 200), lwd=2, col="red")
# (b) housing
plot(density(objective_gains$housing, bw=30), xlim=c(-400, 200), lwd=2, col="blue", xlab="", main="") + grid()
lines(density(subjective_gains$housing, bw=30), xlim=c(-400, 200), lwd=2, col="red")
# (c) both combined 
plot(density(objective_gains$all, bw=30), xlim=c(-400, 200), lwd=2, col="blue", xlab="", main="") + grid()
lines(density(subjective_gains$all, bw=30), xlim=c(-400, 200), lwd=2, col="red")

# Figure 2: CDF of subjective vs. objective gain (including in the inelastic case)
par(mar = c(2.1, 4.1, 1.1, 0.1), cex=1.5)
# (a) transport
cdf_transport <- Ecdf(objective_gains$transport)
cdf_transport_inelastic <- Ecdf(objective_gains_inelastic$transport)
plot(Ecdf(ss$gain_fuel), type="s", lwd=2, col="red", xlab="", main="", ylab=expression("Proportion "<=" x")) + grid()
lines(cdf_transport$x, cdf_transport$y, lwd=2, col="blue")
lines(cdf_transport_inelastic$x, cdf_transport_inelastic$y, lwd=2, lty=2, col="blue")
abline(v = c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), lty=3, col=rgb(1,0,0,0.7))
axis(3, at=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), tck=0.0, lwd=0, lwd.ticks = 0, padj=1.5, col.axis="red", cex.axis=0.9)
# (b) housing
cdf_housing <- Ecdf(objective_gains$housing)
cdf_housing_inelastic <- Ecdf(objective_gains_inelastic$housing)
plot(Ecdf(ss$gain_chauffage), type="s", lwd=2, col="red", xlab="", main="", ylab=expression("Proportion "<=" x")) + grid()
lines(cdf_housing$x, cdf_housing$y, lwd=2, col="blue")
lines(cdf_housing_inelastic$x, cdf_housing_inelastic$y, lwd=2, lty=2, col="blue")
abline(v=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), lty=3, col=rgb(1,0,0,0.7))
axis(3, at=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), tck=0.0, lwd=0, lwd.ticks = 0, padj=1.5, col.axis="red", cex.axis=0.9)
# (c) both combined 
cdf_all <- Ecdf(objective_gains$all)
cdf_all_inelastic <- Ecdf(objective_gains_inelastic$all)
plot(Ecdf(ss$gain), type="s", lwd=2, col="red", xlab="", main="", ylab=expression("Proportion "<=" x")) + grid()
lines(cdf_all$x, cdf_all$y, lwd=2, col="blue")
lines(cdf_all_inelastic$x, cdf_all_inelastic$y, lwd=2, lty=2, col="blue")
abline(v=c(-280, -190, -120, -70, -30, 0, 20, 40, 60, 80), lty=3, col=rgb(1,0,0,0.7))
axis(3, at=c(-280, -190, -120, -70, -30, 0, 20, 40, 60, 80), tck=0.0, lwd=0, lwd.ticks = 0, padj=1.5, col.axis="red", cex.axis=0.9)
# restore graphical parameters
par(mar = mar_old, cex = cex_old)

# Heterogeneity in bias
ggplot(data=fit, aes(x=gain)) + theme_bw() + geom_smooth(method = "auto", aes(y=predicted_winner), se=F) + ylim(c(0,1)) + 
  xlab("Objective gain per consumption unit (density in black)") + ylab("Probability of predicting gain (in blue)") + xlim(c(-250, 200)) + geom_density(aes(y=..scaled..), bw=30) + geom_vline(xintercept=0, col='grey')
mean(fit$mistake[fit$gain > 110]) # 1%
mean(fit$mistake[fit$gain > 105 & fit$gain < 115]) # 1.2%
# mean(fit$gain > 105 & fit$gain < 115) # 1%
# mean(fit$predicted_gain - fit$gain > 110) # 2%
wtd.mean(ss$simule_gain - ss$gain > 110, weights = ss$weight) # 52%
# prediction_gain <- lm(gain ~ predicted_gain, data=fit)
# summary(prediction_gain)
# predicted_gain <- predict(prediction_gain, interval='predict', level=0.95)
# mean(predicted_gain[,3] - predicted_gain[,2]) / 2 # 107: half-length of 90% Confidence Interval

# TODO: plus de contrôles ?
formula_bias <- as.formula(paste("(simule_gain - gain > 110) ~ (sexe=='Féminin') + as.factor(taille_agglo) + (Diplome>=5) + revenu + ecologiste + Gauche_droite + uc + Gilets_jaunes + ", paste(variables_demo, collapse=' + ')))
reg_bias <- lm(formula_bias, data=ss, weights=ss$weight)
summary(reg_bias) # R^2: 0.04 (la moitié due aux gilets jaunes)
logit_bias <- glm(formula_bias, family = binomial(link='logit'), data=ss)
summary(logit_bias)
logit_bias_margins <- logitmfx(formula_bias, s, atmean=FALSE)$mfxest
logit_bias_margins

Table_heterogenous_bias <- stargazer(reg_bias, logit_bias, #
                                     title="Determinants of bias in subjective gains", model.names = T, model.numbers = FALSE, #star.cutoffs = c(0.1, 1e-5, 1e-30), # "Diploma: Bachelor or above", 
                                     covariate.labels = c("Constant", "Sex: Female", "Ecologist","Consumption Units (C.U.)", "Yellow vests: PNR","Yellow vests: understands","Yellow vests: supports", "Yellow vests: is part"),
                                     dep.var.labels = c("Estimated bias per C.U. ($\\widehat{\\gamma}-g$) > 110"), dep.var.caption = "", header = FALSE,
                                     keep = c("Constant", "Gilets_jaunes", "^uc", "Féminin", "ecologiste"),
                                     coef = list(NULL, logit_bias_margins[,1]), 
                                     se = list(NULL, logit_bias_margins[,2]),
                                     add.lines = list(c("Controls: Socio-demo, political leaning", "\\checkmark", "\\checkmark")),
                                     no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:bias")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_heterogenous_bias, fixed=TRUE), fixed=TRUE), collapse=' ')

## 3.2 Robustness to assumptions on elasticities: Same results
# Households perceived elasticities
decrit(ss$Elasticite_fuel, weights = ss$weight) # -0.43 mean perceived gasoline elasticity of French people
decrit(ss$Elasticite_fuel_perso, weights = ss$weight * ss$depense_carburants) # -0.36 perceived own gasoline elasticity (weighted by share in aggregate spending: only 'mean' is meaningful)
decrit(ss$Elasticite_chauffage, weights = ss$weight) # -0.41 mean perceived housing elasticity of French people
decrit(ss$Elasticite_chauffage_perso, weights = ss$weight * ss$depense_chauffage) # -0.33 perceived own housing elasticity (weighted by share in aggregate spending: only 'mean' is meaningful)
decrit(ss$elasticite_fuel_perso, weights = ss$weight)
decrit(ss$elasticite_chauffage_perso, weights = ss$weight) 
# Reasons for lack of elasticity (constraint vs absence of consumption)
wtd.mean((ss$elasticite_chauffage_perso == '0% - Je n\'en consomme déjà pas') / (ss$Elasticite_chauffage_perso == 0), weights=ss$weight, na.rm = T) # 61%
wtd.mean((ss$elasticite_fuel_perso == '0% - Je suis contraint sur tous mes déplacements') / (ss$Elasticite_fuel_perso == 0), weights=ss$weight, na.rm = T) # 64%
# Below are non weighted results for the share of winners in the inelastic case. For the weighted results, see consistency_beliefs_losses.py (setting elasticities to 0 in gains_losses_data.py)
mean(objective_gains_inelastic$all > 0)
mean(objective_gains_inelastic$transport > 0)
mean(objective_gains_inelastic$housing > 0)
wtd.mean((ss$Elasticite_chauffage <= -0.5)[ss$taxe_efficace=='Non'], weights = ss$weight[ss$taxe_efficace=='Non']) # 45%
wtd.mean((ss$Elasticite_fuel <= -0.5)[ss$taxe_efficace=='Non'], weights = ss$weight[ss$taxe_efficace=='Non']) # 43%

## 3.3 Perception on other tax’ properties
# Progressivity: Same results
decrit(ss$progressivite, weights = ss$weight) # 19.4% vs. 59.5%


##### 4 Are beliefs well anchored? #####
## 4.1 Self-interest
# Raw results
mar_old <- par()$mar
cex_old <- par()$cex
par(mar = c(0.1, 3.1, 2.1, 0), cex.lab=1.2)
decrit(ss$simule_gagnant, weights = ss$weight) # objective winning category
# Figure 3 ,Tables VII and VIII: Transition matrix among simulated ...
# (a) winners
decrit(ss$gagnant_categorie[ss$simule_gagnant==1], weights = ss$weight[ss$simule_gagnant==1])
decrit(ss$gagnant_feedback_categorie[ss$simule_gagnant==1], weights = ss$weight[ss$simule_gagnant==1])
crosstab_simule_gagnant <- crosstab(ss$winning_category[ss$simule_gagnant==1], ss$winning_feedback_category[ss$simule_gagnant==1], 
                                    ss$weight[ss$simule_gagnant==1], # dnn=c(expression('Winning category,'~bold(Before)~feedback), ''),
                                    prop.r=T, sort=2:1, cex.axis=0.9) # sort=2:1, dir=c("h", "v"), inv.x=T, inv.y=T, color = FALSE # see mosaicplot
crosstab_simule_gagnant
plot(crosstab_simule_gagnant, sort=2:1, cex.axis=0.9, ylab = expression('Winning category,'~bold(Before)~feedback), xlab=NA)
mtext(side=3, expression('Winning category,'~bold(After)~feedback), line=0.8, cex = 1.2)
# (b) losers
decrit(ss$gagnant_categorie[ss$simule_gagnant==0], weights = ss$weight[ss$simule_gagnant==0])
decrit(ss$gagnant_feedback_categorie[ss$simule_gagnant==0], weights = ss$weight[ss$simule_gagnant==0])
crosstab_simule_perdant <- crosstab(ss$winning_category[ss$simule_gagnant==0], ss$winning_feedback_category[ss$simule_gagnant==0], 
                                    ss$weight[ss$simule_gagnant==0], # dnn=c(expression('Winning category,'~bold(Before)~feedback), ''),
                                    prop.r=T, sort=2:1, cex.axis=0.9) # sort=2:1, dir=c("h", "v"), inv.x=T, inv.y=T, color = FALSE # see mosaicplot
plot(crosstab_simule_perdant, sort=2:1, cex.axis=0.9, ylab = expression('Winning category,'~bold(Before)~feedback), xlab=NA)
mtext(side=3, expression('Winning category,'~bold(After)~feedback), line=0.8, cex = 1.2)
par(mar = mar_old, cex = cex_old)

# Conservative updating: update less correctly
decrit(ss$feedback_infirme_large, weights = ss$weight) # 70%
decrit(ss$update_correct[ss$feedback_infirme_large==T], weights = ss$weight[ss$feedback_infirme_large==T]) # 18%

# Asymmetric updating
base_winner <- lm(update_correct ~ gagnant_categorie=='Gagnant', subset = feedback_infirme_large==T, data=ss, weights = ss$weight)
summary(base_winner)

# 4.2 Beliefs over environmental effectiveness: cf. 5.2, other variables than taxe_efficace are not correlated with our information
# (1bis) logit 1st stage
logit_ee1 <- glm(taxe_efficace!='Non' ~ apres_modifs + info_CC * info_PM, family = binomial(link='logit'), data=ss) # I(info_CC==1 | info_PM==1) + I(info_PM==0 & info_CC==1) + I(info_PM==1 & info_CC==0)
summary(logit_ee1)
logit_ee1_margins <- logitmfx(data=ss, formula=logit_ee1, atmean=FALSE)$mfxest
logit_ee1_margins

# 4.3 Beliefs over progressivity: REACTANCE: people are LESS prone to believe in progressivity after the information.
ols_prog_1 <- lm(progressivite!='Non' ~ info_progressivite, data=ss, weights=ss$weight, na.action = 'na.exclude')
summary(ols_prog_1)
ss$progressif <- fitted.values(ols_prog_1)
# 50 p.p.***
summary(lm(taxe_cible_approbation!='Non' ~ fitted.values(ols_prog_1) + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2, data=ss, weights = ss$weight))
summary(tsls2_si1)
summary(lm(progressivite!='Non' ~ info_progressivite, data=s, subset=s$simule_gain - s$gain < 110, weights=s$weight)) # No significant effect on those with low bias
summary(lm(progressivite!='Non' ~ info_progressivite, data=s, subset=s$simule_gain - s$gain < 50, weights=s$weight))
ols_prog_2 <- lm(gagnant_info_categorie!='Perdant' ~ info_progressivite * revenu + info_progressivite * rev_tot + info_progressivite * I(rev_tot^2), data=ss, weights=ss$weight)
summary(ols_prog_2)
cor(ss$info_progressivite, (ss$progressivite!='Non'), use='complete.obs') # -0.006


##### 5 Motives for acceptance #####
## 5.1 Self-interest
# Identification challenge
sum(ss$weight[ss$simule_gagnant==1])/sum(ss$weight) # 76%
sum(ss$weight[ss$taxe_approbation=='Non' & ss$gagnant_categorie!='Gagnant' & ss$simule_gagnant==1])/sum(ss$weight[ss$simule_gagnant==1]) # 62%
# sum(ss$weight[ss$taxe_approbation=='Non' & ss$gagnant_categorie!='Gagnant'])/sum(ss$weight) # 66%
# sum(ss$weight[ss$taxe_approbation=='Non' & ss$gagnant_categorie=='Perdant'])/sum(ss$weight) # 55%

# (1) Main identification strategy
tsls1_si1 <- lm(gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + 
                  I(traite_cible*traite_cible_conjoint) + cible + Revenu + I(Revenu^2) + Revenu_conjoint + I(Revenu_conjoint^2), data=ss, weights = ss$weight)
summary(tsls1_si1)
ss$non_perdant <- tsls1_si1$fitted.values
# 50 p.p.***
tsls2_si1 <- lm(taxe_cible_approbation!='Non' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2, data=ss, weights = ss$weight)
summary(tsls2_si1)

# Alternative specifications for robustness checks
# (2) With many controls 
variables_reg_self_interest <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "I(hausse_depenses_interaction/uc)", "taxe_efficace", variables_demo, variables_politiques) # 
variables_reg_self_interest <- variables_reg_self_interest[!(variables_reg_self_interest %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
formula_tsls1_si2 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + 
                                      I(traite_cible*traite_cible_conjoint) + cible + tax_acceptance +  (taxe_approbation=='NSP') +", paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si2 <- lm(formula_tsls1_si2, data=ss, weights = ss$weight)
summary(tsls1_si2)
ss$non_perdant <- tsls1_si2$fitted.values
# 52 p.p.***
formula_tsls2_si2 <- as.formula(paste("taxe_cible_approbation!='Non' ~ non_perdant + cible + tax_acceptance + I(taxe_approbation=='NSP') + ", 
                                      paste(variables_reg_self_interest, collapse = ' + '))) # 
tsls2_si2 <- lm(formula_tsls2_si2, data=ss, weights = ss$weight)
summary(tsls2_si2)

# (3) Simple OLS (same results and same distinction as before for 'bis' or not)
ss$non_perdant <- n(ss$gagnant_cible_categorie!='Perdant')
ols_si3 <- lm(formula_tsls2_si2, data=ss, weights = ss$weight)
summary(ols_si3)

# (4) Simple Logit: 53 p.p.***
ss$non_perdant <- n(ss$gagnant_cible_categorie!='Perdant')
# Warning when weighting: it relates to number of trials and not to survey weights. 
# TODO: use svyglm to weight correctly cf. https://stats.stackexchange.com/questions/57107/use-of-weights-in-svyglm-vs-glm
logit_si4 <- glm(formula_tsls2_si2, family = binomial(link='logit'), data=ss)
summary(logit_si4)
logit_si4_margins <- logitmfx(formula_tsls2_si2, s, atmean=FALSE)$mfxest
logit_si4_margins

# (5) IV Feedback
formula_tsls1_si5 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + Simule_gain + Simule_gain2 + taxe_efficace +", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si5 <- lm(formula_tsls1_si5, data=ss, subset=variante_taxe_info=='f', weights = ss$weight, na.action='na.exclude')
summary(tsls1_si5)
ss$non_perdant[ss$variante_taxe_info=='f'] <- tsls1_si5$fitted.values
# 43 p.p. ***
formula_tsls2_si5 <- as.formula(paste("taxe_feedback_approbation!='Non' ~ non_perdant + tax_acceptance + (taxe_approbation=='NSP') + Simule_gain + Simule_gain2 + taxe_efficace +", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
tsls2_si5 <- lm(formula_tsls2_si5, data=s[ss$variante_taxe_info=='f',], weights = ss$weight[ss$variante_taxe_info=='f'])
summary(tsls2_si5)

# Results
TableV <- stargazer(tsls2_si1, tsls2_si2, ols_si3, logit_si4, tsls2_si5, # tsls2_si4: Unrecognized object type
                    title="Effect of self-interest on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                    covariate.labels = c("Believes does not lose", "Initial tax Acceptance ($A^I$)", "",  "Environmentally effective: `Yes'"),
                    dep.var.labels = c("Targeted Acceptance ($A^T$)", "Feedback Acceptance ($A^F$)"), dep.var.caption = "", header = FALSE,
                    keep = c("non_perdant", "tax_acceptance"),
                    coef = list(NULL, NULL, NULL, logit_si4_margins[,1], NULL), 
                    se = list(NULL, NULL, NULL, logit_si4_margins[,2], NULL),
                    add.lines = list(
                      # "Method: 2SLS & \\checkmark & \\checkmark &  & \\checkmark",
                      c("Controls: Incomes ", "\\checkmark ", "\\checkmark ", "\\checkmark  ", "\\checkmark ", "\\checkmark"),
                      c("Controls: Estimated gain ", "", "\\checkmark ", "\\checkmark ", "\\checkmark ", "\\checkmark"),
                      c("Controls: Target of the tax ", "\\checkmark ", "\\checkmark ", "\\checkmark ", "\\checkmark  ", ""),
                      c("Controls: Socio-demo, political leaning ", "", "\\checkmark ", "\\checkmark ", "\\checkmark  ", "\\checkmark  ")),
                    no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="results_private_benefits")
write_clip(sub("\\multicolumn{3}{c}{\\textit{OLS}} & \\textit{logistic} & \\textit{OLS}", "\\multicolumn{2}{c}{\\textit{IV}} & \\textit{OLS} & \\textit{logit} & \\textit{IV}", 
               gsub('\\end{table}', '} {\\footnotesize \\\\ \\quad \\\\ \\textsc{Note:} Standard errors are reported in parentheses. For logit, average marginal effects are reported and not coefficients. }\\end{table}', 
                    gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableV, fixed=TRUE), fixed=TRUE), fixed=T), collapse=' ')

TableXI <- stargazer(tsls1_si1, tsls1_si2, tsls1_si5,
                     title="First stage regressions results for self-interest", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                     covariate.labels = c("Constant", "Transfer to respondent ($T_1$)", "Transfer to spouse ($T_2$)",
                                          "$T_1 \\times T_2$", "Simulated winner ($\\widehat{\\Gamma}$)",
                                          "Initial tax Acceptance ($A^I$)"),
                     dep.var.labels = c("Targeted tax ($G^T$)", "After feedback ($G^F$)"), dep.var.caption = "Believes does not lose", header = FALSE,
                     keep = c("Constant", "traite", "acceptance", "simule_gagnant"),
                     add.lines = list(c("Controls: Incomes", " \\checkmark", " \\checkmark", " \\checkmark"),
                                      c("Controls: Estimated gain", "", " \\checkmark ", " \\checkmark"),
                                      c("Controls: Target of the tax", " \\checkmark", " \\checkmark", " "),
                                      c("Controls: Socio-demo, political leaning", "", " \\checkmark", " \\checkmark")),
                     no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="first_stage_private_benefits")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableXI, fixed=TRUE), fixed=TRUE), collapse=' ')


## 5.2 Environmental effectiveness
# Main identification strategy
# (1) 2SLS both instruments, no controls: 52 p.p.*
tsls1_ee1 <- lm(taxe_efficace!='Non' ~ apres_modifs + info_CC * info_PM, data=ss, weights=ss$weight) # I(info_CC==1 | info_PM==1) + I(info_PM==0 & info_CC==1) + I(info_PM==1 & info_CC==0)
summary(tsls1_ee1)
ss$taxe_efficace.hat <- tsls1_ee1$fitted.values
tsls2_ee1 <- lm(tax_acceptance ~ taxe_efficace.hat, data=ss, weights=ss$weight)
summary(tsls2_ee1)
summary(lm(tax_acceptance ~ info_CC * info_PM, data=ss, weights=ss$weight)) # info_CC is a good instrument

# Alternative specifications for robustness checks
# (2) 2SLS both instruments, with controls: 56 p.p.*
variables_reg_ee <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "Simule_gain", "Simule_gain2", "gagnant_categorie", variables_demo)
variables_reg_ee <- variables_reg_ee[!(variables_reg_ee %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
formula_ee2 <- as.formula(paste("taxe_efficace!='Non' ~ apres_modifs + info_CC * info_PM + ", 
                                paste(variables_reg_ee, collapse = ' + ')))
tsls1_ee2 <- lm(formula_ee2, data=ss, weights = ss$weight, na.action='na.exclude')
summary(tsls1_ee2)
ss$taxe_efficace.hat <- fitted.values(tsls1_ee2)
formula2_ee2 <- as.formula(paste("tax_acceptance ~ taxe_efficace.hat + ",paste(variables_reg_ee, collapse = ' + ')))
tsls2_ee2 <- lm(formula2_ee2, data=ss, weights=ss$weight)
summary(tsls2_ee2)

# (3) OLS with controls:
# 42 p.p.
ss$taxe_efficace.hat <- n(ss$taxe_efficace!='Non')
formula_ee3 <- as.formula(paste("tax_acceptance ~ taxe_efficace.hat + ", paste(variables_reg_ee, collapse = ' + '))) # 
ols_ee3 <- lm(formula_ee3, data=ss, weights = ss$weight)
summary(ols_ee3)

# (4) Logit
# 46 p.p.
ss$taxe_efficace.hat <- n(ss$taxe_efficace!='Non')
logit_ee4 <- glm(formula_ee3, family = binomial(link='logit'), data=ss)
summary(logit_ee4)
logit_ee4_margins <- logitmfx(data=ss, formula=logit_ee4, atmean=FALSE)$mfxest
logit_ee4_margins

# (5) IV, no controls and efficace is yes:
# 56 p.p.
tsls1_ee5 <- lm((taxe_efficace=='Oui') ~ apres_modifs + info_CC * info_PM, data=ss, weights=ss$weight)
summary(tsls1_ee5)
ss$taxe_efficace.yes <- tsls1_ee5$fitted.values
tsls2_ee5 <- lm(tax_acceptance ~ taxe_efficace.yes, data=ss, weights=ss$weight)
summary(tsls2_ee5)

# (6) IV, no controls and approval:
tsls1_ee6 <- lm((taxe_efficace!='Non') ~ apres_modifs + info_CC * info_PM, data=ss, weights=ss$weight)
summary(tsls1_ee6)
ss$taxe_efficace.hat <- tsls1_ee6$fitted.values
tsls2_ee6 <- lm(tax_approval ~ taxe_efficace.hat, data=ss, weights=ss$weight)
summary(tsls2_ee6)

# Results
TableVI <- stargazer(tsls2_ee1, tsls2_ee2, ols_ee3, logit_ee4, tsls2_ee5, tsls2_ee6,
                     title="Effect of believing in environmental effectiveness on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                     covariate.labels = c("Environmental effectiveness: not `No'", "Environmental effectiveness: `Yes'"), # "Constant",
                     dep.var.labels = c("Tax Acceptance ($A^I$)", "Tax Approval ($\\dot{A^I}$)"), dep.var.caption = "", header = FALSE,
                     keep = c("efficace"), # "Constant",
                     coef = list(NULL, NULL, NULL, logit_ee4_margins[,1], NULL, NULL), 
                     se = list(NULL, NULL, NULL, logit_ee4_margins[,2], NULL, NULL),
                     add.lines = list(c("Instruments: info E.E., C.C. \\& P.M. ", "\\checkmark ", "\\checkmark ", "", " ", "\\checkmark ", "\\checkmark"),
                                      c("Controls: Socio-demographics ", "", "\\checkmark ", "\\checkmark  ", "\\checkmark ", "", "")), 
                     no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:ee")
write_clip(sub("\\multicolumn{3}{c}{\\textit{OLS}} & \\textit{logistic} & \\textit{OLS} & \\textit{OLS}", 
               "\\textit{IV} & \\textit{IV} & \\textit{OLS} & \\textit{logit} & \\textit{IV} & \\textit{IV}", 
               gsub('\\end{table}', '} {\\footnotesize \\\\ \\quad \\\\ \\textsc{Note:} Standard errors are reported in parentheses. For logit, average marginal effects are reported and not coefficients. }\\end{table}', 
                    gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableVI, fixed=TRUE), fixed=TRUE), fixed=TRUE), collapse=' ')

TableXII <- stargazer(tsls1_ee1, tsls1_ee2, tsls1_ee5,
                      title="First stage regressions results for environmental effectiveness", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                      # "Info on Climate Change and/or on Particulates", "Info on Climate Change only", "Info on Particulates only"
                      covariate.labels = c("Constant", "Info on Environmental Effectiveness ($Z_{EE}$)",  
                                           "Info on Climate Change ($Z_{CC}$)", "Info on Particulate Matter ($Z_{PM}$)", "$Z_{CC} \\times Z_{PM}$"), 
                      dep.var.labels = c("not `No'", "`Yes'"), dep.var.caption = "Environmental effectiveness", header = FALSE,
                      keep = c("Constant", "info", "apres_modifs"), 
                      column.labels = c("(1, 6)", "(2)", "(5)"), model.numbers = FALSE,
                      add.lines = list(c("Controls ", "", "\\checkmark ", "")), 
                      no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="first_stage_environmental_effectiveness")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableXII, fixed=TRUE), fixed=TRUE), collapse=' ')

# Who is convinced in our LATE: Correlates of sensitivity to information on beliefs over effectiveness
ss$info_ee <- 1*(ss$apres_modifs==T) + (ss$info_CC==1) + (ss$info_PM==1)
ols_ee <- lm(as.formula(paste("taxe_efficace!='Non' ~ ", paste(paste(c("Gilets_jaunes", "revenu", "Gauche_droite", "taille_agglo", "ecologiste", "sexe", "(Diplome>4)", "statut_emploi"), 
                                                                     collapse=' + apres_modifs * '), paste(c("Gilets_jaunes", "revenu", "Gauche_droite", "taille_agglo", "ecologiste", "sexe", "(Diplome>4)", "statut_emploi"), 
                                                                                                           collapse=' + info_CC * info_PM * '), sep=' + '))), weights=ss$weight, data=ss)
summary(ols_ee)
ols_ee_sans_interaction <- lm(as.formula(paste("taxe_efficace!='Non' ~ apres_modifs * info_CC * info_PM +", paste(c("Gilets_jaunes", "revenu", "Gauche_droite", "taille_agglo", "ecologiste", "sexe", "(Diplome>4)", "statut_emploi"), 
                                                                                                                  collapse=' + '))), weights=ss$weight, data=ss)
summary(ols_ee_sans_interaction)
anova(ols_ee_sans_interaction, ols_ee) # We almost reject that interactions terms have no effect: compliers are a bit different from others


## 5.3 Progressivity
# Identification challenge and strategies

variables_reg_prog <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "Simule_gain", "Simule_gain2", variables_demo, variables_energie)
variables_reg_prog <- variables_reg_prog[!(variables_reg_prog %in% 
                                             c("revenu", "rev_tot", "age", "age_65_plus", "fioul", "gaz", "hausse_chauffage", "hausse_essence", "hausse_diesel", "hausse_depenses", "simule_gain"))]

summary(lm(as.formula(paste("progressif ~ ", paste(variables_reg_prog, collapse=' + '))), weights=ss$weight, data=ss))
variables_correlees_prog <- c("Revenu", "Revenu2", "gagnant_categorie", "taxe_efficace", "sexe", "diplome4", "surface")

ss$progressif <- ss$progressivite!='Non'
# (1) OLS with controls and interactions: effect only when interacted (with 101 control variables and no interaction: 27 p.p.***)
formula_ols_prog1 <- as.formula(paste("tax_acceptance ~ progressif + ", paste(paste(variables_reg_prog, collapse=' + '), 
                                                                              paste(c("(taxe_efficace!='Non')", "(gagnant_categorie!='Perdant')"), collapse=' + progressif * '), sep=" + (gagnant_categorie!='Perdant') * (taxe_efficace!='Non') * progressif + progressif * ")))
ols_prog1 <- lm(formula_ols_prog1, weights=ss$weight, data=ss)
summary(ols_prog1)

# # (1bis) OLS with controls: 27 p.p.*** (same result with 101 control variables)
# formula_ols_prog1bis <- as.formula(paste("tax_acceptance ~ progressif + ", paste(variables_reg_prog, collapse=' + ')))
# ols_prog1bis <- lm(formula_ols_prog1bis, weights=ss$weight, data=ss)
# summary(ols_prog1bis)

# (2) OLS simple: 38 p.p.***
ols_prog2 <- lm(tax_acceptance ~ progressif, weights=ss$weight, data=ss)
summary(ols_prog2)

# (3) Logit with controls and interaction: pareil
logit_prog3 <- glm(tax_acceptance ~ progressif, family = binomial(link='logit'), data=ss)
summary(logit_prog3)
logit_prog3_margins <- logitmfx(logit_prog3, data=ss, atmean=FALSE)$mfxest
logit_prog3_margins

# (4) OLS simple: progressivite is yes / approve
ols_prog4 <- lm(tax_approval ~ (progressivite == 'Oui'), weights=ss$weight, data=ss)
summary(ols_prog4)

TableVII <- stargazer(ols_prog1, ols_prog2, logit_prog3, ols_prog4,
                      title="Effect of beliefs over progressivity on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                      covariate.labels = c("Progressivity: not `No' ($P>0$)",
                                           "Interaction: not lose $(P>0) \\times (G>0)$", "Interaction: effective $(P>0) \\times (EE>0)$", "$(P>0) \\times (G>0) \\times (EE>0)$", "Progressivity: `Yes' ($\\dot{P}>0$)"), # "Constant",
                      dep.var.labels = c("Tax Acceptance ($A^I$)", "Tax Approval ($\\dot{A^I}$)"), dep.var.caption = "", header = FALSE,
                      keep = c("progressi"), # "Constant"
                      coef = list(NULL, NULL, logit_prog3_margins[,1], NULL),
                      se = list(NULL, NULL, logit_prog3_margins[,2], NULL),
                      add.lines = list(c("Controls: Socio-demo, energy, G, EE, $G \\times EE$", "\\checkmark ", " ", "", "")), 
                      no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:progressivity")
write_clip(gsub('\\end{table}', '} {\\footnotesize \\\\ \\quad \\\\ \\textsc{Note:} Standard errors are reported in parentheses. For logit, average marginal effects are reported and not coefficients. } \\end{table} ', 
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableVII, fixed=TRUE), fixed=TRUE), collapse=' ')


##### Interaction âge - gagnant #####
# Les pauvres sont plus jeunes que la moyenne, et ils pourraient s'opposer à la taxe en prévision qu'ils deviendront riches plus tard. 
# résultat : les jeunes gagnants approuvent plus que les vieux gagnants... sauf pour les transferts ciblés !
summary(lm(tax_acceptance ~ as.factor(age), data=s, subset=gagnant_categorie=='Gagnant' & progressivite=='Oui', weights=s$weight))
summary(lm(tax_acceptance ~ as.factor(age), data=s, subset=gagnant_feedback_categorie=='Gagnant' & progressivite=='Oui', weights=s$weight))
summary(lm(tax_acceptance ~ as.factor(age), data=s, subset=gagnant_categorie=='Gagnant', weights=s$weight))
summary(lm(tax_acceptance ~ as.factor(age), data=s, subset=gagnant_feedback_categorie=='Gagnant', weights=s$weight))
summary(lm(tax_acceptance ~ as.factor(age), data=s, subset=gagnant_categorie=='Perdant' & simule_gain - gain > 110, weights=s$weight))
summary(lm(tax_acceptance ~ as.factor(age), data=s, subset=gagnant_categorie=='Gagnant' & simule_gain - gain > 110, weights=s$weight))
summary(lm(taxe_cible_approbation!='Non' ~ as.factor(age), data=s, subset=gagnant_cible_categorie=='Gagnant', weights=s$weight))
summary(lm(taxe_cible_approbation!='Non' ~ age, data=s, subset=gagnant_cible_categorie=='Gagnant', weights=s$weight))
summary(lm(taxe_cible_approbation!='Non' ~ as.factor(age) * (gagnant_cible_categorie=='Gagnant'), data=s, weights=s$weight))
summary(lm(taxe_cible_approbation!='Non' ~ (gagnant_cible_categorie=='Gagnant'), data=s, weights=s$weight))


##### Willingness-To-Pay ~ 60€ #####
ggplot() + geom_smooth(data=s[s$taxe_efficace!='Non',], method = "auto", aes(x=gain, y=1*(tax_acceptance), col=" Effective: not `No'")) + ylim(c(0,1)) +
  xlab("Subjective gain, among non believers in ineffectiveness") + ylab("Acceptance rate") + geom_hline(yintercept=0.5, col='red') + theme_bw() + #geom_vline(xintercept=-66, col='red') +
  geom_smooth(data=s, method = "auto", aes(x=gain, y=1*(tax_acceptance), col=' All            ')) + ylim(c(0,1)) +
  xlab("Subjective gain") + ylab("Acceptance rate") + geom_hline(yintercept=0.5, col='red') + theme_bw() + theme(legend.position="top", legend.title=element_blank())
ma <- loess(tax_acceptance ~ gain, data=s, subset=taxe_efficace!='Non', weights=s$weight)
map <- predict(ma)[order(predict(ma))]
predict(ma, -57) # but from the graph, the intersection with 50% is at -66

ggplot(data=s[s$taxe_efficace=='Oui',], aes(x=gain)) + geom_smooth(method = "auto", aes(y=1*(tax_acceptance))) + ylim(c(0,1)) +
  xlab("Subjective gain, among believers in effectiveness") + ylab("Acceptance rate") + geom_hline(yintercept=0.5, col='red') + theme_bw()

summary(lm(as.formula(paste("tax_acceptance ~ (taxe_efficace!='Non') + gain * progressivite + ", paste(c(variables_demo, variables_energie, variables_politiques), collapse=' + '))), data=s, weights=s$weight))
0.4243/7.392e-4
0.3721/1.276e-3
summary(lm(tax_acceptance ~ (taxe_efficace!='Non') + gain, data=s, weights=s$weight))


##### Variance explained (McFadden PseudoR2) #####
# 0.25 when priming is most salient
logit_si <- glm(taxe_info_approbation!='Non' ~ gagnant_info_categorie, data=s, family = "binomial")
PseudoR2(logit_si) # 0.25
logit_ee <- glm(taxe_approbation!='Non' ~ taxe_efficace, data=s, family = "binomial")
PseudoR2(logit_ee) # 0.24
logit_p <- glm(taxe_info_approbation!='Non' ~ progressivite, data=s, subset = !is.na(progressivite), family = "binomial")
PseudoR2(logit_p) # 0.25

# 0.14-0.17 when priming is not salient
logit_si <- glm(taxe_approbation!='Non' ~ gagnant_categorie, data=s, family = "binomial")
PseudoR2(logit_si) # 0.15
logit_ee <- glm(taxe_info_approbation!='Non' ~ taxe_efficace, data=s, family = "binomial")
PseudoR2(logit_ee) # 0.17
logit_p <- glm(taxe_approbation!='Non' ~ progressivite, data=s, subset = !is.na(progressivite), family = "binomial")
PseudoR2(logit_p) # 0.14

logit_si <- glm(taxe_info_approbation!='Non' ~ gagnant_info_categorie, data=s, family = "binomial")
PseudoR2(logit_si) # 0.25
logit_ee <- glm(taxe_approbation!='Non' ~ taxe_efficace, data=s, family = "binomial")
PseudoR2(logit_ee) # 0.24
logit_p <- glm(taxe_info_approbation!='Non' ~ progressivite, data=s, subset = !is.na(progressivite), family = "binomial")
PseudoR2(logit_p) # 0.25

logit_sd <- glm(as.formula(paste("taxe_info_approbation!='Non' ~ ", paste(c(variables_demo, variables_energie), collapse=' + '))), data=s, subset = !is.na(progressivite), family = "binomial")
PseudoR2(logit_sd) # 0.10

# R^2
summary(lm(taxe_approbation!='Non' ~ gagnant_categorie, data=s, weights = s$weight)) # 0.18
summary(lm(taxe_info_approbation!='Non' ~ taxe_efficace, data=s, weights = s$weight))  # 0.22
summary(lm(taxe_approbation!='Non' ~ progressivite, data=s, subset = !is.na(progressivite), weights = s$weight)) # 0.17

summary(lm(taxe_info_approbation!='Non' ~ gagnant_info_categorie, data=s, weights = s$weight)) # 0.32
summary(lm(taxe_approbation!='Non' ~ taxe_efficace, data=s, weights = s$weight)) # 0.29
summary(lm(taxe_info_approbation!='Non' ~ progressivite, data=s, subset = !is.na(progressivite), weights = s$weight)) # 0.32

# R^2: 0.09, adj R^2: 0.13
summary(lm(as.formula(paste("taxe_info_approbation!='Non' ~ ", paste(c(variables_demo, variables_energie), collapse=' + '))), data=s, subset = !is.na(progressivite), family = "binomial"))


##### Trash of papier.R #####
decrit(s$simule_gagnant, weights = s$weight)
# decrit(s$simule_gain - s$gain > 50, weights = s$weight) # 75%
decrit(s$biais_sur, weights = s$weight) # 53%
# decrit(s$simule_gain_inelastique - s$gain > 50, weights = s$weight) # 61%
decrit(s$simule_gain_inelastique - s$gain > 110, weights = s$weight) # 37%
decrit(s$simule_gain_inelastique - n(s$gain), weights = s$weight) # mean 75, median 80

mean(fit$mistake[fit$gain > 105 & fit$gain < 115]) # 1.2%
# mean(fit$gain > 105 & fit$gain < 115) # 1%
# mean(fit$predicted_gain - fit$gain > 110) # 2%
prediction_gain <- lm(gain ~ predicted_gain, data=fit)
summary(prediction_gain)
predicted_gain <- predict(prediction_gain, interval='predict', level=0.95)
mean(predicted_gain[,3] - predicted_gain[,2]) / 2 # 107: half-length of 90% Confidence Interval

## 3.1.2 Robustness to assumptions on elasticities
# Households perceived elasticities
decrit(s$Elasticite_fuel, weights = s$weight) # -0.43 mean perceived gasoline elasticity of French people
decrit(s$Elasticite_fuel_perso, weights = s$weight * s$depense_carburants) # -0.36 perceived own gasoline elasticity (weighted by share in aggregate spending: only 'mean' is meaningful)
decrit(s$Elasticite_chauffage, weights = s$weight) # -0.41 mean perceived housing elasticity of French people
decrit(s$Elasticite_chauffage_perso, weights = s$weight * s$depense_chauffage) # -0.33 perceived own housing elasticity (weighted by share in aggregate spending: only 'mean' is meaningful)
decrit(s$elasticite_fuel_perso, weights = s$weight)
decrit(s$elasticite_chauffage_perso, weights = s$weight)
# Reasons for lack of elasticity (constraint vs absence of consumption)
wtd.mean((s$elasticite_chauffage_perso == '0% - Je n\'en consomme déjà pas') / (s$Elasticite_chauffage_perso == 0), weights=s$weight, na.rm = T) # 61%
wtd.mean((s$elasticite_fuel_perso == '0% - Je suis contraint sur tous mes déplacements') / (s$Elasticite_fuel_perso == 0), weights=s$weight, na.rm = T) # 64%
# Below are non weighted results for the share of winners in the inelastic case. For the weighted results, see consistency_beliefs_losses.py
(setting elasticities to 0 in gains_losses_data.py)
mean(objective_gains_inelastic$all > 0)
mean(objective_gains_inelastic$transport > 0)
mean(objective_gains_inelastic$housing > 0)
wtd.mean((s$Elasticite_chauffage <= -0.5)[s$taxe_efficace=='Non'], weights = s$weight[s$taxe_efficace=='Non']) # 45%
wtd.mean((s$Elasticite_fuel <= -0.5)[s$taxe_efficace=='Non'], weights = s$weight[s$taxe_efficace=='Non']) # 43%

wtd.mean(s$Elasticite_fuel_perso > s$Elasticite_fuel + 0.05 * (s$Elasticite_fuel %in% c(-0.22, -0.05)), weights=s$weight, na.rm = T) # 45%
wtd.mean(s$Elasticite_fuel_perso == s$Elasticite_fuel + 0.05 * (s$Elasticite_fuel %in% c(-0.22, -0.05)), weights=s$weight, na.rm = T) # 33%
wtd.mean((s$Elasticite_fuel_perso > s$Elasticite_fuel + 0.05 * (s$Elasticite_fuel %in% c(-0.22, -0.05)))[s$nb_vehicules > 0], weights=s$weight[s$nb_vehicules > 0], na.rm = T) # 45%
mean(fit$transport_tax_increase < mean(fit$transport_tax_increase)) # 59% consume less fuel than average
# 71% (resp. 80%) think they are strictly more contrained than average for fuel (resp. housing)
wtd.mean(s$Elasticite_fuel_perso > s$Elasticite_fuel, weights=s$weight, na.rm = T) # 71%
wtd.mean(s$Elasticite_chauffage_perso > s$Elasticite_chauffage, weights=s$weight, na.rm = T) # 80%
# Objective proportion of HH with higher expenditure increase in transport: 59% / housing: 67%. cf. consistency_belief_losses.py
# Objective proportion of winners in the totally inelastic case: 53%. cf. consistency_belief_losses.py (after replacing elasticities to 0 in gain_losses_data.py)
wtd.mean(s$Elasticite_chauffage_perso > s$Elasticite_chauffage + 0.05 * (s$Elasticite_chauffage %in% c(-0.22, -0.05)), weights=s$weight, na.rm = T) # 53%
wtd.mean(s$Elasticite_fuel_perso >= s$Elasticite_fuel + 0.05 * (s$Elasticite_fuel %in% c(-0.22, -0.05)), weights=s$weight, na.rm = T) # 78%
wtd.mean(s$Elasticite_chauffage_perso >= s$Elasticite_chauffage + 0.05 * (s$Elasticite_chauffage %in% c(-0.22, -0.05)), weights=s$weight, na.rm = T) # 82%
wtd.mean(s$Elasticite_fuel_perso == s$Elasticite_fuel + 0.05 * (s$Elasticite_fuel %in% c(-0.22, -0.05)), weights=s$weight, na.rm = T) # 33%
wtd.mean(s$Elasticite_chauffage_perso == s$Elasticite_chauffage + 0.05 * (s$Elasticite_chauffage %in% c(-0.22, -0.05)), weights=s$weight, na.rm = T) # 29%
wtd.mean(s$Elasticite_fuel_perso == -0.17 & s$Elasticite_fuel == -0.05, weights = s$weight) # 3%
wtd.mean(s$Elasticite_chauffage_perso == -0.17 & s$Elasticite_chauffage == -0.05, weights = s$weight) # 2%
decrit(s$elasticite_chauffage_perso, weights = s$weight) # 24% contraints
decrit(s$elasticite_fuel_perso, weights = s$weight) # 34% contraints
# more feel more constrained than average among users, i.e. those who do not consume think more that others do not consume
wtd.mean((s$Elasticite_fuel_perso - s$Elasticite_fuel + 0.05 * (s$Elasticite_fuel %in% c(-0.22, -0.05)))[!grepl("déjà", s$elasticite_fuel_perso)] > 0,
         weights=s$weight[!grepl("déjà", s$elasticite_fuel_perso)], na.rm = T) # 64%
wtd.mean((s$Elasticite_chauffage_perso - s$Elasticite_chauffage + 0.05 * (s$Elasticite_chauffage %in% c(-0.22, -0.05)))[!grepl("déjà", s$elasticite_chauffage_perso)] > 0,
         weights=s$weight[!grepl("déjà", s$elasticite_chauffage_perso)], na.rm = T) # 68%

decrit(s$Elasticite_chauffage, weights = s$weight)
decrit(s$Elasticite_fuel, weights = s$weight)

crosstab_gagnant <- crosstab(s$winning_category[s$simule_gagnant==1], rep("", length(which(s$simule_gagnant==1))),
                             s$weight[s$simule_gagnant==1], prop.r=T, sort=2:1, cex.axis=0.9,
                             ylab = expression('Winning category, '~bold(Before)~feedback), xlab=NA, col='white')
text(cex=1, x=0.5, y=c(0.3, 0.7, 0.9), c("Loser: 60%", "Unaffected: 16%", "Winner: 14%"), xpd=TRUE)
mtext(side=3, expression('Winning category,'~bold(After)~feedback), line=0.8, cex = 1.2)
plot(crosstab_simule_gagnant, sort=2:1, cex.axis=0.9, ylab = expression('Winning category, '~bold(Before)~feedback), xlab=NA)
mtext(side=3, expression('Winning category,'~bold(After)~feedback), line=0.8, cex = 1.2)
text(cex=1, x=c(0.08, 0.115, 0.39, 0.24, 0.53, 0.84, 0.65, 0.92, 0.95), y=c(0.3, 0.7, 0.9, 0.3, 0.7, 0.9, 0.3, 0.7, 0.9),
     c("12%", "22%", "79%", "18%", "63%", "13%", "70%", "15%", "8%"), xpd=TRUE)

plot(crosstab_simule_perdant, sort=2:1, cex.axis=0.9, ylab = expression('Winning category, '~bold(Before)~feedback), xlab=NA)
mtext(side=3, expression('Winning category,'~bold(After)~feedback), line=0.8, cex = 1.2)
text(cex=1, x=c(0.025, 0.05, 0.08, 0.075, 0.30, 0.21, 0.53, 0.74, 0.60), y=c(0.38, 0.83, 0.95, 0.38, 0.83, 0.95, 0.38, 0.83, 0.95),
     c("1%", "5%", "16%", "5%", "50%", "3%", "94%", "15%", "7%"), xpd=TRUE)

# Conservative updating
decrit(s$feedback_infirme_large, weights = s$weight) # 70%
decrit(s$update_correct[s$feedback_infirme_large==T], weights = s$weight[s$feedback_infirme_large==T]) # 18%

# Asymmetric updating
sum(s$weight[s$feedback_infirme & s$simule_gagnant==1])/3002 # 46%
sum(s$weight[!is.na(s$update_correct) & s$update_correct==1 & s$feedback_infirme & s$simule_gagnant==1])/sum(s$weight[!is.na(s$update_correct) &
                                                                                                                        s$feedback_infirme & s$simule_gagnant==1]) # 12%
sum(s$weight[s$feedback_infirme & s$simule_gagnant==0])/3002 # 1.6%
sum(s$weight[!is.na(s$update_correct) & s$update_correct==1 & s$feedback_infirme & s$simule_gagnant==0])/sum(s$weight[!is.na(s$update_correct) &
                                                                                                                        s$feedback_infirme & s$simule_gagnant==0]) # 82%

asymmetric_simple <- stargazer(base_winner, controled_winner, base_feedback_winner, controled_feedback_winner, covariates_update_correct,
                               title="Asymmetric updating of winning category", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                               covariate.labels = c("Constant", "Winner, before feedback ($\\dot{G}$)", "Winner, after feedback ($\\dot{G}^F$)",
                                                    "Retired", "Active", "Student", "Yellow Vests: PNR", "Yellow Vests: understands", "Yellow Vests: supports", "Yellow Vests: is part"),
                               dep.var.labels = "Correct updating ($U$)", dep.var.caption = "", header = FALSE,
                               keep = c('Constant', '.*Gagnant.*', 'retraites', 'actifs', 'etudiants', 'Gilets_jaunes'),
                               order = c('Constant', '.*Gagnant.*', 'retraites', 'actifs', 'etudiants', 'Gilets_jaunes'),
                               add.lines = list(c("Among invalidated", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                                                c("Includes controls", "", "\\checkmark", "", "\\checkmark", "\\checkmark")),
                               no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="asymmetric_simple")

# (1bis) logit 1st stage
logit_ee1 <- glm(taxe_efficace!='Non' ~ apres_modifs + info_CC * info_PM, family = binomial(link='logit'), data=s)
summary(logit_ee1)
logit_ee1_margins <- logitmfx(data=s, formula=logit_ee1, atmean=FALSE)$mfxest
logit_ee1_margins

sum(s$weight[s$taxe_approbation=='Non' & s$gagnant_categorie!='Gagnant'])/sum(s$weight) # 66%
sum(s$weight[s$taxe_approbation=='Non' & s$gagnant_categorie=='Perdant'])/sum(s$weight) # 55%

# (1bis) 2SLS both instruments, control by gagnant_categorie because it's correlated with instrument: 59 p.p.*
tsls1_ee1bis <- lm(taxe_efficace!='Non' ~ apres_modifs + info_CC * info_PM + gagnant_categorie, data=s, weights=s$weight)
summary(tsls1_ee1bis)
s$taxe_efficace.hat <- tsls1_ee1bis$fitted.values
tsls2_ee1bis <- lm(tax_acceptance ~ taxe_efficace.hat + gagnant_categorie, data=s, weights=s$weight)
summary(tsls2_ee1bis)

# (2bis) 2SLS both instruments, with controls and interaction
formula_ee2_interaction <- as.formula(paste("taxe_efficace!='Non' ~ apres_modifs + info_CC * info_PM + ",
                                            paste(c("Revenu", "Revenu2"), collapse = ' + info_CC * info_PM * '), sep='info_CC * info_PM * '))
tsls1_ee2_formula_ee2_interaction <- lm(formula_ee2_interaction, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_ee2_formula_ee2_interaction)

reg_gagnant_prog_rev <- lm(gagnant_info_categorie!='Perdant' ~ (progressivite!='Non') * Revenu, data=s, weights=s$weight)
summary(reg_gagnant_prog_rev)

# of winning + progressivity: 0.704
0.223 + 0.332 + 0.183 + (0.127 + 0.172 - 0.400) * wtd.mean(s$taxe_efficace!='Non', weights = s$weight)
# of effective + progressivity: 0.615
0.223 + 0.258 + 0.172 + (0.183 + 0.127 - 0.400) * wtd.mean(s$gagnant_info_categorie!='Perdant', weights = s$weight)
# of effectiveness
0.244 + 0.126 * wtd.mean(s$gagnant_info_categorie=='Gagnant', weights = s$weight) + 0.281 * wtd.mean(s$progressivite=='Oui', weights = s$weight) -
  0.314 * wtd.mean(s$progressivite=='Oui' & s$gagnant_info_categorie=='Gagnant', weights = s$weight)
# Of everything: 0.895
0.223 + 0.332 + 0.258 + 0.127 + 0.183 + 0.172 - 0.400

# Average effect of Progressivity (Yes for approval), other things equal: 0.273
0.228 + 0.098 * wtd.mean(s$gagnant_info_categorie=='Gagnant', weights = s$weight) + 0.281 * wtd.mean(s$taxe_efficace=='Oui', weights = s$weight) -
  0.314 * wtd.mean(s$taxe_efficace=='Oui' & s$gagnant_info_categorie=='Gagnant', weights = s$weight)
# of winning: 0.331
0.303 + 0.126 * wtd.mean(s$taxe_efficace=='Oui', weights = s$weight) + 0.098 * wtd.mean(s$progressivite=='Oui', weights = s$weight) -
  0.314 * wtd.mean(s$taxe_efficace=='Oui' & s$progressivite=='Oui', weights = s$weight)
# of winning + effective: 0.686
0.303 + 0.244 + 0.126 + (0.098 + 0.281 - 0.314) * wtd.mean(s$progressivite=='Oui', weights = s$weight)


# Approval for fully corrected beliefs and 70.3% of winners:
moy_interaction_gagnant_efficace <- wtd.mean((s$gagnant_info_categorie=='Gagnant') * (s$taxe_efficace=='Oui'), weights = s$weight)
moy_interaction_gagnant_progressif <- wtd.mean((s$gagnant_info_categorie=='Gagnant')[(s$prog_na!='NA')] * (s$progressivite=='Oui')[(s$prog_na!='NA')], weights = s$weight[(s$prog_na!='NA')])
moy_interaction_efficace_progressif <- wtd.mean((s$taxe_efficace=='Oui')[(s$prog_na!='NA')] * (s$progressivite=='Oui')[(s$prog_na!='NA')], weights = s$weight[(s$prog_na!='NA')])
moy_interaction_trois_motifs <- wtd.mean((s$gagnant_info_categorie=='Gagnant')[(s$prog_na!='NA')] * (s$taxe_efficace=='Oui')[(s$prog_na!='NA')] * (s$progressivite=='Oui')[(s$prog_na!='NA')],
                                         weights = s$weight[(s$prog_na!='NA')])

approval_correct_beliefs <- wtd.mean(s$taxe_info_approbation=='Oui', weights = s$weight) + (
  0.228 * (1 -  wtd.mean(s$progressivite=='Oui', weights = s$weight)) + 0.303 * (0.703 -  wtd.mean(s$gagnant_info_categorie=='Gagnant', weights = s$weight)) +
    0.244 * (1 - wtd.mean(s$taxe_efficace=='Oui', weights = s$weight))
  + 0.126 * (0.703 -  moy_interaction_gagnant_efficace) + 0.098 * (0.703 - moy_interaction_gagnant_progressif) + 0.281 * (1 - moy_interaction_efficace_progressif)
  - (0.703 -  moy_interaction_trois_motifs) * 0.314
)
approval_correct_beliefs

# 5.2 Who is convinced in our LATE: Correlates of sensitivity to information on beliefs over effectiveness
s$info_ee <- 1*(s$apres_modifs==T) + (s$info_CC==1) + (s$info_PM==1)
ols_ee <- lm(as.formula(paste("taxe_efficace!='Non' ~ ", paste(paste(c("Gilets_jaunes", "revenu", "Gauche_droite", "taille_agglo", "ecologiste", "sexe", "(Diplome>4)", "statut_emploi"), 
                                                                     collapse=' + apres_modifs * '), paste(c("Gilets_jaunes", "revenu", "Gauche_droite", "taille_agglo", "ecologiste", "sexe", "(Diplome>4)", "statut_emploi"), 
                                                                                                           collapse=' + info_CC * info_PM * '), sep=' + '))), weights=s$weight, data=s)
summary(ols_ee)
ols_ee_sans_interaction <- lm(as.formula(paste("taxe_efficace!='Non' ~ apres_modifs * info_CC * info_PM +", 
                                               paste(c("Gilets_jaunes", "revenu", "Gauche_droite", "taille_agglo", "ecologiste", "sexe", "(Diplome>4)", "statut_emploi"), collapse=' + '))), weights=s$weight, data=s)
summary(ols_ee_sans_interaction)
anova(ols_ee_sans_interaction, ols_ee) # We reject at 6% that interactions terms have no effect: compliers are a bit different from others

summary(lm(as.formula(paste("progressif ~ ", paste(variables_reg_prog, collapse=' + '))), weights=s$weight, data=s))
variables_correlees_prog <- c("Revenu", "Revenu2", "gagnant_categorie", "taxe_efficace", "sexe", "diplome4", "surface")


s$temp <- s$taxe_cible_approbation!='Non'
s$non_perdant <- s$gagnant_cible_categorie!='Perdant'
logitmfx(temp ~ non_perdant, s, atmean=FALSE)$mfxest
logit_si <- glm(temp ~ non_perdant, data=s, family = binomial(link='logit'))
summary(logit_si)
summary(margins(logit_si))

for (v in c(variables_reg_self_interest, "taxe_approbation", "prog_na", "taxe_efficace")) {
  if (!is.numeric(s[[v]])) s[[paste(v, 'unclass', sep='_')]] <- as.factor(s[[v]])
  else s[[paste(v, 'unclass', sep='_')]] <- unclass(s[[v]]) }
for (v in variables_reg_self_interest) print(class(s[[paste(v, 'unclass', sep='_')]])) #
#s[[paste(v, 'unclass', sep='_')]] <- unclass(s[[v]]) # print(class(s[[paste(v, 'unclass', sep='_')]])) #
variables_reg_si_unclass <- paste(c(variables_reg_self_interest, "taxe_approbation", "prog_na", "taxe_efficace"), 'unclass', sep='_')
formula_logit_si4f  <- as.formula(paste("1*(taxe_cible_approbation!='Non') ~ non_perdant + cible +", paste(variables_reg_si_unclass, collapse = ' + '))) #
class(s$taxe_approbation=='NSP')
logit_si4f <- logistf(formula_logit_si4f, data=s, firth = T)
ggeffect(logit_si4f, "non_perdant")
logit_si4t <- glm(formula_logit_si4f, family = binomial(link='logit'), data=s)
ggemmeans(logit_si4t, "non_perdant")
summary(margins(logit_si4t))
summary(margins(logit_si4, eps=0.001)) # 0.32

# (5) IV, no controls and efficace is yes: 56 p.p. *
tsls1_ee5_bis <- lm((taxe_efficace=='Oui') ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight)
summary(tsls1_ee5_bis)
s$taxe_efficace.yes <- tsls1_ee5_bis$fitted.values
tsls2_ee5_bis <- lm(tax_acceptance ~ taxe_efficace.yes, data=s, weights=s$weight)
summary(tsls2_ee5_bis)

# (6 bis) IV, no controls and approval: 42 p.p. **
tsls1_ee6_bis <- lm((taxe_efficace=='Oui') ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight)
summary(tsls1_ee6_bis)
s$taxe_efficace.hat <- tsls1_ee6_bis$fitted.values
tsls2_ee6_bis <- lm(tax_approval ~ taxe_efficace.hat, data=s, weights=s$weight)
summary(tsls2_ee6_bis)

summary(lm(tax_acceptance ~ apres_modifs + info_CC, data=s, weights=s$weight)) # info_CC is a good instrument

#todelete
TableXVIII <- stargazer(tsls1_ee1, tsls1_ee2, tsls1_ee5,
                        title="First stage regressions results for environmental effectiveness", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                        # "Info on Climate Change and/or on Particulates", "Info on Climate Change only", "Info on Particulates only"
                        covariate.labels = c("Info on Environmental Effectiveness ($Z_{E}$)",  
                                             "Info on Climate Change ($Z_{CC}$)", "Info on Particulate Matter ($Z_{PM}$)", "$Z_{CC} \\times Z_{PM}$"), 
                        dep.var.labels = c("not ``No''", "``Yes''"), dep.var.caption = "Environmental effectiveness", header = FALSE,
                        keep = c("info", "apres_modifs"), 
                        column.labels = c("(1)", "(2)", "(5,6)"), model.numbers = FALSE,
                        add.lines = list(c("Controls ", "", "\\checkmark ", "")), 
                        no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "ser"), label="first_stage_environmental_effectiveness")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@',
                                                       TableXVIII, fixed=TRUE), fixed=TRUE), collapse=' ')

# omit = c("Diplome", "sexe", "age", "inactif", "Gilets_jaunes", "Gauche_droite", "liberal", "humaniste", "conservateur",
#          "patriote", "apolitique", "ecologiste", "interet_politique", "revenu", "taille_menage", "taille_agglo"),   
formula_ols_prog_3 <- as.formula(paste("progressivite!='Non' ~  info_progressivite * biais_sur + info_progressivite * revenu + info_progressivite * taille_agglo + 
           info_progressivite * taille_menage + info_progressivite * age + info_progressivite * Gilets_jaunes + info_progressivite * sexe + info_progressivite * inactif + 
                                       info_progressivite * (Diplome>4) + ", paste(c(variables_demo, variables_politiques), collapse = '+ ')))
ols_prog_3 <- lm(formula_ols_prog_3, data=s, weights=s$weight)
summary(ols_prog_3)


##### Suggestions Martin #####
# Regression Progressivity with causal effects: 40-56 p.p. for each motive but no significant interaction
tsls1_prog1 <- lm(gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + Simule_gain + Simule_gain2, data=s, subset=variante_taxe_info=='f', weights = s$weight)
s$gagnant_info <- NA
s$gagnant_info[s$variante_taxe_info=='f'] <- tsls1_prog1$fitted.values
formula_prog2 <- as.formula(paste("taxe_efficace!='Non' ~ apres_modifs + info_CC + ",  paste(variables_reg_ee[!(variables_reg_ee %in% c("gagnant_categorie"))], collapse = ' + ')))
tsls1_prog2 <- lm(formula_prog2, data=s, weights = s$weight, na.action='na.exclude')
s$effective <- fitted.values(tsls1_prog2)
formula_tsls_prog1 <- as.formula(paste("taxe_info_approbation!='Non' ~ progressif + ", paste(paste(variables_reg_prog, collapse=' + '), 
                                                                                            " + gagnant_info * effective * progressif + (prog_na == 'NA')")))
tsls_prog1 <- lm(formula_ols_prog1, weights=s$weight, data=s)
summary(tsls_prog1)

# McCrary (2008) test
# A p-value below the significance threshhold indicates that the user can reject the null hypothesis of no sorting.
DCdensity(pmin(s$revenu, 5000), 780, verbose=T)
DCdensity(pmin(s$revenu, 5000), 1140)
DCdensity(pmin(s$revenu, 5000), 1430)
DCdensity(pmin(s$revenu, 5000), 1670)
DCdensity(pmin(s$revenu, 5000), 2220)


##### IV model selection ####
data(card.data)
Xname=c("exper", "expersq", "black", "south", "smsa", "reg661", "reg662", "reg663", "reg664", "reg665", "reg666", "reg667", "reg668", "smsa66")
formula_ex <- as.formula(paste("lwage ~ educ + ", paste(Xname, collapse=' + '), " | nearc4 + ", paste(Xname, collapse=' + ')))
formula_ex <- as.formula("lwage ~ educ | nearc4")
iv_ex <- ivmodelFormula(formula_ex, data = card.data)
iv_ex <- ivmodelFormula(lwage ~ educ + exper + expersq + black + south + smsa + reg661 + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + smsa66 |
                          nearc4 + exper + expersq + black + south + smsa + reg661 + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + smsa66,data=card.data)
summary(ivmodelFormula(lwage ~ educ + black | nearc4 + black, data=card.data))
ARsens.test(iv_ex, deltarange=c(-0.03, 0.03))

Y=card.data[,"lwage"]
D=card.data[,"educ"]
Z=card.data[,"nearc4"]
Xname=c("exper", "expersq", "black", "south", "smsa", "reg661",
        "reg662", "reg663", "reg664", "reg665", "reg666", "reg667",
        "reg668", "smsa66")
X=card.data[,Xname]
foo = ivmodel(Y=card.data$lwage, D=D,Z=Z, X=card.data[,"black"])
summary(foo)

library(ivmodel)
library(AER)
data(card.data)
summary(ivmodel(Y=card.data$lwage, D=card.data$educ, Z=card.data$nearc4, X=card.data$exper))
summary(ivmodelFormula(lwage ~ educ + exper | nearc4 + exper, data=card.data))
summary(ivmodel(Y=card.data$lwage, D=card.data$educ, Z=card.data$nearc4))
summary(ivmodelFormula(lwage ~ educ | nearc4, data=card.data))
summary(ivreg(lwage ~ educ | nearc4, data=card.data))

library(ivmodel)
data(card.data)
library(AER)
library(lmtest)
summary(ivreg(lwage ~ educ + exper | nearc4 + exper, data=card.data), diagnostics = TRUE) 
# t: 8 (p: e-14); Wald: 29 (p< e-13); F (Weak instrument): 58 (p: e-14)
summary(ivmodel(Y=card.data$lwage, D=card.data$educ, Z=card.data$nearc4, X=card.data$exper)) 
# F: 58 (p: e-14); AR/CLR: 83 (p< e-16)
waldtest(lm(educ ~ nearc4 + exper, data=card.data), lm(educ ~ exper, data=card.data))$F[2] # 58

summary(ivreg(lwage ~ educ  + exper + black | nearc4 + nearc2 + exper + black, data=card.data), diagnostics = TRUE) 
# t: 7 (p: e-13); Wald: 80 (p< e-16); F (Weak instrument): 27.277 (p: e-12)
# card.data$region2 <- as.character(card.data$region)
# card.data$region2[card.data$region2=="661"] <- "é"
summary(ivmodel(Y=card.data$lwage, D=card.data$educ, Z=card.data[,c("nearc4", "nearc2")], X=card.data[,c("exper", "black")])) 
# F: 27.277 (p: e-14); AR: 48 (p< e-16); CLR: 94 (p< e-16)
waldtest(lm(educ ~ nearc4 + nearc2 + exper + black, data=card.data), lm(educ ~ exper + black, data=card.data))$F[2] # 27.27


ivreg(as.formula(paste("tax_acceptance ~ (taxe_efficace!='Non') + prog_not_no + (prog_na == 'NA') + ", paste(variables_reg_ee, collapse = ' + '))))
iv_ee2 <- ivreg(tax_acceptance ~ (taxe_efficace!='Non') | apres_modifs + info_CC, data = s, weights=s$weight)
summary(iv_ee2, diagnostics = TRUE) 
iv_ee1 <- ivreg(as.formula(paste("tax_acceptance ~ (taxe_efficace!='Non') + ", paste(variables_reg_ee, collapse = ' + '),
                                 "| apres_modifs + info_CC + ", paste(variables_reg_ee, collapse = ' + '))), data = s, weights=s$weight)
summary(iv_ee1, diagnostics = TRUE) # interpretation: https://rstudio-pubs-static.s3.amazonaws.com/109978_7bfc270924c1474e924dddf42e998d42.html

summary(ivmodelFormula(tax_acceptance ~ (taxe_efficace!='Non') | apres_modifs + info_CC, data = s))
# iv_ee1bis <- ivmodelFormula(as.formula(paste("tax_acceptance ~ (taxe_efficace!='Non') + ", paste(variables_reg_ee, collapse = ' + '),
#                                              "| apres_modifs + info_CC + ", paste(variables_reg_ee, collapse = ' + '))), data = s) # bug with this function
# s$single <- 1*(s$nb_adultes==1)
# s$info_efficace <- 1*(s$apres_modifs==T)
# variables_reg_ee <- c("single", variables_reg_ee[variables_reg_ee != "(nb_adultes==1)"])
# iv_ee1bis <- ivmodel(Y=1*(s$tax_acceptance==T), D=1*(s$taxe_efficace!='Non'), Z=s[,c("info_efficace", "info_CC")], X= s[,variables_reg_ee]) # s[,variables_reg_ee]
# summary(iv_ee1bis)

formula_ee1 <- as.formula(paste("taxe_efficace!='Non' ~ apres_modifs + info_CC + ", paste(variables_reg_ee, collapse = ' + ')))
formula_ee1_no_ins <- as.formula(paste("taxe_efficace!='Non' ~ ", paste(variables_reg_ee, collapse = ' + ')))
tsls1_ee1_no_ins <- lm(formula_ee1_no_ins, data=s, weights=s$weight)
tsls1_ee1_noW <- lm(formula_ee1, data=s, weights=s$weight)
waldtest(tsls1_ee1_noW, tsls1_ee1_no_ins)$F[2] # 8 This function accounts for weights while ivreg doesn't, hence higher F (comparable stat in ivreg is Weak instrument)
waldtest(tsls1_ee1, tsls1_ee1_no_ins, vcov = vcovHC(tsls1_ee1, type="HC0"))$F[2]


##### Different specifications - papier.R #####
# 4.2
# No effect of our information on other variables than taxe_efficace
summary(lm((cause_CC=='anthropique') ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight))
summary(lm(as.numeric(effets_CC) ~ apres_modifs + info_CC * info_PM, data=s, subset=!is.missing(effets_CC), weights=s$weight))

variables_update_ee <- c("Revenu", variables_demo)
variables_update_ee <- variables_update_ee[!(variables_update_ee %in% c("revenu", "rev_tot", "age", "age_65_plus"))]

# Effect of primings on beliefs about environmental effectiveness
formula_update_ee <- as.formula(paste("taxe_efficace!='Non' ~ apres_modifs + info_CC * info_PM + ", 
                                      paste(variables_update_ee, collapse = ' + ')))
reg_update_ee1 <- lm(formula_update_ee, data=s, weights = s$weight, na.action='na.exclude')
summary(reg_update_ee1)

logit_update_ee2 <- glm(formula_update_ee, family = binomial(link='logit'), data=s)
summary(logit_update_ee2)
logit_update_ee2_margins <- logitmfx(formula_update_ee, s, atmean=FALSE)$mfxest
logit_update_ee2_margins

formula_update_ee_bis <- as.formula(paste("taxe_efficace=='Oui' ~ apres_modifs + info_CC * info_PM + ", 
                                          paste(variables_update_ee, collapse = ' + ')))
reg_update_ee3 <- lm(formula_update_ee_bis, data=s, weights = s$weight, na.action='na.exclude')
summary(reg_update_ee3)

Table_update_ee <- stargazer(reg_update_ee1, logit_update_ee2, reg_update_ee3,
                             title="Effect of primings on beliefs about environmental effectiveness", # "Diploma: Bachelor or above", 
                             covariate.labels = c("Info on Environmental Effectiveness ($Z_{E}$)",  
                                                  "Info on Climate Change ($Z_{CC}$)", "Info on Particulate Matter ($Z_{PM}$)", "$Z_{CC} \\times Z_{PM}$"), 
                             dep.var.labels = c("not ``No''", "``Yes''"), dep.var.caption = "Environmental effectiveness", header = FALSE,
                             keep = c("info", "apres_modifs"), 
                             coef = list(NULL, logit_update_ee2_margins[,1], NULL), 
                             se = list(NULL, logit_update_ee2_margins[,2], NULL),
                             column.labels = c("(1)", "(2)", "(3)"), model.numbers = FALSE,
                             add.lines = list(c("Controls: Socio-demographics ", "\\checkmark ", "\\checkmark ", "\\checkmark ")), 
                             no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:update_ee")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@',
                                                       Table_update_ee, fixed=TRUE), fixed=TRUE), collapse=' ')


## 5.1 Self-interest
# Identification challenge
sum(s$weight[s$simule_gagnant==1])/sum(s$weight) # 76%
sum(s$weight[s$taxe_approbation=='Non' & s$gagnant_categorie!='Gagnant' & s$simule_gagnant==1])/sum(s$weight[s$simule_gagnant==1]) # 62%

# (1) Main identification strategy
variables_reg_self_interest <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "prog_na", 
                                 "taxe_efficace", "single",  "hausse_depenses_par_uc", variables_demo) 
variables_reg_self_interest <- variables_reg_self_interest[!(variables_reg_self_interest %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
formula_tsls1_si1 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + 
                                      I(traite_cible*traite_cible_conjoint) + cible + Simule_gain + Simule_gain2 + tax_acceptance +  (taxe_approbation=='NSP') + ", paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si1 <- lm(formula_tsls1_si1, data=s, weights = s$weight)
summary(tsls1_si1)
s$non_perdant <- tsls1_si1$fitted.values
# 57 p.p.***
formula_tsls2_si1 <- as.formula(paste("taxe_cible_approbation!='Non' ~ non_perdant + cible + Simule_gain + Simule_gain2 + tax_acceptance + I(taxe_approbation=='NSP') + ", 
                                      paste(variables_reg_self_interest, collapse = ' + '))) # 
tsls2_si1 <- lm(formula_tsls2_si1, data=s, weights = s$weight)
summary(tsls2_si1) # Effective F-stat from Stata weakivtest: 40.834

# Alternative specifications for robustness checks
# (2) With many controls 
formula_tsls1_si2 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + 
                                      I(traite_cible*traite_cible_conjoint) + cible + Simule_gain + Simule_gain2 + ", paste(variables_reg_self_interest, collapse = ' + ')))

tsls1_si2 <- lm(formula_tsls1_si2, data=s, weights = s$weight)
summary(tsls1_si2)
s$non_perdant <- tsls1_si2$fitted.values
# 57 p.p.***
formula_tsls2_si2 <- as.formula(paste("taxe_cible_approbation!='Non' ~ non_perdant + cible + Simule_gain + Simule_gain2 + ", 
                                      paste(variables_reg_self_interest, collapse = ' + '))) # 
tsls2_si2 <- lm(formula_tsls2_si2, data=s, weights = s$weight)
summary(tsls2_si2) # Effective F-stat from Stata weakivtest: 40.834
# Effective F-stat from Stata weakivtest: 44.093

# (3) Simple OLS: 44 p.p. ***
formula_ols_si3  <- as.formula(paste("taxe_cible_approbation!='Non' ~ non_perdant + cible + tax_acceptance + I(taxe_approbation=='NSP') + prog_na + taxe_efficace +", 
                                     paste(variables_reg_self_interest, collapse = ' + '))) # 
s$non_perdant <- as.numeric(s$gagnant_cible_categorie!='Perdant')
ols_si3 <- lm(formula_ols_si3, data=s, weights = s$weight)
summary(ols_si3)

# (4) Simple Logit: 43 p.p.***
s$non_perdant <- as.numeric(s$gagnant_cible_categorie!='Perdant')
# Warning when weighting: it relates to number of trials and not to survey weights. Option weights = s$weight can be added in logistf, doesn't really change results
logit_si4 <- glm(formula_ols_si3, family = binomial(link='logit'), data=s)
summary(logit_si4) # Warning: Hauck-Donner effect, run logitsf, cf. https://prezi.com/di_n0_npv27n/hauck-donner-effect-and-instability-in-estimation-of-logisti/
logit_si4_margins <- logitmfx(formula_ols_si3, s, atmean=FALSE)$mfxest
logit_si4_margins # reason why mfx and not margins: https://stats.stackexchange.com/questions/409934/why-margins-and-mfx-yield-different-results-in-r/409937#409937
#summary(logistf(formula_ols_si3, data=s, firth = T)) # Firth (93) regression to resolve separation => effect of 2.77*** instead of 2.85***, cf. Heinze & Ploner (03)
# other way to check significance is to run a Likelihood Ratio test instead of the Wald/Chi-squared test reported in z value/Pr(>|z|) in glm, by running anova:
# https://stat.ethz.ch/pipermail/r-help/2001-October/015779.html http://nross626.math.yorku.ca/math4330/R/Regression/Hauck_Donner_effect.pdf
#anova(logit_si4, test='LR') # <2e-16 ***

# (5) IV Feedback
formula_tsls1_si5 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + 
                                      Simule_gain + Simule_gain2 + single + Revenu + Revenu2 +  Revenu_conjoint + Revenu_conjoint2 + prog_na + taxe_efficace +", paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si5 <- lm(formula_tsls1_si5, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_si5)
s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_si5$fitted.values
# 43 p.p. ***
formula_tsls2_si5 <- as.formula(paste("taxe_feedback_approbation!='Non' ~ non_perdant + tax_acceptance + (taxe_approbation=='NSP') + 
                                      Simule_gain + Simule_gain2 + single + Revenu + Revenu2 +  Revenu_conjoint + Revenu_conjoint2 + prog_na + taxe_efficace +", paste(variables_reg_self_interest, collapse = ' + ')))
tsls2_si5 <- lm(formula_tsls2_si5, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
summary(tsls2_si5)
# Effective F-stat from Stata weakivtest: 57.866

# (6) IV Feedback with controls
formula_tsls1_si6 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + 
                                      Simule_gain + Simule_gain2 + prog_na + taxe_efficace +", paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si6 <- lm(formula_tsls1_si6, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_si6)
s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_si6$fitted.values
# 43 p.p. ***
formula_tsls2_si6 <- as.formula(paste("taxe_feedback_approbation!='Non' ~ non_perdant + 
                                      Simule_gain + Simule_gain2 + prog_na + taxe_efficace +", paste(variables_reg_self_interest, collapse = ' + ')))
tsls2_si6 <- lm(formula_tsls2_si6, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
summary(tsls2_si6)
# Effective F-stat from Stata weakivtest: 37.966

# Results
Table_si2 <- stargazer(tsls2_si1, tsls2_si2, ols_si3, logit_si4, tsls2_si5, tsls2_si6, # tsls2_si4: Unrecognized object type
                       title="Effect of self-interest on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                       covariate.labels = c("Believes does not lose", "Initial tax Acceptance ($A^I$)", "",  "Environmentally effective: ``Yes''"),
                       dep.var.labels = c("Targeted Acceptance ($A^T$)", "Feedback Acceptance ($A^F$)"), dep.var.caption = "", header = FALSE,
                       keep = c("non_perdant", "tax_acceptance"),
                       coef = list(NULL, NULL, NULL, logit_si4_margins[,1], NULL, NULL), 
                       se = list(NULL, NULL, NULL, logit_si4_margins[,2], NULL, NULL),
                       add.lines = list(
                         # "Method: 2SLS & \\checkmark & \\checkmark &  & \\checkmark",
                         c("Controls: Incomes ", "\\checkmark ", "\\checkmark ", "\\checkmark  ", "\\checkmark ", "\\checkmark", ""),
                         c("Controls: Estimated gain ", "\\checkmark ", "", "\\checkmark ", "\\checkmark ", "\\checkmark", "\\checkmark"),
                         c("Controls: Target of the tax, single", "\\checkmark ", "\\checkmark ", "\\checkmark ", "\\checkmark  ", "", ""),
                         c("Controls: Socio-demo, other motives ", " \\checkmark", "\\checkmark ", "\\checkmark ", "\\checkmark  ", "\\checkmark", "\\checkmark  ")),
                       no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="results_private_benefits")
write_clip(sub("\\multicolumn{3}{c}{\\textit{OLS}} & \\textit{logistic} & \\multicolumn{2}{c}{\\textit{OLS}}", 
               "\\multicolumn{2}{c}{\\textit{IV}} & \\textit{OLS} & \\textit{logit} & \\multicolumn{2}{c}{\\textit{IV}}", 
               gsub('\\end{table}', '} {\\footnotesize \\\\ \\quad \\\\ \\textsc{Note:} Standard errors are reported in parentheses. 
                    For logit, average marginal effects are reported and not coefficients. The list of controls can be found in Appendix \\ref{set_controls}. }\\end{table}', 
                    gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_si2, fixed=TRUE), fixed=TRUE), fixed=T), collapse=' ')

Table_si1 <- stargazer(tsls1_si1, tsls1_si2, tsls1_si5, tsls1_si6,
                       title="First stage regressions results for self-interest", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                       covariate.labels = c("Transfer to respondent ($T_1$)", "Transfer to spouse ($T_2$)",
                                            "$T_1 \\times T_2$", "Simulated winner ($\\widehat{\\Gamma}$)", "Initial tax Acceptance ($A^I$)"),
                       dep.var.labels = c("Targeted tax ($G^T$)", "After feedback ($G^F$)"), dep.var.caption = "Believes does not lose", header = FALSE,
                       column.labels = c("(1)", "(2)", "(5)", "(6)"), model.numbers = FALSE,
                       keep = c("traite", "acceptance", "simule_gagnant"),
                       add.lines = list(c("Controls: Incomes", " \\checkmark", " \\checkmark", " \\checkmark", ""),
                                        c("Controls: Estimated gain", " \\checkmark", " ", " \\checkmark", " \\checkmark"),
                                        c("Controls: Target of the tax, single", " \\checkmark", " \\checkmark", " ", " "),
                                        c("Controls: Socio-demo, other motives", "\\checkmark", " \\checkmark", " \\checkmark", " \\checkmark"),
                                        c("Effective F-Statistic", "37.728", "43.321", "58.696", "37.966")),
                       no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="first_stage_private_benefits")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', 
                                                       Table_si1, fixed=TRUE), fixed=TRUE), collapse=' ')

# Z test (cf. https://stats.stackexchange.com/questions/93540/testing-equality-of-coefficients-from-two-different-regressions)
(0.571-0.517)/(0.092^2+0.170^2)^0.5 # 0.28: not significantly different


## 5.2 Environmental effectiveness avec 6 spécifications
# Main identification strategy
# Alternative specifications for robustness checks
# (1) 2SLS both instruments, with controls: 48 p.p.** We do not control for progressivity: 
#     as most of the people who did not answer the question were in the second half of the survey, 
#     the absence of response is too correlated with our instrument Z_E (apres_modifs) which bias the results.
variables_reg_ee <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "single", "Simule_gain", "Simule_gain2", "gagnant_categorie", variables_demo)
variables_reg_ee <- variables_reg_ee[!(variables_reg_ee %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
formula_ee1 <- as.formula(paste("taxe_efficace!='Non' ~ apres_modifs + info_CC + ",  paste(variables_reg_ee, collapse = ' + ')))
tsls1_ee1 <- lm(formula_ee1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_ee1)
s$taxe_efficace.hat <- fitted.values(tsls1_ee1)
formula2_ee1 <- as.formula(paste("tax_acceptance ~ taxe_efficace.hat + ", paste(variables_reg_ee, collapse = ' + ')))
tsls2_ee1 <- lm(formula2_ee1, data=s, weights=s$weight)
summary(tsls2_ee1) # Effective F-stat from Stata weakivtest: 5.866

# (2) 2SLS both instruments, no controls: 52 p.p.
tsls1_ee2 <- lm(taxe_efficace!='Non' ~ apres_modifs + info_CC, data=s, weights=s$weight)
summary(tsls1_ee2)
s$taxe_efficace.hat <- tsls1_ee2$fitted.values
tsls2_ee2 <- lm(tax_acceptance ~ taxe_efficace.hat, data=s, weights=s$weight)
summary(tsls2_ee2) # Effective F-stat from Stata weakivtest: 2.523

# (3) OLS with controls: 39 p.p. ***
s$taxe_efficace.hat <- as.numeric(s$taxe_efficace!='Non')
formula_ee3 <- as.formula(paste("tax_acceptance ~ taxe_efficace.hat + prog_not_no + (prog_na == 'NA') + ", paste(variables_reg_ee, collapse = ' + '))) # 
ols_ee3 <- lm(formula_ee3, data=s, weights = s$weight)
summary(ols_ee3)

# (4) Logit
# 37 p.p. ***
s$taxe_efficace.hat <- as.numeric(s$taxe_efficace!='Non')
logit_ee4 <- glm(formula_ee3, family = binomial(link='logit'), data=s) # Warning: Hauck-Donner effect, run logitsf. For a test run anova.glm, not Wald 
summary(logit_ee4)
logit_ee4_margins <- logitmfx(data=s, formula=logit_ee4, atmean=FALSE)$mfxest
logit_ee4_margins
summary(logistf(formula_ee3, data=s, firth = T)) # Firth (93) regression to resolve separation => effect of 2.20*** instead of 2.26***, cf. Heinze & Ploner (03) 
anova(logit_ee4, test='LR') # <2e-16 ***: all is fine

# (5) IV, with controls and efficace is yes: 51 p.p. *
formula_ee5 <- as.formula(paste("taxe_efficace=='Oui' ~ apres_modifs + info_CC + ", 
                                paste(variables_reg_ee, collapse = ' + ')))
tsls1_ee5 <- lm(formula_ee5, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_ee5)
s$taxe_efficace_yes.hat <- tsls1_ee5$fitted.values
formula2_ee5 <- as.formula(paste("tax_acceptance ~ taxe_efficace_yes.hat + ", paste(variables_reg_ee, collapse = ' + ')))
tsls2_ee5 <- lm(formula2_ee5, data=s, weights=s$weight)
summary(tsls2_ee5) # Effective F-stat from Stata weakivtest: 11.145

# (6) IV, with controls and approval: 42 p.p. **
formula_ee6 <- as.formula(paste("taxe_efficace=='Oui' ~ apres_modifs + info_CC + ", 
                                paste(variables_reg_ee, collapse = ' + ')))
tsls1_ee6 <- lm(formula_ee6, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_ee6)
s$taxe_efficace_yes.hat <- tsls1_ee6$fitted.values
formula2_ee6 <- as.formula(paste("tax_approval ~ taxe_efficace_yes.hat + ", paste(variables_reg_ee, collapse = ' + ')))
tsls2_ee6 <- lm(formula2_ee6, data=s, weights=s$weight)
summary(tsls2_ee6)
# Effective F-stat from Stata weakivtest: 11.145

# Results
Table_ee2 <- stargazer(tsls2_ee1, tsls2_ee2, ols_ee3, logit_ee4, tsls2_ee5, tsls2_ee6,
                       title="Effect of believing in environmental effectiveness on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                       covariate.labels = c("Environmental effectiveness: not ``No''", "Environmental effectiveness: ``Yes''"), # "Constant",
                       dep.var.labels = c("Tax Acceptance ($A^I$)", "Tax Approval ($\\dot{A^I}$)"), dep.var.caption = "", header = FALSE,
                       keep = c("efficace"), # "Constant",
                       coef = list(NULL, NULL, NULL, logit_ee4_margins[,1], NULL, NULL), 
                       se = list(NULL, NULL, NULL, logit_ee4_margins[,2], NULL, NULL),
                       add.lines = list(c("Instruments: info E.E., C.C. \\& P.M. ", "\\checkmark ", "\\checkmark ", "", " ", "\\checkmark ", "\\checkmark"),
                                        c("Controls: Socio-demo, other motives ", "\\checkmark ", "", "\\checkmark  ", "\\checkmark ", "\\checkmark ", "\\checkmark ")), 
                       no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:ee")
write_clip(sub("\\multicolumn{3}{c}{\\textit{OLS}} & \\textit{logistic} & \\textit{OLS} & \\textit{OLS}", 
               "\\textit{IV} & \\textit{IV} & \\textit{OLS} & \\textit{logit} & \\textit{IV} & \\textit{IV}", 
               gsub('\\end{table}', '} {\\footnotesize \\\\ \\quad \\\\ \\textsc{Note:} Standard errors are reported in parentheses. 
                    For logit, average marginal effects are reported and not coefficients. The list of controls can be found in Appendix \\ref{set_controls}.}\\end{table}', 
                    gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_ee2, fixed=TRUE), fixed=TRUE), fixed=TRUE), collapse=' ')
# insert \hspace{1.6cm} incomes, estimated gains & & & & & &  \\ 
Table_ee1 <- stargazer(tsls1_ee1, tsls1_ee2, tsls1_ee5,
                       title="First stage regressions results for environmental effectiveness", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                       # "Info on Climate Change and/or on Particulates", "Info on Climate Change only", "Info on Particulates only"
                       covariate.labels = c("Info on Environmental Effectiveness ($Z_{E}$)",  
                                            "Info on Climate Change ($Z_{CC}$)", "Info on Particulate Matter ($Z_{PM}$)", "$Z_{CC} \\times Z_{PM}$"), 
                       dep.var.labels = c("not ``No''", "``Yes''"), dep.var.caption = "Environmental effectiveness", header = FALSE,
                       keep = c("info", "apres_modifs"), 
                       column.labels = c("(1)", "(2)", "(5,6)"), model.numbers = FALSE,
                       add.lines = list(c("Controls ", "", "\\checkmark ", ""), c("Effective F-Statistic", "5.866", "2.523", "11.145")), 
                       no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="first_stage_environmental_effectiveness")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', 
                                                       Table_ee1, fixed=TRUE), fixed=TRUE), collapse=' ')




# D.2
## D.2 Additional specifications: Table XX
# (1) Target: Acceptance ~ win 
variables_reg_self_interest <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "prog_na", 
                                 "taxe_efficace", "single",  "hausse_depenses_par_uc", variables_demo) 
variables_reg_self_interest <- variables_reg_self_interest[!(variables_reg_self_interest %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
formula_tsls1_si1_app <- as.formula(paste("gagnant_cible_categorie=='Gagnant' ~ traite_cible + traite_cible_conjoint + 
                                          I(traite_cible*traite_cible_conjoint) + cible + Simule_gain + Simule_gain2 + tax_acceptance +  (taxe_approbation=='NSP') + ", paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si1_app <- lm(formula_tsls1_si1_app, data=s, weights = s$weight)
summary(tsls1_si1_app)
s$gagnant <- tsls1_si1_app$fitted.values
formula_tsls2_si1_app <- as.formula(paste("taxe_cible_approbation!='Non' ~ gagnant + cible + Simule_gain + Simule_gain2 + tax_acceptance + I(taxe_approbation=='NSP') + ", 
                                          paste(variables_reg_self_interest, collapse = ' + '))) # 
tsls2_si1_app <- lm(formula_tsls2_si1_app, data=s, weights = s$weight)
summary(tsls2_si1_app)

# (2)
formula_tsls2_si2_app <- as.formula(paste("taxe_cible_approbation=='Oui' ~ gagnant + cible + Simule_gain + Simule_gain2 + tax_acceptance + I(taxe_approbation=='NSP') + ", 
                                          paste(variables_reg_self_interest, collapse = ' + '))) # 
tsls2_si2_app <- lm(formula_tsls2_si2_app, data=s, weights = s$weight)
summary(tsls2_si2_app)

# (3)
formula_tsls1_si3_app <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + 
                                          I(traite_cible*traite_cible_conjoint) + cible + Simule_gain + Simule_gain2 + tax_acceptance +  (taxe_approbation=='NSP') + ", paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si3_app <- lm(formula_tsls1_si3_app, data=s, weights = s$weight)
summary(tsls1_si3_app)
s$non_perdant <- tsls1_si3_app$fitted.values
formula_tsls2_si3_app <- as.formula(paste("taxe_cible_approbation=='Oui' ~ non_perdant + cible + Simule_gain + Simule_gain2 + tax_acceptance + I(taxe_approbation=='NSP') + ", 
                                          paste(variables_reg_self_interest, collapse = ' + '))) # 
tsls2_si3_app <- lm(formula_tsls2_si3_app, data=s, weights = s$weight)
summary(tsls2_si3_app)

# (4)
formula_tsls1_si4_app <- as.formula(paste("gagnant_feedback_categorie=='Gagnant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + 
                                          Simule_gain + Simule_gain2 + single + Revenu + Revenu2 +  Revenu_conjoint + Revenu_conjoint2 + prog_na + taxe_efficace +", paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si4_app <- lm(formula_tsls1_si4_app, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_si4_app)
s$gagnant[s$variante_taxe_info=='f'] <- tsls1_si4_app$fitted.values
formula_tsls2_si4_app <- as.formula(paste("taxe_feedback_approbation!='Non' ~ gagnant + tax_acceptance + (taxe_approbation=='NSP') + 
                                          Simule_gain + Simule_gain2 + single + Revenu + Revenu2 +  Revenu_conjoint + Revenu_conjoint2 + prog_na + taxe_efficace +", paste(variables_reg_self_interest, collapse = ' + ')))
tsls2_si4_app <- lm(formula_tsls2_si4_app, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
summary(tsls2_si4_app)

# (5)
formula_tsls2_si5_app <- as.formula(paste("taxe_feedback_approbation=='Oui' ~ gagnant + tax_acceptance + (taxe_approbation=='NSP') + 
                                          Simule_gain + Simule_gain2 + single + Revenu + Revenu2 +  Revenu_conjoint + Revenu_conjoint2 + prog_na + taxe_efficace +", paste(variables_reg_self_interest, collapse = ' + ')))
tsls2_si5_app <- lm(formula_tsls2_si5_app, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
summary(tsls2_si5_app)

# (6)
formula_tsls1_si6_app <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + 
                                          Simule_gain + Simule_gain2 + single + Revenu + Revenu2 +  Revenu_conjoint + Revenu_conjoint2 + prog_na + taxe_efficace +", paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si6_app <- lm(formula_tsls1_si6_app, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_si6_app)
s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_si6_app$fitted.values
formula_tsls2_si6_app <- as.formula(paste("taxe_feedback_approbation=='Oui' ~ non_perdant + tax_acceptance + (taxe_approbation=='NSP') + 
                                          Simule_gain + Simule_gain2 + single + Revenu + Revenu2 +  Revenu_conjoint + Revenu_conjoint2 + prog_na + taxe_efficace +", paste(variables_reg_self_interest, collapse = ' + ')))
tsls2_si6_app <- lm(formula_tsls2_si6_app, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
summary(tsls2_si6_app)


# Results
Table_additional_res <- stargazer(tsls2_si1_app, tsls2_si2_app, tsls2_si3_app, tsls2_si4_app, tsls2_si5_app, tsls2_si6_app,
                                  title="Effect of self-interest on acceptance: second stages of alternative specifications", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                                  covariate.labels = c("Believes wins", "Believes does not lose", "Initial tax Acceptance ($A^I$)"), model.names = FALSE,
                                  dep.var.labels = c("Acceptance", "Approval", "Acceptance", "Approval"), 
                                  dep.var.caption = c("\\multicolumn{3}{c}{Targeted Tax} & \\multicolumn{3}{c}{After Feedback}"), header = FALSE,
                                  keep = c("gagnant", "non_perdant", "tax_acceptance"),
                                  add.lines = list(
                                    c("Controls: Incomes ", "\\checkmark ", "\\checkmark ", "\\checkmark  ", "\\checkmark", "\\checkmark", "\\checkmark"),
                                    c("Controls: Estimated gain ", "", "", "", "\\checkmark", "\\checkmark ", "\\checkmark "),
                                    c("Controls: Target of the tax ", "\\checkmark ", "\\checkmark ", "\\checkmark ", "", "", "")),
                                  no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:alternative_si")
write_clip(sub("\\multicolumn{6}{c}{", "", sub("er Feedback}}", "er Feedback}", gsub('\\end{table}', '} \\end{table}', 
                                                                                     gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_additional_res, fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE), collapse=' ')


##### Champ libre Bénéfices/Problèmes #####
print(s$benefices_autre[!is.na(s$benefices_autre)])
# aucun: ................
# inéquité: ...............
# modifications structurelles: ............
# diesel, kerosène: ...........
# déficit: .........
# état prend argent: ..................
# pépites : "maléfique" "Aucunement bénéfique sauf pour contrer ceux qui utilisent leur véhicule alors qu'ils peuvent faire autrement" 
# "aucune prime de nous sera versé. ce n est que  du blabla de la part du gouvernement" "brassage"
# "elle est bénéfique pour le gouvernement qui n'investirait pas la TOTALITE des ressources budgétaires supplémentaires"
# "Globalement l'ensemble des mesures et le reversement d'une \"compensation\" ne recouvrerait pas la différence de prix sur l'année. 
#     Je  parle de l'ensemble des augmentations ainsi que les augmentation forcément induites et répercutées sur l'ensemble des produits de consommations..." 
# "vos donnees sont erronees" "On est toujours perdant" "je ne pourrai plus me chauffer tout simplement"
# "Je ne vois pas en quoi nous prendre de l'argent pour nous le redonner, changerait quelque chose à notre manière de polluer."
# "J' ai deja réduit au maximum mes consommations d' énergie mais néanmoins  je paye toujours plus" Problème : "Pour la paix sociale"
# "il faut taxer les nouveaux véicule a l'achats et de fasson significative et pas apré leurs mise en services!"
# "Ce projet a entrainé un fort mécontentement; je ne le soutiens pas car je le juge absurde."
print(s$problemes_autre[!is.na(s$problemes_autre)])


##### Confirmation bias ? #####

### Confirmation bias iff coef > 0
## Among winners
summary(lm(((taxe_feedback_approbation=='Oui') - (taxe_approbation=='Oui')) ~ gagnant_categorie, subset = simule_gagnant==1, data=s, weights=s$weight))
summary(lm(((taxe_feedback_approbation!='Non') - (taxe_approbation!='Non')) ~ gagnant_categorie, subset = simule_gagnant==1, data=s, weights=s$weight))
# > 0: .06**: Se mettre à approuver est corrélé à se penser déjà gagnant.
summary(lm(((taxe_feedback_approbation=='Oui') - (taxe_approbation=='Oui')) ~ gagnant_categorie=='Gagnant', subset = simule_gagnant==1, data=s, weights=s$weight))
# 0: Montre que ce sont des gens qui se pensaient gagnants et NSP à l'approbation qui se mettent à approuver.
summary(lm(((taxe_feedback_approbation!='Non') - (taxe_approbation!='Non')) ~ gagnant_categorie=='Gagnant', subset = simule_gagnant==1, data=s, weights=s$weight))
# .03.: Se mettre à approuver est corrélé à se penser déjà non perdant: l'effet précédent concerne aussi en partie les Non affecté.
summary(lm(((taxe_feedback_approbation=='Oui') - (taxe_approbation=='Oui')) ~ gagnant_categorie!='Perdant', subset = simule_gagnant==1, data=s, weights=s$weight))
# < 0: -.03 (p=.13) Se mettre à accepter est corrélé à se penser perdant. => invalide biais de confirmation
summary(lm(((taxe_feedback_approbation!='Non') - (taxe_approbation!='Non')) ~ gagnant_categorie!='Perdant', subset = simule_gagnant==1, data=s, weights=s$weight))
## Among losers
summary(lm(((taxe_feedback_approbation=='Non') - (taxe_approbation=='Non')) ~ gagnant_categorie=='Perdant', subset = simule_gagnant==0, data=s, weights=s$weight))
# 0
summary(lm(((taxe_feedback_approbation=='Non') - (taxe_approbation=='Non')) ~ gagnant_categorie, subset = simule_gagnant==0, data=s, weights=s$weight))
summary(lm(((taxe_feedback_approbation!='Oui') - (taxe_approbation!='Oui')) ~ gagnant_categorie, subset = simule_gagnant==0, data=s, weights=s$weight))
# > 0: .05. Se mettre à désapprouver est corrélé à se penser perdant.
summary(lm(((taxe_feedback_approbation!='Oui') - (taxe_approbation!='Oui')) ~ gagnant_categorie=='Perdant', subset = simule_gagnant==0, data=s, weights=s$weight))
# < 0: -.14*: Se mettre à rejeter est corrélé à se penser gagnant. => invalide le biais de confirmation mais l'échantillon est faible.
summary(lm(((taxe_feedback_approbation=='Non') - (taxe_approbation=='Non')) ~ gagnant_categorie!='Gagnant', subset = simule_gagnant==0, data=s, weights=s$weight))
# 0
summary(lm(((taxe_feedback_approbation!='Oui') - (taxe_approbation!='Oui')) ~ gagnant_categorie!='Gagnant', subset = simule_gagnant==0, data=s, weights=s$weight))
# decrit(s$feedback_confirme)


##### Trash papier2.R #####
barres(file="CC_target_emission", title="", data=dataN("emission_cible", miss=FALSE), nsp=FALSE, sort=T, color = rev(brewer.pal(11, "RdBu")), 
       legend = dataN("emission_cible", return="levels"), labels=c("Emission compatible with +2°C (tCO<sub>2</sub>e/yr p.c.)")) 
barres(file="CC_cause", title="", data=dataN("cause_CC"), nsp=T, sort=T, legend = c("Anthropic", "Natural", "Does not exist", "PNR"), labels=c("Cause of CC"))
barres(file="CC_region", title="", data=dataN("region_CC", miss=FALSE), nsp=FALSE, sort=T, 
       legend = c("India", "As much in both", "European Union", "NSP"), labels=c("Region with biggest consequences of CC"))
barres(file="CC_generation_min_nolegend_notick", title="", rev_color = T, data=dataN("generation_CC_min"), nsp=T, sort=T, thin=T, show_ticks=F,
       legend = c(dataN("generation_CC_min", return="levels")[1:4], "PNR"), labels=c(" "))
barres(file="CC_generation_min", title="", rev_color = T, data=dataN("generation_CC_min"), nsp=T, sort=T, 
       legend = c(dataN("generation_CC_min", return="levels")[1:4], "PNR"), labels=c("First generation of French severely affected by CC (born in...)"))
variables_winners <- names(s)[which(grepl("taxe_gagnant_", names(s)))]
labels_winners <- c("No one", "The poorest", "The middle class", "The richest", "Everyone", "City dwellers", "Certain persons, but no specific income category", "PNR (Don't know, don't want to answer)")
barres(file="tax_winners", title="", data=data1(variables_winners), sort=T, showLegend=FALSE, labels=labels_winners, hover=labels_winners)
variables_losers <- names(s)[which(grepl("taxe_perdant_", names(s)))]
labels_losers <- c("No one", "The poorest", "The middle class", "The richest", "Everyone", "Rural or peri-urban households", "Certain persons, but no specific income category", "PNR (Don't know, don't want to answer)")
barres(file="tax_losers", title="", data=data1(variables_losers), sort=T, showLegend=FALSE, labels=labels_losers, hover=labels_losers)
variables_benefits <- names(s)[which(grepl("benefice", names(s)))[which(grepl("problemes", names(s)))>300]]
variables_benefits <- variables_benefits[!(variables_benefits %in% c("nb_benefices", "benefices_autre"))]
labels_benefits <- c("Fights CC", "Reduces negative impact of pollution on health", "Reduces congestion", "Increases my purchasing power", 
                     "Increases purchasing power of the poorest",
                     "Increases France's independence toward fossils", "Prepares the economy for tomorrow", "None of these reasons", "Other reasons")
barres(file="CC_benefits", title="", data=data1(variables_benefits), sort=T, showLegend=FALSE, labels=labels_benefits, hover=labels_benefits) # pb 35% NSP
variables_problems <- names(s)[which(grepl("problemes", names(s)))]
variables_problems <- variables_problems[!(variables_problems %in% c("nb_problemes", "problemes_autre"))]
labels_problems <- c("Is ineffective to reduce pollution", "Alternatives are insufficient or too expensive", "Penalizes rural households", "Decreases my purchaisng power",
                     "Penalizes the poorest", "Hurts the economy", "Is a pretext to increase taxes", "None of these reasons", "Other reasons")
barres(file="CC_problems", title="", data=data1(variables_problems), sort=T, showLegend=FALSE, labels=labels_problems, hover=labels_problems)
barres(file="CC_talks", title="", data=dataN("parle_CC"), nsp=T, sort=T, 
       legend = c("Several times per month", "Several times per year", "Almost never", "PNR"), labels=c("Talks about CC...")) 
barres(file="diesel_catch_up_val", dataKN(c("rattrapage_diesel")), nsp=TRUE, legend=c("Yes", "No", "PNR"), color=, labels = c("Favorable to catch-up diesel taxes"))
barres(file="shale_val", dataKN(c("schiste_approbation")), nsp=TRUE, legend=c("Yes", "No", "PNR"), color=, labels = c("Favorable to shale gas extraction"))
barres(file="CC_effects", title="", thin=T, data=dataN("effets_CC"), nsp=T, sort=T, 
       legend = c("Insignificant", "Small", "Serious", "Disastrous", "Cataclysmic", "NSP"), labels=c("Consequences of CC"))
labels_resp <- c("Each one of us", "Governments", "Certain foreign countries", "The richest", "Natural causes", "Past generations")
barres(file="CC_responsible", title="", data=data1(names(s)[which(grepl("responsable_CC", names(s)))]), sort=T, showLegend=FALSE, labels=labels_resp, hover=labels_resp)
barres(file="transports_opinionb", thin=T, title="", data=matrix(dataN("transports_avis")[c(4:1,5),], ncol=1),  legend=rev(c("PNR", "Insufficient", "Just enough", "Decent", "Satisfactory")), labels=c(" "))


cause_ols <- lm(formula_determinants_cause, data=s, weights = s$weight)
cause_logit <- glm(formula_determinants_cause, data=s, family="binomial")
summary(cause_ols)
summary(cause_logit)
cause_logit_margins <- logitmfx(formula_determinants_cause, s, atmean=FALSE)$mfxest
cause_logit_margins

formula_determinants_effects <- as.formula(paste("effets_CC > 2 ~ ", paste(variables_determinants, collapse = ' + ')))
effects_ols <- lm(formula_determinants_effects, data=s, weights = s$weight)
effects_logit <- glm(formula_determinants_effects, data=s, family="binomial")
summary(effects_ols)
summary(effects_logit)
effects_logit_margins <- logitmfx(formula_determinants_effects, s, atmean=FALSE)$mfxest
effects_logit_margins

decrit(s$score_ges + s$score_climate_call, weights = s$weight) # variance: 2.08 < mean: 4.15
wtd.var(s$score_ges + s$score_climate_call, weights = s$weight) 
s$score <- (s$score_ges + s$score_climate_call)/7
formula_determinants_score <- as.formula(paste("score ~ ", paste(variables_determinants, collapse = ' + ')))
ges_ols <- lm(formula_determinants_score, data=s, weights = s$weight)
summary(ges_ols)
ges_ologit <- oglmx(formula_determinants_score, data=s, constantMEAN = FALSE, constantSD = FALSE, link="logit", delta=0)
# ges_ologit <- ologit.reg(as.formula(paste("score_ges + score_climate_call ~ ", paste(variables_determinants, collapse = ' + '))), data=s)
# ges_ologit <- polr(as.formula(paste("as.factor(score_ges + score_climate_call) ~ ", paste(variables_determinants, collapse = ' + '))), data=s, Hess = T)
# with polr, regressors should be scaled with scale() 
s$score <- as.factor((s$score_ges + s$score_climate_call)/7)
ges_ologit <- clm(formula_determinants_score, data=s)
summary(ges_ologit)
ges_ologit_margins <- margins.oglmx(ges_ologit, atmeans=FALSE, AME=T)
ges_ologit_margins
ges_logit <- glm(formula_determinants_score, data=s, family="binomial")
summary(ges_logit)
ges_logit_margins <- logitmfx(formula_determinants_score, s, atmean=FALSE)$mfxest
ges_logit_margins

Table_determinants_CC <- stargazer(cause_ols, cause_logit, effects_ols, effects_logit, ges_ols, ges_logit, ges_ologit,
                                   title="Determinants of attitudes towards climate change", model.names = T, model.numbers = FALSE, 
                                   # covariate.labels = c("Constant", "Initial tax: PNR (I don't know)", "Initial tax: Approves", "Sex: Female", "Ecologist","Consumption Units (C.U.)", 
                                   #                      "Yellow Vests: PNR","Yellow Vests: understands","Yellow Vests: supports", "Yellow Vests: is part"),
                                   header = FALSE, dep.var.labels = c("CC anthropic", "CC disastrous", "Knowledge on CC", ""),#  dep.var.caption = "", 
                                   keep = c("Constant", "sexe", "age", "diplome", "taille_agglo", "Gilets_jaunes", "humaniste", "ecologiste", "Gauche_droite"), 
                                   # "interet_politique", "csp", "diesel", "essence", "sexe", "apolitique"
                                   coef = list(NULL, cause_logit_margins[,1], NULL, effects_logit_margins[,1], NULL, ges_logit_margins[,1], NULL),
                                   se = list(NULL, cause_logit_margins[,2], NULL, effects_logit_margins[,2], NULL, ges_logit_margins[,2], NULL),
                                   # add.lines = list(c("Controls: Socio-demo, political leaning", "\\checkmark", "\\checkmark", "\\checkmark")),
                                   no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:bias")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', 
                                                       Table_determinants_CC, fixed=TRUE), fixed=TRUE), collapse=' ')

summary(lm((s$cause_CC=='anthropique') ~ Revenu, data=s)) # Pas significatif
summary(lm((s$cause_CC=='anthropique') ~ sexe, data=s)) # Pas significatif
summary(lm((s$cause_CC=='anthropique') ~ as.factor(taille_agglo), data=s)) # Paris et +100k vs rural : env +8.5 p.p.
summary(lm((s$cause_CC=='anthropique') ~ Gilets_jaunes, data=s)) # -10 p.p. soutient / -20 p.p. est dedans
summary(lm((s$cause_CC=='anthropique') ~ Gauche_droite, data=s)) # -18 p.p. droite / -21 p.p. ext-droite / -12 p.p. indeter / -9 p.p. centre

summary(lm((s$effets_CC > 2) ~ Revenu, data=s)) # Pas significatif
summary(lm((s$effets_CC > 2) ~ sexe, data=s)) # Pas significatif
summary(lm((s$effets_CC > 2) ~ as.factor(taille_agglo), data=s)) # Peu significatif (grandes villes + 6.7 p.p.)
summary(lm((s$effets_CC > 2) ~ Gilets_jaunes, data=s)) # -5.6 p.p. soutient / -7.8 p.p. est dedans
summary(lm((s$effets_CC > 2) ~ Gauche_droite, data=s)) # -19 p.p. droite / -16 p.p. ext-droite / -11 p.p. indeter
summary(lm(effets_CC ~ connaissances_CC + diplome4, data=s, subset=s$effets_CC!=-1, weights = s$weight))
summary(lm(effets_CC ~ (connaissances_CC + diplome4) * gauche_droite, data=s, subset=s$effets_CC!=-1, weights = s$weight))
summary(lm(effets_CC ~ score_ges + diplome4, data=s, subset=s$effets_CC!=-1, weights = s$weight))
summary(lm(effets_CC ~ connaissances_CC * gauche_droite, data=s, subset=s$effets_CC!=-1, weights = s$weight))
summary(lm(effets_CC ~ Diplome, data=s, subset=s$effets_CC!=-1, weights = s$weight))
summary(lm(effets_CC ~ Diplome * gauche_droite, data=s, subset=s$effets_CC!=-1, weights = s$weight))
summary(lm((effets_CC > 2) ~ connaissances_CC, data=s, subset=s$effets_CC!=-1, weights = s$weight))
summary(lm((effets_CC > 2) ~ diplome4 * Gauche_droite, data=s, subset=s$effets_CC!=-1, weights = s$weight))
summary(lm(score_ges ~ gauche_droite, data=s, weights = s$weight))
summary(lm(score_climate_call ~ gauche_droite, data=s, weights = s$weight))
summary(lm(cause_CC=='anthropique' ~ Gauche_droite, data=s, weights = s$weight))

decrit(s$connaissances_CC)
plot(density(s$connaissances_CC))
summary(lm(I((connaissances_CC - mean(connaissances_CC))/sd(connaissances_CC)) ~ I((generation_CC_min-1960)/30), data=s, weights = s$weight))
formula_connaissances_CC <- as.formula(paste("I((connaissances_CC - mean(connaissances_CC))/sd(connaissances_CC)) ~ I((generation_CC_min-1960)/30) + region_CC + diplome + ", 
        paste(variables_determinants[!(variables_determinants %in% c("Gauche_droite", "humaniste", "patriote", "ecologiste", "apolitique", "liberal", 
        "conservateur", "diplome4", "as.factor(ifelse(is.missing(s$Gilets_jaunes), 'NA', as.character(s$Gilets_jaunes)))"))], collapse = ' + ')))
summary(lm(formula_connaissances_CC, data=s, weights = s$weight))
summary(lm(connaissances_CC ~ as.factor(generation_CC_min), data=s, weights = s$weight))

decrit(s$ecologiste, weights = s$weight)
decrit(s$humaniste, weights = s$weight)
decrit(s$patriote, weights = s$weight)
decrit(s$liberal, weights = s$weight)
decrit(s$conservateur, weights = s$weight)
decrit(s$apolitique, weights = s$weight)
decrit(s$Gauche_droite, weights = s$weight, miss=T)
summary(lm((enfant_CC=='Oui') ~ sexe, data=s)) # -3.4 p.p.

decrit(s$changer_si_moyens == T | s$changer_si_politiques==T | s$changer_si_tous==T | s$changer_essaie==T, weights=s$weight) # 85%
decrit((s$emission_cible[s$changer_deja_fait==T] >= 3), weights = s$weight[s$changer_deja_fait==T]) # 79%
decrit((s$emission_cible[s$changer_deja_fait==F] >= 3), weights=s$weight[s$changer_deja_fait==F]) # 85%
# TODO: use US results?
usa_survey <- read.dta("CCES_Panel_Full3waves_VV_V4.dta") # https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/TOE8I1
labels_CC_USA <- levels(usa_survey$CC14_321) # What action needed for climate change? 2014.
levels(usa_survey$CC14_321) <- c("Immediate", "Some", "Wait", "No", "Fake", "PNR", "NA")
decrit(usa_survey$CC14_321, weights = usa_survey$weight) # 30/25/18/20/8
# levels(usa_survey$CC10_321) <- levels(usa_survey$CC12_321) <- c("Immediate", "Some", "Wait", "No", "Fake", "PNR", "NA")
# decrit(usa_survey$CC12_321, weights = usa_survey$weight)
# decrit(usa_survey$CC10_321, weights = usa_survey$weight)

# When question specific to purchasing power:
variables_winners <- names(s)[which(grepl("taxe_gagnant_", names(s)))]
labels_winners <- c("No one", "The poorest", "The middle class", "The richest", "Everyone", "City dwellers", "Certain persons,<br> but no specific income category", "PNR (Don't know, don't want to answer)")
barres(file="tax_winners", title="", data=data1(variables_winners, data=s[s$variante_monetaire==1,], weights = s$weight[s$variante_monetaire==1]), sort=T, showLegend=FALSE, labels=labels_winners, hover=labels_winners)
variables_losers <- names(s)[which(grepl("taxe_perdant_", names(s)))]
labels_losers <- c("No one", "The poorest", "The middle class", "The richest", "Everyone", "Rural or peri-urban households", "Certain persons, <br>but no specific income category", "PNR (Don't know, don't want to answer)")
barres(file="tax_losers", title="", data=data1(variables_losers, data=s[s$variante_monetaire==1,], weights = s$weight[s$variante_monetaire==1]), sort=T, showLegend=FALSE, labels=labels_losers, hover=labels_losers)

decrit(s$taxe_efficace, miss=T, weight=s$weight)
s$elast_fuel_perso <- revalue(s$elast_fuel_perso, c("+ de 30% - Je changerais largement mes habitudes de déplacement"="> 30%",
          "de 20% à 30%"="20 to 30%", "de 10% à 20%"="10 to 20%", "de 0% à 10%"="0 to 10%",
          "0% - Je suis contraint sur tous mes déplacements"="0%: won't reduce", "0% - Je n'en consomme déjà presque pas"="0%: don't consume"))
s$elast_fuel <- revalue(s$elast_fuel, c("+ de 30%"="> 30%", "de 20% à 30%"="20 to 30%", "de 10% à 20%"="10 to 20%", "de 0% à 3%"="0 to 3%", "de 3% à 10%"="3 to 10%"))
barres(file="elasticities", title="", thin=T, data=dataKN(c("Elasticite_chauffage", "Elasticite_fuel", "Elasticite_chauffage_perso", "Elasticite_fuel_perso"), miss=FALSE),
       nsp=FALSE, labels=c("Aggregate: Housing", "Aggregate: Transport", "Own: Housing", "Own: Transport"),
       legend = dataN("Elasticite_chauffage", return="levels", miss=FALSE), show_ticks=T)
# Correlations TODO: use?
cor(s$Elasticite_chauffage, s$Elasticite_chauffage_perso, use='complete.obs') # 0.48
cor(s$Elasticite_fuel, s$Elasticite_fuel_perso, use='complete.obs') # 0.52

decrit(s$schiste_traite, weights=s$weight) # 61% traités
decrit(s[s$schiste_approbation=='Oui',]$schiste_avantage, miss=T, weights=s[s$schiste_approbation=='Oui',]$weight) # 56% aucun, 26% emplois, 18% CC

# ## 5.2 Preferred revenue recycling
# labels_tax_condition <- c("a payment for the 50% poorest French people<br> (those earning less than 1670€/month)", "a payment to all French people", 
#                           "compensation for households forced to consume petroleum products", "a reduction in social contributions", "a VAT cut", 
#                           "a reduction in the public deficit", "the thermal renovation of buildings", "renewable energies (wind, solar, etc.)", "clean transport")
# labels_tax_condition[3] <- "compensation for households constrained<br> to consume petroleum products"
# barres(file="tax_condition", title="", data=data5(names(s)[which(names(s)=='si_pauvres'):(which(names(s)=='si_pauvres')+8)], miss=FALSE), nsp=FALSE, 
#        sort=T, legend = c(yes_no5), labels=labels_tax_condition)

# below: remove? TODO
summary(lm(cause_CC=='anthropique' ~ as.factor(age) + as.factor(diplome) + statut_emploi, data=s, weights=s$weight)) # statut_emploi = étudiant matters for lowering the (omitted) effect of 18-24
summary(lm(cause_CC=='anthropique' ~ Gauche_droite + as.factor(diplome4) + diplome4 * gauche_droite, data=s, weights = s$weight))
summary(lm(cause_CC=='anthropique' ~ Gilets_jaunes + as.factor(diplome4) + diplome4 * gilets_jaunes, data=s, weights = s$weight))
summary(lm(connaissances_CC ~ Gilets_jaunes + as.factor(diplome4) + diplome4 * gilets_jaunes, data=s, weights = s$weight))
summary(lm(connaissances_CC ~ Gauche_droite + as.factor(diplome4) + diplome4 * gauche_droite, data=s, weights = s$weight)) # .
summary(lm(cause_CC=='anthropique' ~ Gauche_droite + connaissances_CC * gauche_droite, data=s, weights = s$weight)) # **

decrit(s$taxe_approbation, weights = s$weight, miss=TRUE)
decrit(s$nb_politiques_env, weights = s$weight) #TODO: exploiter le -2/+2
decrit(s$mode_vie_ecolo, weights = s$weight, miss=TRUE)
decrit(s$normes_vs_taxes, weights = s$weight)
decrit(s$earmarked_vs_compensation, weights = s$weight)

summary(lm("taxe_approbation!='Non' ~ Gauche_droite", data=s, weights = s$weight))
summary(lm("nb_politiques_env/8 ~ Gauche_droite", data=s, weights = s$weight))
summary(lm("taxe_approbation!='Non' ~ diplome4 + as.factor(age)", data=s, weights = s$weight))

formula_determinants_nb_politiques_env_bis <- as.formula(paste("nb_politiques_env/8 ~ ", paste(variables_determinants_policy_bis, collapse = ' + ')))
ols_nb_politiques_env_bis <- lm(formula_determinants_nb_politiques_env_bis, data=s, weights = s$weight)
summary(ols_nb_politiques_env_bis)

# 4.4.2 Mobility and public transportdecrit(s$transports_avis[s$transports_avis!=-1]<2, weights = s$weight[s$transports_avis!=-1])
decrit(s$transports_travail[s$transports_travail!='Non concerné·e'], weights = s$weight[s$transports_travail!='Non concerné·e']) # 65%
# TODO: one or the other
decrit(entd$Mode, weights = entd$weight)
decrit(entd$Mode[entd$dist_subj_km <= 2], weights = entd$weight[entd$dist_subj_km <= 2])
decrit(is.na(entd$dist_subj_km)[!is.na(entd$dist_obj_km)]) # objective distance only present when subjective distance is present
decrit(entd$Mode[entd$dist_subj_km <= 2 & entd$dist_subj_km >= 1], weights = entd$weight[entd$dist_subj_km <= 2 & entd$dist_subj_km >= 1]) # 52%
decrit(entd$Mode[entd$dist_subj_km <= 2 & entd$dist_subj_km >= 1 & !is.na(entd$dist_obj_km)], weights = entd$weight[entd$dist_subj_km <= 2 & entd$dist_subj_km >= 1 & !is.na(entd$dist_obj_km)])
# when objective dist not NA, car -4%, public +4% in subjective: there is selection for calculation of objective
decrit(entd$Mode[entd$dist_obj_km <= 2 & entd$dist_obj_km >= 1], weights = entd$weight[entd$dist_obj_km <= 2 & entd$dist_obj_km >= 1])

# TODO: la seule différence entre variables 6.1 et 6.2 c'est le diplôme (continu ou pas): simplifier
variables_determinants_policy_CC[which(!(variables_determinants_policy_CC %in% variables_determinants_attitudes_CC))]
variables_determinants_attitudes_CC[which(!(variables_determinants_attitudes_CC %in% variables_determinants_policy_CC))]


##### Graphiques note Cepremap #####
barres(file="taxe_condition_val", title="", data=data5(names(s)[which(names(s)=='si_pauvres'):(which(names(s)=='si_pauvres')+8)], miss=FALSE, rev=T)[,rev(c(9,5,8,7,3,4,1,6,2))], nsp=FALSE, 
       sort=F, thin=T, legend = rep("", 5), labels=labels_taxe_condition[rev(c(9,5,8,7,3,4,1,6,2))], margin_l=250) # rev(yes_no5)

barres(file="politiques_climatiques", title="", data=data5(names(s)[(which(names(s)=='si_pauvres')+10):(which(names(s)=='si_pauvres')+17)], 
                                                           miss=FALSE, rev = T)[,rev(c(3,4,1,6,5,8,2,7))], nsp=FALSE, sort=F, legend = rep("", 5), labels=labels_politiques_env[rev(c(3,4,1,6,5,8,2,7))], thin=T, margin_l=200) # rev(yes_no5)

##### Causal complementary  effects #####
# A^T: insignificant without control, crazy coefs when P added.
 
# formula_all_p <- as.formula(paste("progressivite!='Non' ~ taxe_approbation + gain + simule_gain + simule_gagnant * info_progressivite"))
# tsls1_all_p <- lm(formula_all_p, data=s, weights = s$weight, na.action='na.exclude')
# summary(tsls1_all_p)
# s$p <- fitted.values(tsls1_all_p)
# 
# formula2_allp <- as.formula(paste("taxe_info_approbation!='Non' ~ taxe_approbation + gain + simule_gagnant + simule_gain + p"))
# tsls2_allp <- lm(formula2_allp, data=s, weights=s$weight)
# summary(tsls2_allp) 

variables_reg_min <- c("percentile_revenu", "Revenu2", "percentile_revenu_conjoint", "Revenu_conjoint2")
variables_reg_all <- variables_reg_self_interest[!(variables_reg_self_interest %in% c("taxe_efficace", "prog_na"))]
variables_all_all <- c(variables_reg_all, "taxe_approbation")
# variables_reg_all <- c(variables_reg_self_interest[!(variables_reg_self_interest %in% c("taxe_efficace", "prog_na"))], "taxe_approbation")

formula_all0_ee1 <- as.formula(paste("taxe_efficace!='Non' ~ apres_modifs + info_CC + ",  paste(variables_all_all, collapse = ' + ')))
tsls1_all0_ee1 <- lm(formula_all0_ee1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_all0_ee1)
s$taxe_efficace.all <- fitted.values(tsls1_all0_ee1)
formula_all0_si1 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~", paste(variables_all_all, collapse = ' + '), "+ traite_cible * traite_cible_conjoint + cible"))
tsls1_all0_si1 <- lm(formula_all0_si1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_all0_si1)
s$non_perdant <- fitted.values(tsls1_all0_si1)

# !! (0) A^T = ^SI * ^EE + X = 0.61*** ^SI + 0.57. ^EE - 0.32* ^SI*^EE + X (all controls)
formula2_all0 <- as.formula(paste("taxe_cible_approbation!='Non' ~", paste(variables_all_all, collapse = ' + '), " + cible + non_perdant * taxe_efficace.hat"))
tsls2_all0 <- lm(formula2_all0, data=s, weights=s$weight)
summary(tsls2_all0) 

formula_all_ee1 <- as.formula(paste("taxe_efficace!='Non' ~ apres_modifs + info_CC + ",  paste(variables_reg_all, collapse = ' + ')))
tsls1_all_ee1 <- lm(formula_all_ee1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_all_ee1)
s$taxe_efficace.hat <- fitted.values(tsls1_all_ee1)
formula_all_si1 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~", paste(variables_reg_all, collapse = ' + '), "+ traite_cible * traite_cible_conjoint + cible"))
tsls1_all_si1 <- lm(formula_all_si1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_all_si1)
s$non_perdant <- fitted.values(tsls1_all_si1)

# ! (1) A^T = ^SI * ^EE + X = 0.75*** ^SI + 0.81* ^EE - 0.55 ^SI*^EE + X (with controls)
formula2_all1 <- as.formula(paste("taxe_cible_approbation!='Non' ~", paste(variables_reg_all, collapse = ' + '), " + cible + non_perdant * taxe_efficace.hat"))
tsls2_all1 <- lm(formula2_all1, data=s, weights=s$weight)
summary(tsls2_all1) 

# (2) A^T = ^SI * ^EE * P + X = 0.6. ^SI + 2.3* ^EE - 0.3 P - 0.4 ^SI:^EE  + 1.1** ^SI:P+ 1.4** ^EE:P - 2.5* ^SI:^EE:P + X (with controls) /!\  coef EE: 74 when taxe_approbation in X. why?
formula2_all2 <- as.formula(paste("taxe_cible_approbation!='Non' ~", paste(variables_reg_all, collapse = ' + '), " + cible + non_perdant * taxe_efficace.hat * (progressivite!='Non')"))
tsls2_all2 <- lm(formula2_all2, data=s, weights=s$weight)
summary(tsls2_all2) 

# logit_tsls2_all2 <- logitmfx(formula2_all2, s, atmean=FALSE)$mfxest
# logit_tsls2_all2
# tsls2_all2 <- glm(formula2_all2, data=s, weights=s$weight, family = binomial(link='logit'))
# summary(tsls2_all2)

formula_min_ee1 <- as.formula(paste("taxe_efficace!='Non' ~ apres_modifs + info_CC + ",  paste(variables_reg_min, collapse = ' + ')))
tsls1_min_ee1 <- lm(formula_min_ee1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_min_ee1)
s$taxe_efficace.min <- fitted.values(tsls1_min_ee1)
formula_min_si1 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~", paste(variables_reg_min, collapse = ' + '), "+ traite_cible * traite_cible_conjoint + cible"))
tsls1_min_si1 <- lm(formula_min_si1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_min_si1)
s$non_perdant <- fitted.values(tsls1_min_si1)

# (1') A^T = ^SI * ^EE = 0.458^SI + 0.70 ^EE - 0.00 ^SI*^EE (no control)
formula2_min1 <- as.formula(paste("taxe_cible_approbation!='Non' ~", paste(variables_reg_min, collapse = ' + '), " + cible + non_perdant * taxe_efficace.min"))
tsls2_min1 <- lm(formula2_min1, data=s, weights=s$weight)
summary(tsls2_min1)  

# (2') A^T = ^SI * ^EE * P = -1.4 ^SI + 0.2 ^EE - 1.5 P + 5 ^SI:^EE  + 3.8 ^SI:P + 5 ^EE:P - 10 ^SI:^EE:P (no control) /!\  coef EE: 74 when taxe_approbation in X. why?
formula2_min2 <- as.formula(paste("taxe_cible_approbation!='Non' ~", paste(variables_reg_min, collapse = ' + '), " + cible + non_perdant * taxe_efficace.min * (progressivite!='Non')"))
tsls2_min2 <- lm(formula2_min2, data=s, weights=s$weight)
summary(tsls2_min2) 

# (3) A^F = ^SI * ^EE + X = 0.55*** ^SI - 0.67 ^EE - 0.49* ^SI*^EE + X (original controls)
formula_tsls1_all_si3 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + 
      Simule_gain + Simule_gain2 + single + prog_na + taxe_efficace +", paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_all_si3 <- lm(formula_tsls1_all_si3, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_all_si3)
s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_all_si3$fitted.values

formula_tsls2_all_si3 <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_reg_self_interest, collapse = ' + '),  
      " + tax_acceptance + (taxe_approbation=='NSP') + Simule_gain + Simule_gain2 + single + prog_na + non_perdant * taxe_efficace.hat"))
tsls2_all_si3 <- lm(formula_tsls2_all_si3, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
summary(tsls2_all_si3)

# (4) A^F = ^SI * ^EE + X = 0.66*** ^SI + 0.87* ^EE - 0.41 ^SI*^EE + X (with controls)
formula_tsls1_all_si4 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant +", paste(variables_reg_all, collapse = ' + ')))
tsls1_all_si4 <- lm(formula_tsls1_all_si4, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_all_si4)
s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_all_si4$fitted.values
formula2_all4 <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_reg_all, collapse = ' + '), " + non_perdant * taxe_efficace.hat"))
tsls2_all4 <- lm(formula2_all4, data=s, weights=s$weight, subset= variante_taxe_info=='f')
summary(tsls2_all4) 

# !! (4') A^F = ^SI * ^EE + X = 0.55*** ^SI + 0.52* ^EE - 0.26 ^SI*^EE + X (all controls)
formula_tsls1_all_si4 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant +", paste(variables_all_all, collapse = ' + ')))
tsls1_all_si4 <- lm(formula_tsls1_all_si4, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_all_si4)
s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_all_si4$fitted.values
formula2_all4 <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_all_all, collapse = ' + '), " + non_perdant * taxe_efficace.hat"))
tsls2_all4 <- lm(formula2_all4, data=s, weights=s$weight, subset= variante_taxe_info=='f')
summary(tsls2_all4) 

# (4'') A^F = ^SI * ^EE + X = 0.74 ^SI + 1.22 ^EE + 0.04 ^SI*^EE + X (no control)
formula_tsls1_all_si4 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + simule_gain + ", paste(variables_reg_min, collapse = ' + ')))
tsls1_all_si4 <- lm(formula_tsls1_all_si4, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_all_si4)
s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_all_si4$fitted.values
formula2_all4 <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_reg_min, collapse = ' + '), " + simule_gain + non_perdant * taxe_efficace.min"))
tsls2_all4 <- lm(formula2_all4, data=s, weights=s$weight, subset= variante_taxe_info=='f')
summary(tsls2_all4) 

# (5) A^F = ^SI * ^EE * P + X = 0.17 ^SI - 0.08 ^EE + 0.33. P + 0.19 ^EE:^SI + 0.41 ^SI:P + 0.85 ^EE:P - 1.70 ^SI:^EE:P + X (with controls)
formula_tsls1_all_si4 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + simule_gain + ", paste(variables_reg_all, collapse = ' + ')))
tsls1_all_si4 <- lm(formula_tsls1_all_si4, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_all_si4)
s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_all_si4$fitted.values
formula2_all5 <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_reg_all, collapse = ' + '), " + non_perdant * taxe_efficace.hat * (progressivite!='Non')"))
# formula2_all5 <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_all_all, collapse = ' + '), " + non_perdant * taxe_efficace.hat * (progressivite!='Non')"))
tsls2_all5<- lm(formula2_all5, data=s, weights=s$weight, subset= variante_taxe_info=='f')
summary(tsls2_all5) 

# (5') A^F = ^SI * ^EE * P + X = 0.70 ^SI + 0.70 ^EE + 0.62 P - 0.84 ^EE:^SI - 0.56 ^SI:P + 0.06 ^EE:P - 0.95 ^SI:^EE:P + X (no control)
formula_tsls1_all_si4 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + simule_gain + ", paste(variables_reg_min, collapse = ' + ')))
tsls1_all_si4 <- lm(formula_tsls1_all_si4, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_all_si4)
s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_all_si4$fitted.values
formula2_all5 <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_reg_min, collapse = ' + '), " + simule_gain + non_perdant * taxe_efficace.min * (progressivite!='Non')"))
tsls2_all5<- lm(formula2_all5, data=s, weights=s$weight, subset= variante_taxe_info=='f')
summary(tsls2_all5) 

s$v.f <- 1*(s$gagnant_feedback_categorie!='Perdant') + 1*(s$taxe_efficace!='Non')
s$w.f <- s$v.f + 1*(s$progressivite!='Non')
s$v.t <- 1*(s$gagnant_cible_categorie!='Perdant') + 1*(s$taxe_efficace!='Non')
s$w.t <- s$v.t + 1*(s$progressivite!='Non')

# ! (6) A^F = 0.48*** ^V | V = SI + EE: coef similaire à effet d'un seul motif => complémentarité
formula_tsls1_min_vf1 <- as.formula(paste("v.f ~ ", paste(variables_reg_min, collapse = ' + '), " + simule_gain + simule_gagnant + info_CC + apres_modifs"))
tsls1_min_vf1 <- lm(formula_tsls1_min_vf1, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_min_vf1)
s$v.f.hat[s$variante_taxe_info=='f'] <- tsls1_min_vf1$fitted.values
formula2_min_vf <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_reg_min, collapse = ' + '), " + simule_gain + v.f.hat"))
tsls2_min_vf <- lm(formula2_min_vf, data=s, weights=s$weight, subset= variante_taxe_info=='f')
summary(tsls2_min_vf) 

# (6') A^F = 0.47*** ^V + X (with controls) | V = SI + EE: coef similaire à effet d'un seul motif => complémentarité
formula_tsls1_all_vf1 <- as.formula(paste("v.f ~ ", paste(variables_reg_all, collapse = ' + '), " + simule_gagnant + info_CC + apres_modifs"))
tsls1_all_vf1 <- lm(formula_tsls1_all_vf1, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_all_vf1)
s$v.f.hat[s$variante_taxe_info=='f'] <- tsls1_all_vf1$fitted.values
formula2_all_vf <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_reg_all, collapse = ' + '), " + v.f.hat"))
tsls2_all_vf <- lm(formula2_all_vf, data=s, weights=s$weight, subset= variante_taxe_info=='f')
summary(tsls2_all_vf) 

# ! (6'') A^F = 0.47*** ^V + X (all controls) | V = SI + EE: coef similaire à effet d'un seul motif => complémentarité
formula_tsls1_all_vf1 <- as.formula(paste("v.f ~ ", paste(variables_all_all, collapse = ' + '), " + simule_gagnant + info_CC + apres_modifs"))
tsls1_all_vf1 <- lm(formula_tsls1_all_vf1, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_all_vf1)
s$v.f.hat[s$variante_taxe_info=='f'] <- tsls1_all_vf1$fitted.values
formula2_all_vf <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_all_all, collapse = ' + '), " + v.f.hat"))
tsls2_all_vf <- lm(formula2_all_vf, data=s, weights=s$weight, subset= variante_taxe_info=='f')
summary(tsls2_all_vf) 

# (7) A^F = 0.15*** ^W | ^W = ^V + P
# apres_modifs (i.e. info_ee) ne peut pas être inclus car il coïncide avec le sous-échantillon où la progressivité est renseignée
formula_tsls1_min_wf1 <- as.formula(paste("w.f ~ ", paste(variables_reg_min, collapse = ' + '), " + simule_gain + simule_gagnant + info_CC"))
tsls1_min_wf1 <- lm(formula_tsls1_min_wf1, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_min_wf1)
s$w.f.hat[s$variante_taxe_info=='f' & !is.na(s$progressivite)] <- tsls1_min_wf1$fitted.values
formula2_min_wf <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_reg_min, collapse = ' + '), " + simule_gain + w.f.hat"))
tsls2_min_wf <- lm(formula2_min_wf, data=s, weights=s$weight, subset= variante_taxe_info=='f')
summary(tsls2_min_wf) 

# (7') A^F = 0.11** ^W + X (with controls) | ^W = ^V + P
# apres_modifs (i.e. info_ee) ne peut pas être inclus car il coïncide avec le sous-échantillon où la progressivité est renseignée
formula_tsls1_min_wf1 <- as.formula(paste("w.f ~ ", paste(variables_reg_all, collapse = ' + '), " simule_gagnant + info_CC"))
tsls1_min_wf1 <- lm(formula_tsls1_min_wf1, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_min_wf1)
s$w.f.hat[s$variante_taxe_info=='f' & !is.na(s$progressivite)] <- tsls1_min_wf1$fitted.values
formula2_min_wf <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_reg_all, collapse = ' + '), " + w.f.hat"))
tsls2_min_wf <- lm(formula2_min_wf, data=s, weights=s$weight, subset= variante_taxe_info=='f')
summary(tsls2_min_wf) 

# (7'') A^F = 0.04. ^W + X (all controls) | ^W = ^V + P
# apres_modifs (i.e. info_ee) ne peut pas être inclus car il coïncide avec le sous-échantillon où la progressivité est renseignée
formula_tsls1_min_wf1 <- as.formula(paste("w.f ~ ", paste(variables_all_all, collapse = ' + '), " simule_gagnant + info_CC"))
tsls1_min_wf1 <- lm(formula_tsls1_min_wf1, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_min_wf1)
s$w.f.hat[s$variante_taxe_info=='f' & !is.na(s$progressivite)] <- tsls1_min_wf1$fitted.values
formula2_min_wf <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_all_all, collapse = ' + '), " + w.f.hat"))
tsls2_min_wf <- lm(formula2_min_wf, data=s, weights=s$weight, subset= variante_taxe_info=='f')
summary(tsls2_min_wf) 

# ! (8) A^T = 0.56*** ^V | V = SI + EE: coef similaire à effet d'un seul motif => complémentarité
formula_tsls1_min_vt1 <- as.formula(paste("v.t ~ ", paste(variables_reg_min, collapse = ' + '), " + cible + traite_cible * traite_cible_conjoint + info_CC + apres_modifs"))
tsls1_min_vt1 <- lm(formula_tsls1_min_vt1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_min_vt1)
s$v.t.hat <- tsls1_min_vt1$fitted.values
formula2_min_vt <- as.formula(paste("taxe_cible_approbation!='Non' ~", paste(variables_reg_min, collapse = ' + '), " + cible + v.t.hat"))
tsls2_min_vt <- lm(formula2_min_vt, data=s, weights=s$weight)
summary(tsls2_min_vt) 

# (8') A^T = 0.56*** ^V + X (with controls) | V = SI + EE: coef similaire à effet d'un seul motif => complémentarité
formula_tsls1_min_vt1 <- as.formula(paste("v.t ~ ", paste(variables_reg_all, collapse = ' + '), " + cible + traite_cible * traite_cible_conjoint + info_CC + apres_modifs"))
tsls1_min_vt1 <- lm(formula_tsls1_min_vt1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_min_vt1)
s$v.t.hat <- tsls1_min_vt1$fitted.values
formula2_min_vt <- as.formula(paste("taxe_cible_approbation!='Non' ~", paste(variables_reg_all, collapse = ' + '), " + cible + v.t.hat"))
tsls2_min_vt <- lm(formula2_min_vt, data=s, weights=s$weight)
summary(tsls2_min_vt) 

# ! (8'') A^T = 0.54*** ^V + X (all controls) | V = SI + EE: coef similaire à effet d'un seul motif => complémentarité
formula_tsls1_min_vt1 <- as.formula(paste("v.t ~ ", paste(variables_all_all, collapse = ' + '), " + cible + traite_cible * traite_cible_conjoint + info_CC + apres_modifs"))
tsls1_min_vt1 <- lm(formula_tsls1_min_vt1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_min_vt1)
s$v.t.hat <- tsls1_min_vt1$fitted.values
formula2_min_vt <- as.formula(paste("taxe_cible_approbation!='Non' ~", paste(variables_all_all, collapse = ' + '), " + cible + v.t.hat"))
tsls2_min_vt <- lm(formula2_min_vt, data=s, weights=s$weight)
summary(tsls2_min_vt) 

# (9) A^T = -0.00 ^W | ^W = ^V + P
# apres_modifs (i.e. info_ee) ne peut pas être inclus car il coïncide avec le sous-échantillon où la progressivité est renseignée
formula_tsls1_min_wt1 <- as.formula(paste("w.t ~ ", paste(variables_reg_min, collapse = ' + '), " + cible + traite_cible * traite_cible_conjoint + info_CC"))
tsls1_min_wt1 <- lm(formula_tsls1_min_wt1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_min_wt1)
s$w.t.hat[!is.na(s$progressivite)] <- tsls1_min_wt1$fitted.values
formula2_min_wt <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_reg_min, collapse = ' + '), " + cible + w.t.hat"))
tsls2_min_wt <- lm(formula2_min_wt, data=s, weights=s$weight)
summary(tsls2_min_wt) 

# (9') A^T = -0.01 ^W + X (with controls) | ^W = ^V + P
# apres_modifs (i.e. info_ee) ne peut pas être inclus car il coïncide avec le sous-échantillon où la progressivité est renseignée
formula_tsls1_min_wt1 <- as.formula(paste("w.t ~ ", paste(variables_reg_all, collapse = ' + '), " + cible + traite_cible * traite_cible_conjoint + info_CC"))
tsls1_min_wt1 <- lm(formula_tsls1_min_wt1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_min_wt1)
s$w.t.hat[!is.na(s$progressivite)] <- tsls1_min_wt1$fitted.values
formula2_min_wt <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_reg_all, collapse = ' + '), " + cible + w.t.hat"))
tsls2_min_wt <- lm(formula2_min_wt, data=s, weights=s$weight)
summary(tsls2_min_wt) 

# (9'') A^T = -0.01 ^W + X (all controls) | ^W = ^V + P
# apres_modifs (i.e. info_ee) ne peut pas être inclus car il coïncide avec le sous-échantillon où la progressivité est renseignée
formula_tsls1_min_wt1 <- as.formula(paste("w.t ~ ", paste(variables_reg_min, collapse = ' + '), " + cible + traite_cible * traite_cible_conjoint + info_CC"))
tsls1_min_wt1 <- lm(formula_tsls1_min_wt1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_min_wt1)
s$w.t.hat[!is.na(s$progressivite)] <- tsls1_min_wt1$fitted.values
formula2_min_wt <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_reg_min, collapse = ' + '), " + cible + w.t.hat"))
tsls2_min_wt <- lm(formula2_min_wt, data=s, weights=s$weight)
summary(tsls2_min_wt) 

# below we work with Yes ~ Yes instead of not No ~ not No
formula_all3_ee <- as.formula(paste("taxe_efficace=='Oui' ~ apres_modifs + info_CC + ",  paste(variables_reg_all, collapse = ' + ')))
tsls1_all3_ee <- lm(formula_all3_ee, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_all3_ee)
s$taxe_efficace.hat <- fitted.values(tsls1_all3_ee)

formula_all3_si <- as.formula(paste("gagnant_cible_categorie=='Gagnant' ~", paste(variables_all_controls, collapse = ' + '), " + cible + traite_cible * traite_cible_conjoint"))
tsls1_all3_si <- lm(formula_all3_si, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_all3_si)
s$gagnant <- fitted.values(tsls1_all3_si)

# (3) .A^T = ^.SI * ^.EE + X = 0.42*** ^.SI + 0.29 ^E.E - 0.98* ^.SI*^.EE + X (all controls)
formula2_all3 <- as.formula(paste("taxe_cible_approbation=='Oui' ~", paste(variables_all_controls, collapse = ' + '), " + cible + gagnant * taxe_efficace.hat"))
tsls2_all3 <- lm(formula2_all3, data=s, weights=s$weight)
summary(tsls2_all3) 

# (4) .A^F = ^.SI * ^.EE + X = 0.31*** ^.SI + 1.14* ^.EE + 0.06 ^.SI*^.EE + X (all controls)
formula_tsls1_all4_si <- as.formula(paste("gagnant_feedback_categorie=='Gagnant' ~ simule_gagnant +", paste(variables_all_controls, collapse = ' + ')))
tsls1_all4_si <- lm(formula_tsls1_all4_si, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_all4_si)
s$gagnant[s$variante_taxe_info=='f'] <- tsls1_all4_si$fitted.values

formula2_all4 <- as.formula(paste("taxe_feedback_approbatio=='Oui' ~", paste(variables_all_controls, collapse = ' + '), " + gagnant * taxe_efficace.hat"))
tsls2_all4 <- lm(formula2_all4, data=s, weights=s$weight, subset= variante_taxe_info=='f')
summary(tsls2_all4) 


##### Update correct among "feedback correct" #####
summary(lm(update_correct ~ gagnant_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight)) # .69***
summary(lm(update_correct ~ gagnant_categorie=='Gagnant', subset = feedback_infirme_large==T  & round(conso)==7 & !(fuel_2_1 %in% c('Diesel')), data=s, weights = s$weight)) # .54*
summary(lm(formula_update, subset = feedback_infirme_large==T  & round(conso)==7 & !(fuel_2_1 %in% c('Diesel')), data=s, weights = s$weight)) # .24



##### Test similarity of distributions #####
# Pearson's chi-square test of equality of distributions
fq <- list()
fq[['sexe']] <- list(name=c("Féminin", "Masculin"), 
                     freq=c(0.516,0.484))
fq[['csp']] <- list(name=c("Inactif", "Ouvrier", "Cadre", "Indépendant", "Intermédiaire", "Retraité", "Employé", "Agriculteur"), 
                    freq=c(0.1244,0.1214,0.0943,0.0341,0.1364,0.3279,0.1535,0.008))
fq[['region']] <- list(name=c("autre","ARA", "Est", "Nord", "IDF", "Ouest", "SO", "Occ", "Centre", "PACA"), 
                       freq=c(0.001,0.124,0.129,0.093,0.189,0.103,0.093,0.091,0.099,0.078))
fq[['age']] <- list(name=c("18-24", "25-34", "35-49", "50-64", "65+"), 
                    freq=c(0.117,0.147,0.242,0.242,0.252))
fq[['taille_agglo']] <- list(name=c(1:5), 
                             freq=c(0.2166,0.1710,0.1408,0.3083,0.1633))
fq[['diplome4']] <- list(name=c("Aucun diplôme ou brevet", "CAP ou BEP", "Baccalauréat", "Supérieur"), 
                         freq=c(0.301, 0.246, 0.168, 0.285))
variables_strata <- c('sexe', 'age', 'csp', 'diplome4', 'taille_agglo', 'region')
pvalues_representativeness <- c()
for (v in variables_strata) {
  freq_sample <- c()
  for (i in fq[[v]]$name) freq_sample <- c(freq_sample, sum((s[[v]]==i))) # *s$weight
  # print(paste(v, round(chisq.test(freq_sample, p = fq[[v]]$freq, simulate.p.value = T)$p.value, 3)))
  pvalues_representativeness <- c(pvalues_representativeness, chisq.test(freq_sample, p = fq[[v]]$freq, simulate.p.value = T)$p.value)
} 
names(pvalues_representativeness) <- variables_strata
sort(p.adjust(pvalues_representativeness, method = 'fdr'), decreasing=T) 
# Equality rejected at .01 except for sex and CSP
# divide freq_sample by 2: only diploma and age not good / by 4: diploma not good / by 5: all good (at 5%)

ttests <- equivtests_3 <- equivtests_5 <- equivtests_10p <- list()
for (v in c('sexe', 'age', 'csp', 'diplome4', 'taille_agglo', 'region')) {
  for (i in 1:length(fq[[v]]$freq)) {
    ttests[[v]] <- c(ttests[[v]], t.test((1*(s[[v]]==fq[[v]]$name[i])), mu=fq[[v]]$freq[i])$p.value)
    equivtests_5[[v]] <- c(equivtests_5[[v]], tost((s[[v]]==fq[[v]]$name[i]) - fq[[v]]$freq[i],  epsilon = 0.05)$result)
    equivtests_3[[v]] <- c(equivtests_3[[v]], tost((s[[v]]==fq[[v]]$name[i]) - fq[[v]]$freq[i],  epsilon = 0.03)$result)
    equivtests_10p[[v]] <- c(equivtests_10p[[v]], tost((s[[v]]==fq[[v]]$name[i]) - fq[[v]]$freq[i],  epsilon = 0.1*fq[[v]]$freq[i])$result)   }
  names(ttests[[v]]) <- fq[[v]]$name
}
mean(unlist(equivtests_5)=='rejected') # 97% sont équivalents
mean(unlist(equivtests_3)=='rejected') # 76%
mean(unlist(equivtests_10p)=='rejected') # 15%
ttests
sort(p.adjust(unlist(ttests), method = 'fdr'), decreasing=T) 
mean(unlist(ttests)<0.05)
mean(sort(p.adjust(unlist(ttests), method = 'fdr'), decreasing=T)<0.01) 
# good = can't reject they are equal at 5%
# sex: all good / diplome: all bad / age: 2/5 good / csp: 7/8 good / taille_agglo: 3/5 good / region: 7/10 good

tost((s$sexe=='Masculin') - 0.484,  epsilon = 0.05)
temp$
equiv.test <- function(var, null, epsilon = NULL, alpha = 0.05) { # under the assumption of normality
  if (missing(epsilon)) epsilon <- 0.05*mean(as.numeric(var))
  return(tost.stat(mean(as.numeric(var)), sd(as.numeric(var)), length(var), null = null, alpha = alpha, Epsilon = epsilon))
}
temp <- equiv.test((s$sexe=='Masculin'), 0.484, epsilon = 0.05)

variables_strata <- c('sexe', 'age', 'csp', 'diplome4', 'taille_agglo', 'region')
ttests_quotas <- list()
for (v in variables_strata) {
  for (i in 1:length(fq[[v]]$freq)) ttests_quotas[[v]] <- c(ttests_quotas[[v]], t.test((1*(s[[v]]==fq[[v]]$name[i])), mu=fq[[v]]$freq[i])$p.value) # prop.test yields same results
  names(ttests_quotas[[v]]) <- fq[[v]]$name
}
sort(p.adjust(unlist(ttests_quotas), method = 'fdr'), decreasing=T) 
mean(sort(p.adjust(unlist(ttests_quotas), method = 'fdr'), decreasing=T)<0.05) # 21%
length(which(sort(p.adjust(unlist(ttests_quotas), method = 'fdr'), decreasing=T)<0.001)) # 21%

mean_characs <- ttests_characs <- c('taille_menage'=2.36, 'nb_adultes'=2.03, 'uc'=1.60, 'gaz'=0.42, 'fioul'=0.12, 'surface'=97, 'km'=13735, 'conso'=6.39)
for (c in c('taille_menage', 'nb_adultes', 'uc', 'gaz', 'fioul', 'surface', 'km', 'conso'))  ttests_characs[c] <- t.test(as.numeric(s[[c]]), mu=mean_characs[c])$p.value 
ttests_characs
sort(p.adjust(ttests_characs, method = 'fdr'), decreasing=T) 
mean(sort(p.adjust(ttests_characs, method = 'fdr'), decreasing=T)<0.05) # 63%
length(which(sort(p.adjust(ttests_characs, method = 'fdr'), decreasing=T)<0.001)) # 5


## Testing the equality of two distributions
# Résumé : le mieux semble le G-test (qui est similaire au chi2). Hellinger est bien en théorie mais on a pas de source fiable pour la rule of thumb. On peut aussi essayer Maasoumi & Racine (2002).
# The univariate case: usually test the L^p distance between the CDFs (two first cases below). o if for ordered variables (i.e. not categorical).
# o Kolmogorov-Smirnov (KS) test: tests the L^infini distance i.e. D = max |CDF_1 - CDF_2|. Continuous distrib: ks.test() / Discrete (less conservative): KSgeneral::disc_ks_test() (or in the dgof package) https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test
# o Cramer-von Mises test: use the L^2 distance, which seems preferable. dgof::cvm.test() https://stats.stackexchange.com/questions/288416/non-parametric-test-if-two-samples-are-drawn-from-the-same-distribution
# o Anderson-Darling: not conservative like KS and other good properties (ideal to compare response times). http://www.jaqm.ro/issues/volume-6,issue-3/pdfs/1_engmann_cousineau.pdf
# o Mann-Whitney U test: tests whether two groups have similar values or if one has higher values than the other (see also Wilcoxon for equality of medians).
# - Maasoumi & Racine (2002): compare densities comprised of continuous and categorical data using entropy. np::npunitest()
# - Kullback-Leibler divergence D_KL(P||Q) (= relative entropy): measures the information loss of approximating true distribution P by Q. Is a f-divergence and a Bergman divergence.
# > G-test: similar but better than a chi-squared test. G-stat ~ D_KL. This is what should be used to test similarity between categorical distributions. AMR::g.test() https://en.wikipedia.org/wiki/G-test
# - Hellinger distance: H = ||sqrt(P)-sqrt(Q)||_2, another f-divergence. Rule-of-thumb: two distributions are close if H < 0.05. StatMatch::comp.prop (also computes L^1 distance with rule of thumb < 0.03) p. 14 https://ec.europa.eu/eurostat/documents/3888793/5855821/KS-RA-13-020-EN.PDF/477dd541-92ee-4259-95d4-1c42fcf2ef34?version=1.0 https://en.wikipedia.org/wiki/Hellinger_distance
# ? maximum mean discrepancy (Gretton et al., 2012): state-of-the-art mathematical tool for related problems. http://jmlr.csail.mit.edu/papers/v13/gretton12a.html
# - unpaired two-sample t-test or z-test: tests equality of means of two distribution using asymptotic normal approximation.
# - Wald & Wolfowitz (1940) runs test and the (Pearson's) chi-squared test: tests respectively the sign (of the difference) and the distance of two distributions under the null that the frequencies are the same. https://en.wikipedia.org/wiki/Wald%E2%80%93Wolfowitz_runs_test
# - Equivalence tests: here the null hypothesis is the dissimilarity (two one-sided t-tests). Equivalence test of mean under normality assumption: equivalence::tost(x - mean(x))
#
# The multivariate case:
# o (best) Fasano & Franceschini (1987): generalizes KS in higher dimensions*. No implementation in R. *Defining CDF as P(X<x & Y<y) doesn't yield same D (= max...) as P(X<x & Y>y): all 2^d-1 possible arrangements have to be tested, and we take the max of them. https://stats.stackexchange.com/questions/71036/test-if-multidimensional-distributions-are-the-same implementation in C and python: https://stats.stackexchange.com/questions/27288/two-dimensional-kolmogorov-smirnov
# o Peacock (1983): similar to Fasano & Franceschini but more computationally intensive. Exists only for dimension 2 and 3. Peacock.test::peacock2()
# - Li, Maasoumi & Racine (2009): compare densities comprised of continuous and categorical data using entropy. np::npdeneqtest() https://cran.r-project.org/web/packages/np/vignettes/entropy_np.pdf
# o Chacon & Duong (2018): kernel density estimates, up to dimension 6: ks::kde.test https://cran.r-project.org/web/packages/ks/ks.pdf (seen on https://stats.stackexchange.com/questions/27288/two-dimensional-kolmogorov-smirnov)


##### Role endorsement #####
summary(lm((gagnant_categorie!='Perdant') ~ info_PM, data=s))
summary(lm((gagnant_categorie!='Perdant') ~ info_CC, data=s))


##### Heterogeneity SI #####
formula_tsls1a_sio1 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible*traite_cible_conjoint*(percentile_revenu > 45) + cible + I(taxe_approbation=='NSP') + tax_acceptance + ", paste(variables_reg_self_interest, collapse = ' + ')))
tsls1a_sio1 <- lm(formula_tsls1a_sio1, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
formula_tsls1b_sio1 <- as.formula(paste("((gagnant_cible_categorie!='Perdant')*(percentile_revenu > 45)) ~ traite_cible*traite_cible_conjoint*(percentile_revenu > 45) + cible + I(taxe_approbation=='NSP') + tax_acceptance + ", paste(variables_reg_self_interest, collapse = ' + ')))
tsls1b_sio1 <- lm(formula_tsls1b_sio1, data=s, subset = (percentile_revenu > 45) & ((percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10)), weights = s$weight)
s$non_perdant[(s$percentile_revenu <= 60 & s$percentile_revenu >= 10) | (s$percentile_revenu_conjoint <= 60 & s$percentile_revenu_conjoint >= 10)] <- tsls1a_sio1$fitted.values
s$non_perdant_p45_[(s$percentile_revenu > 45) & ((s$percentile_revenu <= 60 & s$percentile_revenu >= 10) | (s$percentile_revenu_conjoint <= 60 & s$percentile_revenu_conjoint >= 10))] <- tsls1b_sio1$fitted.values
formula_tsls2_sio1 <- as.formula(paste("taxe_cible_approbation!='Non' ~ ", paste(variables_reg_self_interest, collapse = ' + '), "+ cible + I(taxe_approbation=='NSP') + tax_acceptance + non_perdant_p45_ + (percentile_revenu > 45)")) # 
tsls2_sio1 <- lm(formula_tsls2_sio1, data=s, subset = (s$percentile_revenu > 45) & ((s$percentile_revenu <= 60 & s$percentile_revenu >= 10) | (s$percentile_revenu_conjoint <= 60 & s$percentile_revenu_conjoint >= 10)), weights = s$weight)
summary(tsls2_sio1) 

iv_sio1 <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", paste(variables_reg_self_interest, collapse=' + '), 
        " + cible + I(taxe_approbation=='NSP') + tax_acceptance + (gagnant_cible_categorie!='Perdant')*(percentile_revenu > 45) |", paste(variables_reg_self_interest, collapse=' + '), 
        " + cible + I(taxe_approbation=='NSP') + tax_acceptance + traite_cible*traite_cible_conjoint*(percentile_revenu > 45)")), 
        data = s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight), diagnostics = TRUE)
iv_sio1