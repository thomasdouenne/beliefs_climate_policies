source("packages_functions.R")

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
abline(lm(gain ~ simule_gain, data=s), col='blue', lwd=2)
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
fit_housing$vrai_gain_chauffage <- 50 * pmin(2, fit_housing$nb_adultes) - fit_housing$obj
fit_housing$estimation_gain_chauffage <- 50 * pmin(2, fit_housing$nb_adultes) - fit_housing$fit
ggplot(data=fit_housing, aes(x=vrai_gain_chauffage)) + 
  geom_smooth(method = "auto", aes(y=1*(estimation_gain_chauffage > 0))) + ylim(c(0,1)) + xlab("Objective gain without fuel (density in black)") + ylab("P(gain - (hausse_carburants-60) > 0) i.e. proba gain") + xlim(c(-200, 120)) + geom_density(aes(y=..scaled..)) + geom_vline(xintercept=0, col='red')
# ggplot(data=fit_housing, aes(x=obj)) + geom_smooth(aes(y=1*(fit < 110)), method = "glm", method.args = list(family = "binomial")) + ylim(c(0,1)) + xlab("Objective housing expenditure increase (density in black)") + ylab("P(hausse_chauffage_interaction < 110) i.e. proba gain") + xlim(c(0, 500)) + geom_density(aes(y=..scaled..)) + geom_vline(xintercept=110, col='red')
# length(which(fit_housing$obj > 50 & fit_housing$obj < 200))/length(fit_housing$obj) # 25%
# length(which(fit_housing$obj > 75 & fit_housing$obj < 170))/length(fit_housing$obj) # 15%
length(which(fit_housing$vrai_gain_chauffage > -50 & fit_housing$vrai_gain_chauffage < 50))/length(fit_housing$vrai_gain_chauffage) # 20% de chances d'avoir une proba entre 0.1 et 0.9
length(which(fit_housing$estimation_gain_chauffage > 0 & fit_housing$vrai_gain_chauffage==50))/length(which(fit_housing$vrai_gain_chauffage==50))
sum(s$weight[s$simule_gain_interaction > -50 & s$simule_gain_interaction < 50])/sum(s$weight) # 21%
ggplot(data=s, aes(x=hausse_carburants)) + geom_density() + xlim(c(0, 300))
ggplot(data=s, aes(x=simule_gain_interaction)) + geom_density() + xlim(c(-200, 300))
ggplot(data=s, aes(x=60 * pmin(2, nb_adultes) - hausse_carburants)) + geom_density() + xlim(c(-100, 200))


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
# summary(lm(as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_wo_missing, collapse=' + '))), data=s))
x <- model.matrix(as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_wo_missing, collapse=' + '))),  data=s)
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
summary(lm(as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_big_regression, collapse=' + '))), data=s))

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
# ols <- summary(lm(as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_big_regression, collapse=' + '))), data=s))
# ols
# sum(ols$residuals^2)
# anova <- summary(aov(as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_big_regression, collapse=' + '))), data=s))
# anova
# sum_sq <- anova[[1]]$'Sum Sq'
# explained_variance <- sum(sum_sq[-length(sum_sq)])
# print(paste("Share of explained variance: ", round(explained_variance/sum(sum_sq), 2), ". Among which, share of explained variance explained by each variable:", sep=""))
# for (i in 1:length(variables_big_regression)) print(paste(variables_big_regression[order(sum_sq[-length(sum_sq)], decreasing = T)][i], round(sort(sum_sq[-length(sum_sq)], decreasing = T)[i]/explained_variance,3)))
# summary(lm((taxe_approbation=='Oui') ~ ecologiste + conservateur, data=s))
# 
# anovaVCA(as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_big_regression, collapse=' + '))), Data=s)
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
# All variables we can think of (TODO: complete the list)
logit_all <- glm((taxe_approbation!='Non') ~ revenu + rev_tot + hausse_carburants + hausse_chauffage + score_climate_call + score_ges
              + Gauche_droite + emission_cible + effets_CC + ecologiste + conservateur + liberal + humaniste + patriote + apolitique
              + sexe + age + diplome4 + statut_emploi + csp + region, 
            family = "binomial", data=s)
summary(logit_all)
PseudoR2(logit_all)

# Only significant variables
summary(glm((taxe_approbation!='Non') ~ hausse_chauffage + score_climate_call + ecologiste + conservateur + humaniste + sexe , 
            binomial, data=s))

# Only demographics
summary(glm((taxe_approbation!='Non') ~ revenu + rev_tot + sexe + age + region, 
            family = "binomial", data=s)) # *: -age, +revenu, -rev_tot, +Homme, qq regions
summary(glm((taxe_approbation!='Non') ~ revenu + rev_tot + sexe + age + diplome4 + statut_emploi + csp + region, 
            family = "binomial", data=s)) # *: +revenu, -rev_tot, +Homme, qq regions
summary(glm((taxe_approbation!='Non') ~ revenu + rev_tot + sexe + Age + Diplome + region, 
            family = "binomial", data=s)) # *: +revenu, -rev_tot, +Homme, qq regions

# Demographics + politics
summary(glm((taxe_approbation!='Non') ~ revenu + rev_tot + sexe + age + diplome4 + statut_emploi + csp + region
            + Gauche_droite + ecologiste + conservateur + liberal + humaniste + patriote + apolitique, 
            family = "binomial", data=s)) # *: apolitique, ecologiste, conservateur, +revenu, -rev_tot, +Homme, qq regions
summary(glm((taxe_approbation!='Non') ~ revenu + rev_tot + sexe + age + diplome4 + statut_emploi + csp + region
            + Gauche_droite + ecologiste + conservateur + liberal + humaniste + patriote + apolitique, 
            family = "binomial", data=s)) # *: apolitique, ecologiste, conservateur, +revenu, -rev_tot, +Homme, qq regions


##### Probit Approbation #####
summary(glm(, family = binomial(link = "probit", data=s)))


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

summary(lm(Elasticite_fuel_perso ~ (taille_agglo == 'rural') + (taille_agglo == '-20k') + (taille_agglo == '20-100k') + (taille_agglo == '+100k'), data=s, subset=variante_partielle=='f', weights = s$weight))
summary(lm(Elasticite_fuel_perso ~ Revenu, data=s, subset=variante_partielle=='f', weights = s$weight))

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


##### Engagement personnel ######
decrit(s$mode_vie_ecolo, weights = s$weight) # 79% !
decrit(s$mode_vie_ecolo, miss=T, weights = s$weight) # 65%
decrit(s$enfant_CC, weights = s$weight)
decrit(s$enfant_CC_pour_CC[s$enfant_CC=='Oui'], weights = s$weight[s$enfant_CC=='Oui'])
decrit(s$enfant_CC_pour_lui[s$enfant_CC=='Oui'], weights = s$weight[s$enfant_CC=='Oui'])
summary(lm((enfant_CC=='Oui') ~ sexe, data=s))


##### Connaissances et opinions CC #####
decrit(s$ges_avion, weights = s$weight) # 47%
decrit(s$ges_boeuf, weights = s$weight) # 47%
decrit(s$ges_nucleaire, weights = s$weight) # 48%
decrit(s$ges_co2, weights = s$weight) # 81%
decrit(s$ges_ch4, weights = s$weight) # 48%
decrit(s$ges_o2, weights = s$weight) # 6%
decrit(s$ges_pm, weights = s$weight) # 58%
decrit(s$score_polluants, weights = s$weight)
decrit(s$score_climate_call, weights = s$weight)
decrit(s$emission_cible, weights = s$weight) # 5
decrit(s$cause_CC, miss=T, weights = s$weight)


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
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info*apres_modifs, data = s, weights = s$weight)) # 0 !
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info*apres_modifs, data = s, subset = transferts_inter!='NSP', weights = s$weight)) # 0 !
decrit(s$variation_aide, weights = s$weight)
load('p_data.RData')
t <- merge(s, t_transferts_inter_a, all=T)
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
decrit(s$aide_non_etats)
decrit(s$aide_non_priorite)
decrit(s$aide_non_global)
decrit(s$aide_non_trop)
decrit(s$aide_non_autonomie)


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


##### Miscellanous #####
decrit(s$rattrapage_diesel, miss = T, weights = s$weight)
decrit(s$hausse_diesel, weights = s$weight) 
summary(lm((rattrapage_diesel!='Non') ~ (diesel==TRUE), data=s, weights = s$weight))
for (j in names(s)) if (grepl('gilets_jaunes', j)) print(decrit(s[[j]], weights=s$weight))


##### Approbation politiques environnementales #####
for (j in names(s)) if (grepl('si_', j)) print(decrit(s[[j]], weights=s$weight))
for (j in 102:109) print(decrit(s[[j]], weights=s$weight))
labels_politiques_env <- c()
for (j in 109:109) labels_politiques_env <- c(labels_politiques_env, gsub(" - Q74", "", gsub(".*) à ", "", Label(s[[j]]))))  
labels_taxe_condition <- c()
for (j in 92:100) labels_taxe_condition <- c(labels_taxe_condition, gsub(" - .*", "", gsub(".*: ", "", Label(s[[j]])))) 
labels_taxe_condition[1] <- "un versement pour les 50% de Français les plus modestes<br> (ceux gagnant moins de 1670€/mois)"
labels_taxe_condition[3] <- "une compensation pour les ménages contraints<br> dans leur consommation de produits pétroliers"
labels_environmental_policies <- c("a tax on kerosene (aviation)", "a tax on red meat", "stricter insulation standards for new buildings", "stricter standards on pollution from new vehicles", "stricter standards on pollution during roadworthiness tests", "the prohibition of polluting vehicles in city centres", "the introduction of urban tolls", "a contribution to a global climate fund")
labels_tax_condition <- c("a payment for the 50% poorest French people<br> (those earning less than 1670€/month)", "a payment to all French people", "compensation for households forced to consume petroleum products", "a reduction in social contributions", "a VAT cut", "a reduction in the public deficit", "the thermal renovation of buildings", "renewable energies (wind, solar, etc.)", "clean transport")
labels_tax_condition[3] <- "compensation for households constrained<br> to consume petroleum products"
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
for (v in variables_benefices[1:(length(variables_benefices)-2)]) {
  labels_benefices <- c(labels_benefices, gsub(" - .*", "", gsub(".*: ", "", Label(s[[v]]))))
  values_benefices <- c(values_benefices, sum(s$weight[which(s[[v]]==T)])/sum(s$weight)) }
labels_problemes <- c()
values_problemes <- c()
for (v in variables_problemes[1:(length(variables_problemes)-2)]) {
  labels_problemes <- c(labels_problemes, gsub(" - .*", "", gsub(".*: ", "", Label(s[[v]]))))
  values_problemes <- c(values_problemes, sum(s$weight[which(s[[v]]==T)])/sum(s$weight)) }

# oui_non(margin_l=430, variables_benefices[1:(length(variables_benefices)-2)], "barres_benefices", labels_benefices)
# oui_non(margin_l=430, variables_problemes[1:(length(variables_problemes)-2)], "barres_problemes", labels_problemes)

# TODO: utiliser nb_bénéfices / problèmes cochés, voire l'influence des traitements

barres_benefices <- barres(file="benefices", title="<b>Bénéfices d'une taxe carbone compensée</b><br>(choix multiples)", data=matrix(values_benefices, ncol=length(values_benefices)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_benefices, hover=labels_benefices, legend="empty")
barres_problemes <- barres(file="problemes", title="<b>Problèmes d'une taxe carbone compensée</b><br>(choix multiples)", data=matrix(values_problemes, ncol=length(values_problemes)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_problemes, hover=labels_problemes, legend="empty")
barres_benefices
barres_problemes
# orca(barres_problemes, "../images/problemes.png")
# orca(barres_benefices, "../images/benefices.png")

# plot(1:3,1:3)
# dev.copy(png, filename="../images/test.png") # plot from R (not from plotly)
# dev.off()


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
abline(lm(gain ~ simule_gain, data=s), col='blue', lwd=2)
lines(seq(-500, 500, by=10), seq(-500, 500, by=10), type='l', col='black') + grid()
abline(h = 0, v=0)
cor(s$gain, s$simule_gain) # 0.05
summary(lm(gain ~ simule_gain, data=s))
sum(s$weight[s$simule_gain < s$gain])/sum(s$weight) # 11% surestiment leurs gains

b <- wtd.mean(s$simule_gain - s$gain, weights = s$weight) # 136
# b <- median(s$simule_gain - s$gain) # 161

loess_gains <- loess((gagnant_categorie!='Perdant') ~ I(simule_gain-b), data=s)
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

  lowess_gains <- loess((gagnant_categorie!='Perdant') ~ I(simule_gain - b_i), data=s)
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
probit_si4 <- glm(taxe_cible_approbation!='Non' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + tax_acceptance, family = binomial(link='probit'), data=s)
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
probitmfx(tax_acceptance ~ traite_cible, data=s)
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
probit_ee5 <- glm(tax_acceptance ~ taxe_efficace.hat + non_perdant + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + taille_agglo + Simule_gain + Simule_gain2, binomial(link='probit'), data=s)
summary(probit_ee5)
probit_ee5_margins <- summary(margins(data=s, model=probit_ee5))
probit_ee5_margins

# (5bis) 20 p.p. *** change to approval (this is what reduces coef) and tax not ineffective, complete controls
s$taxe_inefficace.hat <- n(s$taxe_efficace!='Non')
variables_socio_demo <- c(variables_demo, variables_politiques, variables_energie)
# probit_ee5bis <- glm(formula_ols_ee4bis, binomial(link='probit'), data=s)
formula_probit_ee5bis <- as.formula(paste("tax_approval ~ taxe_inefficace.hat +", paste(variables_socio_demo[!(variables_socio_demo %in% 
          c("age_50_64", "age_65_plus", "gaz", "fioul", "hausse_chauffage", "hausse_depenses", "simule_gain"))], collapse = ' + ')))
probit_ee5bis <- glm(formula_probit_ee5bis, binomial(link='probit'), data=s)
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
summary(lm(tax_acceptance ~ fitted.values(prog1), weights=s$weight, data=s))

s$progressif <- n(s$progressivite=='Oui') 
# (1) OLS simple: 22 p.p.***
ols_prog0 <- lm(tax_acceptance ~ progressif, weights=s$weight, data=s)
summary(ols_prog0)

# (2) OLS: 20 p.p.***
formula_ols_prog1 <- as.formula(paste("tax_acceptance ~ progressif + ", paste(variables_demo, collapse=' + ')))
ols_prog1 <- lm(formula_ols_prog1, weights=s$weight, data=s)
summary(ols_prog1)

# (3) OLS tous contrôles: 12 p.p.***
variables_exogenes <- c(variables_qualite, variables_demo, variables_transport, variables_politiques, variables_energie,
                      variables_Elasticite, variables_elasticite, variables_gilets_jaunes, 
                      variables_politiques_environnementales, variables_connaissances_CC, variables_avis_CC)
formula_ols_prog2 <- as.formula(paste("tax_acceptance ~ progressif + ", paste(variables_exogenes, collapse=' + ')))
ols_prog2 <- lm(formula_ols_prog2, weights=s$weight, data=s)
summary(ols_prog2)

# (4) probit: 21 p.p.***
logit_prog0 <- glm(tax_acceptance ~ progressif, binomial(link="logit"), data=s)
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
nols_prog0 <- lm(tax_acceptance ~ non_progressif, weights=s$weight, data=s)
summary(nols_prog0)

# (2) OLS: 20 p.p.***
nformula_ols_prog1 <- as.formula(paste("tax_acceptance ~ non_progressif + ", paste(variables_demo, collapse=' + ')))
nols_prog1 <- lm(nformula_ols_prog1, weights=s$weight, data=s)
summary(nols_prog1)

# (3) OLS tous contrôles: 12 p.p.***
nformula_ols_prog2 <- as.formula(paste("tax_acceptance ~ non_progressif + ", paste(variables_exogenes, collapse=' + ')))
nols_prog2 <- lm(nformula_ols_prog2, weights=s$weight, data=s)
summary(nols_prog2)

# (4) probit: 21 p.p.***
nprobit_prog0 <- glm(tax_acceptance ~ non_progressif, binomial(link="probit"), data=s)
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
pols_prog2 <- lm(formula_pols_prog2, weights=s$weight, data=s)
summary(pols_prog2)

# (6bis) OLS Approval ~ progressivite != Non, tous contrôles
formula_pnols_prog2 <- as.formula(paste("tax_approval ~ non_progressif + ", paste(variables_exogenes, collapse=' + ')))
pnols_prog2 <- lm(formula_pnols_prog2, weights=s$weight, data=s)
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


##### Graphiques RDD #####
# Seul le revenu du répondant est pris en compte, sont exclus de l'analyse les répondants au revenu > 2220
deciles <- c(780, 1140, 1430, 1670, 2220)
arguments_rdd <- matrix(c(-Inf, deciles[1:3], -Inf, deciles[2:4], deciles[c(1:4,2:5)], rep(T, 4), rep(FALSE, 4)), nrow=8)
ci_mean_cible_acceptance <- apply(arguments_rdd, 1, function(row) { low <- row[1];  up <- row[2];  traite <- row[3]; return(binconf(sum(s$weight[s$taxe_cible_approbation!='Non' & s$revenu >= low & s$revenu < up & s$traite_cible==traite]), sum(s$weight[s$revenu >= low & s$revenu < up & s$traite_cible==traite]), 0.1))})

mar_old <- par()$mar
par(mar=c(4.1, 4.1, 1.1, 1.1))
# RDD All incomes TODO: add confidence interval
plot(c(0, 780), rep(ci_mean_cible_acceptance[1,1], 2), type='l', col = 'cyan', lwd=2,
     xlab="Income of respondent (€/month)", ylab="Average Targeted Tax Acceptance", xlim=c(500, 2000), ylim=c(0.2, 0.55), xaxt='n')
lines(c(780, 1140), rep(ci_mean_cible_acceptance[1,2], 2), type='l', col = 'deepskyblue3', lwd=2)
lines(c(1140, 1430), rep(ci_mean_cible_acceptance[1,3], 2), type='l', col = 'blue', lwd=2)
lines(c(1430, 1670), rep(ci_mean_cible_acceptance[1,4], 2), type='l', col = 'black', lwd=2)
lines(c(780, 1140), rep(ci_mean_cible_acceptance[1,5], 2), type='l', col = 'cyan', lwd=2, lty=2)
lines(c(1140, 1430), rep(ci_mean_cible_acceptance[1,6], 2), type='l', col = 'deepskyblue3', lwd=2, lty=2)
lines(c(1430, 1670), rep(ci_mean_cible_acceptance[1,7], 2), type='l', col = 'blue', lwd=2, lty=2)
lines(c(1670, 2220), rep(ci_mean_cible_acceptance[1,8], 2), type='l', col = 'black', lwd=2, lty=2)
plotCI(x=c((c(500, deciles[c(1:3,1:4)]) + c(deciles[c(1:4,2:4)], 2000))/2) + c(rep(-30,4), rep(30,4)), 
       y=ci_mean_cible_acceptance[1,], li=ci_mean_cible_acceptance[2,], ui=ci_mean_cible_acceptance[3,], add=T,
       col=c('cyan', 'deepskyblue3', 'blue', 'black', 'cyan', 'deepskyblue3', 'blue', 'black'), lwd=0.7, pch=NA)
axis(1, at=c(500, 780, 1140, 1430, 1670, 2000))
grid() + abline(v = c(780, 1140, 1430, 1670), lwd=0.5)
legend("topright", lwd=2, lty=c(1,2), col=c("blue"), title.col = "black", legend=c("Transfer to respondent", "No transfer")) # , text.col = c("blue")

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