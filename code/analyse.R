package('tidyverse')
package("rms")
package('pwr')
package("foreign")
package("memisc")
package("DT")
package("pastecs")
package("lsr")
package("ggplot2")
package("stringr")
package("survey")
package("plotly")
package('gdata')
package("Hmisc")
package("quantreg")
package("rcompanion")
package("DescTools")
package("VCA")
package("glmnet")
package("installr")
# package("doMC") # for parallel computing, does not work on Windows


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


##### Durées #####
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


##### Caractéristiques énergétiques #####
decrit(s$surface, weights = s$weight) # 120
decrit(s$chauffage, weights = s$weight) # 50% gaz ou fioul
decrit(s$km, weights = s$weight) # 11k (moyenne 15k)
decrit(s$conso, weights = s$weight) # 6
decrit(s$nb_vehicules, weights = s$weight)


##### Gain questions générales (TVA, transports, logement) #####
decrit(s$perte_tva, weights = s$weight)
decrit(s$perte_fuel, weights = s$weight)
decrit(s$perte_chauffage, weights = s$weight) # proportions similaires pour les 3, environ 60% pensent perdre plus que la moyenne
temp1 <- temp2 <- s[,c("perte_tva", "perte_fuel", "perte_chauffage", "perte_partielle", "variante_partielle", "weight")]
temp1$perte <- temp1$perte_tva
temp1$variante_perte <- "tva"
temp2$perte <- temp2$perte_partielle
temp2$variante_perte <- temp2$variante_partielle
s_perte <- merge(temp1, temp2, all=T)
# s_perte$variante_perte <- relevel(as.factor(s_perte$variante_perte), "tva")
summary(lm(perte ~ variante_perte, data = s_perte, weights = s_perte$weight)) # *** -0.31 chauffage, -0.18 fuel: les gens s'estiment plus perdants avec hausse TVA
# TODO: restreindre aux seuls gagnants/perdants


##### Gain (dividende - hausse dépenses énergétiques) #####
decrit(110 * s$nb_adultes - s$hausse_depenses, weights = s$weight) # Ok !
decrit(s$gain, weights = s$weight) 
decrit(s$gain_taxe, weights = s$weight)
decrit(s$gain_taxe_feedback, weights = s$weight)
decrit(s$gain_taxe_feedback[s$hausse_depenses < 110 * s$nb_adultes], weights = s$weight[s$hausse_depenses < 110 * s$nb_adultes]) 
# On ne les convainc pas !!!
length(which(s$gain_taxe_feedback=='Gagnant' & s$hausse_depenses < 110 * s$nb_adultes))/length(which(!is.na(s$gain_taxe_feedback) & s$hausse_depenses < 110 * s$nb_adultes))
length(which(s$gain_taxe_feedback=='Gagnant' & s$gain_taxe != 'Gagnant' & s$hausse_depenses < 110 * s$nb_adultes))/length(which(!is.na(s$gain_taxe_feedback) & s$hausse_depenses < 110 * s$nb_adultes & s$gain_taxe != 'Gagnant'))
decrit(s$gain_taxe_progressif, weights = s$weight)

## Plot CDF
cdf_gain <- wtd.Ecdf(s$gain, weights = s$weight)
plot(cdf_gain$x, cdf_gain$ecdf, type='s', lwd=2, col='orange', xlab="Category of subjective gain", ylab="Distribution of answers") + grid()


##### Approbation #####
decrit(s$taxe_approbation) # 11% Dur !!!
decrit(s$taxe_approbation[s$gilets_jaunes_soutien==TRUE])
decrit(s$taxe_approbation[s$gilets_jaunes_oppose==TRUE])
decrit(s$taxe_feedback_approbation) # 17%
decrit(s$taxe_efficace) # 18%
decrit(s$taxe_progressif_approbation) # 19%
summary(lm((taxe_feedback_approbation=='Oui') ~ (gain_taxe!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_progressif_approbation=='Oui') ~ (gain_taxe!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_approbation=='Oui') ~ (taxe_efficace!='Non') + (gain_taxe!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_approbation=='Oui') ~ score_climate_call + score_polluants + (gain_taxe!='Perdant'), data=s, weights = s$weight))


##### OLS Approbation ####
summary(lm((taxe_approbation=='Oui') ~ revenu + rev_tot + hausse_carburants + hausse_chauffage + score_climate_call + score_ges
              + Gauche_droite + emission_cible + effets_CC + ecologiste + conservateur + liberal + humaniste + patriote + apolitique
              + sexe + age + diplome4 + statut_emploi + csp + region, 
            data=s))



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
                              "sexe", "age", "diplome4", "statut_emploi", "csp", "region") # TODO: complete the list
ols <- summary(lm(as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_big_regression, collapse=' + '))), data=s))
ols
sum(ols$residuals^2)
anova <- summary(aov(as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_big_regression, collapse=' + '))), data=s))
anova
sum_sq <- anova[[1]]$'Sum Sq'
explained_variance <- sum(sum_sq[-length(sum_sq)])
print(paste("Share of explained variance: ", round(explained_variance/sum(sum_sq), 2), ". Among which, share of explained variance explained by each variable:", sep=""))
for (i in 1:length(variables_big_regression)) print(paste(variables_big_regression[order(sum_sq[-length(sum_sq)], decreasing = T)][i], round(sort(sum_sq[-length(sum_sq)], decreasing = T)[i]/explained_variance,3)))
summary(lm((taxe_approbation=='Oui') ~ ecologiste + conservateur, data=s))


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

for (v in variables_big_regression) { # display and remove variables with missing values
  na_v <- length(which(is.na(s[[v]])))
  # if ("labelled" %in% class(s[[v]])) na_v <- length(which(is.na(s[[v]]))) #
  # else na_v <- length(which(is.missing(s[[v]]))) 
  if (na_v>0) {
    print(paste(v, na_v))
    variables_big_regression <- variables_big_regression[variables_big_regression!=v] }
}
  
x <- model.matrix(as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_big_regression, collapse=' + '))),  data=s)
y <- ifelse(s$taxe_approbation=="Oui", 1, 0)
#perform grid search to find optimal value of lambda
#family= binomial => logistic regression, alpha=1 => lasso 
# check docs to explore other type.measure options
fit <- glmnet(x, y, alpha=1, family="binomial", weights = s$weight, type.multinomial = "grouped")
cv.out <- cv.glmnet(x, y, alpha=1, family="binomial", weights = s$weight, type.multinomial = "grouped") # TODO: how to choose alpha?; run parallel computing: parallel = T
plot(cv.out)
coefs_lasso <- coef(cv.out, s="lambda.1se") # lambda.min lambda.1se
coefs_lasso <- coef(cv.out, s="lambda.min") # TODO: group variables
data.frame(name = coefs_lasso@Dimnames[[1]][coefs_lasso@i + 1], coefficient = coefs_lasso@x) # doesn't work for multinomial
selected_variables <- coefs_lasso@i - 1
selected_variables <- selected_variables[selected_variables > 0 & selected_variables <= length(variables_big_regression)]
selected_variables <- variables_big_regression[selected_variables]
summary(glm(as.formula(paste("(taxe_approbation=='Oui') ~", paste(selected_variables, collapse=' + '))), binomial, data=s, weights=s$weight))
# lasso <- glmnet(x, y, alpha=1, family="binomial")

# find alpha
foldid=sample(1:10,size=length(y),replace=TRUE)
cv1=cv.glmnet(x,y,foldid=foldid,alpha=1)
cv.5=cv.glmnet(x,y,foldid=foldid,alpha=.5)
cv0=cv.glmnet(x,y,foldid=foldid,alpha=0)

##### Logit Approbation #####
# All variables we can think of (TODO: complete the list)
logit_all <- glm((taxe_approbation=='Oui') ~ revenu + rev_tot + hausse_carburants + hausse_chauffage + score_climate_call + score_ges
              + Gauche_droite + emission_cible + effets_CC + ecologiste + conservateur + liberal + humaniste + patriote + apolitique
              + sexe + age + diplome4 + statut_emploi + csp + region, 
            family = "binomial", data=s)
summary(logit_all)
PseudoR2(logit_all)

# Only significant variables
summary(glm((taxe_approbation=='Oui') ~ hausse_chauffage + score_climate_call + ecologiste + conservateur + humaniste + sexe , 
            binomial, data=s))

# Only demographics
summary(glm((taxe_approbation=='Oui') ~ revenu + rev_tot + sexe + age + region, 
            family = "binomial", data=s)) # *: -age, +revenu, -rev_tot, +Homme, qq regions
summary(glm((taxe_approbation=='Oui') ~ revenu + rev_tot + sexe + age + diplome4 + statut_emploi + csp + region, 
            family = "binomial", data=s)) # *: +revenu, -rev_tot, +Homme, qq regions
summary(glm((taxe_approbation=='Oui') ~ revenu + rev_tot + sexe + Age + Diplome + region, 
            family = "binomial", data=s)) # *: +revenu, -rev_tot, +Homme, qq regions

# Demographics + politics
summary(glm((taxe_approbation=='Oui') ~ revenu + rev_tot + sexe + age + diplome4 + statut_emploi + csp + region
            + Gauche_droite + ecologiste + conservateur + liberal + humaniste + patriote + apolitique, 
            family = "binomial", data=s)) # *: apolitique, ecologiste, conservateur, +revenu, -rev_tot, +Homme, qq regions
summary(glm((taxe_approbation=='Oui') ~ revenu + rev_tot + sexe + age + diplome4 + statut_emploi + csp + region
            + Gauche_droite + ecologiste + conservateur + liberal + humaniste + patriote + apolitique, 
            family = "binomial", data=s)) # *: apolitique, ecologiste, conservateur, +revenu, -rev_tot, +Homme, qq regions


##### Probit Approbation #####
summary(glm(, family = binomial(link = "probit", data=s)))


##### Elasticites #####
decrit(s$Elasticite_fuel[!is.na(s$Elasticite_fuel)])
decrit(s$Elasticite_fuel_perso[!is.na(s$Elasticite_fuel_perso)]) #
decrit(s$Elasticite_chauffage[!is.na(s$Elasticite_chauffage)]) #
decrit(s$Elasticite_chauffage_perso[!is.na(s$Elasticite_chauffage_perso)]) #

summary(lm(Elasticite_fuel ~ (taxe_efficace=='Oui'), data=s, weights = s$weight))
summary(lm(Elasticite_chauffage ~ (taxe_efficace=='Oui'), data=s, weights = s$weight)) # Aucun lien évident élasticité / efficacité environnementale

cor(s$Elasticite_fuel[!is.na(s$Elasticite_fuel)], s$Elasticite_fuel_perso[!is.na(s$Elasticite_fuel_perso)])
cor(s$Elasticite_chauffage[!is.na(s$Elasticite_chauffage)], s$Elasticite_chauffage_perso[!is.na(s$Elasticite_chauffage_perso)])
# Correlation positive entre �lasticit� perso et �lasticit� globale


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
summary(lm((schiste_avantage=='Cela permettrait de créer des emplois et dynamiser les départements concernés') ~ (schiste_traite==1), data=s, subset=schiste_approbation=='Oui' & schiste_avantage !='Aucune de ces deux raisons', weights = s$weight))
summary(lm((schiste_CC=='Elle est valable : toute baisse des émissions va dans la bonne direction') ~ (schiste_traite==1), data=s, subset=schiste_approbation=='Oui' & schiste_CC !='NSP (Ne sait pas, ne se prononce pas)', weights = s$weight))


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
decrit(s$gauche_droite, miss=T) # TODO: pb (should not be number, and missing should be here)
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
for (j in 141:148) print(decrit(s[[j]], weights=s$weight))
