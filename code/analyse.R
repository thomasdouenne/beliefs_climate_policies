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


##### Gain (dividende - hausse dépenses énergétiques) #####
decrit(110 * s$nb_adultes - s$hausse_depenses, weights = s$weight) # Ok !
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


##### Approbation #####
decrit(s$taxe_approbation) # 11% Dur !!!
decrit(s$taxe_approbation[s$gilets_jaunes_soutien==TRUE])
decrit(s$taxe_approbation[s$gilets_jaunes_oppose==TRUE])
decrit(s$taxe_feedback_approbation) # 17%
decrit(s$taxe_efficace) # 18%
decrit(s$taxe_progressif_approbation) # 19%
summary(lm((taxe_feedback_approbation=='Oui') ~ (gagnant_categorie!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_progressif_approbation=='Oui') ~ (gagnant_categorie!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_approbation=='Oui') ~ (taxe_efficace!='Non') + (gagnant_categorie!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_approbation=='Oui') ~ score_climate_call + score_polluants + (gagnant_categorie!='Perdant'), data=s, weights = s$weight))


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
variables_taxe_gagnant <- c("taxe_gagnant_personne", "taxe_gagnant_pauvres", "taxe_gagnant_moyennes", "taxe_gagnant_riches", "taxe_gagnant_tous", "taxe_gagnant_citadins", "taxe_gagnant_certains", "taxe_gagnant_NSP")
variables_taxe_perdant <- c("taxe_perdant_personne", "taxe_perdant_pauvres", "taxe_perdant_moyennes", "taxe_perdant_riches", "taxe_perdant_tous", "taxe_perdant_ruraux", "taxe_perdant_certains", "taxe_perdant_NSP")

variables_approbation <- c("taxe_approbation", "taxe_feedback_approbation", "taxe_progressif_approbation", "taxe_cible_approbation") # TODO: group feedback/progressif
variables_qualite <- c("duree", "duree_info", "duree_champ_libre") # champ_libre != "", exclu, "test_qualite"
variables_aleatoires <- c("info_CC", "info_PM", "variante_monetaire", "apres_modifs", "variante_taxe_info", "variante_progressivite", # cible categorie_cible
                          "info_progressivite") # TODO: missing values variante_progressivite variante_monetaire
variables_demo <- c("sexe", "age", "statut_emploi", "csp", "region", "diplome", "taille_menage", "revenu", "rev_tot", "nb_14_et_plus", "nb_adultes", 
                    "fume", "actualite", "taille_agglo", "uc", "niveau_vie") # weight, TODO: each age
variables_energie <- c("surface", "mode_chauffage", "chauffage", "km", "conso", "diesel", "essence", "nb_vehicules", "gaz", "fioul", "simule_gagnant", # hausse_carburants 
                       "hausse_chauffage", "hausse_diesel", "hausse_essence", "hausse_depenses") # TODO: missing values simule_gagnant, conso, chauffage, mode_chauffage, 
variables_transport <- c("transports_distance", "transports_frequence", "transports_avis", "transports_travail", "transports_courses", "transports_loisirs", 
                         "transports_travail_commun", "transports_travail_actif") # TODO: missing values distance, travail
variables_politiques <- c("interet_politique", "conservateur", "liberal", "humaniste", "patriote", "apolitique", "ecologiste", "Gauche_droite")
variables_gilets_jaunes <- c("gilets_jaunes_dedans", "gilets_jaunes_soutien", "gilets_jaunes_compris", "gilets_jaunes_oppose", "gilets_jaunes_NSP")
variables_gains_subjectifs <- c("perte_relative_tva", "perte_relative_partielle", "gagnant_categorie_partielle", "gain_partielle", "gagnant_categorie", "gain", "gagnant_feedback_categorie", 
                                "gagnant_progressif_categorie", "gain_cible") # TODO: group feedback/progressif
variables_Elasticite <- c("Elasticite_fuel", "Elasticite_fuel_perso", "Elasticite_chauffage", "Elasticite_chauffage_perso")
variables_elasticite <- c("elasticite_fuel", "elasticite_fuel_perso", "elasticite_chauffage", "elasticite_chauffage_perso") # TODO: group fuel/chauffage
variables_taxe_croyances <- c("taxe_efficace", variables_taxe_gagnant, variables_taxe_perdant, "progressivite_feedback_avec_info", 
                              "progressivite_feedback_sans_info", "progressivite_progressif") # TODO: group avec/sans info
variables_benefices <- names(s)[which(grepl("benefice", names(s)))[which(grepl("benefice", names(s)))>300]]
variables_problemes <- names(s)[which(grepl("problemes", names(s)))[which(grepl("problemes", names(s)))>300]]
variables_taxe_condition <- c("si_pauvres", "si_compensee", "si_contraints", "si_baisse_cotsoc", "si_baisse_tva", "si_baisse_deficit", "si_renovation", "si_renouvelables", "si_transports")
variables_politiques <- c("taxe_kerosene", "taxe_viande", "normes_isolation", "normes_vehicules", "controle_technique", "interdiction_polluants", 
                          "peages_urbains", "fonds_mondial") # "rattrapage_diesel"
variables_connaissances_CC <- c("cause_CC", "ges_CO2", "ges_CH4", "ges_O2", "ges_pm", "ges_boeuf", "ges_nucleaire", "ges_avion", "region_CC", 
                                "emission_cible", "score_ges", "score_climate_call")
variables_avis_CC <- c("parle_CC", "effets_CC", "generation_CC_1960", "generation_CC_1990", "generation_CC_2020", "generation_CC_2050", "generation_CC_aucune",
                       "responsable_CC_chacun", "responsable_CC_riches", "responsable_CC_govts", "responsable_CC_etranger", "responsable_CC_passe", 
                       "responsable_CC_nature", "enfant_CC") # TODO: generation_min generation_max , "enfant_CC_pour_lui", "enfant_CC_pour_CC"
variables_comportement_CC <- c("mode_vie_ecolo", "changer_si_politiques", "changer_si_moyens", "changer_si_tous", "changer_non_riches", "changer_non_interet", "changer_non_negation", "changer_deja_fait", "changer_essaie")
variables_schiste <- c("schiste_approbation", "schiste_avantage", "schiste_CC", "schiste_traite")
variables_transferts_inter <- c("transferts_inter", "aide_2p", "transferts_inter_info", "aide_non_autonomie", "aide_non_priorite", "aide_non_etats", 
                                "aide_non_global", "aide_non_trop", "aide_non_autonomie")
variables_depenses_publiques <- c("depenses_confiant", "compris_depenses", "duree_depenses", "nombre_clics_depenses", "depense_totale", # budget_eq, regle_or, variations, dep_i_en_position
                                  "depense_sante", "depense_retraites", "depense_protection", "depense_education", "depense_recherche", "depense_loisirs", "depense_infrastructures", "depense_justice", "depense_armee", "depense_securite", "depense_aide", "recette_totale", 
                                  "en_position_0", "en_position_1", "en_position_2", "en_position_3", "en_position_4", "en_position_5", "en_position_6", "en_position_7", "en_position_8", "en_position_9", "en_position_10")
variables_toutes <- c(variables_approbation, variables_qualite, variables_aleatoires, variables_demo, variables_energie, variables_transport, variables_politiques, 
                      variables_gilets_jaunes, variables_gains_subjectifs, variables_Elasticite, variables_elasticite, variables_taxe_croyances, variables_benefices, 
                      variables_problemes, variables_taxe_condition, variables_politiques, "rattrapage_diesel", variables_connaissances_CC, variables_avis_CC, 
                      variables_comportement_CC, variables_schiste, variables_depenses_publiques)

variables_wo_missing <- variables_toutes
for (v in variables_toutes) { # display and remove variables with missing values
  # na_v <- length(which(is.na(s[[v]]) | is.nan(s[[v]]) | is.infinite(s[[v]])))
  if ("labelled" %in% class(s[[v]])) na_v <- length(which(is.na(s[[v]]))) #
  else na_v <- length(which(is.missing(s[[v]])))
  if (na_v>0) {
    print(paste(v, na_v))
    variables_wo_missing <- variables_wo_missing[variables_wo_missing!=v] }
}

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
selected_variables <- coefs_lasso@i - 1 # TODO: why is norme_vehicules present twice?
selected_variables <- selected_variables[selected_variables > 0 & selected_variables <= length(variables_wo_missing)]
selected_variables <- variables_wo_missing[selected_variables] # length(selected_variables) (.min) when alpha = 1: 58 / 0.5: 62 / 0: 142 (all) ; alpha = 1 and class: 67 / mae: 97
summary(glm(as.formula(paste("(taxe_approbation!='Non') ~", paste(selected_variables, collapse=' + '))), binomial, data=s, weights=s$weight))


significatifs_lasso_dev_oui <- as.formula((taxe_approbation=='Oui') ~ age + fume + niveau_vie + hausse_diesel + (transports_loisirs=="La voiture") + gilets_jaunes_oppose 
                               + perte_relative_partielle + taxe_perdant_pauvres + taxe_benefices_CC + taxe_benefices_aucun + taxe_problemes_aucun
                               + si_compensee + peages_urbains + ges_O2 + ges_avion + changer_si_tous)
summary(glm(significatifs_lasso, binomial, data=s, weights=s$weight))


##### OLS Approbation ####
summary(lm(as.formula(paste("(taxe_approbation!='Non') ~", paste(selected_variables, collapse=' + '))), data=s, weights=s$weight))
summary(lm(significatifs_lasso, data=s, weights=s$weight))
summary(lm(as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_big_regression, collapse=' + '))), data=s))


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
variables_big_regression <- selected_variables[!duplicated(selected_variables) & selected_variables!="ges_pm"] # TODO: why is there a bug with ges_pm?
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
labels_politiques_env <- c()
for (j in 141:148) labels_politiques_env <- c(labels_politiques_env, gsub(" - Q74", "", gsub(".*) à ", "", Label(s[[j]]))))  
labels_taxe_condition <- c()
for (j in 131:139) labels_taxe_condition <- c(labels_taxe_condition, gsub(" - .*", "", gsub(".*: ", "", Label(s[[j]])))) 
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

# TODO: perdants/gagnants + variante_monetaire
# oui_non(margin_l=430, variables_benefices[1:(length(variables_benefices)-2)], "barres_benefices", labels_benefices)
# oui_non(margin_l=430, variables_problemes[1:(length(variables_problemes)-2)], "barres_problemes", labels_problemes)

# TODO: créer variables avec nombre de bénéfices / problèmes cochés, voire l'influence des traitements

barres_benefices <- barres(file="benefices", title="<b>Bénéfices d'une taxe carbone compensée</b><br>(choix multiples)", data=matrix(values_benefices, ncol=length(values_benefices)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_benefices, hover=labels_benefices, legend="empty")
barres_problemes <- barres(file="problemes", title="<b>Problèmes d'une taxe carbone compensée</b><br>(choix multiples)", data=matrix(values_problemes, ncol=length(values_problemes)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_problemes, hover=labels_problemes, legend="empty")
barres_benefices
dev.print(png, '../images/benefice.png')
dev.copy(png, filename="../images/benefice.png") # TODO: manage expost image
dev.off()
barres_problemes
dev.print(png, '../images/probleme.png')
# orca(barres_problemes, "../images/problemes.png")
# orca(barres_benefices, "../images/benefices.png")


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

# TODO: créer variables avec nombre de bénéfices / problèmes cochés, voire l'influence des traitements

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
# TODO: perdants/gagnants + variante_monetaire
# oui_non(margin_l=430, variables_benefices[1:(length(variables_benefices)-2)], "barres_benefices", labels_benefices)
# oui_non(margin_l=430, variables_problemes[1:(length(variables_problemes)-2)], "barres_problemes", labels_problemes)

# TODO: créer variables avec nombre de bénéfices / problèmes cochés, voire l'influence des traitements
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

# TODO: créer variables avec nombre de bénéfices / problèmes cochés, voire l'influence des traitements

barres_gagnants_GJ_approuve <- barres(file="gagnants_GJ_approuve", title="Selon vous, quels seraient <b>les gagnants</b> avec une taxe carbone compensée ?<br><b>Réponses parmi les 36% de soutiens aux gilets jaunes</b>", margin_l = 200, data=matrix(values_gagnant_GJ_approuve, ncol=length(values_gagnant)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_gagnant, hover=labels_gagnant, legend="empty")
barres_gagnants_GJ_oppose <- barres(file="gagnants_GJ_oppose", title="Selon vous, quels seraient <b>les gagnants</b> avec une taxe carbone compensée ?<br><b>Réponses parmi les 25% d'opposants aux gilets jaunes</b>", margin_l = 200, data=matrix(values_gagnant_GJ_oppose, ncol=length(values_gagnant)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_gagnant, hover=labels_gagnant, legend="empty")
barres_perdants_GJ_approuve <- barres(file="perdants_GJ_approuve", title="Selon vous, quels seraient <b>les perdants</b> avec une taxe carbone compensée ?<br><b>Réponses parmi les 36% de soutiens aux gilets jaunes</b>", margin_l = 200, data=matrix(values_perdant_GJ_approuve, ncol=length(values_perdant)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_perdant, hover=labels_perdant, legend="empty")
barres_perdants_GJ_oppose <- barres(file="perdants_GJ_oppose", title="Selon vous, quels seraient <b>les perdants</b> avec une taxe carbone compensée ?<br><b>Réponses parmi les 25% d'opposants aux gilets jaunes</b>", margin_l = 200, data=matrix(values_perdant_GJ_oppose, ncol=length(values_perdant)), sort=T, color=c("brown"), showLegend=FALSE, labels=labels_perdant, hover=labels_perdant, legend="empty")
barres_gagnants_GJ_approuve
barres_gagnants_GJ_oppose
barres_perdants_GJ_approuve
barres_perdants_GJ_oppose
# TODO: combiner les deux graphes opposants/soutiens, pourcentages