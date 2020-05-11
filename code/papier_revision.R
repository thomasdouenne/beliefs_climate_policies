source("packages_functions.R")
load(".RData")

##### Survey #####
# Context of the study
decrit(s$gilets_jaunes >= 0, numbers=T, miss=T, weights=s$weight)
decrit(s$gilets_jaunes[s$centre==T] >= 0, numbers=T, miss=T, weights=s$weight[s$centre==T])


##### Self-interest #####
# TODO: rajouter des thresholds dans les contrôles revenus
# percentiles revenu fit: slightly lack of poor (around percentiles 0 and 25)
plot(1:length(s$percentile_revenu), 100*(1:length(s$percentile_revenu))/length(s$percentile_revenu), type='l', col='red')
lines(1:length(s$percentile_revenu), sort(s$percentile_revenu), type='l')

# (=> F.3) Simple Logit: 43 p.p.***
s$non_perdant <- as.numeric(s$gagnant_cible_categorie!='Perdant')
# Warning when weighting: it relates to number of trials and not to survey weights. Option weights = s$weight can be added in logistf, doesn't really change results
logit_si4 <- glm(formula_ols_si4, family = binomial(link='logit'), data=s)
summary(logit_si4) # Warning: Hauck-Donner effect, run logitsf, cf. https://prezi.com/di_n0_npv27n/hauck-donner-effect-and-instability-in-estimation-of-logisti/
logit_si4_margins <- logitmfx(formula_ols_si3, s, atmean=FALSE)$mfxest
logit_si4_margins # reason why mfx and not margins: https://stats.stackexchange.com/questions/409934/why-margins-and-mfx-yield-different-results-in-r/409937#409937
summary(logistf(formula_ols_si3, data=s, firth = T)) # Firth (93) regression to resolve separation => effect of 2.77*** instead of 2.85***, cf. Heinze & Ploner (03)
# other way to check significance is to run a Likelihood Ratio test instead of the Wald/Chi-squared test reported in z value/Pr(>|z|) in glm, by running anova:
# https://stat.ethz.ch/pipermail/r-help/2001-October/015779.html http://nross626.math.yorku.ca/math4330/R/Regression/Hauck_Donner_effect.pdf
anova(logit_si4, test='LR') # <2e-16 ***


# # (1a revenu percentiles) Main identification strategy. We had a better fit without percentiles.
# tsls1_si1 <- lm(gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + 
#                   I(traite_cible*traite_cible_conjoint) + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single, data=s, weights = s$weight)
# summary(tsls1_si1)
# s$non_perdant <- tsls1_si1$fitted.values
# # 55 p.p.***
# tsls2_si1 <- lm(taxe_cible_approbation!='Non' ~ non_perdant + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single, data=s, weights = s$weight)
# summary(tsls2_si1)
# # Effective F-stat from Stata weakivtest: 35
# 
# iv_si1 <- ivreg(taxe_cible_approbation!='Non' ~ (gagnant_cible_categorie!='Perdant') + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single | 
#     traite_cible + traite_cible_conjoint + I(traite_cible*traite_cible_conjoint) + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single, 
#     data = s, weights = s$weight)
# summary(iv_si1, diagnostics = TRUE)
# 
# # (1b p10-60) Main identification strategy
# tsls1_si1 <- lm(gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + 
#                   I(traite_cible*traite_cible_conjoint) + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + single, data=s, 
#                 subset = percentile_revenu <= 60 & percentile_revenu >= 10, weights = s$weight)
# summary(tsls1_si1)
# s$non_perdant[s$percentile_revenu <= 60 & s$percentile_revenu >= 10] <- tsls1_si1$fitted.values
# # 68 p.p.***
# tsls2_si1 <- lm(taxe_cible_approbation!='Non' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + single, data=s, 
#                 subset = percentile_revenu <= 60 & percentile_revenu >= 10, weights = s$weight)
# summary(tsls2_si1)
# 
# (1c revenu percentiles & revenu in 10-60) Main identification strategy
# tsls1_si1 <- lm(gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint +
#                   I(traite_cible*traite_cible_conjoint) + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single, data=s,
#                 subset = percentile_revenu <= 60 & percentile_revenu >= 10, weights = s$weight)
# summary(tsls1_si1)
# s$non_perdant[s$percentile_revenu <= 60 & s$percentile_revenu >= 10] <- tsls1_si1$fitted.values
# # 69 p.p.***
# tsls2_si1 <- lm(taxe_cible_approbation!='Non' ~ non_perdant + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single, data=s,
#                 subset = percentile_revenu <= 60 & percentile_revenu >= 10, weights = s$weight)
# summary(tsls2_si1)
# 
# iv_si1 <- ivreg(taxe_cible_approbation!='Non' ~ (gagnant_cible_categorie!='Perdant') + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single |
#     traite_cible + traite_cible_conjoint + I(traite_cible*traite_cible_conjoint) + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single,
#     data = s, subset = percentile_revenu <= 60 & percentile_revenu >= 10, weights = s$weight)
# summary(iv_si1, diagnostics = TRUE)
# # Effective F-stat from Stata weakivtest: 18


# # (1f) NOT
# tsls1_si1 <- lm(gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + percentile_revenu__20 + percentile_revenu_70_ + percentile_revenu_conjoint_70_ + single +
#      (s$percentile_revenu < 20) + (s$percentile_revenu > 70) + (s$percentile_revenu_conjoint < 20) + (s$percentile_revenu_conjoint > 70) +
#       percentile_revenu_conjoint__20 + I(traite_cible*traite_cible_conjoint) + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2, data=s, weights = s$weight)
# summary(tsls1_si1)
# s$non_perdant <- tsls1_si1$fitted.values
# # 50 p.p.***
# tsls2_si1 <- lm(taxe_cible_approbation!='Non' ~ non_perdant + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 +
#                   (s$percentile_revenu < 20) + (s$percentile_revenu > 70) + (s$percentile_revenu_conjoint < 20) + (s$percentile_revenu_conjoint > 70) +
#                 percentile_revenu__20 + percentile_revenu_70_ + percentile_revenu_conjoint__20 + percentile_revenu_conjoint_70_ + single, data=s, weights = s$weight)
# summary(tsls2_si1)
# iv_si1 <- ivreg(taxe_cible_approbation!='Non' ~ (gagnant_cible_categorie!='Perdant') + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single
#   + (s$percentile_revenu < 20) + (s$percentile_revenu > 70) + (s$percentile_revenu_conjoint < 20) + (s$percentile_revenu_conjoint > 70) + + percentile_revenu__20 + 
#     percentile_revenu_70_ + percentile_revenu_conjoint__20 + percentile_revenu_conjoint_70_ | traite_cible + traite_cible_conjoint + 
#     I(traite_cible*traite_cible_conjoint) + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single + 
#     (s$percentile_revenu < 20) + (s$percentile_revenu > 70) + (s$percentile_revenu_conjoint < 20) + (s$percentile_revenu_conjoint > 70) +
#     percentile_revenu__20 + percentile_revenu_70_ + percentile_revenu_conjoint__20 + percentile_revenu_conjoint_70_ , data = s, weights = s$weight)
# summary(iv_si1, diagnostics = TRUE)

# seuil_1 <- 20
# seuil_2 <- NA # 60
# seuil_3 <- 70
# s$percentile_revenu__1 <- (s$percentile_revenu - seuil_1)*(s$percentile_revenu <= seuil_1)
# s$percentile_revenu_2_3 <- (s$percentile_revenu - seuil_2)*((s$percentile_revenu <= seuil_3) & (s$percentile_revenu > seuil_2))
# s$percentile_revenu_3_ <- (s$percentile_revenu - seuil_3)*(s$percentile_revenu > seuil_3)
# s$percentile_revenu_conjoint__1 <- (s$percentile_revenu_conjoint - seuil_1)*(s$percentile_revenu_conjoint <= seuil_1) # TODO: colinéarité
# s$percentile_revenu_conjoint_2_3 <- (s$percentile_revenu_conjoint - seuil_2)*((s$percentile_revenu_conjoint <= seuil_3) & (s$percentile_revenu_conjoint > seuil_2))
# s$percentile_revenu_conjoint_3_ <- (s$percentile_revenu_conjoint - seuil_3)*(s$percentile_revenu_conjoint > seuil_3)
# 
# # s$percentile_revenu__1 <- (s$percentile_revenu - seuil_1)*(s$percentile_revenu <= seuil_1)
# # s$percentile_revenu_2_3 <- (s$percentile_revenu - seuil_2)*((s$percentile_revenu <= seuil_3) & (s$percentile_revenu > seuil_2))
# # s$percentile_revenu_3_ <- (s$percentile_revenu - seuil_3)*(s$percentile_revenu > seuil_3)
# # s$percentile_revenu_conjoint__1 <- (s$percentile_revenu_conjoint - seuil_1)*(s$percentile_revenu_conjoint <= seuil_1) # TODO: colinéarité
# # s$percentile_revenu_conjoint_2_3 <- (s$percentile_revenu_conjoint - seuil_2)*((s$percentile_revenu_conjoint <= seuil_3) & (s$percentile_revenu_conjoint > seuil_2))
# # s$percentile_revenu_conjoint_3_ <- (s$percentile_revenu_conjoint - seuil_3)*(s$percentile_revenu_conjoint > seuil_3)
# variables_revenus_main <- c("percentile_revenu", "percentile_revenu_conjoint", "Revenu2", "Revenu_conjoint2")
# variables_revenus_all <- c("Revenu2", "Revenu_conjoint2", "I(percentile_revenu <= 20)", "I(percentile_revenu > 70)", "I(percentile_revenu_conjoint <= 20)", "I(percentile_revenu_conjoint > 70)",
#   "percentile_revenu__20", "percentile_revenu_70_", "percentile_revenu_20_70", "percentile_revenu_conjoint__20", "percentile_revenu_conjoint_70_", "percentile_revenu_conjoint_20_70")
# # continuous piecewise linear (commented version: piecewise linear)
# variables_revenus_all <- c(variables_revenus_main, "percentile_revenu__1", "percentile_revenu_2_3", "percentile_revenu_3_", "percentile_revenu_conjoint__1",
#                        "percentile_revenu_conjoint_2_3", "percentile_revenu_conjoint_3_")
# for (v in length(variables_revenus_all):1) if (all(is.na(s[[variables_revenus_all[v]]]))) variables_revenus_all <- variables_revenus_all[-v]
# variables_reg_self_interest <- c("prog_na", "Simule_gain", "Simule_gain2", "taxe_efficace", "single",  "hausse_depenses_par_uc", variables_revenus_all, variables_demo) 
variables_reg_self_interest <- c("prog_na", "Simule_gain", "Simule_gain2", "taxe_efficace", "single",  "hausse_depenses_par_uc", variables_demo, piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), c(20,70), vector=T)) 
variables_reg_self_interest <- variables_reg_self_interest[!(variables_reg_self_interest %in% c("revenu", "rev_tot", "age", "age_65_plus"))]

# (1) With many controls 
formula_tsls1_si1 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible*traite_cible_conjoint + cible + I(taxe_approbation=='NSP') + tax_acceptance + ", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si1 <- lm(formula_tsls1_si1, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls1_si1)
s$non_perdant[(s$percentile_revenu <= 60 & s$percentile_revenu >= 10) | (s$percentile_revenu_conjoint <= 60 & s$percentile_revenu_conjoint >= 10)] <- tsls1_si1$fitted.values
# 53 p.p.***
formula_tsls2_si1 <- as.formula(paste("taxe_cible_approbation!='Non' ~ ", 
                                      paste(variables_reg_self_interest, collapse = ' + '), "+ cible + I(taxe_approbation=='NSP') + tax_acceptance + non_perdant")) # 
tsls2_si1 <- lm(formula_tsls2_si1, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls2_si1) 

iv_si1 <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", paste(variables_reg_self_interest, collapse=' + '), 
        " + cible + I(taxe_approbation=='NSP') + tax_acceptance + (gagnant_cible_categorie!='Perdant') | . - (gagnant_cible_categorie!='Perdant') + traite_cible*traite_cible_conjoint")), 
        data = s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight), diagnostics = TRUE)
iv_si1 # Effective F-stat from Stata weakivtest: 16

  # # Alternative specifications for robustness checks
# # (2 revenu percentiles and revenu ou revenu conjoint in 10-60) 
# formula_tsls1_si2 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ ", piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), c(20,70)), " + single + cible + traite_cible*traite_cible_conjoint"))
# tsls1_si2 <- lm(formula_tsls1_si2, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
# summary(tsls1_si2)
# s$non_perdant[(s$percentile_revenu <= 60 & s$percentile_revenu >= 10) | (s$percentile_revenu_conjoint <= 60 & s$percentile_revenu_conjoint >= 10)] <- tsls1_si2$fitted.values
# # 59 p.p.*** (without piecewise linear: 64 p.p.*** (10-70: 60 p.p. / 0-60: 56 p.p. / 0-70: 55 p.p. comme avant): lower acceptance rate in 0-10 (and 60-70)?)
# formula_tsls2_si2 <- as.formula(paste("taxe_cible_approbation!='Non' ~ ", piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), c(20,70)), " + single + cible + non_perdant"))
# tsls2_si2 <- lm(formula_tsls2_si2, data=s,
#                 subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
# summary(tsls2_si2)
# 
# iv_si2 <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), c(20,70)), " + single + cible + (gagnant_cible_categorie!='Perdant') | ",
#                                          piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), c(20,70)), " + single + cible + traite_cible*traite_cible_conjoint")),
#     data = s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight), diagnostics = TRUE)
# iv_si2 # Effective F-stat from Stata weakivtest: 17
# 
# explication de l'effet en hausse: par rapport au chiffre attendu, les p0-10 acceptent moins et les p60-70 acceptent plus la réforme,
#    même quand ils se disent resp. gagnant / perdant, atténuant ainsi l'effet quand on les inlcut
summary(lm(taxe_cible_approbation!='Non' ~ percentile_revenu + percentile_revenu_conjoint + Revenu2 + Revenu_conjoint2 + single +
             I(percentile_revenu < 10) + I(percentile_revenu_conjoint < 10) + I(percentile_revenu > 60) + I(percentile_revenu_conjoint > 60), data=s))
summary(lm(taxe_cible_approbation!='Non' ~ percentile_revenu + percentile_revenu_conjoint + Revenu2 + Revenu_conjoint2 + single +
             I(percentile_revenu < 10 | percentile_revenu_conjoint < 10)*traite_cible*traite_cible_conjoint + I(percentile_revenu > 60 | percentile_revenu_conjoint > 60)*traite_cible*traite_cible_conjoint, data=s))
summary(lm(taxe_cible_approbation!='Non' ~ percentile_revenu + percentile_revenu_conjoint + Revenu2 + Revenu_conjoint2 + single +
             I(percentile_revenu < 10 | percentile_revenu_conjoint < 10)*(gagnant_cible_categorie!='Perdant') + I(percentile_revenu > 60 | percentile_revenu_conjoint > 60)*(gagnant_cible_categorie=='Perdant'), data=s))
summary(lm(taxe_cible_approbation!='Non' ~ percentile_revenu + percentile_revenu_conjoint + Revenu2 + Revenu_conjoint2 + single + 
             I(percentile_revenu > 60) + I(percentile_revenu_conjoint > 60), data=s, subset = gagnant_cible_categorie=='Perdant'))
summary(lm(taxe_cible_approbation!='Non' ~ percentile_revenu + percentile_revenu_conjoint + Revenu2 + Revenu_conjoint2 + single +
             I(percentile_revenu < 10) + I(percentile_revenu_conjoint < 10), data=s, subset = gagnant_cible_categorie!='Perdant'))
summary(lm(taxe_cible_approbation!='Non' ~ percentile_revenu + percentile_revenu_conjoint + Revenu2 + Revenu_conjoint2 + single + 
             I(percentile_revenu > 60 | percentile_revenu_conjoint > 60), data=s, subset = gagnant_cible_categorie=='Perdant'))
summary(lm(taxe_cible_approbation!='Non' ~ percentile_revenu + percentile_revenu_conjoint + Revenu2 + Revenu_conjoint2 + single +
             I(percentile_revenu < 10 | percentile_revenu_conjoint < 10), data=s, subset = gagnant_cible_categorie!='Perdant'))
summary(lm(taxe_approbation!='Non' ~ percentile_revenu + percentile_revenu_conjoint + Revenu2 + Revenu_conjoint2 + single + 
             I(percentile_revenu > 60) + I(percentile_revenu_conjoint > 60), data=s, subset = gagnant_categorie=='Perdant'))
summary(lm(taxe_approbation!='Non' ~ percentile_revenu + percentile_revenu_conjoint + Revenu2 + Revenu_conjoint2 + single +
             I(percentile_revenu < 10) + I(percentile_revenu_conjoint < 10), data=s, subset = gagnant_categorie!='Perdant'))

# (3 revenu percentiles and revenu non linéaire) Main identification strategy (ma préférée parce que dans les précédentes on regarde l'effet sous un sous-échantillon)
formula_tsls1_si3 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ ", piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), c(20,70)), 
                                      " + single + cible + I(taxe_approbation=='NSP') + tax_acceptance + traite_cible*traite_cible_conjoint"))
tsls1_si3 <- lm(formula_tsls1_si3, data=s, weights = s$weight)
summary(tsls1_si3)
s$non_perdant <- tsls1_si3$fitted.values
# 50 p.p.***
formula_tsls2_si3 <- as.formula(paste("taxe_cible_approbation!='Non' ~ ", piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), c(20,70)), 
                                      " + I(taxe_approbation=='NSP') + tax_acceptance + single + cible + non_perdant"))
tsls2_si3 <- lm(formula_tsls2_si3, data=s, weights = s$weight)
summary(tsls2_si3)

iv_si3 <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), c(20,70)), 
          " + I(taxe_approbation=='NSP') + tax_acceptance + single + cible + (gagnant_cible_categorie!='Perdant') | . - (gagnant_cible_categorie!='Perdant') + traite_cible*traite_cible_conjoint")), data = s, weights = s$weight), diagnostics = TRUE)
iv_si3 # Effective F-stat from Stata weakivtest: 28
# iv_si1$diagnostics[1,3]

# (4) Simple OLS: 44 p.p. ***
# # p10-p60
# formula_ols_si4  <- as.formula(paste("taxe_cible_approbation!='Non' ~ cible + I(taxe_approbation=='NSP') + ", paste(variables_reg_self_interest, collapse = ' + '), " + non_perdant + tax_acceptance")) # 
# s$non_perdant <- as.numeric(s$gagnant_cible_categorie!='Perdant')
# ols_si4 <- lm(formula_ols_si4, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
# summary(ols_si4)

formula_ols_si4  <- as.formula(paste("taxe_cible_approbation!='Non' ~ cible + I(taxe_approbation=='NSP') + ", paste(variables_reg_self_interest, collapse = ' + '), " + non_perdant + tax_acceptance")) # 
s$non_perdant <- as.numeric(s$gagnant_cible_categorie!='Perdant')
ols_si4 <- lm(formula_ols_si4, data=s, weights = s$weight)
summary(ols_si4)

# # (6) IV Feedback with controls
# formula_tsls1_si6 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + ", 
#                                       paste(variables_reg_self_interest, collapse = ' + ')))
# tsls1_si6 <- lm(formula_tsls1_si6, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
# summary(tsls1_si6)
# s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_si6$fitted.values
# # 44 p.p. ***
# formula_tsls2_si6 <- as.formula(paste("taxe_feedback_approbation!='Non' ~ (taxe_approbation=='NSP') + ", 
#                                       paste(variables_reg_self_interest, collapse = ' + '), " + non_perdant + tax_acceptance"))
# tsls2_si6 <- lm(formula_tsls2_si6, data=s, subset=variante_taxe_info=='f', weights = s$weight) 
# summary(tsls2_si6)
# 
# iv_si6 <- summary(ivreg(as.formula(paste("taxe_feedback_approbation!='Non' ~ tax_acceptance + I(taxe_approbation=='NSP') + ", paste(variables_reg_self_interest, collapse = ' + '),
#       " + (gagnant_feedback_categorie!='Perdant') | . - (gagnant_feedback_categorie!='Perdant') + simule_gagnant")), data = s, subset=variante_taxe_info=='f', weights = s$weight), diagnostics = TRUE)
# iv_si6 # Effective F-stat from Stata weakivtest: 35

# (5) IV Feedback with controls restricted to |simule_gain| < 50
formula_tsls1_si5 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + ", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si5 <- lm(formula_tsls1_si5, data=s, subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight, na.action='na.exclude')
summary(tsls1_si5)
s$non_perdant[s$variante_taxe_info=='f' & abs(s$simule_gain) < 50] <- tsls1_si5$fitted.values
# 64 p.p. ***
formula_tsls2_si5 <- as.formula(paste("taxe_feedback_approbation!='Non' ~ (taxe_approbation=='NSP') + ", 
                                      paste(variables_reg_self_interest, collapse = ' + '), " + non_perdant + tax_acceptance"))
tsls2_si5 <- lm(formula_tsls2_si5, data=s, subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight) 
summary(tsls2_si5)

iv_si5 <- summary(ivreg(as.formula(paste("taxe_feedback_approbation!='Non' ~ tax_acceptance + I(taxe_approbation=='NSP') + ", paste(variables_reg_self_interest, collapse = ' + '),
      " + (gagnant_feedback_categorie!='Perdant') | . - (gagnant_feedback_categorie!='Perdant') + simule_gagnant")), data = s, 
      subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight), diagnostics = TRUE)
iv_si5 # Effective F-stat from Stata weakivtest: 21

# # (6) IV Feedback
# tsls1_si5 <- lm(gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + Simule_gain + Simule_gain2, data=s, subset=variante_taxe_info=='f', weights = s$weight)
# summary(tsls1_si5)
# s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_si5$fitted.values
# # 52 p.p.***
# tsls2_si5 <- lm(taxe_feedback_approbation!='Non' ~ Simule_gain + Simule_gain2 + non_perdant, data=s, subset=variante_taxe_info=='f', weights = s$weight)
# summary(tsls2_si5)
# 
# iv_si5 <- summary(ivreg(taxe_feedback_approbation!='Non' ~ Simule_gain + Simule_gain2 + (gagnant_feedback_categorie!='Perdant') | Simule_gain + Simule_gain2 + simule_gagnant, 
#                         data = s, weights = s$weight), diagnostics = TRUE)
# iv_si5 # Effective F-stat from Stata weakivtest: 36

# # (6) IV Feedback restricted to |simule_gain| < 50
# tsls1_si6 <- lm(gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + Simule_gain + Simule_gain2, data=s, subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight)
# summary(tsls1_si6)
# s$non_perdant[s$variante_taxe_info=='f' & abs(s$simule_gain) < 50] <- tsls1_si6$fitted.values
# # 53 p.p.***
# tsls2_si6 <- lm(taxe_feedback_approbation!='Non' ~ Simule_gain + Simule_gain2 + non_perdant, data=s, subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight)
# summary(tsls2_si6)
# 
# iv_si6 <- summary(ivreg(taxe_feedback_approbation!='Non' ~ Simule_gain + Simule_gain2 + (gagnant_feedback_categorie!='Perdant') | Simule_gain + Simule_gain2 + simule_gagnant, 
#                         data = s, subset = variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight), diagnostics = TRUE)
# iv_si6 # Effective F-stat from Stata weakivtest: 18

f_stats_si <- sprintf("%.1f", round(c(iv_si1$diagnostics[1,3], iv_si3$diagnostics[1,3], iv_si5$diagnostics[1,3]), 1))

Table_si2 <- stargazer(tsls2_si1, tsls2_si3, ols_si3, tsls2_si5, 
                    title="Effect of self-interest on acceptance", star.cutoffs = NA, column.labels = c("\\textit{IV: random target/eligibility}", "$OLS$", "\\textit{IV: discontinuity in feedback}"), column.separate = c(2,1,1),
                    dep.var.labels = c("Targeted Dividend ($A^T$)", "After Feedback ($A^F$)"), dep.var.caption = "Acceptance (``Yes'' or ``Don't know'' to policy support)", header = FALSE,
                    covariate.labels = c("Believes does not lose ($G$)", "Initial tax Acceptance ($A^0$)", "",  "Environmentally effective: ``Yes''"),
                    keep = c("non_perdant", "tax_acceptance"), order = c("non_perdant", "tax_acceptance"), omit.table.layout = 'n', 
                    add.lines = list(
                      # "Method: 2SLS & \\checkmark & \\checkmark &  & \\checkmark",
                      c("Controls: Incomes (piecewise continuous)", "\\checkmark ", "\\checkmark  ", "\\checkmark ", "\\checkmark"), # TODO: non-parametric incomes in (2)?
                      c("\\quad estimated gain, socio-demo, other motives ", "", "", "", ""),
                      # c("Controls: Estimated gain ", "\\checkmark", "", "\\checkmark", "\\checkmark"),
                      c("Controls: Policy assigned", "\\checkmark ", "\\checkmark ", "\\checkmark  ", ""),
                      c("Sub-sample", "[p10; p60]", "", "", "$\\left| \\widehat{\\gamma}\\right|<50$"),
                      c("Effective F-Statistic", f_stats_si[1:2], "", f_stats_si[3])),
                    no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="results_private_benefits")
write_clip(gsub('\\end{table}', '} {\\footnotesize \\parbox[t]{\\textwidth}{\\linespread{1.2}\\selectfont \\textsc{Note:} Standard errors are reported in parentheses. The list of controls can be found in Appendix \\ref{set_controls}. The first-stages for the targeted dividend use as source of exogenous variation in the belief the random assignment of the income threshold that determines eligibility to the dividend. The first-stage for the non-targeted dividend exploits instead the discontinuity in the win/lose feedback when the net gain switches from negative to positive.} }\\end{table}', 
                    gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_si2, fixed=TRUE), fixed=T), collapse=' ')

Table_si1 <- stargazer(tsls1_si1, tsls1_si3, tsls1_si5, 
                    title="First stage regressions results for self-interest", omit.table.layout = 'n', star.cutoffs = NA,
                    covariate.labels = c("Transfer to respondent ($T_1$)", "Transfer to spouse ($T_2$)",
                                         "$T_1 \\times T_2$", "Simulated winner ($\\widehat{\\Gamma}$)", "Initial tax Acceptance ($A^0$)"),
                    dep.var.labels = c("Targeted Dividend ($G^T$)", "After feedback ($G^F$)"), dep.var.caption = "Believes does not lose", header = FALSE,
                    column.labels = c("(1)", "(2)", "(4)"), model.numbers = FALSE,
                    keep = c("traite", "simule_gagnant", "acceptance"), order = c("traite", "simule_gagnant", "acceptance"),
                    add.lines = list(c("Controls: Incomes (piecewise continuous)", " \\checkmark", " \\checkmark", "\\checkmark"),
                                       c("\\quad estimated gain, socio-demo, other motives ", "", "", ""),
                                  # c("Controls: Estimated gain", "", "", " \\checkmark ", " \\checkmark", " \\checkmark"),
                                  c("Controls: Policy assigned", " \\checkmark", " \\checkmark", " "),
                                  # c("Controls: Socio-demo, other motives", "", "", " \\checkmark", " ", " \\checkmark"),
                                  c("Sub-sample", "[p10; p60]", "", "$\\left| \\widehat{\\gamma}\\right|<50$"),
                                  c("Effective F-Statistic", f_stats_si)),
                    no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="first_stage_private_benefits")
write_clip(gsub('\\end{table}', '} {\\footnotesize \\parbox[t]{\\textwidth}{\\linespread{1.2}\\selectfont \\textsc{Note:} In (1,2), the random (conditionally on income) eligibility to the dividend is used as source of exogenous variation in the belief. In (4), the discontinuity in the win/lose feedback when the net gain switches from negative to positive is used. See second stage results, Table \\vref{results_private_benefits}. }} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', 
                                                       Table_si1, fixed=TRUE), fixed=TRUE), collapse=' ')


##### Heterogeneity in LATE (R2) #### 
## By subsetting: same effect for income, but higher effect when simule_gain < 0
# summary(ivreg(taxe_cible_approbation!='Non' ~ (gagnant_cible_categorie!='Perdant') + cible + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single
#   + percentile_revenu__20 + percentile_revenu_70_ + percentile_revenu_conjoint__20 + percentile_revenu_conjoint_70_ | traite_cible + traite_cible_conjoint + 
#     I(traite_cible*traite_cible_conjoint) + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single + 
#     percentile_revenu__20 + percentile_revenu_70_ + percentile_revenu_conjoint__20 + percentile_revenu_conjoint_70_ , data = s, weights = s$weight, subset = s$percentile_revenu > 45))
# 
# summary(ivreg(taxe_cible_approbation!='Non' ~ (gagnant_cible_categorie!='Perdant') + cible + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single
#   + percentile_revenu__20 + percentile_revenu_70_ + percentile_revenu_conjoint__20 + percentile_revenu_conjoint_70_ | traite_cible + traite_cible_conjoint + 
#     I(traite_cible*traite_cible_conjoint) + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single + 
#     percentile_revenu__20 + percentile_revenu_70_ + percentile_revenu_conjoint__20 + percentile_revenu_conjoint_70_ , data = s, weights = s$weight, subset = s$percentile_revenu < 45))
# 
# summary(ivreg(taxe_cible_approbation!='Non' ~ (gagnant_cible_categorie!='Perdant') + cible + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single
#   + percentile_revenu__20 + percentile_revenu_70_ + percentile_revenu_conjoint__20 + percentile_revenu_conjoint_70_ | traite_cible + traite_cible_conjoint + 
#     I(traite_cible*traite_cible_conjoint) + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single + 
#     percentile_revenu__20 + percentile_revenu_70_ + percentile_revenu_conjoint__20 + percentile_revenu_conjoint_70_ , data = s, weights = s$weight, subset = s$Simule_gain > 0))
# 
# summary(ivreg(taxe_cible_approbation!='Non' ~ (gagnant_cible_categorie!='Perdant') + cible + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single
#   + percentile_revenu__20 + percentile_revenu_70_ + percentile_revenu_conjoint__20 + percentile_revenu_conjoint_70_ | traite_cible + traite_cible_conjoint + 
#     I(traite_cible*traite_cible_conjoint) + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single + 
#     percentile_revenu__20 + percentile_revenu_70_ + percentile_revenu_conjoint__20 + percentile_revenu_conjoint_70_ , data = s, weights = s$weight, subset = s$Simule_gain < 0))

# NONSENSE: among simule_gain > 0 there is no variability in simule_gagnant (only because feedback is not displayed for all)
# Why first stage doesn't throw an error: because in Qualtrics number of recipients of the 110€ was not limited to 2. TODO
# length(which(s$simule_gagnant!=1*(s$simule_gain>0)))
# decrit(s$nb_adultes[s$simule_gagnant!=1*(s$simule_gain>0)])
# # no effect for simule_gain > 0 ! meaning the effect is driven by those who discover they lose (in bottom half of incomes)
# #   (for the feedback: the fact that some who wrongly believed they lost turn to accept when they are targeted shows there is an effect also for incredulous winners)
# summary(ivreg(taxe_feedback_approbation!='Non' ~ (gagnant_feedback_categorie!='Perdant') + Simule_gain + Simule_gain2 | 
#                 simule_gagnant + Simule_gain + Simule_gain2, data = s, weights = s$weight, subset = s$Simule_gain > 0))
# 
# summary(ivreg(taxe_feedback_approbation!='Non' ~ (gagnant_feedback_categorie!='Perdant') + Simule_gain + Simule_gain2 | 
#                 simule_gagnant + Simule_gain + Simule_gain2, data = s, weights = s$weight, subset = s$Simule_gain < 0))
# 
# length(which(s$gagnant_categorie=='Perdant' & s$gagnant_cible_categorie=='Gagnant' & s$taxe_approbation=='Non' & s$taxe_cible_approbation=='Oui' & s$simule_gain > 0)) # 62
# length(which(s$gagnant_categorie=='Perdant' & s$gagnant_cible_categorie!='Perdant' & s$taxe_approbation=='Non' & s$taxe_cible_approbation!='Non' & s$simule_gain > 0)) # 140
# length(which(s$gagnant_categorie=='Perdant' & (s$traite_cible + s$traite_cible_conjoint > 0) & s$taxe_approbation=='Non' & s$simule_gain > 0)) # 772

# # Rich update as much (1st stage) but this has less effect on their acceptance
# summary(ivreg(taxe_feedback_approbation!='Non' ~ (gagnant_feedback_categorie!='Perdant') + Simule_gain + Simule_gain2 | 
#                 simule_gagnant + Simule_gain + Simule_gain2, data = s, weights = s$weight, subset = s$percentile_revenu < 50)) # higher effect here
# summary(lm((gagnant_feedback_categorie!='Perdant') ~ simule_gagnant + Simule_gain + Simule_gain2, data = s, weights = s$weight, subset = s$percentile_revenu < 50))
# 
# summary(ivreg(taxe_feedback_approbation!='Non' ~ (gagnant_feedback_categorie!='Perdant') + Simule_gain + Simule_gain2 | 
#                 simule_gagnant + Simule_gain + Simule_gain2, data = s, weights = s$weight, subset = s$percentile_revenu > 50)) # than here
# summary(lm((gagnant_feedback_categorie!='Perdant') ~ simule_gagnant + Simule_gain + Simule_gain2, data = s, weights = s$weight, subset = s$percentile_revenu > 50))

# # no effect for simule_gain < 0 ! Here the mechanism is different: it's motivated reasoning => F stat too low to look at heterogeneity?
# summary(ivreg(as.formula(paste("taxe_approbation!='Non' ~ (taxe_efficace!='Non') | apres_modifs * info_CC")), data = s, subset = s$Simule_gain < 0))
# summary(ivreg(as.formula(paste("taxe_approbation!='Non' ~ (taxe_efficace!='Non') | apres_modifs * info_CC")), data = s, subset = s$Simule_gain > 0))
# summary(lm((taxe_efficace!='Non') ~ apres_modifs * info_CC, data = s, subset = s$Simule_gain < 0))
# summary(lm((taxe_efficace!='Non') ~ apres_modifs * info_CC, data = s, subset = s$Simule_gain > 0))
# summary(ivreg(as.formula(paste("taxe_approbation!='Non' ~ (taxe_efficace!='Non') | apres_modifs * info_CC")), data = s, subset = s$percentile_revenu < 50)) # higher effect here
# summary(ivreg(as.formula(paste("taxe_approbation!='Non' ~ (taxe_efficace!='Non') | apres_modifs * info_CC")), data = s, subset = s$percentile_revenu > 50)) # than here


# ## By interacting
# summary(ivreg(taxe_cible_approbation!='Non' ~ (gagnant_cible_categorie!='Perdant')*percentile_revenu + cible + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single
#   + percentile_revenu__20 + percentile_revenu_70_ + percentile_revenu_conjoint__20 + percentile_revenu_conjoint_70_ | traite_cible + traite_cible_conjoint + 
#     I(traite_cible*traite_cible_conjoint) + cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single + 
#     percentile_revenu__20 + percentile_revenu_70_ + percentile_revenu_conjoint__20 + percentile_revenu_conjoint_70_ , data = s, weights = s$weight))

# TODO: check autres seuils et en faire une table en appendix
summary(ivreg(taxe_cible_approbation!='Non' ~ ((gagnant_cible_categorie!='Perdant') & (percentile_revenu > 45)) + (gagnant_cible_categorie!='Perdant') + percentile_revenu + cible + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single
  + percentile_revenu__1 + percentile_revenu_3_ + percentile_revenu_conjoint__1 + percentile_revenu_conjoint_3_ | traite_cible + traite_cible_conjoint + 
    I(traite_cible*traite_cible_conjoint) + cible + (percentile_revenu > 45) + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single + 
    percentile_revenu__1 + percentile_revenu_3_ + percentile_revenu_conjoint__1 + percentile_revenu_conjoint_3_ , data = s, weights = s$weight))

# # Here we find heterogenous effect:
# summary(ivreg(taxe_cible_approbation!='Non' ~ (gagnant_cible_categorie!='Perdant') * Simule_gain + percentile_revenu + cible + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single
#   + percentile_revenu__20 + percentile_revenu_70_ + percentile_revenu_conjoint__20 + percentile_revenu_conjoint_70_ | traite_cible * traite_cible_conjoint * Simule_gain + 
#     cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single + 
#     percentile_revenu__20 + percentile_revenu_70_ + percentile_revenu_conjoint__20 + percentile_revenu_conjoint_70_ , data = s, weights = s$weight))

# # And here we are close:
# summary(ivreg(taxe_feedback_approbation!='Non' ~ (gagnant_feedback_categorie!='Perdant') * Simule_gain + Simule_gain2 | 
#                 simule_gagnant * Simule_gain + Simule_gain2, data = s, weights = s$weight))
# 
# summary(ivreg(taxe_feedback_approbation!='Non' ~ (gagnant_feedback_categorie!='Perdant') * percentile_revenu + Simule_gain + Simule_gain2 | 
#                 simule_gagnant * percentile_revenu + Simule_gain + Simule_gain2, data = s, weights = s$weight))
# 
# summary(ivreg(as.formula(paste("taxe_approbation!='Non' ~ (taxe_efficace!='Non') * Simule_gain | apres_modifs * info_CC * Simule_gain")), data = s), diagnostics = TRUE)


##### LIML #####
# 1 Yes2, 2 Yes2 OLS, 3 No ~ Yes LIML 
# app: 1 Yes2 logit, 2 No ~ Yes, 3 No ~ Yes OLS, 4 No ~ No, 5 No ~ No OLS
# Spec Yes ~ Yes as main, then present result of CLR for old main (coef consistent but not identified, biased towards OLS)
# To reviewer: result LIML, justification that we use CLR, and aknowledgment our specification was not well identified
# Stata's ivreg2 needed for LIML: ivmodelFormula doesn't work with controls
# ivreg2's result: 2.5 without control / 1.1 with control; none is significant
ivmodel(Y=as.numeric(s$tax_acceptance), D=1*(s$taxe_efficace!='Non'), Z=s$info_CC)
LIML(ivmodel(Y=as.numeric(s$tax_acceptance), D=1*(s$taxe_efficace!='Non'), Z=s[,c('apres_modifs', 'info_CC')], X=s[,variables_reg_ee]))
ivmodelFormula(as.formula(paste("tax_acceptance ~ (taxe_efficace!='Non') |  apres_modifs + info_CC")), data = s)
ivmodelFormula(as.formula(paste("tax_acceptance ~ (taxe_efficace!='Non') |  apres_modifs + info_CC")), data = s, subset = s$Simule_gain > 0)
ivmodelFormula(as.formula(paste("taxe_approbation=='Oui' ~ (taxe_efficace=='Oui') |  apres_modifs + info_CC")), data = s)
ivmodelFormula(as.formula(paste("taxe_approbation=='Oui' ~ (taxe_efficace!='Non') |  apres_modifs + info_CC")), data = s)
ivmodelFormula(as.formula(paste("tax_acceptance ~ ", paste(variables_reg_ee, collapse = ' + '), 
       "+ (taxe_efficace!='Non') | ", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC")), data = s)

summary(ivreg(as.formula(paste("taxe_approbation!='Non' ~ ", paste(variables_reg_ee, collapse = ' + '), 
       "+ (taxe_efficace!='Non') | ", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC")), data = s), diagnostics = TRUE)
summary(ivreg(as.formula(paste("taxe_approbation!='Non' ~ (taxe_efficace!='Non') | apres_modifs + info_CC")), data = s, weights = s$weight), diagnostics = TRUE)
summary(ivreg(as.formula(paste("taxe_approbation!='Non' ~ (taxe_efficace!='Non') | apres_modifs + info_CC")), data = s), diagnostics = TRUE)

summary(ivreg(as.formula(paste("taxe_approbation=='Oui' ~ ", paste(variables_reg_ee, collapse = ' + '), 
       "+ (taxe_efficace=='Oui') | ", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC")), data = s), diagnostics = TRUE)
summary(ivreg(as.formula(paste("taxe_approbation=='Oui' ~ (taxe_efficace=='Oui') | apres_modifs + info_CC")), data = s, weights = s$weight), diagnostics = TRUE)
# use only the best instrument so the coef is unbiased (Pishke p. 19: http://econ.lse.ac.uk/staff/spischke/ec533/Weak%20IV.pdf)
# counter-argument p. 21: LIML or other alternative estimators aren't necessarily better than 2SLS (they work only in homoskedastic case, cf. Andrews et al. 2019, Kolesar 2013)
# counter-counter-argument: then one should use CLR (or AR) confidence sets, and in our case they are infinite, evidence that instruments are too weak and model not identified
# => juste EE en mode Oui ~ Oui (anciennes idées: tout refaire en mode Oui ~ not No / maintenir le 2SLS en disant que c'est pas bien identifié et donner IC à 85% et 40% / introduire hétérogénéité dans effets)
summary(ivreg(as.formula(paste("taxe_approbation!='Non' ~ (taxe_efficace!='Non') | info_CC")), data = s, weights = s$weight), diagnostics = TRUE) 
summary(ivreg(as.formula(paste("taxe_approbation!='Non' ~ (taxe_efficace!='Non') | apres_modifs")), data = s, weights = s$weight), diagnostics = TRUE) 

summary(lm(taxe_efficace!='Non' ~ apres_modifs + info_CC, data=s, weights = s$weight, na.action='na.exclude'))
summary(lm(as.formula(paste("taxe_approbation!='Non' ~ apres_modifs")), data = s, weights = s$weight)) 
summary(lm(as.formula(paste("taxe_approbation!='Non' ~ info_CC")), data = s, weights = s$weight)) 
summary(lm(as.formula(paste("taxe_approbation!='Non' ~ apres_modifs + info_CC")), data = s, weights = s$weight)) 

##### 5.2 EE #####
# (1) Yes ~ Yes, 2SLS: 42*** p.p. 
formula_tsls1_ee1 <- as.formula(paste("taxe_efficace=='Oui' ~", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC"))
tsls1_ee1 <- lm(formula_tsls1_ee1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_ee1)
s$taxe_efficace.hat <- tsls1_ee1$fitted.values
formula_tsls2_ee1 <- as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_reg_ee, collapse = ' + '), " + taxe_efficace.hat"))
tsls2_ee1 <- lm(formula_tsls2_ee1, data=s, weights = s$weight) 
summary(tsls2_ee1)

iv_ee1 <- summary(ivreg(as.formula(paste("taxe_approbation=='Oui' ~ ", paste(variables_reg_ee, collapse = ' + '), "+ (taxe_efficace=='Oui') | ", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC")), data = s))

# (2) Yes ~ Yes, OLS: 37*** p.p.
s$taxe_efficace.hat <- as.numeric(s$taxe_efficace=='Oui')
formula_ee2 <- as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_reg_ee, collapse = ' + '), " + taxe_efficace.hat"))
ols_ee2 <- lm(formula_ee2, data=s, weights = s$weight) 
summary(ols_ee2)

# (3) not No ~ Yes, LIML (cf. Stata)
liml_ee3 <- ivmodelFormula(as.formula(paste("tax_acceptance ~ ", paste(variables_reg_ee, collapse = ' + '), "+ taxe_efficace.hat | ", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC")), data = s)
liml_ee3

# (A1) Yes ~ Yes, logit: 29*** p.p. 
s$taxe_efficace.hat <- as.numeric(s$taxe_efficace=='Oui')
logit_ee1 <- glm(formula_ee2, family = binomial(link='logit'), data=s) # Warning: Hauck-Donner effect, run logitsf. For a test run anova.glm, not Wald  TODO. Also Sargan
summary(logit_ee1)
logit_ee1_margins <- logitmfx(data=s, formula=logit_ee1, atmean=FALSE)$mfxest
logit_ee1_margins

# (A2) not No ~ Yes, 2SLS: 50** p.p.
formula_tsls1_eea2 <- as.formula(paste("taxe_efficace=='Oui' ~", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC"))
tsls1_eea2 <- lm(formula_tsls1_eea2, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_eea2)
s$taxe_efficace.hat <- tsls1_eea2$fitted.values
formula_tsls2_eea2 <- as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_reg_ee, collapse = ' + '), " + taxe_efficace.hat"))
tsls2_eea2 <- lm(formula_tsls2_eea2, data=s, weights = s$weight) 
summary(tsls2_eea2)

iv_eea2 <- summary(ivreg(as.formula(paste("taxe_approbation!='Non' ~ ", paste(variables_reg_ee, collapse = ' + '), "+ (taxe_efficace=='Oui') | ", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC")), data = s))

# (A3) not No ~ Yes, OLS: 37*** p.p.
s$taxe_efficace.hat <- as.numeric(s$taxe_efficace=='Oui')
formula_eea3 <- as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_reg_ee, collapse = ' + '), " + taxe_efficace.hat"))
ols_eea3 <- lm(formula_eea3, data=s, weights = s$weight) 
summary(ols_eea3)

# (A4) not No ~ not No, 2SLS: 48** p.p.
formula_tsls1_eea4 <- as.formula(paste("taxe_efficace!='Non' ~", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC"))
tsls1_eea4 <- lm(formula_tsls1_eea4, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_eea4)
s$taxe_efficace.not_no <- tsls1_eea4$fitted.values
formula_tsls2_eea4 <- as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_reg_ee, collapse = ' + '), " + taxe_efficace.not_no"))
tsls2_eea4 <- lm(formula_tsls2_eea4, data=s, weights = s$weight) 
summary(tsls2_eea4)

iv_eea4 <- summary(ivreg(as.formula(paste("(taxe_approbation!='Non') ~ ", paste(variables_reg_ee, collapse = ' + '),  "+ (taxe_efficace!='Non') | ", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC")), data = s))

# (A5), not No ~ not No, OLS: 41*** p.p.
s$taxe_efficace.not_no <- as.numeric(s$taxe_efficace!='Non')
formula_eea5 <- as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_reg_ee, collapse = ' + '), " + taxe_efficace.not_no"))
ols_eea5 <- lm(formula_eea5, data=s, weights = s$weight) 
summary(ols_eea5)

f_stats_ee <- sprintf("%.1f", round(c(iv_ee1$diagnostics[1,3], iv_eea4$diagnostics[1,3]), 1))
liml_ee3$coef <- liml_ee3$sd <- ols_eea3$coefficients
liml_ee3$coef['taxe_efficace.hat'] <- liml_ee3$LIML$point.est
liml_ee3$sd['taxe_efficace.hat'] <- liml_ee3$LIML$std.err

Table_ee2 <- stargazer(tsls2_ee1, ols_ee2, ols_eea3, title="Effect of believing in environmental effectiveness on approval", star.cutoffs = NA, omit.table.layout = 'n',
                       covariate.labels = c("Believes in effectiveness ($\\dot{E}$)"), # Environmental effectiveness: ``Yes''
                       dep.var.labels = c("Approval ($\\dot{A^0}$)", "Acceptance ($A^0$)"), header = FALSE, column.labels = c("$IV$", "$OLS$", "$LIML$"), dep.var.caption = "Initial Tax \\& Dividend",
                       keep = c("efficace"), # "Constant",
                       coef = list(NULL, NULL, liml_ee3$coef),
                       se = list(NULL, NULL, liml_ee3$sd),
                       add.lines = list(c("Instruments: info E.E. \\& C.C. ", "\\checkmark ", "", "\\checkmark "),
                                        c("Controls: Socio-demo, other motives ", "\\checkmark ", "\\checkmark  ", "\\checkmark "),
                                        c("Effective F-Statistic", f_stats_ee[1], "", "")), 
                       no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:ee")
write_clip(gsub('\\end{table}', "} {\\footnotesize \\parbox[t]{\\textwidth}{\\linespread{1.2}\\selectfont \\textsc{Note:} Standard errors are reported in parentheses. The list of controls can be found in Appendix \\ref{set_controls}, and first stage results in Table \\vref{first_stage_environmental_effectiveness}. The dependent variable corresponds to either initial approval (answer ``Yes'' to support of the policy) or acceptance (answer not ``No''). The first stage exploits the information randomly displayed about climate change (C.C.) and the effectiveness of carbon taxation (E.E.) as exogenous instruments.}}\\end{table}", 
                    gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_ee2, fixed=TRUE), fixed=TRUE), collapse=' ')

Table_ee1 <- stargazer(tsls1_ee1, tsls1_eea4,
                       title="First stage regressions results for environmental effectiveness", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                       # "Info on Climate Change and/or on Particulates", "Info on Climate Change only", "Info on Particulates only"
                       covariate.labels = c("Info on Environmental Effectiveness ($Z_{E}$)",  
                                            "Info on Climate Change ($Z_{CC}$)", "Info on Particulate Matter ($Z_{PM}$)", "$Z_{CC} \\times Z_{PM}$"), 
                       dep.var.labels = c("``Yes''", "not ``No''"), dep.var.caption = "Environmental effectiveness", header = FALSE, star.cutoffs = NA, omit.table.layout = 'n',
                       keep = c("info", "apres_modifs"), 
                       column.labels = c("(1; A2)", "(A4)"), model.numbers = FALSE,
                       add.lines = list(c("Controls ", "\\checkmark ", "\\checkmark "), c("Effective F-Statistic", f_stats_ee)), 
                       no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="first_stage_environmental_effectiveness")
write_clip(gsub('\\end{table}', '} {\\footnotesize \\textsc{Note:} The information randomly displayed about climate change ($Z_{CC}$) and the effectiveness of carbon taxation ($Z_{E}$) are used as sources of exogenous variation in the belief. See discussion in the main text, Section \\vref{subsec:motive_ee}. } \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', 
                                                       Table_ee1, fixed=TRUE), fixed=TRUE), collapse=' ')

Table_eea <- stargazer(logit_ee1, tsls2_eea2, ols_eea3, tsls2_eea4, ols_eea5, title="Effect of believing in environmental effectiveness on support: second stages of alternative specifications", 
                       dep.var.caption = "Initial Tax \\& Dividend", model.names = F, covariate.labels = c("Environmental effectiveness: ``Yes''", "Environmental effectiveness: not ``No''"), 
                       dep.var.labels = c("Approval ($\\dot{A^0}$)", "Acceptance ($A^0$)"), header = FALSE, column.labels = c("$logit$", "$IV$", "$OLS$", "$IV$", "$OLS$"),
                       keep = c("efficace"), star.cutoffs = NA, omit.table.layout = 'n', # "Constant",
                       coef = list(logit_ee1_margins[,1], NULL, NULL, NULL, NULL), # TODO: column names (A1), ...
                       se = list(logit_ee1_margins[,2], NULL, NULL, NULL, NULL),
                       add.lines = list(c("Instruments: info E.E. \\& C.C. ", "", "\\checkmark ", "", "\\checkmark ", ""),
                                        c("Controls: Socio-demo, other motives ", "\\checkmark ", "\\checkmark  ", "\\checkmark ", "\\checkmark ", "\\checkmark "),
                                        c("Effective F-Statistic", "", f_stats_ee[1], "", f_stats_ee[2], "")), 
                       no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:eea")
write_clip(gsub('\\end{table}', '} {\\footnotesize \\parbox[t]{1.05\\textwidth}{\\hspace{-.05\\textwidth} \\linespread{1.2}\\selectfont \\textsc{Note:} Standard errors are reported in parentheses. For logit, average marginal effects are reported and not coefficients. The list of controls can be found in Appendix \\ref{set_controls}, and discussion in the main text, Section \\vref{subsec:motive_ee}.}}\\end{table}',  # first stage results in Table \\vref{first_stage_environmental_effectiveness}.
                    gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_eea, fixed=TRUE), fixed=TRUE), collapse=' ')


##### Causal complementary  effects #####
# The specifications of this section are mis-specified. Or rather, they provide unbiased estimates only if there is not omitted variable.
# variables_all_controls <- variables_reg_all #
variables_all_controls <- c(variables_reg_all, "taxe_approbation") # variables_reg_all #

# I don't include taxe_approbation as control here because this would capture the (non-interacted) effect of infos_EE, I include it in the others to avoid effects from beginning of questionnaire.
formula_all1_ee <- as.formula(paste("taxe_efficace!='Non' ~ apres_modifs + info_CC + ",  paste(variables_reg_all, collapse = ' + ')))
tsls1_all1_ee <- lm(formula_all1_ee, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_all1_ee)
s$taxe_efficace.hat <- fitted.values(tsls1_all1_ee)

# (1) A^T = ^SI * ^EE + X = 0.66*** ^SI + 0.61* ^EE - 0.47 ^SI*^EE + X (all controls for SI et s2 but no approbation for EE) (all controls everywhere: 0.61*** ^SI + 0.57. ^EE - 0.32* ^SI*^EE + X)
formula_all1_si <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~", paste(variables_all_controls, collapse = ' + '), "+ traite_cible * traite_cible_conjoint + cible"))
tsls1_all1_si <- lm(formula_all1_si, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_all1_si)
s$non_perdant <- fitted.values(tsls1_all1_si)

formula2_all1 <- as.formula(paste("taxe_cible_approbation!='Non' ~", paste(variables_all_controls, collapse = ' + '), " + cible + non_perdant * taxe_efficace.hat"))
tsls2_all1 <- lm(formula2_all1, data=s, weights=s$weight)
summary(tsls2_all1)

# (2) A^F = ^SI * ^EE + X = 0.55*** ^SI + 0.52* ^EE - 0.26 ^SI*^EE + X (all controls for SI et s2 but no approbation for EE)
formula_tsls1_all2_si <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant +", paste(variables_all_controls, collapse = ' + ')))
tsls1_all2_si <- lm(formula_tsls1_all2_si, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_all2_si)
s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_all2_si$fitted.values

formula2_all2 <- as.formula(paste("taxe_feedback_approbation!='Non' ~", paste(variables_all_controls, collapse = ' + '), " + non_perdant * taxe_efficace.hat"))
tsls2_all2 <- lm(formula2_all2, data=s, weights=s$weight, subset= variante_taxe_info=='f')
summary(tsls2_all2)


##### All causal effects ####
# # (3) A^T = ^SI * ^EE = 0.14 ^SI + 0.30 ^EE + 1.12 ^(SI*EE) (almost no control), similar results with controls
# formula_all1_ee <- as.formula(paste("taxe_efficace!='Non' ~ cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single + 
#     apres_modifs * info_CC * traite_cible * traite_cible_conjoint"))
# tsls1_all1_ee <- lm(formula_all1_ee, data=s, weights = s$weight, na.action='na.exclude')
# summary(tsls1_all1_ee)
# s$taxe_efficace.hat <- fitted.values(tsls1_all1_ee)
# 
# formula_all1_siee <- as.formula(paste("(gagnant_cible_categorie!='Perdant')*(taxe_efficace!='Non') ~ cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single + 
#     apres_modifs * info_CC * traite_cible * traite_cible_conjoint"))
# tsls1_all1_siee <- lm(formula_all1_siee, data=s, weights = s$weight, na.action='na.exclude')
# summary(tsls1_all1_siee)
# s$siee <- fitted.values(tsls1_all1_siee)
# 
# formula_all1_si <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single + 
#     apres_modifs * info_CC * traite_cible * traite_cible_conjoint"))
# tsls1_all1_si <- lm(formula_all1_si, data=s, weights = s$weight, na.action='na.exclude')
# summary(tsls1_all1_si)
# s$non_perdant <- fitted.values(tsls1_all1_si)
# 
# formula2_all3 <- as.formula(paste("taxe_cible_approbation!='Non' ~ cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single + 
#      + non_perdant + taxe_efficace.hat + siee"))
# tsls2_all3 <- lm(formula2_all3, data=s, weights=s$weight)
# summary(tsls2_all3) 
# 
# # (4) A^F = ^SI * ^EE = 0.14 ^SI + 0.30 ^EE + 1.12 ^(SI*EE) (almost no control), similar results with controls
# formula_all1_ee <- as.formula(paste("taxe_efficace!='Non' ~ Simule_gain + Simule_gain2 + apres_modifs * info_CC * simule_gagnant"))
# tsls1_all1_ee <- lm(formula_all1_ee, data=s, weights = s$weight, subset = !is.na(s$gagnant_feedback_categorie), na.action='na.exclude') # this instruments are weak
# summary(tsls1_all1_ee)
# s$taxe_efficace.hat[!is.na(s$gagnant_feedback_categorie)] <- fitted.values(tsls1_all1_ee)
# 
# formula_all1_siee <- as.formula(paste("(gagnant_feedback_categorie!='Perdant')*(taxe_efficace!='Non') ~ Simule_gain + Simule_gain2 + apres_modifs * info_CC * simule_gagnant"))
# tsls1_all1_siee <- lm(formula_all1_siee, data=s, weights = s$weight, na.action='na.exclude')
# summary(tsls1_all1_siee)
# s$siee <- fitted.values(tsls1_all1_siee)
# 
# formula_all1_si <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ Simule_gain + Simule_gain2 + apres_modifs * info_CC * simule_gagnant"))
# tsls1_all1_si <- lm(formula_all1_si, data=s, weights = s$weight, na.action='na.exclude')
# summary(tsls1_all1_si)
# s$non_perdant <- fitted.values(tsls1_all1_si)
# 
# formula2_all3 <- as.formula(paste("taxe_feedback_approbation!='Non' ~ Simule_gain + Simule_gain2 + non_perdant + taxe_efficace.hat + siee"))
# tsls2_all3 <- lm(formula2_all3, data=s, weights=s$weight)
# summary(tsls2_all3) 

# The appropriate specifications are below. No significant results because EE doesn't have strong instrument.
# A^T ~ ...
iv_siee <- ivreg(taxe_cible_approbation!='Non' ~ cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single
                 + (gagnant_cible_categorie!='Perdant') * (taxe_efficace!='Non') | cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single + 
    apres_modifs * info_CC * traite_cible * traite_cible_conjoint, data = s, weights = s$weight)
summary(iv_siee, diagnostics = TRUE)

# A^F ~ ...
iv_siee <- ivreg(taxe_feedback_approbation!='Non' ~ simule_gain + Simule_gain2 + (gagnant_feedback_categorie!='Perdant') * (taxe_efficace!='Non') | 
                   simule_gain + Simule_gain2 + apres_modifs * info_CC * simule_gagnant, data = s, weights = s$weight)
summary(iv_siee, diagnostics = TRUE)

iv_siee <- ivreg(taxe_feedback_approbation!='Non' ~ simule_gain + (gagnant_feedback_categorie!='Perdant') * (taxe_efficace!='Non') | 
                   simule_gain + apres_modifs * info_CC * simule_gagnant, data = s, weights = s$weight)
summary(iv_siee, diagnostics = TRUE)

# A^T ~ ... +  P
iv_siee <- ivreg(taxe_cible_approbation!='Non' ~ cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single
  + (gagnant_cible_categorie!='Perdant') * (taxe_efficace!='Non') * (progressivite!='Non') | cible + percentile_revenu + Revenu2 + percentile_revenu_conjoint + Revenu_conjoint2 + single + 
    info_CC * traite_cible * traite_cible_conjoint * info_progressivite, data = s, weights = s$weight)
summary(iv_siee, diagnostics = TRUE)

# A^F ~ ... + P
iv_siee <- ivreg(taxe_feedback_approbation!='Non' ~ simule_gain + (gagnant_feedback_categorie!='Perdant') * (taxe_efficace!='Non') * (progressivite!='Non') | 
                   simule_gain + info_CC * simule_gagnant * info_progressivite, data = s, weights = s$weight)
summary(iv_siee, diagnostics = TRUE)

# With controls
# A^T ~ ... + X
summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", paste(variables_all_controls, collapse = ' + '), 
                 "+ cible + (gagnant_cible_categorie!='Perdant') * (taxe_efficace!='Non') | ", paste(variables_all_controls, collapse = ' + '), 
                 "+ cible + apres_modifs + info_CC * traite_cible * traite_cible_conjoint")), data = s, weights = s$weight), diagnostics = TRUE)

# A^F ~ ... + X
summary(ivreg(as.formula(paste("taxe_feedback_approbation!='Non' ~ ", paste(variables_all_controls, collapse = ' + '), 
                 "+ cible + (gagnant_feedback_categorie!='Perdant') * (taxe_efficace!='Non') | ", paste(variables_all_controls, collapse = ' + '), 
                 "+ cible + apres_modifs * info_CC * simule_gagnant")), data = s, weights = s$weight), diagnostics = TRUE)

# A^T ~ ... + P + X
summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", paste(variables_all_controls, collapse = ' + '), 
                 "+ cible + (gagnant_cible_categorie!='Perdant') * (taxe_efficace!='Non') * (progressivite!='Non') | ", paste(variables_all_controls, collapse = ' + '), 
                 "+ cible + info_CC * traite_cible * traite_cible_conjoint * info_progressivite")), data = s, weights = s$weight), diagnostics = TRUE)

# A^F ~ ... + P + X
summary(ivreg(as.formula(paste("taxe_feedback_approbation!='Non' ~ ", paste(variables_all_controls, collapse = ' + '), 
                 "+ cible + (gagnant_feedback_categorie!='Perdant') * (taxe_efficace!='Non') * (progressivite!='Non') | ", paste(variables_all_controls, collapse = ' + '), 
                 "+ cible + info_CC * simule_gagnant * info_progressivite")), data = s, weights = s$weight), diagnostics = TRUE)

summary(lm(benefices_pauvres==T ~ info_progressivite, data = s, weights = s$weight))
summary(ivreg(taxe_info_approbation!='Non' ~ benefices_pauvres==T | info_progressivite, data = s, weights = s$weight))


##### FDR #####
variables_demo_bias <- variables_demo
variables_demo_bias <- variables_demo_bias[!(variables_demo_bias %in% c("sexe", "age_50_64", "age_65_plus", "taille_agglo"))] # , "fume", "actualite"
formula_bias <- as.formula(paste("abs(simule_gain - gain) > 110 ~ (sexe=='Féminin') + as.factor(taille_agglo) + (Diplome>=5) + revenu + ecologiste + Gauche_droite + 
                                 uc + Gilets_jaunes + ", paste(variables_demo_bias, collapse=' + ')))
reg_bias <- lm(formula_bias, data=s, weights=s$weight)
summary(reg_bias) # R^2: 0.06 (half due to Yellow Vests)
# sort(summary(reg_bias)$coefficients[,4]) # TODO: laisser intercept dans la liste des pvalues ?
pvalues_reg_bias <- summary(reg_bias)$coefficients[2:length(summary(reg_bias)$coefficients[,4]),4]
sort(p.adjust(pvalues_reg_bias, method = 'fdr'), decreasing=T) # fdr and BH seem to give same result
sort(qvalue(pvalues_reg_bias)$qvalues, decreasing=T)
for (v in c("Gilets_jaunes", "as.factor(taille_agglo)", "Gauche_droite", "statut_emploi", "csp", "region", "age", "actualite")) { # 
  nullhyp <- names(reg_bias$coefficients)[which(grepl(v, names(reg_bias$coefficients), fixed = T))]
  pvalues_reg_bias <- pvalues_reg_bias[!(names(pvalues_reg_bias) %in% nullhyp)]
  pvalues_reg_bias[v] <- linearHypothesis(reg_bias, nullhyp)$`Pr(>F)`[2]
}
nullhyp <- c('nb_adultes', 'nb_14_et_plus', 'uc')
pvalues_reg_bias <- pvalues_reg_bias[!(names(pvalues_reg_bias) %in% nullhyp)]
pvalues_reg_bias['composition_menage'] <- linearHypothesis(reg_bias, nullhyp)$`Pr(>F)`[2]
nullhyp <- c('revenu', 'rev_tot', 'niveau_vie')
pvalues_reg_bias <- pvalues_reg_bias[!(names(pvalues_reg_bias) %in% nullhyp)]
pvalues_reg_bias['revenus'] <- linearHypothesis(reg_bias, nullhyp)$`Pr(>F)`[2]
sort(p.adjust(pvalues_reg_bias, method = 'BH'), decreasing=T) # fdr and BH seem to give same result
sort(qvalue_truncp(pvalues_reg_bias)$qvalues, decreasing=T)
# sort(qvalue(pvalues_reg_bias, lambda = seq(0.05, 0.75, 0.05))$qvalues, decreasing=T)

logit_bias <- glm(formula_bias, family = binomial(link='logit'), data=s)
summary(logit_bias)
logit_bias_margins <- logitmfx(formula_bias, s, atmean=FALSE)$mfxest
logit_bias_margins
s$Gauche_droite <- relevel(s$Gauche_droite, "Indeterminate")
formula_bias_bis <- as.formula(paste("abs(simule_gain - gain) > 110 ~ taxe_approbation + (sexe=='Féminin') + as.factor(taille_agglo) + (Diplome>=5) + revenu + 
                                      ecologiste + Gauche_droite + uc + Gilets_jaunes + ", paste(variables_demo_bias, collapse=' + ')))
reg_bias_bis <- lm(formula_bias_bis, data=s, weights=s$weight)
summary(reg_bias_bis)
sort(p.adjust(summary(reg_bias_bis)$coefficients[,4], method = 'BH'), decreasing=T)


##### Motivated Reasoning (table 4.2) #####

variables_update <- c("revenu", "(gagnant_categorie=='Gagnant')", "Simule_gain", "as.factor(taille_agglo)", "retraites", "actifs", "etudiants", variables_demo, 
                      variables_politiques, "Gilets_jaunes") # 
variables_update <- variables_update[!(variables_update %in% c("revenu", "rev_tot", "age", "age_65_plus", "taille_agglo", "statut_emploi"))]

# (1)
base_winner <- lm(update_correct ~ gagnant_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight)
summary(base_winner)

variables_update_bis <- c("revenu", "(gagnant_categorie=='Gagnant')", "taxe_approbation", "Simule_gain", "as.factor(taille_agglo)", "retraites", "actifs", "etudiants", 
                          variables_demo, variables_politiques, "Gilets_jaunes") # 
variables_update_bis <- variables_update_bis[!(variables_update_bis %in% c("revenu", "rev_tot", "age", "age_65_plus", "taille_agglo", "statut_emploi"))]

# (2)
formula_update_base <- as.formula(paste("update_correct ~ gain + (gain==0) + I(gain - simule_gain) + ", paste(variables_update_bis, collapse=' + ')))
reg_update_base <- lm(formula_update_base, subset = feedback_infirme_large==T, data=s, weights = s$weight)
summary(reg_update_base)

# (3)
formula_update_diploma <- as.formula(paste("update_correct ~ gain + (gain==0) + I(gain - simule_gain) + diplome4*(taxe_approbation) + ", paste(variables_update_bis, collapse=' + ')))
reg_update_diploma <- lm(formula_update_diploma, subset = feedback_infirme_large==T, data=s, weights = s$weight)
summary(reg_update_diploma)

# (4)
reg_update_with_gain_gagnants <- lm(formula_update_base, subset = feedback_infirme_large==T & simule_gagnant==1, data=s, weights = s$weight)
summary(reg_update_with_gain_gagnants)

# (5)
formula_update_with_gain_no_bug <- as.formula(paste("update_correct ~ gain + (gain==0) + I(gain - simule_gain) + ", paste(variables_update_bis[!(variables_update_bis=='conservateur')], collapse=' + ')))
reg_update_with_gain_perdants <- lm(formula_update_with_gain_no_bug, subset = feedback_infirme_large==T & simule_gagnant==0, data=s, weights = s$weight)
summary(reg_update_with_gain_perdants)

heterogeneity_update <- stargazer(base_winner, reg_update_base, reg_update_diploma, reg_update_with_gain_gagnants, reg_update_with_gain_perdants,
                           title="Heterogeneity in updating.", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                           covariate.labels = c("Constant", "Winner, before feedback ($\\dot{G}$)", "Initial tax: PNR (I don't know)", "Initial tax: Approves",
                                                "Diploma $\\times$ Initial tax: PNR", "Diploma $\\times$ Initial tax: Approves",
                                                "Subjective gain ($g$)", "Subjective gain: unaffected ($g=0$)", "Bias about gain ($g - \\hat{\\gamma}$)",
                                                "Diploma (1 to 4)", "Retired", "Active", "Student", "Yellow Vests: PNR",
                                                "Yellow Vests: understands", "Yellow Vests: supports", "Yellow Vests: is part"),
                           dep.var.labels = c("Correct updating ($U$)"), dep.var.caption = "", header = FALSE, 
                           keep = c('Constant', '.*Gagnant.*', 'taxe_approbation', '^gain', 'I\\(gain', 'diplome4', 'retraites', 'actifs', 'etudiants', 'Gilets_jaunes'), 
                           order = c('Constant', '.*Gagnant.*', 'taxe_approbation', '^gain', 'I\\(gain', 'diplome4', 'retraites', 'actifs', 'etudiants', 'Gilets_jaunes'),
                           omit.table.layout = 'n', star.cutoffs = NA,
                           add.lines = list(c("Includes ``pessimistic winners''", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", ""), 
                                            c("Includes ``optimistic losers''", "\\checkmark", "\\checkmark", "\\checkmark", "", "\\checkmark"), 
                                            c("Includes controls", "", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")),
                           no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="tab:heterogeneity_update")
write_clip(gsub('\\end{table}', '} {\\footnotesize \\parbox[t]{13.5cm}{\\linespread{1.2}\\selectfont \\textsc{Note:} Omitted variables are \\textit{Unemployed/Inactive} and \\textit{Yellow Vests: opposes}. The list of controls can be found in Appendix \\ref{set_controls}.} }\\end{table}', 
                gsub('\\begin{tabular}{@', '\\resizebox{.90\\columnwidth}{!}{ \\begin{tabular}{@', heterogeneity_update, fixed=TRUE), fixed=TRUE), collapse=' ')



##### Heterogeneity in bias #####

variables_demo_bias <- variables_demo
variables_demo_bias <- variables_demo_bias[!(variables_demo_bias %in% c("sexe", "age_50_64", "age_65_plus", "taille_agglo"))]
formula_bias <- as.formula(paste("abs(simule_gain - gain) > 110 ~ (sexe=='Féminin') + as.factor(taille_agglo) + (Diplome>=5) + revenu + ecologiste + Gauche_droite + 
                                 uc + Gilets_jaunes + ", paste(variables_demo_bias, collapse=' + ')))
reg_bias <- lm(formula_bias, data=s, weights=s$weight)
summary(reg_bias) # R^2: 0.06 (half due to Yellow Vests)
logit_bias <- glm(formula_bias, family = binomial(link='logit'), data=s)
summary(logit_bias)
logit_bias_margins <- logitmfx(formula_bias, s, atmean=FALSE)$mfxest
logit_bias_margins
formula_bias_bis <- as.formula(paste("abs(simule_gain - gain) > 110 ~ taxe_approbation + (sexe=='Féminin') + as.factor(taille_agglo) + (Diplome>=5) + revenu + 
                                     ecologiste + Gauche_droite + uc + Gilets_jaunes + ", paste(variables_demo_bias, collapse=' + ')))
reg_bias_bis <- lm(formula_bias_bis, data=s, weights=s$weight)
summary(reg_bias_bis)

Table_heterogenous_bias <- stargazer(reg_bias, logit_bias, reg_bias_bis,#
                                     title="Determinants of bias in subjective gains", model.names = T, model.numbers = FALSE, #star.cutoffs = c(0.1, 1e-5, 1e-30), # "Diploma: Bachelor or above", 
                                     covariate.labels = c("Initial tax: PNR (I don't know)", "Initial tax: Approves",
                                                          "Yellow Vests: PNR","Yellow Vests: understands","Yellow Vests: supports", "Yellow Vests: is part",
                                                          "Ecologist", "Left-right: Indeterminate", "Left-right: Left", "Left-right: Center", "Left-right: Right", "Left-right: Extreme-right"),
                                     dep.var.labels = c("Large bias ($\\left|\\widehat{\\gamma}-g\\right| > 110$)"), dep.var.caption = "", header = FALSE,
                                     keep = c("taxe_approbation", "Gilets_jaunes", "Gauche_droite", "ecologiste"),
                                     order = c("taxe_approbation", "Gilets_jaunes", "ecologiste", "Gauche_droite"),
                                     omit.table.layout = 'n', star.cutoffs = NA,
                                     coef = list(NULL, logit_bias_margins[,1], NULL),
                                     se = list(NULL, logit_bias_margins[,2], NULL),
                                     add.lines = list(c("Controls: Socio-demo, political leaning", "\\checkmark", "\\checkmark", "\\checkmark")),
                                     no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:bias")
write_clip(gsub('\\end{table}', ' } {\\footnotesize \\parbox[t]{12cm}{\\linespread{1.2}\\selectfont \\textsc{Note:}  Standard errors are reported in parentheses. For logit, average marginal effects are reported and not coefficients. Omitted variables are \\textit{Yellow Vests: opposes}; \\textit{Left-right: Extreme-left}. The list of controls can be found in Appendix \\ref{set_controls}. }}  \\end{table} ',
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@',
                                                       Table_heterogenous_bias, fixed=TRUE), fixed=TRUE), collapse=' ')


##### Robustesse des 7 min #####
# TODO: recalculer d'autres poids
# 2 autres seuils (10 min, 15 min) et 3 résultats : SI, EE, et MR

# (1) SI, 11 min: 55*** p.p.
tsls1_si10 <- lm(formula_tsls1_si1, data=sl, subset = ((percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10)), weights = sl$weight)
summary(tsls1_si10)
sl$non_perdant[((sl$percentile_revenu <= 60 & sl$percentile_revenu >= 10) | (sl$percentile_revenu_conjoint <= 60 & sl$percentile_revenu_conjoint >= 10))] <- tsls1_si10$fitted.values
tsls2_si10 <- lm(formula_tsls2_si1, data=sl, subset = ((percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10)), weights = sl$weight)
summary(tsls2_si10) 

iv_si10 <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", paste(variables_reg_self_interest, collapse=' + '),
        " + cible + I(taxe_approbation=='NSP') + tax_acceptance + (gagnant_cible_categorie!='Perdant') | . - (gagnant_cible_categorie!='Perdant') + traite_cible*traite_cible_conjoint")),
        data = sl, subset = (duree > 600 & (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10)), weights = sl$weight), diagnostics = TRUE)

# (2) SI, 0 min: 57*** p.p.
tsls1_si0 <- lm(formula_tsls1_si1, data=ss, subset = ((percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10)), weights = ss$weight, na.action = "na.exclude")
summary(tsls1_si0)
ss$non_perdant[((ss$percentile_revenu <= 60 & ss$percentile_revenu >= 10) | (ss$percentile_revenu_conjoint <= 60 & ss$percentile_revenu_conjoint >= 10))] <- fitted(tsls1_si0)
tsls2_si0 <- lm(formula_tsls2_si1, data=ss, subset = ((percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10)), weights = ss$weight)
summary(tsls2_si0) 

iv_si0 <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", paste(variables_reg_self_interest, collapse=' + '),
        " + cible + I(taxe_approbation=='NSP') + tax_acceptance + (gagnant_cible_categorie!='Perdant') | . - (gagnant_cible_categorie!='Perdant') + traite_cible*traite_cible_conjoint")),
        data = ss, subset = ((percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10)), weights = ss$weight), diagnostics = TRUE)

# (3) SI, qualite: 56*** p.p.
tsls1_siq <- lm(formula_tsls1_si1, data=sq, subset = ((percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10)), weights = sq$weight, na.action = "na.exclude")
summary(tsls1_siq)
sq$non_perdant[((sq$percentile_revenu <= 60 & sq$percentile_revenu >= 10) | (sq$percentile_revenu_conjoint <= 60 & sq$percentile_revenu_conjoint >= 10))] <- fitted(tsls1_siq)
tsls2_siq <- lm(formula_tsls2_si1, data=sq, subset = ((percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10)), weights = sq$weight)
summary(tsls2_siq) 

iv_siq <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", paste(variables_reg_self_interest, collapse=' + '),
        " + cible + I(taxe_approbation=='NSP') + tax_acceptance + (gagnant_cible_categorie!='Perdant') | . - (gagnant_cible_categorie!='Perdant') + traite_cible*traite_cible_conjoint")),
        data = sq, subset = ((percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10)), weights = sq$weight), diagnostics = TRUE)

# (4) MR, 11 min: 53*** p.p. / 22*** p.p.
reg_update_base_10 <- lm(formula_update_base, data=sl, subset = feedback_infirme_large==T, weights = sl$weight)
summary(reg_update_base_10)

# (5) MR, 0 min: 54*** p.p. / 18*** p.p.
reg_update_base_0 <- lm(formula_update_base, subset = feedback_infirme_large==T, data=ss, weights = weight)
summary(reg_update_base_0)

# (6) MR, qualite: 55*** p.p. / 20*** p.p.
reg_update_base_q <- lm(formula_update_base, subset = feedback_infirme_large==T, data=sq, weights = weight)
summary(reg_update_base_q)

f_stats_7min <- sprintf("%.1f", round(c(iv_si0$diagnostics[1,3], iv_si10$diagnostics[1,3], iv_siq$diagnostics[1,3]), 1))

Table_robustesse_exclude <- stargazer(tsls2_si0, tsls2_si10, tsls2_siq, reg_update_base_0, reg_update_base_10, reg_update_base_q,
                                     title="Robustness of main results to the exclusion of answers of poor quality.", model.names = F, model.numbers = FALSE, #star.cutoffs = c(0.1, 1e-5, 1e-30), # "Diploma: Bachelor or above", 
                                     column.labels = c("all", "> 11 min", "not flagged", "all", "> 11 min", "not flagged"), 
                                     covariate.labels = c("Believes does not lose (.53)", "Winner, before feedback (.55)", "Initial tax: Approves (.18)"),
                                     dep.var.labels = c("Acceptance ($A^T$)", "Correct updating ($U$)"), dep.var.caption = "", header = FALSE,
                                     keep = c("non_perdant", '"Gagnant"TRUE', "taxe_approbationOui"),
                                     order = c("non_perdant", '"Gagnant"TRUE', "taxe_approbationOui"),
                                     omit.table.layout = 'n', star.cutoffs = NA,
                                     add.lines = list(c("Original regression: Table (column)", "\\ref{results_private_benefits} (1)", "\\ref{results_private_benefits} (1)", "\\ref{results_private_benefits} (1)", "\\ref{tab:heterogeneity_update} (2)", "\\ref{tab:heterogeneity_update} (2)", "\\ref{tab:heterogeneity_update} (2)"),
                                                      c("Effective F-statistic", f_stats_7min, "", "", ""),
                                                      c("Whole sample size", rep(c(nrow(sl), nrow(ss), nrow(sq)),2))), # TODO
                                     no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:7min")
write_clip(gsub('\\end{table}', ' } {\\footnotesize \\parbox[t]{\\textwidth}{\\linespread{1.2}\\selectfont \\textsc{Note:} Two of our main results are checked on three alternative sampling restrictions: (1) inclusion of answers < 7 min, (2) exclusion of the 10\\% of answers < 11 min, (3) exclusion of flagged (inconsistent) respondents. Weights have been recalculated for each sample. Estimates on the original sample are reported next to variable name. See the original Tables for more details. Correlation between our main variables of interest and response time or being flagged is always below 3\\%. Standard errors are reported in parentheses. }}  \\end{table} ',
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_robustesse_exclude, fixed=TRUE), fixed=TRUE), collapse=' ')

cor(s$duree, s$tax_approval) # -0.01
cor(s$duree, s$tax_acceptance) # -0.0007
cor(s$duree, s$gain) # -0.01
cor(s$mauvaise_qualite, s$tax_acceptance) # .025
cor(s$mauvaise_qualite, s$gain) # -0.01
cor(s$mauvaise_qualite > 0, s$tax_acceptance) # .026
cor(s$mauvaise_qualite > 0, s$gain) # -0.007

# # (2) EE, 10 min: 40** p.p.
# tsls1_ee10 <- lm(formula_tsls1_ee1, data=sl, weights = sl$weight, na.action='na.exclude')
# summary(tsls1_ee10)
# sl$taxe_efficace.hat <- tsls1_ee10$fitted.values
# tsls2_ee10 <- lm(formula_tsls2_ee1, data=sl, weights = sl$weight) 
# summary(tsls2_ee10)
# 
# iv_ee10 <- summary(ivreg(as.formula(paste("taxe_approbation=='Oui' ~ ", paste(variables_reg_ee, collapse = ' + '), "+ (taxe_efficace=='Oui') | ", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC")), data = sl), diagnostics = TRUE)

# # (5) EE, 0 min: 41** p.p.
# formula_tsls1_ee0 <- as.formula(paste("taxe_efficace=='Oui' ~", paste(variables_reg_ee[!(variables_reg_ee %in% c("actualite", "fume"))], collapse = ' + '), " + apres_modifs + info_CC"))
# tsls1_ee0 <- lm(formula_tsls1_ee0, data=ss, weights = ss$weight, na.action='na.exclude')
# summary(tsls1_ee0)
# ss$taxe_efficace.hat <- fitted(tsls1_ee0)
# formula_tsls2_ee0 <- as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_reg_ee[!(variables_reg_ee %in% c("actualite", "fume"))], collapse = ' + '), " + taxe_efficace.hat"))
# tsls2_ee0 <- lm(formula_tsls2_ee0, data=ss, weights = ss$weight) 
# summary(tsls2_ee0)
# 
# iv_ee0 <- summary(ivreg(as.formula(paste("taxe_approbation=='Oui' ~ ", paste(variables_reg_ee, collapse = ' + '), "+ (taxe_efficace=='Oui') | ", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC")), data = ss), diagnostics = TRUE)

# 
# # (-1) SI, 14 min: 60 p.p.
# tsls1_si15 <- lm(formula_tsls1_si1, data=sr, subset = ((percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10)), weights = sr$weight)
# summary(tsls1_si15)
# sr$non_perdant[((sr$percentile_revenu <= 60 & sr$percentile_revenu >= 10) | (sr$percentile_revenu_conjoint <= 60 & sr$percentile_revenu_conjoint >= 10))] <- tsls1_si15$fitted.values
# tsls2_si15 <- lm(formula_tsls2_si1, data=sr, subset = ((percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10)), weights = sr$weight)
# summary(tsls2_si15) 
# 
# iv_si15 <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", paste(variables_reg_self_interest, collapse=' + '),
#         " + cible + I(taxe_approbation=='NSP') + tax_acceptance + (gagnant_cible_categorie!='Perdant') | . - (gagnant_cible_categorie!='Perdant') + traite_cible*traite_cible_conjoint")),
#         data = sr, subset = ((percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10)), weights = sr$weight), diagnostics = TRUE)
# 
# 
# # (-2) EE, 14 min: 39* p.p.
# tsls1_ee15 <- lm(formula_tsls1_ee1, data=sr, weights = weight, na.action='na.exclude')
# summary(tsls1_ee15)
# sr$taxe_efficace.hat <- tsls1_ee15$fitted.values
# tsls2_ee15 <- lm(formula_tsls2_ee1, data=sr, weights = weight) 
# summary(tsls2_ee15)
# 
# iv_ee15 <- summary(ivreg(as.formula(paste("taxe_approbation=='Oui' ~ ", paste(variables_reg_ee, collapse = ' + '), "+ (taxe_efficace=='Oui') | ", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC")), data = sr, weights = sr$weight), diagnostics = TRUE)
# 
# 
# # (-3) MR, 14 min: 68. p.p. / 20*** p.p.
# reg_update_base_15 <- lm(formula_update_base, subset = feedback_infirme_large==T, data=sr, weights = sr$weight)
# summary(reg_update_base_15)

# Table_robustesse_7min <- stargazer(tsls2_si0, tsls2_si10, tsls2_ee0, tsls2_ee10, reg_update_base_0, reg_update_base_10,#
#                                      title="Robustness of main results to the 7 min cutoff excluding faster respondents.", model.names = F, model.numbers = FALSE, #star.cutoffs = c(0.1, 1e-5, 1e-30), # "Diploma: Bachelor or above", 
#                                      column.labels = c("all", "> 11 min", "all", "> 11 min", "all", "> 11 min"), 
#                                      covariate.labels = c("Believes does not lose (.53)", "Believes effective (.42)", "Winner, before feedback (.69)", "Initial tax: Approves (.18)"),
#                                      dep.var.labels = c("Acceptance ($A^T$)", "Approval ($\\dot{A^0}$)", "Correct updating ($U$)"), dep.var.caption = "", header = FALSE,
#                                      keep = c("non_perdant", "taxe_efficace.hat", '"Gagnant"TRUE', "taxe_approbationOui"),
#                                      order = c("non_perdant", "taxe_efficace.hat", '"Gagnant"TRUE', "taxe_approbationOui"),
#                                      omit.table.layout = 'n', star.cutoffs = NA,
#                                      add.lines = list(c("Original regression: Table (column)", "\\ref{results_private_benefits} (1)", "\\ref{results_private_benefits} (1)", "\\ref{tab:ee} (1)", "\\ref{tab:ee} (1)", "\\ref{tab:heterogeneity_update} (2)", "\\ref{tab:heterogeneity_update} (2)"),
#                                                       c("Effective F-statistic", f_stats_7min, "", "")), # TODO
#                                      no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:7min")
# write_clip(gsub('\\end{table}', ' } {\\footnotesize \\parbox[t]{\\textwidth}{\\linespread{1.2}\\selectfont \\textsc{Note:} Standard errors are reported in parentheses. Original estimates are reported next to variable name. See the original Tables for more details. }}  \\end{table} ',
#                 gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_robustesse_7min, fixed=TRUE), fixed=TRUE), collapse=' ')

# f_stats_7min_bis <- sprintf("%.1f", round(c(iv_si0$diagnostics[1,3], iv_si15$diagnostics[1,3], iv_ee0$diagnostics[1,3], iv_ee15$diagnostics[1,3]), 1))
# Table_robustesse_7min_bis <- stargazer(tsls2_si0, tsls2_si15, tsls2_ee0, tsls2_ee15, reg_update_base_0, reg_update_base_15,#
#                                      title="Robustness of main results to the 7 min cutoff excluding faster respondents.", model.names = F, model.numbers = FALSE, #star.cutoffs = c(0.1, 1e-5, 1e-30), # "Diploma: Bachelor or above", 
#                                      column.labels = c("all", "> 14 min", "all", "> 14 min", "all", "> 14 min"), 
#                                      covariate.labels = c("Believes does not lose (.53)", "Believes effective (.42)", "Winner, before feedback (.69)", "Initial tax: Approves (.18)"),
#                                      dep.var.labels = c("Acceptance ($A^T$)", "Approval ($\\dot{A^0}$)", "Correct updating ($U$)"), dep.var.caption = "", header = FALSE,
#                                      keep = c("non_perdant", "taxe_efficace.hat", '"Gagnant"TRUE', "taxe_approbationOui"),
#                                      order = c("non_perdant", "taxe_efficace.hat", '"Gagnant"TRUE', "taxe_approbationOui"),
#                                      omit.table.layout = 'n', star.cutoffs = NA,
#                                      add.lines = list(c("Original regression: Table (column)", "\\ref{results_private_benefits} (1)", "\\ref{results_private_benefits} (1)", "\\ref{tab:ee} (1)", "\\ref{tab:ee} (1)", "\\ref{tab:heterogeneity_update} (2)", "\\ref{tab:heterogeneity_update} (2)"),
#                                                       c("Effective F-statistic", f_stats_7min, "", "")), # TODO
#                                      no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:7min")
# write_clip(gsub('\\end{table}', ' } {\\footnotesize \\parbox[t]{\\textwidth}{\\linespread{1.2}\\selectfont \\textsc{Note:} Standard errors are reported in parentheses. Original estimates are reported next to variable name. See the original Tables for more details. }}  \\end{table} ',
#                 gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_robustesse_7min_bis, fixed=TRUE), fixed=TRUE), collapse=' ')

##### Table D.3 SI 2nd stage alternatives #####
# (1) Target: Acceptance ~ win 
formula_tsls1_sia1 <- as.formula(paste("gagnant_cible_categorie=='Gagnant' ~ traite_cible*traite_cible_conjoint + cible + I(taxe_approbation=='NSP') + tax_acceptance + ", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_sia1 <- lm(formula_tsls1_sia1, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls1_sia1)
s$gagnant[(s$percentile_revenu <= 60 & s$percentile_revenu >= 10) | (s$percentile_revenu_conjoint <= 60 & s$percentile_revenu_conjoint >= 10)] <- tsls1_sia1$fitted.values
formula_tsls2_sia1 <- as.formula(paste("taxe_cible_approbation!='Non' ~ ", 
                                      paste(variables_reg_self_interest, collapse = ' + '), "+ cible + I(taxe_approbation=='NSP') + tax_acceptance + gagnant")) # 
tsls2_sia1 <- lm(formula_tsls2_sia1, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls2_sia1) 

iv_sia1 <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", paste(variables_reg_self_interest, collapse=' + '), 
        " + cible + I(taxe_approbation=='NSP') + tax_acceptance + (gagnant_cible_categorie=='Gagnant') | . - (gagnant_cible_categorie=='Gagnant') + traite_cible*traite_cible_conjoint")), 
        data = s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight), diagnostics = TRUE)

# (2) Target: Approval ~ win
formula_tsls1_sia2 <- as.formula(paste("gagnant_cible_categorie=='Gagnant' ~ traite_cible*traite_cible_conjoint + cible + I(taxe_approbation=='NSP') + tax_acceptance + ", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_sia2 <- lm(formula_tsls1_sia2, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls1_sia2)
s$gagnant[(s$percentile_revenu <= 60 & s$percentile_revenu >= 10) | (s$percentile_revenu_conjoint <= 60 & s$percentile_revenu_conjoint >= 10)] <- tsls1_sia2$fitted.values
formula_tsls2_sia2 <- as.formula(paste("taxe_cible_approbation=='Oui' ~ ", 
                                      paste(variables_reg_self_interest, collapse = ' + '), "+ cible + I(taxe_approbation=='NSP') + tax_acceptance + gagnant")) # 
tsls2_sia2 <- lm(formula_tsls2_sia2, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls2_sia2) 

iv_sia2 <- summary(ivreg(as.formula(paste("taxe_cible_approbation=='Oui' ~ ", paste(variables_reg_self_interest, collapse=' + '), 
        " + cible + I(taxe_approbation=='NSP') + tax_acceptance + (gagnant_cible_categorie=='Gagnant') | . - (gagnant_cible_categorie=='Gagnant') + traite_cible*traite_cible_conjoint")), 
        data = s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight), diagnostics = TRUE)

# (3) Target: Approval ~ not lose
formula_tsls1_sia3 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible*traite_cible_conjoint + cible + I(taxe_approbation=='NSP') + tax_acceptance + ", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_sia3 <- lm(formula_tsls1_sia3, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls1_sia3)
s$non_perdant[(s$percentile_revenu <= 60 & s$percentile_revenu >= 10) | (s$percentile_revenu_conjoint <= 60 & s$percentile_revenu_conjoint >= 10)] <- tsls1_sia3$fitted.values
formula_tsls2_sia3 <- as.formula(paste("taxe_cible_approbation=='Oui' ~ ", 
                                      paste(variables_reg_self_interest, collapse = ' + '), "+ cible + I(taxe_approbation=='NSP') + tax_acceptance + non_perdant")) # 
tsls2_sia3 <- lm(formula_tsls2_sia3, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls2_sia3) 

iv_sia3 <- summary(ivreg(as.formula(paste("taxe_cible_approbation=='Oui' ~ ", paste(variables_reg_self_interest, collapse=' + '), 
        " + cible + I(taxe_approbation=='NSP') + tax_acceptance + (gagnant_cible_categorie!='Perdant') | . - (gagnant_cible_categorie!='Perdant') + traite_cible*traite_cible_conjoint")), 
        data = s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight), diagnostics = TRUE)

# (4) Feedback: Acceptance ~ win
formula_tsls1_sia4 <- as.formula(paste("gagnant_feedback_categorie=='Gagnant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + ", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_sia4 <- lm(formula_tsls1_sia4, data=s, subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight, na.action='na.exclude')
summary(tsls1_sia4)
s$gagnant[s$variante_taxe_info=='f' & abs(s$simule_gain) < 50] <- tsls1_sia4$fitted.values
formula_tsls2_sia4 <- as.formula(paste("taxe_feedback_approbation!='Non' ~ (taxe_approbation=='NSP') + ", 
                                      paste(variables_reg_self_interest, collapse = ' + '), " + gagnant + tax_acceptance"))
tsls2_sia4 <- lm(formula_tsls2_sia4, data=s, subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight) 
summary(tsls2_sia4)

iv_sia4 <- summary(ivreg(as.formula(paste("taxe_feedback_approbation!='Non' ~ tax_acceptance + I(taxe_approbation=='NSP') + ", paste(variables_reg_self_interest, collapse = ' + '),
      " + (gagnant_feedback_categorie=='Gagnant') | . - (gagnant_feedback_categorie=='Gagnant') + simule_gagnant")), data = s, 
      subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight), diagnostics = TRUE)

# (5) Feedback: Approval ~ win
formula_tsls1_sia5 <- as.formula(paste("gagnant_feedback_categorie=='Gagnant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + ", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_sia5 <- lm(formula_tsls1_sia5, data=s, subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight, na.action='na.exclude')
summary(tsls1_sia5)
s$gagnant[s$variante_taxe_info=='f' & abs(s$simule_gain) < 50] <- tsls1_sia5$fitted.values
formula_tsls2_sia5 <- as.formula(paste("taxe_feedback_approbation=='Oui' ~ (taxe_approbation=='NSP') + ", 
                                      paste(variables_reg_self_interest, collapse = ' + '), " + gagnant + tax_acceptance"))
tsls2_sia5 <- lm(formula_tsls2_sia5, data=s, subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight) 
summary(tsls2_sia5)

iv_sia5 <- summary(ivreg(as.formula(paste("taxe_feedback_approbation=='Oui' ~ tax_acceptance + I(taxe_approbation=='NSP') + ", paste(variables_reg_self_interest, collapse = ' + '),
      " + (gagnant_feedback_categorie=='Gagnant') | . - (gagnant_feedback_categorie=='Gagnant') + simule_gagnant")), data = s, 
      subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight), diagnostics = TRUE)

# (6) Feedback: Approval ~ not lose
formula_tsls1_sia6 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + ", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_sia6 <- lm(formula_tsls1_sia6, data=s, subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight, na.action='na.exclude')
summary(tsls1_sia6)
s$non_perdant[s$variante_taxe_info=='f' & abs(s$simule_gain) < 50] <- tsls1_sia6$fitted.values
formula_tsls2_sia6 <- as.formula(paste("taxe_feedback_approbation=='Oui' ~ (taxe_approbation=='NSP') + ", 
                                      paste(variables_reg_self_interest, collapse = ' + '), " + non_perdant + tax_acceptance"))
tsls2_sia6 <- lm(formula_tsls2_sia6, data=s, subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight) 
summary(tsls2_sia6)

iv_sia6 <- summary(ivreg(as.formula(paste("taxe_feedback_approbation=='Oui' ~ tax_acceptance + I(taxe_approbation=='NSP') + ", paste(variables_reg_self_interest, collapse = ' + '),
      " + (gagnant_feedback_categorie!='Perdant') | . - (gagnant_feedback_categorie!='Perdant') + simule_gagnant")), data = s, 
      subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight), diagnostics = TRUE)

f_stats_sia <- sprintf("%.1f", round(c(iv_sia1$diagnostics[1,3], iv_sia2$diagnostics[1,3], iv_sia3$diagnostics[1,3], iv_sia4$diagnostics[1,3], iv_sia5$diagnostics[1,3], iv_sia6$diagnostics[1,3]), 1))
# Results
Table_additional_res <- stargazer(tsls2_sia1, tsls2_sia2, tsls2_sia3, tsls2_sia4, tsls2_sia5, tsls2_sia6,
       title="Effect of self-interest on acceptance: second stages of alternative specifications", #star.cutoffs = c(0.1, 1e-5, 1e-30),
       covariate.labels = c("Believes wins", "Believes does not lose", "Initial tax Acceptance ($A^0$)"), model.names = FALSE,
       dep.var.labels = c("Acceptance", "Approval", "Acceptance", "Approval"), omit.table.layout = 'n', star.cutoffs = NA,
       dep.var.caption = c("\\multicolumn{3}{c}{Targeted Dividend ($A^T$)} & \\multicolumn{3}{c}{After Feedback ($A^F$)"), header = FALSE,
       keep = c("gagnant", "non_perdant"),
       add.lines = list(
            c("Controls: Incomes (piecewise continuous)", "\\checkmark ", "\\checkmark  ", "\\checkmark ", "\\checkmark", "\\checkmark ", "\\checkmark"), 
            c("\\quad estimated gain, socio-demo, other motives ", "", "", "", ""),
            # c("Controls: Estimated gain ", "\\checkmark", "", "\\checkmark", "\\checkmark"),
            c("Controls: Policy assigned", "\\checkmark ", "\\checkmark ", "\\checkmark  ", "", "", ""),
            c("Sub-sample: [p10; p60] ($A^T$) or $\left| \widehat{\gamma}\right|<50$ ($A^F$)", "\\checkmark ", "\\checkmark  ", "\\checkmark ", "\\checkmark", "\\checkmark ", "\\checkmark"),
            c("Effective F-Statistic", f_stats_sia)),
       no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:alternative_si")
write_clip(sub("\\multicolumn{6}{c}{", "", gsub('\\end{table}', '} {\\footnotesize \\parbox[t]{\\textwidth}{\\linespread{1.2}\\selectfont \\textsc{Note:} See results of main specifications, Table \\vref{results_private_benefits}. As in the latter Table, the first-stages for the targeted dividend use as source of exogenous variation in the belief the random assignment of the income threshold that determines eligibility to the dividend. The first-stage for the non-targeted dividend exploits instead the discontinuity in the win/lose feedback when the net gain switches from negative to positive.} }.\\end{table}', 
   gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_additional_res, fixed=TRUE), fixed=TRUE), fixed=TRUE), collapse=' ')

##### Reduced form self-interest #####

# Target on acceptance
formula_reduced_form_targeted_acceptance <- as.formula(paste("taxe_cible_approbation!='Non' ~ traite_cible*traite_cible_conjoint + cible + I(taxe_approbation=='NSP') + tax_acceptance + ", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
reduced_form_targeted_acceptance <- lm(formula_reduced_form_targeted_acceptance, data=s, weights = s$weight)
summary(reduced_form_targeted_acceptance)

# Target on approval
formula_reduced_form_targeted_approval <- as.formula(paste("taxe_cible_approbation=='Oui' ~ traite_cible*traite_cible_conjoint + cible + I(taxe_approbation=='NSP') + tax_acceptance + ", 
                                                             paste(variables_reg_self_interest, collapse = ' + ')))
reduced_form_targeted_approval <- lm(formula_reduced_form_targeted_approval, data=s, weights = s$weight)
summary(reduced_form_targeted_approval)


# Feedback on acceptance
formula_reduced_form_feedback_acceptance <- as.formula(paste("taxe_feedback_approbation!='Non' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + ", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
reduced_form_feedback_acceptance <- lm(formula_reduced_form_feedback_acceptance, data=s, weights = s$weight, na.action='na.exclude')
summary(reduced_form_feedback_acceptance)

# Feedback on approval
formula_reduced_form_feedback_approval <- as.formula(paste("taxe_feedback_approbation=='Oui' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + ", 
                                                             paste(variables_reg_self_interest, collapse = ' + ')))
reduced_form_feedback_approval <- lm(formula_reduced_form_feedback_approval, data=s, weights = s$weight, na.action='na.exclude')
summary(reduced_form_feedback_approval)


# TODO: solve issue in write clip (delete \multicolumn{4}{c})
Table_reduced_form_si <- stargazer(reduced_form_targeted_acceptance, reduced_form_targeted_approval, reduced_form_feedback_acceptance, reduced_form_feedback_approval,
                       title="Reduced form effect of instruments for self-interest on support", star.cutoffs = NA, #column.labels = c("Acceptance", "Approval", "Acceptance", "Approval"),# column.separate = c(2,1,1),
                       dep.var.labels = c("Acceptance", "Approval", "Acceptance", "Approval"), 
                       dep.var.caption = c("\\multicolumn{2}{c}{Targeted Tax} & \\multicolumn{2}{c}{After Feedback}"), header = FALSE,
                       covariate.labels = c("Transfer to respondent ($T_1$)", "Transfer to spouse ($T_2$)",
                                            "$T_1 \\times T_2$", "Simulated winner ($\\widehat{\\Gamma}$)", "Initial tax Acceptance ($A^0$)"),
                       #column.labels = c("(1)", "(2)", "(3)", "(4)"), model.numbers = FALSE,
                       keep = c("traite", "simule_gagnant", "acceptance"), order = c("traite", "simule_gagnant", "acceptance"),
                       add.lines = list(c("Controls: Incomes (piecewise continuous)", " \\checkmark", " \\checkmark", "\\checkmark", "\\checkmark"),
                                        c("\\quad Estimated gain, socio-demo, other motives ", "", "", "", " " ),
                                        # c("Controls: Estimated gain", "", "", " \\checkmark ", " \\checkmark", " \\checkmark"),
                                        c("Controls: Policy assigned", " \\checkmark", " \\checkmark", " ", " "),
                                        # c("Controls: Socio-demo, other motives", "", "", " \\checkmark", " ", " \\checkmark"),
                                        no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="results_private_benefits"))
write_clip(gsub('\\end{table}', '} {\\footnotesize \\parbox[t]{\\textwidth}{\\linespread{1.2}\\selectfont \\textsc{Note:} Standard errors are reported in parentheses. The list of controls can be found in Appendix \\ref{set_controls}.} }\\end{table}', 
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_reduced_form_si, fixed=TRUE), fixed=T), collapse=' ')


##### 5.2 Sargan #####
summary(ivreg(as.formula(paste("taxe_approbation=='Oui' ~ ", paste(variables_reg_ee, collapse = ' + '), "+ (taxe_efficace=='Oui') | ", paste(variables_reg_ee, collapse = ' + '), 
                               " + apres_modifs * info_CC * info_PM")), data = s), diagnostics=T)
summary(ivreg(as.formula(paste("taxe_approbation=='Oui' ~ ", paste(variables_reg_ee, collapse = ' + '), "+ (taxe_efficace=='Oui') | ", paste(variables_reg_ee, collapse = ' + '), 
                               " + apres_modifs * info_CC")), data = s), diagnostics=T)
summary(ivreg(as.formula(paste("taxe_approbation=='Oui' ~ ", paste(variables_reg_ee, collapse = ' + '), "+ (taxe_efficace=='Oui') | ", paste(variables_reg_ee, collapse = ' + '), 
                               " + apres_modifs + info_CC")), data = s), diagnostics=T)


##### CDF for the bias #####
par(mar = c(4.1, 4.1, 1.1, 0.1), cex=1.5)
plot(Ecdf(s$simule_gain - s$gain), type="s", lwd=2, col="red", xlim=c(-100, 400), xlab=expression("Bias: objective minus subjective net gain"), main="", ylab=expression("Proportion "<=" x")) + grid() #  \\widehat{\\gamma} - g
lines(density(s$simule_gain - s$gain, bw=30)$x, density(s$simule_gain - s$gain, bw=30)$y/0.004, xlim=c(-100, 400), lwd=2, type='l', col="darkblue")
par(mar = mar_old, cex = cex_old)

# Identify point in the distriution for figure footnote
sum(((s$simule_gain - s$gain) > 200)*s$weight) / sum(s$weight)
