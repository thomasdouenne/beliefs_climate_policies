## Source to reproduce "Yellow Vests, Pessimistic Beliefs, and Carbon Tax Aversion"
#     by Thomas Douenne & Adrien Fabre, under licence CC-BY
# All files can be found on github: https://github.com/bixiou/beliefs_climate_policies
#     in particular, file to prepare the dataset (preparation.R), to load the packages and functions (packages_functions.R),
#     R environment with all the data prepared (.RData) and python files for complementary computations

source("packages_functions.R")
load(".RData")

##### 1 Introduction #####
decrit(s$taxe_approbation, miss=T, weights = s$weight)


##### 2 Context, survey, and data #####
## 2.1 Context of the study
decrit(s$gilets_jaunes >= 0, numbers=T, miss=T, weights=s$weight)
decrit(s$gilets_jaunes[s$centre!=''] >= 0, numbers=T, miss=T, weights=s$weight[s$centre!=''])


## 2.2 Our survey
# 2.2.1 
median(s$duree/60)

# 2.2.2 Table 2.1: Proportion of respondents per target of the payment
decrit(s$cible) # other lines of the Table are computed in preparation.R

# 2.1.3 Ensuring Data Quality
# Speeder screened out
wtd.mean(sa$duree < 7*60, weights = sa$weight) # 4% in original sample
wtd.mean(s$duree < 7*60, weights = s$weight) # 0 in final sample
# Response time uncorrelated with preferences:
summary(lm(taxe_approbation!='Non' ~ duree, data=s, weights = s$weight))
summary(lm(gagnant_categorie!='Perdant' ~ duree, data=s, weights = s$weight))
summary(lm(gain ~ duree, data=s, weights = s$weight))
# Error at test of quality: screened out
wtd.mean(sa$test_qualite!='Un peu', weights = sa$weight) # 9% in original sample
wtd.mean(s$test_qualite!='Un peu', weights = s$weight) # 0 in final sample
# Bad quality
sum(s$taille_menage > 12) # 10 before we capped it
sum(s$mauvaise_qualite > 0) # 273 mauvaise_qualite: Dummy for an aberrant answer to: revenu, taille_menage, nb_14_et_plus, km, conso, surface or generation_CC
# mauvaise_qualite uncorrelated with preferences
summary(lm(taxe_approbation!='Non' ~ mauvaise_qualite, data=s, weights = s$weight))
summary(lm(gagnant_categorie!='Perdant' ~ mauvaise_qualite, data=s, weights = s$weight))
summary(lm(gain ~ mauvaise_qualite, data=s, weights = s$weight))

## 2.3  Official households surveys
# Matching BdF/ENTD : cf. repository Github openfisca_france_indirect_taxation/build_survey_data/matching_bdf_entd : all files
# Data preparation: cf. prepare_dataset.py
# Computation of net gains: cf. define_tax_incidence_data.py and gain_losses_data.py


##### 3 Pessimistic beliefs #####
## 3.1 Self-interest
# Objective winning category: cf. consistency_belief_losses.py for weighted results reported in comments
decrit(objective_gains$all > 0, weights = objective_gains$weight) # 0.703
decrit(objective_gains$transport > 0) # 0.736
decrit(objective_gains$housing > 0) # 0.6749
# Subjective winning category
decrit(s$gagnant_categorie, weights = s$weight) # 14.0% think they win (21.7% unaffected)
decrit(s$gagnant_fuel_categorie, weights = s$weight) # 15.5% think they win (21.8% unaffected)
decrit(s$gagnant_chauffage_categorie, weights = s$weight) # 17.0% think they win (30.0% unaffected)
# Over-estimation of policy costs 
# Subjective losses
decrit(s$gain_fuel, weights = s$weight) # mean -61 instead of +18 # TODO: mean(objective_gains$all)
decrit(s$gain_chauffage, weights = s$weight) # -43 instead of +6
decrit(s$gain, weights = s$weight) # -89 instead of +24
decrit(as.numeric(s$gain) - s$simule_gain, weights = s$weight) # mean -126, median -116
decrit(s$simule_gain > s$gain, weights = s$weight) # 89%
decrit(s$simule_gain_inelastique - s$gain > 0, weights = s$weight) # 77%

# Figure 3.1: PDF of subjective vs. objective gain
mar_old <- par()$mar
cex_old <- par()$cex
par(mar = c(3.4, 3.4, 1.1, 0.1), cex=1.5)
# (a) transport
plot(density(objective_gains$transport, bw=30), xlim=c(-400, 200), lwd=2, col="darkblue", xlab="", ylab="", main="") + grid()
lines(density(subjective_gains$transport, bw=30), xlim=c(-400, 200), lwd=2, col="orange")
title(ylab="Density", xlab="Gain (in €/year per c.u.)", line=2.3)
# (b) housing
plot(density(objective_gains$housing, bw=30), xlim=c(-400, 200), lwd=2, col="darkblue", xlab="", ylab="", main="") + grid()
lines(density(subjective_gains$housing, bw=30), xlim=c(-400, 200), lwd=2, col="orange")
title(ylab="Density", xlab="Gain (in €/year per c.u.)", line=2.3)
# (c) both combined 
plot(density(objective_gains$all, bw=30), xlim=c(-400, 200), lwd=2, col="darkblue", xlab="", ylab="", main="") + grid()
lines(density(subjective_gains$all, bw=30), xlim=c(-400, 200), lwd=2, col="orange")
title(ylab="Density", xlab="Gain (in €/year per c.u.)", line=2.3)

# Figure 3.2: CDF of subjective vs. objective gain (including in the inelastic case)
par(mar = c(3.4, 3.4, 1.1, 0.1), cex=1.5)
# (a) transport
cdf_transport <- Ecdf(objective_gains$transport)
cdf_transport_inelastic <- Ecdf(objective_gains_inelastic$transport)
plot(Ecdf(s$gain_fuel), type="s", lwd=2, col="orange", main="", ylab="", xlab="") + grid()
lines(cdf_transport$x, cdf_transport$y, lwd=2, col="darkblue")
lines(cdf_transport_inelastic$x, cdf_transport_inelastic$y, lwd=2, lty=2, col="darkblue")
title(ylab=expression("Proportion "<=" x"), xlab="Gain (in €/year per c.u.)", line=2.3)
abline(v = c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), lty=3, col=rgb(1,165/255,0,1))
axis(3, at=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), tck=0.0, lwd=0, lwd.ticks = 0, padj=1.5, col.axis="orange", cex.axis=0.9)
# (b) housing
cdf_housing <- Ecdf(objective_gains$housing)
cdf_housing_inelastic <- Ecdf(objective_gains_inelastic$housing)
plot(Ecdf(s$gain_chauffage), type="s", lwd=2, col="orange", xlim=c(-250, 70), main="", ylab="", xlab="") + grid()
lines(cdf_housing$x, cdf_housing$y, lwd=2, col="darkblue")
lines(cdf_housing_inelastic$x, cdf_housing_inelastic$y, lwd=2, lty=2, col="darkblue")
title(ylab=expression("Proportion "<=" x"), xlab="Gain (in €/year per c.u.)", line=2.3)
abline(v=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), lty=3, col="orange") # rgb(1,0,0,0.7)
axis(3, at=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), tck=0.0, lwd=0, lwd.ticks = 0, padj=1.5, col.axis="orange", cex.axis=0.9)
# (c) both combined 
cdf_all <- Ecdf(objective_gains$all)
cdf_all_inelastic <- Ecdf(objective_gains_inelastic$all)
plot(Ecdf(s$gain), type="s", lwd=2, col="orange", xlim=c(-400, 150), main="", ylab="", xlab="") + grid()
lines(cdf_all$x, cdf_all$y, lwd=2, col="darkblue")
lines(cdf_all_inelastic$x, cdf_all_inelastic$y, lwd=2, lty=2, col="darkblue")
title(ylab=expression("Proportion "<=" x"), xlab="Gain (in €/year per c.u.)", line=2.3)
abline(v=c(-280, -190, -120, -70, -30, 0, 20, 40, 60, 80), lty=3, col="orange")
axis(3, at=c(-280, -190, -120, -70, -30, 0, 20, 40, 60, 80), tck=0.0, lwd=0, lwd.ticks = 0, padj=1.5, col.axis="orange", cex.axis=0.9)
# legend("topleft", col=c("orange", "darkblue", "darkblue"), cex = 0.85, lty = c(1,1,2), lwd=2, legend = c("Subjective", "Objective", "Objective inelastic"))
# restore graphical parameters
par(mar = mar_old, cex = cex_old)

mean(abs(fit$predicted_gain - fit$gain) > 110) # 5%
wtd.mean(abs(s$simule_gain - s$gain) > 110, weights = s$weight) # 55%

# Table 3.1: Heterogeneity in bias
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

Table_heterogenous_bias <- stargazer(reg_bias, logit_bias, reg_bias_bis,
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

# False discovery rate at 5%
pvalues_reg_bias <- summary(reg_bias)$coefficients[2:length(summary(reg_bias)$coefficients[,4]),4]
sort(p.adjust(pvalues_reg_bias, method = 'BH'), decreasing=T) 
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
sort(p.adjust(pvalues_reg_bias, method = 'BH'), decreasing=T) 


## 3.2 Environmental effectiveness
decrit(s$taxe_efficace, weights = s$weight, miss = T) # 16.6% vs. 65.9% (17.6% PNR)

# footnote 24 on elasticities
wtd.mean((s$Elasticite_chauffage[s$taxe_efficace=='Non']<= -0.5), weights = s$weight[s$taxe_efficace=='Non'], na.rm=T) # 45%
wtd.mean((s$Elasticite_fuel[s$taxe_efficace=='Non']<= -0.5), weights = s$weight[s$taxe_efficace=='Non'], na.rm=T) # 43%
decrit(s$Elasticite_fuel, weights = s$weight) # -0.45
decrit(s$Elasticite_chauffage, weights = s$weight) # -0.43

## 3.3 Progressivity
decrit(s$progressivite, weights = s$weight) # 19.4% vs. 59.5% (21.1% PNR)


##### 4 How attitudes shape beliefs #####
## 4.1 Self-interest
# Table 4.1: Binomial confidence intervals around share of respondents
decrit(s$gagnant_categorie, weights= s$weight)
decrit(s$simule_gagnant, weights= s$weight)
# Simulated winners (non weighted):
x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Gagnant' & s$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Gagnant',]$weight)
binconf(x = x, n = n) # 78.8%
x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Non affecté' & s$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Non affecté',]$weight)
binconf(x = x, n = n) # 21.6%
x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Perdant' & s$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Perdant',]$weight)
binconf(x = x, n = n) # 12.2%
x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie != 'Non affecté' & s$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie != 'Non affecté',]$weight)
binconf(x = x, n = n) # 26.1%
x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1,]$weight)
binconf(x = x, n = n) # 25.1%

# Simulated losers (non weighted):
x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Gagnant' & s$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Gagnant',]$weight)
binconf(x = x, n = n) # 81.5%
x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Non affecté' & s$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Non affecté',]$weight)
binconf(x = x, n = n) # 44.9%
x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Perdant' & s$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Perdant',]$weight)
binconf(x = x, n = n) # 93.9%
x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie != 'Non affecté' & s$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie != 'Non affecté',]$weight)
binconf(x = x, n = n) # 92.9%
x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0,]$weight)
binconf(x = x, n = n) # 85.7%

wtd.mean(abs(s$simule_gain) > 110, weights = s$weight) # 28%
mean(fit$mistake[fit$gain > 110]) # 1%

# Table 4.2: Heterogeneity in updating
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
                                            c("Controls: socio-demo, politics, estimated gains", "", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")),
                           no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="tab:heterogeneity_update")
write_clip(gsub('\\end{table}', '} {\\footnotesize \\parbox[t]{13.5cm}{\\linespread{1.2}\\selectfont \\textsc{Note:} Omitted variables are \\textit{Unemployed/Inactive} and \\textit{Yellow Vests: opposes}. The list of controls can be found in Appendix \\ref{set_controls}.} }\\end{table}', 
                gsub('\\begin{tabular}{@', '\\resizebox{.90\\columnwidth}{!}{ \\begin{tabular}{@', heterogeneity_update, fixed=TRUE), fixed=TRUE), collapse=' ')


# 4.2 Environmental effectiveness: see Appendix D.4
# No effect of our information on other variables than taxe_efficace
summary(lm((cause_CC=='anthropique') ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight))
summary(lm(as.numeric(effets_CC) ~ apres_modifs + info_CC * info_PM, data=s, subset=!is.missing(effets_CC), weights=s$weight))

# 4.3 Beliefs over progressivity: see Appendix D.4
cor(s$info_progressivite, (s$progressivite!='Non'), use='complete.obs') # -0.006


##### 5 How beliefs determine attitudes #####
## 5.1 Self-interest
# Identification challenge
sum(s$weight[s$simule_gagnant==1])/sum(s$weight) # 76%
sum(s$weight[s$taxe_approbation=='Non' & s$gagnant_categorie!='Gagnant' & s$simule_gagnant==1])/sum(s$weight[s$simule_gagnant==1]) # 62%

# Table 5.1
variables_reg_self_interest <- c("prog_na", "Simule_gain", "Simule_gain2", "taxe_efficace", "single",  "hausse_depenses_par_uc", variables_demo, piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), c(20,70), vector=T)) 
variables_reg_self_interest <- variables_reg_self_interest[!(variables_reg_self_interest %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
# (1) Main identification strategy (sub-sample p10-p60): 53 p.p.***
formula_tsls1_si1 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible*traite_cible_conjoint + cible + I(taxe_approbation=='NSP') + tax_acceptance + ", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si1 <- lm(formula_tsls1_si1, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls1_si1)
s$non_perdant[(s$percentile_revenu <= 60 & s$percentile_revenu >= 10) | (s$percentile_revenu_conjoint <= 60 & s$percentile_revenu_conjoint >= 10)] <- tsls1_si1$fitted.values
formula_tsls2_si1 <- as.formula(paste("taxe_cible_approbation!='Non' ~ ", 
                                      paste(variables_reg_self_interest, collapse = ' + '), "+ cible + I(taxe_approbation=='NSP') + tax_acceptance + non_perdant")) # 
tsls2_si1 <- lm(formula_tsls2_si1, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls2_si1) 

iv_si1 <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", paste(variables_reg_self_interest, collapse=' + '), 
        " + cible + I(taxe_approbation=='NSP') + tax_acceptance + (gagnant_cible_categorie!='Perdant') | . - (gagnant_cible_categorie!='Perdant') + traite_cible*traite_cible_conjoint")), 
        data = s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight), diagnostics = TRUE)

# Alternative specifications for robustness
# (2) Whole sample: 46 p.p.***
formula_tsls1_si2 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ ", paste(variables_reg_self_interest, collapse=' + '), 
                                      " + single + cible + I(taxe_approbation=='NSP') + tax_acceptance + traite_cible*traite_cible_conjoint"))
tsls1_si2 <- lm(formula_tsls1_si2, data=s, weights = s$weight)
summary(tsls1_si2)
s$non_perdant <- tsls1_si2$fitted.values
formula_tsls2_si2 <- as.formula(paste("taxe_cible_approbation!='Non' ~ ", paste(variables_reg_self_interest, collapse=' + '), 
                                      " + I(taxe_approbation=='NSP') + tax_acceptance + single + cible + non_perdant"))
tsls2_si2 <- lm(formula_tsls2_si2, data=s, weights = s$weight)
summary(tsls2_si2)

iv_si2 <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", paste(variables_reg_self_interest, collapse=' + '), 
          " + I(taxe_approbation=='NSP') + tax_acceptance + single + cible + (gagnant_cible_categorie!='Perdant') | . - (gagnant_cible_categorie!='Perdant') + traite_cible*traite_cible_conjoint")), data = s, weights = s$weight), diagnostics = TRUE)

# (3) Simple OLS: 44 p.p. ***
formula_ols_si3  <- as.formula(paste("taxe_cible_approbation!='Non' ~ cible + I(taxe_approbation=='NSP') + ", paste(variables_reg_self_interest, collapse = ' + '), " + non_perdant + tax_acceptance")) # 
s$non_perdant <- as.numeric(s$gagnant_cible_categorie!='Perdant')
ols_si3 <- lm(formula_ols_si3, data=s, weights = s$weight)
summary(ols_si3) # TODO: result has slightly changed, don't know why => update table

# (4) Feedback ,restricted to |simule_gain| < 50: 64 p.p. ***
formula_tsls1_si4 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + ", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si4 <- lm(formula_tsls1_si4, data=s, subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight, na.action='na.exclude')
summary(tsls1_si4)
s$non_perdant[s$variante_taxe_info=='f' & abs(s$simule_gain) < 50] <- tsls1_si4$fitted.values
formula_tsls2_si4 <- as.formula(paste("taxe_feedback_approbation!='Non' ~ (taxe_approbation=='NSP') + ", 
                                      paste(variables_reg_self_interest, collapse = ' + '), " + non_perdant + tax_acceptance"))
tsls2_si4 <- lm(formula_tsls2_si4, data=s, subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight) 
summary(tsls2_si4)

iv_si4 <- summary(ivreg(as.formula(paste("taxe_feedback_approbation!='Non' ~ tax_acceptance + I(taxe_approbation=='NSP') + ", paste(variables_reg_self_interest, collapse = ' + '),
      " + (gagnant_feedback_categorie!='Perdant') | . - (gagnant_feedback_categorie!='Perdant') + simule_gagnant")), data = s, 
      subset=variante_taxe_info=='f' & abs(simule_gain) < 50, weights = s$weight), diagnostics = TRUE)


f_stats_si <- sprintf("%.1f", round(c(iv_si1$diagnostics[1,3], iv_si2$diagnostics[1,3], iv_si4$diagnostics[1,3]), 1))

Table_si2 <- stargazer(tsls2_si1, tsls2_si2, ols_si3, tsls2_si4, 
                    title="Effect of self-interest on acceptance", star.cutoffs = NA, column.labels = c("\\textit{IV: random target/eligibility}", "$OLS$", "\\textit{IV: discontinuity in feedback}"), column.separate = c(2,1,1),
                    dep.var.labels = c("Targeted Dividend ($A^T$)", "After Feedback ($A^F$)"), dep.var.caption = "Acceptance (``Yes'' or ``Don't know'' to policy support)", header = FALSE,
                    covariate.labels = c("Believes does not lose ($G$)", "Initial tax Acceptance ($A^0$)", "",  "Environmentally effective: ``Yes''"),
                    keep = c("non_perdant", "tax_acceptance"), order = c("non_perdant", "tax_acceptance"), omit.table.layout = 'n', 
                    add.lines = list(
                      # "Method: 2SLS & \\checkmark & \\checkmark &  & \\checkmark",
                      c("Controls: Incomes (piecewise continuous)", "\\checkmark ", "\\checkmark  ", "\\checkmark ", "\\checkmark"), # TODO: non-parametric incomes in (2)?
                      c("\\quad estimated gains, socio-demo, other motives ", "", "", "", ""),
                      # c("Controls: Estimated gain ", "\\checkmark", "", "\\checkmark", "\\checkmark"),
                      c("Controls: Policy assigned", "\\checkmark ", "\\checkmark ", "\\checkmark  ", ""),
                      c("Sub-sample", "[p10; p60]", "", "", "$\\left| \\widehat{\\gamma}\\right|<50$"),
                      c("Effective F-Statistic", f_stats_si[1:2], "", f_stats_si[3])),
                    no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="results_private_benefits")
write_clip(gsub('\\end{table}', '} {\\footnotesize \\parbox[t]{\\textwidth}{\\linespread{1.2}\\selectfont \\textsc{Note:} Standard errors are reported in parentheses. The list of controls can be found in Appendix \\ref{set_controls}. The source of exogenous variation in the belief used in first-stages for the targeted dividend is the random assignment of the income threshold, which determines eligibility to the dividend. The first-stage for the non-targeted dividend exploits instead the discontinuity in the win/lose feedback when the net gain switches from negative to positive.} }\\end{table}', 
                    gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_si2, fixed=TRUE), fixed=T), collapse=' ')

Table_si1 <- stargazer(tsls1_si1, tsls1_si2, tsls1_si4, 
                    title="First stage regressions results for self-interest", omit.table.layout = 'n', star.cutoffs = NA,
                    covariate.labels = c("Transfer to respondent ($T_1$)", "Transfer to spouse ($T_2$)",
                                         "$T_1 \\times T_2$", "Simulated winner ($\\widehat{\\Gamma}$)", "Initial tax Acceptance ($A^0$)"),
                    dep.var.labels = c("Targeted Dividend ($G^T$)", "After feedback ($G^F$)"), dep.var.caption = "Believes does not lose", header = FALSE,
                    column.labels = c("(1)", "(2)", "(4)"), model.numbers = FALSE,
                    keep = c("traite", "simule_gagnant", "acceptance"), order = c("traite", "simule_gagnant", "acceptance"),
                    add.lines = list(c("Controls: Incomes (piecewise continuous)", " \\checkmark", " \\checkmark", "\\checkmark"),
                                       c("\\quad estimated gains, socio-demo, other motives ", "", "", ""),
                                  # c("Controls: Estimated gain", "", "", " \\checkmark ", " \\checkmark", " \\checkmark"),
                                  c("Controls: Policy assigned", " \\checkmark", " \\checkmark", " "),
                                  # c("Controls: Socio-demo, other motives", "", "", " \\checkmark", " ", " \\checkmark"),
                                  c("Sub-sample", "[p10; p60]", "", "$\\left| \\widehat{\\gamma}\\right|<50$"),
                                  c("Effective F-Statistic", f_stats_si)),
                    no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="first_stage_private_benefits")
write_clip(gsub('\\end{table}', '} {\\footnotesize \\parbox[t]{\\textwidth}{\\linespread{1.2}\\selectfont \\textsc{Note:} In (1,2), the random eligibility to the dividend (conditionally on income) is used as source of exogenous variation in the belief. In (4), the discontinuity in the win/lose feedback when the net gain switches from negative to positive is used. Column numbers correspond to second stage results, Table \\vref{results_private_benefits}.}} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', 
                                                       Table_si1, fixed=TRUE), fixed=TRUE), collapse=' ')


## 5.2 Environmental effectiveness
# Table 5.2
variables_reg_ee <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "single", "Simule_gain", "Simule_gain2", "gagnant_categorie", variables_demo)
variables_reg_ee <- variables_reg_ee[!(variables_reg_ee %in% c("revenu", "rev_tot", "age", "age_65_plus"))]

# (1) Yes ~ Yes, 2SLS: 42*** p.p.

formula_tsls1_ee1 <- as.formula(paste("taxe_efficace=='Oui' ~", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC"))
tsls1_ee1 <- lm(formula_tsls1_ee1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_ee1)
s$taxe_efficace.hat <- tsls1_ee1$fitted.values
formula_tsls2_ee1 <- as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_reg_ee, collapse = ' + '), " + taxe_efficace.hat"))
tsls2_ee1 <- lm(formula_tsls2_ee1, data=s, weights = s$weight) 
summary(tsls2_ee1)

iv_ee1 <- summary(ivreg(as.formula(paste("taxe_approbation=='Oui' ~ ", paste(variables_reg_ee, collapse = ' + '), "+ (taxe_efficace=='Oui') | ", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC")), data = s), diagnostics = T)

# (2) Yes ~ Yes, OLS: 37*** p.p.
s$taxe_efficace.hat <- as.numeric(s$taxe_efficace=='Oui')
formula_ee2 <- as.formula(paste("(taxe_approbation=='Oui') ~", paste(variables_reg_ee, collapse = ' + '), " + taxe_efficace.hat"))
ols_ee2 <- lm(formula_ee2, data=s, weights = s$weight) 
summary(ols_ee2)

# (3) not No ~ Yes, LIML (cf. Stata)
liml_ee3 <- ivmodelFormula(as.formula(paste("tax_acceptance ~ ", paste(variables_reg_ee, collapse = ' + '), "+ taxe_efficace.hat | ", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC")), data = s)
liml_ee3

# (A4) not No ~ not No, 2SLS: 48** p.p.
formula_tsls1_eea4 <- as.formula(paste("taxe_efficace!='Non' ~", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC"))
tsls1_eea4 <- lm(formula_tsls1_eea4, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_eea4)
s$taxe_efficace.not_no <- tsls1_eea4$fitted.values
formula_tsls2_eea4 <- as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_reg_ee, collapse = ' + '), " + taxe_efficace.not_no"))
tsls2_eea4 <- lm(formula_tsls2_eea4, data=s, weights = s$weight) 
summary(tsls2_eea4)

iv_eea4 <- summary(ivreg(as.formula(paste("(taxe_approbation!='Non') ~ ", paste(variables_reg_ee, collapse = ' + '),  "+ (taxe_efficace!='Non') | ", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC")), data = s), diagnostics = T)

f_stats_ee <- sprintf("%.1f", round(c(iv_ee1$diagnostics[1,3], iv_eea4$diagnostics[1,3]), 1))
liml_ee3$coef <- liml_ee3$sd <- ols_ee2$coefficients
liml_ee3$coef['taxe_efficace.hat'] <- liml_ee3$LIML$point.est
liml_ee3$sd['taxe_efficace.hat'] <- liml_ee3$LIML$std.err

Table_ee2 <- stargazer(tsls2_ee1, ols_ee2, ols_ee2, title="Effect of believing in environmental effectiveness on approval", star.cutoffs = NA, omit.table.layout = 'n',
                       covariate.labels = c("Believes in effectiveness ($\\dot{E}$)"), # Environmental effectiveness: ``Yes''
                       dep.var.labels = c("Approval ($\\dot{A^0}$)", "Acceptance ($A^0$)"), header = FALSE, column.labels = c("$IV$", "$OLS$", "$LIML$"), dep.var.caption = "Initial Tax \\& Dividend",
                       keep = c("efficace"), # "Constant",
                       coef = list(NULL, NULL, liml_ee3$coef),
                       se = list(NULL, NULL, liml_ee3$sd),
                       add.lines = list(c("Instruments: info E.E. \\& C.C. ", "\\checkmark ", "", "\\checkmark "),
                                        c("Controls: Socio-demo, other motives, ", "\\checkmark ", "\\checkmark  ", "\\checkmark "),
                                        c("\\quad incomes, estimated gains", "", "", ""),
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
                       add.lines = list(c("Controls: Socio-demo, other motives,", "\\checkmark ", "\\checkmark "), 
                                        c("\\quad incomes, estimated gains", "", ""),
                                        c("Effective F-Statistic", f_stats_ee)), 
                       no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="first_stage_environmental_effectiveness")
write_clip(gsub('\\end{table}', '} {\\footnotesize \\textsc{Note:} The information randomly displayed about climate change ($Z_{CC}$) and the effectiveness of carbon taxation ($Z_{E}$) are used as sources of exogenous variation in the belief. See discussion in the main text, Section \\vref{subsec:motive_ee}. We chose the set of instruments that maximizes the effective F-statistics. Our specification is well-founded as the Sargan test does not reject the validity of our over-identification restrictions (p-value of 0.93).} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', 
                                                       Table_ee1, fixed=TRUE), fixed=TRUE), collapse=' ')


## 5.3 Progressivity: see Appendix J


##### 6 Conclusion #####
decrit(s$taxe_approbation, weights = s$weight, miss=T) # 70%
decrit(s$simule_gain > s$gain, weights = s$weight) # 89%


##### Appendix A. Raw data #####
# Table A.1
decrit(s$sexe)
decrit(s$age)
decrit(s$csp)
decrit(s$diplome4)
decrit(s$taille_agglo)
decrit(s$region) # TODO: small diff with Occ and PACA

# Table A.2
# for objective data, see python (BdF), preparation.R (ERFS, cf. wtd.mean(db$nb_adultes, db$wprm)) 
#   and for domestic fuel: https://www.lesechos.fr/industrie-services/energie-environnement/le-chauffage-au-fioul-devient-de-plus-en-plus-cher-147372
decrit(s$taille_menage)
decrit(s$nb_adultes)
decrit(s$uc)
decrit(s$chauffage)
decrit(s$surface)
decrit(s$km)
decrit(s$conso) # TODO: 7.18 and not 7.25

# t-tests test for representativeness of sample
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
ttests_quotas <- list()
for (v in variables_strata) {
  for (i in 1:length(fq[[v]]$freq)) ttests_quotas[[v]] <- c(ttests_quotas[[v]], t.test((1*(s[[v]]==fq[[v]]$name[i])), mu=fq[[v]]$freq[i])$p.value) # prop.test yields same results
  names(ttests_quotas[[v]]) <- fq[[v]]$name
}
mean_characs <- ttests_characs <- c('taille_menage'=2.36, 'nb_adultes'=2.03, 'uc'=1.60, 'gaz'=0.42, 'fioul'=0.12, 'surface'=97, 'km'=13735, 'conso'=6.39)
for (c in c('taille_menage', 'nb_adultes', 'uc', 'gaz', 'fioul', 'surface', 'km', 'conso'))  ttests_characs[c] <- t.test(as.numeric(s[[c]]), mu=mean_characs[c])$p.value 

length(which(sort(p.adjust(c(ttests_characs, unlist(ttests_quotas)), method = 'fdr'), decreasing=T)<0.05)) # 12
length(c(ttests_characs, unlist(ttests_quotas))) # 42


##### Appendix C. The use of official household survey data #####
## C.3 Predicting gains and losses
# Table C.1: cf. test_predictions_ols_regression_with_transports.py and regression_feedback.py
# Figure C.1: cf. test_predictions_binary_models.py (and regression_feedback.py)
# Figure C.2
par(mar = c(4.1, 4.1, 1.1, 0.1), cex=1.5)
plot(Ecdf(s$simule_gain - s$gain), type="s", lwd=2, col="red", xlim=c(-100, 400), xlab=expression("Bias: objective minus subjective net gain (in €/year per C.U.)"), main="", ylab=expression("Proportion "<=" x")) + grid() #  \\widehat{\\gamma} - g
lines(density(s$simule_gain - s$gain, bw=30)$x, density(s$simule_gain - s$gain, bw=30)$y/0.004, xlim=c(-100, 400), lwd=2, type='l', col="darkblue")
par(mar = mar_old, cex = cex_old)
decrit(s$simule_gain > s$gain) # 11%
sum(((s$simule_gain - s$gain) > 200)*s$weight) / sum(s$weight) # 23%

## C.4 Distributive effects
# Figure C.3: cf. consistency_belief_losses.py (function compute_effort_rate_decile() defined in standardize_data_bdf_ptc.py)


##### Appendix D. Beliefs and persistence #####
## Table D.1 Elasticities
variables_reg_elast <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "single", "Simule_gain", "Simule_gain2", variables_demo, variables_energie)
variables_reg_elast <- variables_reg_elast[!(variables_reg_elast %in%
    c("revenu", "rev_tot", "age", "age_65_plus", "fioul", "gaz", "hausse_chauffage", "hausse_essence", "hausse_diesel", "hausse_depenses", "simule_gain"))]
elas_c <- lm(taxe_efficace!='Non' ~ Elasticite_chauffage, data=s, subset = variante_partielle=='c', weights = s$weight)
summary(elas_c)
elas_f <- lm(taxe_efficace!='Non' ~ Elasticite_fuel, data=s, subset = variante_partielle=='f', weights = s$weight)
summary(elas_f)
formula_c <- as.formula(paste("taxe_efficace!='Non' ~ Elasticite_chauffage + ", paste(variables_reg_elast, collapse=' + ')))
elast_c_controls <- lm(formula_c, data=s, subset = variante_partielle=='c', weights = s$weight)
summary(elast_c_controls)
formula_f <- as.formula(paste("taxe_efficace!='Non' ~ Elasticite_fuel + ", paste(variables_reg_elast, collapse=' + ')))
elast_f_controls <- lm(formula_f, data=s, subset = variante_partielle=='f', weights = s$weight)
summary(elast_f_controls)

Table_elast <- stargazer(elas_c, elas_f, elast_c_controls, elast_f_controls, 
        title="Effect of subjective elasticities on perceived environmental effectiveness", model.names = FALSE, #star.cutoffs = c(0.1, 1e-5, 1e-30),
        covariate.labels = c("Price elasticity: Housing", "Price elasticity: Transports", "Income","Size of town", "Age","Domestic fuel", "Natural gas", "Diesel"), 
        dep.var.labels = c("Environmental effectiveness: not ``No''"), dep.var.caption = "", header = FALSE,
        keep = c("Elasticite"),
        add.lines = list(c("Controls: Socio-demo, energy", "", "", "\\checkmark  ", "\\checkmark")),
        no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="table:elasticities_effectiveness")
write_clip(gsub('\\end{table}', '} {\\footnotesize \\textsc{Note:} See discussion in the main text, Section \\vref{subsec:perception_ee}.} \\end{table}', gsub('\\begin{tabular}{@', 
                                                       '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_elast, fixed=TRUE), fixed=TRUE), collapse=' ')


## D.2 Self-interest
# Table D.2
decrit(s$gagnant_categorie[s$simule_gagnant==1], weights = s$weight[s$simule_gagnant==1])
decrit(s$gagnant_feedback_categorie[s$simule_gagnant==1], weights = s$weight[s$simule_gagnant==1])
crosstab(s$winning_category[s$simule_gagnant==1], s$winning_feedback_category[s$simule_gagnant==1], 
                                    s$weight[s$simule_gagnant==1], # dnn=c(expression('Winning category,'~bold(Before)~feedback), ''),
                                    prop.r=T, sort=2:1, cex.axis=0.9) # sort=2:1, dir=c("h", "v"), inv.x=T, inv.y=T, color = FALSE # see mosaicplot

# Table D.3
decrit(s$gagnant_categorie[s$simule_gagnant==0], weights = s$weight[s$simule_gagnant==0])
decrit(s$gagnant_feedback_categorie[s$simule_gagnant==0], weights = s$weight[s$simule_gagnant==0])
crosstab(s$winning_category[s$simule_gagnant==0], s$winning_feedback_category[s$simule_gagnant==0], 
                                    s$weight[s$simule_gagnant==0], # dnn=c(expression('Winning category, '~bold(Before)~feedback), ''),
                                    prop.r=T, sort=2:1, cex.axis=0.9) # sort=2:1, dir=c("h", "v"), inv.x=T, inv.y=T, color = FALSE # see mosaicplot

# Figure D.1
ggplot(data=fit, aes(x=gain)) + theme_bw() + stat_smooth(method = "auto", aes(y=predicted_winner, outfit=fitted_proba<<-..y..), se=F) + ylim(c(0,1)) +  # rerun with outfit=fitted_x<<-..x.. to get value at +100. 0.66*fitted_proba[71]+0.34*fitted_proba[70] = 96%
   xlab("Objective gain per consumption unit (density in black)") + ylab("Probability of predicting gain (in blue)") + xlim(c(-250, 200)) + 
   geom_density(aes(y=..scaled..), bw=30) + geom_vline(xintercept=0, col='grey')

# Table D.4
sb <- s[abs(s$simule_gain) > 110,]
decrit(sb$gagnant_categorie, weights= sb$weight)
decrit(sb$simule_gagnant, weights= sb$weight)
# Simulated winners (non weighted):
x = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==1 & sb$gagnant_categorie == 'Gagnant' & sb$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==1 & sb$gagnant_categorie == 'Gagnant',]$weight)
binconf(x = x, n = n) # 77.6%
x = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==1 & sb$gagnant_categorie == 'Non affecté' & sb$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==1 & sb$gagnant_categorie == 'Non affecté',]$weight)
binconf(x = x, n = n) # 20.7%
x = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==1 & sb$gagnant_categorie == 'Perdant' & sb$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==1 & sb$gagnant_categorie == 'Perdant',]$weight)
binconf(x = x, n = n) # 10.8%
x = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==1 & sb$gagnant_categorie != 'Non affecté' & sb$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==1 & sb$gagnant_categorie != 'Non affecté',]$weight)
binconf(x = x, n = n) # 13.1%
x = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==1 & sb$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==1,]$weight)
binconf(x = x, n = n) # 14.3%

# Simulated winners (non weighted):
x = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==0 & sb$gagnant_categorie == 'Gagnant' & sb$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==0 & sb$gagnant_categorie == 'Gagnant',]$weight)
binconf(x = x, n = n) # 78.4%
x = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==0 & sb$gagnant_categorie == 'Non affecté' & sb$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==0 & sb$gagnant_categorie == 'Non affecté',]$weight)
binconf(x = x, n = n) # 32.7%
x = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==0 & sb$gagnant_categorie == 'Perdant' & sb$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==0 & sb$gagnant_categorie == 'Perdant',]$weight)
binconf(x = x, n = n) # 92.2%
x = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==0 & sb$gagnant_categorie != 'Non affecté' & sb$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==0 & sb$gagnant_categorie != 'Non affecté',]$weight)
binconf(x = x, n = n) # 91.1%
x = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==0 & sb$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(sb[sb$variante_taxe_info == 'f' & sb$simule_gagnant==0,]$weight)
binconf(x = x, n = n) # 83.0%


## D.3 Environmental effectiveness: Table XI
# Table D.5 Effect of primings on beliefs about environmental effectiveness
variables_update_ee <- c("Revenu", variables_demo)
variables_update_ee <- variables_update_ee[!(variables_update_ee %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
# (1) OLS without controls
reg_update_ee1 <- lm(taxe_efficace!='Non' ~ apres_modifs + info_CC * info_PM, data=s, weights = s$weight, na.action='na.exclude')
summary(reg_update_ee1)
# (2) OLS with controls
formula_update_ee <- as.formula(paste("taxe_efficace!='Non' ~ apres_modifs + info_CC * info_PM + ",
                                paste(variables_update_ee, collapse = ' + ')))
reg_update_ee2 <- lm(formula_update_ee, data=s, weights = s$weight, na.action='na.exclude')
summary(reg_update_ee2)
# (3) logit with controls
logit_update_ee3 <- glm(formula_update_ee, family = binomial(link='logit'), data=s)
summary(logit_update_ee3)
logit_update_ee3_margins <- logitmfx(formula_update_ee, s, atmean=FALSE)$mfxest
logit_update_ee3_margins
# (4) OLS Yes ~ (instead of not No)
formula_update_ee_bis <- as.formula(paste("taxe_efficace=='Oui' ~ apres_modifs + info_CC * info_PM + ",
                                      paste(variables_update_ee, collapse = ' + ')))
reg_update_ee4 <- lm(formula_update_ee_bis, data=s, weights = s$weight, na.action='na.exclude')
summary(reg_update_ee4)

Table_update_ee <- stargazer(reg_update_ee1, reg_update_ee2, logit_update_ee3, reg_update_ee4,
                             title="Effect of primings on beliefs about environmental effectiveness", # "Diploma: Bachelor or above",
                             covariate.labels = c("Info on Environmental Effectiveness ($Z_{E}$)",
                                                  "Info on Climate Change ($Z_{CC}$)", "Info on Particulate Matter ($Z_{PM}$)", "$Z_{CC} \\times Z_{PM}$"),
                             dep.var.labels = c("not ``No''", "``Yes''"), dep.var.caption = "Environmental effectiveness", header = FALSE,
                             keep = c("info", "apres_modifs"),
                             coef = list(NULL, NULL, logit_update_ee3_margins[,1], NULL),
                             se = list(NULL, NULL, logit_update_ee3_margins[,2], NULL),
                             column.labels = c("(1)", "(2)", "(3)", "(4)"), model.numbers = FALSE,
                             add.lines = list(c("Controls: Socio-demo ", "", "\\checkmark ", "\\checkmark ", "\\checkmark ")),
                             no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:update_ee")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@',
                                                       Table_update_ee, fixed=TRUE), fixed=TRUE), collapse=' ')

## D.4 Progressivity
# Table D.6 
ols_prog_1 <- lm(progressivite!='Non' ~ info_progressivite, data=s, weights=s$weight)
summary(ols_prog_1)
ols_prog_2 <- lm(progressivite!='Non' ~ info_progressivite * biais_sur, data=s, weights=s$weight)
summary(ols_prog_2)
formula_ols_prog_3 <- as.formula(paste("progressivite!='Non' ~  info_progressivite * biais_sur + ", paste(c(variables_demo, variables_politiques), collapse = '+ ')))
ols_prog_3 <- lm(formula_ols_prog_3, data=s, weights=s$weight)
summary(ols_prog_3)

prog <- stargazer(ols_prog_1, ols_prog_2, ols_prog_3, title="Effect of information on perceived progressivity", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                               covariate.labels = c("Constant", "Information on progressivity ($Z_P$)", "Large bias $(\\left|\\widehat{\\gamma}-g\\right|>110)$",
                                                  "Interaction $Z_P \\times (\\left|\\widehat{\\gamma}-g\\right|>110)$"),
                               keep = c("Constant", "info_progressiviteTRUE$", "biais_sur"),
                               dep.var.labels = "Progressivity: not ``No'' ($P$)", dep.var.caption = "", header = FALSE,
                               add.lines = list(c("Controls: Socio-demo, politics ", "", "", "\\checkmark ")),
                               no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="tab:prog")
write_clip(gsub('\\end{table}', ' } {\\footnotesize \\textsc{Note:} See discussion in the main text, Section \\vref{subsec:persistence-prog}.} \\end{table} ', gsub('\\begin{tabular}{@',
                                                         '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', prog, fixed=TRUE), fixed=TRUE), collapse=' ')


##### Appendix E. Estimation of acceptation motives #####
## E.1 Two stage least squares: first stage results
# Table E.1
Table_si1
# Table E.2
Table_ee1
# Note: of Table E.2
summary(ivreg(as.formula(paste("taxe_approbation=='Oui' ~ ", paste(variables_reg_ee, collapse = ' + '), "+ (taxe_efficace=='Oui') | ", paste(variables_reg_ee, collapse = ' + '), 
                               " + apres_modifs * info_CC * info_PM")), data = s), diagnostics=T)
summary(ivreg(as.formula(paste("taxe_approbation=='Oui' ~ ", paste(variables_reg_ee, collapse = ' + '), "+ (taxe_efficace=='Oui') | ", paste(variables_reg_ee, collapse = ' + '), 
                               " + apres_modifs * info_CC")), data = s), diagnostics=T)
summary(ivreg(as.formula(paste("taxe_approbation=='Oui' ~ ", paste(variables_reg_ee, collapse = ' + '), "+ (taxe_efficace=='Oui') | ", paste(variables_reg_ee, collapse = ' + '), 
                               " + apres_modifs + info_CC")), data = s), diagnostics=T)

## E.2 Additional specifications
# Table E.3
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

Table_additional_res <- stargazer(tsls2_sia1, tsls2_sia2, tsls2_sia3, tsls2_sia4, tsls2_sia5, tsls2_sia6,
                                  title="Effect of self-interest on acceptance: second stages of alternative specifications", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                                  covariate.labels = c("Believes wins", "Believes does not lose", "Initial tax Acceptance ($A^0$)"), model.names = FALSE,
                                  dep.var.labels = c("Acceptance", "Approval", "Acceptance", "Approval"), omit.table.layout = 'n', star.cutoffs = NA,
                                  dep.var.caption = c("\\multicolumn{3}{c}{Targeted Dividend ($A^T$)} & \\multicolumn{3}{c}{After Feedback ($A^F$)"), header = FALSE,
                                  keep = c("gagnant", "non_perdant"),
                                  add.lines = list(
                                    c("Controls: Incomes (piecewise continuous)", "\\checkmark ", "\\checkmark  ", "\\checkmark ", "\\checkmark", "\\checkmark ", "\\checkmark"), 
                                    c("\\quad estimated gains, socio-demo, other motives ", "", "", "", ""),
                                    # c("Controls: Estimated gain ", "\\checkmark", "", "\\checkmark", "\\checkmark"),
                                    c("Controls: Policy assigned", "\\checkmark ", "\\checkmark ", "\\checkmark  ", "", "", ""),
                                    c("Sub-sample: [p10; p60] ($A^T$) or $\\left| \\widehat{\\gamma}\\right|<50$ ($A^F$)", "\\checkmark ", "\\checkmark  ", "\\checkmark ", "\\checkmark", "\\checkmark ", "\\checkmark"),
                                    c("Effective F-Statistic", f_stats_sia)),
                                  no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:alternative_si")
write_clip(sub("\\multicolumn{6}{c}{", "", gsub('\\end{table}', '} {\\footnotesize \\parbox[t]{\\textwidth}{\\linespread{1.2}\\selectfont \\textsc{Note:} See results of main specifications, Table \\vref{results_private_benefits}. As in the latter Table, the source of exogenous variation in the belief used in first-stages for the targeted dividend is the random assignment of the income threshold, which determines eligibility to the dividend. The first-stage for the non-targeted dividend exploits instead the discontinuity in the win/lose feedback when the net gain switches from negative to positive.} }.\\end{table}', 
   gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_additional_res, fixed=TRUE), fixed=TRUE), fixed=TRUE), collapse=' ')
# TODO: pb avec cette table (résultats sont bons mais pb avec stargazer)

# Table E.4
# (1) Heterogeneity: interaction with percentile_revenu > 35
formula_tsls1a_sio1 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ 0 + (percentile_revenu > 35) + traite_cible*traite_cible_conjoint*(percentile_revenu > 35) + cible*(percentile_revenu < 35) + I(taxe_approbation=='NSP')*(percentile_revenu < 35) + tax_acceptance*(percentile_revenu < 35) + ", paste(paste(variables_reg_self_interest, "(percentile_revenu < 35)", sep='*'), collapse = ' + ')))
tsls1a_sio1 <- lm(formula_tsls1a_sio1, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
formula_tsls1b_sio1 <- as.formula(paste("((gagnant_cible_categorie!='Perdant')*(percentile_revenu > 35)) ~ 0 + (percentile_revenu > 35) + traite_cible*traite_cible_conjoint*(percentile_revenu > 35) + cible*(percentile_revenu < 35) + I(taxe_approbation=='NSP')*(percentile_revenu < 35) + tax_acceptance*(percentile_revenu < 35) + ", paste(paste(variables_reg_self_interest, "(percentile_revenu < 35)", sep='*'), collapse = ' + ')))
tsls1b_sio1 <- lm(formula_tsls1b_sio1, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
s$non_perdant[(s$percentile_revenu <= 60 & s$percentile_revenu >= 10) | (s$percentile_revenu_conjoint <= 60 & s$percentile_revenu_conjoint >= 10)] <- tsls1a_sio1$fitted.values
s$non_perdant_p35_[(s$percentile_revenu <= 60 & s$percentile_revenu >= 10) | (s$percentile_revenu_conjoint <= 60 & s$percentile_revenu_conjoint >= 10)] <- tsls1b_sio1$fitted.values
formula_tsls2_sio1 <- as.formula(paste("taxe_cible_approbation!='Non' ~ 0 + (percentile_revenu > 35) + ", paste(paste(variables_reg_self_interest, "(percentile_revenu < 35)", sep='*'), collapse = ' + '), "+ cible*(percentile_revenu < 35) + I(taxe_approbation=='NSP')*(percentile_revenu < 35) + tax_acceptance*(percentile_revenu < 35) + non_perdant + non_perdant_p35_")) # 
tsls2_sio1 <- lm(formula_tsls2_sio1, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls2_sio1) 

iv_sio1 <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ 0 + (percentile_revenu > 35) + ", paste(paste(variables_reg_self_interest, "(percentile_revenu < 35)", sep='*'), collapse=' + '), " + cible*(percentile_revenu < 35) + I(taxe_approbation=='NSP')*(percentile_revenu < 35) + tax_acceptance*(percentile_revenu < 35) + (gagnant_cible_categorie!='Perdant')*(percentile_revenu > 35) | 0 + (percentile_revenu > 35) + ", 
        paste(paste(variables_reg_self_interest, "(percentile_revenu < 35)", sep='*'), collapse=' + '), 
        " + cible*(percentile_revenu < 35) + I(taxe_approbation=='NSP')*(percentile_revenu < 35) + tax_acceptance*(percentile_revenu < 35) + traite_cible*traite_cible_conjoint*(percentile_revenu > 35)")), 
        data = s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight), diagnostics = TRUE)
iv_sio1

# (2) Robustness: additional income slope change at 30th percentile
formula_tsls1_sio2 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible*traite_cible_conjoint + cible + I(taxe_approbation=='NSP') + tax_acceptance + ", 
                                      piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), 30), ' + ', paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_sio2 <- lm(formula_tsls1_sio2, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls1_sio2)
s$non_perdant[(s$percentile_revenu <= 60 & s$percentile_revenu >= 10) | (s$percentile_revenu_conjoint <= 60 & s$percentile_revenu_conjoint >= 10)] <- tsls1_sio2$fitted.values
formula_tsls2_sio2 <- as.formula(paste("taxe_cible_approbation!='Non' ~ ", piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), 30), ' + ', 
                                      paste(variables_reg_self_interest, collapse = ' + '), "+ cible + I(taxe_approbation=='NSP') + tax_acceptance + non_perdant")) # 
tsls2_sio2 <- lm(formula_tsls2_sio2, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls2_sio2) 

formula_tsls1_sio2 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible*traite_cible_conjoint + cible + I(taxe_approbation=='NSP') + tax_acceptance + ", 
iv_sio2 <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), 30), ' + ', paste(variables_reg_self_interest, collapse=' + '), 
        " + cible + I(taxe_approbation=='NSP') + tax_acceptance + (gagnant_cible_categorie!='Perdant') | . - (gagnant_cible_categorie!='Perdant') + traite_cible*traite_cible_conjoint")), 
        data = s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight), diagnostics = TRUE)
iv_sio2

# (3) Robustness: additional income slope change at 40th percentile
formula_tsls1_sio3 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible*traite_cible_conjoint + cible + I(taxe_approbation=='NSP') + tax_acceptance + ", 
                                      piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), 40), ' + ', paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_sio3 <- lm(formula_tsls1_sio3, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls1_sio3)
s$non_perdant[(s$percentile_revenu <= 60 & s$percentile_revenu >= 10) | (s$percentile_revenu_conjoint <= 60 & s$percentile_revenu_conjoint >= 10)] <- tsls1_sio3$fitted.values
formula_tsls2_sio3 <- as.formula(paste("taxe_cible_approbation!='Non' ~ ", piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), 40), ' + ', 
                                      paste(variables_reg_self_interest, collapse = ' + '), "+ cible + I(taxe_approbation=='NSP') + tax_acceptance + non_perdant")) # 
tsls2_sio3 <- lm(formula_tsls2_sio3, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls2_sio3) 

iv_sio3 <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), 40), ' + ', paste(variables_reg_self_interest, collapse=' + '), 
        " + cible + I(taxe_approbation=='NSP') + tax_acceptance + (gagnant_cible_categorie!='Perdant') | . - (gagnant_cible_categorie!='Perdant') + traite_cible*traite_cible_conjoint")), 
        data = s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight), diagnostics = TRUE)
iv_sio3

# (4) Robustness: additional income slope change at 50th percentile
formula_tsls1_sio4 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible*traite_cible_conjoint + cible + I(taxe_approbation=='NSP') + tax_acceptance + ", 
                                      piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), 50), ' + ', paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_sio4 <- lm(formula_tsls1_sio4, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls1_sio4)
s$non_perdant[(s$percentile_revenu <= 60 & s$percentile_revenu >= 10) | (s$percentile_revenu_conjoint <= 60 & s$percentile_revenu_conjoint >= 10)] <- tsls1_sio4$fitted.values
formula_tsls2_sio4 <- as.formula(paste("taxe_cible_approbation!='Non' ~ ", piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), 50), ' + ', 
                                      paste(variables_reg_self_interest, collapse = ' + '), "+ cible + I(taxe_approbation=='NSP') + tax_acceptance + non_perdant")) # 
tsls2_sio4 <- lm(formula_tsls2_sio4, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls2_sio4) 

iv_sio4 <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), 50), ' + ', paste(variables_reg_self_interest, collapse=' + '), 
        " + cible + I(taxe_approbation=='NSP') + tax_acceptance + (gagnant_cible_categorie!='Perdant') | . - (gagnant_cible_categorie!='Perdant') + traite_cible*traite_cible_conjoint")), 
        data = s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight), diagnostics = TRUE)
iv_sio4

# (5) Robustness: additional income slope change at 60th percentile
formula_tsls1_sio5 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible*traite_cible_conjoint + cible + I(taxe_approbation=='NSP') + tax_acceptance + ", 
                                      piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), 60), ' + ', paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_sio5 <- lm(formula_tsls1_sio5, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls1_sio5)
s$non_perdant[(s$percentile_revenu <= 60 & s$percentile_revenu >= 10) | (s$percentile_revenu_conjoint <= 60 & s$percentile_revenu_conjoint >= 10)] <- tsls1_sio5$fitted.values
formula_tsls2_sio5 <- as.formula(paste("taxe_cible_approbation!='Non' ~ ", piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), 60), ' + ', 
                                      paste(variables_reg_self_interest, collapse = ' + '), "+ cible + I(taxe_approbation=='NSP') + tax_acceptance + non_perdant")) # 
tsls2_sio5 <- lm(formula_tsls2_sio5, data=s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight)
summary(tsls2_sio5) 

iv_sio5 <- summary(ivreg(as.formula(paste("taxe_cible_approbation!='Non' ~ ", piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), 60), ' + ', paste(variables_reg_self_interest, collapse=' + '), 
        " + cible + I(taxe_approbation=='NSP') + tax_acceptance + (gagnant_cible_categorie!='Perdant') | . - (gagnant_cible_categorie!='Perdant') + traite_cible*traite_cible_conjoint")), 
        data = s, subset = (percentile_revenu <= 60 & percentile_revenu >= 10) | (percentile_revenu_conjoint <= 60 & percentile_revenu_conjoint >= 10), weights = s$weight), diagnostics = TRUE)
iv_sio5

f_stats_sio <- sprintf("%.1f", round(c(iv_sio1$diagnostics[1,3], iv_sio2$diagnostics[1,3], iv_sio3$diagnostics[1,3], iv_sio4$diagnostics[1,3], iv_sio5$diagnostics[1,3]), 1))
Table_sio <- stargazer(tsls2_sio1, tsls2_sio2, tsls2_sio3, tsls2_sio4, tsls2_sio5, 
       title="Effect of self-interest on acceptance: the role of incomes", #star.cutoffs = c(0.1, 1e-5, 1e-30),
       covariate.labels = c("Believes does not lose ($G^T$)", "Income above 35th percentile ($\\un_{I > p35}$)", "$G^T \\times \\un_{I > p35}$", "Initial tax Acceptance ($A^0$)"),
       omit.table.layout = 'n', star.cutoffs = NA, model.names = FALSE, dep.var.labels = "Acceptance of Tax \\& Targeted Dividend ($A^T$)",
       dep.var.caption = "", header = FALSE, 
       keep = c("non_perdant", "^tax_acceptance", "^percentile_revenu > 35TRUE$"), order = c("non_perdant$", "percentile", "p35", "acceptance"),
       add.lines = list(
            c("Percentile with additional income slope change", "", "30", "40", "50", "60"),
            c("Controls: Incomes (piecewise continuous)", "\\checkmark ", "\\checkmark  ", "\\checkmark ", "\\checkmark", "\\checkmark "), 
            c("\\quad estimated gains, socio-demo, other motives ", "", "", "", "", ""),
            c("Sub-sample: [p10; p60]; Controls: Policy assigned", "\\checkmark  ", "\\checkmark ", "\\checkmark", "\\checkmark ", "\\checkmark"),
            c("Effective F-Statistic", f_stats_sio)),
       no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:alternative_sio")
write_clip(gsub('\\end{table}', '} {\\footnotesize \\parbox[t]{\\textwidth}{\\linespread{1.2}\\selectfont \\textsc{Note:} See results of main specifications, Table \\vref{results_private_benefits}. The source of exogenous variation in the belief used in the first-stage is the random assignment of the income threshold, which determines eligibility to the dividend.} }\\end{table}', 
   gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_sio, fixed=TRUE), fixed=TRUE), collapse=' ')


# Table E.5
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

iv_eea2 <- summary(ivreg(as.formula(paste("taxe_approbation!='Non' ~ ", paste(variables_reg_ee, collapse = ' + '), "+ (taxe_efficace=='Oui') | ", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC")), data = s), diagnostics = T)

# (A3) not No ~ Yes, OLS: 37*** p.p.
s$taxe_efficace.hat <- as.numeric(s$taxe_efficace=='Oui')
formula_eea3 <- as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_reg_ee, collapse = ' + '), " + taxe_efficace.hat"))
ols_eea3 <- lm(formula_eea3, data=s, weights = s$weight) 
summary(ols_eea3)

# (A4) not No ~ not No, 2SLS: 48** p.p. /!\ already defined in 5.2
formula_tsls1_eea4 <- as.formula(paste("taxe_efficace!='Non' ~", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC"))
tsls1_eea4 <- lm(formula_tsls1_eea4, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_eea4)
s$taxe_efficace.not_no <- tsls1_eea4$fitted.values
formula_tsls2_eea4 <- as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_reg_ee, collapse = ' + '), " + taxe_efficace.not_no"))
tsls2_eea4 <- lm(formula_tsls2_eea4, data=s, weights = s$weight) 
summary(tsls2_eea4)

iv_eea4 <- summary(ivreg(as.formula(paste("(taxe_approbation!='Non') ~ ", paste(variables_reg_ee, collapse = ' + '),  "+ (taxe_efficace!='Non') | ", paste(variables_reg_ee, collapse = ' + '), " + apres_modifs + info_CC")), data = s), diagnostics = T)

# (A5), not No ~ not No, OLS: 41*** p.p.
s$taxe_efficace.not_no <- as.numeric(s$taxe_efficace!='Non')
formula_eea5 <- as.formula(paste("(taxe_approbation!='Non') ~", paste(variables_reg_ee, collapse = ' + '), " + taxe_efficace.not_no"))
ols_eea5 <- lm(formula_eea5, data=s, weights = s$weight) 
summary(ols_eea5)

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


##### Appendix F. Controls variables #####
variables_demo # Socio-demo
variables_politiques # Politics
decrit(s$Gauche_droite) # Political leaning
variables_energie # Energy # TODO: Simue_gain appears in it but not in our paper
# Incomes: revenu, revenu_conjoint, Revenu2, Revenu_conjoint2, single
piece.formula(c("percentile_revenu", "percentile_revenu_conjoint"), c(20,70), vector=T) # Incomes (piecewise continuous) (excluding "single")
# Estimated gains: simule_gain, Simule_gain2


##### Appendix H. Profile of the Yellow Vests #####
# Table H.1
decrit(s[s$Gauche_droite=='Extreme-left',]$Gilets_jaunes, miss=T, weights=s[s$Gauche_droite=='Extreme-left',]$weight)
decrit(s[s$Gauche_droite=='Left',]$Gilets_jaunes, miss=T, weights=s[s$Gauche_droite=='Left',]$weight)
decrit(s[s$Gauche_droite=='Center',]$Gilets_jaunes, miss=T, weights=s[s$Gauche_droite=='Center',]$weight)
decrit(s[s$Gauche_droite=='Right',]$Gilets_jaunes, miss=T, weights=s[s$Gauche_droite=='Right',]$weight)
decrit(s[s$Gauche_droite=='Extreme-right',]$Gilets_jaunes, miss=T, weights=s[s$Gauche_droite=='Extreme-right',]$weight)
decrit(s[s$Gauche_droite=='Indeterminate',]$Gilets_jaunes, miss=T, weights=s[s$Gauche_droite=='Indeterminate',]$weight)

decrit(s[s$liberal==T,]$Gilets_jaunes, miss=T, weights=s[s$liberal==T,]$weight)
decrit(s[s$conservateur==T,]$Gilets_jaunes, miss=T, weights=s[s$conservateur==T,]$weight)
decrit(s[s$humaniste==T,]$Gilets_jaunes, miss=T, weights=s[s$humaniste==T,]$weight)
decrit(s[s$patriote==T,]$Gilets_jaunes, miss=T, weights=s[s$patriote==T,]$weight)
decrit(s[s$apolitique==T,]$Gilets_jaunes, miss=T, weights=s[s$apolitique==T,]$weight)
decrit(s[s$ecologiste==T,]$Gilets_jaunes, miss=T, weights=s[s$ecologiste==T,]$weight)

decrit(s$taille_agglo, weights=s$weight)
decrit(s[s$taille_agglo=='rural',]$Gilets_jaunes, miss=T, weights=s[s$taille_agglo=='rural',]$weight)
decrit(s[s$taille_agglo=='-20k',]$Gilets_jaunes, miss=T, weights=s[s$taille_agglo=='-20k',]$weight)
decrit(s[s$taille_agglo=='20-100k',]$Gilets_jaunes, miss=T, weights=s[s$taille_agglo=='20-100k',]$weight)
decrit(s[s$taille_agglo=='+100k',]$Gilets_jaunes, miss=T, weights=s[s$taille_agglo=='+100k',]$weight)
decrit(s[s$taille_agglo=='Paris',]$Gilets_jaunes, miss=T, weights=s[s$taille_agglo=='Paris',]$weight)

decrit(s$diplome4, weights=s$weight)
decrit(s[s$diplome4==1,]$Gilets_jaunes, miss=T, weights=s[s$diplome4==1,]$weight)
decrit(s[s$diplome4==2,]$Gilets_jaunes, miss=T, weights=s[s$diplome4==2,]$weight)
decrit(s[s$diplome4==3,]$Gilets_jaunes, miss=T, weights=s[s$diplome4==3,]$weight)
decrit(s[s$diplome4==4,]$Gilets_jaunes, miss=T, weights=s[s$diplome4==4,]$weight)

decrit(s$age, weights=s$weight)
decrit(s[s$age_18_24==T,]$Gilets_jaunes, miss=T, weights=s[s$age_18_24==T,]$weight)
decrit(s[s$age_25_34==T,]$Gilets_jaunes, miss=T, weights=s[s$age_25_34==T,]$weight)
decrit(s[s$age_35_49==T,]$Gilets_jaunes, miss=T, weights=s[s$age_35_49==T,]$weight)
decrit(s[s$age_50_64==T,]$Gilets_jaunes, miss=T, weights=s[s$age_50_64==T,]$weight)
decrit(s[s$age_65_plus==T,]$Gilets_jaunes, miss=T, weights=s[s$age_65_plus==T,]$weight)

for (i in 1:10) {
  cat(i)
  print(decrit(s[s$revenu_decile == i,]$Gilets_jaunes, weights=s[s$revenu_decile == i,]$weight))
}

decrit(s$sexe, weights=s$weight)
decrit(s[s$sexe=='Féminin',]$Gilets_jaunes, miss=T, weights=s[s$sexe=='Féminin',]$weight)
decrit(s[s$sexe=='Masculin',]$Gilets_jaunes, miss=T, weights=s[s$sexe=='Masculin',]$weight)

decrit(s$Gilets_jaunes, weights=s$weight)


##### Appendix I. Descriptive statistics on support #####
decrit(s$taxe_approbation, weights = s$weight, miss=T) # Initial stage
decrit(s$taxe_feedback_approbation, weights = s$weight, miss=T) # After feedback
decrit(s$taxe_cible_approbation[s$cible==20], weights = s$weight[s$cible==20], miss=T) # Targeted dividend: bottom 20%
decrit(s$taxe_cible_approbation[s$cible==30], weights = s$weight[s$cible==30], miss=T) # Targeted dividend: bottom 30%
decrit(s$taxe_cible_approbation[s$cible==40], weights = s$weight[s$cible==40], miss=T) # Targeted dividend: bottom 40%
decrit(s$taxe_cible_approbation[s$cible==50], weights = s$weight[s$cible==50], miss=T) # Targeted dividend: bottom 50%
decrit(s$taxe_cible_approbation, weights = s$weight, miss=T) # Targeted dividend: all


##### Appendix J. Relation between support and belief in progressivity #####
# Table J.1
variables_reg_prog <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "single", "Simule_gain", "Simule_gain2", variables_demo)
variables_reg_prog <- variables_reg_prog[!(variables_reg_prog %in%
    c("revenu", "rev_tot", "age", "age_65_plus", "fioul", "gaz", "hausse_chauffage", "hausse_essence", "hausse_diesel", "hausse_depenses", "simule_gain"))]
s$progressif <- (s$prog_na == 'Oui' | s$prog_na == 'NSP') # Attention à ne pas inclure les NA
s$effective <- s$taxe_efficace!='Non'
s$gagnant_info <- s$gagnant_info_categorie!='Perdant'

# (1) OLS with controls and interactions
formula_ols_prog1 <- as.formula(paste("taxe_info_approbation!='Non' ~ progressif + ", paste(paste(variables_reg_prog, collapse=' + '),
          " + gagnant_info * effective * progressif + (prog_na == 'NA')")))
ols_prog1 <- lm(formula_ols_prog1, weights=s$weight, data=s)
summary(ols_prog1) # sum of all effects True: all :0.879. P+G: 0.727. P+E:0.692. G+E: 0.637

# (2) OLS with controls and all interactions
formula_ols_prog2 <- as.formula(paste("taxe_info_approbation!='Non' ~ progressif + ", paste(paste(variables_reg_prog, collapse=' + '),
   " + gagnant_info * effective * progressif + progressif * Revenu + (prog_na == 'NA') ")))
# No effect from prog*gauche_droite/gilets_jaunes + progressif * gauche_droite + progressif * gilets_jaunes
ols_prog2 <- lm(formula_ols_prog2, weights=s$weight, data=s)
summary(ols_prog2)

# (3) OLS simple: 56 p.p.***
ols_prog3 <- lm(taxe_info_approbation!='Non' ~ progressif + (prog_na == 'NA'), weights=s$weight, data=s)
summary(ols_prog3)

# (4) logit simple
logit_prog4 <- glm(taxe_info_approbation!='Non' ~ progressif + (prog_na == 'NA'), family = binomial(link='logit'), data=s)
summary(logit_prog4)
logit_prog4_margins <- logitmfx(logit_prog4, data=s, atmean=FALSE)$mfxest
logit_prog4_margins

s$progressif <- (s$prog_na == 'Oui')
s$effective <- s$taxe_efficace=='Oui'
s$gagnant_info <- s$gagnant_info_categorie=='Gagnant'

# (5) Strict OLS with controls and all interactions
formula_ols_prog5 <- as.formula(paste("taxe_info_approbation=='Oui' ~ progressif + ", paste(paste(variables_reg_prog, collapse=' + '),
   " + gagnant_info * effective * progressif + progressif * Revenu + (prog_na == 'NA')")))
#  + taxe_approbation: no dramatic difference / + (gagnant_info_categorie!='Perdant') * revenu * progressif: no effect
ols_prog5 <- lm(formula_ols_prog5, weights=s$weight, data=s)
summary(ols_prog5) # sum of all effects True: all :0.935. P+G: 0.599. P+E:0.709. G+E: 0.673

# (6) Strict OLS simple
ols_prog6 <- lm(taxe_info_approbation=='Oui' ~ progressif + (prog_na == 'NA'), weights=s$weight, data=s)
summary(ols_prog6)

Table_prog <- stargazer(ols_prog1, ols_prog2, ols_prog3, logit_prog4, ols_prog5, ols_prog6,
  title="Support of the Tax \\& Dividend in function of beliefs in each motive.",
          covariate.labels = c("Progressivity $(P)$", "Income ($I$, in k\\euro{}/month)", "Winner $(G^1)$", "Effective $(E)$", "$(G^1 \\times E)$",
                               "Interaction: winner $(P \\times G^1)$", "Interaction: effective $(P \\times E)$", "Interaction: income $(P \\times I)$",
                               "$P \\times G^1 \\times E$"), # "Constant",
          dep.var.labels = c("Broad definition of variables (\\textit{not ``No''})", "Strict definitions (\\textit{``Yes''})"),
          dep.var.caption = "Support (after information)", header = FALSE, star.cutoffs = NA, omit.table.layout = 'n',
          keep = c("progressi", "gagnant", 'effective', 'Revenu$'),
          coef = list(NULL, NULL, NULL, logit_prog4_margins[,1], NULL, NULL), perl=T,
          se = list(NULL, NULL, NULL, logit_prog4_margins[,2], NULL, NULL),
          add.lines = list(c("Controls: Socio-demo", "\\checkmark ", "\\checkmark ", " ", "", "\\checkmark ", "")),
          no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:progressivity")
write_clip(gsub('\\end{table}', "} {\\footnotesize \\parbox[t]{\\textwidth}{\\linespread{1.2}\\selectfont \\textsc{Note:} Standard errors are reported in parentheses. For logit, average marginal effects are reported and not coefficients. The list of controls can be found in Appendix \\ref{set_controls}. Covariates and dependent variables refer either to broad (1-4) or strict (5-6) definitions of the beliefs, where strict dummies do not cover ``PNR'' or ``Unaffected'' answers.}} \\end{table} ",
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_prog, fixed=TRUE), fixed=TRUE), collapse=' ')

# Average effect of Progressivity (Not no for acceptance), other things equal: 0.274
0.223 + 0.183 * wtd.mean(s$gagnant_info_categorie!='Perdant', weights = s$weight) + 0.172 * wtd.mean(s$taxe_efficace!='Non', weights = s$weight) -
  0.400 * wtd.mean(s$taxe_efficace!='Non' & s$gagnant_info_categorie!='Perdant', weights = s$weight)

# footnote 31: combined effects on approval
# of winning + progressivity: 0.644
0.228 + 0.303 + 0.098 + (0.126 + 0.281 - 0.314) * wtd.mean(s$taxe_efficace=='Oui', weights = s$weight)
# of effective + progressivity: 0.736
0.228 + 0.244 + 0.281 + (0.098 + 0.126 - 0.314) * wtd.mean(s$gagnant_info_categorie=='Gagnant', weights = s$weight)
# of winning + effective: 0.686
0.303 + 0.244 + 0.126 + (0.098 + 0.281 - 0.314) * wtd.mean(s$progressivite=='Oui', weights = s$weight)
# Of everything: 0.966
0.228 + 0.303 + 0.244 + 0.126 + 0.098 + 0.281 - 0.314
# Results are very close to the cumulative effect of the three motives: 0.903, i.e. accounting for that losers should believe they would lose
0.228 + 0.703 * 0.303 + 0.244 + 0.703 * 0.126 + 0.703 * 0.098 + 0.281 - 0.703 * 0.314


##### Appendix K. Willingness to pay #####
# Figure K.1
wtd.mean(s$uc, weights = s$weight) # 1.6
ggplot() + geom_smooth(data=s[s$taxe_efficace!='Non',], method = "auto", aes(x=gain, y=1*(tax_acceptance), col=" Effective: not ``No''")) + ylim(c(0,1)) +
 xlab("Subjective gain, among non believers in ineffectiveness") + ylab("Acceptance rate") + geom_hline(yintercept=0.5, col='red') + theme_bw() + 
 geom_smooth(data=s, method = "auto", aes(x=gain, y=1*(tax_acceptance), col=' All            ')) + ylim(c(0,1)) + #geom_vline(xintercept=-66, col='red') +
 xlab("Subjective gain") + ylab("Acceptance rate") + geom_hline(yintercept=0.5, col='red') + theme_bw() + theme(legend.position="top", ) + # legend.position="top", 
 scale_color_manual(name="Among:", values=c(" Effective: not `No'"="#000000", ' All            '="#99CCDD"))


##### Appendix L. Ensuring data quality #####
# Speedest screened out
wtd.mean(sa$duree < 7*60, weights = sa$weight) # 4% in original sample
wtd.mean(s$duree < 7*60, weights = s$weight) # 0 in final sample
# Response time uncorrelated with preferences (see also below: Note of Table L.1):
summary(lm(taxe_approbation!='Non' ~ duree, data=s, weights = s$weight))
summary(lm(gagnant_categorie!='Perdant' ~ duree, data=s, weights = s$weight))
summary(lm(gain ~ duree, data=s, weights = s$weight))
# Error at test of quality: screened out
wtd.mean(sa$test_qualite!='Un peu', weights = sa$weight) # 9% in original sample
wtd.mean(s$test_qualite!='Un peu', weights = s$weight) # 0 in final sample
# Bad quality
sum(s$taille_menage > 12) # 10 before we capped it
sum(s$mauvaise_qualite > 0) # 273 mauvaise_qualite: Dummy for an aberrant answer to: revenu, taille_menage, nb_14_et_plus, km, conso, surface or generation_CC
# mauvaise_qualite uncorrelated with preferences (see also below: Note of Table L.1):
summary(lm(taxe_approbation!='Non' ~ mauvaise_qualite, data=s, weights = s$weight))
summary(lm(gagnant_categorie!='Perdant' ~ mauvaise_qualite, data=s, weights = s$weight))
summary(lm(gain ~ mauvaise_qualite, data=s, weights = s$weight))
# For 58 persons with incomes > 10k€/month, see preparation.R, line ~860

# Table L.1
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

# Note of Table L.1
cor(s$duree, s$tax_approval) # -0.01
cor(s$duree, s$tax_acceptance) # -0.0007
cor(s$duree, s$gain) # -0.01
cor(s$mauvaise_qualite, s$tax_acceptance) # .025
cor(s$mauvaise_qualite, s$gain) # -0.01
cor(s$mauvaise_qualite > 0, s$tax_acceptance) # .026
cor(s$mauvaise_qualite > 0, s$gain) # -0.007

# clean heaviest objects
rm(list=ls()[grepl("tsls|ols|logit_|iv_|reg_|iv._", ls()) & !grepl("variables", ls())])
