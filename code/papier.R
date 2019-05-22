## Source to reproduce "Can We Reconcile French People with the Carbon Tax? Disentangling Beliefs from Preferences"
#     by Thomas Douenne & Adrien Fabre, under licence CC-BY
# All files can be found on github: https://github.com/bixiou/beliefs_climate_policies
#     in particular, file to prepare the dataset (preparation.R), to load the packages and functions (packages_functions.R),
#     R environment with all the data prepared (.RData) and python files for complementary computations

source("packages_functions.R")
load(".RData")

##### 2 Data #####
## 2.1 Survey "Beliefs climate policies"
# 2.1.1 Table I: Sample Characteristics
decrit(s$sexe)
decrit(s$age)
decrit(s$csp)
decrit(s$diplome4)
decrit(s$taille_agglo)
decrit(s$region)

# 2.1.2 Table I: Proportion of respondents per target of the payment
decrit(as.numeric(s$cible)) # other lines of the Table are computed in preparation.R

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

## 2.2  French households surveys
# Matching BdF/ENTD : Cf. repository Github openfisca_france_indirect_taxation\build_survey_data\matching_bdf_entd : all files
# Data preparation: cf. prepare_dataset.py
# Computation of net gains: cf. define_tax_incidence_data.py and gain_losses_data.py


##### 3 Perceptions #####
## 3.1 Self-interest
# Over-estimation of policy costs 
# Subjective losses
decrit(s$gain_fuel, weights = s$weight) # mean -61 instead of +18
decrit(s$gain_chauffage, weights = s$weight) # -43 instead of +6
decrit(s$gain, weights = s$weight) # -89 instead of +24
decrit(s$simule_gain > s$gain, weights = s$weight) # 89%
decrit(as.numeric(s$gain) - s$simule_gain, weights = s$weight) # mean -126, median -116
decrit(s$simule_gain_inelastique - s$gain > 0, weights = s$weight) # 77%
# Objective winning category: cf. consistency_belief_losses.py for weighted results reported in comments
decrit(objective_gains$all > 0, weights = objective_gains$weight) # 0.703
decrit(objective_gains$transport > 0) # 0.736
decrit(objective_gains$housing > 0) # 0.6749
# Subjective winning category
decrit(s$gagnant_categorie, weights = s$weight) # 14.0% think they win (21.7% unaffected)
decrit(s$gagnant_fuel_categorie, weights = s$weight) # 15.5% think they win (21.8% unaffected)
decrit(s$gagnant_chauffage_categorie, weights = s$weight) # 17.0% think they win (30.0% unaffected)

# Figure 1: PDF of subjective vs. objective gain
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

# Figure 2: CDF of subjective vs. objective gain (including in the inelastic case)
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

## Relative gain in partial reforms
decrit(s$perte_relative_chauffage, weights = s$weight, miss=T) # -: 15 / =: 21 / +: 54%
decrit(s$perte_relative_fuel, weights = s$weight, miss=T) # 10 / 29 / 56%
decrit(s$perte_relative_tva, weights = s$weight, miss=T) # 2 / 32 / 60%
decrit(s$perte_relative_partielle, weights = s$weight, miss=T)
decrit(s$perte_relative_chauffage[s$fioul == 0 & s$gaz == 0], weights = s$weight[s$fioul == 0 & s$gaz == 0], miss=T)

# TableIII: Heterogeneity in bias
mean(abs(fit$predicted_gain - fit$gain) > 110) # 5%
wtd.mean(abs(s$simule_gain - s$gain) > 110, weights = s$weight) # 55%

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
summary(reg_bias_bis) # TODO: redefine Gilets_jaunes pour que les NAs comptent

Table_heterogenous_bias <- stargazer(reg_bias, logit_bias, reg_bias_bis,#
     title="Determinants of bias in subjective gains", model.names = T, model.numbers = FALSE, #star.cutoffs = c(0.1, 1e-5, 1e-30), # "Diploma: Bachelor or above", 
     covariate.labels = c("Constant", "Initial tax: PNR (I don't know)", "Initial tax: Approves", "Sex: Female", "Ecologist","Consumption Units (C.U.)", 
                          "Yellow Vests: PNR","Yellow Vests: understands","Yellow Vests: supports", "Yellow Vests: is part"),
     dep.var.labels = c("Large bias ($\\left|\\widehat{\\gamma}-g\\right| > 110$)"), dep.var.caption = "", header = FALSE,
     keep = c("Constant", "taxe_approbation", "Gilets_jaunes", "^uc", "Féminin", "ecologiste"),
     coef = list(NULL, logit_bias_margins[,1], NULL),
     se = list(NULL, logit_bias_margins[,2], NULL),
     add.lines = list(c("Controls: Socio-demo, political leaning", "\\checkmark", "\\checkmark", "\\checkmark")),
     no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:bias")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@',
                                                       Table_heterogenous_bias, fixed=TRUE), fixed=TRUE), collapse=' ')

## 3.2 Environmental effectiveness
decrit(s$taxe_efficace, weights = s$weight, miss = T) # 16.6% vs. 65.9%
wtd.mean((s$Elasticite_chauffage[s$taxe_efficace=='Non']<= -0.5), weights = s$weight[s$taxe_efficace=='Non'], na.rm=T) # 45%
wtd.mean((s$Elasticite_fuel[s$taxe_efficace=='Non']<= -0.5), weights = s$weight[s$taxe_efficace=='Non'], na.rm=T) # 43%
decrit(s$Elasticite_fuel, weights = s$weight) # -0.45
decrit(s$Elasticite_chauffage, weights = s$weight) # -0.43
variables_reg_elast <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "(nb_adultes==1)", "Simule_gain", "Simule_gain2", variables_demo, variables_energie)
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
TableXV <- stargazer(elas_c, elas_f, elast_c_controls, elast_f_controls, 
                    title="Effect of subjective elasticities on perceived environmental effectiveness", model.names = FALSE, #star.cutoffs = c(0.1, 1e-5, 1e-30),
                    covariate.labels = c("Price elasticity: Housing", "Price elasticity: Transports", "Income","Size of town", "Age","Domestic fuel", "Natural gas", "Diesel"), 
                    dep.var.labels = c("Environmental effectiveness: not ``No''"), dep.var.caption = "", header = FALSE,
                    keep = c("Elasticite"),
                    add.lines = list(c("Controls: Socio-demographics, energy", "", "", "\\checkmark  ", "\\checkmark")),
                    no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="table:elasticities_effectiveness")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableXV, fixed=TRUE), fixed=TRUE), collapse=' ')

## 3.3 Progressivity
decrit(s$progressivite, weights = s$weight) # 19.4% vs. 59.5%


##### 4 Are beliefs well anchored? #####
## 4.1 Self-interest
x = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==1 & ss$gagnant_feedback_categorie == 'Gagnant',]$weight) + sum(ss[ss$variante_taxe_info == 'f' & 
                                                                      ss$simule_gagnant==0 & ss$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==1,]$weight) + sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==0,]$weight)
binconf(x = x, n = n) # Conservative updating overall: 39%
decrit(s$gagnant_categorie[s$simule_gagnant==1], weights=s$weight[s$simule_gagnant==1]) # 60%
wtd.mean(abs(s$simule_gain) > 110, weights = s$weight) # 28%
mean(fit$mistake[fit$gain > 110]) # 1%
# share aligned with feedback
mar_old <- par()$mar
cex_old <- par()$cex
par(mar = c(0.1, 3.1, 2.1, 0), cex.lab=1.2)
decrit(s$simule_gagnant, weights = s$weight) # objective winning category
# Figure 3 ,Tables XII and XIII: Transition matrix among simulated ...
# (a) winners
decrit(s$gagnant_categorie[s$simule_gagnant==1], weights = s$weight[s$simule_gagnant==1])
decrit(s$gagnant_feedback_categorie[s$simule_gagnant==1], weights = s$weight[s$simule_gagnant==1])
crosstab_simule_gagnant <- crosstab(s$winning_category[s$simule_gagnant==1], s$winning_feedback_category[s$simule_gagnant==1], 
                                    s$weight[s$simule_gagnant==1], # dnn=c(expression('Winning category,'~bold(Before)~feedback), ''),
                                    prop.r=T, sort=2:1, cex.axis=0.9) # sort=2:1, dir=c("h", "v"), inv.x=T, inv.y=T, color = FALSE # see mosaicplot
crosstab_simule_gagnant
# (b) losers
decrit(s$gagnant_categorie[s$simule_gagnant==0], weights = s$weight[s$simule_gagnant==0])
decrit(s$gagnant_feedback_categorie[s$simule_gagnant==0], weights = s$weight[s$simule_gagnant==0])
crosstab_simule_perdant <- crosstab(s$winning_category[s$simule_gagnant==0], s$winning_feedback_category[s$simule_gagnant==0], 
                                    s$weight[s$simule_gagnant==0], # dnn=c(expression('Winning category, '~bold(Before)~feedback), ''),
                                    prop.r=T, sort=2:1, cex.axis=0.9) # sort=2:1, dir=c("h", "v"), inv.x=T, inv.y=T, color = FALSE # see mosaicplot
crosstab_simule_perdant
par(mar = mar_old, cex = cex_old)

## Table IV: Use binomial law to compute confidence intervals around share of respondents
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

## Table XIV: Robustess test on respondents with high gain or loss:
## Use binomial law to compute confidence intervals around share of respondents
ss <- s[abs(s$simule_gain) > 110,]
decrit(ss$gagnant_categorie, weights= ss$weight)
decrit(ss$simule_gagnant, weights= ss$weight)
# Simulated winners (non weighted):
x = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==1 & ss$gagnant_categorie == 'Gagnant' & ss$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==1 & ss$gagnant_categorie == 'Gagnant',]$weight)
binconf(x = x, n = n) # 77.6%
x = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==1 & ss$gagnant_categorie == 'Non affecté' & ss$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==1 & ss$gagnant_categorie == 'Non affecté',]$weight)
binconf(x = x, n = n) # 20.7%
x = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==1 & ss$gagnant_categorie == 'Perdant' & ss$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==1 & ss$gagnant_categorie == 'Perdant',]$weight)
binconf(x = x, n = n) # 10.8%
x = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==1 & ss$gagnant_categorie != 'Non affecté' & ss$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==1 & ss$gagnant_categorie != 'Non affecté',]$weight)
binconf(x = x, n = n) # 13.1%
x = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==1 & ss$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==1,]$weight)
binconf(x = x, n = n) # 14.3%

# Simulated winners (non weighted):
x = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==0 & ss$gagnant_categorie == 'Gagnant' & ss$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==0 & ss$gagnant_categorie == 'Gagnant',]$weight)
binconf(x = x, n = n) # 78.4%
x = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==0 & ss$gagnant_categorie == 'Non affecté' & ss$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==0 & ss$gagnant_categorie == 'Non affecté',]$weight)
binconf(x = x, n = n) # 32.7%
x = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==0 & ss$gagnant_categorie == 'Perdant' & ss$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==0 & ss$gagnant_categorie == 'Perdant',]$weight)
binconf(x = x, n = n) # 92.2%
x = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==0 & ss$gagnant_categorie != 'Non affecté' & ss$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==0 & ss$gagnant_categorie != 'Non affecté',]$weight)
binconf(x = x, n = n) # 91.1%
x = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==0 & ss$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(ss[ss$variante_taxe_info == 'f' & ss$simule_gagnant==0,]$weight)
binconf(x = x, n = n) # 83.0%

# TableV: Determinants of correct revision
# Those who think they win update more correctly when they should update
base_winner <- lm(update_correct ~ gagnant_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight)
summary(base_winner)
s$retraites <- s$statut_emploi == 'retraité·e'
s$actifs <- s$statut_emploi %in% c("autre actif", "CDD", "CDI", "fonctionnaire", "intérimaire ou contrat précaire")
s$etudiants <- s$statut_emploi == 'étudiant·e'
variables_update <- c("revenu", "(gagnant_categorie=='Gagnant')", "Simule_gain", "as.factor(taille_agglo)", "retraites", "actifs", "etudiants", variables_demo, 
                      variables_politiques, "Gilets_jaunes") # 
variables_update <- variables_update[!(variables_update %in% c("revenu", "rev_tot", "age", "age_65_plus", "taille_agglo", "statut_emploi"))]
formula_update <- as.formula(paste("update_correct ~ ", paste(variables_update, collapse=' + ')))
covariates_update_correct <- lm(formula_update, subset = feedback_infirme_large==T, data=s, weights = s$weight)
summary(covariates_update_correct)

variables_update_bis <- c("revenu", "(gagnant_categorie=='Gagnant')", "taxe_approbation", "Simule_gain", "as.factor(taille_agglo)", "retraites", "actifs", "etudiants", 
                          variables_demo, variables_politiques, "Gilets_jaunes") # 
variables_update_bis <- variables_update_bis[!(variables_update_bis %in% c("revenu", "rev_tot", "age", "age_65_plus", "taille_agglo", "statut_emploi"))]
formula_update_bis <- as.formula(paste("update_correct ~ ", paste(variables_update_bis, collapse=' + ')))
covariates_update_correct_bis <- lm(formula_update_bis, subset = feedback_infirme_large==T, data=s, weights = s$weight)
summary(covariates_update_correct_bis)

asymmetric_simple <- stargazer(base_winner, covariates_update_correct, covariates_update_correct_bis,
                               title="Asymmetric updating of winning category", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                               covariate.labels = c("Constant", "Winner, before feedback ($\\dot{G}$)", "Initial tax: PNR (I don't know)", "Initial tax: Approves",
                                                    "Retired", "Active", "Student", "Yellow Vests: PNR", "Yellow Vests: understands", "Yellow Vests: supports", "Yellow Vests: is part"),
                               dep.var.labels = "Correct updating ($U$)", dep.var.caption = "", header = FALSE, 
                               keep = c('Constant', '.*Gagnant.*', 'taxe_approbation', 'retraites', 'actifs', 'etudiants', 'Gilets_jaunes'), 
                               order = c('Constant', '.*Gagnant.*', 'taxe_approbation', 'retraites', 'actifs', 'etudiants', 'Gilets_jaunes'),
                               add.lines = list(c("Among invalidated", "\\checkmark", "\\checkmark", "\\checkmark"), 
                                                c("Includes controls", "", "\\checkmark", "\\checkmark")),
                               no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="asymmetric_simple")
write_clip(gsub('\\end{table}', ' } \\\\ \\quad \\\\ {\\footnotesize \\textsc{Note:} Omitted variables are \\textit{Unemployed/Inactive} and \\textit{Yellow Vests: opposes} }  \\end{table} ', 
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', asymmetric_simple, fixed=TRUE), fixed=TRUE), collapse=' ')

# Some alternative specifications
summary(lm(update_correct_large ~ gagnant_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight))
summary(lm(update_correct ~ gagnant_feedback_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight))
summary(lm(update_correct ~ (gagnant_categorie=='Gagnant') + taxe_approbation + gain + Gauche_droite + sexe + as.factor(age) +
             diplome + region + revenu + I(revenu^2) + revenu_conjoint + I(revenu_conjoint^2) + statut_emploi + csp +
             as.factor(taille_agglo), subset = feedback_infirme_large==T, data=s, weights = s$weight))
summary(lm(update_correct ~ (gagnant_feedback_categorie=='Gagnant') + taxe_approbation + gain + Gauche_droite + sexe + as.factor(age) +
             diplome + region + revenu + I(revenu^2) + revenu_conjoint + I(revenu_conjoint^2) + statut_emploi + csp +
             as.factor(taille_agglo), subset = feedback_infirme_large==T, data=s, weights = s$weight))

# 4.2 Beliefs over environmental effectiveness
# Table XVIII: cf. 5.2
# No effect of our information on other variables than taxe_efficace
summary(lm((cause_CC=='anthropique') ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight))
summary(lm(as.numeric(effets_CC) ~ apres_modifs + info_CC * info_PM, data=s, subset=!is.missing(effets_CC), weights=s$weight))

variables_update_ee <- c("Revenu", variables_demo)
variables_update_ee <- variables_update_ee[!(variables_update_ee %in% c("revenu", "rev_tot", "age", "age_65_plus"))]

reg_update_ee1 <- lm(taxe_efficace!='Non' ~ apres_modifs + info_CC * info_PM, data=s, weights = s$weight, na.action='na.exclude')
summary(reg_update_ee1)

formula_update_ee <- as.formula(paste("taxe_efficace!='Non' ~ apres_modifs + info_CC * info_PM + ", 
                                paste(variables_update_ee, collapse = ' + ')))
reg_update_ee2 <- lm(formula_update_ee, data=s, weights = s$weight, na.action='na.exclude')
summary(reg_update_ee2)

logit_update_ee3 <- glm(formula_update_ee, family = binomial(link='logit'), data=s)
summary(logit_update_ee3)
logit_update_ee3_margins <- logitmfx(formula_update_ee, s, atmean=FALSE)$mfxest
logit_update_ee3_margins

formula_update_ee_bis <- as.formula(paste("taxe_efficace=='Oui' ~ apres_modifs + info_CC * info_PM + ", 
                                      paste(variables_update_ee, collapse = ' + ')))
reg_update_ee4 <- lm(formula_update_ee_bis, data=s, weights = s$weight, na.action='na.exclude')
summary(reg_update_ee4)

Table_update_ee <- stargazer(reg_update_ee1, reg_update_ee2, logit_update_ee3, reg_update_ee4,
                             title="Effect of primings on beliefs about environmental effectiveness",# model.names = T, model.numbers = FALSE, #star.cutoffs = c(0.1, 1e-5, 1e-30), # "Diploma: Bachelor or above", 
                             covariate.labels = c("Info on Environmental Effectiveness ($Z_{E}$)",  
                                                  "Info on Climate Change ($Z_{CC}$)", "Info on Particulate Matter ($Z_{PM}$)", "$Z_{CC} \\times Z_{PM}$"), 
                             dep.var.labels = c("not ``No''", "``Yes''"), dep.var.caption = "Environmental effectiveness", header = FALSE,
                             keep = c("info", "apres_modifs"), 
                             coef = list(NULL, NULL, logit_update_ee3_margins[,1], NULL), 
                             se = list(NULL, NULL, logit_update_ee3_margins[,2], NULL),
                             column.labels = c("(1)", "(2)", "(3)", "(4)"), model.numbers = FALSE,
                             add.lines = list(c("Controls: Socio-demographics ", "", "\\checkmark ", "\\checkmark ", "\\checkmark ")), 
                             no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:update_ee")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@',
                                                       Table_update_ee, fixed=TRUE), fixed=TRUE), collapse=' ')


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
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableXVIII, fixed=TRUE), fixed=TRUE), collapse=' ')


# 4.3 Beliefs over progressivity
s$inactif <- s$statut_emploi %in% c("inactif", "au chômage")
cor(s$info_progressivite, (s$progressivite!='Non'), use='complete.obs') # -0.006
# Table XVI: Effect of information on perceived progressivity
ols_prog_1 <- lm(progressivite!='Non' ~ info_progressivite, data=s, weights=s$weight)
summary(ols_prog_1)
ols_prog_2 <- lm(progressivite!='Non' ~ info_progressivite * biais_sur, data=s, weights=s$weight)
summary(ols_prog_2)
formula_ols_prog_3 <- as.formula(paste("progressivite!='Non' ~  info_progressivite * biais_sur + info_progressivite * revenu + info_progressivite * taille_agglo + 
                   info_progressivite * taille_menage + info_progressivite * age + info_progressivite * Gilets_jaunes + info_progressivite * sexe + info_progressivite * inactif + 
                   info_progressivite * (Diplome>4) + ", paste(c(variables_demo, variables_politiques), collapse = '+ ')))
ols_prog_3 <- lm(formula_ols_prog_3, data=s, weights=s$weight)
summary(ols_prog_3)

prog <- stargazer(ols_prog_1, ols_prog_2, ols_prog_3, title="Effect of information on perceived progressivity", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                               covariate.labels = c("Constant", "Information on progressivity ($Z_P$)", "Large bias $(\\left|\\widehat{\\gamma}-g\\right|>110)$",
                                                  "Interaction $Z_P \\times (\\left|\\widehat{\\gamma}-g\\right|>110)$"),
                               omit = c("Diplome", "sexe", "age", "inactif", "Gilets_jaunes", "Gauche_droite", "liberal", "humaniste", "conservateur",
                                        "patriote", "apolitique", "ecologiste", "interet_politique", "revenu", "taille_menage", "taille_agglo"),             
                               dep.var.labels = "Progressivity: not No ($P$)", dep.var.caption = "", header = FALSE,
                               add.lines = list(c("Controls: Socio-demo, politics ", "", "", "\\checkmark ")),
                               no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="tab:prog")
write_clip(gsub('\\end{table}', ' } \\end{table} ', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', prog, fixed=TRUE), fixed=TRUE), collapse=' ')


##### 5 Motives for acceptance #####
## 5.1 Self-interest
# Identification challenge
sum(s$weight[s$simule_gagnant==1])/sum(s$weight) # 76%
sum(s$weight[s$taxe_approbation=='Non' & s$gagnant_categorie!='Gagnant' & s$simule_gagnant==1])/sum(s$weight[s$simule_gagnant==1]) # 62%

# (1) Main identification strategy
tsls1_si1 <- lm(gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + 
                  I(traite_cible*traite_cible_conjoint) + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + (nb_adultes==1), data=s, weights = s$weight)
summary(tsls1_si1)
s$non_perdant <- tsls1_si1$fitted.values
# 57 p.p.***
tsls2_si1 <- lm(taxe_cible_approbation!='Non' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + (nb_adultes==1), data=s, weights = s$weight)
summary(tsls2_si1)

# Alternative specifications for robustness checks
s$prog_na <- s$progressivite
s$prog_na[is.na(s$prog_na)] <- "NA"
# (2) With many controls 
variables_reg_self_interest <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "prog_na", "taxe_efficace", "(nb_adultes==1)", 
                                 "I(hausse_depenses_interaction/uc)", variables_demo) # 
variables_reg_self_interest <- variables_reg_self_interest[!(variables_reg_self_interest %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
formula_tsls1_si2 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + 
    I(traite_cible*traite_cible_conjoint) + cible + tax_acceptance +  (taxe_approbation=='NSP') + ", paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si2 <- lm(formula_tsls1_si2, data=s, weights = s$weight)
summary(tsls1_si2)
s$non_perdant <- tsls1_si2$fitted.values
# 57 p.p.***
formula_tsls2_si2 <- as.formula(paste("taxe_cible_approbation!='Non' ~ non_perdant + cible + tax_acceptance + I(taxe_approbation=='NSP') + ", 
                                      paste(variables_reg_self_interest, collapse = ' + '))) # 
tsls2_si2 <- lm(formula_tsls2_si2, data=s, weights = s$weight)
summary(tsls2_si2)

# (3) Simple OLS: 44 p.p. ***
formula_ols_si3  <- as.formula(paste("taxe_cible_approbation!='Non' ~ non_perdant + cible + tax_acceptance + I(taxe_approbation=='NSP') + prog_na + taxe_efficace +", 
                                      paste(variables_reg_self_interest, collapse = ' + '))) # 
s$non_perdant <- as.numeric(s$gagnant_cible_categorie!='Perdant')
ols_si3 <- lm(formula_ols_si3, data=s, weights = s$weight)
summary(ols_si3)

# (4) Simple Logit: 43 p.p.***
s$non_perdant <- as.numeric(s$gagnant_cible_categorie!='Perdant')
# Warning when weighting: it relates to number of trials and not to survey weights. 
logit_si4 <- glm(formula_ols_si3, family = binomial(link='logit'), data=s)
summary(logit_si4)
logit_si4_margins <- logitmfx(formula_ols_si3, s, atmean=FALSE)$mfxest
logit_si4_margins

# (5) IV Feedback
tsls1_si5 <- lm(gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + Simule_gain + Simule_gain2, data=s, subset=variante_taxe_info=='f', weights = s$weight)
summary(tsls1_si5)
s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_si5$fitted.values
# 52 p.p.***
tsls2_si5 <- lm(taxe_feedback_approbation!='Non' ~ non_perdant + Simule_gain + Simule_gain2, data=s, subset=variante_taxe_info=='f', weights = s$weight)
summary(tsls2_si5)

# (6) IV Feedback with controls
formula_tsls1_si6 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + Simule_gain + Simule_gain2 + (nb_adultes==1) + 
                                      prog_na + taxe_efficace +", paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si6 <- lm(formula_tsls1_si6, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_si6)
s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_si6$fitted.values
# 43 p.p. ***
formula_tsls2_si6 <- as.formula(paste("taxe_feedback_approbation!='Non' ~ non_perdant + tax_acceptance + (taxe_approbation=='NSP') + Simule_gain + Simule_gain2 + (nb_adultes==1) + 
                                      prog_na + taxe_efficace +", paste(variables_reg_self_interest, collapse = ' + ')))
tsls2_si6 <- lm(formula_tsls2_si6, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
summary(tsls2_si6)

# Results
TableVI <- stargazer(tsls2_si1, tsls2_si2, ols_si3, logit_si4, tsls2_si5, tsls2_si6, # tsls2_si4: Unrecognized object type
                    title="Effect of self-interest on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                    covariate.labels = c("Believes does not lose", "Initial tax Acceptance ($A^I$)", "",  "Environmentally effective: ``Yes''"),
                    dep.var.labels = c("Targeted Acceptance ($A^T$)", "Feedback Acceptance ($A^F$)"), dep.var.caption = "", header = FALSE,
                    keep = c("non_perdant", "tax_acceptance"),
                    coef = list(NULL, NULL, NULL, logit_si4_margins[,1], NULL, NULL), 
                    se = list(NULL, NULL, NULL, logit_si4_margins[,2], NULL, NULL),
                    add.lines = list(
                      # "Method: 2SLS & \\checkmark & \\checkmark &  & \\checkmark",
                      c("Controls: Incomes ", "\\checkmark ", "\\checkmark ", "\\checkmark  ", "\\checkmark ", " ", "\\checkmark"),
                      c("Controls: Estimated gain ", "", "\\checkmark ", "\\checkmark ", "\\checkmark ", "\\checkmark", "\\checkmark"),
                      c("Controls: Target of the tax, single ", "\\checkmark ", "\\checkmark ", "\\checkmark ", "\\checkmark  ", "", ""),
                      c("Controls: Socio-demo, other motives ", "", "\\checkmark ", "\\checkmark ", "\\checkmark  ", "", "\\checkmark  ")),
                    no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="results_private_benefits")
write_clip(sub("\\multicolumn{3}{c}{\\textit{OLS}} & \\textit{logistic} & \\multicolumn{2}{c}{\\textit{OLS}}", 
               "\\multicolumn{2}{c}{\\textit{IV}} & \\textit{OLS} & \\textit{logit} & \\multicolumn{2}{c}{\\textit{IV}}", 
gsub('\\end{table}', '} {\\footnotesize \\\\ \\quad \\\\ \\textsc{Note:} Standard errors are reported in parentheses. 
     For logit, average marginal effects are reported and not coefficients. }\\end{table}', 
                    gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableVI, fixed=TRUE), fixed=TRUE), fixed=T), collapse=' ')

TableXVII <- stargazer(tsls1_si1, tsls1_si2, tsls1_si5, tsls1_si6,
                    title="First stage regressions results for self-interest", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                    covariate.labels = c("Transfer to respondent ($T_1$)", "Transfer to spouse ($T_2$)",
                                         "$T_1 \\times T_2$", "Initial tax Acceptance ($A^I$)", "Simulated winner ($\\widehat{\\Gamma}$)"),
                    dep.var.labels = c("Targeted tax ($G^T$)", "After feedback ($G^F$)"), dep.var.caption = "Believes does not lose", header = FALSE,
                    column.labels = c("(1)", "(2)", "(5)", "(6)"), model.numbers = FALSE,
                    keep = c("traite", "acceptance", "simule_gagnant"),
                    add.lines = list(c("Controls: Incomes", " \\checkmark", " \\checkmark", "", " \\checkmark"),
                                  c("Controls: Estimated gain", "", " \\checkmark ", " \\checkmark", " \\checkmark"),
                                  c("Controls: Target of the tax, single", " \\checkmark", " \\checkmark", " ", " "),
                                  c("Controls: Socio-demo, other motives", "", " \\checkmark", " ", " \\checkmark")),
                    no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "ser"), label="first_stage_private_benefits")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableXVII, fixed=TRUE), fixed=TRUE), collapse=' ')

# Z test (cf. https://stats.stackexchange.com/questions/93540/testing-equality-of-coefficients-from-two-different-regressions)
(0.571-0.517)/(0.092^2+0.170^2)^0.5 # 0.28: not significantly different


## 5.2 Environmental effectiveness
# Main identification strategy
# Alternative specifications for robustness checks
# (1) 2SLS both instruments, with controls: 45 p.p.** We do not control for progressivity: 
#     as most of the people who did not answer the question were in the second half of the survey, 
#     the absence of response is too correlated with our instrument Z_E (apres_modifs) which bias the results.
variables_reg_ee <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "(nb_adultes==1)", "Simule_gain", "Simule_gain2", "gagnant_categorie", variables_demo)
variables_reg_ee <- variables_reg_ee[!(variables_reg_ee %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
formula_ee1 <- as.formula(paste("taxe_efficace!='Non' ~ apres_modifs + info_CC + ", 
                                      paste(variables_reg_ee, collapse = ' + ')))
tsls1_ee1 <- lm(formula_ee1, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_ee1)
s$taxe_efficace.hat <- fitted.values(tsls1_ee1)
formula2_ee1 <- as.formula(paste("tax_acceptance ~ taxe_efficace.hat + ", paste(variables_reg_ee, collapse = ' + ')))
tsls2_ee1 <- lm(formula2_ee1, data=s, weights=s$weight)
summary(tsls2_ee1)

# (2) 2SLS both instruments, no controls: 52 p.p.*
tsls1_ee2 <- lm(taxe_efficace!='Non' ~ apres_modifs + info_CC, data=s, weights=s$weight) # I(info_CC==1 | info_PM==1) + I(info_PM==0 & info_CC==1) + I(info_PM==1 & info_CC==0)
summary(tsls1_ee2)
s$taxe_efficace.hat <- tsls1_ee2$fitted.values
tsls2_ee2 <- lm(tax_acceptance ~ taxe_efficace.hat, data=s, weights=s$weight)
summary(tsls2_ee2)
summary(lm(tax_acceptance ~ info_CC * info_PM, data=s, weights=s$weight)) # info_CC is a good instrument

s$prog_not_no <- (s$prog_na == 'Oui' | s$prog_na == 'NSP')
# (3) OLS with controls: 39 p.p. ***
s$prog_na <- s$progressivite
s$prog_na[is.na(s$progressivite)] <- "NA"
s$taxe_efficace.hat <- as.numeric(s$taxe_efficace!='Non')
formula_ee3 <- as.formula(paste("tax_acceptance ~ taxe_efficace.hat + prog_not_no + (prog_na == 'NA') + ", paste(variables_reg_ee, collapse = ' + '))) # 
ols_ee3 <- lm(formula_ee3, data=s, weights = s$weight)
summary(ols_ee3)

# (4) Logit
# 37 p.p. ***
s$taxe_efficace.hat <- as.numeric(s$taxe_efficace!='Non')
logit_ee4 <- glm(formula_ee3, family = binomial(link='logit'), data=s)
summary(logit_ee4)
logit_ee4_margins <- logitmfx(data=s, formula=logit_ee4, atmean=FALSE)$mfxest
logit_ee4_margins


# (5) IV, with controls and efficace is yes: 47 p.p. *
formula_ee5 <- as.formula(paste("taxe_efficace=='Oui' ~ apres_modifs + info_CC + ", 
                                paste(variables_reg_ee, collapse = ' + ')))
tsls1_ee5 <- lm(formula_ee5, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_ee5)
s$taxe_efficace_yes.hat <- tsls1_ee5$fitted.values
formula2_ee5 <- as.formula(paste("tax_acceptance ~ taxe_efficace_yes.hat + ", paste(variables_reg_ee, collapse = ' + ')))
tsls2_ee5 <- lm(formula2_ee5, data=s, weights=s$weight)
summary(tsls2_ee5)

# (5) IV, no controls and efficace is yes: 56 p.p. *
tsls1_ee5_bis <- lm((taxe_efficace=='Oui') ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight)
summary(tsls1_ee5_bis)
s$taxe_efficace.yes <- tsls1_ee5_bis$fitted.values
tsls2_ee5_bis <- lm(tax_acceptance ~ taxe_efficace.yes, data=s, weights=s$weight)
summary(tsls2_ee5_bis)

# (6) IV, with controls and approval: 42 p.p. **
formula_ee6 <- as.formula(paste("taxe_efficace=='Oui' ~ apres_modifs + info_CC + ", 
                                paste(variables_reg_ee, collapse = ' + ')))
tsls1_ee6 <- lm(formula_ee6, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_ee6)
s$taxe_efficace_yes.hat <- tsls1_ee6$fitted.values
formula2_ee6 <- as.formula(paste("tax_approval ~ taxe_efficace_yes.hat + ", paste(variables_reg_ee, collapse = ' + ')))
tsls2_ee6 <- lm(formula2_ee6, data=s, weights=s$weight)
summary(tsls2_ee6)

# (6 bis) IV, no controls and approval: 42 p.p. **
tsls1_ee6_bis <- lm((taxe_efficace=='Oui') ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight)
summary(tsls1_ee6_bis)
s$taxe_efficace.hat <- tsls1_ee6_bis$fitted.values
tsls2_ee6_bis <- lm(tax_approval ~ taxe_efficace.hat, data=s, weights=s$weight)
summary(tsls2_ee6_bis)

# Results
TableVII <- stargazer(tsls2_ee1, tsls2_ee2, ols_ee3, logit_ee4, tsls2_ee5, tsls2_ee6,
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
      For logit, average marginal effects are reported and not coefficients. }\\end{table}', 
                    gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableVII, fixed=TRUE), fixed=TRUE), fixed=TRUE), collapse=' ')

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
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableXVIII, fixed=TRUE), fixed=TRUE), collapse=' ')

## 5.3 Progressivity
# Identification challenge and strategies
s$prog_na <- s$progressivite
s$prog_na[is.na(s$prog_na)] <- "NA"
s$progressif <- (s$prog_na == 'Oui' | s$prog_na == 'NSP') # Attention à ne pas inclure les NA
s$effective <- s$taxe_efficace!='Non'
s$gagnant_info <- s$gagnant_info_categorie!='Perdant'
variables_reg_prog <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "(nb_adultes==1)", "Simule_gain", "Simule_gain2", variables_demo)
variables_reg_prog <- variables_reg_prog[!(variables_reg_prog %in% 
    c("revenu", "rev_tot", "age", "age_65_plus", "fioul", "gaz", "hausse_chauffage", "hausse_essence", "hausse_diesel", "hausse_depenses", "simule_gain"))]

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

TableVIII <- stargazer(ols_prog1, ols_prog2, ols_prog3, logit_prog4, ols_prog5, ols_prog6,
  title="Effect of beliefs over progressivity on acceptance. Covariates refer either to broad (1-4) or strict (5-6) definitions of the beliefs, 
    where strict dummies do not cover ``PNR'' or ``Unaffected' answers.", 
                            covariate.labels = c("Progressivity $(P)$", "Income ($I$, in k\\euro{}/month)", "Winner $(G^P)$", "Effective $(E)$", "$(G^P \\times E)$",
                                                 "Interaction: winner $(P \\times G^P)$", "Interaction: effective $(P \\times E)$", "Interaction: income $(P \\times I)$", 
                                                 "$P \\times G^P \\times E$"), # "Constant",
                            dep.var.labels = c("Acceptance ($A^P$) on \\textit{not ``No''}", "Approval ($\\dot{A^P}$) on \\textit{``Yes''}"), dep.var.caption = "", header = FALSE,
                            keep = c("progressi", "gagnant", 'effective', 'Revenu$'), # "Constant"
                            coef = list(NULL, NULL, NULL, logit_prog4_margins[,1], NULL, NULL), perl=T,
                            se = list(NULL, NULL, NULL, logit_prog4_margins[,2], NULL, NULL), 
                            add.lines = list(c("Controls: Socio-demographics", "\\checkmark ", "\\checkmark ", " ", "", "\\checkmark ", "")),
                            no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:progressivity")
write_clip(gsub('\\end{table}', '} {\\footnotesize \\\\ \\quad \\\\ \\textsc{Note:} Standard errors are reported in parentheses. 
                For logit, average marginal effects are reported and not coefficients. } \\end{table} ',
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableVIII, fixed=TRUE), fixed=TRUE), collapse=' ')

# Average effect of Progressivity (Not no for acceptance), other things equal: 0.274
0.223 + 0.183 * wtd.mean(s$gagnant_info_categorie!='Perdant', weights = s$weight) + 0.172 * wtd.mean(s$taxe_efficace!='Non', weights = s$weight) - 
  0.400 * wtd.mean(s$taxe_efficace!='Non' & s$gagnant_info_categorie!='Perdant', weights = s$weight)
# of winning: 0.395
0.332 + 0.127 * wtd.mean(s$taxe_efficace!='Non', weights = s$weight) + 0.183 * wtd.mean(s$progressivite!='Non', weights = s$weight) - 
  0.400 * wtd.mean(s$taxe_efficace!='Non' & s$progressivite!='Non', weights = s$weight)
# of effectiveness: 0.313
0.258 + 0.127 * wtd.mean(s$gagnant_info_categorie!='Perdant', weights = s$weight) + 0.172 * wtd.mean(s$progressivite!='Non', weights = s$weight) - 
  0.400 * wtd.mean(s$progressivite!='Non' & s$gagnant_info_categorie!='Perdant', weights = s$weight)

## 5.4 Complementarity between motives
# 5.4.1 Combined effects
# Average effect of Progressivity (Not no for acceptance), other things equal:
# of winning + progressivity: 0.644
0.228 + 0.303 + 0.098 + (0.126 + 0.281 - 0.314) * wtd.mean(s$taxe_efficace=='Oui', weights = s$weight)
# of effective + progressivity: 0.736
0.228 + 0.244 + 0.281 + (0.098 + 0.126 - 0.314) * wtd.mean(s$gagnant_info_categorie=='Gagnant', weights = s$weight)
# of winning + effective: 0.686
0.303 + 0.244 + 0.126 + (0.098 + 0.281 - 0.314) * wtd.mean(s$progressivite=='Oui', weights = s$weight)
# Of everything: 0.966
0.228 + 0.303 + 0.244 + 0.126 + 0.098 + 0.281 - 0.314
# Results are very close to the cumulative effect of the three motives: 0.903
0.228 + 0.703 * 0.303 + 0.244 + 0.703 * 0.126 + 0.703 * 0.098 + 0.281 - 0.703 * 0.314

# 5.4.2 Willingness-to-pay
ggplot() + geom_smooth(data=s[s$taxe_efficace!='Non',], method = "auto", aes(x=gain, y=1*(tax_acceptance), col=" Effective: not `No'")) + ylim(c(0,1)) +
   xlab("Subjective gain, among non believers in ineffectiveness") + ylab("Acceptance rate") + geom_hline(yintercept=0.5, col='red') + theme_bw() + #geom_vline(xintercept=-66, col='red') +
  geom_smooth(data=s, method = "auto", aes(x=gain, y=1*(tax_acceptance), col=' All            ')) + ylim(c(0,1)) +
  xlab("Subjective gain") + ylab("Acceptance rate") + geom_hline(yintercept=0.5, col='red') + theme_bw() + theme(legend.position="top", ) + # legend.position="top", 
  scale_color_manual(name="Among:", values=c(" Effective: not `No'"="#000000", ' All            '="#99CCDD"))


##### Appendix A. Raw data #####
# cf. quotas.xls for objective data (from INSEE)
decrit(s$sexe)
decrit(s$age)
decrit(s$csp)
decrit(s$diplome4)
decrit(s$taille_agglo)
decrit(s$region)

# for objective data, see python (BdF), preparation.R (ERFS, cf. wtd.mean(db$nb_adultes, db$wprm)) 
#   and for domestic fuel: https://www.lesechos.fr/industrie-services/energie-environnement/le-chauffage-au-fioul-devient-de-plus-en-plus-cher-147372
decrit(s$taille_menage)
decrit(s$nb_adultes)
decrit(s$chauffage)
decrit(s$surface)
decrit(s$km)
decrit(s$conso)


##### Appendix B. Estimation for feedback #####
## B.2 Predicting gains and losses
# Table: cf. test_predictions_ols_regression_with_transports.py and regression_feedback.py
# Figure: cf. test_predictions_binary_models.py (and regression_feedback.py)

## B.3 Distributive effects
# Figure: cf. consistency_belief_losses.py (function compute_effort_rate_decile() defined in standardize_data_bdf_ptc.py)


##### Appendix C. Beliefs' persistence #####
## C.1 Self-interest: Tables VII and VIII
ggplot(data=fit, aes(x=gain)) + theme_bw() + geom_smooth(method = "auto", aes(y=predicted_winner), se=F) + ylim(c(0,1)) + 
  xlab("Objective gain per consumption unit (density in black)") + ylab("Probability of predicting gain (in blue)") + xlim(c(-250, 200)) + 
  geom_density(aes(y=..scaled..), bw=30) + geom_vline(xintercept=0, col='grey')
crosstab_simule_gagnant
crosstab_simule_perdant


## C.2 Environmental effectiveness: Table X
TableXV # TODO: renumber Tables


##### Appendix D. Estimation of acceptation motives #####
## D.1 Two stage least squares: first stage results
TableXVII
TableXVIII

## D.2 Additional specifications
# (1) Target: Acceptance ~ win 
iv1_si1 <- lm(gagnant_cible_categorie=='Gagnant' ~ traite_cible + traite_cible_conjoint + 
                  I(traite_cible*traite_cible_conjoint) + cible + Revenu + I(Revenu^2) + Revenu_conjoint + I(Revenu_conjoint^2) + (nb_adultes==1), data=s, weights = s$weight)
summary(iv1_si1)
s$gagnant <- iv1_si1$fitted.values
iv2_si1 <- lm(taxe_cible_approbation!='Non' ~ gagnant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + (nb_adultes==1), data=s, weights = s$weight)
summary(iv2_si1)

# (2) Target: Approval ~ win
iv1_si2 <- lm(gagnant_cible_categorie=='Gagnant' ~ traite_cible + traite_cible_conjoint + 
                  I(traite_cible*traite_cible_conjoint) + cible + Revenu + I(Revenu^2) + Revenu_conjoint + I(Revenu_conjoint^2) + (nb_adultes==1), data=s, weights = s$weight)
summary(iv1_si2)
s$gagnant <- iv1_si2$fitted.values
iv2_si2 <- lm(taxe_cible_approbation=='Oui' ~ gagnant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + (nb_adultes==1), data=s, weights = s$weight)
summary(iv2_si2)

# (3) Target: Approval ~ not lose
iv1_si3 <- lm(gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + 
                  I(traite_cible*traite_cible_conjoint) + cible + Revenu + I(Revenu^2) + Revenu_conjoint + I(Revenu_conjoint^2) + (nb_adultes==1), data=s, weights = s$weight)
summary(iv1_si3)
s$non_perdant <- iv1_si3$fitted.values
iv2_si3 <- lm(taxe_cible_approbation=='Oui' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2 + (nb_adultes==1), data=s, weights = s$weight)
summary(iv2_si3)

# (4) Feedback: Acceptance ~ win
iv1_si4 <- lm(gagnant_feedback_categorie=='Gagnant' ~ simule_gagnant + Simule_gain + Simule_gain2, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(iv1_si4)
s$gagnant[s$variante_taxe_info=='f'] <- iv1_si4$fitted.values
iv2_si4 <- lm(taxe_feedback_approbation!='Non' ~ gagnant + Simule_gain + Simule_gain2, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
summary(iv2_si4)

# (5) Feedback: Approval ~ win
iv1_si5 <- lm(gagnant_feedback_categorie=='Gagnant' ~ simule_gagnant + Simule_gain + Simule_gain2, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(iv1_si5)
s$gagnant[s$variante_taxe_info=='f'] <- iv1_si5$fitted.values
iv2_si5 <- lm(taxe_feedback_approbation=='Oui' ~ gagnant + Simule_gain + Simule_gain2, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
summary(iv2_si5)

# (6) Feedback: Approval ~ not lose
iv1_si6 <- lm(gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + Simule_gain + Simule_gain2, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(iv1_si6)
s$non_perdant[s$variante_taxe_info=='f'] <- iv1_si6$fitted.values
iv2_si6 <- lm(taxe_feedback_approbation=='Oui' ~ non_perdant + Simule_gain + Simule_gain2, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
summary(iv2_si6)

# Results
TableXIX <- stargazer(iv2_si1, iv2_si2, iv2_si3, iv2_si4, iv2_si5, iv2_si6,
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
write_clip(sub("\\multicolumn{6}{c}{", "", sub("er Feedback}}", "er Feedback}", gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', 
                                                                                                                      TableXIX, fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE), collapse=' ')
