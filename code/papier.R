source("packages_functions.R")
load(".RData")

# TODO: check nb_adultes et nb_beneficiaires for simule_gain

##### 2 Data #####
## 2.1 Survey "Beliefs climate policies"
# 2.1.1 Table I: Sample Characteristics
decrit(s$sexe)
decrit(s$age)
decrit(s$csp)
decrit(s$diplome4)
decrit(s$taille_agglo)
decrit(s$region)

# 2.1.2 Table II: Proportion of respondents per target of the payment
decrit(n(s$cible)) # other lines of the Table are computed in other files (provided on demand)

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
s$taille_menage[s$taille_menage > 12] # most look like zipcode, but we capped it since

## 2.2  French households surveys
# cf. python files


##### 3 Perceptions #####
## 3.1 Impact on purchasing power
# Over-estimation of policy costs TODO: correct figures in paper
# Subjective losses
decrit(s$gain_fuel, weights = s$weight) # mean -61 instead of +18
decrit(s$gain_chauffage, weights = s$weight) # -43 instead of +6
decrit(s$gain, weights = s$weight) # -89 instead of +24
# Objective winning category: cf. consistency_belief_losses.py for weighted results
decrit(objective_gains$transport > 0)
decrit(objective_gains$housing > 0)
decrit(objective_gains$all > 0, weights = objective_gains$weight)
# Subjective winning category
decrit(s$gagnant_categorie, weights = s$weight) # 14.0% think they win (21.7% unaffected)
decrit(s$gagnant_fuel_categorie, weights = s$weight) # 15.5% think they win (21.8% unaffected)
decrit(s$gagnant_chauffage_categorie, weights = s$weight) # 17.0% think they win (30.0% unaffected)

# TODO: check ça ordi adrien, mettre dans papier
decrit(s$gagnant_categorie, weights = s$weight) # 64/22/14 +1/0/-1
decrit(s$simule_gagnant, weights = s$weight)
decrit(n(s$gain) - s$simule_gain, weights = s$weight) # mean -126, median -116
decrit(s$simule_gain > s$gain, weights = s$weight) # 89%
decrit(s$simule_gain - s$gain > 50, weights = s$weight) # 75%
decrit(s$simule_gain_inelastique - s$gain > 0, weights = s$weight) # 77%
decrit(s$simule_gain_inelastique - s$gain > 50, weights = s$weight) # 61%
decrit(s$simule_gain_inelastique - n(s$gain), weights = s$weight) # mean 75, median 80

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
plot(Ecdf(s$gain_fuel), type="s", lwd=2, col="red", xlab="", main="", ylab=expression("Proportion "<=" x")) + grid()
lines(cdf_transport$x, cdf_transport$y, lwd=2, col="blue")
lines(cdf_transport_inelastic$x, cdf_transport_inelastic$y, lwd=2, lty=2, col="blue")
abline(v = c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), lty=3, col=rgb(1,0,0,0.7))
axis(3, at=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), tck=0.0, lwd=0, lwd.ticks = 0, padj=1.5, col.axis="red", cex.axis=0.9)
# (b) housing
cdf_housing <- Ecdf(objective_gains$housing)
cdf_housing_inelastic <- Ecdf(objective_gains_inelastic$housing)
plot(Ecdf(s$gain_chauffage), type="s", lwd=2, col="red", xlab="", main="", ylab=expression("Proportion "<=" x")) + grid()
lines(cdf_housing$x, cdf_housing$y, lwd=2, col="blue")
lines(cdf_housing_inelastic$x, cdf_housing_inelastic$y, lwd=2, lty=2, col="blue")
abline(v=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), lty=3, col=rgb(1,0,0,0.7))
axis(3, at=c(-190, -110, -70, -40, -15, 0, 10, 20, 30, 40), tck=0.0, lwd=0, lwd.ticks = 0, padj=1.5, col.axis="red", cex.axis=0.9)
# (c) both combined 
cdf_all <- Ecdf(objective_gains$all)
cdf_all_inelastic <- Ecdf(objective_gains_inelastic$all)
plot(Ecdf(s$gain), type="s", lwd=2, col="red", xlab="", main="", ylab=expression("Proportion "<=" x")) + grid()
lines(cdf_all$x, cdf_all$y, lwd=2, col="blue")
lines(cdf_all_inelastic$x, cdf_all_inelastic$y, lwd=2, lty=2, col="blue")
abline(v=c(-280, -190, -120, -70, -30, 0, 20, 40, 60, 80), lty=3, col=rgb(1,0,0,0.7))
axis(3, at=c(-280, -190, -120, -70, -30, 0, 20, 40, 60, 80), tck=0.0, lwd=0, lwd.ticks = 0, padj=1.5, col.axis="red", cex.axis=0.9)
# restore graphical parameters
par(mar = mar_old, cex = cex_old)

# Heterogeneity in bias
reg_bias <- lm((simule_gain - gain > 50) ~ (sexe=='Féminin') + as.factor(taille_agglo) + (Diplome>=5) + revenu + ecologiste + Gauche_droite + uc + Gilets_jaunes, data=s, weights=s$weight)
summary(reg_bias) # R^2: 0.03 (la moitié due aux gilets jaunes)
logit_bias <- glm((simule_gain - gain > 50) ~ (sexe=='Féminin') + as.factor(taille_agglo) + (Diplome>=5) + revenu + ecologiste + Gauche_droite + uc + Gilets_jaunes, family = binomial(link='logit'), data=s)
summary(logit_bias)
logit_bias_margins <- logitmfx(formula_bias, s, atmean=FALSE)$mfxest
logit_bias_margins # TODO: add logit in Table

Table_heterogenous_bias <- stargazer(reg_bias, logit_bias, #
     title="Determinants of bias in subjective gains", model.names = T, model.numbers = FALSE, #star.cutoffs = c(0.1, 1e-5, 1e-30),
     covariate.labels = c("Constant", "Sex: Female", "Diploma: Bachelor or above", "Ecologist","Consumption Units (C.U.)", "Yellow vests: PNR","Yellow vests: understands","Yellow vests: supports", "Yellow vests: is part"),
     dep.var.labels = c("Estimated bias per C.U. ($\\widehat{\\gamma}-g$) > 50"), dep.var.caption = "", header = FALSE,
     omit = c("Gauche_droite", "taille_agglo", "revenu"),
     coef = list(NULL, logit_bias_margins[,1]), 
     se = list(NULL, logit_bias_margins[,2]),
     add.lines = list(c("Controls: Size of town, political leaning, income", "\\checkmark", "\\checkmark")),
     no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:bias")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_heterogenous_bias, fixed=TRUE), fixed=TRUE), collapse=' ')



## 3.2 Robustness to assumptions on elasticities
# Households perceived elasticities
decrit(s$Elasticite_fuel, weights = s$weight) # -0.43 mean perceived gasoline elasticity of French people
decrit(s$Elasticite_fuel_perso, weights = s$weight * s$depense_carburants) # -0.36 perceived own gasoline elasticity (weighted by share in aggregate spending: only 'mean' is meaningful)
decrit(s$Elasticite_chauffage, weights = s$weight) # -0.41 mean perceived housing elasticity of French people
decrit(s$Elasticite_chauffage_perso, weights = s$weight * s$depense_chauffage) # -0.33 perceived own housing elasticity (weighted by share in aggregate spending: only 'mean' is meaningful)
# 71% (resp. 80%) think they are strictly more contrained than average for fuel (resp. housing)
wtd.mean(s$Elasticite_fuel_perso > s$Elasticite_fuel, weights=s$weight, na.rm = T) # 71%
wtd.mean(s$Elasticite_chauffage_perso > s$Elasticite_chauffage, weights=s$weight, na.rm = T) # 80%
# Objective proportion of HH with higher expenditure increase in transport: 59% / housing: 67%. cf. consistency_belief_losses.py 
# Objective proportion of winners in the totally inelastic case: 53%. cf. consistency_belief_losses.py (after replacing elasticities to 0 in gain_losses_data.py)
wtd.mean(s$Elasticite_fuel_perso > s$Elasticite_fuel + 0.05 * (s$Elasticite_fuel %in% c(-0.22, -0.05)), weights=s$weight, na.rm = T) # 45%
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
wtd.mean((s$Elasticite_fuel_perso - s$Elasticite_fuel + 0.05 * (s$Elasticite_fuel %in% c(-0.22, -0.05)))[!grepl("déjà", s$elasticite_fuel_perso)] > 0, weights=s$weight[!grepl("déjà", s$elasticite_fuel_perso)], na.rm = T) # 64%
wtd.mean((s$Elasticite_chauffage_perso - s$Elasticite_chauffage + 0.05 * (s$Elasticite_chauffage %in% c(-0.22, -0.05)))[!grepl("déjà", s$elasticite_chauffage_perso)] > 0, weights=s$weight[!grepl("déjà", s$elasticite_chauffage_perso)], na.rm = T) # 68%

# Reasons for lack of elasticity (constraint vs absence of consumption)
wtd.mean((s$elasticite_chauffage_perso == '0% - Je n\'en consomme déjà pas') / (s$Elasticite_chauffage_perso == 0), weights=s$weight, na.rm = T) # 61%
wtd.mean((s$elasticite_fuel_perso == '0% - Je suis contraint sur tous mes déplacements') / (s$Elasticite_fuel_perso == 0), weights=s$weight, na.rm = T) # 64%


## 3.3 Perception on other tax’ properties
# Environmental effectiveness: Table IX
decrit(s$taxe_efficace, weights = s$weight, miss = T) # 16.6% vs. 65.9%
variables_reg_elast <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "Simule_gain", "Simule_gain2", variables_demo, variables_energie)
variables_reg_elast <- variables_reg_elast[!(variables_reg_elast %in%
    c("revenu", "rev_tot", "age", "age_65_plus", "fioul", "gaz", "hausse_chauffage", "hausse_essence", "hausse_diesel", "hausse_depenses", "simule_gain"))]
elas_c <- lm(taxe_efficace!='Non' ~ Elasticite_chauffage, data=s, subset = variante_partielle=='c', weights = s$weight)
summary(elas_c)
elas_f <- lm(taxe_efficace!='Non' ~ Elasticite_fuel, data=s, subset = variante_partielle=='f', weights = s$weight)
summary(elas_f)
formula_c <- as.formula(paste("taxe_efficace!='Non' ~ Elasticite_chauffage + ", paste(variables_reg_elast, collapse=' + ')))
elast_c_controls <- lm(formula_c, data=s, subset = variante_partielle=='c', weights = s$weight)
summary(elas_c_controls)
formula_f <- as.formula(paste("taxe_efficace!='Non' ~ Elasticite_fuel + ", paste(variables_reg_elast, collapse=' + ')))
elast_f_controls <- lm(formula_f, data=s, subset = variante_partielle=='f', weights = s$weight)
summary(elas_f_controls)
TableX <- stargazer(elas_c, elas_f, elast_c_controls, elast_f_controls, #  elas_c_controls, elas_f_controls,
                    title="Effect of subjective elasticities on perceived environmental effectiveness", model.names = FALSE, #star.cutoffs = c(0.1, 1e-5, 1e-30),
                    covariate.labels = c("Price elasticity: Housing", "Price elasticity: Transports", "Income","Size of town", "Age","Domestic fuel", "Natural gas", "Diesel"), 
                    dep.var.labels = c("Environmental effectiveness: not `No'"), dep.var.caption = "", header = FALSE,
                    keep = c("Elasticite"),
                    add.lines = list(c("Controls: Socio-demographics, incomes, energy", "", "", "\\checkmark  ", "\\checkmark")),
                    no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="table:elasticities_effectiveness")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableX, fixed=TRUE), fixed=TRUE), collapse=' ')

# Progressivity
decrit(s$progressivite, weights = s$weight) # 19.4% vs. 59.5%


##### 4 Are beliefs well anchored? #####
## 4.1 Self-interest
# Raw results
mar_old <- par()$mar
cex_old <- par()$cex
par(mar = c(0.1, 3.1, 2.1, 0), cex.lab=1.2)
decrit(s$simule_gagnant, weights = s$weight) # objective winning category
# Figure 3 ,Tables VII and VIII: Transition matrix among simulated ...
# (a) winners
decrit(s$gagnant_categorie[s$simule_gagnant==1], weights = s$weight[s$simule_gagnant==1])
decrit(s$gagnant_feedback_categorie[s$simule_gagnant==1], weights = s$weight[s$simule_gagnant==1])
crosstab_simule_gagnant <- crosstab(s$winning_category[s$simule_gagnant==1], s$winning_feedback_category[s$simule_gagnant==1], 
                                    s$weight[s$simule_gagnant==1], # dnn=c(expression('Winning category,'~bold(Before)~feedback), ''),
                                    prop.r=T, sort=2:1, cex.axis=0.9) # sort=2:1, dir=c("h", "v"), inv.x=T, inv.y=T, color = FALSE # see mosaicplot
crosstab_simule_gagnant
plot(crosstab_simule_gagnant, sort=2:1, cex.axis=0.9, ylab = expression('Winning category,'~bold(Before)~feedback), xlab=NA)
mtext(side=3, expression('Winning category,'~bold(After)~feedback), line=0.8, cex = 1.2)
# (b) losers
decrit(s$gagnant_categorie[s$simule_gagnant==0], weights = s$weight[s$simule_gagnant==0])
decrit(s$gagnant_feedback_categorie[s$simule_gagnant==0], weights = s$weight[s$simule_gagnant==0])
crosstab_simule_perdant <- crosstab(s$winning_category[s$simule_gagnant==0], s$winning_feedback_category[s$simule_gagnant==0], 
                                    s$weight[s$simule_gagnant==0], # dnn=c(expression('Winning category,'~bold(Before)~feedback), ''),
                                    prop.r=T, sort=2:1, cex.axis=0.9) # sort=2:1, dir=c("h", "v"), inv.x=T, inv.y=T, color = FALSE # see mosaicplot
plot(crosstab_simule_perdant, sort=2:1, cex.axis=0.9, ylab = expression('Winning category,'~bold(Before)~feedback), xlab=NA)
mtext(side=3, expression('Winning category,'~bold(After)~feedback), line=0.8, cex = 1.2)
par(mar = mar_old, cex = cex_old)

## Use binomial law to compute confidence intervals around share of respondents
decrit(s$gagnant_categorie, weights= s$weight)
decrit(s$simule_gagnant, weights= s$weight)
# Simulés gagnants :
x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Gagnant' & s$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Gagnant',]$weight)
binconf(x = x, n = n)

x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Non affecté' & s$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Non affecté',]$weight)
binconf(x = x, n = n)

x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Perdant' & s$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Perdant',]$weight)
binconf(x = x, n = n)

x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie != 'Non affecté' & s$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie != 'Non affecté',]$weight)
binconf(x = x, n = n)

x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1,]$weight)
binconf(x = x, n = n)

# Simulés perdants :
x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Gagnant' & s$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Gagnant',]$weight)
binconf(x = x, n = n)

x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Non affecté' & s$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Non affecté',]$weight)
binconf(x = x, n = n)

x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Perdant' & s$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Perdant',]$weight)
binconf(x = x, n = n)

x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie != 'Non affecté' & s$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie != 'Non affecté',]$weight)
binconf(x = x, n = n)

x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0,]$weight)
binconf(x = x, n = n)


# Conservative updating
decrit(s$feedback_infirme_large, weights = s$weight) # 70%
decrit(s$update_correct[s$feedback_infirme_large==T], weights = s$weight[s$feedback_infirme_large==T]) # 18%

# Asymmetric updating
sum(s$weight[s$feedback_infirme & s$simule_gagnant==1])/3002 # 46%
sum(s$weight[!is.na(s$update_correct) & s$update_correct==1 & s$feedback_infirme & s$simule_gagnant==1])/sum(s$weight[!is.na(s$update_correct) & s$feedback_infirme & s$simule_gagnant==1]) # 12%
sum(s$weight[s$feedback_infirme & s$simule_gagnant==0])/3002 # 1.6%
sum(s$weight[!is.na(s$update_correct) & s$update_correct==1 & s$feedback_infirme & s$simule_gagnant==0])/sum(s$weight[!is.na(s$update_correct) & s$feedback_infirme & s$simule_gagnant==0]) # 82%
# Les gens qui se croient gagnants updatent plus correctement que les autres lorsqu'ils doivent le faire
# summary(lm(update_correct_large ~ gagnant_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight))
base_winner <- lm(update_correct ~ gagnant_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight)
base_feedback_winner <- lm(update_correct ~ gagnant_feedback_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight) 
controled_winner <- lm(update_correct ~ (gagnant_categorie=='Gagnant') + taxe_approbation + gain + Gauche_droite + sexe + as.factor(age) + 
                         diplome + region + revenu + I(revenu^2) + revenu_conjoint + I(revenu_conjoint^2) + statut_emploi + csp + 
                         as.factor(taille_agglo), subset = feedback_infirme_large==T, data=s, weights = s$weight) 
controled_feedback_winner <- lm(update_correct ~ (gagnant_feedback_categorie=='Gagnant') + taxe_approbation + gain + Gauche_droite + sexe + as.factor(age) + 
                         diplome + region + revenu + I(revenu^2) + revenu_conjoint + I(revenu_conjoint^2) + statut_emploi + csp + 
                         as.factor(taille_agglo), subset = feedback_infirme_large==T, data=s, weights = s$weight)

# Determinants of correct revision
s$retraites <- s$statut_emploi == 'retraité·e'
s$actifs <- s$statut_emploi %in% c("autre actif", "CDD", "CDI", "fonctionnaire", "intérimaire ou contrat précaire")
s$etudiants <- s$statut_emploi == 'étudiant·e'
variables_update <- c("niveau_vie", "(gagnant_categorie=='Gagnant')", "Simule_gain", "as.factor(taille_agglo)", "retraites", "actifs", "etudiants", variables_demo, variables_politiques, "Gilets_jaunes", "score_ges") # 
variables_update <- variables_update[!(variables_update %in% c("revenu", "rev_tot", "age", "age_65_plus", "taille_agglo", "statut_emploi"))]
formula_update <- as.formula(paste("update_correct ~ ", paste(variables_update, collapse=' + ')))
covariates_update_correct <- lm(formula_update, subset = feedback_infirme_large==T, data=s, weights = s$weight)
summary(covariates_update_correct)

asymmetric_simple <- stargazer(base_winner, controled_winner, base_feedback_winner, controled_feedback_winner, covariates_update_correct,
          title="Asymmetric updating of winning category", #star.cutoffs = c(0.1, 1e-5, 1e-30),
          covariate.labels = c("Constant", "Winner, before feedback ($\\dot{G}$)", "Winner, after feedback ($\\dot{G}^F$)",
                               "Retired", "Active", "Student", "Yellow vests: PNR", "Yellow vests: understands", "Yellow vests: supports", "Yellow vests: is part"),
          dep.var.labels = "Correct updating ($U$)", dep.var.caption = "", header = FALSE, 
          keep = c('Constant', '.*Gagnant.*', 'retraites', 'actifs', 'etudiants', 'Gilets_jaunes'), 
          order = c('Constant', '.*Gagnant.*', 'retraites', 'actifs', 'etudiants', 'Gilets_jaunes'),
          add.lines = list(c("Among invalidated", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"), 
                             c("Includes controls", "", "\\checkmark", "", "\\checkmark", "\\checkmark")),
          no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="asymmetric_simple")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', asymmetric_simple, fixed=TRUE), fixed=TRUE), collapse=' ')


##### 5 Motives for acceptance #####
## 5.1 Self-interest
# Identification challenge
sum(s$weight[s$simule_gagnant==1])/sum(s$weight) # 76%
sum(s$weight[s$taxe_approbation=='Non' & s$gagnant_categorie!='Gagnant' & s$simule_gagnant==1])/sum(s$weight[s$simule_gagnant==1]) # 62%
# sum(s$weight[s$taxe_approbation=='Non' & s$gagnant_categorie!='Gagnant'])/sum(s$weight) # 66%
# sum(s$weight[s$taxe_approbation=='Non' & s$gagnant_categorie=='Perdant'])/sum(s$weight) # 55%

# (1) Main identification strategy
tsls1_si1 <- lm(gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + 
                  I(traite_cible*traite_cible_conjoint) + cible + Revenu + I(Revenu^2) + Revenu_conjoint + I(Revenu_conjoint^2), data=s, weights = s$weight)
summary(tsls1_si1)
s$non_perdant <- tsls1_si1$fitted.values
# 50 p.p.***
tsls2_si1 <- lm(taxe_cible_approbation!='Non' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2, data=s, weights = s$weight)
summary(tsls2_si1)

# Alternative specifications for robustness checks
# (2) With many controls 
variables_reg_self_interest <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "I(hausse_depenses_interaction/uc)", "taxe_efficace", variables_demo, variables_politiques) # 
variables_reg_self_interest <- variables_reg_self_interest[!(variables_reg_self_interest %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
formula_tsls1_si2 <- as.formula(paste("gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + 
    I(traite_cible*traite_cible_conjoint) + cible + tax_acceptance +  (taxe_approbation=='NSP') +", paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si2 <- lm(formula_tsls1_si2, data=s, weights = s$weight)
summary(tsls1_si2)
s$non_perdant <- tsls1_si2$fitted.values
# 52 p.p.***
formula_tsls2_si2 <- as.formula(paste("taxe_cible_approbation!='Non' ~ non_perdant + cible + tax_acceptance + I(taxe_approbation=='NSP') + ", 
                                      paste(variables_reg_self_interest, collapse = ' + '))) # 
tsls2_si2 <- lm(formula_tsls2_si2, data=s, weights = s$weight)
summary(tsls2_si2)

# (3) Simple OLS (same results and same distinction as before for 'bis' or not)
s$non_perdant <- n(s$gagnant_cible_categorie!='Perdant')
ols_si3 <- lm(formula_tsls2_si2, data=s, weights = s$weight)
summary(ols_si3)

# (4) Simple Logit: 53 p.p.***
s$non_perdant <- n(s$gagnant_cible_categorie!='Perdant')
# Warning when weighting: it relates to number of trials and not to survey weights. 
# TODO: use svyglm to weight correctly cf. https://stats.stackexchange.com/questions/57107/use-of-weights-in-svyglm-vs-glm
logit_si4 <- glm(formula_tsls2_si2, family = binomial(link='logit'), data=s)
summary(logit_si4)
logit_si4_margins <- logitmfx(formula_tsls2_si2, s, atmean=FALSE)$mfxest
logit_si4_margins

# (5) IV Feedback
formula_tsls1_si5 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + Simule_gain + Simule_gain2 + taxe_efficace +", 
                                         paste(variables_reg_self_interest, collapse = ' + ')))
tsls1_si5 <- lm(formula_tsls1_si5, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(tsls1_si5)
s$non_perdant[s$variante_taxe_info=='f'] <- tsls1_si5$fitted.values
# 43 p.p. ***
formula_tsls2_si5 <- as.formula(paste("taxe_feedback_approbation!='Non' ~ non_perdant + tax_acceptance + (taxe_approbation=='NSP') + Simule_gain + Simule_gain2 + taxe_efficace +", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
tsls2_si5 <- lm(formula_tsls2_si5, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
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
               gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableV, fixed=TRUE), fixed=TRUE), fixed=T), collapse=' ')

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
tsls1_ee1 <- lm(taxe_efficace!='Non' ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight) # I(info_CC==1 | info_PM==1) + I(info_PM==0 & info_CC==1) + I(info_PM==1 & info_CC==0)
summary(tsls1_ee1)
s$taxe_efficace.hat <- tsls1_ee1$fitted.values
tsls2_ee1 <- lm(tax_acceptance ~ taxe_efficace.hat, data=s, weights=s$weight)
summary(tsls2_ee1)
summary(lm(tax_acceptance ~ info_CC * info_PM, data=s, weights=s$weight)) # info_CC is a good instrument

# Alternative specifications for robustness checks
# (2) 2SLS both instruments, with controls: 56 p.p.*
variables_reg_ee <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "Simule_gain", "Simule_gain2", "gagnant_categorie", variables_demo)
variables_reg_ee <- variables_reg_ee[!(variables_reg_ee %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
formula_ee2 <- as.formula(paste("taxe_efficace!='Non' ~ apres_modifs + info_CC * info_PM + ", 
                                      paste(variables_reg_ee, collapse = ' + ')))
tsls1_ee2 <- lm(formula_ee2, data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1_ee2)
s$taxe_efficace.hat <- fitted.values(tsls1_ee2)
formula2_ee2 <- as.formula(paste("tax_acceptance ~ taxe_efficace.hat + ",paste(variables_reg_ee, collapse = ' + ')))
tsls2_ee2 <- lm(formula2_ee2, data=s, weights=s$weight)
summary(tsls2_ee2)

# (3) OLS with controls:
# 42 p.p.
s$taxe_efficace.hat <- n(s$taxe_efficace!='Non')
formula_ee3 <- as.formula(paste("tax_acceptance ~ taxe_efficace.hat + ", paste(variables_reg_ee, collapse = ' + '))) # 
ols_ee3 <- lm(formula_ee3, data=s, weights = s$weight)
summary(ols_ee3)

# (4) Logit
# 46 p.p.
s$taxe_efficace.hat <- n(s$taxe_efficace!='Non')
logit_ee4 <- glm(formula_ee3, family = binomial(link='logit'), data=s)
summary(logit_ee4)
logit_ee4_margins <- logitmfx(data=s, formula=logit_ee4, atmean=FALSE)$mfxest
logit_ee4_margins

# (5) IV, no controls and efficace is yes:
# 56 p.p.
tsls1_ee5 <- lm((taxe_efficace=='Oui') ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight)
summary(tsls1_ee5)
s$taxe_efficace.yes <- tsls1_ee5$fitted.values
tsls2_ee5 <- lm(tax_acceptance ~ taxe_efficace.yes, data=s, weights=s$weight)
summary(tsls2_ee5)

# (6) IV, no controls and approval:
tsls1_ee6 <- lm((taxe_efficace!='Non') ~ apres_modifs + info_CC * info_PM, data=s, weights=s$weight)
summary(tsls1_ee6)
s$taxe_efficace.hat <- tsls1_ee6$fitted.values
tsls2_ee6 <- lm(tax_approval ~ taxe_efficace.hat, data=s, weights=s$weight)
summary(tsls2_ee6)

# Results
TableVI <- stargazer(tsls2_ee1, tsls2_ee2, ols_ee3, logit_ee4, tsls2_ee5, tsls2_ee6,
                     title="Effect of believing in environmental effectiveness on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                     covariate.labels = c("Environmental effectiveness: not `No'", "Environmental effectiveness: `Yes'"), # "Constant",
                     dep.var.labels = c("Tax Acceptance ($A^I$)", "Tax Approval ($\\dot{A^I}$)"), dep.var.caption = "", header = FALSE,
                     keep = c("efficace"), # "Constant",
                     coef = list(NULL, NULL, NULL, logit_ee4_margins[,1], NULL, NULL), 
                     se = list(NULL, NULL, NULL, logit_ee4_margins[,2], NULL, NULL),
                     add.lines = list(c("Instruments: info E.E., C.C. \& P.M. ", "\\checkmark ", "\\checkmark ", "", " ", "\\checkmark ", "\\checkmark"),
                       c("Controls: Socio-demographics ", "", "\\checkmark ", "\\checkmark  ", "\\checkmark ", "", "")), 
                     no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:ee")
write_clip(sub("\\multicolumn{3}{c}{\\textit{OLS}} & \\textit{logistic} & \\textit{OLS} & \\textit{OLS}", 
               "\\textit{IV} & \\textit{IV} & \\textit{OLS} & \\textit{logit} & \\textit{IV} & \\textit{IV}", 
               gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableVI, fixed=TRUE), fixed=TRUE), fixed=TRUE), collapse=' ')

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


## 5.3 Progressivity
# Identification challenge and strategies

variables_reg_prog <- c("Revenu", "Revenu2", "Revenu_conjoint", "Revenu_conjoint2", "Simule_gain", "Simule_gain2", "gagnant_categorie", variables_demo, variables_energie)
variables_reg_prog <- variables_reg_prog[!(variables_reg_prog %in% 
    c("revenu", "rev_tot", "age", "age_65_plus", "fioul", "gaz", "hausse_chauffage", "hausse_essence", "hausse_diesel", "hausse_depenses", "simule_gain"))]

s$progressif <- s$progressivite!='Non'
# (1) OLS with controls: 27 p.p.*** (same result with 101 control variables)
formula_ols_prog1 <- as.formula(paste("tax_acceptance ~ progressif + ", paste(variables_reg_prog, collapse=' + ')))
ols_prog1 <- lm(formula_ols_prog1, weights=s$weight, data=s)
summary(ols_prog1)

# (2) OLS simple: 38 p.p.***
ols_prog2 <- lm(tax_acceptance ~ progressif, weights=s$weight, data=s)
summary(ols_prog2)

# (3) Logit with controls: 25 p.p. ***
formula_prog3 <- as.formula(paste("tax_acceptance ~ progressif + ", paste(variables_reg_prog, collapse=' + '))) # 
logit_prog3 <- glm(formula_prog3, family = binomial(link='logit'), data=s[s$region!='autre',])
summary(logit_prog3)
logit_prog3_margins <- logitmfx(logit_prog3, data=s[s$region!='autre',], atmean=FALSE)
logit_prog3_margins
# logit_prog3_margins <- summary(margins(data=s[s$region!='autre',], model=logit_prog3, variables = c("progressif")))
# logit_prog3_margins

# (4) OLS with controls: progressivite is yes
formula_ols_prog4 <- as.formula(paste("tax_acceptance ~ (progressivite == 'Oui') + ", paste(variables_reg_prog, collapse=' + ')))
ols_prog4 <- lm(formula_ols_prog4, weights=s$weight, data=s)
summary(ols_prog4)

# (5) OLS with controls: approve
formula_ols_prog5 <- as.formula(paste("tax_approval ~ progressif + ", paste(variables_reg_prog, collapse=' + ')))
ols_prog5 <- lm(formula_ols_prog5, weights=s$weight, data=s)
summary(ols_prog5)

# (6) OLS with controls: progressivite is yes / approve
formula_ols_prog6 <- as.formula(paste("tax_approval ~ (progressivite == 'Oui') + ", paste(variables_reg_prog, collapse=' + ')))
ols_prog6 <- lm(formula_ols_prog6, weights=s$weight, data=s)
summary(ols_prog6)

TableVII <- stargazer(ols_prog1, ols_prog2, logit_prog3, ols_prog4, ols_prog5, ols_prog6,
                            title="Effect of beliefs over progressivity on acceptance", #star.cutoffs = c(0.1, 1e-5, 1e-30),
                            covariate.labels = c("Progressivity: not `No'", "Progressivity: `Yes'"), # "Constant",
                            dep.var.labels = c("Tax Acceptance ($A^I$)", "Tax Approval ($\\dot{A^I}$)"), dep.var.caption = "", header = FALSE,
                            keep = c("progressi"), # "Constant", 
                            coef = list(NULL, NULL, logit_prog3_margins$mfxest[,1], NULL, NULL, NULL),
                            se = list(NULL, NULL, logit_prog3_margins$mfxest[,2], NULL, NULL, NULL),
                            add.lines = list(c("Controls: Socio-demo, energy ", "\\checkmark ", " ", "\\checkmark ", "\\checkmark ", "\\checkmark ", "\\checkmark")), 
                            no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:progressivity")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', TableVII, fixed=TRUE), fixed=TRUE), collapse=' ')


##### Appendix A. Raw data #####
## A.1 Perception of net gains
## A.2 Estimation for feedback
# A.2.2: cf. test_predictions_ols_regression_with_transports.py and regression_feedback.py
# A.2.3: cf. test_predictions_binary_models.py (and regression_feedback.py)

## A.3 Distributive effects
# TODO: reference to python file


##### Appendix B. Perceptions #####
## B.1 Self-interest: Tables VII and VIII
crosstab_simule_gagnant
crosstab_simule_perdant

## B.2 Environmental effectiveness: Table X
TableX 

##### Apendix C. Estimation pf acceptation motives #####
## C.1 Two stage least squares: first stage results
TableXI
TableXII

## C.2 Additional specifications
# (1) Target: Acceptance ~ win 
iv1_si1 <- lm(gagnant_cible_categorie=='Gagnant' ~ traite_cible + traite_cible_conjoint + 
                  I(traite_cible*traite_cible_conjoint) + cible + Revenu + I(Revenu^2) + Revenu_conjoint + I(Revenu_conjoint^2), data=s, weights = s$weight)
summary(iv1_si1)
s$gagnant <- iv1_si1$fitted.values
# 50 p.p.***
iv2_si1 <- lm(taxe_cible_approbation!='Non' ~ gagnant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2, data=s, weights = s$weight)
summary(iv2_si1)

# (2) Target: Approval ~ win
iv1_si2 <- lm(gagnant_cible_categorie=='Gagnant' ~ traite_cible + traite_cible_conjoint + 
                  I(traite_cible*traite_cible_conjoint) + cible + Revenu + I(Revenu^2) + Revenu_conjoint + I(Revenu_conjoint^2), data=s, weights = s$weight)
summary(iv1_si2)
s$gagnant <- iv1_si2$fitted.values
# 50 p.p.***
iv2_si2 <- lm(taxe_cible_approbation=='Oui' ~ gagnant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2, data=s, weights = s$weight)
summary(iv2_si2)

# (3) Target: Approval ~ not lose
iv1_si3 <- lm(gagnant_cible_categorie!='Perdant' ~ traite_cible + traite_cible_conjoint + 
                  I(traite_cible*traite_cible_conjoint) + cible + Revenu + I(Revenu^2) + Revenu_conjoint + I(Revenu_conjoint^2), data=s, weights = s$weight)
summary(iv1_si3)
s$non_perdant <- iv1_si3$fitted.values
# 50 p.p.***
iv2_si3 <- lm(taxe_cible_approbation=='Oui' ~ non_perdant + cible + Revenu + Revenu2 + Revenu_conjoint + Revenu_conjoint2, data=s, weights = s$weight)
summary(iv2_si3)

# (4) Feedback: Acceptance ~ win
formula_iv1_si4 <- as.formula(paste("gagnant_feedback_categorie=='Gagnant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + Simule_gain + Simule_gain2 + taxe_efficace +", 
                                         paste(variables_reg_self_interest, collapse = ' + ')))
iv1_si4 <- lm(formula_iv1_si5, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(iv1_si4)
s$gagnant[s$variante_taxe_info=='f'] <- iv1_si4$fitted.values
# 43 p.p. ***
formula_iv2_si4 <- as.formula(paste("taxe_feedback_approbation!='Non' ~ gagnant + tax_acceptance + (taxe_approbation=='NSP') + Simule_gain + Simule_gain2 + taxe_efficace +", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
iv2_si4 <- lm(formula_iv2_si4, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
summary(iv2_si4)

# (5) Feedback: Approval ~ win
formula_iv1_si5 <- as.formula(paste("gagnant_feedback_categorie=='Gagnant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + Simule_gain + Simule_gain2 + taxe_efficace +", 
                                         paste(variables_reg_self_interest, collapse = ' + ')))
iv1_si5 <- lm(formula_iv1_si5, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(iv1_si5)
s$gagnant[s$variante_taxe_info=='f'] <- iv1_si5$fitted.values
# 43 p.p. ***
formula_iv2_si5 <- as.formula(paste("taxe_feedback_approbation=='Oui' ~ gagnant + tax_acceptance + (taxe_approbation=='NSP') + Simule_gain + Simule_gain2 + taxe_efficace +", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
iv2_si5 <- lm(formula_iv2_si5, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
summary(iv2_si5)

# (6) Feedback: Approval ~ not lose
formula_iv1_si6 <- as.formula(paste("gagnant_feedback_categorie!='Perdant' ~ simule_gagnant + tax_acceptance + (taxe_approbation=='NSP') + Simule_gain + Simule_gain2 + taxe_efficace +", 
                                         paste(variables_reg_self_interest, collapse = ' + ')))
iv1_si6 <- lm(formula_iv1_si6, data=s, subset=variante_taxe_info=='f', weights = s$weight, na.action='na.exclude')
summary(iv1_si6)
s$non_perdant[s$variante_taxe_info=='f'] <- iv1_si6$fitted.values
# 43 p.p. ***
formula_iv2_si6 <- as.formula(paste("taxe_feedback_approbation=='Oui' ~ non_perdant + tax_acceptance + (taxe_approbation=='NSP') + Simule_gain + Simule_gain2 + taxe_efficace +", 
                                      paste(variables_reg_self_interest, collapse = ' + ')))
iv2_si6 <- lm(formula_iv2_si6, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f'])
summary(iv2_si6)

# Results
TableXIV <- stargazer(iv2_si1, iv2_si2, iv2_si3, iv2_si4, iv2_si5, iv2_si6,
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
                  TableXIV, fixed=TRUE), fixed=TRUE), fixed=TRUE), fixed=TRUE), collapse=' ')
