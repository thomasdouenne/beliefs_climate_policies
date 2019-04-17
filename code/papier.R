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
sum(s$taille_menage > 12) # 10
sum(s$mauvaise_qualite > 0) # 250 mauvaise_qualite: Dummy for an aberrant answer to: revenu, taille_menage, nb_14_et_plus, km, surface or generation_CC
# mauvaise_qualite uncorrelated with preferences
summary(lm(taxe_approbation!='Non' ~ mauvaise_qualite, data=s, weights = s$weight))
summary(lm(gagnant_categorie!='Perdant' ~ mauvaise_qualite, data=s, weights = s$weight))
summary(lm(gain ~ mauvaise_qualite, data=s, weights = s$weight))
s$taille_menage[s$taille_menage > 12] # most look like zipcode

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
decrit(objective_gains$all > 0)
# Subjective winning category
decrit(s$gagnant_categorie, weights = s$weight) # 14.0% think they win (21.7% unaffected)
decrit(s$gagnant_fuel_categorie, weights = s$weight) # 15.5% think they win (21.8% unaffected)
decrit(s$gagnant_chauffage_categorie, weights = s$weight) # 17.0% think they win (30.0% unaffected)

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


## 3.2 Robustness to assumptions on elasticities
# Households perceived elasticities
decrit(s$Elasticite_fuel, weights = s$weight) # -0.43 mean perceived gasoline elasticity of French people
decrit(s$Elasticite_fuel_perso, weights = s$weight * s$depense_carburants) # -0.41 perceived own gasoline elasticity (weighted by share in aggregate spending: only 'mean' is meaningful)
decrit(s$Elasticite_chauffage, weights = s$weight) # -0.41 mean perceived housing elasticity of French people
decrit(s$Elasticite_chauffage_perso, weights = s$weight * s$depense_chauffage) # -0.33 perceived own housing elasticity (weighted by share in aggregate spending: only 'mean' is meaningful)
# 71% (resp. 80%) think they are strictly more contrained than average for fuel (resp. housing)
wtd.mean(s$Elasticite_fuel_perso > s$Elasticite_fuel, weights=s$weight, na.rm = T) # 71%
wtd.mean(s$Elasticite_chauffage_perso > s$Elasticite_chauffage, weights=s$weight, na.rm = T) # 80%
# Objective proportion of HH with higher expenditure increase in transport: 59% / housing: 67%. cf. consistency_belief_losses.py 
# Objective proportion of winners in the totally inelastic case: 53%. cf. consistency_belief_losses.py (after replacing elasticities to 0 in gain_losses_data.py)


## 3.3 Perception on other tax’ properties
# Environmental effectiveness: Table IX
decrit(s$taxe_efficace, weights = s$weight, miss = T) # 16.6% vs. 65.9%
elas_c <- lm(taxe_efficace!='Non' ~ Elasticite_chauffage, data=s, subset = variante_partielle=='c', weights = s$weight)
summary(elas_c)
elas_f <- lm(taxe_efficace!='Non' ~ Elasticite_fuel, data=s, subset = variante_partielle=='f', weights = s$weight)
summary(elas_f)
elas_c_controls <- lm(taxe_efficace!='Non' ~ Elasticite_chauffage + revenu + taille_agglo + age + fioul + gaz + diesel, data=s, subset = variante_partielle=='c', weights = s$weight)
summary(elas_c_controls)
elas_f_controls <- lm(taxe_efficace!='Non' ~ Elasticite_fuel + revenu + taille_agglo + age + fioul + gaz + diesel, data=s, subset = variante_partielle=='f', weights = s$weight)
summary(elas_f_controls)
TableIX <- stargazer(elas_c, elas_f, elas_c_controls, elas_f_controls, type="latex",
          dep.var.labels=c("Environmental effectiveness"),
          covariate.labels=c("Price elasticity housing", "Price elsticity transports", "Income","Size urban unit",
                             "Age","Domestic fuel", "Natural gas", "Diesel"))
# TODO: use Elasticite_partielle ? cf. ci-dessous
# summary(lm(taxe_efficace!='Non' ~ Elasticite_partielle, data=s, weights = s$weight))
# summary(lm(taxe_efficace!='Non' ~ Elasticite_partielle + revenu + taille_agglo + age + fioul + gaz + diesel, data=s, weights = s$weight))
# summary(lm(taxe_efficace!='Non' ~ Elasticite_partielle * variante_partielle, data=s, weights = s$weight))
# summary(lm(taxe_efficace!='Non' ~ Elasticite_partielle * variante_partielle + revenu + taille_agglo + age + fioul + gaz + diesel, data=s, weights = s$weight))

# Progressivity
decrit(s$progressivite, weights = s$weight) # 19.4% vs. 59.5%


##### 4. Are beliefs well anchored? #####
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

# Conservative updating
decrit(s$feedback_infirme_large, weights = s$weight) # 70%
decrit(s$update_correct[s$feedback_infirme_large==T], weights = s$weight[s$feedback_infirme_large==T]) # 18%

# Asymmetric updating
sum(s$weight[s$feedback_infirme & s$simule_gagnant==1])/3002 # 46%
sum(s$weight[!is.na(s$update_correct) & s$update_correct==1 & s$feedback_infirme & s$simule_gagnant==1])/sum(s$weight[!is.na(s$update_correct) & s$feedback_infirme & s$simule_gagnant==1]) # 12%
sum(s$weight[s$feedback_infirme & s$simule_gagnant==0])/3002 # 1.6%
sum(s$weight[!is.na(s$update_correct) & s$update_correct==1 & s$feedback_infirme & s$simule_gagnant==0])/sum(s$weight[!is.na(s$update_correct) & s$feedback_infirme & s$simule_gagnant==0]) # 82%
# 3.3 Les gens qui se croient gagnants updatent plus correctement que les autres lorsqu'ils doivent le faire
# summary(lm(update_correct_large ~ gagnant_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight))
base_winner <- lm(update_correct ~ gagnant_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight)
base_feedback_winner <- lm(update_correct ~ gagnant_feedback_categorie=='Gagnant', subset = feedback_infirme_large==T, data=s, weights = s$weight) 
controled_winner <- lm(update_correct ~ (gagnant_categorie=='Gagnant') + taxe_approbation + gain + Gauche_droite + sexe + as.factor(age) + 
                         diplome + region + revenu + I(revenu^2) + revenu_conjoint + I(revenu_conjoint^2) + statut_emploi + csp + 
                         as.factor(taille_agglo), subset = feedback_infirme_large==T, data=s, weights = s$weight) 
controled_feedback_winner <- lm(update_correct ~ (gagnant_feedback_categorie=='Gagnant') + taxe_approbation + gain + Gauche_droite + sexe + as.factor(age) + 
                         diplome + region + revenu + I(revenu^2) + revenu_conjoint + I(revenu_conjoint^2) + statut_emploi + csp + 
                         as.factor(taille_agglo), subset = feedback_infirme_large==T, data=s, weights = s$weight)
asymmetric_simple <- stargazer(base_winner, controled_winner, base_feedback_winner, controled_feedback_winner,
          title="Asymmetric updating of winning category", #star.cutoffs = c(0.1, 1e-5, 1e-30),
          covariate.labels = c("Constant", "Winner, before feedback ($\\dot{G}$)", "Winner, after feedback ($\\dot{G}^F$)"),
          dep.var.labels = "Correct updating ($U$)", dep.var.caption = "", header = FALSE, keep = c('Constant', '.*Gagnant.*'), 
          add.lines = c("Among invalidated & \\checkmark & \\checkmark & \\checkmark & \\checkmark", "Includes controls & & \\checkmark & & \\checkmark & "),
          no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser"), label="asymmetric_simple")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', asymmetric_simple, fixed=TRUE), fixed=TRUE), collapse=' ')


##### 5. Motives for acceptance #####
## 5.1 Self-interest
# Identification challenge
sum(s$weight[s$simule_gagnant==1])/sum(s$weight) # 76%
sum(s$weight[s$taxe_approbation=='Non' & s$gagnant_categorie!='Gagnant' & s$simule_gagnant==1])/sum(s$weight[s$simule_gagnant==1]) # 62%
# sum(s$weight[s$taxe_approbation=='Non' & s$gagnant_categorie!='Gagnant'])/sum(s$weight) # 66%
# sum(s$weight[s$taxe_approbation=='Non' & s$gagnant_categorie=='Perdant'])/sum(s$weight) # 55%
# Main identification strategy

# Alternative specifications for robustness checks

# Results
TableV # TODO

## 5.2 Environmental effectiveness
# Main identification strategy
# Alternative specifications for robustness checks
# Results
TableVI # TODO

## 5.3 Progressivity
# Identification challenge and strategies


### Appendix A. Raw data
## A.1 Perception of net gains
## A.2 Estimation for feedback
## A.3 Distributive effects
# TODO: reference to python file


### Appendix B. Perceptions
## B.1 Self-interest: Tables VII and VIII
crosstab_simule_gagnant
crosstab_simule_perdant

## B.2 Environmental effectiveness: Table IX
TableIX


### Apendix C. Estimation acceptation motives
## C.1 Two stage least squares: first stage results
TableX # TODO 1st stage self-interest

## C.2 Additional specifications
TableXI # TODO 1st stage environmental effectiveness