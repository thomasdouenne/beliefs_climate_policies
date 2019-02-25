require(rddtools)
require(rddapp)

s$dummy_approbation_cible <- 1*(s$taxe_cible_approbation=='Oui')

### Seuil 20 ###
s_20 <- subset(s, cible20==1 & revenu < 1140)
data_rdd_20 <- rdd_data(y=s_20$dummy_approbation_cible, x=s_20$revenu, cutpoint=780)
bandwidthsize_20 <- rdd_bw_ik(data_rdd_20)
model_rdd_20 = rdd_reg_np(rdd_object = data_rdd_20, bw = bandwidthsize_20)
summary(model_rdd_20)
plot(model_rdd_20)

### Seuil 30 ###
s_30 <- subset(s, cible30==1 & revenu < 1430)
data_rdd_30 <- rdd_data(y=s_30$dummy_approbation_cible, x=s_30$revenu, cutpoint=1140)
bandwidthsize_30 <- rdd_bw_ik(data_rdd_30)
model_rdd_30 = rdd_reg_np(rdd_object = data_rdd_30, bw = bandwidthsize_30)
summary(model_rdd_30)
plot(model_rdd_30)

### Seuil 40 ###
s_40 <- subset(s, cible40==1 & revenu < 1670)
data_rdd_40 <- rdd_data(y=s_40$dummy_approbation_cible, x=s_40$revenu, cutpoint=1430)
bandwidthsize_40 <- rdd_bw_ik(data_rdd_40)
model_rdd_40 = rdd_reg_np(rdd_object = data_rdd_40, bw = bandwidthsize_40)
summary(model_rdd_40)
plot(model_rdd_40)


### Seuil 50 ###
s_50 <- subset(s, cible50==1 & revenu < 2250)
data_rdd_50 <- rdd_data(y=s_50$dummy_approbation_cible, x=s_50$revenu, cutpoint=1670)
bandwidthsize_50 <- rdd_bw_ik(data_rdd_50)
model_rdd_50 = rdd_reg_np(rdd_object = data_rdd_50, bw = bandwidthsize_50)
summary(model_rdd_50)
plot(model_rdd_50)

### Multivariate RDD ###
cible <- s$cible
revenu <- s$revenu
revenu_conjoint <- s$revenu_conjoint
surface <- s$score_climate_call # This is just here as an example of control variable
dummy_approbation_cible <- s$dummy_approbation_cible

model_2_variables_20 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | surface, cutpoint = c(780, 780), method = "center", subset = cible==20 & revenu<1140 & revenu_conjoint != 0, t.design = c("leq", "geq"))
summary(model_2_variables_20)

model_2_variables_30 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | surface, cutpoint = c(1140, 1140), method = "center", subset = cible==30 & revenu<1430 & revenu_conjoint != 0, t.design = c("leq", "geq"))
summary(model_2_variables_30)

model_2_variables_40 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | surface, cutpoint = c(1430, 1430), method = "center", subset = cible==40 & revenu<1670 & revenu_conjoint != 0, t.design = c("leq", "geq"))
summary(model_2_variables_40)

model_2_variables_50 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | surface, cutpoint = c(1670, 1670), method = "center", subset = cible==40 & revenu<2200 & revenu_conjoint != 0, t.design = c("leq", "geq"))
summary(model_2_variables_40)


### Pooling thresholds ###
s$traite_transfert <- 1 * (s$cible==20) * (s$revenu<780) + 1 * (s$cible==30) * (s$revenu<1140) + 1 * (s$cible==40) * (s$revenu<1430) + 1 * (s$cible==50) * (s$revenu<1670)
s$traite_transfert_conjoint <- (s$nb_adultes > 1) * (1 * (s$cible==20) * (s$revenu_conjoint<780) + 1 * (s$cible==30) * (s$revenu_conjoint<1140) + 1 * (s$cible==40) * (s$revenu_conjoint<1430) + 1 * (s$cible==50) * (s$revenu_conjoint<1670))
summary(lm(dummy_approbation_cible ~ taxe_approbation + traite_transfert + traite_transfert_conjoint + revenu + revenu_conjoint, data=s, subset = revenu<1670 & revenu>780, weights = s$weight))
# Etre eligible augmente la probabilitÃ© d'approbation de 9.3%. Lorsque le conjoint est Ã©ligible, cela l'augmene de 4.5%.
# Vérifier sur quelle population appliquer estimer l'effet (l'allocation n'est aléatoire que pour les déciles 3, 4 et 5).

### Instrumental variable ###
s_20_50 <- subset(s, revenu > 780 & revenu < 1670)
s_20_50$gain_taxe__20[is.na(s_20_50$gain_taxe__20)] <- 0
s_20_50$gain_taxe_20_30[is.na(s_20_50$gain_taxe_20_30)] <- 0
s_20_50$gain_taxe_30_40[is.na(s_20_50$gain_taxe_30_40)] <- 0
s_20_50$gain_taxe_40_50[is.na(s_20_50$gain_taxe_40_50)] <- 0

s_20_50$transfert_seuil_gagnant <- 1 * (s_20_50$gain_taxe__20=='Gagnant') + 1 * (s_20_50$gain_taxe_20_30=='Gagnant') + 1 * (s_20_50$gain_taxe_30_40=='Gagnant') + 1 * (s_20_50$gain_taxe_40_50=='Gagnant')
decrit(s_20_50$transfert_seuil_gagnant)

y <- s_20_50$dummy_approbation_cible
x <- s_20_50$transfert_seuil_gagnant
z1 <- s_20_50$traite_transfert
z2 <- s_20_50$traite_transfert_conjoint
c <- s_20_50$score_climate_call # This is just here as an example of control variable

# Simple OLS #
ols_approve_winner <- lm(y ~ x, data=s_20_50, weights = s$weight)
summary(ols_approve_winner)

# 2SLS #
cor(z1,x)
cor(z2,x)

tsls1<-lm(x ~ z1 + z2)
summary(tsls1)

d.hat<-fitted.values(tsls1)
tsls2<-lm(y ~ c + d.hat)
summary(tsls2)
# The effect (around 0.6, similar to OLS) is very substantial
