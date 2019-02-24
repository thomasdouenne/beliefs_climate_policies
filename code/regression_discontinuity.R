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
surface <- s$surface # This is just here as an example of control variable
dummy_approbation_cible <- s$dummy_approbation_cible

model_2_variables_20 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | surface, cutpoint = c(780, 780), method = "center", subset = cible==20 & x1<1140 & x2 != 0, t.design = c("leq", "geq"))
summary(model_2_variables_20)

model_2_variables_30 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | surface, cutpoint = c(1140, 1140), method = "center", subset = cible==30 & x1<1430 & x2 != 0, t.design = c("leq", "geq"))
summary(model_2_variables_30)

model_2_variables_40 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | surface, cutpoint = c(1430, 1430), method = "center", subset = cible==40 & x1<1670 & x2 != 0, t.design = c("leq", "geq"))
summary(model_2_variables_40)

model_2_variables_50 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | surface, cutpoint = c(1670, 1670), method = "center", subset = cible==40 & x1<2200 & x2 != 0, t.design = c("leq", "geq"))
summary(model_2_variables_40)


### Pooling thresholds ###
s$traite_transfert <- 1 * (s$cible==20) * (s$revenu<780) + 1 * (s$cible==30) * (s$revenu<1140) + 1 * (s$cible==40) * (s$revenu<1430) + 1 * (s$cible==50) * (s$revenu<1670)
s$traite_transfert_conjoint <- (s$nb_adultes > 1) * (1 * (s$cible==20) * (s$revenu_conjoint<780) + 1 * (s$cible==30) * (s$revenu_conjoint<1140) + 1 * (s$cible==40) * (s$revenu_conjoint<1430) + 1 * (s$cible==50) * (s$revenu_conjoint<1670))
summary(lm(dummy_approbation_cible ~ taxe_approbation + traite_transfert + traite_transfert_conjoint + revenu + revenu_conjoint, data=s, subset = revenu<2200, weights = s$weight))
# Etre eligible augmente la probabilité d'approbation de 9.3%. Lorsque le conjoint est éligible, cela l'augmene de 4.5%.
