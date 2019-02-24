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

### Seuil 30 conjoint ###
decrit(s_30$revenu_conjoint)
s_30_bis <- subset(s_30, revenu_conjoint != 0)
data_rdd_30_bis <- rdd_data(y=s_30_bis$dummy_approbation_cible, x=s_30_bis$revenu_conjoint, cutpoint=1140)
bandwidthsize_30_bis <- rdd_bw_ik(data_rdd_30_bis)
model_rdd_30_bis = rdd_reg_np(rdd_object = data_rdd_30_bis, bw = bandwidthsize_30_bis)
summary(model_rdd_30_bis)
plot(model_rdd_30_bis)

### Seuil 40 ###
s_40 <- subset(s, cible40==1 & revenu < 1670)
data_rdd_40 <- rdd_data(y=s_40$dummy_approbation_cible, x=s_40$revenu, cutpoint=1430)
bandwidthsize_40 <- rdd_bw_ik(data_rdd_40)
model_rdd_40 = rdd_reg_np(rdd_object = data_rdd_40, bw = bandwidthsize_40)
summary(model_rdd_40)
plot(model_rdd_40)


### Multivariate RDD ###
cible <- s$cible
revenu <- s$revenu
revenu_conjoint <- s$revenu_conjoint
surface <- s$surface
dummy_approbation_cible <- s$dummy_approbation_cible

model_2_variables_20 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | surface, cutpoint = c(780, 780), method = "center", subset = cible==20 & x1<1140 & x2 != 0, t.design = c("leq", "geq"))
summary(model_2_variables_20)

model_2_variables_30 = mrd_est(y ~ x1 + x2 | x3, cutpoint = c(1140, 1140), method = "center", subset = cible==30 & x1<1430 & x2 != 0, t.design = c("leq", "geq"))
summary(model_2_variables_30)

model_2_variables_40 = mrd_est(y ~ x1 + x2 | x3, cutpoint = c(1430, 1430), method = "center", subset = cible==40 & x1<1670 & x2 != 0, t.design = c("leq", "geq"))
summary(model_2_variables_40)

# To do: pool the three groups together
