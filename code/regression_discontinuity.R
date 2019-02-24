require(rddtools)

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
