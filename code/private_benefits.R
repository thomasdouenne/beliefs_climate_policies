# Ce script recense différentes méthodes pour estimer l'effet sur l'approbation de la taxe carbone compensée de se savoir gagnant vis-à-vis de cette taxe
# les deux principales méthodes qui peuvent être utilisées sont :
# 1) le RDD : on exploite les différences de réponse des ménages juste en dessous et juste au dessus des seuils de compensation
# 2) l'IV : on instrumente en utilisant l'allocation aléatoire des ménages à des mécanismes auxquels ils sont éligibles ou non
# La meilleure méthode consiste à combiner ces deux approches : 2SLS avec RDD paramétriques pour l'équation de first stage.
# On obtient que ceteris paribus, se savoir gagnant augmente la proabilité d'approbation d'environ 50 p.p.

##### 1. Spécification principale : 2SLS avec RDD paramétrique à multiples seuils en first stage #####
s$dummy_approbation_cible <- 1*(s$taxe_cible_approbation=='Oui')

s$traite_transfert <- 1 * (s$cible==20) * (s$revenu<780) + 1 * (s$cible==30) * (s$revenu<1140) + 1 * (s$cible==40) * (s$revenu<1430) + 1 * (s$cible==50) * (s$revenu<1670)
s$traite_transfert_conjoint <- (s$nb_adultes > 1) * (1 * (s$cible==20) * (s$revenu_conjoint<780) + 1 * (s$cible==30) * (s$revenu_conjoint<1140) + 1 * (s$cible==40) * (s$revenu_conjoint<1430) + 1 * (s$cible==50) * (s$revenu_conjoint<1670))

s$revenu_2 <- s$revenu^2
s$revenu_conjoint_2 <- s$revenu_conjoint^2

s$gain_taxe__20[is.na(s$gain_taxe__20)] <- 0
s$gain_taxe_20_30[is.na(s$gain_taxe_20_30)] <- 0
s$gain_taxe_30_40[is.na(s$gain_taxe_30_40)] <- 0
s$gain_taxe_40_50[is.na(s$gain_taxe_40_50)] <- 0
s$transfert_seuil_gagnant <- 1 * (s$gain_taxe__20=='Gagnant') + 1 * (s$gain_taxe_20_30=='Gagnant') + 1 * (s$gain_taxe_30_40=='Gagnant') + 1 * (s$gain_taxe_40_50=='Gagnant')

s_0_70 <- subset(s, categorie_cible != '70_')

y_dummy_approbation_cible <- s_0_70$dummy_approbation_cible
x_transfert_seuil_gagnant <- s_0_70$transfert_seuil_gagnant
z1_traite_transfert <- s_0_70$traite_transfert
z2_traite_transfert_conjoint <- s_0_70$traite_transfert_conjoint
c1_taxe_approbation <- s_0_70$taxe_approbation # Peut-être ajouter d'autres variables de contrôle

tsls_rdd_1 <- lm(transfert_seuil_gagnant ~ taxe_approbation + traite_transfert + traite_transfert_conjoint + revenu + revenu_conjoint + revenu_2 + revenu_conjoint_2, data=s_0_70, weights = s$weight)

d_rdd.hat <- fitted.values(tsls_rdd_1)
tsls_rdd_2 <- lm(dummy_approbation_cible ~ d_rdd.hat + taxe_approbation + revenu + revenu_conjoint, data=s_0_70, weights = s$weight)
summary(tsls_rdd_2)
# On estime un TOT : ceteris paribus, se considérer comme gagnant augmente la probabilité d'approbation de 47 p.p.
# Note : je ne suis pas sûr que d_rdd.hat exprime ce que l'on souhaite : quel rôle des variables de contrôle dans le 1er et 2e stage ? Revoir la théorie


##### 2. RDD paramétrique à multiples seuils (20-30-40-50) #####
rdd_ms <- lm(dummy_approbation_cible ~ traite_transfert + traite_transfert_conjoint + (traite_transfert * traite_transfert_conjoint) + taxe_approbation + revenu + revenu_2 + revenu_conjoint + revenu_conjoint_2, data=s_0_70, weights = s$weight)
summary(rdd_ms)
# On estime que lorsque le répondant est éligible au transfert, ceteris paribus la probabilité d'approbation augmente de 10 p.p.
# On estime que lorsque son conjoint est éligible au transfert, ceteris paribus la probabilité d'approbation de 5 p.p.
# On estime que l'effet marginal du cumul de la double égibilité est négatif, et réduit l'approbation de 3 p.p.
# Les effets plus faibles qu'avec l'IV proviennet (a priori) du fait qu'être éligible n'est pas suffisant pour se considérer gagnant.


##### 3. 2SLS standard #####
s_20_50 <- subset(s, categorie_cible != '70_' & categorie_cible != '50_70' & categorie_cible != '0_20')

y_dummy_approbation_cible <- s_20_50$dummy_approbation_cible
x_transfert_seuil_gagnant <- s_20_50$transfert_seuil_gagnant
z1_traite_transfert <- s_20_50$traite_transfert
z2_traite_transfert_conjoint <- s_20_50$traite_transfert_conjoint
c1_taxe_approbation <- s_20_50$taxe_approbation # Peut-être ajouter d'autres variables de contrôle

# Simple OLS #
ols_approve_winner <- lm(y_dummy_approbation_cible ~ x_transfert_seuil_gagnant + c1_taxe_approbation, data=s_20_50, weights = s$weight)
summary(ols_approve_winner)
# Une OLS standard nous donne qu'une personne se percevant gagnante a une probabilité d'approbation supérieure de 51 p.p.

# 2SLS #
cor(z1_traite_transfert,x_transfert_seuil_gagnant)
cor(z2_traite_transfert_conjoint,x_transfert_seuil_gagnant) # Les instrumments ne sont pas faibles

tsls1 <- lm(x_transfert_seuil_gagnant ~ z1_traite_transfert + z2_traite_transfert_conjoint + c1_taxe_approbation)
summary(tsls1)

d.hat <- fitted.values(tsls1)
tsls2 <- lm(y_dummy_approbation_cible ~ d.hat + c1_taxe_approbation)
summary(tsls2)
# L'effet sur l'approbation est plus élevé que dans la 2SLS avec RDD en first stage, et plus élevé qu'avec OLS (ce qui est surprenant) : 0.66 p.p. d'augmentation
# Le problème de cette méthode est que l'on affecte les répondants aléatoirement aux différents mécanismes, mais on compare des mécanises différents
# En moyenne, les mécanismes auxquels ils sont éligibles transferent moins à plus de gens

# Test effet des seuils sur les ménages jamais éligibles pour écarter l'effet gagnant/perdant de l'acceptation #
s_70_plus <- subset(s, categorie_cible == '70_' & traite_transfert != 1 & traite_transfert_conjoint != 1) # To do : mieux définir ce sample, ce critère revenu n'est pas suffisant du fait du revenu du conjoint
ols_approuve_seuils <- lm(dummy_approbation_cible ~ (cible==20) + (cible==30) + (cible==40), data=s_70_plus, weights = s$weight)
summary(ols_approuve_seuils)
# On ne détecte aucun effet significatif du seuil pour les non-éligibles affectés aléatoirement à l'un d'entre eux.
# A noter toutefois le nombre très faible d'observations
# A noter aussi cette bizarrerie : la présence de personnes recevant un transfert positif dans ce sous-échantillon...


##### 4. RDD non-paramétrique par seuil #####
require(rddtools)

# Seuil 20 #
s_0_30 <- subset(s, cible20==1 & (categorie_cible == '_20'  | categorie_cible == '20_30'))
data_rdd_0_30 <- rdd_data(y=s_0_30$dummy_approbation_cible, x=s_0_30$revenu, cutpoint=780)
bandwidthsize_0_30 <- rdd_bw_ik(data_rdd_0_30)
model_rdd_0_30 = rdd_reg_np(rdd_object = data_rdd_0_30, bw = bandwidthsize_0_30)
summary(model_rdd_0_30)
plot(model_rdd_0_30)

# Seuil 30 #
s_20_40 <- subset(s, cible30==1 & (categorie_cible == '20_30'  | categorie_cible == '30_40'))
data_rdd_20_40 <- rdd_data(y=s_20_40$dummy_approbation_cible, x=s_20_40$revenu, cutpoint=1140)
bandwidthsize_20_40 <- rdd_bw_ik(data_rdd_20_40)
model_rdd_20_40 = rdd_reg_np(rdd_object = data_rdd_20_40, bw = bandwidthsize_20_40)
summary(model_rdd_20_40)
plot(model_rdd_20_40)

# Seuil 40 #
s_30_50 <- subset(s, cible40==1 & (categorie_cible == '30_40'  | categorie_cible == '40_50'))
data_rdd_30_50 <- rdd_data(y=s_30_50$dummy_approbation_cible, x=s_30_50$revenu, cutpoint=1430)
bandwidthsize_30_50 <- rdd_bw_ik(data_rdd_30_50)
model_rdd_30_50 = rdd_reg_np(rdd_object = data_rdd_30_50, bw = bandwidthsize_30_50)
summary(model_rdd_30_50)
plot(model_rdd_30_50)

# Seuil 50 #
s_40_70 <- subset(s, cible50==1 & (categorie_cible == '40_50'  | categorie_cible == '50_70'))
data_rdd_40_70 <- rdd_data(y=s_40_70$dummy_approbation_cible, x=s_40_70$revenu, cutpoint=1670)
bandwidthsize_40_70 <- rdd_bw_ik(data_rdd_40_70)
model_rdd_40_70 = rdd_reg_np(rdd_object = data_rdd_40_70, bw = bandwidthsize_40_70)
summary(model_rdd_40_70)
plot(model_rdd_40_70)
# Les résultats sont tous très mauvais, je ne saurais pas interpréter les points sur les graphiques
# Le problème est peut-être juste que la taille de l'échantillon est trop limitée pour détecter des effets non-paramétriques sur sous-échantillon


##### 5. RDD non-paramétrique multivarié par seuil #####
require(rddapp)

cible <- s$cible
categorie_cible <- s$categorie_cible
revenu <- s$revenu
revenu_conjoint <- s$revenu_conjoint
approbation_avant <- s$taxe_approbation # This is just here as an example of control variable
dummy_approbation_cible <- s$dummy_approbation_cible

model_2_variables_20 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | approbation_avant, cutpoint = c(780, 780), method = "center", subset = cible==20 & (categorie_cible == '_20'  | categorie_cible == '20_30'), t.design = c("leq", "geq"))
summary(model_2_variables_20)

model_2_variables_30 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | approbation_avant, cutpoint = c(1140, 1140), method = "center", subset = cible==30 & (categorie_cible == '20_30'  | categorie_cible == '30_40'), t.design = c("leq", "geq"))
summary(model_2_variables_30)

model_2_variables_40 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | approbation_avant, cutpoint = c(1430, 1430), method = "center", subset = cible==40 & (categorie_cible == '30_40'  | categorie_cible == '40_50'), t.design = c("leq", "geq"))
summary(model_2_variables_40)

model_2_variables_50 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | approbation_avant, cutpoint = c(1670, 1670), method = "center", subset = cible==50 & (categorie_cible == '40_50'  | categorie_cible == '50_70'), t.design = c("leq", "geq"))
summary(model_2_variables_50)
# De même, rien de significatif, et rien qui n'ait vraiment de sens
