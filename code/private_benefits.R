# Ce script recense différentes méthodes pour estimer l'effet sur l'approbation de la taxe carbone compensée de se savoir gagnant vis-à-vis de cette taxe
# les deux principales méthodes qui peuvent être utilisées sont :
# 1) le RDD : on exploite les différences de réponse des ménages juste en dessous et juste au dessus des seuils de compensation
# 2) l'IV : on instrumente en utilisant l'allocation aléatoire des ménages à des mécanismes auxquels ils sont éligibles ou non
# La meilleure méthode consiste à combiner ces deux approches : 2SLS avec RDD paramétriques pour l'équation de first stage.
# On obtient que ceteris paribus, se savoir gagnant augmente la proabilité d'approbation d'environ 50 p.p.
# Ce résultat est confirmé lorsqu'on effectue la même procédure (2SLS avec RDD paramétrique en 1st stage) appliqué au feedback.

##### 1. Spécification principale : 2SLS avec RDD paramétrique à multiples seuils en first stage #####
# Simple OLS #
summary(lm((taxe_cible_approbation!='Non') ~ (gagnant_cible_categorie!='Perdant')*(taxe_approbation!='Non'), data=s, subset=categorie_cible!='70_', weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ (gagnant_cible_categorie!='Perdant') + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2) + (taxe_approbation!='Non') + simule_gain_cible, data=s, subset=categorie_cible!='70_', weights = s$weight))
# Une OLS standard nous donne qu'une personne se percevant gagnante a une probabilité d'approbation supérieure de 48 p.p.

# Lorsqu'on examine les résultats à la première question, l'effet des bénéfices privés sur l'approbation est nettement plus faible. On peut interpréter cela comme une confiance moindre dans cette réponse.
summary(lm((taxe_approbation!='Non') ~ (gain_echelle >4) + (gain_echelle < -4) + (taxe_efficace!='Non') + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2) + simule_gain_cible, data=s, subset=categorie_cible!='70_', weights = s$weight))

cor(s$simule_gain_cible, s$traite_cible)
cor(s$simule_gain_cible[s$nb_adultes>1], s$traite_cible_conjoint[s$nb_adultes>1])

cor((s$gagnant_cible_categorie!='Perdant'), s$traite_cible)
cor((s$gagnant_cible_categorie!='Perdant'), s$traite_cible_conjoint)

# avant il y avait hausse_depenses au lieu de simule_gain_cible
tsls_rdd_1 <- lm((gagnant_cible_categorie!='Perdant') ~ traite_cible * traite_cible_conjoint + (taxe_approbation!='Non') + simule_gain_cible + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, subset=cible!='70_', weights = s$weight)
summary(tsls_rdd_1) # TODO: exclure les >70 ou pas ?
gagnant.hat <- fitted.values(tsls_rdd_1)
summary(lm((taxe_cible_approbation!='Non') ~ gagnant.hat + (taxe_approbation!='Non') + simule_gain_cible + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, subset=cible!='70_', weights = s$weight))
# On estime un TOT : ceteris paribus, se considérer comme gagnant augmente la probabilité d'approbation de 47 p.p.
# Note : je ne suis pas sûr que d_rdd.hat exprime ce que l'on souhaite : quel rôle des variables de contrôle dans le 1er et 2e stage ? Revoir la théorie
tsls_rdd_1 <- lm((gagnant_cible_categorie!='Perdant') ~ traite_cible * traite_cible_conjoint + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, subset=cible!='70_', weights = s$weight)
summary(tsls_rdd_1) # TODO: exclure les >70 ou pas ?
gagnant.hat <- fitted.values(tsls_rdd_1)
summary(lm((taxe_cible_approbation!='Non') ~ gagnant.hat + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, subset=cible!='70_', weights = s$weight))

# Avec effet hétérogène par seuil
# s$cible <- relevel(as.factor(s$cible), '50')
summary(lm((taxe_cible_approbation!='Non') ~ gagnant.hat*cible + taxe_approbation + simule_gain_cible + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, subset=cible!='70_', weights = s$weight))
# Les résultats ne sont pas significatifs pour les effets par seuil. L'effet d_rdd.hat varie selon la catégorie omise, et perd en significativité.


##### 2. RDD paramétrique à multiples seuils (20-30-40-50) #####
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible * traite_cible_conjoint + cible + taxe_approbation + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, weights = s$weight))
# On estime que lorsque le répondant est éligible au transfert, ceteris paribus la probabilité d'approbation augmente de 10 p.p.
# On estime que lorsque son conjoint est éligible au transfert, ceteris paribus la probabilité d'approbation de 5 p.p.
# On estime que l'effet marginal du cumul de la double égibilité est négatif, et réduit l'approbation de 3 p.p.
# Les effets plus faibles qu'avec l'IV proviennet (a priori) du fait qu'être éligible n'est pas suffisant pour se considérer gagnant.

# 20%*** (conjoint 10%) for 20_30
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible * traite_cible_conjoint, subset = categorie_cible == '20_30', data=s, weights = s$weight))
# 15%* (15%*) for 30_40
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible * traite_cible_conjoint, subset = categorie_cible == '30_40', data=s, weights = s$weight))
# 12% (17%*) for 40_50
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible * traite_cible_conjoint, subset = categorie_cible == '40_50', data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible + traite_cible_conjoint, subset = categorie_cible == '20_30', data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible + traite_cible_conjoint, subset = categorie_cible == '30_40', data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible + traite_cible_conjoint, subset = categorie_cible == '40_50', data=s, weights = s$weight))
# 13%*** (resp. 8%***) more chance of acceptance with traite_cible (resp. traite_cible_conjoint), with -7%. if both are treated
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible * traite_cible_conjoint + cible + taxe_approbation + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible * traite_cible_conjoint + cible + taxe_approbation + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), subset=is.element(categorie_cible, c('20_30', '30_40', '40_50')), data=s, weights = s$weight))
# Effect driven by cible=30 for respondent and cible=20 for conjoint
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible * traite_cible_conjoint * cible + taxe_approbation + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ traite_cible * traite_cible_conjoint * cible + taxe_approbation + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2), data=s, weights = s$weight))
summary(lm((taxe_cible_approbation!='Non') ~ versement_cible*cible + simule_gain_cible + taxe_approbation + revenu + revenu_conjoint, data=s, weights = s$weight))


##### 3. 2SLS standard #####
# 2SLS #
cor(s$traite_cible, s$gagnant_cible_categorie !='Perdant') 
cor(s$traite_cible_conjoint, s$gagnant_cible_categorie !='Perdant') # Les instrumments ne sont pas faibles

tsls1 <- lm(gagnant_cible_categorie !='Perdant'~ traite_cible + traite_cible_conjoint + (taxe_approbation!='Non'), subset=is.element(categorie_cible, c('20_30', '30_40', '40_50')), data=s, weights = s$weight, na.action='na.exclude')
summary(tsls1)
gagnant.hat <- fitted.values(tsls1)
# gagnant.hat: 69% MAIS manquent des contrôles
summary(lm((taxe_cible_approbation!='Non') ~ gagnant.hat + (taxe_approbation!='Non'), data=s[is.element(s$categorie_cible, c('20_30', '30_40', '40_50')),], weights = s$weight[is.element(s$categorie_cible, c('20_30', '30_40', '40_50'))]))
# L'effet sur l'approbation est plus élevé que dans la 2SLS avec RDD en first stage, et plus élevé qu'avec OLS (ce qui est surprenant) : 0.66 p.p. d'augmentation
# Le problème de cette méthode est que l'on affecte les répondants aléatoirement aux différents mécanismes, mais on compare des mécanises différents
# En moyenne, les mécanismes auxquels ils sont éligibles transfèrent moins à plus de gens

# Test effet des seuils sur les ménages jamais éligibles pour écarter l'effet gagnant/perdant de l'acceptation #
# Plus la mesure est ciblée sur les plus pauvres, plus elle est acceptée
summary(lm(taxe_cible_approbation!='Non' ~ (cible==20) + (cible==30) + (cible==40), subset=categorie_cible == '70_' & traite_cible != 1 & traite_cible_conjoint != 1, data=s, weights = s$weight))
decrit(s$traite_cible[s$categorie_cible == '70_']) # TODO: qui sont ces gens traités malgré que catégorie_cible == 70_ ?
decrit(s$traite_cible_conjoint[s$categorie_cible == '70_'])


##### 4. RDD non-paramétrique par seuil #####
# Seuil 20 #
s_0_30 <- subset(s, cible20==1 & (categorie_cible == '_20'  | categorie_cible == '20_30'))
data_rdd_0_30 <- rdd_data(y=s_0_30$taxe_cible_approbation!='Non', x=s_0_30$revenu, cutpoint=780)
bandwidthsize_0_30 <- rdd_bw_ik(data_rdd_0_30)
model_rdd_0_30 = rdd_reg_np(rdd_object = data_rdd_0_30, bw = bandwidthsize_0_30)
summary(model_rdd_0_30)
plot(model_rdd_0_30)

# Seuil 30 #
s_20_40 <- subset(s, cible30==1 & (categorie_cible == '20_30'  | categorie_cible == '30_40'))
data_rdd_20_40 <- rdd_data(y=s_20_40$taxe_cible_approbation!='Non', x=s_20_40$revenu, cutpoint=1140)
bandwidthsize_20_40 <- rdd_bw_ik(data_rdd_20_40)
model_rdd_20_40 = rdd_reg_np(rdd_object = data_rdd_20_40, bw = bandwidthsize_20_40)
summary(model_rdd_20_40)
plot(model_rdd_20_40)

# Seuil 40 #
s_30_50 <- subset(s, cible40==1 & (categorie_cible == '30_40'  | categorie_cible == '40_50'))
data_rdd_30_50 <- rdd_data(y=s_30_50$taxe_cible_approbation!='Non', x=s_30_50$revenu, cutpoint=1430)
bandwidthsize_30_50 <- rdd_bw_ik(data_rdd_30_50)
model_rdd_30_50 = rdd_reg_np(rdd_object = data_rdd_30_50, bw = bandwidthsize_30_50)
summary(model_rdd_30_50)
plot(model_rdd_30_50)

# Seuil 50 #
s_40_70 <- subset(s, cible50==1 & (categorie_cible == '40_50'  | categorie_cible == '50_70'))
data_rdd_40_70 <- rdd_data(y=s_40_70$taxe_cible_approbation!='Non', x=s_40_70$revenu, cutpoint=1670)
bandwidthsize_40_70 <- rdd_bw_ik(data_rdd_40_70)
model_rdd_40_70 = rdd_reg_np(rdd_object = data_rdd_40_70, bw = bandwidthsize_40_70)
summary(model_rdd_40_70)
plot(model_rdd_40_70)
# Les résultats sont corrects mais pas tous significatifs. Pb: difficile d'interpréter les points sur les graphiques


##### 5. RDD non-paramétrique multivarié par seuil #####
cible <- s$cible
categorie_cible <- s$categorie_cible
revenu <- s$revenu
revenu_conjoint <- s$revenu_conjoint
approbation_avant <- s$taxe_approbation # This is just here as an example of control variable
dummy_approbation_cible <- s$taxe_cible_approbation!='Non'

model_2_variables_20 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | approbation_avant, cutpoint = c(780, 780), method = "center", subset = cible==20 & (categorie_cible == '_20'  | categorie_cible == '20_30'), t.design = c("leq", "geq"))
summary(model_2_variables_20)

model_2_variables_30 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | approbation_avant, cutpoint = c(1140, 1140), method = "center", subset = cible==30 & (categorie_cible == '20_30'  | categorie_cible == '30_40'), t.design = c("leq", "geq"))
summary(model_2_variables_30)

model_2_variables_40 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | approbation_avant, cutpoint = c(1430, 1430), method = "center", subset = cible==40 & (categorie_cible == '30_40'  | categorie_cible == '40_50'), t.design = c("leq", "geq"))
summary(model_2_variables_40)

model_2_variables_50 = mrd_est(dummy_approbation_cible ~ revenu + revenu_conjoint | approbation_avant, cutpoint = c(1670, 1670), method = "center", subset = cible==50 & (categorie_cible == '40_50'  | categorie_cible == '50_70'), t.design = c("leq", "geq"))
summary(model_2_variables_50)
# Rien de significatif, et rien qui n'ait vraiment de sens


##### 6. 2SLS avec RDD paramétrique pour le feedback
# OLS simple: 0.30***
summary(lm(taxe_feedback_approbation=='Oui' ~ (gagnant_feedback_categorie == 'Gagnant') + taxe_approbation, data=s, weights = s$weight))
summary(lm(taxe_feedback_approbation!='Non' ~ (gagnant_feedback_categorie != 'Perdant') + (taxe_approbation!='Non'), data=s, weights = s$weight))

# RDD simple - effet d'être gagnant: 0.08***
summary(lm(taxe_feedback_approbation=='Oui' ~ simule_gagnant + simule_gain + I(simule_gain^2) + taxe_approbation, data=s, weights = s$weight))

# 2SLS avec 1st stage RDD - effet de se considérer gagnant
cor(s$gagnant_feedback_categorie == 'Gagnant', n(s$simule_gagnant), use="complete.obs") # 0.24
tsls_rdd_feedback_1 <- lm(gagnant_feedback_categorie == 'Gagnant' ~ simule_gagnant + taxe_approbation + simule_gain + I(simule_gain^2), data=s, weights = s$weight, na.action="na.exclude")
gagnant_feedback.hat <- fitted.values(tsls_rdd_feedback_1)
summary(lm(taxe_feedback_approbation=='Oui' ~ gagnant_feedback.hat + taxe_approbation + simule_gain + I(simule_gain^2), data=s, weights = s$weight))
# Les résultats sont sensiblement les mêmes que dans le cas des seuils :
# 1) Etre gagnant augmente la probabilité d'approuver de 10p.p., 2) se considérer gagnant augmente la probabilité d'approuver de 41 p.p.
# L'effet estimé est ici local, et concerne les personnes qui sont à la limite de gagner/perdre.
cor(s$gagnant_feedback_categorie != 'Perdant', n(s$simule_gagnant), use="complete.obs") # 0.24
tsls_rdd_feedback_1 <- lm(gagnant_feedback_categorie != 'Perdant' ~ simule_gagnant + (taxe_approbation!='Non') + simule_gain + I(simule_gain^2), data=s, weights = s$weight, na.action="na.exclude")
gagnant_feedback.hat <- fitted.values(tsls_rdd_feedback_1)
summary(lm(taxe_feedback_approbation!='Non' ~ gagnant_feedback.hat + (taxe_approbation!='Non') + simule_gain + I(simule_gain^2), data=s, weights = s$weight))


##### 7. Probit - WIP
library("mfx")
probitmarg <- probitmfx(taxe_feedback_approbation!='Non' ~ (gagnant_feedback_categorie != 'Perdant') + (taxe_approbation!='Non') + revenu + I(revenu^2) + revenu_conjoint + (taxe_efficace!='Non') + simule_gain, data = s, atmean = TRUE)
probitmarg
probitmarg2 <- probitmfx(taxe_feedback_approbation!='Non' ~ (gagnant_feedback_categorie != 'Perdant') + revenu + revenu_conjoint + I(revenu^2) + I(revenu_conjoint^2) + (taxe_approbation!='Non') + simule_gain, data = s, atmean = TRUE)
probitmarg2 # TODO: solve bug (seems to come from revenu_conjoint^2)


##### 8. Biprobit - WIP...
s$dummy_approbation_feedback <- s$taxe_feedback_approbation != 'Non'
s$dummy_declare_gagnant_feedback_categorie <- s$gagnant_feedback_categorie == 'Gagnant'
biprobit_feedback <- biprobit(dummy_approbation_feedback~1+dummy_declare_gagnant_feedback_categorie, rho=~1+dummy_declare_gagnant_feedback_categorie, data=s)
summary(biprobit_feedback)
margins(biprobit_feedback) # TODO: bug
summary(margins(biprobit_feedback, variables = "dummy_declare_gagnant_feedback_categorie"))
