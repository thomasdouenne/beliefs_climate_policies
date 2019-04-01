##### RÃ©gressions: progressivitÃ© #####
decrit(s$progressivite, weights = s$weight)
decrit(s$progressivite[s$info_progressivite==T])
decrit(s$progressivite[s$info_progressivite==FALSE])
# On ne convainc pas les rÃ©pondants que la taxe est progressive !


##### 2SLS info progressivite #####
# Instrument too weak
tsls1_progressivite <- lm((progressivite!='Non') ~ info_progressivite + (taxe_approbation != 'Non') + (gagnant_feedback_categorie!='Perdant') + (taxe_efficace!='Non'), data=s, weights = s$weight, na.action="na.exclude")
summary(tsls1_progressivite)
progressivite.hat <- fitted.values(tsls1_progressivite)
summary(lm((taxe_info_approbation!='Non') ~ progressivite.hat + (taxe_approbation != 'Non') + (gagnant_feedback_categorie!='Perdant') + (taxe_efficace!='Non'), data=s, weights = s$weight))


# OLS: 0.56***
summary(lm((taxe_info_approbation!='Non') ~ (progressivite!='Non'), data=s, weights = s$weight)) # TODO: rajouter contrÃ´les
# OLS forme rÃ©duite comme 2SLS ne fonctionnent pas
summary(lm((taxe_info_approbation!='Non') ~ progressivite.hat, data=s, weights = s$weight))
summary(lm((taxe_info_approbation!='Non') ~ info_progressivite, data=s, weights = s$weight))
# petit effet: 0.03. sur la diffÃ©rence d'acceptation
summary(lm(((taxe_info_approbation!='Non') - (taxe_approbation!='Non')) ~ info_progressivite, data=s, weights = s$weight))



##### 2SLS feedback #####
# OLS1: 9*** p.p. Peut-Ãªtre que mÃªme sans changer d'avis, le 5/6 les convainc que leur groupe va gagner, donc que c'est progressif
# Problème : gagnant_categorie ne suffit pas à isoler l'effet vis-à-vis de leur situation personnelle -> d'où l'effet  très fort estimé pour la progressivité
tsls1_progressivite <- lm((progressivite=='Oui') ~ simule_gagnant + simule_gain + gagnant_feedback_categorie + (taxe_approbation!='Non') + (taxe_efficace!='Non') + taille_agglo + revenu, data=s, weights = s$weight, na.action="na.exclude")
summary(tsls1_progressivite)
progressivite.hat <- fitted.values(tsls1_progressivite)
# 2SLS : 36 p.p., not significant
summary(lm((taxe_info_approbation!='Non') ~ progressivite.hat + gagnant_feedback_categorie + simule_gain + (taxe_approbation!='Non') + (taxe_efficace!='Non') + taille_agglo + revenu, data=s, weights = s$weight))


##### OLS et Probit #####
# L'effet du gain personnel est plus faible, mais encore une fois il est associé à une cetritude plus faible à ce stade
library("mfx")

summary(lm((taxe_info_approbation!='Non') ~ (progressivite!='Non') + (taxe_efficace != 'Non') + (gagnant_categorie == 'Gagnant') + taille_agglo + revenu + I(revenu^2) + revenu_conjoint + simule_gain, data = s, weights = s$weight, na.action="na.exclude"))

probit_marg <- probitmfx((taxe_info_approbation!='Non') ~ (progressivite!='Non') + (taxe_efficace != 'Non') + (gagnant_feedback_categorie != 'Perdant') + taille_agglo + revenu + I(revenu^2) + revenu_conjoint + simule_gain, data = s, atmean = TRUE)
probit_marg

##### Effet cumulé des trois déterminants #####
summary(lm((taxe_info_approbation=='Oui') ~ (progressivite=='Oui') * (taxe_efficace == 'Oui') * (gagnant_feedback_categorie == 'Gagnant') + taille_agglo + revenu + I(revenu^2) + revenu_conjoint + simule_gain, data = s, weights = s$weight, na.action="na.exclude"))
probit_marg <- probitmfx((taxe_info_approbation=='Oui') ~ (progressivite=='Oui') * (taxe_efficace == 'Oui') * (gagnant_feedback_categorie == 'Gagnant') + taille_agglo + revenu + I(revenu^2) + revenu_conjoint + simule_gain, data = s, atmean = TRUE)
probit_marg
