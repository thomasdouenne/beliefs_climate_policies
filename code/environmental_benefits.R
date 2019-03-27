##### Effet simple sur l'approbation ####

# Effet de info_CC mais pas de info_PM
summary(lm(taxe_approbation!='Non' ~ info_CC * info_PM, data=s, weights = s$weight))

# pas d'interaction entre info_PM et taille_agglo
summary(lm(taxe_approbation!='Non' ~ info_CC * info_PM + info_PM * (taille_agglo==5), data=s, weights = s$weight))
summary(lm(taxe_approbation!='Non' ~ info_CC * info_PM + info_PM * (taille_agglo>2), data=s, weights = s$weight))
summary(lm(taxe_approbation!='Non' ~ info_CC * info_PM + info_PM * taille_agglo, data=s, weights = s$weight))
summary(lm(taxe_approbation!='Non' ~ info_CC * info_PM + info_PM * info_CC * (taille_agglo==5), data=s, weights = s$weight))

# pas d'effet sur l'approbation de l'info sur l'efficacité
summary(lm(taxe_approbation!='Non' ~ apres_modifs, data=s, weights = s$weight))
summary(lm(taxe_efficace!='Non' ~ apres_modifs, data=s, weights = s$weight))

##### IV: A ~ efficace ~ info_CC/PM #####
# Pourquoi l'instrument info_CC/PM fonctionne mais pas apres_modifs? Peut-être que l'info convainc plus en profondeur les gens,
#   i.e. les convainc de la nÃ©cessitÃ© d'une taxe car le CC est grave; alors que les ancrés ce que la taxe est efficace les fait 
#   répondre qu'elle l'est par automatisme, sans changer leur acceptation pour autant

# 2SLS avec benefices_CC: effet de 110* p.p. ! (LATE on compliers -> small population that might have very different preferences regarding the environment)
decrit(s$taxe_efficace, weights = s$weight)
tsls1<-lm(taxe_efficace != 'Non' ~ info_CC * info_PM, data=s, weights = s$weight)
summary(tsls1)
taxe_efficace.hat <- fitted.values(tsls1)
summary(lm(taxe_approbation!='Non' ~ taxe_efficace.hat, data=s, weights = s$weight))

# 60p.p. : with controls and with both types of instruments (infos CC et PM, and "apres_modifs")
# Should we keep info_PM ? Correlation very weak
tsls1<-lm(taxe_efficace != 'Non' ~ apres_modifs + (gagnant_categorie != 'Perdant') + taille_agglo + revenu + I(revenu^2) + revenu_conjoint + I(revenu_conjoint^2) + simule_gain + I(simule_gain^2), data=s, weights = s$weight)
summary(tsls1)
taxe_efficace.hat <- fitted.values(tsls1)
summary(lm(taxe_approbation!='Non' ~ taxe_efficace.hat + (gagnant_categorie != 'Perdant') + taille_agglo + revenu + I(revenu^2) + revenu_conjoint + I(revenu_conjoint^2) + simule_gain + I(simule_gain^2), data=s, weights = s$weight))

# Controls only in second stage
tsls1<-lm(taxe_efficace != 'Non' ~ info_CC * info_PM, data=s, weights = s$weight)
summary(tsls1)
taxe_efficace.hat <- fitted.values(tsls1)
summary(lm(taxe_approbation!='Non' ~ taxe_efficace.hat + (gagnant_categorie != 'Perdant') + taille_agglo + revenu + I(revenu^2) + revenu_conjoint, data=s, weights = s$weight))

# 52 p.p. : same without controls, but significant at 8% only
tsls1<-lm(taxe_efficace != 'Non' ~ info_CC * info_PM + apres_modifs, data=s, weights = s$weight)
summary(tsls1)
taxe_efficace.hat <- fitted.values(tsls1)
summary(lm(taxe_approbation!='Non' ~ taxe_efficace.hat, data=s, weights = s$weight))

# Simple OLS: 0.51 cf. Logit pour les contrôles
summary(lm(taxe_approbation!='Non' ~ (taxe_efficace != 'Non'), data=s, weights = s$weight))
summary(lm((s$taxe_approbation == 'Oui') ~ (s$taxe_efficace == 'Oui'), data=s, weights = s$weight)) # 0.43

summary(lm(taxe_approbation!='Non' ~ (taxe_efficace != 'Non') + (gagnant_categorie != 'Perdant') + revenu + I(revenu^2) + revenu_conjoint + I(revenu_conjoint^2) + simule_gain + I(simule_gain^2) + taille_agglo, data=s, weights = s$weight))
library("mfx")
probit_marg <- probitmfx(taxe_approbation!='Non' ~ (taxe_efficace != 'Non') + (gagnant_categorie != 'Perdant') + taille_agglo + revenu + I(revenu^2) + revenu_conjoint + simule_gain, data = s, atmean = TRUE)
probit_marg


# 2SLS avec benefices_CC: pareil, effet de 106** p.p. 
decrit(s$benefices_CC, weights = s$weight)
tsls1_env <- lm(benefices_CC==T ~ info_CC, data=s, weights = s$weight)
summary(tsls1_env)
benefices_CC.hat <- fitted(tsls1_env)
summary(lm(taxe_approbation!='Non' ~ benefices_CC.hat, data=s, weights = s$weight))
# OLS: 0.37***
summary(lm(taxe_approbation!='Non' ~ benefices_CC, data=s, weights = s$weight))
summary(lm(s$taxe_approbation!='Non' ~ info_CC * info_PM + info_PM * (taille_agglo==5), data=s, weights=s$weight))

# Simple OLS #
ols_approuve_efficace <- lm(y ~ x, data=s, weights = s$weight)
summary(ols_approuve_efficace) # Effet assez fort : +0.39, si l'on ne contrôle pour rien

cor(as.numeric(s$info_CC), as.numeric(s$taxe_efficace))
cor(s$info_PM, as.numeric(s$taxe_efficace))
cor(as.numeric(s$apres_modifs), as.numeric(s$taxe_efficace))

tsls1<-lm(x ~ z1 + z2 + z3)
summary(tsls1)

d.hat<-fitted.values(tsls1)
tsls2<-lm(y ~ d.hat)
summary(tsls2)
# Parce que les instruments sont faibles, l'effet n'est pas significatif

### Que signifie la faiblesse des instruments ? ###
decrit(s$cause_CC)
View(s$cause_CC)

s$cause_CC_pas_reel <- 1 * (s$cause_CC == "n\'est pas une réalité")
s$cause_CC_naturel <- 1 * (s$cause_CC == "est principalement dû à la variabilité naturelle du climat")
s$cause_CC_humain <- 1 * (s$cause_CC == "est principalement dû à l\'activité humaine")
s$cause_CC_NSP <- 1 * (s$cause_CC == "NSP")

decrit(s$cause_CC_pas_reel)
decrit(s$cause_CC_naturel)
decrit(s$cause_CC_humain) # 70%...
decrit(s$cause_CC_NSP)

summary(lm(s$cause_CC_humain ~ s$info_CC + s$info_PM + (s$info_CC * s$info_PM), data=s, weights = s$weight))
summary(lm(s$cause_CC_pas_reel ~ s$info_CC + s$info_PM + (s$info_CC * s$info_PM), data=s, weights = s$weight))
# Nos informations n'ont pas d'effet significatif sur les croyances relatives aux causes du CC

s$effets_CC_insignifiant <- 1 * (s$effets_CC == "Insignifiants, voire bénéfiques")
s$effets_CC_faibles <- 1 * (s$effets_CC == "Faibles, car les humains sauraient vivre avec")
s$effets_CC_graves <- 1 * (s$effets_CC == "Graves, car il y aurait plus de catastrophes naturelles")
s$effets_CC_desastreux <- 1 * (s$effets_CC == "Désastreux, les modes de vie seraient largement altérés")
s$effets_CC_cataclysmiques <- 1 * (s$effets_CC == "Cataclysmiques, l'humanité disparaîtrait")
s$effets_CC_NSP <- 1 * (s$effets_CC == "NSP")

decrit(s$effets_CC_insignifiant)
decrit(s$effets_CC_faibles)
decrit(s$effets_CC_graves)
decrit(s$effets_CC_desastreux)
decrit(s$effets_CC_cataclysmiques)
decrit(s$effets_CC_NSP)

summary(lm(s$effets_CC_insignifiant ~ s$info_CC + s$info_PM + (s$info_CC * s$info_PM), data=s, weights = s$weight))
summary(lm(s$effets_CC_faibles ~ s$info_CC + s$info_PM + (s$info_CC * s$info_PM), data=s, weights = s$weight))
summary(lm(s$effets_CC_graves ~ s$info_CC + s$info_PM + (s$info_CC * s$info_PM), data=s, weights = s$weight))
summary(lm(s$effets_CC_desastreux ~ s$info_CC + s$info_PM + (s$info_CC * s$info_PM), data=s, weights = s$weight))
summary(lm(s$effets_CC_cataclysmiques ~ s$info_CC + s$info_PM + (s$info_CC * s$info_PM), data=s, weights = s$weight))
summary(lm(s$effets_CC_NSP ~ s$info_CC + s$info_PM + (s$info_CC * s$info_PM), data=s, weights = s$weight))
# Nos informations n'ont pas d'effet significatif sur les croyances relatives aux effets du CC

### Logit ###
# Attention : l'instrument étant faible, on utilise une régression en faisant reposer l'identification sur l'usage des variables de contrôle
# La difficulté est d'inclure des contrôles suffisant, mais pas de "mauvais contrôles" qui captureraient une partie de l'effet que l'on souhaite identifier
s$dummy_taxe_efficace <- s$taxe_efficace != 'Non'
logit_taxe_approbation <- glm(taxe_approbation!='Non' ~ dummy_taxe_efficace + revenu + fioul + gaz + taille_menage + surface, data=s, family=binomial(link="logit"))
summary(logit_taxe_approbation) # TODO : ajouter les variables de contrôle pertinentes pour avoir une bonne spécification
margins(logit_taxe_approbation)


##### Non-working IVs #####
## Informer sur l'efficacité de la taxe convainc mais pas assez pour influer sur l'acceptation
# apres_modifs: rajout d'une info sur l'efficacité: "Les scientifiques s'accordent à dire qu'une taxe carbone serait efficace pour diminuer la pollution."
cor(s$info_efficacite, (s$taxe_efficace != 'Non'))
tsls_efficacite_1 <- lm((taxe_efficace != 'Non') ~ apres_modifs, data=s, weights = s$weight)
summary(tsls_efficacite_1)
taxe_efficace.hat <- fitted.values(tsls_efficacite_1)
summary(lm((taxe_approbation != 'Non') ~ taxe_efficace.hat, data=s, weights = s$weight))

tsls_efficacite_1 <- lm((taxe_efficace == 'Oui') ~ apres_modifs, data=s, weights = s$weight)
summary(tsls_efficacite_1)
taxe_efficace.hat <- fitted.values(tsls_efficacite_1)
summary(lm((taxe_approbation == 'Oui') ~ taxe_efficace.hat, data=s, weights = s$weight))

## Les autres 2SLS envisagés ne fonctionnent pas (faute de l'instrument)
decrit(s$effets_CC)
cor(n(s$effets_CC), n(s$info_CC), use="complete.obs") # -0.02
summary(lm(n(effets_CC) ~ info_CC, data=s, weights = s$weight))
# Nos informations n'ont pas d'effet significatif sur les croyances relatives aux effets du CC
summary(lm((as.factor(s$cause_CC)=='anthropique') ~ info_CC, data=s, weights = s$weight))
summary(lm(emission_cible ~ info_CC, data=s, weights = s$weight))
summary(lm(generation_CC_2020==T ~ info_CC, data=s, weights = s$weight))
summary(lm(responsable_CC_govts==T ~ info_CC, data=s, weights = s$weight))

summary(lm(responsable_CC_govts==T ~ info_CC, data=s, weights = s$weight))

##### Stats descriptives #####
decrit(s$taxe_efficace[s$apres_modifs==T], miss=T)
decrit(s$taxe_efficace[s$apres_modifs==FALSE], miss=T)
