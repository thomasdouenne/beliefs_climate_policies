##### Effet simple sur l'approbation ####

# Effet de info_CC mais pas de info_PM
summary(lm(taxe_approbation!='Non' ~ info_CC * info_PM, data=s, weights = s$weight))

# pas d'interaction entre info_PM et taille_agglo
summary(lm(taxe_approbation!='Non' ~ info_CC * info_PM + info_PM * (taille_agglo==5), data=s, weights = s$weight))
summary(lm(taxe_approbation!='Non' ~ info_CC * info_PM + info_PM * (taille_agglo>2), data=s, weights = s$weight))
summary(lm(taxe_approbation!='Non' ~ info_CC * info_PM + info_PM * taille_agglo, data=s, weights = s$weight))
summary(lm(taxe_approbation!='Non' ~ info_CC * info_PM + info_PM * info_CC * (taille_agglo==5), data=s, weights = s$weight))

# pas d'effet de l'info sur l'efficacités$t
summary(lm(taxe_approbation!='Non' ~ apres_modifs, data=s, weights = s$weight))


##### IV: A ~ efficace ~ info_CC/PM #####
# 2SLS avec benefices_CC: effet de 110* p.p. !
decrit(s$taxe_efficace, weights = s$weight)
tsls1<-lm(taxe_efficace != 'Non' ~ info_CC * info_PM, data=s, weights = s$weight)
summary(tsls1)
taxe_efficace.hat <- fitted.values(tsls1)
summary(lm(taxe_approbation!='Non' ~ taxe_efficace.hat, data=s, weights = s$weight))
# Simple OLS: 0.51 cf. Logit pour les contrôles
summary(lm(taxe_approbation!='Non' ~ (taxe_efficace != 'Non'), data=s, weights = s$weight))
summary(lm((s$taxe_approbation == 'Oui') ~ (s$taxe_efficace == 'Oui'), data=s, weights = s$weight)) # 0.43

# 2SLS avec benefices_CC: pareil, effet de 106** p.p. 
decrit(s$benefices_CC, weights = s$weight)
tsls1_env <- lm(benefices_CC==T ~ info_CC, data=s, weights = s$weight)
summary(tsls1_env)
benefices_CC.hat <- fitted(tsls1_env)
summary(lm(taxe_approbation!='Non' ~ benefices_CC.hat, data=s, weights = s$weight))
# OLS: 0.37***
summary(lm(taxe_approbation!='Non' ~ benefices_CC, data=s, weights = s$weight))


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