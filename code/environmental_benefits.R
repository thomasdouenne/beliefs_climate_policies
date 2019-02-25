### Instrumental variable ###
s$dummy_approbation <- 1*(s$taxe_approbation=='Oui')

s$dummy_taxe_efficace <- 1 * (s$taxe_efficace == 'Oui')

y <- s$dummy_approbation
x <- s$dummy_taxe_efficace
z1 <- as.numeric(s$info_CC)
z2 <- as.numeric(s$info_PM)
c <- s$score_climate_call # This is just here as an example of control variable

# Simple OLS #
ols_approuve_efficace <- lm(y ~ x, data=s, weights = s$weight)
summary(ols_approuve_efficace) # Effet assez fort : +0.39, si l'on ne contrôle pour rien

# 2SLS #
cor(z1,x)
cor(z2,x) # Problème : weak instruments ! Nos informations ne changent pas leur croyance vis-à-vis de l'efficacité de la taxe

tsls1<-lm(x ~ z1 + z2)
summary(tsls1)

d.hat<-fitted.values(tsls1)
tsls2<-lm(y ~ c + d.hat)
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
