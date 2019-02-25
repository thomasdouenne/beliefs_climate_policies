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
summary(ols_approuve_efficace)

# 2SLS #
cor(z1,x)
cor(z2,x) # Problème : weak instruments ! Nos informations ne changent pas leur croyance vis-à-vis de l'efficacité de la taxe

tsls1<-lm(x ~ z1 + z2)
summary(tsls1)

d.hat<-fitted.values(tsls1)
tsls2<-lm(y ~ c + d.hat)
summary(tsls2)
# Parce que les instruments sont faibles, l'effet n'est pas significatif
