##### RÃ©gressions: progressivitÃ© #####
decrit(s$progressivite[s$info_progressivite==T])
decrit(s$progressivite[s$info_progressivite==FALSE])
# On ne convainc pas les rÃ©pondants que la taxe est progressive !

tsls1_progressivite <- lm((progressivite!='Non') ~ info_progressivite, data=s, weights = s$weight, na.action="na.exclude")
summary(tsls1_progressivite)
progressivite.hat <- fitted.values(tsls1_progressivite)

# OLS: 0.56***
summary(lm((taxe_info_approbation!='Non') ~ (progressivite!='Non'), data=s, weights = s$weight)) # TODO: rajouter contrÃ´les
# OLS forme rÃ©duite comme 2SLS ne fonctionnent pas
summary(lm((taxe_info_approbation!='Non') ~ progressivite.hat, data=s, weights = s$weight))
summary(lm((taxe_info_approbation!='Non') ~ info_progressivite, data=s, weights = s$weight))
# petit effet: 0.03. sur la diffÃ©rence d'acceptation
summary(lm(((taxe_info_approbation!='Non') - (taxe_approbation!='Non')) ~ info_progressivite, data=s, weights = s$weight))

# OLS1: 9*** p.p. Peut-Ãªtre que mÃªme sans changer d'avis, le 5/6 les convainc que leur groupe va gagner, donc que c'est progressif
tsls1_progressivite <- lm((progressivite!='Non') ~ simule_gagnant + (gagnant_categorie!='Perdant') + simule_gain + taxe_approbation, data=s, weights = s$weight, na.action="na.exclude")
summary(tsls1_progressivite)
progressivite.hat <- fitted.values(tsls1_progressivite)
# OLS: 138*** p.p. 
summary(lm((taxe_info_approbation!='Non') ~ progressivite.hat + (gagnant_categorie!='Perdant') + simule_gain + taxe_approbation, data=s, weights = s$weight))
