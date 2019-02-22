##### Durées #####
decrit(s$duree/60) # 18 min (moyenne 24)
decrit(s$duree_depenses/60) # 2 min (moyenne 3.5)
decrit(s$duree_champ_libre/60) # 0.1 min (moyenne 1)
decrit(s$duree_info_CC_PM/60) # 0.4 min (moyenne 1)
decrit(s$duree_info_CC/60) # 0.3 min (moyenne 1.7)
decrit(s$duree_info_PM/60) # 0.2 min (moyenne 0.5)
decrit(s$duree_no_info/60) # 0.05 min (moyenne 0.1)
length(which(!is.na(s$depenses_confiant)))/nrow(s) # 27%


##### Caractéristiques énergétiques #####
decrit(s$surface) # 120
decrit(s$chauffage) # 50% gaz ou fioul
decrit(s$km) # 11k (moyenne 15k)
decrit(s$conso) # 6
decrit(s$nb_vehicules)


##### Gain (dividende - hausse dépenses énergétiques) #####
decrit(110 * s$nb_adultes - s$hausse_depenses) # Ok !
decrit(s$gain) 
decrit(s$gain_taxe)
decrit(s$gain_taxe_feedback)
decrit(s$gain_taxe_feedback[s$hausse_depenses < 110 * s$nb_adultes]) # On ne les convainc pas !!!
decrit(s$gain_taxe_progressif)


##### Approbation #####
decrit(s$taxe_approbation) # 11% Dur !!!
decrit(s$taxe_feedback_approbation) # 17%
decrit(s$taxe_progressif_approbation) # 19%
summary(lm((taxe_feedback_approbation=='Oui') ~ (gain_taxe!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_progressif_approbation=='Oui') ~ (gain_taxe!='Perdant'), data=s, weights = s$weight))


##### Ciblage #####
decrit(s$categorie_cible)
decrit(s$taxe_cible_approbation)
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==20), data=s, subset=categorie_cible=='20_30', weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==30), data=s, subset=categorie_cible=='30_40', weights = s$weight)) # *
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==40), data=s, subset=categorie_cible=='40_50', weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==20), data=s, subset=(categorie_cible=='20_30' & taxe_cible_approbation!='NSP'), weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==30), data=s, subset=(categorie_cible=='30_40' & taxe_cible_approbation!='NSP'), weights = s$weight)) # *
summary(lm((taxe_cible_approbation=='Oui') ~ (cible==40), data=s, subset=(categorie_cible=='40_50' & taxe_cible_approbation!='NSP'), weights = s$weight))

summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==20, weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==30, weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==40, weights = s$weight)) # *
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==50, weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==20 & taxe_cible_approbation!='NSP', weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==30 & taxe_cible_approbation!='NSP', weights = s$weight))
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==40 & taxe_cible_approbation!='NSP', weights = s$weight)) # *
summary(lm((taxe_cible_approbation=='Oui') ~ categorie_cible, data=s, subset=cible==50 & taxe_cible_approbation!='NSP', weights = s$weight))


##### Enfant ######
decrit(s$enfant_CC)
decrit(s$enfant_CC_pour_CC[s$enfant_CC=='Oui'])
decrit(s$enfant_CC_pour_lui[s$enfant_CC=='Oui'])


##### Connaissances CC #####
decrit(s$ges_avion) # 47%
decrit(s$ges_boeuf) # 47%
decrit(s$ges_nucleaire) # 48%
decrit(s$ges_co2) # 81%
decrit(s$ges_ch4) # 48%
decrit(s$ges_o2) # 6%
decrit(s$ges_pm) # 58%
decrit(s$emission_cible) # 5
