package('pwr')
package("foreign")
package("memisc")
package("DT")
package("pastecs")
package("lsr")
package("ggplot2")
package("stringr")
package("survey")
package("plotly")
package('gdata')
package('tidyverse')
package("Hmisc")

##### Dur√©es #####
decrit(s$duree/60) # 18 min (moyenne 24)
decrit(s$duree_depenses/60) # 2 min (moyenne 3.5)
decrit(s$duree_champ_libre/60) # 0.1 min (moyenne 1)
decrit(s$duree_info_CC_PM/60) # 0.4 min (moyenne 1)
decrit(s$duree_info_CC/60) # 0.3 min (moyenne 1.7)
decrit(s$duree_info_PM/60) # 0.2 min (moyenne 0.5)
decrit(s$duree_no_info/60) # 0.05 min (moyenne 0.1)
length(which(!is.na(s$depenses_confiant)))/nrow(s) # 27%
plot(density(sa$duree/60), xlim=c(0,30))
plot(Ecdf(sa$duree/60)$x, Ecdf(sa$duree/60)$ecdf)
sa$duree_min <- sa$duree/60
Ecdf(sa$duree_min) 


##### Caract√©ristiques √©nerg√©tiques #####
decrit(s$surface) # 120
decrit(s$chauffage, weights = s$weight) # 50% gaz ou fioul
decrit(s$km) # 11k (moyenne 15k)
decrit(s$conso) # 6
decrit(s$nb_vehicules)


##### Gain questions g√©n√©rales (TVA, transports, logement) #####
decrit(s$perte_tva)
decrit(s$perte_fuel)
decrit(s$perte_chauffage) # proportions similaires pour les 3, environ 60% pensent perdre plus que la moyenne


##### Gain (dividende - hausse d√©penses √©nerg√©tiques) #####
decrit(110 * s$nb_adultes - s$hausse_depenses) # Ok !
decrit(s$gain) 
decrit(s$gain_taxe)
decrit(s$gain_taxe_feedback)
decrit(s$gain_taxe_feedback[s$hausse_depenses < 110 * s$nb_adultes], weights = s$weight[s$hausse_depenses < 110 * s$nb_adultes]) # On ne les convainc pas !!!
length(which(s$gain_taxe_feedback=='Gagnant' & s$hausse_depenses < 110 * s$nb_adultes))/length(which(!is.na(s$gain_taxe_feedback) & s$hausse_depenses < 110 * s$nb_adultes))
length(which(s$gain_taxe_feedback=='Gagnant' & s$gain_taxe != 'Gagnant' & s$hausse_depenses < 110 * s$nb_adultes))/length(which(!is.na(s$gain_taxe_feedback) & s$hausse_depenses < 110 * s$nb_adultes & s$gain_taxe != 'Gagnant'))
decrit(s$gain_taxe_progressif)


##### Approbation #####
decrit(s$taxe_approbation) # 11% Dur !!!
decrit(s$taxe_approbation[s$gilets_jaunes_soutien==TRUE])
decrit(s$taxe_approbation[s$gilets_jaunes_oppose==TRUE])
decrit(s$taxe_feedback_approbation) # 17%
decrit(s$taxe_efficace) # 18%
decrit(s$taxe_progressif_approbation) # 19%
summary(lm((taxe_feedback_approbation=='Oui') ~ (gain_taxe!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_progressif_approbation=='Oui') ~ (gain_taxe!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_approbation=='Oui') ~ (taxe_efficace!='Non') + (gain_taxe!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_approbation=='Oui') ~ score_climate_call + score_polluants + (gain_taxe!='Perdant'), data=s, weights = s$weight))


##### Elasticites #####
decrit(s$Elasticite_fuel[!is.na(s$Elasticite_fuel)]) # To do : rÈsoudre problËme avec le calcul des ÈlasticitÈs (moyenne ‡ -17...)
decrit(s$Elasticite_fuel_perso[!is.na(s$Elasticite_fuel_perso)]) #
decrit(s$Elasticite_chauffage[!is.na(s$Elasticite_chauffage)]) #
decrit(s$Elasticite_chauffage_perso[!is.na(s$Elasticite_chauffage_perso)]) #

summary(lm(Elasticite_fuel ~ (taxe_efficace=='Oui'), data=s, weights = s$weight))
summary(lm(Elasticite_chauffage ~ (taxe_efficace=='Oui'), data=s, weights = s$weight)) # Aucun lien √©vident √©lasticit√© / efficacit√© environnementale

cor(s$Elasticite_fuel[!is.na(s$Elasticite_fuel)], s$Elasticite_fuel_perso[!is.na(s$Elasticite_fuel_perso)])
cor(s$Elasticite_chauffage[!is.na(s$Elasticite_chauffage)], s$Elasticite_chauffage_perso[!is.na(s$Elasticite_chauffage_perso)])
# Correlation positive entre ÈlasticitÈ perso et ÈlasticitÈ globale


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
decrit(s$enfant_CC[s$sexe=='Masculin'])
decrit(s$enfant_CC[s$sexe=='FÈminin'])
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
decrit(s$score_polluants)
decrit(s$score_climate_call)
decrit(s$emission_cible) # 5


##### Transferts inter #####
decrit(s$transferts_inter)
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info, data = s, weights = s$weight)) # 0 !
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info, data = s, subset = transferts_inter!='NSP', weights = s$weight)) # 0 !
decrit(s$variation_aide, weights = s$weight)


##### D√©penses publiques #####
categories_depenses <- c("sante", "education", "retraites", "securite", "recherche", "justice", "armee", "protection", "infrastructures", "loisirs", "aide")
i <- 0
for (v in categories_depenses) {
  print(summary(lm(s[[paste('variation', v, sep='_')]] ~ s[[paste('dep', i, 'en_position', sep='_')]], weights=s$weight)))
  i <- i+1 }
# *** pour justice


##### Miscellanous #####
decrit(s$rattrapage_diesel, miss = T) # TODO: correlation avec avoir diesel
decrit(s$hausse_diesel) # This needs to be fixed !
summary(lm((rattrapage_diesel!='Non') ~ (diesel==TRUE), data=s, weights = s$weight))
for (j in names(s)) if (grepl('gilets_jaunes', j)) print(decrit(s[[j]], weights=s$weight))


##### Approbation politiques environnementales #####
decrit(s$si_pauvres)
for (j in names(s)) if (grepl('si_', j)) print(decrit(s[[j]], weights=s$weight))
for (j in 141:148) print(decrit(s[[j]], weights=s$weight))
