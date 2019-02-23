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

##### Durées #####
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


##### Caractéristiques énergétiques #####
decrit(s$surface, weights = s$weight) # 120
decrit(s$chauffage, weights = s$weight) # 50% gaz ou fioul
decrit(s$km, weights = s$weight) # 11k (moyenne 15k)
decrit(s$conso, weights = s$weight) # 6
decrit(s$nb_vehicules, weights = s$weight)


##### Gain questions générales (TVA, transports, logement) #####
decrit(s$perte_tva, weights = s$weight)
decrit(s$perte_fuel, weights = s$weight)
decrit(s$perte_chauffage, weights = s$weight) # proportions similaires pour les 3, environ 60% pensent perdre plus que la moyenne


##### Gain (dividende - hausse dépenses énergétiques) #####
decrit(110 * s$nb_adultes - s$hausse_depenses, weights = s$weight) # Ok !
decrit(s$gain, weights = s$weight) 
decrit(s$gain_taxe, weights = s$weight)
decrit(s$gain_taxe_feedback, weights = s$weight)
decrit(s$gain_taxe_feedback[s$hausse_depenses < 110 * s$nb_adultes], weights = s$weight[s$hausse_depenses < 110 * s$nb_adultes]) 
# On ne les convainc pas !!!
length(which(s$gain_taxe_feedback=='Gagnant' & s$hausse_depenses < 110 * s$nb_adultes))/length(which(!is.na(s$gain_taxe_feedback) & s$hausse_depenses < 110 * s$nb_adultes))
length(which(s$gain_taxe_feedback=='Gagnant' & s$gain_taxe != 'Gagnant' & s$hausse_depenses < 110 * s$nb_adultes))/length(which(!is.na(s$gain_taxe_feedback) & s$hausse_depenses < 110 * s$nb_adultes & s$gain_taxe != 'Gagnant'))
decrit(s$gain_taxe_progressif, weights = s$weight)


##### Approbation #####
decrit(s$taxe_approbation, weights = s$weight) # 11% Dur !!!
decrit(s$taxe_feedback_approbation, weights = s$weight) # 17%
decrit(s$taxe_efficace, weights = s$weight) # 18%
decrit(s$taxe_progressif_approbation, weights = s$weight) # 19%
summary(lm((taxe_feedback_approbation=='Oui') ~ (gain_taxe!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_progressif_approbation=='Oui') ~ (gain_taxe!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_approbation=='Oui') ~ (taxe_efficace!='Non') + (gain_taxe!='Perdant'), data=s, weights = s$weight))
summary(lm((taxe_approbation=='Oui') ~ score_climate_call + score_polluants + (gain_taxe!='Perdant'), data=s, weights = s$weight))

##### Elasticites #####
decrit(s$Elasticite_fuel) # -0.42
decrit(s$Elasticite_fuel_perso) # -0.29
decrit(s$Elasticite_chauffage) # -0.41
decrit(s$Elasticite_chauffage_perso) # -0.20
summary(lm(Elasticite_fuel ~ (taxe_efficace=='Oui'), data=s, weights = s$weight))
summary(lm(Elasticite_chauffage ~ (taxe_efficace=='Oui'), data=s, weights = s$weight)) # Aucun lien évident élasticité / efficacité environnementale


##### Ciblage #####
decrit(s$categorie_cible, weights = s$weight)
decrit(s$taxe_cible_approbation, weights = s$weight)
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


##### Gaz de schiste #####
decrit(s$schiste_approbation, weights = s$weight)
decrit(s$schiste_approbation, miss=T, weights = s$weight)
decrit(s$schiste_traite, weights = s$weight)
decrit(s$schiste_avantage, weights = s$weight)
decrit(s$schiste_CC, weights = s$weight)
summary(lm((schiste_approbation=='Oui') ~ (schiste_traite==0), data=s, weights = s$weight)) # Encore rien de très significatif
summary(lm((schiste_avantage=='Cela permettrait de créer des emplois et dynamiser les départements concernés') ~ (schiste_traite==1), data=s, subset=schiste_approbation=='Oui' & schiste_avantage !='Aucune de ces deux raisons', weights = s$weight))
summary(lm((schiste_CC=='Elle est valable : toute baisse des émissions va dans la bonne direction') ~ (schiste_traite==1), data=s, subset=schiste_approbation=='Oui' & schiste_CC !='NSP (Ne sait pas, ne se prononce pas)', weights = s$weight))


##### Engagement personnel ######
decrit(s$mode_vie_ecolo, weights = s$weight) # 79% !
decrit(s$mode_vie_ecolo, miss=T, weights = s$weight) # 65%
decrit(s$enfant_CC, weights = s$weight) # TODO: H/F
decrit(s$enfant_CC_pour_CC[s$enfant_CC=='Oui'], weights = s$weight[s$enfant_CC=='Oui'])
decrit(s$enfant_CC_pour_lui[s$enfant_CC=='Oui'], weights = s$weight[s$enfant_CC=='Oui'])
summary(lm((enfant_CC=='Oui') ~ sexe, data=s))


##### Connaissances CC #####
decrit(s$ges_avion, weights = s$weight) # 47%
decrit(s$ges_boeuf, weights = s$weight) # 47%
decrit(s$ges_nucleaire, weights = s$weight) # 48%
decrit(s$ges_co2, weights = s$weight) # 81%
decrit(s$ges_ch4, weights = s$weight) # 48%
decrit(s$ges_o2, weights = s$weight) # 6%
decrit(s$ges_pm, weights = s$weight) # 58%
decrit(s$score_polluants, weights = s$weight)
decrit(s$score_climate_call, weights = s$weight)
decrit(s$emission_cible, weights = s$weight) # 5


##### Transferts inter #####
decrit(s$transferts_inter, weights = s$weight)
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info, data = s, weights = s$weight)) # 0 !
summary(lm((transferts_inter=='Oui') ~ transferts_inter_info, data = s, subset = transferts_inter!='NSP', weights = s$weight)) # 0 !
decrit(s$variation_aide, weights = s$weight)


##### Dépenses publiques #####
categories_depenses <- c("sante", "education", "retraites", "securite", "recherche", "justice", "armee", "protection", "infrastructures", "loisirs", "aide")
i <- 0
for (v in categories_depenses) {
  print(summary(lm(s[[paste('variation', v, sep='_')]] ~ s[[paste('dep', i, 'en_position', sep='_')]], weights=s$weight)))
  i <- i+1 }
# *** pour justice


##### Miscellanous #####
decrit(s$rattrapage_diesel, miss = T, weights = s$weight) # TODO: correlation avec avoir diesel
decrit(s$hausse_diesel, weights = s$weight) # This needs to be fixed !
summary(lm((rattrapage_diesel!='Non') ~ (diesel==TRUE), data=s, weights = s$weight))
for (j in names(s)) if (grepl('gilets_jaunes', j)) print(decrit(s[[j]], weights=s$weight))


##### Approbation politiques environnementales #####
for (j in names(s)) if (grepl('si_', j)) print(decrit(s[[j]], weights=s$weight))
for (j in 141:148) print(decrit(s[[j]], weights=s$weight))
