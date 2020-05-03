## Source to reproduce "French Attitudes over Climate Change and Climate Policies"
#     by Thomas Douenne & Adrien Fabre, under licence CC-BY
# All files can be found on github: https://github.com/bixiou/beliefs_climate_policies
#     in particular, file to prepare the dataset (preparation.R), to load the packages and functions (packages_functions.R),
#     R environment with all the data prepared (.RData) and python files for complementary computations

source("packages_functions.R")
load(".RData")

# /!\ for numerical items with NSP, subset=!is.missing() in regressions (parle_CC, transports_..., gilets_jaunes, perte_relative_...) TODO: remove this? check analyse.R/Trash for TODOs

##### 2. Survey and data #####


##### 3. Attitudes over Climate Change #####
## 3.1 Knowledge
decrit(s$cause_CC, miss=T, weights = s$weight) # 72% anthropogenic, 20% natural, 3% doesn't exist
decrit(s$ges_CO2, weights = s$weight) # 77%
decrit(s$ges_O2, weights = s$weight) # 4%
decrit(s$ges_CH4, weights = s$weight) # 48%
decrit(s$ges_pm, weights = s$weight) # 61%
decrit(s$ges_avion, weights = s$weight) # 59%
decrit(s$ges_boeuf, weights = s$weight) # 46%
decrit(s$ges_nucleaire, weights = s$weight) # 50%
decrit(s$score_climate_call, weights = s$weight)

decrit(s$region_CC, weights = s$weight) # 65% as much, 29% India, 6% UE
decrit(s$emission_cible, weights = s$weight) # 5
decrit(s$emission_cible >= 5, weights = s$weight) # 59%
decrit(s$emission_cible <= 2, weights = s$weight) # 17%

decrit(s$generation_CC_min, weights = s$weight) #1960-2050: 11%-27%-43%-19%
decrit(s$generation_CC_min >= 2020, weights = s$weight) # 62%

# Figures 2-7
barres(file="CC_cause_nolegend", title="", thin=T, data=dataN("cause_CC"), nsp=T, sort=T, legend = c("Anthropogenic", "Natural", "Does not exist", "PNR"), labels=c(" "), show_ticks = F)
ges_climate_call <- rev(paste("ges_correct", c("avion", "nucleaire", "boeuf", "O2", "CO2", "CH4", "pm"), sep="_")) 
labels_ges_climate_call <- rev(c("Plane vs. train", "Nuclear vs. wind", "Beef vs. pasta", "Oxygen", "CO<sub>2</sub>", "Methane", "Particulates")) 
oui_non(margin_l=40, ges_climate_call, NSP=FALSE, colors=color(3)[1:2], en=c("Correct", "Wrong"), labels = labels_ges_climate_call, sort=FALSE) # colors=color(3)[1:2], 
s$region_CC <- as.factor(s$region_CC)
s$region_CC <- relevel(relevel(s$region_CC, "Autant dans les deux"), "L'Inde")
barres(file="CC_region_nolegend", title="", data=dataN("region_CC", miss=FALSE), nsp=FALSE, sort=T, show_ticks = F, thin=T,
       legend = c("India", "As much in both", "European Union", "NSP"), labels=c(" "))
barres(file="CC_target_emission_nolegend", title="", data=dataN("emission_cible", miss=FALSE), nsp=FALSE, sort=T, color = rev(brewer.pal(11, "RdBu")), 
       legend = dataN("emission_cible", return="levels"), labels=c(" ")) 
barres(file="CC_generation_min_nolegend", title="", rev_color = T, data=dataN("generation_CC_min"), nsp=T, sort=T, thin=T,
       legend = c(dataN("generation_CC_min", return="levels")[1:4], "PNR"), labels=c(" "))
barres(file="CC_effects_nolegend", title="", thin=T, data=dataN("effets_CC"), nsp=T, sort=T, 
       legend = c("Insignificant", "Small", "Serious", "Disastrous", "Cataclysmic", "PNR"), labels=c(" "))

## 3.2 Opinions
decrit(s$effets_CC, miss=T, weights = s$weight) # 18% cataclysmiques; 28% désastreux, 35% graves
decrit(s$parle_CC, miss=T, weights = s$weight) # 3 thirds
decrit(s$responsable_CC_chacun, miss=T, weights = s$weight) # 63%
decrit(s$responsable_CC_govts, miss=T, weights = s$weight) # 47%
decrit(s$responsable_CC_etranger, miss=T, weights = s$weight) # 42%
decrit(s$responsable_CC_riches, miss=T, weights = s$weight) # 25%
decrit(s$responsable_CC_nature, miss=T, weights = s$weight) # 23%
decrit(s$responsable_CC_passe, miss=T, weights = s$weight) # 21%

# Figures 8-9
s$parle_CC_factor <- as.factor(s$parle_CC)
s$parle_CC_factor <- relevel(relevel(s$parle_CC_factor, "Plusieurs fois par an"), "Plusieurs fois par mois")
barres(file="CC_talks_nolegend", title="", data=dataN("parle_CC_factor"), nsp=T, sort=T, show_ticks = F, thin = T,
       legend = c("Several times per month", "Several times per year", "Almost never", "PNR"), labels=c(" ")) 
labels_resp <- c("Each one of us", "Governments", "Certain foreign countries", "The richest", "Natural causes", "Past generations")
barres(file="CC_responsible", title="", data=data1(names(s)[which(grepl("responsable_CC", names(s)))]), sort=T, showLegend=FALSE, labels=labels_resp, hover=labels_resp)

## 3.3 Reaction needed
decrit(s$mode_vie_ecolo, miss=T, weights = s$weight) # 65%
decrit(s$changer_deja_fait==T | s$changer_essaie==T, weights=s$weight) # 36%
decrit(s$changer_non_interet == T | s$changer_non_riches==T | s$changer_non_negation==T, weights=s$weight) # 11%
decrit(s$changer_non_interet, weights=s$weight) # 2%
decrit(s$changer_si_moyens == T | s$changer_si_politiques==T | s$changer_si_tous==T, weights=s$weight) # 82%
decrit(s$changer_si_tous, weights=s$weight) # 47%
decrit(s$changer_si_moyens, weights=s$weight) # 45%
decrit(s$changer_si_politiques, weights=s$weight) # 43%
decrit(s$changer_deja_fait, weights=s$weight) # 23%
decrit(s$changer_essaie, weights=s$weight) # 15%
decrit(s$changer_non_negation, weights=s$weight) # 5%
decrit(s$changer_non_riches, weights=s$weight) # 5%

decrit(s$ecologiste, weights=s$weight) # 15% écologistes
decrit(s$changer_deja_fait, weights=s$weight) # 23%
decrit(s$enfant_CC, weights = s$weight) # 20% oui
decrit(s$enfant_CC_pour_lui[s$enfant_CC=='Oui'], weights = s$weight[s$enfant_CC=='Oui']) # 86%
decrit(s$enfant_CC_pour_CC[s$enfant_CC=='Oui'], weights = s$weight[s$enfant_CC=='Oui']) # 37%

# Figure 10
variables_changer <- names(s)[which(grepl("changer", names(s)))]
labels_changer <- c("Yes, if policies were going in this direction", "Yes, if I had the financial means", "Yes, if everyone was doing the same",
                    "No, only the richest must change it", "No, it is against my personal interest", "No, because CC is not a real problem",
                    "No, I already adopted a sustainable way of life", "I try but I have difficulties changing my habits")
barres(file="change_if_no", title="", data=data1(variables_changer), sort=T, showLegend=FALSE, labels=labels_changer, hover=labels_changer, margin_l=250)


##### 4. Attitudes over Carbon Tax and Dividend #####
## 4.1 Massive rejection
decrit(s$taxe_approbation, miss=T, weights=s$weight) # 10% Yes, 70% No
wtd.mean((s$gain_chauffage<0)[(s$fioul | s$gaz) == FALSE & s$mode_chauffage=='individuel' & s$chauffage!='NSP'], 
         weights = s$weight[(s$fioul | s$gaz) == FALSE & s$mode_chauffage=='individuel' & s$chauffage!='NSP']) # 30%: ce n'est pas dû au mode de chauffage
decrit(s$si_compensee %in% c('Plutôt', 'Tout à fait'), weights = s$weight) # 37%
# Figure 11
barres(file="approval", title="", data=matrix(dataN("taxe_approbation")[c(2,1,3)], ncol=1), legend = c("Yes", "No", "PNR"), labels = c(" "))

## 4.2 Perceived winners and losers
decrit(s$taxe_gagnant_riches, weights=s$weight) # 30%
decrit(s$taxe_perdant_riches, weights=s$weight) # 2%
wtd.mean(s$taxe_perdant_pauvres==T, s$weight) - wtd.mean(s$taxe_gagnant_pauvres==T, s$weight) # 40%
wtd.mean(s$taxe_perdant_moyennes==T, s$weight) - wtd.mean(s$taxe_gagnant_moyennes==T, s$weight) # 53%
decrit(s$taxe_perdant_riches, weights=s$weight) # 2%
decrit(s$taxe_gagnant_citadins, weights=s$weight) # 19%
decrit(s$taxe_perdant_ruraux, weights=s$weight) # 34%

s$nb_taxe_gagnant <- 0
for (v in variables_taxe_gagnant) s$nb_taxe_gagnant[s[[v]]==T] <- 1 + s$nb_taxe_gagnant[s[[v]]==T]
decrit(s$nb_taxe_gagnant) # 1.16
s$nb_taxe_perdant <- 0
for (v in variables_taxe_perdant) s$nb_taxe_perdant[s[[v]]==T] <- 1 + s$nb_taxe_perdant[s[[v]]==T]
decrit(s$nb_taxe_perdant) # 1.74

# Figures 12
variables_winners <- names(s)[which(grepl("taxe_gagnant_", names(s)))]
labels_winners <- c("No one", "The poorest", "The middle class", "The richest", "Everyone", "City dwellers", "Certain persons, but no specific income category", "PNR (Don't know, don't want to answer)")
barres(file="tax_winners_synchro", title="", data=data1(variables_winners), sort=T, showLegend=FALSE, labels=labels_winners, hover=labels_winners, xrange=c(0, 0.58), margin_l=270)
variables_losers <- names(s)[which(grepl("taxe_perdant_", names(s)))]
labels_losers <- c("No one", "The poorest", "The middle class", "The richest", "Everyone", "Rural or peri-urban households", "Certain persons, but no specific income category", "PNR (Don't know, don't want to answer)")
barres(file="tax_losers_synchro", title="", data=data1(variables_losers), sort=T, showLegend=FALSE, labels=labels_losers, hover=labels_losers, xrange=c(0, 0.58), margin_l=270)

## 4.3 Perceived pros and cons
decrit(s$problemes_ruraux, weights=s$weight) # 47%
decrit(s$problemes_pauvres, weights=s$weight) # 29%
decrit(s$problemes_pretexte, weights=s$weight) # 43%
decrit(s$problemes_inefficace, weights=s$weight) # 37%
decrit(s$problemes_alternatives, weights=s$weight) # 31%
decrit(s$problemes_revenu, weights=s$weight) # 31%
decrit(s$problemes_economie, weights=s$weight) # 14%
decrit(s$problemes_aucun, weights=s$weight) # 9%
decrit(s$problemes_autre_choix, weights=s$weight) # 2%

decrit(s$benefices_aucun, weights=s$weight) # 44%
decrit(s$benefices_CC, weights=s$weight) # 30%
decrit(s$benefices_sante, weights=s$weight) # 27%
decrit(s$benefices_enjeu, weights=s$weight) # 16%
decrit(s$benefices_independance, weights=s$weight) # 11%
decrit(s$benefices_pauvres, weights=s$weight) # 10%
decrit(s$benefices_circulation, weights=s$weight) # 10%
decrit(s$benefices_revenu, weights=s$weight) # 8%
decrit(s$benefices_autre_choix, weights=s$weight) # 5%

decrit(s$nb_benefices) # 1.14
decrit(s$nb_problemes) # 2.36
decrit(s$nb_problemes - s$nb_benefices) # 15% more benefits, 27% as much, 58% more problems

# Figure 13
variables_benefits <- names(s)[which(grepl("benefice", names(s)))[which(grepl("problemes", names(s)))>300]]
variables_benefits <- variables_benefits[!(variables_benefits %in% c("nb_benefices", "benefices_autre"))]
labels_benefits <- c("Fights CC", "Reduces negative impact of pollution on health", "Reduces traffic congestion", "Increases my purchasing power", 
                     "Increases purchasing power of the poorest",
                     "Increases France's independence toward fossils", "Prepares the economy for tomorrow", "None of these reasons", "Other reasons")
barres(file="CC_benefits_synchro", title="", data=data1(variables_benefits), sort=T, showLegend=FALSE, labels=labels_benefits, hover=labels_benefits, xrange=c(0, 0.47), margin_l=261) # pb 35% NSP
variables_problems <- names(s)[which(grepl("problemes", names(s)))]
variables_problems <- variables_problems[!(variables_problems %in% c("nb_problemes", "problemes_autre"))]
labels_problems <- c("Is ineffective to reduce pollution", "Alternatives are insufficient or too expensive", "Penalizes rural households", "Decreases my purchasing power",
                     "Penalizes the poorest", "Hurts the economy", "Is a pretext to increase taxes", "None of these reasons", "Other reasons")
barres(file="CC_problems_synchro", title="", data=data1(variables_problems), sort=T, showLegend=FALSE, labels=labels_problems, hover=labels_problems, xrange=c(0, 0.47), margin_l=261)

## 4.4 Consumption and mobility constraints
# 4.4.1 Perceived elasticities
decrit(s$Elasticite_fuel_perso==0, weights=s$weight) # 54%
decrit(s$Elasticite_chauffage_perso==0, weights=s$weight) # 61%
decrit(s$elasticite_fuel_perso[s$Elasticite_fuel_perso==0], weights=s$weight[s$Elasticite_fuel_perso==0]) # 64%
decrit(s$elasticite_chauffage_perso[s$Elasticite_chauffage_perso==0], weights=s$weight[s$Elasticite_chauffage_perso==0]) # 61%
decrit(s$Elasticite_fuel, weights=s$weight) # -0.45
decrit(s$Elasticite_fuel_perso, weights=s$weight*s$depense_carburants) # -0.36
decrit(s$Elasticite_chauffage, weights=s$weight) # -0.43
decrit(s$Elasticite_chauffage_perso, weights=s$weight*s$depense_chauffage) # -0.33

# Figure 14
s$elast_chauffage_perso <- factor(s$elasticite_chauffage_perso, levels(as.factor(s$elasticite_chauffage_perso))[c(1,6:2)])
s$elast_fuel_perso <- factor(s$elasticite_fuel_perso, levels(as.factor(s$elasticite_fuel_perso))[c(1,6:2)])
s$elast_chauffage_perso <- revalue(s$elast_chauffage_perso, c("+ de 30% - Je changerais largement ma consommation"="> 30%", 
                                                              "de 20% à 30%"="20 to 30%", "de 10% à 20%"="10 to 20%", "de 0% à 10%"="0 to 10%", 
                                                              "0% - Je ne la réduirais pas"="0%: constrained", "0% - Je n'en consomme déjà pas"="0%: don't consume")) # won't reduce
barres(file="elasticities_perso", thin=T, title="", data=dataKN(c("elast_chauffage_perso", "elast_fuel_perso"), miss=FALSE), 
       nsp=FALSE, labels=c("Own: Housing", "Own: Transport"), legend = c("", "same as above", "", dataN("elast_chauffage_perso", return="legend")[4:6]), show_ticks=T)
s$elast_chauffage <- factor(s$elasticite_chauffage, levels(as.factor(s$elasticite_chauffage))[c(1,4,3,5,2)])
s$elast_fuel <- factor(s$elasticite_fuel, levels(as.factor(s$elasticite_fuel))[c(1,4,3,5,2)])
s$elast_chauffage <- revalue(s$elast_chauffage, c("+ de 30%"="> 30%", "de 20% à 30%"="20 to 30%", 
                                                  "de 10% à 20%"="10 to 20%", "de 0% à 3%"="0 to 3%", "de 3% à 10%"="3 to 10%"))
barres(file="elasticities_agg", thin=T, title="", data=dataKN(c("elast_fuel", "elast_chauffage"), miss=FALSE), color=color(6)[1:5],
       nsp=FALSE, labels=c("Aggregate: Transport", "Aggregate: Housing"), legend = dataN("elast_chauffage", return="legend"), show_ticks=T)


# 4.4.2 Mobility and public transportdecrit(s$transports_avis[s$transports_avis!=-1]<2, weights = s$weight[s$transports_avis!=-1])
decrit(s$transports_travail[s$transports_travail!='Non concerné·e'], weights = s$weight[s$transports_travail!='Non concerné·e']) # 65%
decrit(entd$Mode[entd$dist_subj_km <= 2 & entd$dist_subj_km >= 1], weights = entd$weight[entd$dist_subj_km <= 2 & entd$dist_subj_km >= 1]) # 52%

decrit(s$transports_distance <= 10, weights = s$weight) # 73%
decrit(s$transports_travail_commun=='Non' & s$transports_travail_actif=='Non', weights = s$weight) # 58%
decrit(s$transports_travail_commun=='Oui, aucun pb' | s$transports_travail_actif=='Oui, aucun pb', weights = s$weight) # 15%
decrit(s$transports_avis %in% c('Insuffisante', 'Convenable'), weights = s$weight)  # 52%
decrit(s$transports_avis %in% c('Limitée', 'Satisfaisante'), weights = s$weight)  # 40%

# Fgures 15-19
data_transports_use <- dataKN(c("transports_travail", "transports_courses", "transports_loisirs"))
data_transports_use[3,] <- data_transports_use[3,] + data_transports_use[5,]
barres(file="transports_use", title="", thin=T, nsp=T, data=data_transports_use[c(1,4,3,2,6),], legend=c("Walk/bike", "Public transport", "Other", "Car", "Unconcerned"), labels=c("Work", "Shopping", "Leisure"))
s$transports_minutes <- "NSP"
s$transports_minutes[s$transports_distance <= 5] <- "5 ou moins"
s$transports_minutes[s$transports_distance > 5 & s$transports_distance <= 10] <- "6 à 10"
s$transports_minutes[s$transports_distance > 10 & s$transports_distance <= 20] <- "11 à 20"
s$transports_minutes[s$transports_distance > 20] <- "plus que 20"
barres(file="transports_distance", title="", thin=T, data=matrix(dataN("transports_minutes")[c(2,3,1,4,5),1], ncol=1), labels=c(" "), legend=c("5 or less", "6 to 10", "11 to 20", "more than 20", "PNR"))
barres(file="transports_frequency", title="", thin=T, nsp=T, data=matrix(dataN("transports_frequence")[c(4:1,5),1], ncol=1), legend=rev(c("PNR", "< 3/day", "1/hour - 4/day", "1/h - 2/h", "> 3/h")), labels=(" "))
# Sans changer de logement ni de lieu de travail, il serait possible pour le répondant prenant sa voiture de prendre les transports en commun pour ses trajets domicile-travail
barres(file="transports_work", title="", thin=T, nsp=T, data=dataKN(c("transports_travail_commun", "transports_travail_actif"))[c(2,3,1,4),], legend=c("Yes, no difficulty", "Yes, but bothering", "No", "PNR"), color=color(6, grey=T)[c(1,2,5,6)], labels=c("Public transport", "Walk or bike"))
barres(file="transports_opinions", thin=T, title="", data=matrix(dataN("transports_avis")[c(4:1,5),], ncol=1),  legend=rev(c("PNR", "Insufficient", "Limited, but enough", "Decent, but not enough", "Satisfactory")), labels=c(" "))


##### 5. Attitudes over Other Policies #####
## 5.1 Preferred revenue recycling
for (variable in variables_taxe_condition) {  cat(variable);  print(decrit(s[variable], weights=s$weight)) }

# Figure 20
labels_tax_condition <- c("a payment for the 50% poorest French<br> (those earning less than 1670€/month)", "a payment to all French people", 
                          "compensation for households forced to consume petroleum products", "a reduction in social contributions", "a VAT cut", 
                          "a reduction in the public deficit", "the thermal renovation of buildings", "renewable energies (wind, solar, etc.)", "non polluting transport")
labels_tax_condition[3] <- "compensation for households constrained<br> to consume petroleum products"
barres(file="tax_condition_val", title="", data=data5(names(s)[which(names(s)=='si_pauvres'):(which(names(s)=='si_pauvres')+8)], miss=FALSE, rev=T)[,rev(c(9,5,8,7,3,4,1,6,2))], nsp=FALSE, 
       sort=F, thin=T, legend = rep("", 5), labels=labels_tax_condition[rev(c(9,5,8,7,3,4,1,6,2))], margin_l=220) # rev(yes_no5)

## 5.2 Other instruments
# Favored environmental policies
for (variable in variables_politiques_environnementales) {  cat(variable);  print(decrit(s[variable], weights=s$weight)) }

# Figure 21
labels_environmental_policies <- c("a tax on kerosene (aviation)", "a tax on red meat", "stricter insulation standards for new buildings", 
                                   "stricter standards on pollution from new vehicles", "stricter standards during roadworthiness tests", 
                                   "the prohibition of polluting vehicles in city centres", "the introduction of urban tolls", "a contribution to a global climate fund")
barres(file="environmental_policies", title="", data=data5(names(s)[(which(names(s)=='si_pauvres')+10):(which(names(s)=='si_pauvres')+17)], 
                                                           miss=FALSE, rev = T)[,rev(c(3,4,1,6,5,8,2,7))], nsp=FALSE, sort=F, legend = rep("", 5), labels=labels_environmental_policies[rev(c(3,4,1,6,5,8,2,7))], thin=T) # rev(yes_no5)

# Diesel taxation
decrit(s$rattrapage_diesel, miss=T, weights=s$weight) # 59% non, 29% oui
decrit(s$diesel, weights=s$weight) # 57%
decrit(s[s$diesel==T,]$rattrapage_diesel, miss=T, weights=s[s$diesel==T,]$weight) # 80% non, 12% oui
decrit(s[s$taille_agglo=='rural',]$rattrapage_diesel, miss=T, weights=s[s$taille_agglo=='rural',]$weight) # 73% No
decrit(s[s$taille_agglo=='Paris',]$rattrapage_diesel, miss=T, weights=s[s$taille_agglo=='Paris',]$weight) # 40% No
# Figure 22
barres(file="diesel_catch_up", dataKN(c("rattrapage_diesel")), thin=F, show_ticks = T, nsp=TRUE, legend=c("Yes", "No", "PNR"), labels = c(" "))

# Shale gas (cf. Appendix for regression)
decrit(s$schiste_approbation, miss=T, weights=s$weight) # 59% non, 16% oui
decrit(s$schiste_avantage, miss=T, weights=s$weight) # 56% aucun, 26% emplois, 18% CC
decrit(s$schiste_CC, miss=T, weights=s$weight) # 43% malvenue, 25% valable
# Figure 23
barres(file="shale_val_nolegend", dataKN(c("schiste_approbation")), nsp=TRUE, legend=c("Yes", "No", "PNR"), labels = c(" "))


##### 6. Determinants #####
## 6.1 Attitudes over CC
s$existe <- 1-1*(s$cause_CC=="n'existe pas")
s$proximite_cible <- 3 - (s$emission_cible > 2) - (s$emission_cible > 4) - (s$emission_cible > 6)
s$inde <- 1*(s$region_CC=="L'Inde")

factanal(s[,c("score_ges", "score_climate_call", "anthropique", "existe", "proximite_cible", "inde")], 1)
s$connaissances_EFA <- 0.212*s$score_ges + 0.182*s$score_climate_call + 0.601*s$anthropique + 0.398*s$existe + 0.20*s$proximite_cible
s$connaissances_CC <- s$score_ges + s$score_climate_call + 3*(s$cause_CC=='anthropique') - 2*(s$cause_CC=="n'existe pas") + 3 - (s$emission_cible > 2) - (s$emission_cible > 4) - (s$emission_cible > 6)
cor(s$connaissances_EFA, s$connaissances_CC) # 0.999
decrit(s$connaissances_CC, weights = s$weight) # -2 (0) to +13 (22), quartiles: 6 8 9, mean 7.6, sd 2.5
decrit(s$connaissances_CC)
sd(s$connaissances_CC)
s$connaissances_CC <- (s$connaissances_CC - mean(s$connaissances_CC))/sd(s$connaissances_CC)

decrit(s$Gauche_droite, weights=s$weight) # 40% indeterminate

# Figure 24
s$anthropique <- s$cause_CC=='anthropique'
s$mode_vie_ecolo_oui <- s$mode_vie_ecolo=='Oui'
s$male <- s$sexe=='Masculin'
s$ecolo <- s$ecologiste==T
data_cor <- s[,c("anthropique", "connaissances_CC", "effets_CC", "parle_CC", "mode_vie_ecolo_oui", "nb_politiques_env", "tax_acceptance", "ecolo", "diplome4", "age", "taille_agglo", "Revenu")] # , "gauche_droite", "gilets_jaunes"
names(data_cor) <- c("Anthropogenic", "Knowledge", "Perceived gravity", "Frequency of talks", "Ecological lifestyle", "# of policies supported", "Tax acceptance", "Ecologist", "Diploma", "Age", "Size of town", "Income")
corr <- cor(data_cor, use="complete.obs")
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(data_cor) # corrplot does not work when some packages are loaded before 'corrplot' => if it doesn't work, restart R and load only corrplot.
corrplot(corr, method='color', p.mat = p.mat, sig.level = 0.01, diag=FALSE, tl.srt=35, tl.col='black', insig = 'blank', addCoef.col = 'black', addCoefasPercent = T , type='upper') #, order='hclust'

# Table I
variables_determinants_attitudes <- c("Revenu", "Revenu_conjoint", "Gilets_jaunes", "as.factor(diplome4)", # as.factor(ifelse(is.missing(s$Gilets_jaunes), 'NA', as.character(s$Gilets_jaunes)))
                            "(nb_adultes==1)", variables_demo, variables_politiques, variables_energie, variables_mobilite) # 
variables_determinants_attitudes <- variables_determinants_attitudes[!(variables_determinants_attitudes %in% c("revenu", "rev_tot", "niveau_vie", "age", "age_18_24", "diplome4",
                                                                                 names(s)[which(grepl("Chauffage", names(s)))], names(s)[which(grepl("Mode_chauffage", names(s)))],
                                                                                 names(s)[which(grepl("hausse_", names(s)))]))]
variables_determinants_attitudes_CC <- variables_determinants_attitudes[c(21, 27, 3, 28, 4, 17:20, 1, 6, 15, 39)]
for (v in variables_determinants_attitudes) if (!(v %in% variables_determinants_attitudes_CC)) variables_determinants_attitudes_CC <- c(variables_determinants_attitudes_CC, v)

s$Gauche_droite <- relevel(s$Gauche_droite, 'Indeterminate')

# (1) Cause of CC (anthropogenic or not)
formula_determinants_cause <- as.formula(paste("cause_CC=='anthropique' ~ ", paste(variables_determinants_attitudes_CC, collapse = ' + ')))
cause_ols1 <- lm(formula_determinants_cause, data=s, weights = s$weight)
summary(cause_ols1)

# (2) Cause of CC: diploma, age
cause_ols2 <- lm(cause_CC=='anthropique' ~ age_25_34 + age_35_49 + age_50_64 + age_65_plus, data=s, weights = s$weight)
summary(cause_ols2)

# (3) Cause of CC: politics
cause_ols3 <- lm(cause_CC=='anthropique' ~ Gauche_droite + as.factor(diplome4) + diplome4 * gauche_droite_na + diplome4 * indeterminate, data=s, weights = s$weight)
summary(cause_ols3)

# (4) Knowledge of CC
formula_determinants_connaissances <- as.formula(paste("connaissances_CC ~ ", paste(variables_determinants_attitudes_CC, collapse = ' + ')))
knowledge_ols1 <- lm(formula_determinants_connaissances, data=s, weights = s$weight)
summary(knowledge_ols1)

# (5) Effects of CC
formula_determinants_effets <- as.formula(paste("(effets_CC > 2) ~  ", paste(variables_determinants_attitudes_CC, collapse = ' + ')))
effects_ols1 <- lm(formula_determinants_effets, data=s, weights = s$weight)
summary(effects_ols1)

# (6) Effects of CC: 
effects_ols2 <- lm((effets_CC > 2) ~ Gauche_droite + as.factor(diplome4) + diplome4 * gauche_droite_na + diplome4 * indeterminate, data=s, weights = s$weight) # diplome4 ou connaissances_CC ?
summary(effects_ols2)

Table_determinants_attitudes_CC <- stargazer(cause_ols1, cause_ols2, cause_ols3, knowledge_ols1, effects_ols1, effects_ols2,
                                   title="Determinants of attitudes towards climate change (CC).", model.names = FALSE, model.numbers = T, 
                                   covariate.labels = c("Interest in politics (0 to 2)", "Ecologist", "Yellow Vests: PNR", "Yellow Vests: understands", 
                                                        "Yellow Vests: supports", "Yellow Vests: is part", "Left-right: Extreme-left", "Left-right: Left", 
                                                        "Left-right: Center", "Left-right: Right", "Left-right: Extreme-right", "Diploma: \\textit{CAP} or \\textit{BEP}", 
                                                        "Diploma: \\textit{Baccalauréat}", "Diploma: Higher", "Age: 25 -- 34","Age: 35 -- 49","Age: 50 -- 64", "Age: $\\geq$ 65", 
                                                        "Income (k\\euro{}/month)", "Sex: Male", "Size of town (1 to 5)", "Frequency of public transit", "Diploma $\\times$ Left-right", "Diploma $\\times$ Left-right: Indeterminate"),
                                   header = FALSE, dep.var.labels = c("CC is anthropogenic", "Knowledge about CC", "CC is disastrous"),  dep.var.caption = "", 
                                   keep = c("sexe", "Revenu$", "age_", "\\(diplome", "diplome4:", "taille_agglo", "Gilets_jaunes", "ecologiste", 
                                            "Gauche_droite", "interet_politique", "transports_frequence"), # "humaniste", , "transports_avis", "conso"
                                   add.lines = list(c("Additional covariates & \\checkmark &  &  & \\checkmark & \\checkmark &  \\\\ ")),
                                   no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:determinants_attitudes_CC")
write_clip(gsub('\\end{table}', '}{\\\\ $\\quad$ \\\\                \\footnotesize \\textsc{Note:} Standard errors are reported in parentheses. Interaction term is computed using numeric variables. Omitted modalities are: \\textit{Yellow Vests: opposes}, \\textit{Left-right: Indeterminate}, \\textit{Diploma: Brevet or no diploma}, \\textit{Age: 18 -- 24}. Additional covariates are defined in \\ref{app:covariates}. }                \\end{table*} ', 
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', gsub('\\begin{table}', '\\begin{table*}',
                                                       Table_determinants_attitudes_CC, fixed=TRUE), fixed=TRUE), fixed=T), collapse=' ')


## 6.2 Attitudes over policies
normes_environnementales <- c("normes_isolation", "normes_vehicules", "controle_technique", "interdiction_polluants")
taxes_environnementales <- c("taxe_kerosene", "taxe_viande", "peages_urbains", "fonds_mondial")
s$normes_vs_taxes <- 0
for (v in normes_environnementales) s$normes_vs_taxes <- s$normes_vs_taxes + s[[v]]
for (v in taxes_environnementales) s$normes_vs_taxes <- s$normes_vs_taxes - s[[v]]

earmarked <- c("si_renovation", "si_renouvelables", "si_transports")
compensations <- c("si_pauvres", "si_compensee", "si_contraints")
s$earmarked_vs_compensation <- 0
for (v in earmarked) s$earmarked_vs_compensation <- s$earmarked_vs_compensation + s[[v]]
for (v in compensations) s$earmarked_vs_compensation <- s$earmarked_vs_compensation - s[[v]]

decrit(s$normes_vs_taxes) # mean 2.8, sd 3.3
decrit(s$earmarked_vs_compensation) # mean 1.4, sd 3.1
s$normes_vs_taxes_normalized <- (s$normes_vs_taxes - mean(s$normes_vs_taxes))/sd(s$normes_vs_taxes)
s$earmarked_vs_normalized <- (s$earmarked_vs_compensation - mean(s$earmarked_vs_compensation))/sd(s$earmarked_vs_compensation)

# Table II
variables_determinants_policy <- c("Revenu", "Revenu_conjoint", "connaissances_CC", "(effets_CC > 2)", "Gilets_jaunes",
                            "(nb_adultes==1)", variables_demo, variables_politiques, variables_energie, variables_mobilite) # 
variables_determinants_policy <- variables_determinants_policy[!(variables_determinants_policy %in% c("revenu", "rev_tot", "niveau_vie", "age", "age_18_24",
                                                                                 names(s)[which(grepl("Chauffage", names(s)))], names(s)[which(grepl("Mode_chauffage", names(s)))],
                                                                                 names(s)[which(grepl("hausse_", names(s)))]))]
variables_determinants_policy_CC <- variables_determinants_policy[c(3, 4, 23, 29, 5, 30, 11, 19:22, 1, 7, 17, 41)]
for (v in variables_determinants_policy) if (!(v %in% variables_determinants_policy_CC)) variables_determinants_policy_CC <- c(variables_determinants_policy_CC, v)

variables_determinants_policy_CC_bis <- variables_determinants_policy_CC[!(variables_determinants_policy_CC %in% c("Gilets_jaunes", "Gauche_droite", "connaissances_CC", "interet_politique", "ecologiste"))]
variables_determinants_policy_CC_ter <- c("connaissances_CC", "(effets_CC > 2)", "diplome4", "age_25_34", "age_35_49", "age_50_64", "age_65_plus", "Revenu", "sexe", "taille_agglo", "transports_frequence")

s$Gauche_droite <- relevel(s$Gauche_droite, 'Indeterminate')
formula_determinants_taxe_approbation <- as.formula(paste("taxe_approbation!='Non' ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_taxe_approbation <- lm(formula_determinants_taxe_approbation, data=s, weights = s$weight)
summary(ols_taxe_approbation)

formula_determinants_taxe_approbation_bis <- as.formula(paste("taxe_approbation!='Non' ~ ", paste(variables_determinants_policy_CC_ter, collapse = ' + ')))
ols_taxe_approbation_bis <- lm(formula_determinants_taxe_approbation_bis, data=s, weights = s$weight)
summary(ols_taxe_approbation_bis)

formula_determinants_nb_politiques_env <- as.formula(paste("nb_politiques_env/8 ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_nb_politiques_env <- lm(formula_determinants_nb_politiques_env, data=s, weights = s$weight)
summary(ols_nb_politiques_env)

formula_determinants_mode_vie_ecolo <- as.formula(paste("mode_vie_ecolo == 'Oui' ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_mode_vie_ecolo <- lm(formula_determinants_mode_vie_ecolo, data=s, weights = s$weight)
summary(ols_mode_vie_ecolo)

formula_determinants_normes_vs_taxes <- as.formula(paste("normes_vs_taxes_normalized ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_normes_vs_taxes <- lm(formula_determinants_normes_vs_taxes, data=s, weights = s$weight)
summary(ols_normes_vs_taxes)

formula_determinants_earmarked_vs_compensation <- as.formula(paste("earmarked_vs_compensation_normalized ~ ",  paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_earmarked_vs_compensation <- lm(formula_determinants_earmarked_vs_compensation, data=s, weights = s$weight)
summary(ols_earmarked_vs_compensation)

Table_politiques_env <- stargazer(ols_taxe_approbation, ols_taxe_approbation_bis, ols_nb_politiques_env, ols_normes_vs_taxes, ols_earmarked_vs_compensation, ols_mode_vie_ecolo,
                                   title="Determinants of attitudes towards climate policies", model.names = FALSE, model.numbers = T, 
                                  covariate.labels = c("Knowledge about CC", "CC is disastrous", "Interest in politics (0 to 2)", "Ecologist", "Yellow Vests: PNR", "Yellow Vests: understands",
                                                       "Yellow Vests: supports", "Yellow Vests: is part", "Left-right: Extreme-left", "Left-right: Left",
                                                       "Left-right: Center", "Left-right: Right", "Left-right: Extreme-right", "Diploma (1 to 4)",
                                                       "Age: 25 -- 34","Age: 35 -- 49","Age: 50 -- 64", "Age: $\\geq$ 65",
                                                       "Income (k\\euro{}/month)", "Sex: Male", "Size of town (1 to 5)", "Frequency of public transit"),
                                   header = FALSE, dep.var.labels = c("Tax \\& dividend", "Share of policies", "norms vs. taxes", "earmarking vs. transfers", "ecological lifestyle"),  dep.var.caption = "", 
                                   keep = c("Revenu$", "effets_CC", "connaissances_CC", "sexe", "age_", "diplome", "_agglo", "interet_politique", "Gilets_jaunes", "ecologiste", "Gauche_droite", "transports_frequence"), 
                                   add.lines = list(c("Additional covariates & \\checkmark & & \\checkmark  & \\checkmark & \\checkmark & \\checkmark  \\\\ ")),
                                   no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:politiques_env")
write_clip(gsub('\\end{table}', '} \\\\ \\quad \\\\ {\\footnotesize \\textsc{Note:} Standard errors are reported in parentheses. Omitted variables are \\textit{Yellow Vests: opposes}, \\textit{Age : 18 -- 24} and \\textit{Left-right: Indeterminate}. Additional covariates are defined in \\ref{app:covariates}.} \\end{table*}', 
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', gsub('\\begin{table}', '\\begin{table*}',
                                                  gsub('\\\\[-1.8ex] & \\multicolumn{2}{c}{Tax \\& dividend} & Share of policies & norms vs. taxes & earmarking vs. transfers & ecological lifestyle \\\\',
                                                       '\\\\[-1.8ex] & \\multicolumn{2}{c}{Acceptance of} & Share of policies & Norms & Earmarking & Ecological \\\\ \\\\[-1.8ex] & \\multicolumn{2}{c}{Tax \\& dividend} & approved & vs. taxes & vs. transfers & lifestyle \\\\',
                                                      Table_politiques_env, fixed=TRUE), fixed=TRUE), fixed=T), fixed=T), collapse=' ')

# Yellow Vests and ecologists favor norms over taxes and earmarking over transfers
decrit(s$normes_vs_taxes, weights=s$weight)
decrit(s$normes_vs_taxes[s$gilets_jaunes_dedans==T | s$gilets_jaunes_soutien==T], weights=s$weight[s$gilets_jaunes_dedans==T | s$gilets_jaunes_soutien==T])
decrit(s$normes_vs_taxes[s$ecologiste==T], weights=s$weight[s$ecologiste==T])
decrit(s$earmarked_vs_compensation, weights=s$weight)
decrit(s$earmarked_vs_compensation[s$gilets_jaunes_dedans==T | s$gilets_jaunes_soutien==T], weights=s$weight[s$gilets_jaunes_dedans==T | s$gilets_jaunes_soutien==T])
decrit(s$earmarked_vs_compensation[s$ecologiste==T], weights=s$weight[s$ecologiste==T])


##### Appendix #####
# TODO: clean Appendix


## Winners/losers in purchasing power:
gagnant_pauvres_monetaire_1 <- lm((s$taxe_gagnant_pauvres==T) ~ variante_monetaire, data=s, weights=s$weight)
summary(gagnant_pauvres_monetaire_1) # 0.058 / +0.044

gagnant_citadins_monetaire_2 <- lm((s$taxe_gagnant_citadins==T) ~ variante_monetaire, data=s, weights=s$weight)
summary(gagnant_citadins_monetaire_2) # 0.207 / -0.029 *

perdant_riches_monetaire_3 <- lm((s$taxe_perdant_riches==T) ~ variante_monetaire, data=s, weights=s$weight)
summary(perdant_riches_monetaire_3) # 0.009 / +0.012 **

perdant_ruraux_monetaire_4 <- lm((s$taxe_perdant_ruraux==T) ~ variante_monetaire, data=s, weights=s$weight)
summary(perdant_ruraux_monetaire_4) # 0.352 / -0.014

Table_variante_monetaire <- stargazer(gagnant_pauvres_monetaire_1, gagnant_citadins_monetaire_2, perdant_riches_monetaire_3, perdant_ruraux_monetaire_4,
                             title="Effect of defining winners/losers in terms of purchasing power", model.names = FALSE, #star.cutoffs = c(0.1, 1e-5, 1e-30),
                             covariate.labels = c("Constant", "In purchasing power"), 
                             dep.var.labels = c("Poors expected to win", "City dwellers expected to win", "Rich expected to lose", "Rural expected to lose"),# dep.var.caption = "", header = FALSE,
                             keep = c("Constant", "variante_monetaire"),
                             column.labels = c("(1)", "(2)", "(3)", "(4)"), model.numbers = FALSE,
                             no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="table:purchasing_power")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', 
                                                       '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_variante_monetaire, fixed=TRUE), fixed=TRUE), collapse=' ')


## Diesel
# variables_diesel <- c("Revenu", "score_ges", "score_climate_call", "as.factor(taille_aggl)", variables_demo, variables_energie) # 
# variables_diesel <- variables_diesel[!(variables_diesel %in% c("revenu", "rev_tot", "age", "age_65_plus",
#                                                                names(s)[which(grepl("Chauffage", names(s)))], names(s)[which(grepl("Mode_chauffage", names(s)))],
#                                                                names(s)[which(grepl("hausse_", names(s)))]))]
variables_determinants_diesel <- c("Revenu", "Revenu_conjoint", "connaissances_CC", "as.factor(taille_agglo)", "Gilets_jaunes",
                                   "(nb_adultes==1)", variables_demo, variables_politiques, variables_energie, variables_mobilite) # 
variables_determinants_diesel <- variables_determinants_diesel[!(variables_determinants_diesel %in% c("revenu", "rev_tot", "niveau_vie", "taille_agglo", "age", "age_18_24",
                                                                                                      names(s)[which(grepl("Chauffage", names(s)))], names(s)[which(grepl("Mode_chauffage", names(s)))],
                                                                                                      names(s)[which(grepl("hausse_", names(s)))]))]
variables_determinants_diesel_CC <- variables_determinants_diesel[c(3, 28, 5, 29, 4, 33, 34, 35, 40)]
for (v in variables_determinants_diesel) if (!(v %in% variables_determinants_diesel_CC)) variables_determinants_diesel_CC <- c(variables_determinants_diesel_CC, v)

formula_diesel <- as.formula(paste("rattrapage_diesel!='Non' ~",
                                   paste(variables_determinants_diesel_CC, collapse = ' + ')))
ols_diesel_1 <- lm(formula_diesel, data=s, weights = s$weight)
summary(ols_diesel_1) # Strongest determinant: use of Diesel / Scores ges and CC also matter

ols_diesel_2 <- lm("rattrapage_diesel!='Non' ~ diesel + as.factor(taille_agglo)", data=s, weights = s$weight)
summary(ols_diesel_2)

ols_diesel_3 <- lm("rattrapage_diesel!='Non' ~ Gilets_jaunes", data=s, weights = s$weight)
summary(ols_diesel_3)

ols_diesel_4 <- lm("rattrapage_diesel!='Non' ~ Gauche_droite", data=s, weights = s$weight)
summary(ols_diesel_4)

Table_diesel <- stargazer(ols_diesel_1, ols_diesel_2, ols_diesel_3, ols_diesel_4, # TODO: object 'ols_diesel_4' not found
                                  title="Determinants of attitudes towards diesel taxation", model.names = FALSE, model.numbers = T, 
                                  covariate.labels = c("Knowledge about CC", "Ecologist", "Yellow Vests: PNR", "Yellow Vests: understands", 
                                                       "Yellow Vests: supports", "Yellow Vests: is part", "Left-right: Extreme-left", "Left-right: Left", 
                                                       "Left-right: Center", "Left-right: Right", "Left-right: Extreme-right",
                                                       "Size of town: -20k", "Size of town: 20-100k", "Size of town: +100k", "Size of town: Paris",
                                                       "Diesel", "Gasoline", "Number vehicles", "Frequency of public transit"),
                                  header = FALSE, dep.var.labels = c("Acceptance increase in diesel taxation"),  dep.var.caption = "", 
                                  keep = c("connaissances_CC", "_agglo", "Gilets_jaunes", "ecologiste", "Gauche_droite", "transports_frequence", "diesel", "essence", "nb_vehicules"), 
                                  add.lines = list(c("Additional covariates & \\checkmark &  &  &  \\\\ ")), # => remove empty line in latex
                                  no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:determinants_diesel")
write_clip(gsub('\\end{table}', '} \\\\ \\quad \\\\ {\\footnotesize \\textsc{Note:} Standard errors are reported in parentheses. Omitted variables are \\textit{Yellow Vests: opposes}, \\textit{Age : 18 -- 24} and \\textit{Left-right: Indeterminate}. Additional covariates are defined in Appendix C.} \\end{table*}', 
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', gsub('\\begin{table}', '\\begin{table*}',
                                                                                                 gsub('\\\\[-1.8ex] & Acceptance increase in diesel taxation & \\multicolumn{3}{c}{NA} \\\\',
                                                                                                      '\\\\[-1.8ex] & \\multicolumn{3}{c}{Acceptance increase in diesel taxation} \\\\',
                                                                                                      Table_diesel, fixed=TRUE), fixed=TRUE), fixed=T), fixed=T), collapse=' ')


# logit_diesel <- glm(formula_diesel, family = binomial(link='logit'), data=s)
# summary(logit_diesel)
# logit_diesel_margins <- logitmfx(formula_diesel, s, atmean=FALSE)$mfxest
# logit_diesel_margins

## Shale gas
reg_shale_1 <- lm((schiste_approbation!='Non') ~ (schiste_traite==1), data=s, weights = s$weight)
summary(reg_shale_1) # - 3.9 p.p. acceptance when treated
variables_reg_schiste <- c("Revenu", "score_ges", "score_climate_call", variables_demo) # 
variables_reg_schiste <- variables_reg_schiste[!(variables_reg_schiste %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
formula_schiste_approbation <- as.formula(paste("schiste_approbation!='Non' ~ (schiste_traite==1) + ",
                                                paste(variables_reg_schiste, collapse = ' + ')))
reg_shale_2 <- lm(formula_schiste_approbation, data=s, weights = s$weight)
summary(reg_shale_2) # - 5.1 p.p. acceptance when treated / Scores, Sex and Education matter
logit_shale_3 <- glm(formula_schiste_approbation, family = binomial(link='logit'), data=s)
summary(logit_shale_3)
logit_shale_3_margins <- logitmfx(formula_schiste_approbation, s, atmean=FALSE)$mfxest
logit_shale_3_margins # -5.7 p.p. with logit
summary(lm((schiste_approbation=='Oui') ~ (schiste_traite==1), data=s, weights = s$weight)) # Not significant for approval

decrit(s$schiste_approbation)
Table_shale_gas <- stargazer(reg_shale_1, reg_shale_2, logit_shale_3, 
                             title="Effect of being treated on acceptance of shale gas exploitation", model.names = TRUE, #star.cutoffs = c(0.1, 1e-5, 1e-30),
                             covariate.labels = c("Treated"), 
                             dep.var.labels = c("Shale gas exploitation: not ``No''"),# dep.var.caption = "", header = FALSE,
                             keep = c("schiste_traite"),
                             coef = list(NULL, NULL, logit_shale_3_margins[,1]), 
                             se = list(NULL, NULL, logit_shale_3_margins[,2]),
                             column.labels = c("(1)", "(2)", "(3)"), model.numbers = FALSE,
                             add.lines = list(c("Controls: Socio-demographics, scores", "", "\\checkmark", "\\checkmark")),
                             no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="table:shale_gas")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', 
                                                       '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', Table_shale_gas, fixed=TRUE), fixed=TRUE), collapse=' ')


## Additional specifications determinants of policy approval
variables_determinants_policy_with_less_controls <- c("connaissances_CC", "(effets_CC > 2)", "diplome4", "age_25_34", "age_35_49", "age_50_64", "age_65_plus", "Revenu", "sexe", "taille_agglo", "transports_frequence")

formula_determinants_nb_politiques_env_bis <- as.formula(paste("nb_politiques_env/8 ~ ", paste(variables_determinants_policy_with_less_controls, collapse = ' + ')))
ols_nb_politiques_env_bis <- lm(formula_determinants_nb_politiques_env_bis, data=s, weights = s$weight)
summary(ols_nb_politiques_env_bis)

ols_nb_politiques_env_left_right <- lm("nb_politiques_env/8 ~ Gauche_droite", data=s)
summary(ols_nb_politiques_env_left_right)

ols_tax_and_dividend_left_right <- lm("taxe_approbation!='Non' ~ Gauche_droite", data=s)
summary(ols_tax_and_dividend_left_right)

Table_politiques_env_additional <- stargazer(ols_nb_politiques_env_bis, ols_nb_politiques_env_left_right, ols_tax_and_dividend_left_right,
                                  title="Determinants of attitudes towards climate policies, additional specifications", model.names = FALSE, model.numbers = T, 
                                  covariate.labels = c("Knowledge about CC", "CC is disastrous", "Diploma (1 to 4)", 
                                                       "Age: 25 -- 34","Age: 35 -- 49","Age: 50 -- 64", "Age: $\\geq$ 65", 
                                                       "Income (k\\euro{}/month)", "Sex: Male", "Size of town (1 to 5)", "Frequency of public transit",
                                                       "Left-right: Extreme-left", "Left-right: Left", 
                                                       "Left-right: Center", "Left-right: Right", "Left-right: Extreme-right"),
                                  header = FALSE, dep.var.labels = c("Share of policies", "Tax \\& dividend"),  dep.var.caption = "", 
                                  keep = c("Revenu$", "effets_CC", "connaissances_CC", "sexe", "age_", "diplome", "_agglo", "interet_politique", "Gilets_jaunes", "ecologiste", "Gauche_droite", "transports_frequence"), 
                                  #add.lines = list(c("Additional covariates & \\checkmark & & \\checkmark  & \\checkmark & \\checkmark & \\checkmark  \\\\ ")),
                                  no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:politiques_env")
write_clip(gsub('\\end{table}', '} \\\\ \\quad \\\\ {\\footnotesize \\textsc{Note:} Standard errors are reported in parentheses. Omitted variables are \\textit{Yellow Vests: opposes}, \\textit{Age : 18 -- 24} and \\textit{Left-right: Indeterminate}. Additional covariates are defined in Appendix C.} \\end{table*}', 
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', gsub('\\begin{table}', '\\begin{table*}',
                                                                                                 gsub('\\\\[-1.8ex] & \\multicolumn{2}{c}{Tax \\& dividend} & Share of policies & norms vs. taxes & earmarking vs. transfers & ecological lifestyle \\\\',
                                                                                                      '\\\\[-1.8ex] & \\multicolumn{2}{c}{Acceptance of} & Share of policies & Norms & Earmarking & Ecological \\\\ \\\\[-1.8ex] & \\multicolumn{2}{c}{Tax \\& dividend} & approved & vs. taxes & vs. transfers & lifestyle \\\\',
                                                                                                      Table_politiques_env_additional, fixed=TRUE), fixed=TRUE), fixed=T), fixed=T), collapse=' ')


# Attitudes over carbon tax recycling
variables_determinants_policy <- c("Revenu", "Revenu_conjoint", "connaissances_CC", "(effets_CC > 2)", "Gilets_jaunes",
                                   "(nb_adultes==1)", variables_demo, variables_politiques, variables_energie, variables_mobilite) # 
variables_determinants_policy <- variables_determinants_policy[!(variables_determinants_policy %in% c("revenu", "rev_tot", "niveau_vie", "age", "age_18_24",
                                                                                                      names(s)[which(grepl("Chauffage", names(s)))], names(s)[which(grepl("Mode_chauffage", names(s)))],
                                                                                                      names(s)[which(grepl("hausse_", names(s)))]))]
variables_determinants_policy_CC <- variables_determinants_policy[c(3, 4, 23, 29, 5, 30, 11, 19:22, 1, 7, 17, 41)]
for (v in variables_determinants_policy) if (!(v %in% variables_determinants_policy_CC)) variables_determinants_policy_CC <- c(variables_determinants_policy_CC, v)

formula_determinants_si_transports <- as.formula(paste("si_transports ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_si_transports <- lm(formula_determinants_si_transports, data=s, weights = s$weight)
summary(ols_si_transports)

formula_determinants_si_baisse_tva <- as.formula(paste("si_baisse_tva ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_si_baisse_tva <- lm(formula_determinants_si_baisse_tva, data=s, weights = s$weight)
summary(ols_si_baisse_tva)

formula_determinants_si_renouvelables <- as.formula(paste("si_renouvelables ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_si_renouvelables <- lm(formula_determinants_si_renouvelables, data=s, weights = s$weight)
summary(ols_si_renouvelables)

formula_determinants_si_renovation <- as.formula(paste("si_renovation ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_si_renovation <- lm(formula_determinants_si_renovation, data=s, weights = s$weight)
summary(ols_si_renovation)

formula_determinants_si_contraints <- as.formula(paste("si_contraints ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_si_contraints <- lm(formula_determinants_si_contraints, data=s, weights = s$weight)
summary(ols_si_contraints)

formula_determinants_si_baisse_cotsoc <- as.formula(paste("si_baisse_cotsoc ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_si_baisse_cotsoc <- lm(formula_determinants_si_baisse_cotsoc, data=s, weights = s$weight)
summary(ols_si_baisse_cotsoc)

formula_determinants_si_pauvres <- as.formula(paste("si_pauvres ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_si_pauvres <- lm(formula_determinants_si_pauvres, data=s, weights = s$weight)
summary(ols_si_pauvres)

formula_determinants_si_baisse_deficit <- as.formula(paste("si_baisse_deficit ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_si_baisse_deficit <- lm(formula_determinants_si_baisse_deficit, data=s, weights = s$weight)
summary(ols_si_baisse_deficit)

formula_determinants_si_compensee <- as.formula(paste("si_compensee ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_si_compensee <- lm(formula_determinants_si_compensee, data=s, weights = s$weight)
summary(ols_si_compensee)


Table_modes_recyclage <- stargazer(ols_si_transports, ols_si_baisse_tva, ols_si_renouvelables, ols_si_renovation, ols_si_contraints, ols_si_baisse_cotsoc, ols_si_pauvres, ols_si_baisse_deficit, ols_si_compensee,
                                  title="Determinants of attitudes towards carbon tax revenue recycling", model.names = FALSE, model.numbers = T, 
                                  covariate.labels = c("Knowledge about CC", "CC is disastrous", "Interest in politics (0 to 2)", "Ecologist", "Yellow Vests: PNR", "Yellow Vests: understands", 
                                                       "Yellow Vests: supports", "Yellow Vests: is part", "Left-right: Extreme-left", "Left-right: Left", 
                                                       "Left-right: Center", "Left-right: Right", "Left-right: Extreme-right", "Diploma (1 to 4)", 
                                                       "Age: 25 -- 34","Age: 35 -- 49","Age: 50 -- 64", "Age: $\\geq$ 65", 
                                                       "Income (k\\euro{}/month)", "Sex: Male", "Size of town (1 to 5)", "Frequency of public transit"),
                                  header = FALSE, dep.var.labels = c("Transports", "VAT", "Renewables", "Renovation", "Constrained", "CotSoc", "Poors", "Deficit", "Compensated"),  dep.var.caption = "", 
                                  keep = c("Revenu$", "effets_CC", "connaissances_CC", "sexe", "age_", "diplome", "_agglo", "interet_politique", "Gilets_jaunes", "ecologiste", "Gauche_droite", "transports_frequence"), 
                                  add.lines = list(c("Additional covariates & \\checkmark & \\checkmark & \\checkmark  & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark  \\\\ ")),
                                  no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:politiques_env")
write_clip(gsub('\\end{table}', '} \\\\ \\quad \\\\ {\\footnotesize \\textsc{Note:} Standard errors are reported in parentheses. Omitted variables are \\textit{Yellow Vests: opposes}, \\textit{Age : 18 -- 24} and \\textit{Left-right: Indeterminate}. Additional covariates are defined in Appendix C.} \\end{table*}', 
                gsub('\\begin{tabular}{@', '\\hspace*{-1.3cm} \\resizebox{1.15\\columnwidth}{!}{ \\begin{tabular}{@', gsub('\\begin{table}', '\\begin{table*}',
                                                                                                 gsub('\\\\[-1.8ex] & Transports & VAT & Renewables & Renovation & Constrained & CotSoc & Poors & Deficit & Compensated \\\\',
                                                                                                      '\\\\[-1.8ex] & Non-polluting & VAT & Renewable & Renovation & Transfer & Reduction & Transfer & Reduction & Transfer \\\\ \\\\[-1.8ex] & transports & cut & energies & of buildings & constrained hh. & soc. contri. & poor hh. & pub. deficit & all hh. \\\\',
                                                                                                      Table_modes_recyclage, fixed=TRUE), fixed=TRUE), fixed=T), fixed=T), collapse=' ')


# Attitudes over alternative climate policies, by policy
formula_determinants_normes_isolation <- as.formula(paste("normes_isolation ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_normes_isolation <- lm(formula_determinants_normes_isolation, data=s, weights = s$weight)
summary(ols_normes_isolation)

formula_determinants_normes_vehicules <- as.formula(paste("normes_vehicules ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_normes_vehicules <- lm(formula_determinants_normes_vehicules, data=s, weights = s$weight)
summary(ols_normes_vehicules)

formula_determinants_taxe_kerosene <- as.formula(paste("taxe_kerosene ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_taxe_kerosene <- lm(formula_determinants_taxe_kerosene, data=s, weights = s$weight)
summary(ols_taxe_kerosene)

formula_determinants_interdiction_polluants <- as.formula(paste("interdiction_polluants ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_interdiction_polluants <- lm(formula_determinants_interdiction_polluants, data=s, weights = s$weight)
summary(ols_interdiction_polluants)

formula_determinants_controle_technique <- as.formula(paste("controle_technique ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_controle_technique <- lm(formula_determinants_controle_technique, data=s, weights = s$weight)
summary(ols_controle_technique)

formula_determinants_fonds_mondial <- as.formula(paste("fonds_mondial ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_fonds_mondial <- lm(formula_determinants_fonds_mondial, data=s, weights = s$weight)
summary(ols_fonds_mondial)

formula_determinants_taxe_viande <- as.formula(paste("taxe_viande ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_taxe_viande <- lm(formula_determinants_taxe_viande, data=s, weights = s$weight)
summary(ols_taxe_viande)

formula_determinants_peages_urbains <- as.formula(paste("peages_urbains ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_peages_urbains <- lm(formula_determinants_peages_urbains, data=s, weights = s$weight)
summary(ols_peages_urbains)

Table_politiques_env_par_pol <- stargazer(ols_normes_isolation, ols_normes_vehicules, ols_taxe_kerosene, ols_interdiction_polluants, ols_controle_technique, ols_fonds_mondial, ols_taxe_viande, ols_peages_urbains,
                                  title="Determinants of attitudes towards specific climate policies", model.names = FALSE, model.numbers = T, 
                                  covariate.labels = c("Knowledge about CC", "CC is disastrous", "Interest in politics (0 to 2)", "Ecologist", "Yellow Vests: PNR", "Yellow Vests: understands", 
                                                       "Yellow Vests: supports", "Yellow Vests: is part", "Left-right: Extreme-left", "Left-right: Left", 
                                                       "Left-right: Center", "Left-right: Right", "Left-right: Extreme-right", "Diploma (1 to 4)", 
                                                       "Age: 25 -- 34","Age: 35 -- 49","Age: 50 -- 64", "Age: $\\geq$ 65", 
                                                       "Income (k\\euro{}/month)", "Sex: Male", "Size of town (1 to 5)", "Frequency of public transit"),
                                  header = FALSE, dep.var.labels = c("Buildings", "Vehicles", "Kerosene", "Interdiction", "Control", "Fund", "Meat", "Tolls"),  dep.var.caption = "", 
                                  keep = c("Revenu$", "effets_CC", "connaissances_CC", "sexe", "age_", "diplome", "_agglo", "interet_politique", "Gilets_jaunes", "ecologiste", "Gauche_droite", "transports_frequence"), 
                                  add.lines = list(c("Additional covariates & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark  \\\\ ")),
                                  no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:politiques_env")
write_clip(gsub('\\end{table}', '} \\\\ \\quad \\\\ {\\footnotesize \\textsc{Note:} Standard errors are reported in parentheses. Omitted variables are \\textit{Yellow Vests: opposes}, \\textit{Age : 18 -- 24} and \\textit{Left-right: Indeterminate}. Additional covariates are defined in Appendix C.} \\end{table*}', 
                gsub('\\begin{tabular}{@', '\\hspace*{-1.3cm} \\resizebox{1.15\\columnwidth}{!}{ \\begin{tabular}{@', gsub('\\begin{table}', '\\begin{table*}',
                                                                                                 gsub('\\\\[-1.8ex] & Buildings & Vehicles & Kerosene & Interdiction & Control & Fund & Meat & Tolls \\\\',
                                                                                                      '\\\\[-1.8ex] & Norms for & Norms for & Tax on & Prohibition & Norms for & Contribution & Tax on & Urban \\\\ \\\\[-1.8ex] & buildings & new vehicles & kerosene & pol. vehicles & old vehicles & climate fund & red meat & tolls \\\\',
                                                                                                      Table_politiques_env_par_pol, fixed=TRUE), fixed=TRUE), fixed=T), fixed=T), collapse=' ')


## Yellow Vests
s$anthropique <- s$cause_CC=='anthropique'
s$mode_vie_ecolo_oui <- s$mode_vie_ecolo=='Oui'
s$male <- s$sexe=='Masculin'
s$ecolo <- s$ecologiste==T
s$extreme_gauche <- s$Gauche_droite=='Extreme-left'
s$gauche <- s$Gauche_droite=='Left'
s$centre <- s$Gauche_droite=='Center'
s$droite <- s$Gauche_droite=='Right'
s$extreme_droite <- s$Gauche_droite=='Extreme-right'
s$indetermine <- s$Gauche_droite=='Indeterminate'
#s$gilets_jaunes_soutien_dedans <- (s$gilets_jaunes_dedans==T) + (s$gilets_jaunes_soutien==T) - (s$gilets_jaunes_dedans==T)*(s$gilets_jaunes_soutien==T)
data_cor <- s[,c("gilets_jaunes", "taille_agglo", "diplome4", "male", "Revenu", "age", "ecolo", "extreme_gauche", "gauche", "centre", "droite", "extreme_droite", "indetermine")] # , "gauche_droite", "gilets_jaunes"
names(data_cor) <- c("Yellow Vests", "Size of town", "Diploma", "Male", "Income", "Age", "Ecologist" , "Extreme-left", "Left", "Center", "Right", "Extreme-right", "Indeterminate")
corr <- cor(data_cor, use="complete.obs")
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(data_cor)
# corrplot(corr, method='color', p.mat = p.mat, sig.level = 0.01, diag=T, tl.srt=45, tl.col='black', insig = 'blank', type='upper') # , order='hclust', addCoef.col = 'white', addCoefasPercent = T
corrplot(corr, method='color', p.mat = p.mat, sig.level = 0.01, diag=FALSE, tl.srt=35, tl.col='black', insig = 'blank', addCoef.col = 'black', addCoefasPercent = T , type='upper') #, order='hclust'


decrit(s[s$Gauche_droite=='Extreme-left',]$Gilets_jaunes, miss=T, weights=s[s$Gauche_droite=='Extreme-left',]$weight)
decrit(s[s$Gauche_droite=='Left',]$Gilets_jaunes, miss=T, weights=s[s$Gauche_droite=='Left',]$weight)
decrit(s[s$Gauche_droite=='Center',]$Gilets_jaunes, miss=T, weights=s[s$Gauche_droite=='Center',]$weight)
decrit(s[s$Gauche_droite=='Right',]$Gilets_jaunes, miss=T, weights=s[s$Gauche_droite=='Right',]$weight)
decrit(s[s$Gauche_droite=='Extreme-right',]$Gilets_jaunes, miss=T, weights=s[s$Gauche_droite=='Extreme-right',]$weight)
decrit(s[s$Gauche_droite=='Indeterminate',]$Gilets_jaunes, miss=T, weights=s[s$Gauche_droite=='Indeterminate',]$weight)

decrit(s[s$liberal==T,]$Gilets_jaunes, miss=T, weights=s[s$liberal==T,]$weight)
decrit(s[s$conservateur==T,]$Gilets_jaunes, miss=T, weights=s[s$conservateur==T,]$weight)
decrit(s[s$humaniste==T,]$Gilets_jaunes, miss=T, weights=s[s$humaniste==T,]$weight)
decrit(s[s$patriote==T,]$Gilets_jaunes, miss=T, weights=s[s$patriote==T,]$weight)
decrit(s[s$apolitique==T,]$Gilets_jaunes, miss=T, weights=s[s$apolitique==T,]$weight)
decrit(s[s$ecologiste==T,]$Gilets_jaunes, miss=T, weights=s[s$ecologiste==T,]$weight)

decrit(s$taille_agglo, weights=s$weight)
decrit(s[s$taille_agglo=='rural',]$Gilets_jaunes, miss=T, weights=s[s$taille_agglo=='rural',]$weight)
decrit(s[s$taille_agglo=='-20k',]$Gilets_jaunes, miss=T, weights=s[s$taille_agglo=='-20k',]$weight)
decrit(s[s$taille_agglo=='20-100k',]$Gilets_jaunes, miss=T, weights=s[s$taille_agglo=='20-100k',]$weight)
decrit(s[s$taille_agglo=='+100k',]$Gilets_jaunes, miss=T, weights=s[s$taille_agglo=='+100k',]$weight)
decrit(s[s$taille_agglo=='Paris',]$Gilets_jaunes, miss=T, weights=s[s$taille_agglo=='Paris',]$weight)

decrit(s$diplome4, weights=s$weight)
decrit(s[s$diplome4==1,]$Gilets_jaunes, miss=T, weights=s[s$diplome4==1,]$weight)
decrit(s[s$diplome4==2,]$Gilets_jaunes, miss=T, weights=s[s$diplome4==2,]$weight)
decrit(s[s$diplome4==3,]$Gilets_jaunes, miss=T, weights=s[s$diplome4==3,]$weight)
decrit(s[s$diplome4==4,]$Gilets_jaunes, miss=T, weights=s[s$diplome4==4,]$weight)

decrit(s$age, weights=s$weight)
decrit(s[s$age_18_24==T,]$Gilets_jaunes, miss=T, weights=s[s$age_18_24==T,]$weight)
decrit(s[s$age_25_34==T,]$Gilets_jaunes, miss=T, weights=s[s$age_25_34==T,]$weight)
decrit(s[s$age_35_49==T,]$Gilets_jaunes, miss=T, weights=s[s$age_35_49==T,]$weight)
decrit(s[s$age_50_64==T,]$Gilets_jaunes, miss=T, weights=s[s$age_50_64==T,]$weight)
decrit(s[s$age_65_plus==T,]$Gilets_jaunes, miss=T, weights=s[s$age_65_plus==T,]$weight)

for (i in 1:10) {
  cat(i)
  print(decrit(s[s$revenu_decile == i,]$Gilets_jaunes, weights=s[s$revenu_decile == i,]$weight))
}

decrit(s$sexe, weights=s$weight)
decrit(s[s$sexe=='Féminin',]$Gilets_jaunes, miss=T, weights=s[s$sexe=='Féminin',]$weight)
decrit(s[s$sexe=='Masculin',]$Gilets_jaunes, miss=T, weights=s[s$sexe=='Masculin',]$weight)

decrit(s$Gilets_jaunes, weights=s$weight)

# Moins important :
summary(lm((s$taxe_approbation=='Oui') ~ Revenu, data=s)) # 0.7 p.p. (très faible)
summary(lm((s$taxe_approbation=='Oui') ~ sexe, data=s)) # 3.5 p.p.
summary(lm((s$taxe_approbation=='Oui') ~ as.factor(taille_agglo), data=s)) # Paris et +100k vs rural : +8.5 p.p.
summary(lm((s$taxe_approbation=='Oui') ~ Gilets_jaunes, data=s)) # -15 p.p. soutient / -14 p.p. est dedans
summary(lm((s$taxe_approbation=='Oui') ~ Gauche_droite, data=s)) # pas significtaif (centre .)


##### Online Appendix #####
# Construction of the knowledge index
connaissances <- s[,c("score_ges", "score_climate_call", "anthropique", "existe", "proximite_cible", "inde")]
factanal(connaissances, 1)
unlist(cronbach(connaissances))
# pca <- principal(s[,c("score_ges", "score_climate_call", "anthropique", "existe", "proximite_cible", "inde")], 1)
s$connaissances_efa <- 0.212*s$score_ges + 0.182*s$score_climate_call + 0.601*s$anthropique + 0.398*s$existe + 0.20*s$proximite_cible
cor(s$connaissances_efa, s$connaissances_CC) # 0.999

s$connaissances_CC_old <- s$score_ges + s$score_climate_call + 3*(s$cause_CC=='anthropique') - 3*(s$cause_CC=="n'existe pas") + 3 - (s$emission_cible > 2) - (s$emission_cible > 4) - (s$emission_cible > 6) + (s$region_CC=="L'Inde")
cor(s$connaissances_CC_old, s$connaissances_CC) # 0.98

connaissances_all <- cbind(s$connaissances_CC, connaissances)
names(connaissances_all) <- c("Knowledge", "GhG", "Activities",  "Anthropogenic", "Exists", "Target", "Region")
corrc <- cor(connaissances_all, use="complete.obs")
p.matc <- cor.mtest(connaissances_all)
corrplot(corrc, method='color', p.mat = p.matc, sig.level = 0.01, diag=FALSE, tl.srt=35, tl.col='black', insig = 'blank', addCoef.col = 'black', addCoefasPercent = T , type='upper') #, order='hclust'

formula_K_robust_K <- as.formula(paste("taxe_approbation!='Non' ~ ", paste(variables_determinants_policy_CC[-1], collapse = ' + ')))
ols_K_robust_K <- lm(formula_K_robust_K, data=s, weights = s$weight)
summary(ols_K_robust_K)

formula_K_robust_anthro <-  as.formula(paste("taxe_approbation!='Non' ~ anthropique + ", paste(variables_determinants_policy_CC[-1], collapse = ' + ')))
ols_K_robust_anthro <- lm(formula_K_robust_anthro, data=s, weights = s$weight)
summary(ols_K_robust_anthro)

formula_K_robust_exists <-  as.formula(paste("taxe_approbation!='Non' ~ existe + ", paste(variables_determinants_policy_CC[-1], collapse = ' + ')))
ols_K_robust_exists <- lm(formula_K_robust_exists, data=s, weights = s$weight)
summary(ols_K_robust_exists)

formula_K_robust_ghg <-  as.formula(paste("taxe_approbation!='Non' ~ score_ges + ", paste(variables_determinants_policy_CC[-1], collapse = ' + ')))
ols_K_robust_ghg <- lm(formula_K_robust_ghg, data=s, weights = s$weight)
summary(ols_K_robust_ghg)

formula_K_robust_acti <-  as.formula(paste("taxe_approbation!='Non' ~ score_climate_call + ", paste(variables_determinants_policy_CC[-1], collapse = ' + ')))
ols_K_robust_acti <- lm(formula_K_robust_acti, data=s, weights = s$weight)
summary(ols_K_robust_acti)

formula_K_robust_target <-  as.formula(paste("taxe_approbation!='Non' ~ proximite_cible + ", paste(variables_determinants_policy_CC[-1], collapse = ' + ')))
ols_K_robust_target <- lm(formula_K_robust_target, data=s, weights = s$weight)
summary(ols_K_robust_target)

Table_K_robust <- stargazer(ols_taxe_approbation, ols_K_robust_anthro, ols_K_robust_exists, ols_K_robust_ghg, ols_K_robust_acti, ols_K_robust_target,
    title="Robustness of the determinants of Tax \\& Dividend Acceptance To Knowledge Variables", model.names = FALSE, model.numbers = T, 
    covariate.labels = c("Knowledge about CC", "CC is Anthropogenic", "CC Exists", "Score GhG", "Score Activities", "Score Target proximity",
                         "Ecologist", "Yellow Vests: PNR", "Yellow Vests: understands",
                         "Yellow Vests: supports", "Yellow Vests: is part", "Left-right: Extreme-left", "Left-right: Left",
                         "Left-right: Center", "Left-right: Right", "Left-right: Extreme-right", "Sex: Male"),
    header = FALSE, dep.var.labels = c("Tax \\& dividend"),  dep.var.caption = "", 
    keep = c("connaissances_CC", "anthropique", "existe", "score_ges", "score_climate_call", "proximite_cible", "sexe", "Gilets_jaunes", "ecologiste", "Gauche_droite"),
    add.lines = list(c("Additional covariates & \\checkmark & & \\checkmark  & \\checkmark & \\checkmark & \\checkmark  \\\\ ")),
    no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:robust_K")
write_clip(gsub('\\end{table}', '} \\\\ \\quad \\\\ {\\footnotesize \\textsc{Note:} Standard errors are reported in parentheses. Omitted variables are \\textit{Yellow Vests: opposes}, \\textit{Age : 18 -- 24} and \\textit{Left-right: Indeterminate}. Additional covariates are the same as in Table II.} \\end{table*}', 
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', gsub('\\begin{table}', '\\begin{table*}', Table_K_robust, fixed=TRUE), fixed=TRUE), fixed=T), collapse=' ')


# 6.1 in logit
cause_logit1 <- glm(formula_determinants_cause, family = binomial(link='logit'), data=s)
logit_cause1_margins <- logitmfx(cause_logit1, s, atmean=FALSE)$mfxest
logit_cause1_margins

cause_logit2 <- glm(cause_CC=='anthropique' ~ age_25_34 + age_35_49 + age_50_64 + age_65_plus, family = binomial(link='logit'), data=s)
logit_cause2_margins <- logitmfx(cause_logit2, s, atmean=FALSE)$mfxest
logit_cause2_margins

# cause_logit3 <- glm(cause_CC=='anthropique' ~ Gauche_droite + as.factor(diplome4) + diplome4 : gauche_droite_na + diplome4 : indeterminate, family = binomial(link='logit'), data=s)
cause_logit3 <- glm(cause_CC=='anthropique' ~ Gauche_droite + as.factor(diplome4) + diplome4 : gauche_droite, family = binomial(link='logit'), data=s)
logit_cause3_margins <- logitmfx(cause_logit3, s, atmean=FALSE)$mfxest
logit_cause3_margins

effects_logit <- glm(formula_determinants_effets, family = binomial(link='logit'), data=s)
logit_effects_margins <- logitmfx(effects_logit, s, atmean=FALSE)$mfxest
logit_effects_margins

effects_logit2 <- glm((effets_CC > 2) ~ Gauche_droite + as.factor(diplome4) + diplome4 : gauche_droite_na + diplome4 : indeterminate, family = binomial(link='logit'), data=s)
logit_effects2_margins <- logitmfx(effects_logit2, s, atmean=FALSE)$mfxest
logit_effects2_margins

Table_determinants_attitudes_CC_logit <- stargazer(cause_logit1, cause_logit2, cause_logit3, effects_logit, effects_logit2,
     title="Determinants of attitudes towards climate change (CC) with logit regressions.", model.names = FALSE, model.numbers = T, 
     covariate.labels = c("Interest in politics (0 to 2)", "Ecologist", "Yellow Vests: PNR", "Yellow Vests: understands",
                          "Yellow Vests: supports", "Yellow Vests: is part", "Left-right: Extreme-left", "Left-right: Left",
                          "Left-right: Center", "Left-right: Right", "Left-right: Extreme-right", "Diploma: \\textit{CAP} or \\textit{BEP}",
                          "Diploma: \\textit{Baccalauréat}", "Diploma: Higher", "Age: 25 -- 34","Age: 35 -- 49","Age: 50 -- 64", "Age: $\\geq$ 65",
                          "Income (k\\euro{}/month)", "Sex: Male", "Size of town (1 to 5)", "Frequency of public transit", "Diploma $\\times$ Left-right", "Diploma $\\times$ Left-right: Indeterminate"),
     header = FALSE, dep.var.labels = c("CC is anthropogenic", "CC is disastrous"),  dep.var.caption = "", 
     coef = list(logit_cause1_margins[,1], logit_cause2_margins[,1], logit_cause3_margins[,1], logit_effects_margins[,1], logit_effects2_margins[,1]),
     se = list(logit_cause1_margins[,2], logit_cause2_margins[,2], logit_cause3_margins[,2], logit_effects_margins[,2], logit_effects2_margins[,2]),
     keep = c("sexe", "Revenu$", "age_", "\\(diplome", "diplome4:", "taille_agglo", "Gilets_jaunes", "ecologiste", 
              "Gauche_droite", "interet_politique", "transports_frequence"), # "humaniste", , "transports_avis", "conso"
     add.lines = list(c("Additional covariates & \\checkmark &  &  & \\checkmark &  \\\\ ")),
     no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:determinants_attitudes_CC")
write_clip(gsub('\\end{table}', '}{\\\\ $\\quad$ \\\\                \\footnotesize \\textsc{Note:} Standard errors are reported in parentheses. Interaction term is computed using numeric variables. Omitted modalities are: \\textit{Yellow Vests: opposes}, \\textit{Left-right: Indeterminate}, \\textit{Diploma: Brevet or no diploma}, \\textit{Age: 18 -- 24}. Additional covariates are defined in \\ref{app:covariates}. }                \\end{table*} ', 
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', gsub('\\begin{table}', '\\begin{table*}',
       Table_determinants_attitudes_CC_logit, fixed=TRUE), fixed=TRUE), fixed=T), collapse=' ')

# 6.2 in logit
tax_logit <- glm(formula_determinants_taxe_approbation, family = binomial(link='logit'), data=s)
tax_logit_margins <- logitmfx(tax_logit, s, atmean=FALSE)$mfxest
tax_logit_margins

tax_logit2 <- glm(formula_determinants_taxe_approbation_bis, family = binomial(link='logit'), data=s)
tax_logit2_margins <- logitmfx(tax_logit2, s, atmean=FALSE)$mfxest
tax_logit2_margins

env_logit <- glm(formula_determinants_nb_politiques_env, family = binomial(link='logit'), data=s)
env_logit_margins <- logitmfx(env_logit, s, atmean=FALSE)$mfxest
env_logit_margins

ecologit <- glm(formula_determinants_mode_vie_ecolo, family = binomial(link='logit'), data=s)
ecologit_margins <- logitmfx(ecologit, s, atmean=FALSE)$mfxest
ecologit_margins

Table_politiques_env_logit <- stargazer(tax_logit, tax_logit2, env_logit, ecologit,
    title="Determinants of attitudes towards climate policies with logit regressions.", model.names = FALSE, model.numbers = T, 
    covariate.labels = c("Knowledge about CC", "CC is disastrous", "Interest in politics (0 to 2)", "Ecologist", "Yellow Vests: PNR", "Yellow Vests: understands",
                         "Yellow Vests: supports", "Yellow Vests: is part", "Left-right: Extreme-left", "Left-right: Left",
                         "Left-right: Center", "Left-right: Right", "Left-right: Extreme-right", "Diploma (1 to 4)",
                         "Age: 25 -- 34","Age: 35 -- 49","Age: 50 -- 64", "Age: $\\geq$ 65",
                         "Income (k\\euro{}/month)", "Sex: Male", "Size of town (1 to 5)", "Frequency of public transit"),
    header = FALSE, dep.var.labels = c("Tax \\& dividend", "Share of policies", "Ecological lifestyle"),  dep.var.caption = "", 
    coef = list(tax_logit_margins[,1], tax_logit2_margins[,1], env_logit_margins[,1], ecologit_margins[,1]),
    se = list(tax_logit_margins[,2], tax_logit2_margins[,2], env_logit_margins[,2], ecologit_margins[,2]),
    keep = c("Revenu$", "effets_CC", "connaissances_CC", "sexe", "age_", "diplome", "_agglo", "interet_politique", "Gilets_jaunes", "ecologiste", "Gauche_droite", "transports_frequence"), 
    add.lines = list(c("Additional covariates & \\checkmark & & \\checkmark  & \\checkmark  \\\\ ")), # => remove empty line in latex 
    no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:politiques_env_logit")
write_clip(gsub('\\end{table}', '} \\\\ \\quad \\\\ {\\footnotesize \\textsc{Note:} Standard errors are reported in parentheses. Omitted variables are \\textit{Yellow Vests: opposes}, \\textit{Age : 18 -- 24} and \\textit{Left-right: Indeterminate}. Additional covariates are defined in Appendix C.} \\end{table*}', 
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', gsub('\\begin{table}', '\\begin{table*}',
                 gsub('\\\\[-1.8ex] & \\multicolumn{2}{c}{Tax \\& dividend} & Share of policies & norms vs. taxes & earmarking vs. transfers & ecological lifestyle \\\\',
                      '\\\\[-1.8ex] & \\multicolumn{2}{c}{Acceptance of} & Share of policies & Norms & Earmarking & Ecological \\\\ \\\\[-1.8ex] & \\multicolumn{2}{c}{Tax \\& dividend} & approved & vs. taxes & vs. transfers & lifestyle \\\\',
                      Table_politiques_env_logit, fixed=TRUE), fixed=TRUE), fixed=T), fixed=T), collapse=' ')

# No interaction: replacing left-right by Yellow Vests and diploma by knowledge
interact_yv <- lm((effets_CC > 2) ~ Gilets_jaunes + as.factor(diplome4) + diplome4 : gilets_jaunes, data=s, weights = s$weight)
summary(interact_yv)
interact_k <- lm((effets_CC > 2) ~ Gauche_droite + connaissances_CC + connaissances_CC : gauche_droite, data=s, weights = s$weight)
summary(interact_k)
interact_yv_k <- lm((effets_CC > 2) ~ Gilets_jaunes + connaissances_CC + connaissances_CC : gilets_jaunes, data=s, weights = s$weight)
summary(interact_yv_k)
Table_interaction <- stargazer(interact_yv, interact_k, interact_yv_k,
       title="Robustness of the absence interaction on perceived effects between political orientation and knowledge.", model.names = FALSE, model.numbers = T, 
       covariate.labels = c("Constant", "Yellow Vests: PNR", "Yellow Vests: understands",
                            "Yellow Vests: supports", "Yellow Vests: is part", "Diploma: \\textit{CAP} or \\textit{BEP}",
                            "Diploma: \\textit{Baccalauréat}", "Diploma: Higher", "Diploma $\\times$ Yellow Vests", "Left-right: Left",
                            "Left-right: Center", "Left-right: Right", "Left-right: Extreme-right", "Knowledge CC", "Knowledge CC $\\times$ Left-right", "Knowledge CC $\\times$ Yellow Vests"),
       header = FALSE, dep.var.labels = c("CC is disastrous"),  dep.var.caption = "", 
       no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:robustness_no_interaction")
write_clip(gsub('\\end{table}', '}{\\\\ $\\quad$ \\\\                \\footnotesize \\textsc{Note:} Standard errors are reported in parentheses. Interaction term is computed using numeric variables. Omitted modalities are: \\textit{Yellow Vests: opposes}, \\textit{Left-right: Extreme-left}, \\textit{Diploma: Brevet or no diploma}. }                \\end{table*} ', 
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', gsub('\\begin{table}', '\\begin{table*}',
            Table_interaction, fixed=TRUE), fixed=TRUE), fixed=T), collapse=' ')

# # Preferred types of policies (Yellow Vests and Ecologists)
# s$acceptance_norms <- s$normes_isolation + s$normes_vehicules + s$controle_technique + s$interdiction_polluants
# s$acceptance_taxes <- s$taxe_kerosene + s$taxe_viande + s$peages_urbains + s$fonds_mondial
# 
# decrit(s$acceptance_norms[s$ecologiste==TRUE], weights = s$weight[s$ecologiste==TRUE])
# decrit(s$acceptance_taxes[s$ecologiste==TRUE], weights = s$weight[s$ecologiste==TRUE])
# 
# decrit(s$acceptance_norms[s$Gilets_jaunes=='soutient'], weights = s$weight[s$Gilets_jaunes=='soutient'])
# decrit(s$acceptance_taxes[s$Gilets_jaunes=='soutient'], weights = s$weight[s$Gilets_jaunes=='soutient'])
# 
# decrit(s$acceptance_norms[s$Gilets_jaunes=='est_dedans'], weights = s$weight[s$Gilets_jaunes=='est_dedans'])
# decrit(s$acceptance_taxes[s$Gilets_jaunes=='est_dedans'], weights = s$weight[s$Gilets_jaunes=='est_dedans'])
# 
# s$acceptance_earmarked <- s$si_renovation + s$si_renouvelables + s$si_transports
# s$acceptance_compensations <- s$si_pauvres + s$si_compensee + s$si_contraints
# 
# decrit(s$acceptance_earmarked[s$ecologiste==TRUE], weights = s$weight[s$ecologiste==TRUE])
# decrit(s$acceptance_compensations[s$ecologiste==TRUE], weights = s$weight[s$ecologiste==TRUE])
# 
# decrit(s$acceptance_earmarked[s$Gilets_jaunes=='soutient'], weights = s$weight[s$Gilets_jaunes=='soutient'])
# decrit(s$acceptance_compensations[s$Gilets_jaunes=='soutient'], weights = s$weight[s$Gilets_jaunes=='soutient'])
# 
# decrit(s$acceptance_earmarked[s$Gilets_jaunes=='est_dedans'], weights = s$weight[s$Gilets_jaunes=='est_dedans'])
# decrit(s$acceptance_compensations[s$Gilets_jaunes=='est_dedans'], weights = s$weight[s$Gilets_jaunes=='est_dedans'])
# 
# 
# ##### Cronbach's alpha: #####
# 
# s$ges_CO2_num_cor <- 1 * (s$ges_CO2 == TRUE)
# s$ges_CH4_num_cor <- 1 * (s$ges_CH4 == TRUE)
# s$ges_O2_num_cor <- 1 * (s$ges_O2 == FALSE)
# s$ges_pm_num_cor <- 1 * (s$ges_pm == FALSE)
# s$ges_avion_num_cor <- 1 * (s$ges_avion == TRUE)
# s$ges_boeuf_num_cor <- 1 * (s$ges_boeuf == TRUE)
# s$ges_nucleaire_num_cor <- 1 * (s$ges_nucleaire == FALSE)
# 
# s$existe <- 1-1*(s$cause_CC=="n'existe pas")
# s$proximite_cible <- 3 - (s$emission_cible > 2) - (s$emission_cible > 4) - (s$emission_cible > 6)
# s$inde <- 1*(s$region_CC=="L'Inde")
# 
# #unlist(cronbach(s[,c("score_ges", "score_climate_call", "anthropique", "existe", "proximite_cible", "inde")]))
# connaissances <- s[,c("ges_CO2_num_cor", "ges_CH4_num_cor", "ges_O2_num_cor", "ges_pm_num_cor", "ges_avion_num_cor", "ges_boeuf_num_cor", "ges_nucleaire_num_cor",
#                       "anthropique", "existe", "proximite_cible", "inde")]
# unlist(cronbach(connaissances))
# 
# EFA <- factanal(connaissances, 1)
# EFA # 1 factor not enough (even 5, the max, is not enough)
# temp <- EFA$loadings
# factor <- factor.pa(connaissances, 1)
# pca <- principal(connaissances, 1)
# 
# s$connaissances_efa <- 0.264*s$ges_CO2_num_cor + 0.248*s$ges_CH4_num_cor + 0.114*s$ges_O2_num_cor - 0.167*s$ges_pm_num_cor + 0.302*s$ges_boeuf_num_cor + 0.692*s$anthropique + 0.35*s$existe + 0.202*s$proximite_cible
# cor(s$connaissances_efa, s$connaissances_CC)
# 
# s$connaissances_CC_wo_region <- s$score_ges + s$score_climate_call + 3*((s$cause_CC=='anthropique') - (s$cause_CC=="n'existe pas")) +   3 - (s$emission_cible > 2) - (s$emission_cible > 4) - (s$emission_cible > 6)
# s$connaissances_CC <- s$score_ges + s$score_climate_call + 3*((s$cause_CC=='anthropique') - (s$cause_CC=="n'existe pas")) + 3 - (s$emission_cible > 2) - (s$emission_cible > 4) - (s$emission_cible > 6) + (s$region_CC=="L'Inde")
# s$connaissances_CC <- (s$connaissances_CC - mean(s$connaissances_CC))/sd(s$connaissances_CC)
# cor(s$connaissances_CC_wo_region, s$connaissances_CC)
# 
# pca <- prcomp(connaissances, rank. = 1, scale = T)
# pca
# s$connaissances_pca <- -pca$x
# cor(s$connaissances_pca, s$connaissances_CC) # 0.904
# cor(s$connaissances_pca, s$connaissances_efa) # 0.923
# 
# connaissances_all <- cbind(s$connaissances_CC, s$connaissances_pca, s$connaissances_efa, connaissances)
# names(connaissances_all) <- c("connaissances_CC", "connaissances_pca",  "connaissances_efa", "ges_CO2_num_cor", "ges_CH4_num_cor", "ges_O2_num_cor", "ges_pm_num_cor", "ges_avion_num_cor", "ges_boeuf_num_cor", "ges_nucleaire_num_cor",
#                           "anthropique", "existe", "proximite_cible", "inde")
# corrc <- cor(connaissances_all, use="complete.obs")
# p.matc <- cor.mtest(connaissances_all)
# corrplot(corrc, method='color', p.mat = p.matc, sig.level = 0.01, diag=FALSE, tl.srt=35, tl.col='black', insig = 'blank', addCoef.col = 'black', addCoefasPercent = T , type='upper') #, order='hclust'
