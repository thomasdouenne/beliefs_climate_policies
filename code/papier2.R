## Source to reproduce "French Preferences over Climate Policies"
#     by Thomas Douenne & Adrien Fabre, under licence CC-BY
# All files can be found on github: https://github.com/bixiou/beliefs_climate_policies
#     in particular, file to prepare the dataset (preparation.R), to load the packages and functions (packages_functions.R),
#     R environment with all the data prepared (.RData) and python files for complementary computations

source("packages_functions.R")
load(".RData")

# /!\ for numerical items with NSP, subset=!is.missing() in regressions (parle_CC, transports_..., gilets_jaunes, perte_relative_...)

##### 2. Survey and data #####




##### 3. Attitudes over Climate Change #####
## 3.1 Knowledge
decrit(s$ges_avion, weights = s$weight) # 59%
decrit(s$ges_boeuf, weights = s$weight) # 46%
decrit(s$ges_nucleaire, weights = s$weight) # 50%
decrit(s$ges_CO2, weights = s$weight) # 77%
decrit(s$ges_CH4, weights = s$weight) # 48%
decrit(s$ges_O2, weights = s$weight) # 4%
decrit(s$ges_pm, weights = s$weight) # 61%

decrit(s$emission_cible, weights = s$weight) # 5
decrit(s$cause_CC, miss=T, weights = s$weight) # 72% anthropic, 20% natural, 3% doesn't exist
decrit(s$region_CC, weights = s$weight) # 65% autant, 29% Inde, 6% UE
decrit(s$generation_CC_min, weights = s$weight) #1960-2050: 11%-27%-43%-19%

decrit(s$score_climate_call, weights = s$weight)
decrit(s$emission_cible >= 5, weights = s$weight)
decrit(s$emission_cible <= 2, weights = s$weight)
decrit(s$generation_CC_min >= 2020, weights = s$weight)

decrit(s$connaissances_CC)
plot(density(s$connaissances_CC))
summary(lm(I((connaissances_CC - mean(connaissances_CC))/sd(connaissances_CC)) ~ I((generation_CC_min-1960)/30), data=s, weights = s$weight))
formula_connaissances_CC <- as.formula(paste("I((connaissances_CC - mean(connaissances_CC))/sd(connaissances_CC)) ~ I((generation_CC_min-1960)/30) + region_CC + diplome + ", 
        paste(variables_determinants[!(variables_determinants %in% c("Gauche_droite", "humaniste", "patriote", "ecologiste", "apolitique", "liberal", 
        "conservateur", "diplome4", "as.factor(ifelse(is.missing(s$Gilets_jaunes), 'NA', as.character(s$Gilets_jaunes)))"))], collapse = ' + ')))
summary(lm(formula_connaissances_CC, data=s, weights = s$weight))
# summary(lm(connaissances_CC ~ as.factor(generation_CC_min), data=s, weights = s$weight))

ges_climate_call <- rev(paste("ges_correct", c("avion", "nucleaire", "boeuf", "O2", "CO2", "CH4", "pm"), sep="_")) 
labels_ges_climate_call <- rev(c("Plane vs. train", "Nuclear vs. wind", "Beaf vs. pasta", "Oxygen", "CO<sub>2</sub>", "Methane", "Particulates")) 
oui_non(margin_l=40, ges_climate_call, NSP=FALSE, colors=color(3)[1:2], en=c("Correct", "Wrong"), labels = labels_ges_climate_call, sort=FALSE) # colors=color(3)[1:2], 
barres(file="CC_target_emission_nolegend", title="", data=dataN("emission_cible", miss=FALSE), nsp=FALSE, sort=T, color = rev(brewer.pal(11, "RdBu")), 
       legend = dataN("emission_cible", return="levels"), labels=c(" ")) 
barres(file="CC_cause_nolegend", title="", thin=T, data=dataN("cause_CC"), nsp=T, sort=T, legend = c("Anthropic", "Natural", "Does not exist", "PNR"), labels=c(" "), show_ticks = F)
barres(file="CC_generation_min_nolegend", title="", rev_color = T, data=dataN("generation_CC_min"), nsp=T, sort=T, thin=T,
       legend = c(dataN("generation_CC_min", return="levels")[1:4], "PNR"), labels=c(" "))
s$region_CC <- as.factor(s$region_CC)
s$region_CC <- relevel(relevel(s$region_CC, "Autant dans les deux"), "L'Inde")
barres(file="CC_region_nolegend", title="", data=dataN("region_CC", miss=FALSE), nsp=FALSE, sort=T, show_ticks = F, thin=T,
       legend = c("India", "As much in both", "European Union", "NSP"), labels=c(" "))

usa_survey <- read.dta("CCES_Panel_Full3waves_VV_V4.dta") # https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/TOE8I1
labels_CC_USA <- levels(usa_survey$CC14_321) # What action needed for climate change? 2014. 
levels(usa_survey$CC14_321) <- c("Immediate", "Some", "Wait", "No", "Fake", "PNR", "NA")
decrit(usa_survey$CC14_321, weights = usa_survey$weight) # 30/25/18/20/8
# levels(usa_survey$CC10_321) <- levels(usa_survey$CC12_321) <- c("Immediate", "Some", "Wait", "No", "Fake", "PNR", "NA")
# decrit(usa_survey$CC12_321, weights = usa_survey$weight)
# decrit(usa_survey$CC10_321, weights = usa_survey$weight)

## 3.2 Opinions
decrit(s$ecologiste, weights=s$weight) # 15% écologistes
decrit(s$parle_CC, miss=T, weights = s$weight) # 3 tiers
decrit(s$effets_CC, miss=T, weights = s$weight) # 18% cataclysmiques; 28% désastreux, 35% graves
decrit(s$responsable_CC_chacun, miss=T, weights = s$weight) # 63%
decrit(s$responsable_CC_govts, miss=T, weights = s$weight) # 47%
decrit(s$responsable_CC_etranger, miss=T, weights = s$weight) # 42%
decrit(s$responsable_CC_riches, miss=T, weights = s$weight) # 25%
decrit(s$responsable_CC_nature, miss=T, weights = s$weight) # 23%
decrit(s$responsable_CC_passe, miss=T, weights = s$weight) # 21%
decrit(s$ecologiste, miss=T, weights = s$weight)

labels_resp <- c("Each one of us", "Governments", "Certain foreign countries", "The richest", "Natural causes", "Past generations")
barres(file="CC_responsible", title="", data=data1(names(s)[which(grepl("responsable_CC", names(s)))]), sort=T, showLegend=FALSE, labels=labels_resp, hover=labels_resp)
barres(file="CC_effects_nolegend", title="", thin=T, data=dataN("effets_CC"), nsp=T, sort=T, 
       legend = c("Insignificant", "Small", "Serious", "Disastrous", "Cataclysmic", "PNR"), labels=c(" "))
s$parle_CC <- as.factor(s$parle_CC)
s$parle_CC <- relevel(relevel(s$parle_CC, "Plusieurs fois par an"), "Plusieurs fois par mois")
barres(file="CC_talks_nolegend", title="", data=dataN("parle_CC"), nsp=T, sort=T, show_ticks = F, thin = T,
       legend = c("Several times per month", "Several times per year", "Almost never", "PNR"), labels=c(" ")) 

## 3.3 Reaction needed
decrit(s$ecologiste, weights = s$weight)
decrit(s$humaniste, weights = s$weight)
decrit(s$patriote, weights = s$weight)
decrit(s$liberal, weights = s$weight)
decrit(s$conservateur, weights = s$weight)
decrit(s$apolitique, weights = s$weight)
decrit(s$Gauche_droite, weights = s$weight, miss=T)
decrit(s$mode_vie_ecolo, miss=T, weights = s$weight) # 65%
decrit(s$enfant_CC, weights = s$weight) # 20% oui
decrit(s$enfant_CC_pour_CC[s$enfant_CC=='Oui'], weights = s$weight[s$enfant_CC=='Oui']) # 37%
decrit(s$enfant_CC_pour_lui[s$enfant_CC=='Oui'], weights = s$weight[s$enfant_CC=='Oui']) # 86%
summary(lm((enfant_CC=='Oui') ~ sexe, data=s)) #V -3.4 p.p.

decrit(s$changer_deja_fait==T | s$changer_essaie==T, weights=s$weight) # 36%
decrit(s$changer_non_interet == T | s$changer_non_riches==T | s$changer_non_negation==T, weights=s$weight) # 11%
decrit(s$changer_si_moyens == T | s$changer_si_politiques==T | s$changer_si_tous==T | s$changer_essaie==T, weights=s$weight) # 85%
decrit(s$changer_si_moyens == T | s$changer_si_politiques==T | s$changer_si_tous==T, weights=s$weight) # 82%
decrit(s$changer_si_tous, weights=s$weight) # 47%
decrit(s$changer_si_moyens, weights=s$weight) # 45%
decrit(s$changer_si_politiques, weights=s$weight) # 43%
decrit(s$changer_deja_fait, weights=s$weight) # 23%
decrit(s$changer_essaie, weights=s$weight) # 15%
decrit(s$changer_non_negation, weights=s$weight) # 5%
decrit(s$changer_non_riches, weights=s$weight) # 5%
decrit(s$changer_non_interet, weights=s$weight) # 2%

variables_changer <- names(s)[which(grepl("changer", names(s)))]
labels_changer <- c("Yes, if policies were going in this direction", "Yes, if I had the financial means", "Yes, if everyone was doing the same",
                    "No, only the richest must change it", "No, it is against my personal interest", "No, because CC is not a real problem",
                    "No, I already adopted a sustainable way of life", "I try but I have difficulties changing my habits")
barres(file="change_if_no", title="", data=data1(variables_changer), sort=T, showLegend=FALSE, labels=labels_changer, hover=labels_changer, margin_l=250)

decrit((s$emission_cible[s$changer_deja_fait==T] >= 3), weights = s$weight[s$changer_deja_fait==T]) # 79%
decrit((s$emission_cible[s$changer_deja_fait==F] >= 3), weights=s$weight[s$changer_deja_fait==F]) # 85%


##### 4. Attitudes over Carbon Tax and Dividend #####

## 4.1 Massive rejection
barres(file="approval", title="", data=matrix(dataN("taxe_approbation")[c(2,1,3)], ncol=1), legend = c("Yes", "No", "PNR"), labels = c(" "))

## 4.2 Perceived winners and losers
variables_winners <- names(s)[which(grepl("taxe_gagnant_", names(s)))]
labels_winners <- c("No one", "The poorest", "The middle class", "The richest", "Everyone", "City dwellers", "Certain persons, but no specific income category", "PNR (Don't know, don't want to answer)")
barres(file="tax_winners_synchro", title="", data=data1(variables_winners), sort=T, showLegend=FALSE, labels=labels_winners, hover=labels_winners, xrange=c(0, 0.58), margin_l=270)
variables_losers <- names(s)[which(grepl("taxe_perdant_", names(s)))]
labels_losers <- c("No one", "The poorest", "The middle class", "The richest", "Everyone", "Rural or peri-urban households", "Certain persons, but no specific income category", "PNR (Don't know, don't want to answer)")
barres(file="tax_losers_synchro", title="", data=data1(variables_losers), sort=T, showLegend=FALSE, labels=labels_losers, hover=labels_losers, xrange=c(0, 0.58), margin_l=270)

# When question specific to purchasing power:
variables_winners <- names(s)[which(grepl("taxe_gagnant_", names(s)))]
labels_winners <- c("No one", "The poorest", "The middle class", "The richest", "Everyone", "City dwellers", "Certain persons,<br> but no specific income category", "PNR (Don't know, don't want to answer)")
barres(file="tax_winners", title="", data=data1(variables_winners, data=s[s$variante_monetaire==1,], weights = s$weight[s$variante_monetaire==1]), sort=T, showLegend=FALSE, labels=labels_winners, hover=labels_winners)
variables_losers <- names(s)[which(grepl("taxe_perdant_", names(s)))]
labels_losers <- c("No one", "The poorest", "The middle class", "The richest", "Everyone", "Rural or peri-urban households", "Certain persons, <br>but no specific income category", "PNR (Don't know, don't want to answer)")
barres(file="tax_losers", title="", data=data1(variables_losers, data=s[s$variante_monetaire==1,], weights = s$weight[s$variante_monetaire==1]), sort=T, showLegend=FALSE, labels=labels_losers, hover=labels_losers)

summary(lm((s$taxe_gagnant_riches==T) ~ variante_monetaire, data=s)) # 0.308 / -0.029 .
summary(lm((s$taxe_gagnant_pauvres==T) ~ variante_monetaire, data=s)) # 0.058 / +0.044 ***
summary(lm((s$taxe_gagnant_moyennes==T) ~ variante_monetaire, data=s)) # 0.041 / +0.016 *
summary(lm((s$taxe_gagnant_citadins==T) ~ variante_monetaire, data=s)) # 0.21 / -0.031 *
summary(lm((s$taxe_gagnant_certains==T) ~ variante_monetaire, data=s)) # 0.139 / -0.012

summary(lm((s$taxe_perdant_riches==T) ~ variante_monetaire, data=s)) # 0.009 / +0.012 **
summary(lm((s$taxe_perdant_pauvres==T) ~ variante_monetaire, data=s)) # 0.48 / -0.01
summary(lm((s$taxe_perdant_moyennes==T) ~ variante_monetaire, data=s)) # 0.55 / + 0.054 **
summary(lm((s$taxe_perdant_ruraux==T) ~ variante_monetaire, data=s)) # 0.36 / -0.022
summary(lm((s$taxe_perdant_certains==T) ~ variante_monetaire, data=s)) # 0.09 / -0.011

s$nb_taxe_gagnant <- 0
for (v in variables_taxe_gagnant) s$nb_taxe_gagnant[s[[v]]==T] <- 1 + s$nb_taxe_gagnant[s[[v]]==T]
decrit(s$nb_taxe_gagnant)
s$nb_taxe_perdant <- 0
for (v in variables_taxe_perdant) s$nb_taxe_perdant[s[[v]]==T] <- 1 + s$nb_taxe_perdant[s[[v]]==T]
decrit(s$nb_taxe_perdant)

## 4.3 Perceived pros and cons
decrit(s$benefices_aucun, weights=s$weight) # 44%
decrit(s$benefices_CC, weights=s$weight) # 30%
decrit(s$benefices_sante, weights=s$weight) # 27%
decrit(s$benefices_enjeu, weights=s$weight) # 16%
decrit(s$benefices_independance, weights=s$weight) # 11%
decrit(s$benefices_pauvres, weights=s$weight) # 10%
decrit(s$benefices_circulation, weights=s$weight) # 10%
decrit(s$benefices_revenu, weights=s$weight) # 8%
decrit(s$benefices_autre_choix, weights=s$weight) # 5%

print(s$benefices_autre[!is.na(s$benefices_autre)])
# aucun: ................
# inéquité: ...............
# modifications structurelles: ............
# diesel, kerosène: ...........
# déficit: .........
# état prend argent: ..................
# pépites : "maléfique" "Aucunement bénéfique sauf pour contrer ceux qui utilisent leur véhicule alors qu'ils peuvent faire autrement" 
# "aucune prime de nous sera versé. ce n est que  du blabla de la part du gouvernement" "brassage"
# "elle est bénéfique pour le gouvernement qui n'investirait pas la TOTALITE des ressources budgétaires supplémentaires"
# "Globalement l'ensemble des mesures et le reversement d'une \"compensation\" ne recouvrerait pas la différence de prix sur l'année. 
#     Je  parle de l'ensemble des augmentations ainsi que les augmentation forcément induites et répercutées sur l'ensemble des produits de consommations..." 
# "vos donnees sont erronees" "On est toujours perdant" "je ne pourrai plus me chauffer tout simplement"
# "Je ne vois pas en quoi nous prendre de l'argent pour nous le redonner, changerait quelque chose à notre manière de polluer."
# "J' ai deja réduit au maximum mes consommations d' énergie mais néanmoins  je paye toujours plus" Problème : "Pour la paix sociale"
# "il faut taxer les nouveaux véicule a l'achats et de fasson significative et pas apré leurs mise en services!"
# "Ce projet a entrainé un fort mécontentement; je ne le soutiens pas car je le juge absurde."
print(s$problemes_autre[!is.na(s$problemes_autre)])

variables_benefits <- names(s)[which(grepl("benefice", names(s)))[which(grepl("problemes", names(s)))>300]]
variables_benefits <- variables_benefits[!(variables_benefits %in% c("nb_benefices", "benefices_autre"))]
labels_benefits <- c("Fights CC", "Reduces negative impact of pollution on health", "Reduces congestion", "Increases my purchasing power", 
                     "Increases purchasing power of the poorest",
                     "Increases France's independence toward fossils", "Prepares the economy for tomorrow", "None of these reasons", "Other reasons")
barres(file="CC_benefits_synchro", title="", data=data1(variables_benefits), sort=T, showLegend=FALSE, labels=labels_benefits, hover=labels_benefits, xrange=c(0, 0.47), margin_l=261) # pb 35% NSP

decrit(s$problemes_ruraux, weights=s$weight) # 47%
decrit(s$problemes_pretexte, weights=s$weight) # 43%
decrit(s$problemes_inefficace, weights=s$weight) # 37%
decrit(s$problemes_revenu, weights=s$weight) # 31%
decrit(s$problemes_alternatives, weights=s$weight) # 31%
decrit(s$problemes_pauvres, weights=s$weight) # 29%
decrit(s$problemes_economie, weights=s$weight) # 14%
decrit(s$problemes_aucun, weights=s$weight) # 9%
decrit(s$problemes_autre_choix, weights=s$weight) # 2%

variables_problems <- names(s)[which(grepl("problemes", names(s)))]
variables_problems <- variables_problems[!(variables_problems %in% c("nb_problemes", "problemes_autre"))]
labels_problems <- c("Is ineffective to reduce pollution", "Alternatives are insufficient or too expensive", "Penalizes rural households", "Decreases my purchaisng power",
                     "Penalizes the poorest", "Hurts the economy", "Is a pretext to increase taxes", "None of these reasons", "Other reasons")
barres(file="CC_problems_synchro", title="", data=data1(variables_problems), sort=T, showLegend=FALSE, labels=labels_problems, hover=labels_problems, xrange=c(0, 0.47), margin_l=261)

decrit(s$nb_benefices) # 1.14
decrit(s$nb_problemes) # 2.36
decrit(s$nb_problemes - s$nb_benefices) # 15% more benefits, 27% as much, 58% more problems

s$nb_benefits <- 0
for (v in variables_benefits[variables_benefits != 'benefices_aucun']) s$nb_benefits[s[[v]]==T] <- 1 + s$nb_benefits[s[[v]]==T]
decrit(s$nb_benefits)

decrit(s$taxe_efficace, miss=T, weight=s$weight)

## 4.4 Consumption and mobility constraints
# 4.4.1 Perceived elasticities
s$elast_chauffage_perso <- factor(s$elasticite_chauffage_perso, levels(as.factor(s$elasticite_chauffage_perso))[c(1,6:2)])
s$elast_fuel_perso <- factor(s$elasticite_fuel_perso, levels(as.factor(s$elasticite_fuel_perso))[c(1,6:2)])
s$elast_chauffage_perso <- revalue(s$elast_chauffage_perso, c("+ de 30% - Je changerais largement ma consommation"="> 30%", 
                                                              "de 20% à 30%"="20 to 30%", "de 10% à 20%"="10 to 20%", "de 0% à 10%"="0 to 10%", 
                                                              "0% - Je ne la réduirais pas"="0%: constrained", "0% - Je n'en consomme déjà pas"="0%: don't consume")) # won't reduce
# s$elast_fuel_perso <- revalue(s$elast_fuel_perso, c("+ de 30% - Je changerais largement mes habitudes de déplacement"="> 30%", 
#           "de 20% à 30%"="20 to 30%", "de 10% à 20%"="10 to 20%", "de 0% à 10%"="0 to 10%", 
#           "0% - Je suis contraint sur tous mes déplacements"="0%: won't reduce", "0% - Je n'en consomme déjà presque pas"="0%: don't consume"))
barres(file="elasticities_perso", thin=T, title="", data=dataKN(c("elast_chauffage_perso", "elast_fuel_perso"), miss=FALSE), 
       nsp=FALSE, labels=c("Own: Housing", "Own: Transport"), legend = c("", "same as above", "", dataN("elast_chauffage_perso", return="legend")[4:6]), show_ticks=T)
s$elast_chauffage <- factor(s$elasticite_chauffage, levels(as.factor(s$elasticite_chauffage))[c(1,4,3,5,2)])
s$elast_fuel <- factor(s$elasticite_fuel, levels(as.factor(s$elasticite_fuel))[c(1,4,3,5,2)])
s$elast_chauffage <- revalue(s$elast_chauffage, c("+ de 30%"="> 30%", "de 20% à 30%"="20 to 30%", 
                                                  "de 10% à 20%"="10 to 20%", "de 0% à 3%"="0 to 3%", "de 3% à 10%"="3 to 10%"))
# s$elast_fuel <- revalue(s$elast_fuel, c("+ de 30%"="> 30%", "de 20% à 30%"="20 to 30%", "de 10% à 20%"="10 to 20%", "de 0% à 3%"="0 to 3%", "de 3% à 10%"="3 to 10%"))
barres(file="elasticities_agg", thin=T, title="", data=dataKN(c("elast_fuel", "elast_chauffage"), miss=FALSE), color=color(6)[1:5],
       nsp=FALSE, labels=c("Aggregate: Transport", "Aggregate: Housing"), legend = dataN("elast_chauffage", return="legend"), show_ticks=T)
barres(file="elasticities", title="", thin=T, data=dataKN(c("Elasticite_chauffage", "Elasticite_fuel", "Elasticite_chauffage_perso", "Elasticite_fuel_perso"), miss=FALSE), 
       nsp=FALSE, labels=c("Aggregate: Housing", "Aggregate: Transport", "Own: Housing", "Own: Transport"), 
       legend = dataN("Elasticite_chauffage", return="levels", miss=FALSE), show_ticks=T)
# Correlations
cor(s$Elasticite_chauffage, s$Elasticite_chauffage_perso, use='complete.obs') # 0.48
cor(s$Elasticite_fuel, s$Elasticite_fuel_perso, use='complete.obs') # 0.52


# 4.4.2 Mobility and public transport
barres(file="transports_opinions", thin=T, title="", data=matrix(dataN("transports_avis")[c(4:1,5),], ncol=1),  legend=rev(c("PNR", "Insufficient", "Limited, but enough", "Decent, but not enough", "Satisfactory")), labels=c(" "))
decrit(s$transports_avis[s$transports_avis!=-1]<2, weights = s$weight[s$transports_avis!=-1])
decrit(s$transports_travail[s$transports_travail!='Non concerné·e'], weights = s$weight[s$transports_travail!='Non concerné·e'])
data_transports_use <- dataKN(c("transports_travail", "transports_courses", "transports_loisirs"))
data_transports_use[3,] <- data_transports_use[3,] + data_transports_use[5,]
barres(file="transports_use", title="", thin=T, nsp=T, data=data_transports_use[c(1,4,3,2,6),], legend=c("Walk/bike", "Public transport", "Other", "Car", "Unconcerned"), labels=c("Work", "Shopping", "Leisure"))
decrit(s$transports_travail_actif)
decrit(s$transports_minutes)
decrit(s$transports_distance <= 10, weights = s$weight)
s$transports_minutes <- "NSP"
s$transports_minutes[s$transports_distance <= 5] <- "5 ou moins"
s$transports_minutes[s$transports_distance > 5 & s$transports_distance <= 10] <- "6 à 10"
s$transports_minutes[s$transports_distance > 10 & s$transports_distance <= 20] <- "11 à 20"
s$transports_minutes[s$transports_distance > 20] <- "plus que 20" # TODO: preparation
barres(file="transports_distance", title="", thin=T, data=matrix(dataN("transports_minutes")[c(2,3,1,4,5),1], ncol=1), labels=c(" "), legend=c("5 or less", "6 to 10", "11 to 20", "more than 20", "PNR"))
# Sans changer de logement ni de lieu de travail, il serait possible pour le répondant prenant sa voiture de prendre les transports en commun pour ses trajets domicile-travail
barres(file="transports_frequency", title="", thin=T, nsp=T, data=matrix(dataN("transports_frequence")[c(4:1,5),1], ncol=1), legend=rev(c("PNR", "< 3/day", "1/hour - 4/day", "1/h - 2/h", "> 3/h")), labels=(" "))
barres(file="transports_work", title="", thin=T, nsp=T, data=dataKN(c("transports_travail_commun", "transports_travail_actif"))[c(3:1,4),], legend=c("Yes, no difficulty", "Yes, but bothering", "No", "PNR"), color=color(6, grey=T)[c(1,2,5,6)], labels=c("Public transport", "Walk or bike"))
# TODO: one or the other
decrit(entd$Mode, weights = entd$weight)
decrit(entd$Mode[entd$dist_subj_km <= 2], weights = entd$weight[entd$dist_subj_km <= 2])
decrit(is.na(entd$dist_subj_km)[!is.na(entd$dist_obj_km)]) # objective distance only present when subjective distance is present
decrit(entd$Mode[entd$dist_subj_km <= 2 & entd$dist_subj_km >= 1], weights = entd$weight[entd$dist_subj_km <= 2 & entd$dist_subj_km >= 1])
decrit(entd$Mode[entd$dist_subj_km <= 2 & entd$dist_subj_km >= 1 & !is.na(entd$dist_obj_km)], weights = entd$weight[entd$dist_subj_km <= 2 & entd$dist_subj_km >= 1 & !is.na(entd$dist_obj_km)])
# when objective dist not NA, car -4%, public +4% in subjective: there is selection for calculation of objective
decrit(entd$Mode[entd$dist_obj_km <= 2 & entd$dist_obj_km >= 1], weights = entd$weight[entd$dist_obj_km <= 2 & entd$dist_obj_km >= 1])
decrit(s$transports_travail_commun=='Non' & s$transports_travail_actif=='Non', weights = s$weight)


##### 5. Attitudes over Other Policies #####
## 5.1 Preferred revenue recycling
for (variable in variables_taxe_condition) {
  cat(variable)
  print(decrit(s[variable], weights=s$weight))
}

labels_tax_condition <- c("a payment for the 50% poorest French<br> (those earning less than 1670€/month)", "a payment to all French people", 
                          "compensation for households forced to consume petroleum products", "a reduction in social contributions", "a VAT cut", 
                          "a reduction in the public deficit", "the thermal renovation of buildings", "renewable energies (wind, solar, etc.)", "non polluting transport")
labels_tax_condition[3] <- "compensation for households constrained<br> to consume petroleum products"
barres(file="tax_condition_val", title="", data=data5(names(s)[which(names(s)=='si_pauvres'):(which(names(s)=='si_pauvres')+8)], miss=FALSE, rev=T)[,rev(c(9,5,8,7,3,4,1,6,2))], nsp=FALSE, 
       sort=F, thin=T, legend = rep("", 5), labels=labels_tax_condition[rev(c(9,5,8,7,3,4,1,6,2))], margin_l=220) # rev(yes_no5)

## 5.2 Other instruments

# Favored environmental policies
for (variable in variables_politiques_environnementales) {
  cat(variable)
  print(decrit(s[variable], weights=s$weight))
}

labels_environmental_policies <- c("a tax on kerosene (aviation)", "a tax on red meat", "stricter insulation standards for new buildings", 
                                   "stricter standards on pollution from new vehicles", "stricter standards during roadworthiness tests", 
                                   "the prohibition of polluting vehicles in city centres", "the introduction of urban tolls", "a contribution to a global climate fund")
barres(file="environmental_policies", title="", data=data5(names(s)[(which(names(s)=='si_pauvres')+10):(which(names(s)=='si_pauvres')+17)], 
                                                           miss=FALSE, rev = T)[,rev(c(3,4,1,6,5,8,2,7))], nsp=FALSE, sort=F, legend = rep("", 5), labels=labels_environmental_policies[rev(c(3,4,1,6,5,8,2,7))], thin=T) # rev(yes_no5)

## Diesel taxation
decrit(s$rattrapage_diesel, miss=T, weights=s$weight) # 59% non, 29% oui
decrit(s[s$diesel==T,]$rattrapage_diesel, miss=T, weights=s[s$diesel==T,]$weight) # 81% non, 12% oui
decrit(s[s$taille_agglo=='Paris',]$rattrapage_diesel, miss=T, weights=s[s$taille_agglo=='Paris',]$weight) # 81% non, 12% oui

barres(file="diesel_catch_up", dataKN(c("rattrapage_diesel")), thin=F, show_ticks = T, nsp=TRUE, legend=c("Yes", "No", "PNR"), labels = c(" "))

# Shale gas
decrit(s$schiste_approbation, miss=T, weights=s$weight) # 59% non, 16% oui
decrit(s$schiste_traite, weights=s$weight) # 61% traités
decrit(s$schiste_avantage, miss=T, weights=s$weight) # 56% aucun, 26% emplois, 18% CC
decrit(s[s$schiste_approbation=='Oui',]$schiste_avantage, miss=T, weights=s[s$schiste_approbation=='Oui',]$weight) # 56% aucun, 26% emplois, 18% CC
decrit(s$schiste_CC, miss=T, weights=s$weight) # 43% malvenue, 25% valable


barres(file="shale_val_nolegend", dataKN(c("schiste_approbation")), nsp=TRUE, legend=c("Yes", "No", "PNR"), labels = c(" "))


## 5.2 Preferred revenue recycling
labels_tax_condition <- c("a payment for the 50% poorest French people<br> (those earning less than 1670€/month)", "a payment to all French people", 
                          "compensation for households forced to consume petroleum products", "a reduction in social contributions", "a VAT cut", 
                          "a reduction in the public deficit", "the thermal renovation of buildings", "renewable energies (wind, solar, etc.)", "clean transport")
labels_tax_condition[3] <- "compensation for households constrained<br> to consume petroleum products"
barres(file="tax_condition", title="", data=data5(names(s)[which(names(s)=='si_pauvres'):(which(names(s)=='si_pauvres')+8)], miss=FALSE), nsp=FALSE, 
       sort=T, legend = c(yes_no5), labels=labels_tax_condition)


##### 6. Determinants #####
## 6.1 Attitudes over CC
decrit(s$Gauche_droite, weights=s$weight)
s$connaissances_CC <- s$score_ges + s$score_climate_call + 3*((s$cause_CC=='anthropique') - (s$cause_CC=="n'existe pas")) + 
  3 - (s$emission_cible > 2) - (s$emission_cible > 4) - (s$emission_cible > 6) + (s$region_CC=='Inde')
decrit(s$connaissances_CC, weights = s$weight)
decrit(s$connaissances_CC)
sd(s$connaissances_CC)
s$connaissances_CC <- (s$connaissances_CC - mean(s$connaissances_CC))/sd(s$connaissances_CC)
summary(lm(cause_CC=='anthropique' ~ as.factor(age) + as.factor(diplome) + statut_emploi, data=s, weights=s$weight)) # statut_emploi = étudiant matters for lowering the (omitted) effect of 18-24
summary(lm(cause_CC=='anthropique' ~ Gauche_droite + as.factor(diplome4) + diplome4 * gauche_droite, data=s, weights = s$weight))
summary(lm(cause_CC=='anthropique' ~ Gilets_jaunes + as.factor(diplome4) + diplome4 * gilets_jaunes, data=s, weights = s$weight))
summary(lm((effets_CC > 2) ~ Gilets_jaunes + as.factor(diplome4) + diplome4 * gilets_jaunes, data=s, weights = s$weight))
summary(lm(connaissances_CC ~ Gilets_jaunes + as.factor(diplome4) + diplome4 * gilets_jaunes, data=s, weights = s$weight))
summary(lm(connaissances_CC ~ Gauche_droite + as.factor(diplome4) + diplome4 * gauche_droite, data=s, weights = s$weight)) # .
summary(lm(cause_CC=='anthropique' ~ Gauche_droite + connaissances_CC * gauche_droite, data=s, weights = s$weight)) # **
summary(lm((effets_CC > 2) ~ Gauche_droite + connaissances_CC * gauche_droite, data=s, weights = s$weight))
summary(lm((effets_CC > 2) ~ Gilets_jaunes + connaissances_CC * gilets_jaunes, data=s, weights = s$weight))

s$anthropique <- s$cause_CC=='anthropique'
s$mode_vie_ecolo_oui <- s$mode_vie_ecolo=='Oui'
s$male <- s$sexe=='Masculin'
s$ecolo <- s$ecologiste==T
data_cor <- s[,c("anthropique", "connaissances_CC", "effets_CC", "parle_CC", "mode_vie_ecolo_oui", "nb_politiques_env", "tax_acceptance", "ecolo", "diplome4", "age", "taille_agglo", "Revenu")] # , "gauche_droite", "gilets_jaunes"
names(data_cor) <- c("Anthropic", "Knowledge", "Gravity", "Frequency talks", "Ecological lifestyle", "Number policies", "Tax acceptance", "Ecologist", "Diploma", "Age", "Size of town", "Income")
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

variables_determinants_attitudes <- c("Revenu", "Revenu_conjoint", "Gilets_jaunes", "as.factor(diplome4)", # as.factor(ifelse(is.missing(s$Gilets_jaunes), 'NA', as.character(s$Gilets_jaunes)))
                            "(nb_adultes==1)", variables_demo, variables_politiques, variables_energie, variables_mobilite) # 
variables_determinants_attitudes <- variables_determinants_attitudes[!(variables_determinants_attitudes %in% c("revenu", "rev_tot", "niveau_vie", "age", "age_18_24", "diplome4",
                                                                                 names(s)[which(grepl("Chauffage", names(s)))], names(s)[which(grepl("Mode_chauffage", names(s)))],
                                                                                 names(s)[which(grepl("hausse_", names(s)))]))]
variables_determinants_attitudes_CC <- variables_determinants_attitudes[c(21, 27, 3, 28, 4, 17:20, 1, 6, 15, 39)]
for (v in variables_determinants_attitudes) if (!(v %in% variables_determinants_attitudes_CC)) variables_determinants_attitudes_CC <- c(variables_determinants_attitudes_CC, v)

s$Gauche_droite <- relevel(s$Gauche_droite, 'Indeterminate')

# (1) Cause of CC (anthropic or not)
formula_determinants_cause <- as.formula(paste("cause_CC=='anthropique' ~ ", paste(variables_determinants_attitudes_CC, collapse = ' + ')))
cause_ols1 <- lm(formula_determinants_cause, data=s, weights = s$weight)
summary(cause_ols1)

# (2) Cause of CC: diploma, age
cause_ols2 <- lm(cause_CC=='anthropique' ~ age_25_34 + age_35_49 + age_50_64 + age_65_plus, data=s, weights = s$weight)
summary(cause_ols2)

# (3) Cause of CC: politics
cause_ols3 <- lm(cause_CC=='anthropique' ~ Gauche_droite + as.factor(diplome4) + diplome4 * gauche_droite, data=s, weights = s$weight)
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
effects_ols2 <- lm((effets_CC > 2) ~ Gauche_droite + as.factor(diplome4) + diplome4 * gauche_droite, data=s, weights = s$weight) # diplome4 ou connaissances_CC ?
summary(effects_ols2)

Table_determinants_attitudes_CC <- stargazer(cause_ols1, cause_ols2, cause_ols3, knowledge_ols1, effects_ols1, effects_ols2,
                                   title="Determinants of attitudes towards climate change (CC).", model.names = FALSE, model.numbers = T, 
                                   covariate.labels = c("Interest in politics (0 to 2)", "Ecologist", "Yellow Vests: PNR", "Yellow Vests: understands", 
                                                        "Yellow Vests: supports", "Yellow Vests: is part", "Left-right: Extreme-left", "Left-right: Left", 
                                                        "Left-right: Center", "Left-right: Right", "Left-right: Extreme-right", "Diploma: \\textit{CAP} or \\textit{BEP}", 
                                                        "Diploma: \\textit{Baccalauréat}", "Diploma: Higher", "Age: 25 -- 34","Age: 35 -- 49","Age: 50 -- 64", "Age: $\\geq$ 65", 
                                                        "Income (k\\euro{}/month)", "Sex: Male", "Size of town (1 to 5)", "Frequency of public transit", "Diploma $\\times$ Left-right"),
                                   header = FALSE, dep.var.labels = c("CC is anthropic", "Knowledge on CC", "CC is disastrous"),  dep.var.caption = "", 
                                   keep = c("sexe", "Revenu$", "age_", "\\(diplome", "diplome4:", "taille_agglo", "Gilets_jaunes", "ecologiste", 
                                            "Gauche_droite", "interet_politique", "transports_frequence"), # "humaniste", , "transports_avis", "conso"
                                   add.lines = list(c("Additional covariates & \\checkmark &  &  & \\checkmark & \\checkmark &  \\\\ ")),
                                   no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:determinants_attitudes_CC")
write_clip(gsub('\\end{table}', '}{\\\\ $\\quad$ \\\\                \\footnotesize \\textsc{Note:} Standard errors are reported in parentheses. Interaction term is computed using numeric variables. Omitted modalities are: \\textit{Yellow Vests: opposes}, \\textit{Left-right: Indeterminate}, \\textit{Diploma: Brevet or no diploma}, \\textit{Age: 18 -- 24}. Additional covariates are defined in \\ref{app:covariatesTODO}. }                \\end{table*} ', 
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', gsub('\\begin{table}', '\\begin{table*}',
                                                       Table_determinants_attitudes_CC, fixed=TRUE), fixed=TRUE), fixed=T), collapse=' ')


## 6.2 Attitudes over policies
s$nb_politiques_env <- 0
for (v in variables_politiques_environnementales) s$nb_politiques_env[s[[v]]>0] <- 1 + s$nb_politiques_env[s[[v]]>0]

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

s$normes_vs_taxes <- (s$normes_vs_taxes - mean(s$normes_vs_taxes))/sd(s$normes_vs_taxes)
s$earmarked_vs_compensation <- (s$earmarked_vs_compensation - mean(s$earmarked_vs_compensation))/sd(s$earmarked_vs_compensation)

decrit(s$taxe_approbation, weights = s$weight, miss=TRUE)
decrit(s$nb_politiques_env, weights = s$weight) #TODO: exploiter le -2/+2
decrit(s$mode_vie_ecolo, weights = s$weight, miss=TRUE)
decrit(s$normes_vs_taxes, weights = s$weight)
decrit(s$earmarked_vs_compensation, weights = s$weight)

variables_determinants_policy <- c("Revenu", "Revenu_conjoint", "connaissances_CC", "Gilets_jaunes",
                            "(nb_adultes==1)", variables_demo, variables_politiques, variables_energie, variables_mobilite) # 
variables_determinants_policy <- variables_determinants_policy[!(variables_determinants_policy %in% c("revenu", "rev_tot", "niveau_vie", "age", "age_18_24",
                                                                                 names(s)[which(grepl("Chauffage", names(s)))], names(s)[which(grepl("Mode_chauffage", names(s)))],
                                                                                 names(s)[which(grepl("hausse_", names(s)))]))]
variables_determinants_policy_CC <- variables_determinants_policy[c(3, 22, 28, 4, 29, 10, 18:21, 1, 6, 16, 40)]
for (v in variables_determinants_policy) if (!(v %in% variables_determinants_policy_CC)) variables_determinants_policy_CC <- c(variables_determinants_policy_CC, v)

variables_determinants_policy_CC_bis <- variables_determinants_policy_CC[!(variables_determinants_policy_CC %in% c("Gilets_jaunes", "Gauche_droite", "connaissances_CC", "interet_politique", "ecologiste"))]
variables_determinants_policy_CC_ter <- c("diplome4", "age_25_34", "age_35_49", "age_50_64", "age_65_plus", "Revenu", "sexe", "taille_agglo", "transports_frequence")
# TODO: la seule différence entre variables 6.1 et 6.2 c'est le diplôme (continu ou pas): simplifier
formula_determinants_taxe_approbation <- as.formula(paste("taxe_approbation!='Non' ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_taxe_approbation_bis <- lm(formula_determinants_taxe_approbation, data=s, weights = s$weight)
summary(ols_taxe_approbation)

formula_determinants_taxe_approbation_bis <- as.formula(paste("taxe_approbation!='Non' ~ ", paste(variables_determinants_policy_CC_ter, collapse = ' + ')))
ols_taxe_approbation_bis <- lm(formula_determinants_taxe_approbation_bis, data=s, weights = s$weight)
summary(ols_taxe_approbation_bis)

summary(lm("taxe_approbation!='Non' ~ Gauche_droite", data=s, weights = s$weight))
summary(lm("nb_politiques_env/8 ~ Gauche_droite", data=s, weights = s$weight))
summary(lm("taxe_approbation!='Non' ~ diplome4 + as.factor(age)", data=s, weights = s$weight))

formula_determinants_nb_politiques_env <- as.formula(paste("nb_politiques_env/8 ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_nb_politiques_env <- lm(formula_determinants_nb_politiques_env, data=s, weights = s$weight)
summary(ols_nb_politiques_env)

# formula_determinants_nb_politiques_env_bis <- as.formula(paste("nb_politiques_env/8 ~ ", paste(variables_determinants_policy_bis, collapse = ' + ')))
# ols_nb_politiques_env_bis <- lm(formula_determinants_nb_politiques_env_bis, data=s, weights = s$weight)
# summary(ols_nb_politiques_env_bis)

formula_determinants_mode_vie_ecolo <- as.formula(paste("mode_vie_ecolo == 'Oui' ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_mode_vie_ecolo <- lm(formula_determinants_mode_vie_ecolo, data=s, weights = s$weight)
summary(ols_mode_vie_ecolo)

formula_determinants_normes_vs_taxes <- as.formula(paste("normes_vs_taxes ~ ", paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_normes_vs_taxes <- lm(formula_determinants_normes_vs_taxes, data=s, weights = s$weight)
summary(ols_normes_vs_taxes)

formula_determinants_earmarked_vs_compensation <- as.formula(paste("earmarked_vs_compensation ~ ",  paste(variables_determinants_policy_CC, collapse = ' + ')))
ols_earmarked_vs_compensation <- lm(formula_determinants_earmarked_vs_compensation, data=s, weights = s$weight)
summary(ols_earmarked_vs_compensation)

Table_politiques_env <- stargazer(ols_taxe_approbation, ols_taxe_approbation_bis, ols_nb_politiques_env, ols_normes_vs_taxes, ols_earmarked_vs_compensation, ols_mode_vie_ecolo,
                                   title="Determinants of attitudes towards climate policies", model.names = FALSE, model.numbers = T, 
                                  covariate.labels = c("Knowledge on CC", "Interest in politics (0 to 2)", "Ecologist", "Yellow Vests: PNR", "Yellow Vests: understands", 
                                                       "Yellow Vests: supports", "Yellow Vests: is part", "Left-right: Extreme-left", "Left-right: Left", 
                                                       "Left-right: Center", "Left-right: Right", "Left-right: Extreme-right", "Diploma (1 to 4)", 
                                                       "Age: 25 -- 34","Age: 35 -- 49","Age: 50 -- 64", "Age: $\\geq$ 65", 
                                                       "Income (k\\euro{}/month)", "Sex: Male", "Size of town (1 to 5)", "Frequency of public transit"),
                                   header = FALSE, dep.var.labels = c("Tax \\& dividend", "Share of policies", "norms vs. taxes", "earmarking vs. transfers", "ecological lifestyle"),  dep.var.caption = "", 
                                   keep = c("Revenu$", "connaissances_CC", "sexe", "age_", "diplome", "_agglo", "interet_politique", "Gilets_jaunes", "ecologiste", "Gauche_droite", "transports_frequence"), 
                                   add.lines = list(c("Additional covariates & \\checkmark & & \\checkmark  & \\checkmark & \\checkmark & \\checkmark  \\\\ ")),
                                   no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:politiques_env")
write_clip(gsub('\\end{table}', '} \\\\ \\quad \\\\ {\\footnotesize \\textsc{Note:} Standard errors are reported in parentheses. Omitted variables are \\textit{Yellow Vests: opposes}, \\textit{Age : 18 -- 24} and \\textit{Left-right: Indeterminate}. Additional covariates are defined in \\ref{app:covariatesTODO}.} \\end{table*}', 
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', gsub('\\begin{table}', '\\begin{table*}',
                                                  gsub('\\\\[-1.8ex] & \\multicolumn{2}{c}{Tax \\& dividend} & Share of policies & norms vs. taxes & earmarking vs. transfers & ecological lifestyle \\\\',
                                                       '\\\\[-1.8ex] & \\multicolumn{2}{c}{Acceptance of} & Share of policies & Norms & Earmarking & Ecological \\\\ \\\\[-1.8ex] & \\multicolumn{2}{c}{Tax \\& dividend} & approved & vs. taxes & vs. transfers & lifestyle \\\\',
                                                      Table_politiques_env, fixed=TRUE), fixed=TRUE), fixed=T), fixed=T), collapse=' ')


##### Appendix #####

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

Table_diesel <- stargazer(ols_diesel_1, ols_diesel_2, ols_diesel_3, ols_diesel_4,
                                  title="Determinants of attitudes towards diesel taxation", model.names = FALSE, model.numbers = T, 
                                  covariate.labels = c("Knowledge on CC", "Ecologist", "Yellow Vests: PNR", "Yellow Vests: understands", 
                                                       "Yellow Vests: supports", "Yellow Vests: is part", "Left-right: Extreme-left", "Left-right: Left", 
                                                       "Left-right: Center", "Left-right: Right", "Left-right: Extreme-right",
                                                       "Size of town: -20k", "Size of town: 20-100k", "Size of town: +100k", "Size of town: Paris",
                                                       "Diesel", "Gasoline", "Number vehicles", "Frequency of public transit"),
                                  header = FALSE, dep.var.labels = c("Acceptance increase in diesel taxation"),  dep.var.caption = "", 
                                  keep = c("connaissances_CC", "_agglo", "Gilets_jaunes", "ecologiste", "Gauche_droite", "transports_frequence", "diesel", "essence", "nb_vehicules"), 
                                  add.lines = list(c("Additional covariates & \\checkmark &  &  &  \\\\ ")),
                                  no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:determinants_diesel")
write_clip(gsub('\\end{table}', '} \\\\ \\quad \\\\ {\\footnotesize \\textsc{Note:} Standard errors are reported in parentheses. Omitted variables are \\textit{Yellow Vests: opposes}, \\textit{Age : 18 -- 24} and \\textit{Left-right: Indeterminate}. Additional covariates are defined in \\ref{app:covariatesTODO}.} \\end{table*}', 
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
                             title="Effect of being treated on acceptance of shale gas exploitation", model.names = FALSE, #star.cutoffs = c(0.1, 1e-5, 1e-30),
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
# 6.1 in logit
cause_logit1 <- glm(formula_determinants_cause, family = binomial(link='logit'), data=s)
summary(cause_logit1)
logit_cause1_margins <- logitmfx(cause_logit1, s, atmean=FALSE)$mfxest
logit_cause1_margins

cause_logit2 <- glm(cause_CC=='anthropique' ~ age_25_34 + age_35_49 + age_50_64 + age_65_plus, family = binomial(link='logit'), data=s)
summary(cause_logit2)
logit_cause2_margins <- logitmfx(cause_logit2, s, atmean=FALSE)$mfxest
logit_cause2_margins

cause_logit3 <- glm(cause_CC=='anthropique' ~ Gauche_droite + as.factor(diplome4) + diplome4 : gauche_droite, family = binomial(link='logit'), data=s)
summary(cause_logit3)
logit_cause3_margins <- logitmfx(cause_logit3, s, atmean=FALSE)$mfxest
logit_cause3_margins

effects_logit <- glm(formula_determinants_effets, family = binomial(link='logit'), data=s)
summary(effects_logit)
logit_effects_margins <- logitmfx(effects_logit, s, atmean=FALSE)$mfxest
logit_effects_margins

effects_logit2 <- glm((effets_CC > 2) ~ Gauche_droite + as.factor(diplome4) + diplome4 : gauche_droite, family = binomial(link='logit'), data=s)
summary(effects_logit2)
logit_effects2_margins <- logitmfx(effects_logit2, s, atmean=FALSE)$mfxest
logit_effects2_margins

Table_determinants_attitudes_CC_logit <- stargazer(cause_logit1, cause_logit2, cause_logit3, effects_logit, effects_logit2,
     title="Determinants of attitudes towards climate change (CC).", model.names = FALSE, model.numbers = T, 
     covariate.labels = c("Interest in politics (0 to 2)", "Ecologist", "Yellow Vests: PNR", "Yellow Vests: understands", 
                          "Yellow Vests: supports", "Yellow Vests: is part", "Left-right: Extreme-left", "Left-right: Left", 
                          "Left-right: Center", "Left-right: Right", "Left-right: Extreme-right", "Diploma: \\textit{CAP} or \\textit{BEP}", 
                          "Diploma: \\textit{Baccalauréat}", "Diploma: Higher", "Age: 25 -- 34","Age: 35 -- 49","Age: 50 -- 64", "Age: $\\geq$ 65", 
                          "Income (k\\euro{}/month)", "Sex: Male", "Size of town (1 to 5)", "Frequency of public transit", "Diploma $\\times$ Left-right"),
     header = FALSE, dep.var.labels = c("CC is anthropic", "CC is disastrous"),  dep.var.caption = "", 
     coef = list(logit_cause1_margins[,1], logit_cause2_margins[,1], logit_cause3_margins[,1], logit_effects_margins[,1], logit_effects2_margins[,1]),
     se = list(logit_cause1_margins[,2], logit_cause2_margins[,2], logit_cause3_margins[,2], logit_effects_margins[,2], logit_effects2_margins[,2]),
     keep = c("sexe", "Revenu$", "age_", "\\(diplome", "diplome4:", "taille_agglo", "Gilets_jaunes", "ecologiste", 
              "Gauche_droite", "interet_politique", "transports_frequence"), # "humaniste", , "transports_avis", "conso"
     add.lines = list(c("Additional covariates & \\checkmark &  &  & \\checkmark &  \\\\ ")),
     no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:determinants_attitudes_CC")
write_clip(gsub('\\end{table}', '}{\\\\ $\\quad$ \\\\                \\footnotesize \\textsc{Note:} Standard errors are reported in parentheses. Interaction term is computed using numeric variables. Omitted modalities are: \\textit{Yellow Vests: opposes}, \\textit{Left-right: Indeterminate}, \\textit{Diploma: Brevet or no diploma}, \\textit{Age: 18 -- 24}. Additional covariates are defined in \\ref{app:covariatesTODO}. }                \\end{table*} ', 
                gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', gsub('\\begin{table}', '\\begin{table*}',
                                                                                                 Table_determinants_attitudes_CC_logit, fixed=TRUE), fixed=TRUE), fixed=T), collapse=' ')

# 6.2 in logit
