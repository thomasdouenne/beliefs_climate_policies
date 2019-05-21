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

ges_climate_call <- rev(paste("ges_correct", c("avion", "nucleaire", "boeuf", "O2", "CO2", "CH4", "pm"), sep="_")) 
labels_ges_climate_call <- rev(c("Plane", "Nuclear", "Beaf", "Oxygen", "CO<sub>2</sub>", "Methane", "Particulates")) 
oui_non(margin_l=20, ges_climate_call, NSP=FALSE, en=c("Correct", "Wrong"), labels = labels_ges_climate_call, sort=FALSE)
barres(file="CC_target_emission", title="", data=dataN("emission_cible", miss=FALSE), nsp=FALSE, sort=T, color = rev(brewer.pal(11, "RdBu")), 
       legend = dataN("emission_cible", return="levels"), labels=c("Emission compatible with +2°C (tCO<sub>2</sub>e/yr p.c.)")) 
barres(file="CC_cause", title="", data=dataN("cause_CC"), nsp=T, sort=T, legend = c("Anthropic", "Natural", "Does not exist", "NSP"), labels=c("Cause of CC"))
barres(file="CC_generation_min", title="", rev_color = T, data=dataN("generation_CC_min"), nsp=T, sort=T, 
       legend = c(dataN("generation_CC_min", return="levels")[1:4], "PNR"), labels=c("First generation of French severely affected by CC (born in...)"))
s$region_CC <- as.factor(s$region_CC)
s$region_CC <- relevel(relevel(s$region_CC, "Autant dans les deux"), "L'Inde")
barres(file="CC_region", title="", data=dataN("region_CC", miss=FALSE), nsp=FALSE, sort=T, 
       legend = c("India", "As much in both", "European Union", "NSP"), labels=c("Region with biggest consequences of CC"))


## 3.2 Opinions
decrit(s$ecologiste, weights=s$weight) # 15% écologistes
decrit(s$parle_CC, miss=T, weights = s$weight) # 3 tiers
decrit(s$effets_CC, miss=T, weights = s$weight) # 20% cataclysmiques; 31% désastreux, 38% graves
decrit(s$responsable_CC_chacun, miss=T, weights = s$weight) # 63%
decrit(s$responsable_CC_govts, miss=T, weights = s$weight) # 47%
decrit(s$responsable_CC_etranger, miss=T, weights = s$weight) # 42%
decrit(s$responsable_CC_riches, miss=T, weights = s$weight) # 25%
decrit(s$responsable_CC_nature, miss=T, weights = s$weight) # 23%
decrit(s$responsable_CC_passe, miss=T, weights = s$weight) # 21%
decrit(s$ecologiste, miss=T, weights = s$weight)

labels_resp <- c("Each one of us", "Governments", "Certain foreign countries", "The richest", "Natural causes", "Past generations")
barres(file="CC_responsible", title="", data=data1(names(s)[which(grepl("responsable_CC", names(s)))]), sort=T, showLegend=FALSE, labels=labels_resp, hover=labels_resp)
barres(file="CC_effects", title="", thin=T, data=dataN("effets_CC"), nsp=T, sort=T, 
       legend = c("Insignificant", "Small", "Serious", "Disastrous", "Cataclysmic", "NSP"), labels=c("Consequences of CC"))
s$parle_CC <- as.factor(s$parle_CC)
s$parle_CC <- relevel(relevel(s$parle_CC, "Plusieurs fois par an"), "Plusieurs fois par mois")
barres(file="CC_talks", title="", data=dataN("parle_CC"), nsp=T, sort=T, 
       legend = c("Several times per month", "Several times per year", "Almost never", "PNR"), labels=c("Talks about CC...")) 

## 3.3 Reaction needed
decrit(s$mode_vie_ecolo, miss=T, weights = s$weight) # 65%
decrit(s$enfant_CC, weights = s$weight) # 20% oui
decrit(s$enfant_CC_pour_CC[s$enfant_CC=='Oui'], weights = s$weight[s$enfant_CC=='Oui']) # 37%
decrit(s$enfant_CC_pour_lui[s$enfant_CC=='Oui'], weights = s$weight[s$enfant_CC=='Oui']) # 86%
summary(lm((enfant_CC=='Oui') ~ sexe, data=s)) #V -3.4 p.p.

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
barres(file="changer_si_non", title="", data=data1(variables_changer), sort=T, showLegend=FALSE, labels=labels_changer, hover=labels_changer)

decrit((s$emission_cible[s$changer_deja_fait==T] >= 3), weights = s$weight[s$changer_deja_fait==T]) # 79%
decrit((s$emission_cible[s$changer_deja_fait==F] >= 3), weights=s$weight[s$changer_deja_fait==F]) # 85%

##### 4. Attitudes over Carbon Tax and Dividend #####

## 4.1 Massive rejection

## 4.2 Perceptived benefits
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
barres(file="CC_benefits", title="", data=data1(variables_benefits), sort=T, showLegend=FALSE, labels=labels_benefits, hover=labels_benefits)
# pb 35% NSP

## 4.3 Perceived problems
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
barres(file="CC_problems", title="", data=data1(variables_problems), sort=T, showLegend=FALSE, labels=labels_problems, hover=labels_problems)


## 4.4 Perceived elasticities
s$elast_chauffage_perso <- factor(s$elasticite_chauffage_perso, levels(as.factor(s$elasticite_chauffage_perso))[c(1,6:2)])
s$elast_fuel_perso <- factor(s$elasticite_fuel_perso, levels(as.factor(s$elasticite_fuel_perso))[c(1,6:2)])
s$elast_chauffage_perso <- revalue(s$elast_chauffage_perso, c("+ de 30% - Je changerais largement ma consommation"="> 30%", 
          "de 20% à 30%"="20 to 30%", "de 10% à 20%"="10 to 20%", "de 0% à 10%"="0 to 10%", 
          "0% - Je ne la réduirais pas"="0%: won't reduce", "0% - Je n'en consomme déjà pas"="0%: don't consume"))
# s$elast_fuel_perso <- revalue(s$elast_fuel_perso, c("+ de 30% - Je changerais largement mes habitudes de déplacement"="> 30%", 
#           "de 20% à 30%"="20 to 30%", "de 10% à 20%"="10 to 20%", "de 0% à 10%"="0 to 10%", 
#           "0% - Je suis contraint sur tous mes déplacements"="0%: won't reduce", "0% - Je n'en consomme déjà presque pas"="0%: don't consume"))
barres(file="elasticities_perso", thin=T, title="", data=dataKN(c("elast_chauffage_perso", "elast_fuel_perso"), miss=FALSE), 
       nsp=FALSE, labels=c("Own: Housing", "Own: Transport"), legend = dataN("elast_chauffage_perso", return="legend"), show_ticks=FALSE)
s$elast_chauffage <- factor(s$elasticite_chauffage, levels(as.factor(s$elasticite_chauffage))[c(1,4,3,5,2)])
s$elast_fuel <- factor(s$elasticite_fuel, levels(as.factor(s$elasticite_fuel))[c(1,4,3,5,2)])
s$elast_chauffage <- revalue(s$elast_chauffage, c("+ de 30%"="> 30%", "de 20% à 30%"="20 to 30%", 
                                "de 10% à 20%"="10 to 20%", "de 0% à 3%"="0 to 3%", "de 3% à 10%"="3 to 10%"))
# s$elast_fuel <- revalue(s$elast_fuel, c("+ de 30%"="> 30%", "de 20% à 30%"="20 to 30%", "de 10% à 20%"="10 to 20%", "de 0% à 3%"="0 to 3%", "de 3% à 10%"="3 to 10%"))
barres(file="elasticities_agg", thin=T, title="", data=dataKN(c("elasticite_chauffage", "elasticite_fuel"), miss=FALSE), 
       nsp=FALSE, labels=c("Aggregate: Housing", "Aggregate: Transport"), legend = dataN("elast_chauffage", return="legend"), show_ticks=FALSE)
barres(file="elasticities", title="", thin=T, data=dataKN(c("Elasticite_chauffage", "Elasticite_fuel", "Elasticite_chauffage_perso", "Elasticite_fuel_perso"), miss=FALSE), 
       nsp=FALSE, labels=c("Aggregate: Housing", "Aggregate: Transport", "Own: Housing", "Own: Transport"), 
       legend = dataN("Elasticite_chauffage", return="levels", miss=FALSE), show_ticks=FALSE)


##### 5. Attitudes over Other Policies #####
## 5.1 Other instruments

# Favored environmental policies
labels_environmental_policies <- c("a tax on kerosene (aviation)", "a tax on red meat", "stricter insulation standards for new buildings", 
                 "stricter standards on pollution from new vehicles", "stricter standards on pollution during roadworthiness tests", 
        "the prohibition of polluting vehicles in city centres", "the introduction of urban tolls", "a contribution to a global climate fund")
barres(file="environmental_policies", title="", data=data5(names(s)[(which(names(s)=='si_pauvres')+10):(which(names(s)=='si_pauvres')+17)], 
                                                           miss=FALSE, rev=T), nsp=FALSE, sort=T, legend = rev(yes_no5), labels=labels_environmental_policies)

## 5.2 Preferred revenue recycling
labels_tax_condition <- c("a payment for the 50% poorest French people<br> (those earning less than 1670€/month)", "a payment to all French people", 
                          "compensation for households forced to consume petroleum products", "a reduction in social contributions", "a VAT cut", 
            "a reduction in the public deficit", "the thermal renovation of buildings", "renewable energies (wind, solar, etc.)", "clean transport")
labels_tax_condition[3] <- "compensation for households constrained<br> to consume petroleum products"
barres(file="tax_condition_val", title="", data=data5(names(s)[which(names(s)=='si_pauvres'):(which(names(s)=='si_pauvres')+8)], miss=FALSE, rev=T), nsp=FALSE, 
       sort=T, thin=T, legend = rev(yes_no5), labels=labels_tax_condition)
barres(file="tax_condition_valr", title="", data=data5(names(s)[which(names(s)=='si_pauvres'):(which(names(s)=='si_pauvres')+8)], miss=FALSE), nsp=FALSE, 
       sort=T, legend = c(yes_no5), rev_color=T, labels=labels_tax_condition)

## Favored environmental policies

## Diesel taxation
decrit(s$rattrapage_diesel, miss=T) # 59% non, 29% oui

variables_diesel <- c("Revenu", "score_ges", "score_climate_call", variables_demo, variables_energie) # 
variables_diesel <- variables_diesel[!(variables_diesel %in% c("revenu", "rev_tot", "age", "age_65_plus",
                                                               names(s)[which(grepl("Chauffage", names(s)))], names(s)[which(grepl("Mode_chauffage", names(s)))],
                                                               names(s)[which(grepl("hausse_", names(s)))]))]
formula_diesel <- as.formula(paste("rattrapage_diesel!='Non' ~ ",
                                   paste(variables_diesel, collapse = ' + ')))
summary(lm(formula_diesel, data=s, weights = s$weight)) # Strongest determinant: use of Diesel / Scores ges and CC also matter
logit_diesel <- glm(formula_diesel, family = binomial(link='logit'), data=s)
summary(logit_diesel)
logit_diesel_margins <- logitmfx(formula_diesel, s, atmean=FALSE)$mfxest
logit_diesel_margins

barres(file="diesel_catch_up_val", dataKN(c("rattrapage_diesel")), nsp=TRUE, legend=c("Yes", "No", "PNR"), color=, labels = c("Favorable to catch-up diesel taxes"))

# Shale gas
decrit(s$schiste_approbation, miss=T) # 59% non, 17% oui
decrit(s$schiste_traite) # 59% traités
decrit(s$schiste_avantage, miss=T) # 56% aucun, 26% emplois, 18% CC
decrit(s$schiste_CC, miss=T) # 44% malvenue, 25% valable

summary(lm((schiste_approbation!='Non') ~ (schiste_traite==1), data=s, weights = s$weight)) # - 3.9 p.p. acceptance when treated
variables_reg_schiste <- c("Revenu", "score_ges", "score_climate_call", variables_demo) # 
variables_reg_schiste <- variables_reg_schiste[!(variables_reg_schiste %in% c("revenu", "rev_tot", "age", "age_65_plus"))]
formula_schiste_approbation <- as.formula(paste("schiste_approbation!='Non' ~ (schiste_traite==1) + ",
                                                paste(variables_reg_schiste, collapse = ' + ')))
summary(lm(formula_schiste_approbation, data=s, weights = s$weight)) # - 5.1 p.p. acceptance when treated / Scores, Sex and Education matter
logit_schiste_approbation <- glm(formula_schiste_approbation, family = binomial(link='logit'), data=s)
summary(logit_schiste_approbation)
logit_schiste_approbation_margins <- logitmfx(formula_schiste_approbation, s, atmean=FALSE)$mfxest
logit_schiste_approbation_margins # -5.7 p.p. with logit
summary(lm((schiste_approbation=='Oui') ~ (schiste_traite==1), data=s, weights = s$weight)) # Not significant for approval

barres(file="shale_val", dataKN(c("schiste_approbation")), nsp=TRUE, legend=c("Yes", "No", "PNR"), color=, labels = c("Favorable to shale gas extraction"))


## 5.2 Preferred revenue recycling
labels_tax_condition <- c("a payment for the 50% poorest French people<br> (those earning less than 1670€/month)", "a payment to all French people", 
                          "compensation for households forced to consume petroleum products", "a reduction in social contributions", "a VAT cut", 
            "a reduction in the public deficit", "the thermal renovation of buildings", "renewable energies (wind, solar, etc.)", "clean transport")
labels_tax_condition[3] <- "compensation for households constrained<br> to consume petroleum products"
barres(file="tax_condition", title="", data=data5(names(s)[which(names(s)=='si_pauvres'):(which(names(s)=='si_pauvres')+8)], miss=FALSE), nsp=FALSE, 
       sort=T, legend = c(yes_no5), labels=labels_tax_condition)

##### 6. Determinants

## 6.1 Attitudes over CC
summary(lm((s$cause_CC=='anthropique') ~ Revenu, data=s)) # Pas significatif
summary(lm((s$cause_CC=='anthropique') ~ sexe, data=s)) # Pas significatif
summary(lm((s$cause_CC=='anthropique') ~ as.factor(taille_agglo), data=s)) # Paris et +100k vs rural : env +8.5 p.p.
summary(lm((s$cause_CC=='anthropique') ~ Gilets_jaunes, data=s)) # -10 p.p. soutient / -20 p.p. est dedans
summary(lm((s$cause_CC=='anthropique') ~ Gauche_droite, data=s)) # -18 p.p. droite / -21 p.p. ext-droite / -12 p.p. indeter / -9 p.p. centre

summary(lm((s$effets_CC > 2) ~ Revenu, data=s)) # Pas significatif
summary(lm((s$effets_CC > 2) ~ sexe, data=s)) # Pas significatif
summary(lm((s$effets_CC > 2) ~ as.factor(taille_agglo), data=s)) # Peu significatif (grandes villes + 6.7 p.p.)
summary(lm((s$effets_CC > 2) ~ Gilets_jaunes, data=s)) # -5.6 p.p. soutient / -7.8 p.p. est dedans
summary(lm((s$effets_CC > 2) ~ Gauche_droite, data=s)) # -19 p.p. droite / -16 p.p. ext-droite / -11 p.p. indeter

variables_determinants <- c("Revenu", "Revenu_conjoint", "as.factor(ifelse(is.missing(s$Gilets_jaunes), 'NA', as.character(s$Gilets_jaunes)))", 
                            "(nb_adultes==1)", variables_demo, variables_politiques, variables_energie) # 
variables_determinants <- variables_determinants[!(variables_determinants %in% c("revenu", "rev_tot", "niveau_vie", "age", "age_65_plus",
                                                               names(s)[which(grepl("Chauffage", names(s)))], names(s)[which(grepl("Mode_chauffage", names(s)))],
                                                               names(s)[which(grepl("hausse_", names(s)))]))]
formula_determinants_cause <- as.formula(paste("cause_CC=='anthropique' ~ ", paste(variables_determinants, collapse = ' + ')))
cause_ols <- lm(formula_determinants_cause, data=s, weights = s$weight)
cause_logit <- glm(formula_determinants_cause, data=s, family="binomial")
summary(cause_ols)
summary(cause_logit)
cause_logit_margins <- logitmfx(formula_determinants_cause, s, atmean=FALSE)$mfxest
cause_logit_margins

formula_determinants_effects <- as.formula(paste("effets_CC > 2 ~ ", paste(variables_determinants, collapse = ' + ')))
effects_ols <- lm(formula_determinants_effects, data=s, weights = s$weight)
effects_logit <- glm(formula_determinants_effects, data=s, family="binomial")
summary(effects_ols)
summary(effects_logit)
effects_logit_margins <- logitmfx(formula_determinants_effects, s, atmean=FALSE)$mfxest
effects_logit_margins

s$score <- (s$score_ges + s$score_climate_call)/7
formula_determinants_score <- as.formula(paste("score ~ ", paste(variables_determinants, collapse = ' + ')))
ges_ols <- lm(formula_determinants_score, data=s, weights = s$weight)
summary(ges_ols)
ges_ologit <- oglmx(formula_determinants_score, data=s, constantMEAN = FALSE, constantSD = FALSE, link="logit", delta=0)
# ges_ologit <- ologit.reg(as.formula(paste("score_ges + score_climate_call ~ ", paste(variables_determinants, collapse = ' + '))), data=s)
# ges_ologit <- polr(as.formula(paste("as.factor(score_ges + score_climate_call) ~ ", paste(variables_determinants, collapse = ' + '))), data=s, Hess = T)
# with polr, regressors should be scaled with scale() 
s$score <- as.factor((s$score_ges + s$score_climate_call)/7)
ges_ologit <- clm(formula_determinants_score, data=s)
summary(ges_ologit)
ges_ologit_margins <- margins.oglmx(ges_ologit, atmeans=FALSE, AME=T)
ges_ologit_margins
ges_logit <- glm(formula_determinants_score, data=s, family="binomial")
summary(ges_logit)
ges_logit_margins <- logitmfx(formula_determinants_score, s, atmean=FALSE)$mfxest
ges_logit_margins

Table_determinants_CC <- stargazer(cause_ols, cause_logit, effects_ols, effects_logit, ges_ols, ges_logit, ges_ologit,
           title="Determinants of attitudes towards climate change", model.names = T, model.numbers = FALSE, 
           # covariate.labels = c("Constant", "Initial tax: PNR (I don't know)", "Initial tax: Approves", "Sex: Female", "Ecologist","Consumption Units (C.U.)", 
           #                      "Yellow Vests: PNR","Yellow Vests: understands","Yellow Vests: supports", "Yellow Vests: is part"),
           header = FALSE, dep.var.labels = c("CC anthropic", "CC disastrous", "Knowledge on CC", ""),#  dep.var.caption = "", 
           keep = c("Constant", "sexe", "age", "diplome", "taille_agglo", "Gilets_jaunes", "humaniste", "ecologiste", "Gauche_droite"), 
           # "interet_politique", "csp", "diesel", "essence", "sexe", "apolitique"
           coef = list(NULL, cause_logit_margins[,1], NULL, effects_logit_margins[,1], NULL, ges_logit_margins[,1], NULL),
           se = list(NULL, cause_logit_margins[,2], NULL, effects_logit_margins[,2], NULL, ges_logit_margins[,2], NULL),
           # add.lines = list(c("Controls: Socio-demo, political leaning", "\\checkmark", "\\checkmark", "\\checkmark")),
           no.space=TRUE, intercept.bottom=FALSE, intercept.top=TRUE, omit.stat=c("adj.rsq", "f", "ser", "ll", "aic"), label="tab:bias")
write_clip(gsub('\\end{table}', '} \\end{table}', gsub('\\begin{tabular}{@', '\\makebox[\\textwidth][c]{ \\begin{tabular}{@', 
                                                       Table_determinants_CC, fixed=TRUE), fixed=TRUE), collapse=' ')
            

## 6.2 Attitudes over policies
summary(lm((s$taxe_approbation=='Oui') ~ Revenu, data=s)) # 0.7 p.p. (très faible)
summary(lm((s$taxe_approbation=='Oui') ~ sexe, data=s)) # 3.5 p.p.
summary(lm((s$taxe_approbation=='Oui') ~ as.factor(taille_agglo), data=s)) # Paris et +100k vs rural : +8.5 p.p.
summary(lm((s$taxe_approbation=='Oui') ~ Gilets_jaunes, data=s)) # -15 p.p. soutient / -14 p.p. est dedans
summary(lm((s$taxe_approbation=='Oui') ~ Gauche_droite, data=s)) # pas significtaif (centre .)

formula_determinants_taxe_approbation <- as.formula(paste("taxe_approbation!='Non' ~ ",
                                               paste(variables_determinants, collapse = ' + ')))
summary(lm(formula_determinants_taxe_approbation, data=s, weights = s$weight))

formula_determinants_mode_vie_ecolo <- as.formula(paste("mode_vie_ecolo=='Oui' ~ ",
                                                          paste(variables_determinants, collapse = ' + ')))
summary(lm(formula_determinants_mode_vie_ecolo, data=s, weights = s$weight))

s$nb_politiques_env <- 0
for (v in variables_politiques_environnementales) s$nb_politiques_env[s[[v]]>0] <- 1 + s$nb_politiques_env[s[[v]]>0]
formula_determinants_politiques_env <- as.formula(paste("nb_politiques_env/8 ~ ",
                                                        paste(variables_determinants, collapse = ' + ')))
summary(lm(formula_determinants_politiques_env, data=s, weights = s$weight))

formula_determinants_politiques_env <- as.formula(paste("taxe_kerosene ~ ",
                                                        paste(variables_determinants, collapse = ' + ')))
summary(lm(formula_determinants_politiques_env, data=s, weights = s$weight))
