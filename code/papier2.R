## Source to reproduce "French Preferences over Climate Policies"
#     by Thomas Douenne & Adrien Fabre, under licence CC-BY
# All files can be found on github: https://github.com/bixiou/beliefs_climate_policies
#     in particular, file to prepare the dataset (preparation.R), to load the packages and functions (packages_functions.R),
#     R environment with all the data prepared (.RData) and python files for complementary computations

source("packages_functions.R")
load(".RData")


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


## 3.2 Opinions
decrit(s$parle_CC, miss=T, weights = s$weight) # 3 tiers
decrit(s$effets_CC, miss=T, weights = s$weight) # 20% cataclysmiques; 31% désastreux, 38% graves
decrit(s$responsable_CC_chacun, miss=T, weights = s$weight) # 63%
decrit(s$responsable_CC_govts, miss=T, weights = s$weight) # 47%
decrit(s$responsable_CC_etranger, miss=T, weights = s$weight) # 42%
decrit(s$responsable_CC_riches, miss=T, weights = s$weight) # 25%
decrit(s$responsable_CC_nature, miss=T, weights = s$weight) # 23%
decrit(s$responsable_CC_passe, miss=T, weights = s$weight) # 21%

variables_responsable <- names(s)[which(grepl("responsable_CC", names(s)))]
labels_responsable <- c()
values_responsable <- c()
for (v in variables_responsable[1:(length(variables_responsable))]) {
  labels_responsable <- c("Each of us", "The richest", "Governments", "Some foreign countries", "Past generations", "Natural causes")
  values_responsable <- c(values_responsable, sum(s$weight[which(s[[v]]==T)])/sum(s$weight)) }

barres_responsable <- barres(file="responsable_CC", title="", data=matrix(values_responsable, ncol=length(values_responsable)), sort=T, color=c("#66B3B3"), showLegend=FALSE, labels=labels_responsable, hover=labels_responsable, legend="empty")
barres_responsable
#TODO: add % value at the end of each row + cut top space




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

variables_benefices <- names(s)[which(grepl("benefice", names(s)))[which(grepl("problemes", names(s)))>300]]
variables_benefices <- variables_benefices[!(variables_benefices %in% c("nb_benefices", "benefices_autre"))]
labels_benefices <- c()
values_benefices <- c()
for (v in variables_benefices[1:(length(variables_benefices))]) {
  labels_benefices <- c("Fights CC", "Reduces negative impact of pollution on health", "Reduces congestion", "Increases my purchasing power", "Increases purchasing power of the poorest",
                       "Increases France's independence toward fossils", "Prepares the economy for tomorrow", "None of these reasons", "Other reasons")
  values_benefices <- c(values_benefices, sum(s$weight[which(s[[v]]==T)])/sum(s$weight)) }
barres_benefices <- barres(file="benefices", title="", data=matrix(values_benefices, ncol=length(values_benefices)), sort=T, color=c("#66B3B3"), showLegend=FALSE, labels=labels_benefices, hover=labels_benefices, legend="empty")
barres_benefices
#TODO: correct benefice_autre dans preparation.R

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

variables_problemes <- names(s)[which(grepl("problemes", names(s)))[which(grepl("problemes", names(s)))>300]]
variables_problemes <- variables_problemes[!(variables_problemes %in% c("nb_problemes", "problemes_autre"))]
labels_problemes <- c()
values_problemes <- c()
for (v in variables_problemes[1:(length(variables_problemes))]) {
  labels_problemes <- c("Is ineffective to reduce pollution", "Alternatives are insufficient or too expensive", "Penalizes rural households", "Decreases my purchaisng power",
                        "Penalizes the poorest", "Hurts the economy", "Is a pretext to increase taxes", "None of these reasons", "Other reasons")
  values_problemes <- c(values_problemes, sum(s$weight[which(s[[v]]==T)])/sum(s$weight)) }
barres_problemes <- barres(file="problemes", title="", data=matrix(values_problemes, ncol=length(values_problemes)), sort=T, color=c("#66B3B3"), showLegend=FALSE, labels=labels_problemes, hover=labels_problemes, legend="empty")
barres_problemes



##### 5. Attitudes over Other Policies #####

### 5.1 Other instruments

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

oui_non(margin_l=10, c("rattrapage_diesel"), NSP=TRUE, en=TRUE, labels = rev(c("Favorable to catch-up diesel taxes")), sort=FALSE)

## Shale gas
# Approbation :
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

oui_non(margin_l=10, c("schiste_approbation"), NSP=TRUE, en=TRUE, labels = rev(c("Favorable to shale gas extraction")), sort=FALSE)


## 5.2 Other revenue recycling
