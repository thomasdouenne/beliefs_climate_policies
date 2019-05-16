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
# TODO: figure for binary variables (correct/wrong), maybe include emission_cible

ges_climate_call <-  names(s)[which(grepl("ges", names(s)))]
ges_climate_call <- ges_climate_call[!(ges_climate_call %in% c("peages_urbains", "score_ges"))]
labels_ges_climate_call <- c("CO2", "CH4", "O2", "PM", "Plane", "Beaf", "Nuclear")
oui_non(margin_l=10, ges_climate_call, NSP=FALSE, en=TRUE, "questions_ges", labels_ges_climate_call)


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




##### 5. Attitudes over Other Policies #####

