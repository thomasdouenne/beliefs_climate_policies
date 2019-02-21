setwd("/var/www/beliefs_climate_policies")

# options(download.file.method = "wget"); # For Ubuntu 14.04
package <- function(p) { 
  if (!is.element(p, installed.packages()[,1])) {
    install.packages(p); 
  }
  library(p, character.only = TRUE)
} # loads packages with automatical install if needed

package('pwr')
package("foreign")
package("memisc")
package("Hmisc")
package("DT")
package("pastecs")
package("lsr")
package("ggplot2")
package("stringr")
package("survey")
package("plotly")
package('gdata')
package('tidyverse')

Fs <- function(QID) { s[QID][[1]] }
Vs <- function(QID) { as.vector(Ff(QID))  }
n <- function(var) { as.numeric(as.vector(var)) }
fs <- function(id) { s[paste("QID", id, sep="")][[1]] }
vs <- function(id) { as.vector(f(id)) }
NSPs <- function(QID) { length(V(QID)[V(QID) == "NSP (Je ne veux pas répondre)"])/length(V(QID)) }
nsps <- function(id) { length(v(id)[v(id) == "NSP (Je ne veux pas répondre)"])/length(v(id)) }
Label <- function(var) {
  if (length(annotation(var))==1) { annotation(var)[1] }
  else { label(var)  }
}
decrit <- function(variable, miss = FALSE, weights = NULL) { 
  if (length(annotation(variable))>0) {
    if (!miss) {
      if (is.element("Oui", levels(as.factor(variable)))| is.element("quotient", levels(as.factor(variable)))  | is.element("Pour", levels(as.factor(variable)))) { describe(as.factor(variable[variable!="" & !is.na(variable)]), weights = weights[variable!="" & !is.na(variable)]) }
      else { describe(as.numeric(as.vector(variable[variable!="" & !is.na(variable)])), weights = weights[variable!="" & !is.na(variable)]) }
    }
    else describe(as.factor(include.missings(variable[variable!="" & !is.na(variable)])), weights = weights[variable!="" & !is.na(variable)]) }
  else {  describe(variable[variable!=""], weights = weights[variable!=""])  }
}
clean_number <- function(vec, high_numbers='') { 
   numeric_vec <- as.numeric(gsub(",", ".", gsub("[[:alpha:]  !#$%&')?/(@:;€_-]","",vec)))
   if (high_numbers=='remove') { is.na(numeric_vec) <- numeric_vec>10000 }
   else if (high_numbers=='divide') { numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)] <- numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)]/12 }
   else if (high_numbers=='divide&remove') { 
     numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)] <- numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)]/12
     is.na(numeric_vec) <- numeric_vec>6000 }
   return(numeric_vec)
}

##### Correspondance zipcode - region #####
communes_agglo <- read.xls("table-appartenance-geo-communes-18_V2.xls", pattern="CODGEO") # 2018
communes_PLM <- read.xls("table-appartenance-geo-communes-18_V2.xls", sheet=2, pattern="CODGEO") # Paris Lyon Marseille
communes_data <- read.csv("correspondance-code-insee-code-postal.csv", sep=";") # 2013
communes_agglo <- communes_agglo[,c('CODGEO', 'TUU2015')]
communes_PLM <- communes_PLM[,c('CODGEO', 'TUU2015')]
colnames(communes_agglo) <- c('Code.INSEE', 'taille_agglo')
colnames(communes_PLM) <- c('Code.INSEE', 'taille_agglo')
communes_agglo$Code.INSEE <- as.character(communes_agglo$Code.INSEE)
communes_PLM$Code.INSEE <- as.character(communes_PLM$Code.INSEE)
communes_data <- communes_data[,c('Code.INSEE', "Code.Postal", 'Population')]
communes <- merge(merge(communes_agglo, communes_PLM, all=T), communes_data, all=T)
sum(communes$Population[is.na(communes$taille_agglo)], na.rm=T) # 750k missing because of Code.INSEE renaming
taille_agglo <- aggregate(1000*Population ~ taille_agglo, communes, sum)
colnames(taille_agglo) <- c('taille_agglo', 'pop')
taille_agglo$share <- taille_agglo$pop / sum(taille_agglo$pop)
taille_agglo # total: 63.76 M
taille_agglo$share[taille_agglo$taille_agglo==0] # rural
sum(taille_agglo$share[taille_agglo$taille_agglo<=3 & taille_agglo$taille_agglo>0]) # <20k
sum(taille_agglo$share[taille_agglo$taille_agglo<=5 & taille_agglo$taille_agglo>3]) # <100k
sum(taille_agglo$share[taille_agglo$taille_agglo<8 & taille_agglo$taille_agglo>5]) # >100k
taille_agglo$share[taille_agglo$taille_agglo==8] # Paris


##### Preparation #####

weighting_s <- function(data, taille_agglo='automatic', printWeights = T) { # cf. deprecated/Quotas 2018.xlsx
  d <- data
  d$csp <- factor(d$csp)
  d$region <- factor(d$region)
  levels(d$csp) <- c(levels(d$csp),"missing")
  levels(d$Region) <- c(levels(d$Region),"missing")
  levels(d$taille_agglo) <- c(levels(d$taille_agglo),"missing")
  levels(d$sexe) <- c(levels(d$sexe),"missing")
  d$csp[is.na(d$csp) | d$csp=="" | d$csp=="NSP"] <- "missing"
  d$taille_agglo[is.na(d$taille_agglo)] <- "missing"
  d$sexe[d$sexe=="" | d$sexe=="Autre"] <- "missing"

  unweigthed <- svydesign(ids=~1, data=d)
  sexe <- data.frame(sexe = c("missing","Femme", "Homme"), Freq=nrow(d)*c(0.0001,0.515,0.485)) # http://www.insee.fr/fr/themes/detail.asp?ref_id=bilan-demo&reg_id=0&page=donnees-detaillees/bilan-demo/pop_age2.htm
  csp <- data.frame(csp = c("missing","Inactifs", "Ouvriers", "Cadres", "Indépendants", "Professions intermédiaires", "Retraités", "Employés", "Agriculteurs"),
                    Freq=nrow(d)*c(0.0001,0.124,0.121,0.094,0.034,0.136,0.327,0.153,0.009))
  Region <- data.frame(Region = c("Autre","Auvergne-Rhône-Alpes", "Grand Est et Bourgogne-Franche-Comté", "Hauts-de-France", "Île-de-France", "Bretagne et Normandie", "Nouvelle-Aquitaine", "Occitanie", "Centre-Val de Loire et Pays de la Loire", "Provence-Alpes-Côte d'Azur"), 
                       Freq=nrow(d)*c(0.005,0.124,0.129,0.093,0.189,0.103,0.093,0.091,0.098,0.078))
  age <- data.frame(age = c("","18 à 24 ans", "25 à 34 ans", "35 à 49 ans", "50 à 64 ans", "65 ans ou plus"), 
                    Freq=nrow(d)*c(0.00001,0.104,0.152,0.248,0.246,0.251)) # Données/estim-pop-reg-sexe...
  taille_agglo <- data.frame(taille_agglo = c(1:5), Freq=nrow(d)*c(0.225,0.171,0.136,0.299,0.167))
  # revenu <- data.frame(revenu = c(), Freq=nrow(d)*c())
  diplome4 <- data.frame(diplome4 = c("Aucun diplôme ou brevet", "CAP ou BEP", "Baccalauréat", "Supérieur"),  # http://webcache.googleusercontent.com/search?q=cache:rUvf6u0uCnEJ:www.insee.fr/fr/themes/tableau.asp%3Freg_id%3D0%26ref_id%3Dnattef07232+&cd=1&hl=fr&ct=clnk&gl=fr&lr=lang_en%7Clang_es%7Clang_fr
                        Freq=nrow(d)*c(0.301, 0.246, 0.168, 0.285)) # TODO: raccourcir bac+5

  if (length(which(d$taille_agglo==""))>0) raked <- rake(design= unweigthed, sample.margins = list(~sexe,~diplome4,~Region,~csp,~age),
                population.margins = list(sexe,diplome4,Region,csp,age))    
  else raked <- rake(design= unweigthed, sample.margins = list(~sexe,~diplome4,~taille_agglo,~Region,~csp,~age),
                population.margins = list(sexe,diplome4,taille_agglo,Region,csp,age)) 

  if (printWeights) {    print(summary(weights(raked))  )
    print(sum( weights(raked) )^2/(length(weights(raked))*sum(weights(raked)^2)) ) # <0.5 : problématique   
    print( length(which(weights(raked)<0.25 | weights(raked)>4))/ length(weights(raked)))
    return(weights(trimWeights(raked, lower=0.25, upper=4, strict=TRUE)))
  }
  else { return(weights(trimWeights(raked, lower=0.25, upper=4, strict=TRUE))) }
}

relabel_and_rename_s <- function() {
  # Notation: ~ means that it's a random variant; * means that another question is exactly the same (in another random branch)
  for (i in 1:length(s)) {
    label(s[[i]]) <- paste(names(s)[i], ": ", label(s[[i]]), sep="");
    print(paste(i, label(s[[i]])))
  }
  names(s)[1] <<- "date"
  label(s[[1]]) <<- "date: Date de commencement du sondage"
  names(s)[2] <<- "endDate"
  label(s[[2]]) <<- "endDate: Date de fin"
  names(s)[3] <<- "status"
  label(s[[3]]) <<- "status: 'IP Address'"
  names(s)[4] <<- "ip" # no data
  label(s[[4]]) <<- "ip: Adresse IP"
  names(s)[5] <<- "progress"
  label(s[[5]]) <<- "progress: Progrès"
  names(s)[6] <<- "duree"
  label(s[[6]]) <<- "duree: Durée de complétion du questionnaire (pn secondes)"
  names(s)[7] <<- "finished"
  label(s[[7]]) <<- "finished: Terminé"
  names(s)[8] <<- "recordedDate"
  label(s[[8]]) <<- "recordedDate: Date enregistrée"
  names(s)[9] <<- "X_recordId"
  label(s[[9]]) <<- "X_recordId: ID de réponse"
  names(s)[10] <<- "recipientLastName"
  label(s[[10]]) <<- "recipientLastName: Nom du destinataire"
  names(s)[11] <<- "recipientFirstName"
  label(s[[11]]) <<- "recipientFirstName: Prénom du destinataire"
  names(s)[12] <<- "recipientEmail"
  label(s[[12]]) <<- "recipientEmail: Adresse e-mail du destinataire"
  names(s)[13] <<- "externalDataReference"
  label(s[[13]]) <<- "externalDataReference: Référence externe"
  names(s)[14] <<- "locationLatitude"
  label(s[[14]]) <<- "locationLatitude: LocationLatitude - Latitude de l'emplacement"
  names(s)[15] <<- "locationLongitude"
  label(s[[15]]) <<- "locationLongitude: LocationLongitude - Longitude de l'emplacement"
  names(s)[16] <<- "distributionChannel"
  label(s[[16]]) <<- "distributionChannel: DistributionChannel - Canal de distribution"
  names(s)[17] <<- "langue"
  label(s[[17]]) <<- "langue - Langue du répondant"
  # names(s)[18] <<- "bienvenu"
  # label(s[[18]]) <<- "Q66 - Bienvenu dans l'enquête \"fiscalité des français\". Ce sondage a été réalisé..."
  # names(s)[19] <<- "revenu"
  # label(s[[19]]) <<- "revenu: Revenu mensuel net - Q39"
  # names(s)[20] <<- "taille_foyer"
  # label(s[[20]]) <<- "taille_foyer: Taille de foyer #(vous, membres de votre famille vivant avec vous et personnes à votre charge) - Q41"
  # names(s)[21] <<- "situation_maritale"
  # label(s[[21]]) <<- "situation_maritale: Situation maritale (marié-e/seul-e/en couple) - Q42"
  # names(s)[22] <<- "revenu_conjoint"
  # label(s[[22]]) <<- "revenu_conjoint: Revenu du conjoint (mensuel net) - Q56"
  # names(s)[23] <<- "sexe"
  # label(s[[23]]) <<- "sexe: Sexe - Q125"
  # names(s)[24] <<- "csp"
  # label(s[[24]]) <<- "csp: Catégorie Socio-Professionnelle: Agriculteurs exploitants/Artisans, commerçants et chefs d'entreprise/Cadres et professions intellectuelles supérieures/Professions intermédiaires/Employés/Ouvriers/Retraités/Autres inactifs - Q124"
  # names(s)[25] <<- "age"
  # label(s[[25]]) <<- "age: Âge - Q75"
  # names(s)[26] <<- "diplome"
  # label(s[[26]]) <<- "diplome: Diplôme le plus haut (obtenu ou prévu: Aucun/Brevet/CAP/Bac/+2/+3/>+4/NSP) - Q76"
  # names(s)[27] <<- "region"
  # label(s[[27]]) <<- "region: Dans quelle région vivez-vous ? (nouvelles régions de métropole + outre-mer) - Q122"
  # names(s)[28] <<- "statut_emploi"
  # label(s[[28]]) <<- "statut_emploi: Statut d'emploi (Chômage/CDD/CDI/fonctionnaire/étudiant-e/retraité-e/précaire/autre actif/autre inactif/NSP) - Q43"
  names(s)[18] <<- "premier_clic_no_info"
  label(s[[18]]) <<- "premier_clic_no_info: Premier clic - Ancrage: pas d'information - Q147"
  names(s)[19] <<- "dernier_clic_no_info"
  label(s[[19]]) <<- "dernier_clic_no_info: Dernier clic - Ancrage: pas d'information - Q147"
  names(s)[20] <<- "duree_no_info"
  label(s[[20]]) <<- "duree_no_info: Temps de soumission - Ancrage: pas d'information - Q147"
  names(s)[21] <<- "nombre_clics_no_info"
  label(s[[21]]) <<- "nombre_clics_no_info: Nombre de clics - Ancrage: pas d'information - Q147"
  names(s)[22] <<- "premier_clic_info_PM"
  label(s[[22]]) <<- "premier_clic_info_PM: Premier clic - Ancrage: information sur les particules fines - Q144"
  names(s)[23] <<- "dernier_clic_info_PM"
  label(s[[23]]) <<- "dernier_clic_info_PM: Dernier clic - Ancrage: information sur les particules fines - Q144"
  names(s)[24] <<- "duree_info_PM"
  label(s[[24]]) <<- "duree_info_PM: Temps de soumission - Ancrage: information sur les particules fines - Q144"
  names(s)[25] <<- "nombre_clics_info_PM"
  label(s[[25]]) <<- "nombre_clics_info_PM: Nombre de clics - Ancrage: information sur les particules fines - Q144"
  names(s)[26] <<- "premier_clic_info_PM"
  label(s[[26]]) <<- "premier_clic_info_CC: Premier clic - Ancrage: information sur le changement climatique - Q145"
  names(s)[27] <<- "dernier_clic_info_CC"
  label(s[[27]]) <<- "dernier_clic_info_CC: Dernier clic - Ancrage: information sur le changement climatique - Q145"
  names(s)[28] <<- "duree_info_CC"
  label(s[[28]]) <<- "duree_info_CC: Temps de soumission - Ancrage: information sur le changement climatique - Q145"
  names(s)[29] <<- "nombre_clics_info_CC"
  label(s[[29]]) <<- "nombre_clics_info_CC_: Nombre de clics - Ancrage: information sur le changement climatique - Q145"
  names(s)[30] <<- "premier_clic_info_CC_PM"
  label(s[[30]]) <<- "premier_clic_info_CC_PM: Premier clic - Ancrage: informations sur les particules fines et sur le changement climatique - Q146"
  names(s)[31] <<- "dernier_clic_info_CC_PM"
  label(s[[31]]) <<- "dernier_clic_info_CC_PM: Dernier clic - Ancrage: informations sur les particules fines et sur le changement climatique - Q146"
  names(s)[32] <<- "duree_info_CC_PM"
  label(s[[32]]) <<- "duree_info_CC_PM: Temps de soumission - Ancrage: informations sur les particules fines et sur le changement climatique - Q146"
  names(s)[33] <<- "nombre_clics_info_CC_PM"
  label(s[[33]]) <<- "nombre_clics_info_CC_PM: Nombre de clics - Ancrage: informations sur les particules fines et sur le changement climatique - Q146"
  names(s)[34] <<- "code_postal"
  label(s[[34]]) <<- "code_postal: Code Postal - Q93"
  names(s)[35] <<- "sexe"
  label(s[[35]]) <<- "sexe: Sexe (Masculin/Féminin) - Q96"
  names(s)[36] <<- "age"
  label(s[[36]]) <<- "age: Tranche d'âge (18-24/25-34/35-49/50-64/65+) - Q184"
  names(s)[37] <<- "statut_emploi"
  label(s[[37]]) <<- "statut_emploi: Statut d'emploi (Chômage/CDD/CDI/fonctionnaire/étudiant-e/retraité-e/précaire/autre actif/autre inactif) - Q35"
  names(s)[38] <<- "csp"
  label(s[[38]]) <<- "csp: Catégorie Socio-Professionnelle: Agriculteur/rice /Artisan, commerçant.e/Profession libérale, cadre/Professions intermédiaire/Employé.e/Ouvrier/ère/Retraité.e/Autres inactif/ve - Q98"
  names(s)[39] <<- "diplome"
  label(s[[39]]) <<- "diplome: Diplôme le plus haut obtenu ou prévu: Aucun/Brevet/CAP/Bac/+2/+3/>+4) - Q102"
  names(s)[40] <<- "taille_menage"
  label(s[[40]]) <<- "taille_menage: Taille du ménage #(vous, membres de votre famille vivant avec vous et personnes à votre charge) - Q29"
  names(s)[41] <<- "revenu"
  label(s[[41]]) <<- "revenu: Revenu mensuel net du répondant - Q148"
  names(s)[42] <<- "rev_tot"
  label(s[[42]]) <<- "rev_tot: Revenu mensuel net du ménage - Q25"
  names(s)[43] <<- "nb_14_et_plus"
  label(s[[43]]) <<- "nb_14_et_plus: Nombre de personnes âgées d'au moins 14 ans dans le ménage - Q31"
  names(s)[44] <<- "nb_adultes"
  label(s[[44]]) <<- "nb_adultes: Nombre de personnes majeures dans le ménage - Q149"
  names(s)[45] <<- "surface"
  label(s[[45]]) <<- "surface: Surface du logement (en m²) - Q175"
  names(s)[46] <<- "mode_chauffage"
  label(s[[46]]) <<- "mode_chauffage: Mode de chauffage du logement (individuel/collectif/NSP) - Q178"
  names(s)[47] <<- "chauffage"
  label(s[[47]]) <<- "chauffage: source d'énergie princiale (Électricité/Gaz de ville/Butane, propane, gaz en citerne/Fioul, mazout, pétrole/Bois, solaire, géothermie, aérothermie (pompe à chaleur)/Autre/NSP)"
  names(s)[48] <<- "nb_vehicules_texte"
  label(s[[48]]) <<- "nb_vehicules_texte: Nombre de véhicules motorisés dont dispose le ménage - Q37"
  names(s)[49] <<- "km_0"
  label(s[[49]]) <<- "km_0: (nb_vehicules=0) Nombre de kilomètres parcourus en voiture ou moto par le répondant lors des 12 derniers mois - Q142"
  names(s)[50] <<- "fuel_1"
  label(s[[50]]) <<- "fuel_1: (nb_vehicules=1) Carburant du véhicule (Essence/Diesel/Électrique ou hybride/Autre) - Q77"
  names(s)[51] <<- "conso_1"
  label(s[[51]]) <<- "conso_1: (nb_vehicules=1) Consommation moyenne du véhicule (en litres aux 100 km) - Q174"
  names(s)[52] <<- "km_1"
  label(s[[52]]) <<- "km_1: (nb_vehicules=1) Nombre de kilomètres parcourus par le véhicule lors des 12 derniers mois - Q38"
  names(s)[53] <<- "fuel_2_1"
  label(s[[53]]) <<- "fuel_2_1: (nb_vehicules=2) Carburant du véhicule principal (Essence/Diesel/Électrique ou hybride/Autre) - Q100"
  names(s)[54] <<- "fuel_2_2"
  label(s[[54]]) <<- "fuel_2_2: (nb_vehicules=2) Carburant du deuxième véhicule (Essence/Diesel/Électrique ou hybride/Autre) - Q101"
  names(s)[55] <<- "conso_2"
  label(s[[55]]) <<- "conso_2: (nb_vehicules=2) Consommation moyenne des véhicules du ménage (en litres aux 100 km) - Q176"
  names(s)[56] <<- "km_2"
  label(s[[56]]) <<- "km_2: (nb_vehicules=2) Nombre de kilomètres parcourus par l'ensemble des véhicules lors des 12 derniers mois - Q141"
  names(s)[57] <<- "perte_tva"
  label(s[[57]]) <<- "perte_tva: Une hausse de la TVA ferait perdre plus à votre ménage que la moyenne (Oui, beaucoup/un peu plus/Autant que la moyenne/Non, un peu/beaucoup moins/NSP) - Q150"
  names(s)[58] <<- "perte_fuel"
  label(s[[58]]) <<- "perte_fuel: ~ Une hausse des taxes sur les carburants ferait perdre plus à votre ménage que la moyenne (Oui, beaucoup/un peu plus/Autant que la moyenne/Non, un peu/beaucoup moins/NSP) - Q151"
  names(s)[59] <<- "gain_taxe_fuel"
  label(s[[59]]) <<- "gain_taxe_fuel: ~ Ménage Gagnant/Non affecté/Perdant par hausse taxe carburants redistribuée à tous (+0.11/13 €/L diesel/essence, +60€/an /adulte) - Q152"
  names(s)[60] <<- "gain_taxe_fuel_hausse"
  label(s[[60]]) <<- "gain_taxe_fuel_hausse: ~ (gain_taxe_fuel=Gagnant) Hausse de pouvoir d'achat du ménage suite à hausse taxe carburants redistribuée à tous (seuils à 10/20/30/40 €/an /UC) - Q153"
  names(s)[61] <<- "gain_taxe_fuel_baisse"
  label(s[[61]]) <<- "gain_taxe_fuel_baisse: ~ (gain_taxe_fuel=Perdant) Baisse de pouvoir d'achat du ménage suite à hausse taxe carburants redistribuée à tous (seuils à 15/40/70/110/160 €/an /UC) - Q154"
  names(s)[62] <<- "elasticite_fuel_perso"
  label(s[[62]]) <<- "elasticite_fuel_perso: ~ Réduction de la conso de carburants du ménage suite à augmentation du prix de 0.5€/L (0% - Je n'en consomme déjà presque pas/0% - Je suis contraint sur tous mes déplacements/de 0% à 10%/de 10% à 20%/de 20% à 30%/+ de 30% - Je changerais largement mes habitudes de déplacement) - Q159"
  names(s)[63] <<- "elasticite_fuel"
  label(s[[63]]) <<- "elasticite_fuel: ~ Réduction moyenne de la conso de carburants des Français suite à augmentation du prix de 0.5€/L (de 0% à 3%/de 3% à 10%/de 10% à 20%/de 20% à 30%/+ de 30%) - Q162"
  names(s)[64] <<- "perte_chauffage"
  label(s[[64]]) <<- "perte_chauffage: ~ Une hausse des taxes sur le fioul et le gaz ferait perdre plus à votre ménage que la moyenne (Oui, beaucoup/un peu plus/Autant que la moyenne/Non, un peu/beaucoup moins/NSP) - Q155"
  names(s)[65] <<- "gain_taxe_chauffage"
  label(s[[65]]) <<- "gain_taxe_chauffage: ~ Ménage Gagnant/Non affecté/Perdant par hausse taxe fioul et gaz redistribuée à tous (+13/15% gaz/fioul, +50€/an /adulte) - Q156"
  names(s)[66] <<- "gain_taxe_chauffage_hausse"
  label(s[[66]]) <<- "gain_taxe_chauffage_hausse: ~ (gain_taxe_chauffage=Gagnant) Hausse de pouvoir d'achat du ménage suite à hausse taxe fioul et gaz redistribuée à tous (seuils à 10/20/30/40 €/an /UC) - Q157"
  names(s)[67] <<- "gain_taxe_chauffage_baisse"
  label(s[[67]]) <<- "gain_taxe_chauffage_baisse: ~ (gain_taxe_chauffage=Perdant) Baisse de pouvoir d'achat du ménage suite à hausse taxe fioul et gaz redistribuée à tous (seuils à 15/40/70/110/160 €/an /UC) - Q158"
  names(s)[68] <<- "elasticite_chauffage_perso"
  label(s[[68]]) <<- "elasticite_chauffage_perso: ~ Réduction de la conso de fioul et gaz du ménage suite à augmentation du prix de 30% (0% - Je n'en consomme déjà presque pas/0% - Je suis contraint sur tous mes déplacements/de 0% à 10%/de 10% à 20%/de 20% à 30%/+ de 30% - Je changerais largement mes habitudes de déplacement) - Q160"
  names(s)[69] <<- "elasticite_chauffage"
  label(s[[69]]) <<- "elasticite_chauffage: ~ Réduction moyenne de la conso de fioul et gaz des Français suite à augmentation du prix de 30% (de 0% à 3%/de 3% à 10%/de 10% à 20%/de 20% à 30%/+ de 30%) - Q163"
  names(s)[70] <<- "gain_taxe"
  label(s[[70]]) <<- "gain_taxe: Ménage Gagnant/Non affecté/Perdant par hausse taxe carbone redistribuée à tous (+110€/an /adulte, +13/15% gaz/fioul, +0.11/13 €/L diesel/essence) - Q164"
  names(s)[71] <<- "gain_taxe_hausse"
  label(s[[71]]) <<- "gain_taxe_hausse: ~ (gain_taxe=Gagnant) Hausse de pouvoir d'achat du ménage suite à hausse taxe carbone redistribuée à tous (seuils à 20/40/60/80 €/an /UC) - Q165"
  names(s)[72] <<- "gain_taxe_chauffage_baisse"
  label(s[[72]]) <<- "gain_taxe_baisse: ~ (gain_taxe=Perdant) Baisse de pouvoir d'achat du ménage suite à hausse taxe carbone redistribuée à tous (seuils à 30/70/120/190/280 €/an /UC) - Q166"
  names(s)[73] <<- "taxe_efficace"
  label(s[[73]]) <<- "taxe_efficace: Une hausse de taxe carbone compensée permettrait de réduire la pollution et de lutter contre le changement climatique (Oui/Non/NSP) - Q10"
  names(s)[74] <<- "taxe_perdant_personne"
  label(s[[74]]) <<- "taxe_perdant_personne: Personne - Catégories perdantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[75] <<- "taxe_perdant_pauvres"
  label(s[[75]]) <<- "taxe_perdant_pauvres: Les plus pauvres - Catégories perdantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[76] <<- "taxe_perdant_moyennes"
  label(s[[76]]) <<- "taxe_perdant_moyennes: Les classes moyennes - Catégories perdantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[77] <<- "taxe_perdant_riches"
  label(s[[77]]) <<- "taxe_perdant_riches: Les plus riches - Catégories perdantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[78] <<- "taxe_perdant_tous"
  label(s[[78]]) <<- "taxe_perdant_tous: Tous les Français - Catégories perdantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[79] <<- "taxe_perdant_ruraux"
  label(s[[79]]) <<- "taxe_perdant_ruraux: Les ruraux ou péri-urbains - Catégories perdantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[80] <<- "taxe_perdant_certains"
  label(s[[80]]) <<- "taxe_perdant_certains: Certains Français, mais pas une catégorie de revenus particulière - Catégories perdantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[81] <<- "taxe_perdant_NSP"
  label(s[[81]]) <<- "taxe_perdant_NSP: NSP (Ne sais pas, ne se prononce pas) - Catégories perdantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[82] <<- "taxe_gagnant_personne"
  label(s[[82]]) <<- "taxe_gagnant_personne: Personne - Catégories gagnantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[83] <<- "taxe_gagnant_pauvres"
  label(s[[83]]) <<- "taxe_gagnant_pauvres: Les plus pauvres - Catégories gagnantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[84] <<- "taxe_gagnant_moyennes"
  label(s[[84]]) <<- "taxe_gagnant_moyennes: Les classes moyennes - Catégories gagnantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[85] <<- "taxe_gagnant_riches"
  label(s[[85]]) <<- "taxe_gagnant_riches: Les plus riches - Catégories gagnantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[86] <<- "taxe_gagnant_tous"
  label(s[[86]]) <<- "taxe_gagnant_tous: Tous les Français - Catégories gagnantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[87] <<- "taxe_gagnant_citadins"
  label(s[[87]]) <<- "taxe_gagnant_citadins: Les citadins - Catégories gagnantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[88] <<- "taxe_gagnant_certains"
  label(s[[88]]) <<- "taxe_gagnant_certains: Certains Français, mais pas une catégorie de revenus particulière - Catégories gagnantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[89] <<- "taxe_gagnant_NSP"
  label(s[[89]]) <<- "taxe_gagnant_NSP: NSP (Ne sais pas, ne se prononce pas) - Catégories gagnantes en pouvoir d'achat suite à hausse de taxe carbone compensée - Q190"
  names(s)[90] <<- "taxe_approbation"
  label(s[[90]]) <<- "taxe_approbation: Approbation d'une hausse de la taxe carbone compensée (+110€/an /adulte, +13/15% gaz/fioul, +0.11/13 €/L diesel/essence) - Q15"
  names(s)[91] <<- "gain_taxe_feedback"
  label(s[[91]]) <<- "gain_taxe_feedback: ~ info si le ménage est gagnant/perdant - Ménage Gagnant/Non affecté/Perdant par hausse taxe carbone redistribuée à tous (+110€/an /adulte, +13/15% gaz/fioul, +0.11/13 €/L diesel/essence) - Q63"
  names(s)[92] <<- "taxe_feedback_approbation"
  label(s[[92]]) <<- "taxe_feedback_approbation: ~ info si le ménage est gagnant/perdant - Approbation d'une hausse de la taxe carbone compensée (+110€/an /adulte, +13/15% gaz/fioul, +0.11/13 €/L diesel/essence) - Q64"
  names(s)[93] <<- "taxe_benefices_CC_f"
  label(s[[93]]) <<- "taxe_benefices_CC_f: * Participe à la lutte contre le changement climatique - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[94] <<- "taxe_benefices_sante_f"
  label(s[[94]]) <<- "taxe_benefices_sante_f: * Réduit les effets néfastes de la pollution sur la santé - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[95] <<- "taxe_benefices_circulation_f"
  label(s[[95]]) <<- "taxe_benefices_circulation_f: * Réduit les embouteillages - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[96] <<- "taxe_benefices_revenu_f"
  label(s[[96]]) <<- "taxe_benefices_revenu_f: * Augmente mon pouvoir d'achat - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[97] <<- "taxe_benefices_pauvres_f"
  label(s[[97]]) <<- "taxe_benefices_pauvres_f: * Augmente le pouvoir d'achat des plus modestes - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[98] <<- "taxe_benefices_independance_f"
  label(s[[98]]) <<- "taxe_benefices_independance_f: * Favorise l'indépendance de la France aux importations d'énergie fossile - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[99] <<- "taxe_benefices_enjeu_f"
  label(s[[99]]) <<- "taxe_benefices_enjeux_f: * Prépare l'économie aux enjeux de demain - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[100] <<- "taxe_benefices_aucun_f"
  label(s[[100]]) <<- "taxe_benefices_aucun_f: * Pour aucune de ces raisons - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[101] <<- "taxe_benefices_autre_choix_f"
  label(s[[101]]) <<- "taxe_benefices_autre_choix_f: * Autre (préciser) - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[102] <<- "taxe_benefices_choix_f"
  label(s[[102]]) <<- "taxe_benefices_choix_f: * Champ libre - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[103] <<- "taxe_problemes_inefficace_f"
  label(s[[103]]) <<- "taxe_problemes_inefficace_f: * Est inefficace pour réduire la pollution - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[104] <<- "taxe_problemes_alternatives_f"
  label(s[[104]]) <<- "taxe_problemes_alternatives_f: * Les alternatives sont insuffisantes ou trop chères - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[105] <<- "taxe_problemes_ruraux_f"
  label(s[[105]]) <<- "taxe_problemes_ruraux_f: * Pénalise les milieux ruraux - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[106] <<- "taxe_problemes_revenu_f"
  label(s[[106]]) <<- "taxe_problemes_revenu_f: * Diminue mon pouvoir d'achat - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[107] <<- "taxe_problemes_pauvres_f"
  label(s[[107]]) <<- "taxe_problemes_pauvres_f: * Diminue le pouvoir d'achat de certains ménages modestes - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[108] <<- "taxe_problemes_economie_f"
  label(s[[108]]) <<- "taxe_problemes_economie_f: * Nuit à l'économie et à l'emploi - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[109] <<- "taxe_problemes_pretexte_f"
  label(s[[109]]) <<- "taxe_problemes_pretexte_f: * Est un prétexte pour augmenter les impôts - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[110] <<- "taxe_problemes_aucun_f"
  label(s[[110]]) <<- "taxe_problemes_aucun_f: * Pour aucune de ces raisons - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[111] <<- "taxe_problemes_autre_choix_f"
  label(s[[111]]) <<- "taxe_problemes_autre_choix_f: * Autre (préciser) - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[112] <<- "taxe_problemes_autre_f"
  label(s[[112]]) <<- "taxe_problemes_autre_f: * Champ libre - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[113] <<- "gain_taxe_progressif"
  label(s[[113]]) <<- "gain_taxe_progressif: ~ info que taxe est progressive - Ménage Gagnant/Non affecté/Perdant par hausse taxe carbone redistribuée à tous (+110€/an /adulte, +13/15% gaz/fioul, +0.11/13 €/L diesel/essence) - Q185"
  names(s)[114] <<- "taxe_progressif_approbation"
  label(s[[114]]) <<- "taxe_progressif_approbation: ~ info que taxe est progressive - Approbation d'une hausse de la taxe carbone compensée (+110€/an /adulte, +13/15% gaz/fioul, +0.11/13 €/L diesel/essence) - Q186"
  names(s)[115] <<- "taxe_benefices_CC_p"
  label(s[[115]]) <<- "taxe_benefices_CC_p: * Participe à la lutte contre le changement climatique - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[116] <<- "taxe_benefices_sante_p"
  label(s[[116]]) <<- "taxe_benefices_sante_p: * Réduit les effets néfastes de la pollution sur la santé - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[117] <<- "taxe_benefices_circulation_p"
  label(s[[117]]) <<- "taxe_benefices_circulation_p: * Réduit les embouteillages - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[118] <<- "taxe_benefices_revenu_p"
  label(s[[118]]) <<- "taxe_benefices_revenu_p: * Augmente mon pouvoir d'achat - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[119] <<- "taxe_benefices_pauvres_p"
  label(s[[119]]) <<- "taxe_benefices_pauvres_p: * Augmente le pouvoir d'achat des plus modestes - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[120] <<- "taxe_benefices_independance_p"
  label(s[[120]]) <<- "taxe_benefices_independance_p: * Favorise l'indépendance de la France aux importations d'énergie fossile - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[121] <<- "taxe_benefices_enjeu_p"
  label(s[[121]]) <<- "taxe_benefices_enjeux_p: * Prépare l'économie aux enjeux de demain - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[122] <<- "taxe_benefices_aucun_p"
  label(s[[122]]) <<- "taxe_benefices_aucun_p: * Pour aucune de ces raisons - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[123] <<- "taxe_benefices_autre_choix_p"
  label(s[[123]]) <<- "taxe_benefices_autre_choix_p: * Autre (préciser) - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[124] <<- "taxe_benefices_autre_p"
  label(s[[124]]) <<- "taxe_benefices_autre_p: * Champ libre - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[125] <<- "taxe_problemes_inefficace_p"
  label(s[[125]]) <<- "taxe_problemes_inefficace_p: * Est inefficace pour réduire la pollution - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[126] <<- "taxe_problemes_alternatives_p"
  label(s[[126]]) <<- "taxe_problemes_alternatives_p: * Les alternatives sont insuffisantes ou trop chères - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[127] <<- "taxe_problemes_ruraux_p"
  label(s[[127]]) <<- "taxe_problemes_ruraux_p: * Pénalise les milieux ruraux - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[128] <<- "taxe_problemes_revenu_p"
  label(s[[128]]) <<- "taxe_problemes_revenu_p: * Diminue mon pouvoir d'achat - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[129] <<- "taxe_problemes_pauvres_p"
  label(s[[129]]) <<- "taxe_problemes_pauvres_p: * Diminue le pouvoir d'achat de certains ménages modestes - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[130] <<- "taxe_problemes_economie_p"
  label(s[[130]]) <<- "taxe_problemes_economie_p: * Nuit à l'économie et à l'emploi - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[131] <<- "taxe_problemes_pretexte_p"
  label(s[[131]]) <<- "taxe_problemes_pretexte_p: * Est un prétexte pour augmenter les impôts - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[132] <<- "taxe_problemes_aucun_p"
  label(s[[132]]) <<- "taxe_problemes_aucun_p: * Pour aucune de ces raisons - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[133] <<- "taxe_problemes_autre_choix_p"
  label(s[[133]]) <<- "taxe_problemes_autre_choix_p: * Autre (préciser) - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[134] <<- "taxe_problemes_autre_p" # TODO: enlever 'taxe_' de 73 à 134
  label(s[[134]]) <<- "taxe_problemes_autre_p: * Champ libre - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[135] <<- "gain_taxe__20"
  label(s[[135]]) <<- ""
  names(s)[136] <<- "taxe__20_approbation"
  label(s[[136]]) <<- ""
  names(s)[137] <<- "gain_taxe_20_30"
  label(s[[137]]) <<- ""
  names(s)[138] <<- "taxe_20_30_approbation"
  label(s[[138]]) <<- ""
  names(s)[139] <<- "gain_taxe_30_40"
  label(s[[139]]) <<- ""
  names(s)[140] <<- "taxe_30_40_approbation"
  label(s[[140]]) <<- ""
  names(s)[141] <<- "gain_taxe_40_50"
  label(s[[141]]) <<- ""
  names(s)[142] <<- "taxe_40_50_approbation"
  label(s[[142]]) <<- ""
  names(s)[143] <<- "gain_taxe_50_70"
  label(s[[143]]) <<- ""
  names(s)[144] <<- "taxe_50_70_approbation"
  label(s[[144]]) <<- ""
  names(s)[145] <<- "gain_taxe_70_"
  label(s[[145]]) <<- ""
  names(s)[146] <<- "taxe_70__approbation"
  label(s[[146]]) <<- ""
  names(s)[147] <<- "si_pauvres"
  label(s[[147]]) <<- ""
  names(s)[148] <<- "si_compensee"
  label(s[[148]]) <<- ""
  names(s)[149] <<- "si_contraints"
  label(s[[149]]) <<- ""
  names(s)[150] <<- "si_baisse_cotsoc"
  label(s[[150]]) <<- ""
  names(s)[151] <<- "si_baisse_tva"
  label(s[[151]]) <<- ""
  names(s)[152] <<- "si_baisse_deficit"
  label(s[[152]]) <<- ""
  names(s)[153] <<- "si_renovation"
  label(s[[153]]) <<- ""
  names(s)[154] <<- "si_renouvelables"
  label(s[[154]]) <<- ""
  names(s)[155] <<- "si_transports"
  label(s[[155]]) <<- ""
  names(s)[156] <<- "test_qualite"
  label(s[[156]]) <<- ""
  names(s)[157] <<- "taxe_kerosene"
  label(s[[157]]) <<- ""
  names(s)[158] <<- "taxe_viande"
  label(s[[158]]) <<- ""
  names(s)[159] <<- "normes_isolation"
  label(s[[159]]) <<- ""
  names(s)[160] <<- "normes_vehicules"
  label(s[[160]]) <<- ""
  names(s)[161] <<- "controle_technique"
  label(s[[161]]) <<- ""
  names(s)[162] <<- "interdiction_polluants"
  label(s[[162]]) <<- ""
  names(s)[163] <<- "peages_urbains"
  label(s[[163]]) <<- ""
  names(s)[164] <<- "fonds_mondial"
  label(s[[164]]) <<- ""
  names(s)[165] <<- "rattrapage_diesel"
  label(s[[165]]) <<- ""
  names(s)[166] <<- "parle_CC"
  label(s[[166]]) <<- ""
  names(s)[167] <<- "cause_CC"
  label(s[[167]]) <<- ""
  names(s)[168] <<- "ges_co2" # TODO: majuscules (tva aussi)
  label(s[[168]]) <<- ""
  names(s)[169] <<- "ges_ch4"
  label(s[[169]]) <<- ""
  names(s)[170] <<- "ges_o2"
  label(s[[170]]) <<- ""
  names(s)[171] <<- "ges_pm"
  label(s[[171]]) <<- ""
  names(s)[172] <<- "ges_boeuf"
  label(s[[172]]) <<- ""
  names(s)[173] <<- "ges_nucleaire"
  label(s[[173]]) <<- ""
  names(s)[174] <<- "ges_avion"
  label(s[[174]]) <<- ""
  names(s)[175] <<- "effets_CC"
  label(s[[175]]) <<- ""
  names(s)[176] <<- "region_CC"
  label(s[[176]]) <<- ""
  names(s)[177] <<- "generation_CC_1960"
  label(s[[177]]) <<- ""
  names(s)[178] <<- "generation_CC_1990"
  label(s[[178]]) <<- ""
  names(s)[179] <<- "generation_CC_2020"
  label(s[[179]]) <<- ""
  names(s)[180] <<- "generation_CC_2050"
  label(s[[180]]) <<- ""
  names(s)[181] <<- "generation_CC_aucune"
  label(s[[181]]) <<- ""
  names(s)[182] <<- "responsable_CC_chacun"
  label(s[[182]]) <<- ""
  names(s)[183] <<- "responsable_CC_riches"
  label(s[[183]]) <<- ""
  names(s)[184] <<- "responsable_CC_govts"
  label(s[[184]]) <<- ""
  names(s)[185] <<- "responsable_CC_etranger"
  label(s[[185]]) <<- ""
  names(s)[186] <<- "responsable_CC_passe"
  label(s[[186]]) <<- ""
  names(s)[187] <<- "responsable_CC_nature"
  label(s[[187]]) <<- ""
  names(s)[188] <<- "emission_cible"
  label(s[[188]]) <<- ""
  names(s)[189] <<- "enfant_CC"
  label(s[[189]]) <<- ""
  names(s)[190] <<- "enfant_CC_pour_lui"
  label(s[[190]]) <<- ""
  names(s)[191] <<- "enfant_CC_pour_CC"
  label(s[[191]]) <<- ""
  names(s)[192] <<- "changer_si_politiques"
  label(s[[192]]) <<- ""
  names(s)[193] <<- "changer_si_moyens"
  label(s[[193]]) <<- ""
  names(s)[194] <<- "changer_si_tous"
  label(s[[194]]) <<- ""
  names(s)[195] <<- "changer_non_riches"
  label(s[[195]]) <<- ""
  names(s)[196] <<- "changer_non_interet"
  label(s[[196]]) <<- ""
  names(s)[197] <<- "changer_non_negation"
  label(s[[197]]) <<- ""
  names(s)[198] <<- "changer_deja_fait"
  label(s[[198]]) <<- ""
  names(s)[199] <<- "changer_essaie"
  label(s[[199]]) <<- ""
  names(s)[200] <<- "mode_vie_ecolo"
  label(s[[200]]) <<- ""
  names(s)[201] <<- "fume"
  label(s[[201]]) <<- ""
  names(s)[202] <<- "schiste_approbation"
  label(s[[202]]) <<- ""
  names(s)[203] <<- "schiste_avantage"
  label(s[[203]]) <<- ""
  names(s)[204] <<- "schiste_CC"
  label(s[[204]]) <<- ""
  names(s)[205] <<- "transports_distance_choix"
  label(s[[205]]) <<- ""
  names(s)[206] <<- "transports_distance"
  label(s[[206]]) <<- ""
  names(s)[207] <<- "transports_frequence"
  label(s[[207]]) <<- ""
  names(s)[208] <<- "transports_avis"
  label(s[[208]]) <<- ""
  names(s)[209] <<- "transports_travail"
  label(s[[209]]) <<- ""
  names(s)[210] <<- "transports_courses"
  label(s[[210]]) <<- ""
  names(s)[211] <<- "transports_loisirs"
  label(s[[211]]) <<- ""
  names(s)[212] <<- "transports_travail_commun"
  label(s[[212]]) <<- ""
  names(s)[213] <<- "transports_travail_actif"
  label(s[[213]]) <<- ""
  names(s)[214] <<- "interet_politique"
  label(s[[214]]) <<- ""
  names(s)[215] <<- "extr_gauche"
  label(s[[215]]) <<- ""
  names(s)[216] <<- "gauche"
  label(s[[216]]) <<- ""
  names(s)[217] <<- "centre"
  label(s[[217]]) <<- ""
  names(s)[218] <<- "droite"
  label(s[[218]]) <<- ""
  names(s)[219] <<- "extr_droite"
  label(s[[219]]) <<- ""
  names(s)[220] <<- "conservateur"
  label(s[[220]]) <<- ""
  names(s)[221] <<- "liberal"
  label(s[[221]]) <<- ""
  names(s)[222] <<- "humaniste"
  label(s[[222]]) <<- ""
  names(s)[223] <<- "patriote"
  label(s[[223]]) <<- ""
  names(s)[224] <<- "apolitique"
  label(s[[224]]) <<- ""
  names(s)[225] <<- "ecologiste"
  label(s[[225]]) <<- ""
  names(s)[226] <<- "actualite"
  label(s[[226]]) <<- ""
  names(s)[227] <<- "gilets_jaunes_dedans"
  label(s[[227]]) <<- ""
  names(s)[228] <<- "gilets_jaunes_soutien"
  label(s[[228]]) <<- ""
  names(s)[229] <<- "gilets_jaunes_compris"
  label(s[[229]]) <<- ""
  names(s)[230] <<- "gilets_jaunes_oppose"
  label(s[[230]]) <<- ""
  names(s)[231] <<- "gilets_jaunes_NSP"
  label(s[[231]]) <<- ""
  names(s)[232] <<- "transferts_inter_a"
  label(s[[232]]) <<- ""
  names(s)[233] <<- "transferts_inter_a_info"
  label(s[[233]]) <<- ""
  names(s)[234] <<- "premier_clic_depenses"
  label(s[[234]]) <<- ""
  names(s)[235] <<- "dernier_clic_depenses"
  label(s[[235]]) <<- ""
  names(s)[236] <<- "duree_depenses"
  label(s[[236]]) <<- ""
  names(s)[237] <<- "nombre_clics_depenses"
  label(s[[237]]) <<- ""
  names(s)[238] <<- "depenses_confiant"
  label(s[[238]]) <<- ""
  names(s)[239] <<- "compris_depenses"
  label(s[[239]]) <<- ""
  names(s)[240] <<- "premier_clic_champ_libre"
  label(s[[240]]) <<- ""
  names(s)[241] <<- "dernier_clic_champ_libre"
  label(s[[241]]) <<- ""
  names(s)[242] <<- "duree_champ_libre"
  label(s[[242]]) <<- ""
  names(s)[243] <<- "nombre_clics_champ_libre"
  label(s[[243]]) <<- ""
  names(s)[244] <<- "champ_libre"
  label(s[[244]]) <<- ""
  names(s)[245] <<- "depense_totale"
  label(s[[245]]) <<- "depense_totale: Montant de la dépense publique totale souhaitée (en G€ = Mds€) - Q196"
  names(s)[246] <<- "depense_sante"
  label(s[[246]]) <<- "depense_sante: Système de santé - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q196"
  names(s)[247] <<- "depense_retraites"
  label(s[[247]]) <<- "depense_retraites: Retraites - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q196"
  names(s)[248] <<- "depense_protection"
  label(s[[248]]) <<- "depense_protection: Protection sociale (chômage, allocs, APL, RSA...) - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q196"
  names(s)[249] <<- "depense_education"
  label(s[[249]]) <<- "depense_education: Éducation - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q196"
  names(s)[250] <<- "depense_recherche"
  label(s[[250]]) <<- "depense_recherche: Recherche scientifique - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q196"
  names(s)[251] <<- "depense_loisirs"
  label(s[[251]]) <<- "depense_loisirs: Loisirs : culture, médias et sport - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q196"
  names(s)[252] <<- "depense_infrastructures"
  label(s[[252]]) <<- "depense_infrastructures: Infrastructures - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q196"
  names(s)[253] <<- "depense_justice"
  label(s[[253]]) <<- "depense_justice: Justice - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q196"
  names(s)[254] <<- "depense_armee"
  label(s[[254]]) <<- "depense_armee: Défense et armée - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q196"
  names(s)[255] <<- "depense_securite"
  label(s[[255]]) <<- "depense_securite: Sécurité intérieure (police, gendarmerie) - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q196"
  names(s)[256] <<- "depense_aide"
  label(s[[256]]) <<- "depense_aide: Aides aux pays pauvres - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q196"
  names(s)[257] <<- "variation_totale"
  label(s[[257]]) <<- "variation_totale: Variation de la dépense publique totale souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q196"
  names(s)[258] <<- "variation_sante"
  label(s[[258]]) <<- "variation_sante: Système de santé - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q196"
  names(s)[259] <<- "variation_retraites"
  label(s[[259]]) <<- "variation_retraites: Retraites - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q196"
  names(s)[260] <<- "variation_protection"
  label(s[[260]]) <<- "variation_protection: Protection sociale - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q196"
  names(s)[261] <<- "variation_education"
  label(s[[261]]) <<- "variation_education: Éducation - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q196"
  names(s)[262] <<- "variation_recherche"
  label(s[[262]]) <<- "variation_recherche: Recherche scientifique - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q196"
  names(s)[263] <<- "variation_loisirs"
  label(s[[263]]) <<- "variation_loisirs: Loisirs : culture, médias et sport - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q196"
  names(s)[264] <<- "variation_infrastructures"
  label(s[[264]]) <<- "variation_infrastructures: Infrastructures - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q196"
  names(s)[265] <<- "variation_justice"
  label(s[[265]]) <<- "variation_justice: Justice - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q196"
  names(s)[266] <<- "variation_armee"
  label(s[[266]]) <<- "variation_armee: Défense et armée - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q196"
  names(s)[267] <<- "variation_securite"
  label(s[[267]]) <<- "variation_securite: Sécurité intérieure (police, gendarmerie) - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q196"
  names(s)[268] <<- "variation_aide"
  label(s[[268]]) <<- "variation_aide: Aides aux pays pauvres - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q196"
  names(s)[269] <<- "recette_totale"
  label(s[[269]]) <<- "recette_totale: Recette publique totale souhaitée (en G€) - Q196"
  names(s)[270] <<- "variation_recette"
  label(s[[270]]) <<- "variation_recette: Variation de la recette publique totale souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q196"
  names(s)[271] <<- "budget_equilibre"
  label(s[[271]]) <<- "budget_equilibre: Niveau de dépenses publiques requis pour atteindre l'équilibre budgétaire, d'après les dépenses souhaitées (en G€) - Q196"
  names(s)[272] <<- "regle_or"
  label(s[[272]]) <<- "regle_or: Niveau de dépenses publiques requis pour atteindre un déficit de 3% de PIB, d'après les dépenses souhaitées (en G€) - Q196"
  names(s)[273] <<- "id"
  label(s[[273]]) <<- ""
  names(s)[274] <<- "duree2"
  label(s[[274]]) <<- ""
  names(s)[275] <<- "exclu"
  label(s[[275]]) <<- "exclu: Vide si tout est ok (Screened/QuotaMet sinon)"
  names(s)[276] <<- "taille_agglo"
  label(s[[276]]) <<- "taille_agglo: Taille d'agglomération: [1;5]=rural/-20k/20-100k/+100k/Région parisienne - embedded data"
  names(s)[277] <<- "region"
  label(s[[277]]) <<- ""
  names(s)[278] <<- "schiste_traite"
  label(s[[278]]) <<- "schiste_traite: Département du répondant potentiellement concerné par l'exploitation du gaz de schiste - Q197 embedded data"
  names(s)[279] <<- "gaz"
  label(s[[279]]) <<- "gaz: Indicatrice que chauffage = 'Gaz de ville' ou 'Butane, propane, gaz en citerne'"
  names(s)[280] <<- "fioul"
  label(s[[280]]) <<- "fioul: Indicatrice que chauffage = 'Fioul, mazout, pétrole'"
  names(s)[281] <<- "nb_vehicules"
  label(s[[281]]) <<- "nb_vehicules"
  names(s)[282] <<- "hausse_depenses"
  label(s[[282]]) <<- "hausse_depenses: Hausse des dépenses énergétiques simulées pour le ménage, suite à la taxe (élasticité de 0.4/0.2 pour carburants/chauffage)"
  names(s)[283] <<- "gagnant"
  label(s[[283]]) <<- "gagnant: Indicatrice sur la prédiction que le ménage serait gagnant avec la taxe compensée, d'après nos simulations"
  names(s)[284] <<- "hausse_chauffage"
  label(s[[284]]) <<- "hausse_chauffage:  Hausse des dépenses de chauffage simulées pour le ménage, suite à la taxe (élasticité de 0.2)"
  names(s)[285] <<- "hausse_diesel"
  label(s[[285]]) <<- "hausse_diesel: Hausse des dépenses de diesel simulées pour le ménage, suite à la taxe (élasticité de 0.4)"
  names(s)[286] <<- "hausse_essence"
  label(s[[286]]) <<- "hausse_essence: Hausse des dépenses d'essence simulées pour le ménage, suite à la taxe (élasticité de 0.4)"
  names(s)[287] <<- "en_position_0"
  label(s[[287]]) <<- "en_position_0: Catégorie de dépense affichée en position 0 (0: sante; 1: retraites; 2: protection; 3: education; 4: recherche; 5: loisirs; 6: infrastructures; 7: justice; 8: armee; 9: securite; 10: aide)"
  names(s)[288] <<- "en_position_1"
  label(s[[288]]) <<- "en_position_1: Catégorie de dépense affichée en position 1 (0: sante; 1: retraites; 2: protection; 3: education; 4: recherche; 5: loisirs; 6: infrastructures; 7: justice; 8: armee; 9: securite; 10: aide)"
  names(s)[289] <<- "en_position_2"
  label(s[[289]]) <<- "en_position_2: Catégorie de dépense affichée en position 2 (0: sante; 1: retraites; 2: protection; 3: education; 4: recherche; 5: loisirs; 6: infrastructures; 7: justice; 8: armee; 9: securite; 10: aide)"
  names(s)[290] <<- "en_position_3"
  label(s[[290]]) <<- "en_position_3: Catégorie de dépense affichée en position 3 (0: sante; 1: retraites; 2: protection; 3: education; 4: recherche; 5: loisirs; 6: infrastructures; 7: justice; 8: armee; 9: securite; 10: aide)"
  names(s)[291] <<- "en_position_4"
  label(s[[291]]) <<- "en_position_4: Catégorie de dépense affichée en position 4 (0: sante; 1: retraites; 2: protection; 3: education; 4: recherche; 5: loisirs; 6: infrastructures; 7: justice; 8: armee; 9: securite; 10: aide)"
  names(s)[292] <<- "en_position_5"
  label(s[[292]]) <<- "en_position_5: Catégorie de dépense affichée en position 5 (0: sante; 1: retraites; 2: protection; 3: education; 4: recherche; 5: loisirs; 6: infrastructures; 7: justice; 8: armee; 9: securite; 10: aide)"
  names(s)[293] <<- "en_position_6"
  label(s[[293]]) <<- "en_position_6: Catégorie de dépense affichée en position 6 (0: sante; 1: retraites; 2: protection; 3: education; 4: recherche; 5: loisirs; 6: infrastructures; 7: justice; 8: armee; 9: securite; 10: aide)"
  names(s)[294] <<- "en_position_7"
  label(s[[294]]) <<- "en_position_7: Catégorie de dépense affichée en position 7 (0: sante; 1: retraites; 2: protection; 3: education; 4: recherche; 5: loisirs; 6: infrastructures; 7: justice; 8: armee; 9: securite; 10: aide)"
  names(s)[295] <<- "en_position_8"
  label(s[[295]]) <<- "en_position_8: Catégorie de dépense affichée en position 8 (0: sante; 1: retraites; 2: protection; 3: education; 4: recherche; 5: loisirs; 6: infrastructures; 7: justice; 8: armee; 9: securite; 10: aide)"
  names(s)[296] <<- "en_position_9"
  label(s[[296]]) <<- "en_position_9: Catégorie de dépense affichée en position 9 (0: sante; 1: retraites; 2: protection; 3: education; 4: recherche; 5: loisirs; 6: infrastructures; 7: justice; 8: armee; 9: securite; 10: aide)"
  names(s)[297] <<- "en_position_10"
  label(s[[297]]) <<- "en_position_10: Catégorie de dépense affichée en position 10 (0: sante; 1: retraites; 2: protection; 3: education; 4: recherche; 5: loisirs; 6: infrastructures; 7: justice; 8: armee; 9: securite; 10: aide)"
  names(s)[298] <<- "info_CC"
  label(s[[298]]) <<- "info_CC: "
  names(s)[299] <<- "info_PM"
  label(s[[299]]) <<- "info_PM: "
  names(s)[300] <<- "variante_monetaire"
  label(s[[300]]) <<- "variante_monetaire: Indicatrice aléatoire que les questions sur les perdant_/gagnant_ de la taxe portent sur le pouvoir d'achat ou non (0: quels seraient les gagnants-perdant/1: quelles catégories gagneraient-perdraient en pouvoir d'achat)"
  names(s)[301] <<- "cible20"
  label(s[[301]]) <<- "cible20: Indicatrice aléatoire que la réforme ciblée compense les 20% les plus modestes (20/30/40/50)"
  names(s)[302] <<- "cible30"
  label(s[[302]]) <<- "cible30: Indicatrice aléatoire que la réforme ciblée compense les 30% les plus modestes (20/30/40/50)"  
  names(s)[301] <<- "cible40"
  label(s[[301]]) <<- "cible40: Indicatrice aléatoire que la réforme ciblée compense les 40% les plus modestes (20/30/40/50)"
  names(s)[302] <<- "cible50"
  label(s[[302]]) <<- "cible50: Indicatrice aléatoire que la réforme ciblée compense les 50% les plus modestes (20/30/40/50)"
  # names(s)[96] <<- "test_qualite"
  # label(s[[96]]) <<- "test_qualite: Merci de sélectionner 'Un peu' (Pas du tout/Un peu/Beaucoup/Complètement/NSP) - Q129"
  # names(s)[97] <<- "champ_libre"
  # label(s[[97]]) <<- "champ_libre:  Champ libre (Vous êtes libre de laisser toute idée relative à la fiscalité, ainsi que tout commentaire ou critique concernant ce sondage) - Q62"
  # names(s)[98] <<- "id"
  # label(s[[98]]) <<- "id: ID Bilendi"
  # names(s)[99] <<- "duree"
  # label(s[[99]]) <<- "duree: Durée de complétion du questionnaire (en secondes)"
  # names(s)[100] <<- "exclu"
  # label(s[[100]]) <<- "exclu: Vide si tout est ok (Screened/QuotaMet sinon)"
  # names(s)[101] <<- "taille_agglo"
  # label(s[[101]]) <<- "taille_agglo: Taille d'agglomération: [15]=rural/-20k/20-100k/+100k/Région parisienne - embedded data"
  # names(s)[102] <<- "depense_totale"
  # label(s[[102]]) <<- "depense_totale: Montant de la dépense publique totale souhaitée (en G€ = Mds€) - Q116"
  # names(s)[103] <<- "depense_sante"
  # label(s[[103]]) <<- "depense_sante: Système de santé - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116"
  # names(s)[104] <<- "depense_retraites"
  # label(s[[104]]) <<- "depense_retraites: Retraites - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116"
  # names(s)[105] <<- "depense_protection"
  # label(s[[105]]) <<- "depense_protection: Protection sociale (chômage, allocs, APL, RSA...) - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116"
  # names(s)[106] <<- "depense_education"
  # label(s[[106]]) <<- "depense_education: Éducation - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116"
  # names(s)[107] <<- "depense_recherche"
  # label(s[[107]]) <<- "depense_recherche: Recherche scientifique - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116"
  # names(s)[108] <<- "depense_loisirs"
  # label(s[[108]]) <<- "depense_loisirs: Loisirs : culture, médias et sport - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116"
  # names(s)[109] <<- "depense_infrastructures"
  # label(s[[109]]) <<- "depense_infrastructures: Infrastructures - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116"
  # names(s)[110] <<- "depense_justice"
  # label(s[[110]]) <<- "depense_justice: Justice - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116"
  # names(s)[111] <<- "depense_armee"
  # label(s[[111]]) <<- "depense_armee: Défense et armée - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116"
  # names(s)[112] <<- "depense_securite"
  # label(s[[112]]) <<- "depense_securite: Sécurité intérieure (police, gendarmerie) - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116"
  # names(s)[113] <<- "variation_totale"
  # label(s[[113]]) <<- "variation_totale: Variation de la dépense publique totale souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116"
  # names(s)[114] <<- "variation_sante"
  # label(s[[114]]) <<- "variation_sante: Système de santé - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116"
  # names(s)[115] <<- "variation_retraites"
  # label(s[[115]]) <<- "variation_retraites: Retraites - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116"
  # names(s)[116] <<- "variation_protection"
  # label(s[[116]]) <<- "variation_protection: Protection sociale - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116"
  # names(s)[117] <<- "variation_education"
  # label(s[[117]]) <<- "variation_education: Éducation - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116"
  # names(s)[118] <<- "variation_recherche"
  # label(s[[118]]) <<- "variation_recherche: Recherche scientifique - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116"
  # names(s)[119] <<- "variation_loisirs"
  # label(s[[119]]) <<- "variation_loisirs: Loisirs : culture, médias et sport - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116"
  # names(s)[120] <<- "variation_infrastructures"
  # label(s[[120]]) <<- "variation_infrastructures: Infrastructures - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116"
  # names(s)[121] <<- "variation_justice"
  # label(s[[121]]) <<- "variation_justice: Justice - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116"
  # names(s)[122] <<- "variation_armee"
  # label(s[[122]]) <<- "variation_armee: Défense et armée - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116"
  # names(s)[123] <<- "variation_securite"
  # label(s[[123]]) <<- "variation_securite: Sécurité intérieure (police, gendarmerie) - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116"
  # names(s)[124] <<- "recette_totale"
  # label(s[[124]]) <<- "recette_totale: Recette publique totale souhaitée (en G€) - Q116"
  # names(s)[125] <<- "variation_recette"
  # label(s[[125]]) <<- "variation_recette: Variation de la recette publique totale souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116"
  # names(s)[126] <<- "budget_equilibre"
  # label(s[[126]]) <<- "budget_equilibre: Niveau de dépenses publiques requis pour atteindre l'équilibre budgétaire, d'après les dépenses souhaitées (en G€) - Q116"
  # names(s)[127] <<- "regle_or"
  # label(s[[127]]) <<- "regle_or: Niveau de dépenses publiques requis pour atteindre un déficit de 3% de PIB, d'après les dépenses souhaitées (en G€) - Q116"
  # names(s)[128] <<- "variation_aide"
  # label(s[[128]]) <<- "variation_aide: Aides aux pays pauvres - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116"
  # names(s)[129] <<- "depense_aide"
  # label(s[[129]]) <<- "depense_aide: Aides aux pays pauvres - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116"
  # names(s)[130] <<- "transferts_inter_actuel_vu"
  # label(s[[130]]) <<- "transferts_inter_actuel_vu: Affichage de la question transferts_inter_actuel (0 ou rien)"
  # names(s)[131] <<- "transferts_inter_info"
  # label(s[[131]]) <<- "transferts_inter_info: Information sur le montant actuel de l'aide au développement (0,3% du PIB) (0 ou rien)"
  # names(s)[132] <<- "victoire_vu"
  # label(s[[132]]) <<- "victoire_vu: regardé le match - Question pour des champions: Hier vous avez ... - Q130"
  # names(s)[133] <<- "victoire_fete"
  # label(s[[133]]) <<- "victoire_fete: célébré la victoire - Question pour des champions: Hier vous avez ... - Q130"
  # names(s)[134] <<- "victoire_rue"
  # label(s[[134]]) <<- "victoire_rue: célébré la victoire dans la rue ou dans un bar - Question pour des champions: Hier vous avez ... - Q130"
  # names(s)[135] <<- "victoire_entendu"
  # label(s[[135]]) <<- "victoire_entendu: entendu des klaxons - Question pour des champions: Hier vous avez ... - Q130"
  # names(s)[136] <<- "victoire_klaxons"
  # label(s[[136]]) <<- "victoire_klaxons: été dans une voiture qui klaxonnait - Question pour des champions: Hier vous avez ... - Q130"
  # names(s)[137] <<- "victoire_bu_choix"
  # label(s[[137]]) <<- "victoire_bu_choix: bu - Question pour des champions: Hier vous avez ... - Q130"
  # names(s)[138] <<- "victoire_soutenu"
  # label(s[[138]]) <<- "victoire_soutenu: soutenu les bleus - Question pour des champions: Hier vous avez ... - Q130"
  # names(s)[139] <<- "victoire_chant"
  # label(s[[139]]) <<- "victoire_soutenu: chanté la marseillaise - Question pour des champions: Hier vous avez ... - Q130"
  # names(s)[140] <<- "victoire_bonne_ambiance"
  # label(s[[140]]) <<- "victoire_bonne_ambiance: apprécié l'ambiance générale - Question pour des champions: Hier vous avez ... - Q130"
  # names(s)[141] <<- "victoire_bon_moment"
  # label(s[[141]]) <<- "victoire_bon_moment: passé un moment particulièrement bon - Question pour des champions: Hier vous avez ... - Q130"
  # names(s)[142] <<- "victoire_joueur_choix"
  # label(s[[142]]) <<- "victoire_joueur_choix: J'ai un joueur préféré - Q130"
  # names(s)[143] <<- "victoire_bu"
  # label(s[[143]]) <<- "victoire_bu: nombre de verres bus - Question pour des champions: Hier vous avez bu ... - Q130"
  # names(s)[144] <<- "victoire_joueur"
  # label(s[[144]]) <<- "victoire_joueur: J'ai un joueur préféré, c'est ... (champ libre) - Q130"
  # names(s)[145] <<- "revenu2"
  # label(s[[145]]) <<- "revenu2: Revenu mensuel net - Q39"
  
  s <<- s[,c(1,2,7,19:145)]
}

convert_s <- function() {
  lab <- label(s$csp)
  # s$csp <<- factor(s$csp, levels=c(levels(s$csp), "Cadres", "Indépendants", "Ouvriers", 'Inactifs', "Professions intermédiaires", "Retraités", "Employés", "Agriculteurs"))
  s$csp <<-as.character(s$csp)
  s$csp[grepl("cadre",s$csp)] <<- "Cadres"
  s$csp[grepl("Artisan",s$csp)] <<- "Indépendants"
  s$csp[grepl("iaire",s$csp)] <<- "Professions intermédiaires"
  s$csp[grepl("etrait",s$csp)] <<- "Retraités"
  s$csp[grepl("Employ",s$csp)] <<- "Employés"
  s$csp[grepl("Agricul",s$csp)] <<- "Agriculteurs"
  s$csp[grepl("Ouvrier",s$csp)] <<- "Ouvriers"
  s$csp[grepl("Inactif",s$csp)] <<- "Inactifs"
  # label(s$csp) <<- lab
  s$csp <<- as.factor(s$csp)
  
  for (i in 1:length(f)) {
    levels(s[[i]]) <<- c(levels(s[[i]]), "NSP")
    s[[i]][s[[i]] == "NSP (Ne sait pas, ne se prononce pas)"] <<- "NSP"
    s[[i]][s[[i]] == "NSP (Ne sait pas, ne se prononce pas)."] <<- "NSP"
    s[[i]][s[[i]] == "NSP (Ne sait pas, ne veut pas répondre)"] <<- "NSP"
    s[[i]][s[[i]] == "NSP (Ne veut pas répondre)"] <<- "NSP"
  }

  s$variante_transferts_inter <<- "s"
  s$variante_transferts_inter[which(s$transferts_inter_l_choix!="")] <<- "l"
  label(s$variante_transferts_inter) <<- "variante_transferts_inter: Variante dans la saisie de transferts_inter; s/l: simple/libre:  s: curseur 0-20%/NSP,  l: champ de saisie/NSP - Q73,91"
  s$transferts_inter_info <<- n(s$transferts_inter_info)
  s$transferts_inter_info[which(is.missing(s$transferts_inter_info))] <<- TRUE
  s$transferts_inter_l <<- as.numeric(gsub(',', '.', as.vector(s$transferts_inter_l)))
  s$transferts_inter <<- -1
  s$transferts_inter[s$transferts_inter_s!=""] <<- n(s$transferts_inter_s[s$transferts_inter_s!=""])
  s$transferts_inter[!is.na(s$transferts_inter_l)] <<- n(s$transferts_inter_l[!is.na(s$transferts_inter_l)])
  s$transferts_inter <<- as.item(as.numeric(s$transferts_inter), missing.values=-1, annotation="transferts_inter: Transferts internationaux, variantes (simple) avec curseur 0-20% (s) ou champ (Quelle % des revenus des pays riches devrait être transférée aux pays pauvres ?) - Q73,91")
  s$transferts_inter_l_cru <<- as.numeric(gsub(',', '.', as.vector(s$transferts_inter_l_cru)))
  s$transferts_inter_cru <<- -1
  s$transferts_inter_cru[s$transferts_inter_s_cru!=""] <<- n(s$transferts_inter_s_cru[s$transferts_inter_s_cru!=""])
  s$transferts_inter_cru[!is.na(s$transferts_inter_l_cru)] <<- n(s$transferts_inter_l_cru[!is.na(s$transferts_inter_l_cru)])
  s$transferts_inter_cru <<- as.item(as.numeric(s$transferts_inter_cru), missing.values=-1, annotation="transferts_inter_cru: Perception des transferts internationaux désirés, variantes (simple) avec curseur 0-20% (s) ou champ (Quelle est la réponse typique au % des revenus des pays riches devrait être transférée aux pays pauvres ?) - Q73,91")
  s$transferts_inter_actuel <<- n(s$transferts_inter_actuel)
  s$transferts_inter_actuel_vu <<- n(s$transferts_inter_actuel_vu)
  s$transferts_inter_actuel_vu[is.na(s$transferts_inter_actuel_vu)] <<- 1

  s$revenu2 <<- clean_number(s$revenu2, high_numbers='divide')
  s$revenu <<- clean_number(s$revenu, high_numbers='divide')
  s$revenu[s$revenu2!="" & !is.na(s$revenu2)] <<- s$revenu2[s$revenu2!="" & !is.na(s$revenu2)]
  s$revenu_conjoint <<- clean_number(s$revenu_conjoint, high_numbers='divide')
  s$victoire_bu <<- clean_number(s$victoire_bu)
  s$avantager <<- clean_number(s$avantager)
  s$avantager <<- clean_number(s$avantager)
  s$transferts_inter_l <<- clean_number(s$transferts_inter_l)
  s$transferts_inter_l_cru <<- clean_number(s$transferts_inter_l_cru)
  s$smic <<- clean_number(s$smic)
  for (i in c(
     "revenu", "revenu_conjoint","taille_foyer", "duree", "variation_aide", "depense_aide",
      # "transferts_inter_s_cru", "transferts_inter_l_cru", "transferts_inter_actuel",  "transferts_inter_s", "transferts_inter_l", "transferts_inter_cru",
              "avantager", "desavantager", "rdb_leg", "smic", "depense_totale", "depense_sante", "depense_retraites", "depense_protection",
     "depense_education", "depense_recherche", "depense_loisirs", "depense_infrastructures", "depense_justice", "depense_armee", "depense_securite",
     "variation_totale", "variation_sante", "variation_retraites", "variation_protection", "recette_totale", "variation_recette", "budget_equilibre", "regle_or",
     "variation_education", "variation_recherche", "variation_loisirs", "variation_infrastructures", "variation_justice", "variation_armee", "variation_securite"
              )) {
    lab <- label(s[[i]])
    s[[i]] <<- as.numeric(as.vector(s[[i]]))
    label(s[[i]]) <<- lab
  }

  s$choix_impot_tous <<- 'NSP'
  s$choix_impot_tous[s$choix_impot_referendum=='Oui' & s$choix_impot_deliberation=='Oui' & s$choix_impot_sondage=='Oui' & s$choix_impot_repartition=='Oui' & s$choix_impot_satisfaisant=='Non'] <<- 'Oui' #  s$choix_impot_repartition=='Oui' -> 14% Oui  & s$choix_impot_satisfaisant=='Non' -> 28% Oui
  s$choix_impot_tous[s$choix_impot_referendum=='Non' | s$choix_impot_deliberation=='Non' | s$choix_impot_sondage=='Non' | s$choix_impot_repartition=='Non' | s$choix_impot_satisfaisant=='Oui'] <<- 'Non'
  s$choix_impot_tous[s$choix_impot_referendum=='Non' | s$choix_impot_deliberation=='Non' | s$choix_impot_sondage=='Non' | s$choix_impot_satisfaisant=='NSP' | s$choix_impot_repartition=='NSP'] <<- 'NSP'
  label(s$choix_impot_tous) <<- "choix_impot_tous: Vaut Oui ssi le répondant a répondu 'Oui' aux 5 questions choix_impot_, 'NSP' s'iel a répondu 'NSP' à au moins l'une d'entre elles, 'Non' s'iel s'est exprimé sur les 5 questions sans répondre 'Oui' à chaque fois"
  
  for (j in c("vitesse_reduction", "vitesse_maintien", "fusion_irpp_cotsoc_contre", "approbation_seuls", "approbation_triple", "approbation_triple_info",
              "choix_impot_repartition", "choix_impot_satisfaisant", "choix_impot_sondage", "choix_impot_deliberation",
              "choix_impot_referendum", "democratie_delegative", "legislatives_vote", "legislatives_abstention", "choix_impot_tous"
              )) {
    s[j][[1]] <<- as.item(as.character(s[j][[1]]),
                labels = structure(c("","Non","NSP","Oui"), names = c("NA","Non","NSP","Oui")), 
                missing.values = c("","NSP"), annotation=attr(s[j][[1]], "label"))
  }

  s$variante_vitesse <<- "pro_reduction"
  s$variante_vitesse[s$vitesse_maintien!=""] <<- "pro_maintien"
  label(s$variante_vitesse) <<- "variante_vitesse: Variante de la formulation de la question sur la position quant à la réduction de la vitesse maximale autorisée de 90 à 80km/h: 'pour le maintien' (pro_maintien) ou 'pour la réduction' (pro_reduction) '?' - Q99,100"
  s$vitesse <<- -1
  s$vitesse[s$vitesse_reduction=="Oui" | s$vitesse_maintien=="Non"] <<- 80  
  s$vitesse[s$vitesse_reduction=="Non" | s$vitesse_maintien=="Oui"] <<- 90  
  s$vitesse <<- as.item(s$vitesse, labels = structure(c(-1,80,90)), #names = c("Extrême gauche","Gauche","Centre","Droite","Extrême droite")),
                                missing.values = -1, annotation="vitesse: Position quant à la réduction de la vitesse maximale autorisée de 90 à 80km/h (deux variantes dans la formulation) - Q99,100")

  s$compris_approbation <<- as.item(as.character(s$compris_approbation),
                labels = structure(c("","Oui","Non","Bug: Le graphique ne s'est pas affiché"), names = c("NA","Oui","Non","Bug")), annotation=attr(s$compris_approbation, "label"))
  s$compris_depenses <<- as.item(as.character(s$compris_depenses),
                labels = structure(c("","Oui","Non","Bug: le graphique ne s'est pas affiché correctement."), names = c("NA","Oui","Non","Bug")), annotation=attr(s$compris_depenses, "label"))

  for (j in 1:length(f)) { if (grepl("delegation", names(f)[j]) & grepl("choix", names(f)[j])) {
    s[j][[1]] <<- as.item(as.character(s[j][[1]]),
                labels = structure(c("","votez directement","déléguez votre vote à","vous abstenez", "NSP"), names = c("NA","vote","délégation","abstention", "NSP")),
                missing.values = c("","NSP"), annotation=attr(s[j][[1]], "label"))
  }}

  s$variante_vote <<- "pro_vote"
  s$variante_vote[s$legislatives_abstention!=""] <<- "pro_abstention"
  label(s$variante_vote) <<- "variante_vote: Variante de la formulation de la question sur les législatives 2017: 'Avez-vous été voter' (pro_vote) ou 'Vous-êtes vous abstenu' (pro_abstention) '?' - Q90,91"
  s$vote <<- -1
  s$vote[s$legislatives_abstention=="Oui" | s$legislatives_vote=="Non"] <<- FALSE
  s$vote[s$legislatives_abstention=="Non" | s$legislatives_vote=="Oui"] <<- TRUE
  s$vote <<- as.item(s$vote, labels = structure(c(-1,TRUE,FALSE)), #names = c("Extrême gauche","Gauche","Centre","Droite","Extrême droite")), 
                                missing.values = -1, annotation="vote: A été voter au premier tour des élections législatives de 2017 ou s'est abstenu.e (deux variantes dans la formulation) - Q90,91")

  s$variante_diner <<- "neutre"
  s$variante_diner[s$diner_amis!=""] <<- "pro_amis"
  s$variante_diner[s$diner_famille!=""] <<- "pro_famille"
  label(s$variante_diner) <<- "variante_diner: Variante de la formulation de la question sur la préférence entre un dîner entre amis ou en famille: pas d'amorce (neutre) / 'La question a pour but de valider l'hyp que les Fr préfèrent dîner ...' en famille/entre amis - Q92,93,94"
  s$diner <<- "NSP"
  s$diner[s$diner_simple!=""] <<- as.character(s$diner_simple[s$diner_simple!=""])
  s$diner[s$diner_famille!=""] <<- as.character(s$diner_famille[s$diner_famille!=""])
  s$diner[s$diner_amis!=""] <<- as.character(s$diner_amis[s$diner_amis!=""])
  s$diner <<- as.item(s$diner, labels = structure(c("NSP","Entre amis","En famille"), names = c("NSP","Entre amis","En famille")),
                                missing.values = c("NSP"), annotation="diner: Préférence entre un dîner entre amis ou en famille (trois variantes dans la formulation: sans amorce et avec amorce dans un sens ou dans l'autre) - Q92,93,94")

  # TODO: fusion_irpp merge
  s$gauche_droite <<- pmax(-2,pmin(2,-2 * grepl("extrême gauche", s$extr_gauche) - grepl("De gauche", s$gauche) + grepl("De droite", s$droite) + 2 * grepl("extrême droite", s$extr_droite)))
  is.na(s$gauche_droite) <<- (s$gauche_droite == 0) & !grepl("centre", s$centre)
  s$Gauche_droite <<- as.factor(s$gauche_droite)
  s$gauche_droite <<- as.item(as.numeric(as.vector(s$gauche_droite)), labels = structure(c(-2:2),
                          names = c("Extrême gauche","Gauche","Centre","Droite","Extrême droite")), annotation=attr(s$gauche_droite, "label"))
  levels(s$Gauche_droite) <<- c("Extreme-left", "Left", "Center", "Right", "Extreme-right", "Indeterminate")
  s$Gauche_droite[is.na(s$Gauche_droite)] <<- "Indeterminate"

  s$rdb_leg <<- as.item(as.numeric(s$rdb_leg),  missing.values = -1, annotation=attr(s$rdb_leg, "label"))
  s$rdb_leg[s$rdb_leg_choix == "NSP"] <<- -1
  s$rdb[!is.na(s$rdb_leg)] <<- s$rdb_leg[!is.na(s$rdb_leg)]
  s$rdb <<- as.item(as.numeric(s$rdb),  missing.values = -1, annotation="rdb: Revenu de base désiré, variantes gar/ass/rdb/aid: montant minimal garanti tous français/minimal qu'État devrait assurer à chacun-e/du rdb + explication/aides de l'État pour sans revenus; Vague 1, 3: champ de saisie/NSP, Vague 2: champ de saisie/NSP/on devrait pas - Q122,Q23,Q24,Q25,22")
  s$smic[s$smic_choix=='NSP'] <<- -1
  s$smic <<- as.item(as.numeric(s$smic),  missing.values = -1, annotation=attr(s$smic, "label"))
  s$variante_rdb[!is.na(s$rdb_leg)] <<- "leg"
  s$variante_rdb <<- factor(s$variante_rdb, c("gar","ass","rdb", "leg","aid")) # Pour l'ordre d'affichage dans les graphiques # c("aid","rdb","ass","gar")
  label(s$variante_rdb) <<- "variante_rdb: Variante dans la formulation de rdb (revenu de base); gar/ass/rdb/leg/aid: montant minimal garanti tous français/minimal qu'État devrait assurer à chacun-e/du rdb + explication/seul >25 ans touchant que les aides de l'État/aides de l'État pour sans revenus; Vague 1, 3: champ de saisie/NSP, Vague 2: champ de saisie/NSP/on devrait pas - Q122,Q23,Q24,Q25,22"
  s$desavantager <<- as.item(as.numeric(s$desavantager),  missing.values = -1, annotation=attr(s$desavantager, "label"))
  s$desavantager[s$desavantager_choix == "NSP"] <<- -1
  s$avantager <<- as.item(as.numeric(s$avantager),  missing.values = -1, annotation=attr(s$avantager, "label"))
  s$avantager[s$avantager_choix == "NSP"] <<- -1

  s$approbation <<- s$approbation_seuls
  s$approbation[s$approbation_triple != ""] <<- s$approbation_triple[s$approbation_triple != ""]
  s$approbation[s$approbation_triple_info != ""] <<- s$approbation_triple_info[s$approbation_triple_info != ""]
  # s$approbation <<- as.item(as.numeric(s$approbation),  missing.values = c(-1,-2))
  # s$approbation[s$approbation_nivvie == "NSP" | s$approbation_seuls == "NSP" | s$approbation_triple == "NSP"] <<- -1
  # s$Approbation_variante <<- "seuls"
  # s$Approbation_variante[s$approbation_triple != ""] <<- "triple"
  # s$Approbation_variante[s$approbation_triple_info != ""] <<- "triple_info"
  s$Approbation_variante <<- "triple"
  s$Approbation_variante[s$approbation_seuls!=""] <<- "seuls"
  s$approbation_info <<- FALSE
  s$approbation_info[s$approbation_triple_info != ""] <<- TRUE
  label(s$Approbation_variante) <<- "Approbation_variante: Variante sur la réforme de redistribution des revenus : indiv (approbation_mediane, tous les adultes)/triple (contient en plus la distribution avant impôt)/seuls (célibs sans enfant > 25 ans)"
  label(s$approbation_info) <<- "approbation_info: des exemples de l'effet de la réforme sur 7 niveaux de vie ont été affichés: 800>950/1100>1130/1500>1500/1800>1800/3000>2950/4000>3700/5000>4500/20k>16k"

  temp <- label(s$diplome)
  s$diplome <<- factor(s$diplome, c("","Aucun diplôme","Brevet des collèges","CAP ou BEP","Baccalauréat","Bac +2 (BTS, DUT, DEUG, écoles de formation sanitaires et sociales...)","Bac +3 (licence...)","Bac +5 ou plus (master, école d'ingénieur ou de commerce, doctorat, médecine, maîtrise, DEA, DESS...)","NSP (Ne se prononce pas)","NSP","Q76 - Quel est votre plus haut diplôme (ou celui que vous comptez avoir si vous ê...") )
  label(s$diplome) <<- temp
  temp <- label(s$interet_politique)
  s$interet_politique <<- factor(s$interet_politique, c("","Beaucoup","Un peu","Presque pas (ou pas du tout)","NSP","NSP (Je ne veux pas répondre)","Q20 - À quel point êtes-vous intéressé·e par la politique ?"))
  label(s$interet_politique) <<- temp
  temp <- label(s$refugies)  
  s$refugies <<- factor(s$refugies, c("","Il faut accepter tous les réfugiés qui fuient la guerre ou la persécution","Il faut accepter beaucoup plus de réfugiés qu'actuellement","Il faut accepter un quota de réfugiés plus élevé qu'actuellement","Il faut accepter un quota de réfugiés correspondant au nombre actuel de personnes accueillies légalement sur le territoire","Il faut accepter moins de réfugiés qu'actuellement","Il faut refuser à tout réfugié l'entrée sur le territoire","Il faut laisser à chaque commune le choix de fixer son quota de réfugiés","Il faut autoriser chaque français à parrainer un réfugié, le parrain serait responsable de l'intégration et si besoin de l'hébergement et de l'entretien du réfugié","NSP","NSP (Je ne veux pas répondre)","Q111 - Quelle politique faut-il adopter vis-à-vis des réfugiés qui veulent entrer...", "Il faut accepter autant de réfugiés qu'actuellement","Il faut accepter plus de réfugiés plus élevé qu'actuellement","Il faut accepter plus de réfugiés qu'actuellement"))
  s$refugies[s$refugies_info!=""] <<- s$refugies_info[s$refugies_info!=""]
  s$refugies[s$refugies_info_bis!=""] <<- s$refugies_info_bis[s$refugies_info_bis!=""]
  s$refugies[s$refugies=="Il faut accepter plus de réfugiés qu'actuellement" | s$refugies=="Il faut accepter plus de réfugiés plus élevé qu'actuellement"] <<- "Il faut accepter un quota de réfugiés plus élevé qu'actuellement"
  s$refugies[s$refugies=="Il faut accepter autant de réfugiés qu'actuellement"] <<- "Il faut accepter un quota de réfugiés correspondant au nombre actuel de personnes accueillies légalement sur le territoire"
  label(s$refugies) <<- temp
  s$refugies_info_bis[s$refugies_info!=""] <<- s$refugies_info[s$refugies_info!=""]
  s$refugies_info <<- FALSE
  s$refugies_info[s$refugies_info_bis!=""] <<- TRUE
  label(s$refugies_info) <<- "refugies_info: Le répondant a vu l'information sur le nombre et la proportion de demandeurs d'asile accueillis en France lors des cinq dernières années (20k/an soit un quart des demandes acceptées en moyenne)"
  s$Refugies <<- as.character(s$refugies)
  s$Refugies[s$refugies=="Il faut accepter tous les réfugiés qui fuient la guerre ou la persécution"] <<- "Tous"
  s$Refugies[s$refugies=="Il faut accepter beaucoup plus de réfugiés qu'actuellement"] <<- "Beaucoup plus"
  s$Refugies[s$refugies=="Il faut accepter un quota de réfugiés plus élevé qu'actuellement"] <<- "Plus"
  s$Refugies[s$refugies=="Il faut accepter un quota de réfugiés correspondant au nombre actuel de personnes accueillies légalement sur le territoire"] <<- "Autant"
  s$Refugies[s$refugies=="Il faut accepter moins de réfugiés qu'actuellement"] <<- "Moins"
  s$Refugies[s$refugies=="Il faut refuser à tout réfugié l'entrée sur le territoire"] <<- "Aucun"
  s$Refugies[s$refugies=="Il faut laisser à chaque commune le choix de fixer son quota de réfugiés"] <<- "Quota commune"
  s$Refugies[s$refugies=="Il faut autoriser chaque français à parrainer un réfugié, le parrain serait responsable de l'intégration et si besoin de l'hébergement et de l'entretien du réfugié"] <<- "Parrainage"
  s$Refugies <<- factor(s$Refugies, levels = c("", "Tous", "Beaucoup plus", "Plus", "Autant", "Moins",  "Aucun", "Quota commune", "Parrainage", "NSP"))
  label(s$Refugies) <<- label(s$refugies)
  
  s$rev_tot <<- 12 * pmax(s$revenu, s$revenu + s$revenu_conjoint, na.rm=T) # TODO: check bien défini pour tous
  s$revdisp <<- round((12 * s$revenu / s$rev_tot) * (s$rev_tot -  irpp_p(s$rev_tot,grepl("Mari", s$situation_maritale),s$taille_foyer))/12)
  s$niveau_vie <<- ((s$rev_tot -  irpp_p(s$rev_tot,grepl("Mari", s$situation_maritale),s$taille_foyer))/12) / uc(grepl("Mari", s$situation_maritale), s$taille_foyer)
  s$rev_tot <<- s$rev_tot/12
  s$disadvantaged <<- FALSE
  s$disadvantaged[s$revdisp>3000 & !is.na(s$revdisp) & s$Approbation_variante!="seuls"] <<- TRUE
  s$disadvantaged[s$revenu>2800 & s$Approbation_variante=="seuls" & !is.na(s$revenu)] <<- TRUE
  is.na(s$disadvantaged) <<- is.na(s$revenu)
  
  s$Age <<- (s$age == "18 à 24 ans") + 2*(s$age == "25 à 34 ans") + 3.3*(s$age == "35 à 49 ans") + 4.6*(s$age == "50 à 64 ans") + 7*(s$age == "65 ans ou plus")
  s$age <<- as.factor(as.character(s$age))
  s$Diplome <<- (s$diplome == "Brevet des collèges") + 2*(s$diplome=="CAP ou BEP") + 3*(s$diplome=="Baccalauréat") + 4*(s$diplome=="Bac +2 (BTS, DUT, DEUG, écoles de formation sanitaires et sociales...)") + 5*(s$diplome=="Bac +3 (licence...)") + 6*(s$diplome=="Bac +5 ou plus (master, école d'ingénieur ou de commerce, doctorat, médecine, maîtrise, DEA, DESS...)") - (s$diplome=="NSP (Ne se prononce pas)")
  s$diplome4 <<- as.character(s$diplome)
  s$diplome4[s$Diplome<2] <<- "Aucun diplôme ou brevet"
  s$diplome4[s$Diplome>3] <<- "Supérieur"
  s$Region <<- as.character(s$region)
  s$Region[s$region=="Bretagne" | s$region=="Normandie"] <<- "Bretagne et Normandie"
  s$Region[s$region=="Centre-Val de Loire" | s$region=="Pays de la Loire"] <<- "Centre-Val de Loire et Pays de la Loire"
  s$Region[s$region=="Grand Est" | s$region=="Bourgogne-Franche-Comté"] <<- "Grand Est et Bourgogne-Franche-Comté"
  s$Region[s$region=="NSP" | s$region=="Autre (outre-mer)" | s$region=="Corse"] <<- "Autre"
  s$Region <<- as.factor(s$Region)
  s$taille_agglo <<- as.factor(gsub("[[:alpha:] ]", "", s$taille_agglo))
  s <<- s[s$taille_agglo!="%1%",]
  
  for (i in 1:length(f)) {
    if (grepl("victoire_", names(f)[i])) {
      if (names(f)[i]!="victoire_bu" & names(f)[i]!="victoire_joueur") s[i][[1]] <<- as.logical(s[i][[1]]!="") 
      is.na(s[i][[1]]) <<- as.numeric(substr(s$endDate,9,10))<=15
    }
  }
}

prepare_s <- function(exclude_speeder=TRUE, exclude_screened=TRUE, exclude_quotas_full=TRUE, only_finished=TRUE, clean=TRUE, clean_all=FALSE) {
  # setwd("/home/adrien/Google Drive/Economie/Travail/enquete/codes")
  # setwd("C:/Users/a.fabre/Google Drive/Economie/Travail/enquete/codes")
  # pes <<- read.csv("fin.csv", sep=";")
  # s <<- read.delim("politique.tsv", fileEncoding="UTF-16")
  # f_data <- read.delim("fin.tsv", fileEncoding="UTF-16")
  s <- read_csv("survey.csv")
  for (i in 1:length(s)) { label(s[[i]]) <- toString(s[i][[1]][1]) } # Use the first line to create variable names labels then remove it - to run only once
  s <- s[-c(1,2),]
  if (exclude_screened) { s <<- s[s$Q_TerminateFlag=="",] } # remove Screened
  if (exclude_speeder) { s <<- s[n(s$`Duration (in seconds)`) > 540,] } # remove speedest
  # if (exclude_quotas_full) { s <<- s[s[101][[1]] %in% c(1:5),]  } # remove those with a problem for the taille d'agglo
  if (exclude_quotas_full) { s <<- s[s$Q_TerminateFlag=="",]  } # remove those with a problem for the taille d'agglo
  if (only_finished) { s <<- s[Vf("Finished")=="True",] }
  
  relabel_and_rename_s()
  convert_s()
  
  s$sample <<- "a"
  s$sample[s$finished=="True"] <<- "e"
  s$sample[s$finished=="True" & n(s$duree) >= 450] <<- "p"
  s$sample[s$finished=="True" & n(s$duree) >= 450 & s$test_qualite=='Un peu'] <<- "f" # TODO:"q"? excluded because out of quotas
  s$sample[s$finished=="True" & n(s$duree) >= 450 & s$exclu==""] <<- "r"
  s$Sample <<- s$sample
  
  s <<- s[-which(is.element(s$id, s$id[duplicated(s$id)]) & !duplicated(s$id) & is.na(s$revenu)),]
  
  s$weight <<- weighting_s(s)
}

prepare_f(exclude_screened=FALSE)
tp <- merge(merge_f_m('p'), f, all=T)
fp <- f
# prepare_f(exclude_screened=FALSE, exclude_speeder=FALSE)
# te <- merge(merge_f_m('e'), f, all=T)
# fe <- f
# prepare_f(clean=FALSE)
# tc <- merge(merge_f_m('c'), f, all=T)
# fc <- f
prepare_f(exclude_quotas_full = FALSE)
tq <- merge(merge_f_m('q'), f, all=T)
fq <- f
prepare_f()
t <- merge(merge_f_m(), f, all=T)
ff <- f

f <- fq
t <- tq