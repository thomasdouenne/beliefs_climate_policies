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

Ff <- function(QID) { f[QID][[1]] }
Vf <- function(QID) { as.vector(Ff(QID))  }
n <- function(var) { as.numeric(as.vector(var)) }
ff <- function(id) { f[paste("QID", id, sep="")][[1]] }
vf <- function(id) { as.vector(f(id)) }
NSPf <- function(QID) { length(V(QID)[V(QID) == "NSP (Je ne veux pas répondre)"])/length(V(QID)) }
nspf <- function(id) { length(v(id)[v(id) == "NSP (Je ne veux pas répondre)"])/length(v(id)) }
Label <- function(var) {
  if (length(annotation(var))==1) { annotation(var)[1] }
  else { label(var)  }
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

weighting_f <- function(data, taille_agglo='automatic', printWeights = T) { # cf. deprecated/Quotas 2018.xlsx
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

relabel_and_rename_f <- function() {
  for (i in 1:length(f)) {
    label(f[[i]]) <<- paste(names(f)[i], ": ", label(f[[i]]), sep="");
  }
  names(f)[1] <<- "date";
  label(f[[1]]) <<- "date: Date de commencement du sondage";
  names(f)[2] <<- "endDate";
  label(f[[2]]) <<- "endDate: Date de fin";
  names(f)[3] <<- "status";
  label(f[[3]]) <<- "status: 'IP Address'";
  names(f)[4] <<- "ip"; # no data
  label(f[[4]]) <<- "ip: Adresse IP";
  names(f)[5] <<- "progress";
  label(f[[5]]) <<- "progress: Progrès";
  names(f)[6] <<- "duree2";
  label(f[[6]]) <<- "duree2: Durée de complétion du questionnaire (pn secondes)";
  names(f)[7] <<- "finished";
  label(f[[7]]) <<- "finished: Terminé";
  names(f)[8] <<- "recordedDate";
  label(f[[8]]) <<- "recordedDate: Date enregistrée";
  names(f)[9] <<- "X_recordId";
  label(f[[9]]) <<- "X_recordId: ID de réponse";
  names(f)[10] <<- "recipientLastName";
  label(f[[10]]) <<- "recipientLastName: Nom du destinataire";
  names(f)[11] <<- "recipientFirstName";
  label(f[[11]]) <<- "recipientFirstName: Prénom du destinataire";
  names(f)[12] <<- "recipientEmail";
  label(f[[12]]) <<- "recipientEmail: Adresse e-mail du destinataire";
  names(f)[13] <<- "externalDataReference";
  label(f[[13]]) <<- "externalDataReference: Référence externe";
  names(f)[14] <<- "locationLatitude";
  label(f[[14]]) <<- "locationLatitude: LocationLatitude - Latitude de l'emplacement";
  names(f)[15] <<- "locationLongitude";
  label(f[[15]]) <<- "locationLongitude: LocationLongitude - Longitude de l'emplacement";
  names(f)[16] <<- "distributionChannel";
  label(f[[16]]) <<- "distributionChannel: DistributionChannel - Canal de distribution";
  names(f)[17] <<- "langue";
  label(f[[17]]) <<- "langue - Langue du répondant";
  names(f)[18] <<- "bienvenu";
  label(f[[18]]) <<- "Q66 - Bienvenu dans l'enquête \"fiscalité des français\". Ce sondage a été réalisé...";
  names(f)[19] <<- "revenu";
  label(f[[19]]) <<- "revenu: Revenu mensuel net - Q39";
  names(f)[20] <<- "taille_foyer";
  label(f[[20]]) <<- "taille_foyer: Taille de foyer #(vous, membres de votre famille vivant avec vous et personnes à votre charge) - Q41";
  names(f)[21] <<- "situation_maritale";
  label(f[[21]]) <<- "situation_maritale: Situation maritale (marié-e/seul-e/en couple) - Q42";
  names(f)[22] <<- "revenu_conjoint";
  label(f[[22]]) <<- "revenu_conjoint: Revenu du conjoint (mensuel net) - Q56";
  names(f)[23] <<- "sexe";
  label(f[[23]]) <<- "sexe: Sexe - Q125";
  names(f)[24] <<- "csp";
  label(f[[24]]) <<- "csp: Catégorie Socio-Professionnelle: Agriculteurs exploitants/Artisans, commerçants et chefs d'entreprise/Cadres et professions intellectuelles supérieures/Professions intermédiaires/Employés/Ouvriers/Retraités/Autres inactifs - Q124";
  names(f)[25] <<- "age";
  label(f[[25]]) <<- "age: Âge - Q75";
  names(f)[26] <<- "diplome";
  label(f[[26]]) <<- "diplome: Diplôme le plus haut (obtenu ou prévu: Aucun/Brevet/CAP/Bac/+2/+3/>+4/NSP) - Q76";
  names(f)[27] <<- "region";
  label(f[[27]]) <<- "region: Dans quelle région vivez-vous ? (nouvelles régions de métropole + outre-mer) - Q122";
  names(f)[28] <<- "statut_emploi";
  label(f[[28]]) <<- "statut_emploi: Statut d'emploi (Chômage/CDD/CDI/fonctionnaire/étudiant-e/retraité-e/précaire/autre actif/autre inactif/NSP) - Q43";
  names(f)[29] <<- "";
  label(f[[29]]) <<- "";
  names(f)[30] <<- "";
  label(f[[30]]) <<- "";
  names(f)[31] <<- "";
  label(f[[31]]) <<- "";
  names(f)[32] <<- "";
  label(f[[32]]) <<- "";
  names(f)[33] <<- "";
  label(f[[33]]) <<- "";
  names(f)[34] <<- "";
  label(f[[34]]) <<- "";
  names(f)[35] <<- "";
  label(f[[35]]) <<- "";
  names(f)[36] <<- "";
  label(f[[36]]) <<- "";
  names(f)[37] <<- ""; # TODO: check valid number
  label(f[[37]]) <<- "";
  names(f)[38] <<- "";
  label(f[[38]]) <<- "";
  names(f)[39] <<- "";
  label(f[[39]]) <<- "";
  names(f)[40] <<- "";
  label(f[[40]]) <<- "";
  names(f)[41] <<- "";
  label(f[[41]]) <<- "";
  names(f)[42] <<- "";
  label(f[[42]]) <<- "";
  names(f)[43] <<- "";
  label(f[[43]]) <<- "";
  names(f)[44] <<- "";
  label(f[[44]]) <<- "";
  names(f)[45] <<- "";
  label(f[[45]]) <<- "";
  names(f)[46] <<- "";
  label(f[[46]]) <<- "";
  names(f)[47] <<- "";
  label(f[[47]]) <<- "";
  names(f)[48] <<- "";
  label(f[[48]]) <<- "";
  names(f)[49] <<- "";
  label(f[[49]]) <<- "";
  names(f)[50] <<- "";
  label(f[[50]]) <<- "";
  names(f)[51] <<- "";
  label(f[[51]]) <<- "";
  names(f)[52] <<- "";
  label(f[[52]]) <<- "";
  names(f)[53] <<- "";
  label(f[[53]]) <<- "";
  names(f)[54] <<- "";
  label(f[[54]]) <<- "";
  names(f)[55] <<- "";
  label(f[[55]]) <<- "";
  names(f)[56] <<- "";
  label(f[[56]]) <<- "";
  names(f)[57] <<- "";
  label(f[[57]]) <<- "";
  names(f)[58] <<- "";
  label(f[[58]]) <<- "";
  names(f)[59] <<- "";
  label(f[[59]]) <<- "";
  names(f)[60] <<- "";
  label(f[[60]]) <<- "";
  names(f)[61] <<- "";
  label(f[[61]]) <<- "";
  names(f)[62] <<- "";
  label(f[[62]]) <<- "";
  names(f)[63] <<- "";
  label(f[[63]]) <<- "";
  names(f)[64] <<- "";
  label(f[[64]]) <<- "";
  names(f)[65] <<- "";
  label(f[[65]]) <<- "";
  names(f)[66] <<- "";
  label(f[[66]]) <<- "";
  names(f)[67] <<- "";
  label(f[[67]]) <<- "";
  names(f)[68] <<- "";
  label(f[[68]]) <<- "";
  names(f)[69] <<- "";
  label(f[[69]]) <<- "";
  names(f)[70] <<- "";
  label(f[[70]]) <<- "";
  names(f)[71] <<- "";
  label(f[[71]]) <<- "";
  names(f)[72] <<- ""; 
  label(f[[72]]) <<- "";
  names(f)[73] <<- "";
  label(f[[73]]) <<- "";
  names(f)[74] <<- "";
  label(f[[74]]) <<- "";
  names(f)[75] <<- "";
  label(f[[75]]) <<- "";
  names(f)[76] <<- "";
  label(f[[76]]) <<- "";
  names(f)[77] <<- "";
  label(f[[77]]) <<- "";
  names(f)[78] <<- "";
  label(f[[78]]) <<- ""; # TODO: marxiste ou souverainiste?
  names(f)[79] <<- "";
  label(f[[79]]) <<- "";
  names(f)[80] <<- "";
  label(f[[80]]) <<- "";
  names(f)[81] <<- "";
  label(f[[81]]) <<- "";
  names(f)[82] <<- "";
  label(f[[82]]) <<- "";
  names(f)[83] <<- "";
  label(f[[83]]) <<- "";
  names(f)[84] <<- "";
  label(f[[84]]) <<- "";
  names(f)[85] <<- "";
  label(f[[85]]) <<- "";
  names(f)[86] <<- "";
  label(f[[86]]) <<- "";
  names(f)[87] <<- "";
  label(f[[87]]) <<- "";
  names(f)[88] <<- "";
  label(f[[88]]) <<- "";
  names(f)[89] <<- "";
  label(f[[89]]) <<- "";
  names(f)[90] <<- "";
  label(f[[90]]) <<- "";
  names(f)[91] <<- "";
  label(f[[91]]) <<- "";
  names(f)[92] <<- "";
  label(f[[92]]) <<- "";
  names(f)[93] <<- "";
  label(f[[93]]) <<- "";
  names(f)[94] <<- "";
  label(f[[94]]) <<- "";
  names(f)[95] <<- "";
  label(f[[95]]) <<- "";
  names(f)[96] <<- "test_qualite";
  label(f[[96]]) <<- "test_qualite: Merci de sélectionner 'Un peu' (Pas du tout/Un peu/Beaucoup/Complètement/NSP) - Q129";
  names(f)[97] <<- "champ_libre";
  label(f[[97]]) <<- "champ_libre:  Champ libre (Vous êtes libre de laisser toute idée relative à la fiscalité, ainsi que tout commentaire ou critique concernant ce sondage) - Q62";
  names(f)[98] <<- "id";
  label(f[[98]]) <<- "id: ID Bilendi";
  names(f)[99] <<- "duree";
  label(f[[99]]) <<- "duree: Durée de complétion du questionnaire (en secondes)";
  names(f)[100] <<- "exclu";
  label(f[[100]]) <<- "exclu: Vide si tout est ok (Screened/QuotaMet sinon)";
  names(f)[101] <<- "taille_agglo";
  label(f[[101]]) <<- "taille_agglo: Taille d'agglomération: [1;5]=rural/-20k/20-100k/+100k/Région parisienne - embedded data";
  names(f)[102] <<- "depense_totale";
  label(f[[102]]) <<- "depense_totale: Montant de la dépense publique totale souhaitée (en G€ = Mds€) - Q116";
  names(f)[103] <<- "depense_sante";
  label(f[[103]]) <<- "depense_sante: Système de santé - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116";
  names(f)[104] <<- "depense_retraites";
  label(f[[104]]) <<- "depense_retraites: Retraites - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116";
  names(f)[105] <<- "depense_protection";
  label(f[[105]]) <<- "depense_protection: Protection sociale (chômage, allocs, APL, RSA...) - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116";
  names(f)[106] <<- "depense_education";
  label(f[[106]]) <<- "depense_education: Éducation - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116";
  names(f)[107] <<- "depense_recherche";
  label(f[[107]]) <<- "depense_recherche: Recherche scientifique - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116";
  names(f)[108] <<- "depense_loisirs";
  label(f[[108]]) <<- "depense_loisirs: Loisirs : culture, médias et sport - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116";
  names(f)[109] <<- "depense_infrastructures";
  label(f[[109]]) <<- "depense_infrastructures: Infrastructures - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116";
  names(f)[110] <<- "depense_justice";
  label(f[[110]]) <<- "depense_justice: Justice - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116";
  names(f)[111] <<- "depense_armee";
  label(f[[111]]) <<- "depense_armee: Défense et armée - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116";
  names(f)[112] <<- "depense_securite";
  label(f[[112]]) <<- "depense_securite: Sécurité intérieure (police, gendarmerie) - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116";
  names(f)[113] <<- "variation_totale";
  label(f[[113]]) <<- "variation_totale: Variation de la dépense publique totale souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116";
  names(f)[114] <<- "variation_sante";
  label(f[[114]]) <<- "variation_sante: Système de santé - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116";
  names(f)[115] <<- "variation_retraites";
  label(f[[115]]) <<- "variation_retraites: Retraites - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116";
  names(f)[116] <<- "variation_protection";
  label(f[[116]]) <<- "variation_protection: Protection sociale - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116";
  names(f)[117] <<- "variation_education";
  label(f[[117]]) <<- "variation_education: Éducation - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116";
  names(f)[118] <<- "variation_recherche";
  label(f[[118]]) <<- "variation_recherche: Recherche scientifique - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116";
  names(f)[119] <<- "variation_loisirs";
  label(f[[119]]) <<- "variation_loisirs: Loisirs : culture, médias et sport - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116";
  names(f)[120] <<- "variation_infrastructures";
  label(f[[120]]) <<- "variation_infrastructures: Infrastructures - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116";
  names(f)[121] <<- "variation_justice";
  label(f[[121]]) <<- "variation_justice: Justice - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116";
  names(f)[122] <<- "variation_armee";
  label(f[[122]]) <<- "variation_armee: Défense et armée - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116";
  names(f)[123] <<- "variation_securite";
  label(f[[123]]) <<- "variation_securite: Sécurité intérieure (police, gendarmerie) - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116";
  names(f)[124] <<- "recette_totale";
  label(f[[124]]) <<- "recette_totale: Recette publique totale souhaitée (en G€) - Q116";
  names(f)[125] <<- "variation_recette";
  label(f[[125]]) <<- "variation_recette: Variation de la recette publique totale souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116";
  names(f)[126] <<- "budget_equilibre";
  label(f[[126]]) <<- "budget_equilibre: Niveau de dépenses publiques requis pour atteindre l'équilibre budgétaire, d'après les dépenses souhaitées (en G€) - Q116";
  names(f)[127] <<- "regle_or";
  label(f[[127]]) <<- "regle_or: Niveau de dépenses publiques requis pour atteindre un déficit de 3% de PIB, d'après les dépenses souhaitées (en G€) - Q116";
  names(f)[128] <<- "variation_aide";
  label(f[[128]]) <<- "variation_aide: Aides aux pays pauvres - Variation de la dépense publique souhaitée (en % par rapport à l'actuelle - curseur 0-20%) - Q116";
  names(f)[129] <<- "depense_aide";
  label(f[[129]]) <<- "depense_aide: Aides aux pays pauvres - Montant de la dépense publique souhaitée (en G€ = Mds€) - Q116";
  names(f)[130] <<- "transferts_inter_actuel_vu";
  label(f[[130]]) <<- "transferts_inter_actuel_vu: Affichage de la question transferts_inter_actuel (0 ou rien)";
  names(f)[131] <<- "transferts_inter_info";
  label(f[[131]]) <<- "transferts_inter_info: Information sur le montant actuel de l'aide au développement (0,3% du PIB) (0 ou rien)";
  names(f)[132] <<- "victoire_vu"
  label(f[[132]]) <<- "victoire_vu: regardé le match - Question pour des champions: Hier vous avez ... - Q130"
  names(f)[133] <<- "victoire_fete"
  label(f[[133]]) <<- "victoire_fete: célébré la victoire - Question pour des champions: Hier vous avez ... - Q130"
  names(f)[134] <<- "victoire_rue"
  label(f[[134]]) <<- "victoire_rue: célébré la victoire dans la rue ou dans un bar - Question pour des champions: Hier vous avez ... - Q130"
  names(f)[135] <<- "victoire_entendu"
  label(f[[135]]) <<- "victoire_entendu: entendu des klaxons - Question pour des champions: Hier vous avez ... - Q130"
  names(f)[136] <<- "victoire_klaxons"
  label(f[[136]]) <<- "victoire_klaxons: été dans une voiture qui klaxonnait - Question pour des champions: Hier vous avez ... - Q130"
  names(f)[137] <<- "victoire_bu_choix"
  label(f[[137]]) <<- "victoire_bu_choix: bu - Question pour des champions: Hier vous avez ... - Q130"
  names(f)[138] <<- "victoire_soutenu"
  label(f[[138]]) <<- "victoire_soutenu: soutenu les bleus - Question pour des champions: Hier vous avez ... - Q130"
  names(f)[139] <<- "victoire_chant"
  label(f[[139]]) <<- "victoire_soutenu: chanté la marseillaise - Question pour des champions: Hier vous avez ... - Q130"
  names(f)[140] <<- "victoire_bonne_ambiance"
  label(f[[140]]) <<- "victoire_bonne_ambiance: apprécié l'ambiance générale - Question pour des champions: Hier vous avez ... - Q130"
  names(f)[141] <<- "victoire_bon_moment"
  label(f[[141]]) <<- "victoire_bon_moment: passé un moment particulièrement bon - Question pour des champions: Hier vous avez ... - Q130"
  names(f)[142] <<- "victoire_joueur_choix"
  label(f[[142]]) <<- "victoire_joueur_choix: J'ai un joueur préféré - Q130"
  names(f)[143] <<- "victoire_bu"
  label(f[[143]]) <<- "victoire_bu: nombre de verres bus - Question pour des champions: Hier vous avez bu ... - Q130"
  names(f)[144] <<- "victoire_joueur"
  label(f[[144]]) <<- "victoire_joueur: J'ai un joueur préféré, c'est ... (champ libre) - Q130"
  names(f)[145] <<- "revenu2";
  label(f[[145]]) <<- "revenu2: Revenu mensuel net - Q39";
  
  f <<- f[,c(1,2,7,19:145)]
}

convert_f <- function() {
  lab <- label(f$csp)
  # f$csp <<- factor(f$csp, levels=c(levels(f$csp), "Cadres", "Indépendants", "Ouvriers", 'Inactifs', "Professions intermédiaires", "Retraités", "Employés", "Agriculteurs"))
  f$csp <<-as.character(f$csp)
  f$csp[grepl("cadre",f$csp)] <<- "Cadres"
  f$csp[grepl("Artisan",f$csp)] <<- "Indépendants"
  f$csp[grepl("iaire",f$csp)] <<- "Professions intermédiaires"
  f$csp[grepl("etrait",f$csp)] <<- "Retraités"
  f$csp[grepl("Employ",f$csp)] <<- "Employés"
  f$csp[grepl("Agricul",f$csp)] <<- "Agriculteurs"
  f$csp[grepl("Ouvrier",f$csp)] <<- "Ouvriers"
  f$csp[grepl("Inactif",f$csp)] <<- "Inactifs"
  # label(f$csp) <<- lab
  f$csp <<- as.factor(f$csp)
  
  for (i in 1:length(f)) {
    levels(f[[i]]) <<- c(levels(f[[i]]), "NSP")
    f[[i]][f[[i]] == "NSP (Ne sait pas, ne se prononce pas)"] <<- "NSP"
    f[[i]][f[[i]] == "NSP (Ne sait pas, ne se prononce pas)."] <<- "NSP"
    f[[i]][f[[i]] == "NSP (Ne sait pas, ne veut pas répondre)"] <<- "NSP"
    f[[i]][f[[i]] == "NSP (Ne veut pas répondre)"] <<- "NSP"
  }

  f$variante_transferts_inter <<- "s"
  f$variante_transferts_inter[which(f$transferts_inter_l_choix!="")] <<- "l"
  label(f$variante_transferts_inter) <<- "variante_transferts_inter: Variante dans la saisie de transferts_inter; s/l: simple/libre:  s: curseur 0-20%/NSP,  l: champ de saisie/NSP - Q73,91"
  f$transferts_inter_info <<- n(f$transferts_inter_info)
  f$transferts_inter_info[which(is.missing(f$transferts_inter_info))] <<- TRUE
  f$transferts_inter_l <<- as.numeric(gsub(',', '.', as.vector(f$transferts_inter_l)))
  f$transferts_inter <<- -1
  f$transferts_inter[f$transferts_inter_s!=""] <<- n(f$transferts_inter_s[f$transferts_inter_s!=""])
  f$transferts_inter[!is.na(f$transferts_inter_l)] <<- n(f$transferts_inter_l[!is.na(f$transferts_inter_l)])
  f$transferts_inter <<- as.item(as.numeric(f$transferts_inter), missing.values=-1, annotation="transferts_inter: Transferts internationaux, variantes (simple) avec curseur 0-20% (s) ou champ (Quelle % des revenus des pays riches devrait être transférée aux pays pauvres ?) - Q73,91")
  f$transferts_inter_l_cru <<- as.numeric(gsub(',', '.', as.vector(f$transferts_inter_l_cru)))
  f$transferts_inter_cru <<- -1
  f$transferts_inter_cru[f$transferts_inter_s_cru!=""] <<- n(f$transferts_inter_s_cru[f$transferts_inter_s_cru!=""])
  f$transferts_inter_cru[!is.na(f$transferts_inter_l_cru)] <<- n(f$transferts_inter_l_cru[!is.na(f$transferts_inter_l_cru)])
  f$transferts_inter_cru <<- as.item(as.numeric(f$transferts_inter_cru), missing.values=-1, annotation="transferts_inter_cru: Perception des transferts internationaux désirés, variantes (simple) avec curseur 0-20% (s) ou champ (Quelle est la réponse typique au % des revenus des pays riches devrait être transférée aux pays pauvres ?) - Q73,91")
  f$transferts_inter_actuel <<- n(f$transferts_inter_actuel)
  f$transferts_inter_actuel_vu <<- n(f$transferts_inter_actuel_vu)
  f$transferts_inter_actuel_vu[is.na(f$transferts_inter_actuel_vu)] <<- 1

  f$revenu2 <<- clean_number(f$revenu2, high_numbers='divide')
  f$revenu <<- clean_number(f$revenu, high_numbers='divide')
  f$revenu[f$revenu2!="" & !is.na(f$revenu2)] <<- f$revenu2[f$revenu2!="" & !is.na(f$revenu2)]
  f$revenu_conjoint <<- clean_number(f$revenu_conjoint, high_numbers='divide')
  f$victoire_bu <<- clean_number(f$victoire_bu)
  f$avantager <<- clean_number(f$avantager)
  f$avantager <<- clean_number(f$avantager)
  f$transferts_inter_l <<- clean_number(f$transferts_inter_l)
  f$transferts_inter_l_cru <<- clean_number(f$transferts_inter_l_cru)
  f$smic <<- clean_number(f$smic)
  for (i in c(
     "revenu", "revenu_conjoint","taille_foyer", "duree", "variation_aide", "depense_aide",
      # "transferts_inter_s_cru", "transferts_inter_l_cru", "transferts_inter_actuel",  "transferts_inter_s", "transferts_inter_l", "transferts_inter_cru",
              "avantager", "desavantager", "rdb_leg", "smic", "depense_totale", "depense_sante", "depense_retraites", "depense_protection",
     "depense_education", "depense_recherche", "depense_loisirs", "depense_infrastructures", "depense_justice", "depense_armee", "depense_securite",
     "variation_totale", "variation_sante", "variation_retraites", "variation_protection", "recette_totale", "variation_recette", "budget_equilibre", "regle_or",
     "variation_education", "variation_recherche", "variation_loisirs", "variation_infrastructures", "variation_justice", "variation_armee", "variation_securite"
              )) {
    lab <- label(f[[i]])
    f[[i]] <<- as.numeric(as.vector(f[[i]]))
    label(f[[i]]) <<- lab
  }

  f$choix_impot_tous <<- 'NSP'
  f$choix_impot_tous[f$choix_impot_referendum=='Oui' & f$choix_impot_deliberation=='Oui' & f$choix_impot_sondage=='Oui' & f$choix_impot_repartition=='Oui' & f$choix_impot_satisfaisant=='Non'] <<- 'Oui' #  f$choix_impot_repartition=='Oui' -> 14% Oui  & f$choix_impot_satisfaisant=='Non' -> 28% Oui
  f$choix_impot_tous[f$choix_impot_referendum=='Non' | f$choix_impot_deliberation=='Non' | f$choix_impot_sondage=='Non' | f$choix_impot_repartition=='Non' | f$choix_impot_satisfaisant=='Oui'] <<- 'Non'
  f$choix_impot_tous[f$choix_impot_referendum=='Non' | f$choix_impot_deliberation=='Non' | f$choix_impot_sondage=='Non' | f$choix_impot_satisfaisant=='NSP' | f$choix_impot_repartition=='NSP'] <<- 'NSP'
  label(f$choix_impot_tous) <<- "choix_impot_tous: Vaut Oui ssi le répondant a répondu 'Oui' aux 5 questions choix_impot_, 'NSP' s'iel a répondu 'NSP' à au moins l'une d'entre elles, 'Non' s'iel s'est exprimé sur les 5 questions sans répondre 'Oui' à chaque fois"
  
  for (j in c("vitesse_reduction", "vitesse_maintien", "fusion_irpp_cotsoc_contre", "approbation_seuls", "approbation_triple", "approbation_triple_info",
              "choix_impot_repartition", "choix_impot_satisfaisant", "choix_impot_sondage", "choix_impot_deliberation",
              "choix_impot_referendum", "democratie_delegative", "legislatives_vote", "legislatives_abstention", "choix_impot_tous"
              )) {
    f[j][[1]] <<- as.item(as.character(f[j][[1]]),
                labels = structure(c("","Non","NSP","Oui"), names = c("NA","Non","NSP","Oui")), 
                missing.values = c("","NSP"), annotation=attr(f[j][[1]], "label"))
  }

  f$variante_vitesse <<- "pro_reduction"
  f$variante_vitesse[f$vitesse_maintien!=""] <<- "pro_maintien"
  label(f$variante_vitesse) <<- "variante_vitesse: Variante de la formulation de la question sur la position quant à la réduction de la vitesse maximale autorisée de 90 à 80km/h: 'pour le maintien' (pro_maintien) ou 'pour la réduction' (pro_reduction) '?' - Q99,100"
  f$vitesse <<- -1
  f$vitesse[f$vitesse_reduction=="Oui" | f$vitesse_maintien=="Non"] <<- 80  
  f$vitesse[f$vitesse_reduction=="Non" | f$vitesse_maintien=="Oui"] <<- 90  
  f$vitesse <<- as.item(f$vitesse, labels = structure(c(-1,80,90)), #names = c("Extrême gauche","Gauche","Centre","Droite","Extrême droite")),
                                missing.values = -1, annotation="vitesse: Position quant à la réduction de la vitesse maximale autorisée de 90 à 80km/h (deux variantes dans la formulation) - Q99,100")

  f$compris_approbation <<- as.item(as.character(f$compris_approbation),
                labels = structure(c("","Oui","Non","Bug: Le graphique ne s'est pas affiché"), names = c("NA","Oui","Non","Bug")), annotation=attr(f$compris_approbation, "label"))
  f$compris_depenses <<- as.item(as.character(f$compris_depenses),
                labels = structure(c("","Oui","Non","Bug: le graphique ne s'est pas affiché correctement."), names = c("NA","Oui","Non","Bug")), annotation=attr(f$compris_depenses, "label"))

  for (j in 1:length(f)) { if (grepl("delegation", names(f)[j]) & grepl("choix", names(f)[j])) {
    f[j][[1]] <<- as.item(as.character(f[j][[1]]),
                labels = structure(c("","votez directement","déléguez votre vote à","vous abstenez", "NSP"), names = c("NA","vote","délégation","abstention", "NSP")),
                missing.values = c("","NSP"), annotation=attr(f[j][[1]], "label"))
  }}

  f$variante_vote <<- "pro_vote"
  f$variante_vote[f$legislatives_abstention!=""] <<- "pro_abstention"
  label(f$variante_vote) <<- "variante_vote: Variante de la formulation de la question sur les législatives 2017: 'Avez-vous été voter' (pro_vote) ou 'Vous-êtes vous abstenu' (pro_abstention) '?' - Q90,91"
  f$vote <<- -1
  f$vote[f$legislatives_abstention=="Oui" | f$legislatives_vote=="Non"] <<- FALSE
  f$vote[f$legislatives_abstention=="Non" | f$legislatives_vote=="Oui"] <<- TRUE
  f$vote <<- as.item(f$vote, labels = structure(c(-1,TRUE,FALSE)), #names = c("Extrême gauche","Gauche","Centre","Droite","Extrême droite")), 
                                missing.values = -1, annotation="vote: A été voter au premier tour des élections législatives de 2017 ou s'est abstenu.e (deux variantes dans la formulation) - Q90,91")

  f$variante_diner <<- "neutre"
  f$variante_diner[f$diner_amis!=""] <<- "pro_amis"
  f$variante_diner[f$diner_famille!=""] <<- "pro_famille"
  label(f$variante_diner) <<- "variante_diner: Variante de la formulation de la question sur la préférence entre un dîner entre amis ou en famille: pas d'amorce (neutre) / 'La question a pour but de valider l'hyp que les Fr préfèrent dîner ...' en famille/entre amis - Q92,93,94"
  f$diner <<- "NSP"
  f$diner[f$diner_simple!=""] <<- as.character(f$diner_simple[f$diner_simple!=""])
  f$diner[f$diner_famille!=""] <<- as.character(f$diner_famille[f$diner_famille!=""])
  f$diner[f$diner_amis!=""] <<- as.character(f$diner_amis[f$diner_amis!=""])
  f$diner <<- as.item(f$diner, labels = structure(c("NSP","Entre amis","En famille"), names = c("NSP","Entre amis","En famille")),
                                missing.values = c("NSP"), annotation="diner: Préférence entre un dîner entre amis ou en famille (trois variantes dans la formulation: sans amorce et avec amorce dans un sens ou dans l'autre) - Q92,93,94")

  # TODO: fusion_irpp merge
  f$gauche_droite <<- pmax(-2,pmin(2,-2 * grepl("extrême gauche", f$extr_gauche) - grepl("De gauche", f$gauche) + grepl("De droite", f$droite) + 2 * grepl("extrême droite", f$extr_droite)))
  is.na(f$gauche_droite) <<- (f$gauche_droite == 0) & !grepl("centre", f$centre)
  f$Gauche_droite <<- as.factor(f$gauche_droite)
  f$gauche_droite <<- as.item(as.numeric(as.vector(f$gauche_droite)), labels = structure(c(-2:2),
                          names = c("Extrême gauche","Gauche","Centre","Droite","Extrême droite")), annotation=attr(f$gauche_droite, "label"))
  levels(f$Gauche_droite) <<- c("Extreme-left", "Left", "Center", "Right", "Extreme-right", "Indeterminate")
  f$Gauche_droite[is.na(f$Gauche_droite)] <<- "Indeterminate"

  f$rdb_leg <<- as.item(as.numeric(f$rdb_leg),  missing.values = -1, annotation=attr(f$rdb_leg, "label"))
  f$rdb_leg[f$rdb_leg_choix == "NSP"] <<- -1
  f$rdb[!is.na(f$rdb_leg)] <<- f$rdb_leg[!is.na(f$rdb_leg)]
  f$rdb <<- as.item(as.numeric(f$rdb),  missing.values = -1, annotation="rdb: Revenu de base désiré, variantes gar/ass/rdb/aid: montant minimal garanti tous français/minimal qu'État devrait assurer à chacun-e/du rdb + explication/aides de l'État pour sans revenus; Vague 1, 3: champ de saisie/NSP, Vague 2: champ de saisie/NSP/on devrait pas - Q122,Q23,Q24,Q25,22")
  f$smic[f$smic_choix=='NSP'] <<- -1
  f$smic <<- as.item(as.numeric(f$smic),  missing.values = -1, annotation=attr(f$smic, "label"))
  f$variante_rdb[!is.na(f$rdb_leg)] <<- "leg"
  f$variante_rdb <<- factor(f$variante_rdb, c("gar","ass","rdb", "leg","aid")) # Pour l'ordre d'affichage dans les graphiques # c("aid","rdb","ass","gar")
  label(f$variante_rdb) <<- "variante_rdb: Variante dans la formulation de rdb (revenu de base); gar/ass/rdb/leg/aid: montant minimal garanti tous français/minimal qu'État devrait assurer à chacun-e/du rdb + explication/seul >25 ans touchant que les aides de l'État/aides de l'État pour sans revenus; Vague 1, 3: champ de saisie/NSP, Vague 2: champ de saisie/NSP/on devrait pas - Q122,Q23,Q24,Q25,22"
  f$desavantager <<- as.item(as.numeric(f$desavantager),  missing.values = -1, annotation=attr(f$desavantager, "label"))
  f$desavantager[f$desavantager_choix == "NSP"] <<- -1
  f$avantager <<- as.item(as.numeric(f$avantager),  missing.values = -1, annotation=attr(f$avantager, "label"))
  f$avantager[f$avantager_choix == "NSP"] <<- -1

  f$approbation <<- f$approbation_seuls
  f$approbation[f$approbation_triple != ""] <<- f$approbation_triple[f$approbation_triple != ""]
  f$approbation[f$approbation_triple_info != ""] <<- f$approbation_triple_info[f$approbation_triple_info != ""]
  # f$approbation <<- as.item(as.numeric(f$approbation),  missing.values = c(-1,-2))
  # f$approbation[f$approbation_nivvie == "NSP" | f$approbation_seuls == "NSP" | f$approbation_triple == "NSP"] <<- -1
  # f$Approbation_variante <<- "seuls"
  # f$Approbation_variante[f$approbation_triple != ""] <<- "triple"
  # f$Approbation_variante[f$approbation_triple_info != ""] <<- "triple_info"
  f$Approbation_variante <<- "triple"
  f$Approbation_variante[f$approbation_seuls!=""] <<- "seuls"
  f$approbation_info <<- FALSE
  f$approbation_info[f$approbation_triple_info != ""] <<- TRUE
  label(f$Approbation_variante) <<- "Approbation_variante: Variante sur la réforme de redistribution des revenus : indiv (approbation_mediane, tous les adultes)/triple (contient en plus la distribution avant impôt)/seuls (célibs sans enfant > 25 ans)"
  label(f$approbation_info) <<- "approbation_info: des exemples de l'effet de la réforme sur 7 niveaux de vie ont été affichés: 800>950/1100>1130/1500>1500/1800>1800/3000>2950/4000>3700/5000>4500/20k>16k"

  temp <- label(f$diplome)
  f$diplome <<- factor(f$diplome, c("","Aucun diplôme","Brevet des collèges","CAP ou BEP","Baccalauréat","Bac +2 (BTS, DUT, DEUG, écoles de formation sanitaires et sociales...)","Bac +3 (licence...)","Bac +5 ou plus (master, école d'ingénieur ou de commerce, doctorat, médecine, maîtrise, DEA, DESS...)","NSP (Ne se prononce pas)","NSP","Q76 - Quel est votre plus haut diplôme (ou celui que vous comptez avoir si vous ê...") )
  label(f$diplome) <<- temp
  temp <- label(f$interet_politique)
  f$interet_politique <<- factor(f$interet_politique, c("","Beaucoup","Un peu","Presque pas (ou pas du tout)","NSP","NSP (Je ne veux pas répondre)","Q20 - À quel point êtes-vous intéressé·e par la politique ?"))
  label(f$interet_politique) <<- temp
  temp <- label(f$refugies)  
  f$refugies <<- factor(f$refugies, c("","Il faut accepter tous les réfugiés qui fuient la guerre ou la persécution","Il faut accepter beaucoup plus de réfugiés qu'actuellement","Il faut accepter un quota de réfugiés plus élevé qu'actuellement","Il faut accepter un quota de réfugiés correspondant au nombre actuel de personnes accueillies légalement sur le territoire","Il faut accepter moins de réfugiés qu'actuellement","Il faut refuser à tout réfugié l'entrée sur le territoire","Il faut laisser à chaque commune le choix de fixer son quota de réfugiés","Il faut autoriser chaque français à parrainer un réfugié, le parrain serait responsable de l'intégration et si besoin de l'hébergement et de l'entretien du réfugié","NSP","NSP (Je ne veux pas répondre)","Q111 - Quelle politique faut-il adopter vis-à-vis des réfugiés qui veulent entrer...", "Il faut accepter autant de réfugiés qu'actuellement","Il faut accepter plus de réfugiés plus élevé qu'actuellement","Il faut accepter plus de réfugiés qu'actuellement"))
  f$refugies[f$refugies_info!=""] <<- f$refugies_info[f$refugies_info!=""]
  f$refugies[f$refugies_info_bis!=""] <<- f$refugies_info_bis[f$refugies_info_bis!=""]
  f$refugies[f$refugies=="Il faut accepter plus de réfugiés qu'actuellement" | f$refugies=="Il faut accepter plus de réfugiés plus élevé qu'actuellement"] <<- "Il faut accepter un quota de réfugiés plus élevé qu'actuellement"
  f$refugies[f$refugies=="Il faut accepter autant de réfugiés qu'actuellement"] <<- "Il faut accepter un quota de réfugiés correspondant au nombre actuel de personnes accueillies légalement sur le territoire"
  label(f$refugies) <<- temp
  f$refugies_info_bis[f$refugies_info!=""] <<- f$refugies_info[f$refugies_info!=""]
  f$refugies_info <<- FALSE
  f$refugies_info[f$refugies_info_bis!=""] <<- TRUE
  label(f$refugies_info) <<- "refugies_info: Le répondant a vu l'information sur le nombre et la proportion de demandeurs d'asile accueillis en France lors des cinq dernières années (20k/an soit un quart des demandes acceptées en moyenne)"
  f$Refugies <<- as.character(f$refugies)
  f$Refugies[f$refugies=="Il faut accepter tous les réfugiés qui fuient la guerre ou la persécution"] <<- "Tous"
  f$Refugies[f$refugies=="Il faut accepter beaucoup plus de réfugiés qu'actuellement"] <<- "Beaucoup plus"
  f$Refugies[f$refugies=="Il faut accepter un quota de réfugiés plus élevé qu'actuellement"] <<- "Plus"
  f$Refugies[f$refugies=="Il faut accepter un quota de réfugiés correspondant au nombre actuel de personnes accueillies légalement sur le territoire"] <<- "Autant"
  f$Refugies[f$refugies=="Il faut accepter moins de réfugiés qu'actuellement"] <<- "Moins"
  f$Refugies[f$refugies=="Il faut refuser à tout réfugié l'entrée sur le territoire"] <<- "Aucun"
  f$Refugies[f$refugies=="Il faut laisser à chaque commune le choix de fixer son quota de réfugiés"] <<- "Quota commune"
  f$Refugies[f$refugies=="Il faut autoriser chaque français à parrainer un réfugié, le parrain serait responsable de l'intégration et si besoin de l'hébergement et de l'entretien du réfugié"] <<- "Parrainage"
  f$Refugies <<- factor(f$Refugies, levels = c("", "Tous", "Beaucoup plus", "Plus", "Autant", "Moins",  "Aucun", "Quota commune", "Parrainage", "NSP"))
  label(f$Refugies) <<- label(f$refugies)
  
  f$rev_tot <<- 12 * pmax(f$revenu, f$revenu + f$revenu_conjoint, na.rm=T) # TODO: check bien défini pour tous
  f$revdisp <<- round((12 * f$revenu / f$rev_tot) * (f$rev_tot -  irpp_p(f$rev_tot,grepl("Mari", f$situation_maritale),f$taille_foyer))/12)
  f$niveau_vie <<- ((f$rev_tot -  irpp_p(f$rev_tot,grepl("Mari", f$situation_maritale),f$taille_foyer))/12) / uc(grepl("Mari", f$situation_maritale), f$taille_foyer)
  f$rev_tot <<- f$rev_tot/12
  f$disadvantaged <<- FALSE
  f$disadvantaged[f$revdisp>3000 & !is.na(f$revdisp) & f$Approbation_variante!="seuls"] <<- TRUE
  f$disadvantaged[f$revenu>2800 & f$Approbation_variante=="seuls" & !is.na(f$revenu)] <<- TRUE
  is.na(f$disadvantaged) <<- is.na(f$revenu)
  
  f$Age <<- (f$age == "18 à 24 ans") + 2*(f$age == "25 à 34 ans") + 3.3*(f$age == "35 à 49 ans") + 4.6*(f$age == "50 à 64 ans") + 7*(f$age == "65 ans ou plus")
  f$age <<- as.factor(as.character(f$age))
  f$Diplome <<- (f$diplome == "Brevet des collèges") + 2*(f$diplome=="CAP ou BEP") + 3*(f$diplome=="Baccalauréat") + 4*(f$diplome=="Bac +2 (BTS, DUT, DEUG, écoles de formation sanitaires et sociales...)") + 5*(f$diplome=="Bac +3 (licence...)") + 6*(f$diplome=="Bac +5 ou plus (master, école d'ingénieur ou de commerce, doctorat, médecine, maîtrise, DEA, DESS...)") - (f$diplome=="NSP (Ne se prononce pas)")
  f$diplome4 <<- as.character(f$diplome)
  f$diplome4[f$Diplome<2] <<- "Aucun diplôme ou brevet"
  f$diplome4[f$Diplome>3] <<- "Supérieur"
  f$Region <<- as.character(f$region)
  f$Region[f$region=="Bretagne" | f$region=="Normandie"] <<- "Bretagne et Normandie"
  f$Region[f$region=="Centre-Val de Loire" | f$region=="Pays de la Loire"] <<- "Centre-Val de Loire et Pays de la Loire"
  f$Region[f$region=="Grand Est" | f$region=="Bourgogne-Franche-Comté"] <<- "Grand Est et Bourgogne-Franche-Comté"
  f$Region[f$region=="NSP" | f$region=="Autre (outre-mer)" | f$region=="Corse"] <<- "Autre"
  f$Region <<- as.factor(f$Region)
  f$taille_agglo <<- as.factor(gsub("[[:alpha:] ]", "", f$taille_agglo))
  f <<- f[f$taille_agglo!="%1%",]
  
  for (i in 1:length(f)) {
    if (grepl("victoire_", names(f)[i])) {
      if (names(f)[i]!="victoire_bu" & names(f)[i]!="victoire_joueur") f[i][[1]] <<- as.logical(f[i][[1]]!="") 
      is.na(f[i][[1]]) <<- as.numeric(substr(f$endDate,9,10))<=15
    }
  }
}

prepare_f <- function(exclude_speeder=TRUE, exclude_screened=TRUE, exclude_quotas_full=TRUE, only_finished=TRUE, clean=TRUE, clean_all=FALSE) {
  # setwd("/home/adrien/Google Drive/Economie/Travail/enquete/codes")
  setwd("C:/Users/a.fabre/Google Drive/Economie/Travail/enquete/codes")
  # pef <<- read.csv("fin.csv", sep=";")
  # f <<- read.delim("politique.tsv", fileEncoding="UTF-16")
  # f_data <- read.delim("fin.tsv", fileEncoding="UTF-16")
  f_data <- read.csv2("fin.csv")
  f_agglo <- read.delim("fin_with_CC_16_07.tsv", fileEncoding="UTF-16")
  f_sup <- read.delim('fin_supp.tsv', fileEncoding="UTF-16")
  for (i in 1:length(f_data)) { label(f_data[[i]]) <- toString(f_data[i][[1]][1]) } # Use the first line to create variable names labels then remove it - to run only once
  for (i in 1:length(f_agglo)) { label(f_agglo[[i]]) <- toString(f_agglo[i][[1]][1]) }
  for (i in 1:length(f_sup)) { label(f_sup[[i]]) <- toString(f_sup[i][[1]][1]) }
  f_data <- f_data[-c(1,2),]
  f_agglo <- f_agglo[-c(1,2),]
  f_sup <- f_sup[-c(1,2),]
  f_data <- merge(f_data, f_sup, all=TRUE)
  f_data <- merge(f_data, f_agglo, all=TRUE) 
  f <<- cbind(f_data[,-c(97:109,145)],f_data[,c(97:109,145)])
  if (exclude_screened) { f <<- f[f$Q_TerminateFlag=="",] } # remove Screened
  if (exclude_speeder) { f <<- f[as.numeric(as.vector(f$Duration)) >= 450,] } # remove speedest
  if (exclude_quotas_full) { f <<- f[f[101][[1]] %in% c(1:5),]  } # remove those with a problem for the taille d'agglo
  if (only_finished) { f <<- f[Vf("Finished")=="True",] }
  
  relabel_and_rename_f()
  convert_f()
  
  f$sample <<- "a"
  f$sample[f$finished=="True"] <<- "e"
  f$sample[f$finished=="True" & n(f$duree) >= 450] <<- "p"
  f$sample[f$finished=="True" & n(f$duree) >= 450 & f$test_qualite=='Un peu'] <<- "f" # TODO:"q"? excluded because out of quotas
  f$sample[f$finished=="True" & n(f$duree) >= 450 & f$exclu==""] <<- "r"
  f$Sample <<- f$sample
  
  # remove duplicated TODO: complete
  # for (i in which(duplicated(f$id) & is.na(f$revenu))) { print(paste(i, which(f$id==f$id[i] & !duplicated(f$id) & is.na(f$revenu))))  }
  # f$revenu[which(duplicated(f$id) & is.na(f$revenu))] == -99
  f <<- f[-which(is.element(f$id, f$id[duplicated(f$id)]) & !duplicated(f$id) & is.na(f$revenu)),]
  # f <- f[-which(f$revenu==-99),]
  # which(is.element(f$id, f$id[duplicated(f$id)]) & !duplicated(f$id) & !is.na(f$revenu))
  # which(is.element(f$id, f$id[duplicated(f$id)]) & !is.na(f$revenu))
  # for (i in which(is.element(f$id, f$id[duplicated(f$id)]) & !duplicated(f$id) & !is.na(f$revenu))) { print(which(f$id==f$id[i] & duplicated(f$id) & !is.na(f$revenu)))  }
  
  # removing one third of variables
  # r <- c(3,9:17,176) # completely useless columns
  # r <- c(r,47:50,55:58,62:65,68:71,74:77,94:97,125:128) # + columns to be removed after cleaning (durations, click counts)
  # r <- c(r,9,39:46,51,53,84:86,129,130,140:155,158,160,161,164,168,171,192) # + columns to be removed because we can infer their values from other columns
  # if (clean_all) { f <<- f[,-c(r,1,2,4,5,7,8,177,178,182)] } # remove also semi-useless columns: date, variantes, ID, ip, finished
  # else if (clean) { f <<- f[,-r] } 
  # rm(pep, pos = ".GlobalEnv")
  
  f$weight <<- weighting_f(f)
}

merge_f_m <- function(type='') {
  f$vague <<- 3
  
  if (type=='' | type=='q') d <- m
  else if (type=='a') d <- ma
  else if (type=='e') d <- me
  else if (type=='p') d <- mp
  else if (type=='c') d <- mc
  
  d$sexe <- as.character(d$sexe)
  d$sexe[d$sexe=="Un homme"] <- "Homme"
  d$sexe[d$sexe=="Une femme"] <- "Femme"
  d$sexe <- as.factor(d$sexe)
  
  d$refugies_info <- FALSE 
  label(d$refugies_info) <- "refugies_info: Le répondant a vu l'information sur le nombre et la proportion de demandeurs d'asile accueillis en France lors des cinq dernières années (20k/an soit un quart des demandes acceptées en moyenne)"
  d$Refugies <- as.character(d$refugies)
  d$Refugies[d$refugies=="Il faut accepter tous les réfugiés qui fuient la guerre ou la persécution"] <- "Tous"
  d$Refugies[d$refugies=="Il faut accepter beaucoup plus de réfugiés qu'actuellement"] <- "Beaucoup plus"
  d$Refugies[d$refugies=="Il faut accepter un quota de réfugiés plus élevé qu'actuellement"] <- "Plus"
  d$Refugies[d$refugies=="Il faut accepter un quota de réfugiés correspondant au nombre actuel de personnes accueillies légalement sur le territoire"] <- "Autant"
  d$Refugies[d$refugies=="Il faut accepter moins de réfugiés qu'actuellement"] <- "Moins"
  d$Refugies[d$refugies=="Il faut refuser à tout réfugié l'entrée sur le territoire"] <- "Aucun"
  d$Refugies[d$refugies=="Il faut laisser à chaque commune le choix de fixer son quota de réfugiés"] <- "Quota commune"
  d$Refugies[d$refugies=="Il faut autoriser chaque français à parrainer un réfugié, le parrain serait responsable de l'intégration et si besoin de l'hébergement et de l'entretien du réfugié"] <- "Parrainage"
  d$Refugies <- factor(d$Refugies, levels = c("", "Tous", "Beaucoup plus", "Plus", "Autant", "Moins",  "Aucun", "Quota commune", "Parrainage", "NSP"))
  label(d$Refugies) <- label(d$refugies)
  is.na(d$Refugies) <- d$Refugies==""
 
  d$refugies_echelle <- as.numeric(d$Refugies)
  f$refugies_echelle <<- as.numeric(f$Refugies)
  is.na(d$refugies_echelle) <- d$refugies_echelle==1 |d$refugies_echelle>7
  is.na(f$refugies_echelle) <<- f$refugies_echelle==1 | f$refugies_echelle>7
  d$moinsRefugies <- (d$refugies_echelle== 6) | (d$refugies_echelle == 7)
  f$moinsRefugies <<- (f$refugies_echelle == 6) | (f$refugies_echelle == 7)
  is.na(d$moinsRefugies) <- (d$refugies_echelle == 1)
  d$plusRefugies <- (d$refugies_echelle== 2) | (d$refugies_echelle == 3) | (d$refugies_echelle == 4)
  f$plusRefugies <<- (f$refugies_echelle== 2) | (f$refugies_echelle == 3) | (f$refugies_echelle == 4)
  is.na(d$plusRefugies) <- (d$refugies_echelle == 1)

  d$transferts_inter_actuel_vu <- 0
  d$transferts_inter_info <- 0
  label(d$transferts_inter_info) <- label(f$transferts_inter_info)
  label(d$transferts_inter_actuel_vu) <- label(f$transferts_inter_actuel_vu)
  
  d$variante_fusion <- "pro_fusion"
  f$variante_fusion <<- "anti_fusion"
  label(d$variante_fusion) <- "variante_fusion: Variante dans la formulation de la préférence sur la fusion de l'IRPP et des cotisations sociales : 'êtes-vous en faveur/opposé à ... ?'"
  label(f$variante_fusion) <<- label(d$variante_fusion)
  f$fusion_irpp_cotsoc[f$fusion_irpp_cotsoc_contre=="Oui"] <<- "Non"
  f$fusion_irpp_cotsoc[f$fusion_irpp_cotsoc_contre=="Non"] <<- "Oui"
  label(f$fusion_irpp_cotsoc) <<- label(d$fusion_irpp_cotsoc)
  
  d$Approbation_variante <- "indiv"
  d$approbation <- d$approbation_mediane
  d$approbation_info <- FALSE
  d$compris_approbation <- "Bug"
  d$compris_approbation[grepl("parfois pas su répondre", d$incompris)] <- "Non"
  d$compris_approbation[grepl("Non", d$incompris)] <- "Oui"
  d$disadvantaged <- (d$revdisp > 3000)
  d$smic <- NA
  d$smic <- as.item(as.numeric(d$smic),  missing.values = -1, annotation=attr(f$smic, "label"))
  return(d)
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
# TODO: check fa$revenu[fa$revenu/60>20000 & !is.na(fa$revenu)], qualité revenu=8.333333e+18 et autres