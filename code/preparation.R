# setwd("/var/www/beliefs_climate_policies/code")
# setwd("C:/Users/thoma/Documents/Github/beliefs_climate_policies/code")
# setwd("C:/Users/t.douenne/Documents/Github/beliefs_climate_policies/code")
# setwd("/home/adrien/Documents/beliefs_climate_policies/code")
# setwd("C:/Users/a.fabre/Documents/beliefs_climate_policies/code")

source("packages_functions.R")

clean_number <- function(vec, high_numbers='') { 
   numeric_vec <- as.numeric(gsub(",", ".", gsub("[[:alpha:]  !#$%&')?/(@:;€_-]","",vec)))
   if (high_numbers=='remove') { is.na(numeric_vec) <- numeric_vec>10000 }
   else if (high_numbers=='divide') { numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)] <- numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)]/12 }
   else if (high_numbers=='divide&remove') { 
     numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)] <- numeric_vec[numeric_vec>10000 & !is.na(numeric_vec)]/12
     is.na(numeric_vec) <- numeric_vec>6000 }
   return(numeric_vec)
}
uc <- function(nb_pers, nb_14_et_plus) {
  # https://www.insee.fr/fr/metadonnees/definition/c1802
  return(1 + 0.5 * pmax(0, nb_14_et_plus - 1) + 0.3 * pmax(0, nb_pers - nb_14_et_plus))
}
quotient <- function(nb_pers, nb_adultes) {
	# https://droit-finances.commentcamarche.com/contents/907-quotient-familial-calcul-du-nombre-de-parts-fiscales
	# nb de parts fiscales en fonction de la situation et de nb_pers = 1 / 2 / 3 / 4 / 5
	# marie: x / 2 / 2.5 / 3 / 4 --- en concubinage: x / 1 / 1.5 /2 / 3 (= marie - 1) --- seul: 1 / 2 / 2.5 / 3.5 / 4.5
	return((nb_pers == 1) + (nb_pers == 2)*2 + (nb_pers == 3)*2.5 + (nb_pers == 4)*3 + (nb_pers > 4)*pmin(6, nb_pers - 1) + (nb_adultes==1)*(nb_pers > 3)*0.5 )
}
irpp <- function(rev, nb_adultes, nb_pers) {
	# quotient <- (nb_pers < 2) + (nb_pers == 2) * 2 + (nb_pers == 3) * 2.5 + (nb_pers == 4) * 3 + (nb_pers > 4) * pmin(6, nb_pers - 1)
	income <- 0.9334 * rev / quotient(nb_pers, nb_adultes) # (1 + (0.029 * 1.28))*0.9 : passage au brut (+28% en moyenne), CSG+CRDS non déductibles (2,90%), puis abattement de 10%
	ir <- 0
	ir <- ir + (income - 12815.25*12) * 0.45 * (income > 12815.25*12)
	ir <- ir + (pmin(income, 12676*12) - 6051.42*12) * 0.41  * (income > 6051.42*12)
	ir <- ir + (pmin(income, 6051.42*12) - 2257.17*12) * 0.3  * (income > 2257.17*12)
	ir <- ir + (pmin(income, 2257.17*12) - 817.25*12) * 0.14  * (income > 817.25*12)
	
	ir <- quotient(nb_pers, nb_adultes) * ir
	seuil_decote <- (nb_adultes>1)*2585/12 + (nb_adultes<=1)*1569/12
	# decote <- (1920 - 0.75 * ir) * (marie & ir<2560) + (1165 - 0.75 * ir) * (!(marie) & ir<1553)
	decote <- (ir < seuil_decote) * 0.75 * (seuil_decote - ir)
	return(pmax((ir-decote),0)) # vrai calcul
}

##### Correspondance zipcode - region #####
# communes_agglo <- read.xls("table-appartenance-geo-communes-18_V2.xls", pattern="CODGEO") # 2018
# communes_PLM <- read.xls("table-appartenance-geo-communes-18_V2.xls", sheet=2, pattern="CODGEO") # Paris Lyon Marseille
# communes_data <- read.csv("correspondance-code-insee-code-postal.csv", sep=";") # 2013
# communes_agglo <- communes_agglo[,c('CODGEO', 'TUU2015')]
# communes_PLM <- communes_PLM[,c('CODGEO', 'TUU2015')]
# colnames(communes_agglo) <- c('Code.INSEE', 'taille_agglo')
# colnames(communes_PLM) <- c('Code.INSEE', 'taille_agglo')
# communes_agglo$Code.INSEE <- as.character(communes_agglo$Code.INSEE)
# communes_PLM$Code.INSEE <- as.character(communes_PLM$Code.INSEE)
# communes_data <- communes_data[,c('Code.INSEE', "Code.Postal", 'Population')]
# communes <- merge(merge(communes_agglo, communes_PLM, all=T), communes_data, all=T)
# sum(communes$Population[is.na(communes$taille_agglo)], na.rm=T) # 750k missing because of Code.INSEE renaming
# taille_agglo <- aggregate(1000*Population ~ taille_agglo, communes, sum)
# colnames(taille_agglo) <- c('taille_agglo', 'pop')
# taille_agglo$share <- taille_agglo$pop / sum(taille_agglo$pop)
# taille_agglo # total: 63.76 M
# taille_agglo$share[taille_agglo$taille_agglo==0] # rural
# sum(taille_agglo$share[taille_agglo$taille_agglo<=3 & taille_agglo$taille_agglo>0]) # <20k
# sum(taille_agglo$share[taille_agglo$taille_agglo<=5 & taille_agglo$taille_agglo>3]) # <100k
# sum(taille_agglo$share[taille_agglo$taille_agglo<8 & taille_agglo$taille_agglo>5]) # >100k
# taille_agglo$share[taille_agglo$taille_agglo==8] # Paris


##### Quantiles de revenus ERFS 2014 #####
# quantiles <- function(data, weights = NULL)  {
#   if (is.null(weights)) return(function(q) { pmax(0,quantile(data, probs = q, na.rm = TRUE, names = FALSE) / 12)} )
#   else return(function(q) { pmax(0, wtd.quantile(data, probs = q, na.rm = TRUE, weights = weights) / 12)} )
# }
# seuils_all <- function(q) {
#   s <- vector("numeric", length=5)
#   for (i in 1:9) s[i] <- q(i/10)
#   # s[1] <- q(0.1, weights = weights)
#   # s[2] <- q(0.2, weights = weights)
#   # s[3] <- q(0.3, weights = weights)
#   # s[4] <- q(0.4, weights = weights)
#   # s[5] <- q(0.5, weights = weights)
#   # s[6] <- q(0.6, weights = weights)
#   # s[7] <- q(0.7, weights = weights)
#   # s[8] <- q(0.8, weights = weights)
#   # s[9] <- q(0.9, weights = weights)
#   return(round(s,0))
# }
# wd <- getwd()
# setwd("U:/Données/ERFS_2014")
# setwd("/media/adrien/dd/adrien/DD/Économie/Données/ERFS_2014/Stata")
# indiv <- read.dta13("fpr_indiv_2014.dta")
# irft4 <- read.dta13("fpr_irf14e14t4.dta")
# menage <- read.dta13("fpr_menage_2014.dta")
# menage$presta_sociales <- menage$prest_fam_petite_enfance + menage$prest_fam_autres + menage$prest_precarite_rsa + menage$m_rsa_actm + menage$prest_precarite_hand + menage$prest_precarite_vieil + menage$prest_logement + menage$ppe
# irft4 <- irft4[,which(is.element(colnames(irft4),c("noindiv", "noiprm", "nbinde", "nbenf18", "ag", "mchoe", "ancinatm")))]
# menage <- menage[,which(is.element(colnames(menage),c("ident14", "revdispm","impots" ,"revdecm", "nivviem", "presta_sociales", "revenu_ajuste", "rev_cat_net", "wpri")))]
# # Prestas aux parents, i.e. aux deux adultes les plus âgés du ménage
# temp <- merge(indiv, irft4, by="noindiv")
# db <- merge(temp, menage, by="ident14")
# names(db)[1] <- "group"
# names(db)[28] <- "age"
# db$age <- as.numeric(db$age)
# temp <- aggregate(age ~ group, db, function(vec) {
#   if (length(vec) >= 2) return(max(vec[-which.max(vec)]))
#   else return(-1)})
# names(temp)[2] <- "age_second"
# db$order <- seq(len=nrow(db))
# db <- merge(db, temp, all=TRUE, by="group")
# temp <- aggregate((age >= pmax(age_second, 18)) ~ group, db, sum)
# names(temp)[2] <- "adult_above_second"
# db$order <- seq(len=nrow(db))
# db <- merge(db, temp, all=TRUE, by="group")
# temp <- aggregate((age > 17) ~ group, db, sum)
# names(temp)[2] <- "nb_adultes"
# db$order <- seq(len=nrow(db))
# db <- merge(db, temp, all=TRUE, by="group")
# db$revenu_imputable_i <- db$salaires_i + db$chomage_i + db$retraites_i + db$rag_i + db$rnc_i + db$ric_i + db$pens_alim_recue_i # Ce dernier, dont l'ajout est discutable, ne représente que  5 milliards
# temp <- aggregate(revenu_imputable_i ~ group, db, sum)
# names(temp)[2] <- "revenu_imputable_m"
# db <- merge(temp, db, all=TRUE,by="group")
# db <- db[sort.list(db$order),]
# # On exclut les revenus négatifs
# db$proportion_imputee[db$revenu_imputable_m > 0] <- db$revenu_imputable_i[db$revenu_imputable_m > 0] / db$revenu_imputable_m[db$revenu_imputable_m > 0]
# db$proportion_imputee[db$revenu_imputable_m == 0 & db$age > 17 & db$nb_adultes>0] <- 1 / db$nb_adultes[db$revenu_imputable_m == 0 & db$age > 17 & db$nb_adultes>0]
# db$proportion_imputee[db$revenu_imputable_m == 0 & db$age < 18] <- 0
# 
# db$revtot_i_par <- db$revdecm * db$proportion_imputee + db$presta_sociales * (db$age >= pmax(db$age_second, 18)) /  pmax(1, db$adult_above_second)
# sum(db$revtot_i_par, na.rm=T)/sum(menage$presta_sociales + menage$revdecm, na.rm=T) # 1.000032
# 
# # Déciles de revenus inflatés : (croissance PIB 2014-2018	1.06075007	https://www.insee.fr/fr/statistiques/2830613#tableau-Tableau1 )
# deciles_erfs_inflates <- 1.06075007*seuils_all(quantiles(db$revtot_i_par[!is.na(db$revtot_i_par) & db$age > 17]))
# # This is the one used:
# round(deciles_erfs_inflates) # 229 779 1142 1429 1671 1922 2222 2641 3436
# deciles_erfs_inflates_weighted <- 1.06075007*seuils_all(quantiles(db$revtot_i_par[!is.na(db$revtot_i_par) & db$age > 17], weights = db$wprm[!is.na(db$revtot_i_par) & db$age > 17]))
# round(deciles_erfs_inflates_weighted) # 237 789 1151 1436 1677 1927 2231 2657 3462
# deciles_menage_erfs_inflates_weighted <- 1.06075007*seuils_all(quantiles(db$revdecm + db$presta_sociales, weights = db$wprm))
# 
# distribution_revenu_erfs <- wtd.Ecdf(db$revtot_i_par[!is.na(db$revtot_i_par) & db$age > 17 & !is.na(db$age)])
# distribution_revenu_erfs_weighted <- wtd.Ecdf(db$revtot_i_par[!is.na(db$revtot_i_par) & db$age > 17 & !is.na(db$age)], weights = db$wprm[!is.na(db$revtot_i_par) & db$age > 17 & !is.na(db$age)])
# # plot(distribution_revenu_erfs$x, distribution_revenu_erfs$ecdf, type='l', xlim=c(0,60000), col="blue")
# distribution_rev_tot_erfs <- wtd.Ecdf(db$revdecm + db$presta_sociales)
# distribution_rev_tot_erfs_weighted <- wtd.Ecdf(db$revdecm + db$presta_sociales, weights = db$wprm)
# 
# # Distribution of adults > 70th. income percentile in function of their spouse income
# db$revenu_conjoint <- db$revdecm + db$presta_sociales - db$revtot_i_par # selon la définition du sondage où il n'y a que deux parents
# db$revenu_conjoint[db$nb_adultes < 2] <- 9999*12
# share__20 <- length(which((db$revtot_i_par < 734*12 | (db$revenu_conjoint < 734*12 & db$revtot_i_par > 2095*12)) & !is.na(db$revtot_i_par) & db$age > 17))/length(which(!is.na(db$revtot_i_par) & db$age > 17))
# share_20_30 <- length(which(((db$revtot_i_par < 1077*12 & db$revtot_i_par >= 734*12) | (db$revenu_conjoint < 1077*12 & db$revenu_conjoint >= 734*12 & db$revtot_i_par > 2095*12)) & !is.na(db$revtot_i_par) & db$age > 17))/length(which(!is.na(db$revtot_i_par) & db$age > 17))
# share_30_40 <- length(which(((db$revtot_i_par < 1347*12 & db$revtot_i_par >= 1077*12) | (db$revenu_conjoint < 1347*12 & db$revenu_conjoint >= 1077*12 & db$revtot_i_par > 2095*12)) & !is.na(db$revtot_i_par) & db$age > 17))/length(which(!is.na(db$revtot_i_par) & db$age > 17))
# share_40_50 <- length(which(((db$revtot_i_par < 1575*12 & db$revtot_i_par >= 1347*12) | (db$revenu_conjoint < 1575*12 & db$revenu_conjoint >= 1347*12 & db$revtot_i_par > 2095*12)) & !is.na(db$revtot_i_par) & db$age > 17))/length(which(!is.na(db$revtot_i_par) & db$age > 17))
# share_50_70 <- length(which(((db$revtot_i_par < 2095*12 & db$revtot_i_par >= 1575*12) | (db$revenu_conjoint < 2095*12 & db$revenu_conjoint >= 1575*12 & db$revtot_i_par > 2095*12)) & !is.na(db$revtot_i_par) & db$age > 17))/length(which(!is.na(db$revtot_i_par) & db$age > 17))
# share_70_ <- length(which(db$revtot_i_par >= 2095*12 & db$revenu_conjoint >= 2095*12 & !is.na(db$revtot_i_par) & db$age > 17))/length(which(!is.na(db$revtot_i_par) & db$age > 17))
# sum(c(share__20, share_20_30, share_30_40, share_40_50, share_50_70, share_70_))
# expected_target_proportions <- share_70_ / 4 + c('20' = share__20 + share_20_30/2, '30' = share_20_30/2 + share_30_40/2, '40' = share_30_40/2 + share_40_50/2, '50' = share_40_50/2 + share_50_70)
# round(expected_target_proportions, 3) 
# # decrit(s$cible) # incredibly close!
# # shares <- c('_20' = share__20, '20_30'=share_20_30, '30_40'=share_30_40, '40_50'=share_40_50, '50_70'=share_50_70, '70_'=share_70_)
# # decrit(s$categorie_cible)
# 
# rm(db, temp, irft4, menage, indiv)
# setwd(wd)


##### Preparation #####

relabel_and_rename_s <- function() {
  # Notation: ~ means that it's a random variant; * means that another question is exactly the same (in another random branch)
  
  # The commented lines below should be executed before creating relabel_and_rename, to ease the filling of each name and label
  # for (i in 1:length(s)) {
  #   label(s[[i]]) <- paste(names(s)[i], ": ", label(s[[i]]), sep="");
  #   print(paste(i, label(s[[i]])))
  # }
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
  names(s)[6] <<- "duree2"
  label(s[[6]]) <<- "duree2: Durée de complétion du questionnaire (pn secondes)"
  names(s)[7] <<- "fini"
  label(s[[7]]) <<- "fini: Terminé"
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
  names(s)[26] <<- "premier_clic_info_CC"
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
  label(s[[38]]) <<- "csp: Catégorie Socio-Professionnelle: Agriculteur/Indépendant: Artisan, commerçant.e/Cadre: Profession libérale, cadre/Intermédiaire: Profession intermédiaire/Employé/Ouvrier/Retraité/Inactif: Autres inactif/ve - Q98"
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
  label(s[[48]]) <<- "nb_vehicules_texte: Nombre de véhicules motorisés dont dispose le ménage (Aucun/Un/Deux ou plus) - Q37"
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
  names(s)[57] <<- "perte_relative_tva"
  label(s[[57]]) <<- "perte_relative_tva: Une hausse de la TVA ferait perdre plus à votre ménage que la moyenne (Oui, beaucoup/un peu plus/Autant que la moyenne/Non, un peu/beaucoup moins/NSP) - Q150"
  names(s)[58] <<- "perte_relative_fuel"
  label(s[[58]]) <<- "perte_relative_fuel: ~ Une hausse des taxes sur les carburants ferait perdre plus à votre ménage que la moyenne (Oui, beaucoup/un peu plus/Autant que la moyenne/Non, un peu/beaucoup moins/NSP) - Q151"
  names(s)[59] <<- "gagnant_fuel_categorie"
  label(s[[59]]) <<- "gagnant_fuel_categorie: ~ Ménage Gagnant/Non affecté/Perdant par hausse taxe carburants redistribuée à tous (+0.11/13 €/L diesel/essence, +60€/an /adulte) - Q152"
  names(s)[60] <<- "gain_taxe_fuel_hausse"
  label(s[[60]]) <<- "gain_taxe_fuel_hausse: ~ (gagnant_fuel_categorie=Gagnant) Hausse de pouvoir d'achat du ménage suite à hausse taxe carburants redistribuée à tous (seuils à 10/20/30/40 €/an /UC) - Q153"
  names(s)[61] <<- "gain_taxe_fuel_baisse"
  label(s[[61]]) <<- "gain_taxe_fuel_baisse: ~ (gagnant_fuel_categorie=Perdant) Baisse de pouvoir d'achat du ménage suite à hausse taxe carburants redistribuée à tous (seuils à 15/40/70/110/160 €/an /UC) - Q154"
  names(s)[62] <<- "elasticite_fuel_perso"
  label(s[[62]]) <<- "elasticite_fuel_perso: ~ Réduction de la conso de carburants du ménage suite à augmentation du prix de 0.5€/L (0% - Je n'en consomme déjà presque pas/0% - Je suis contraint sur tous mes déplacements/de 0% à 10%/de 10% à 20%/de 20% à 30%/+ de 30% - Je changerais largement mes habitudes de déplacement) - Q159"
  names(s)[63] <<- "elasticite_fuel"
  label(s[[63]]) <<- "elasticite_fuel: ~ Réduction moyenne de la conso de carburants des Français suite à augmentation du prix de 0.5€/L (de 0% à 3%/de 3% à 10%/de 10% à 20%/de 20% à 30%/+ de 30%) - Q162"
  names(s)[64] <<- "perte_relative_chauffage"
  label(s[[64]]) <<- "perte_relative_chauffage: ~ Une hausse des taxes sur le fioul et le gaz ferait perdre plus à votre ménage que la moyenne (Oui, beaucoup/un peu plus/Autant que la moyenne/Non, un peu/beaucoup moins/NSP) - Q155"
  names(s)[65] <<- "gagnant_chauffage_categorie"
  label(s[[65]]) <<- "gagnant_chauffage_categorie: ~ Ménage Gagnant/Non affecté/Perdant par hausse taxe fioul et gaz redistribuée à tous (+13/15% gaz/fioul, +50€/an /adulte) - Q156"
  names(s)[66] <<- "gain_taxe_chauffage_hausse"
  label(s[[66]]) <<- "gain_taxe_chauffage_hausse: ~ (gagnant_chauffage_categorie=Gagnant) Hausse de pouvoir d'achat du ménage suite à hausse taxe fioul et gaz redistribuée à tous (seuils à 10/20/30/40 €/an /UC) - Q157"
  names(s)[67] <<- "gain_taxe_chauffage_baisse"
  label(s[[67]]) <<- "gain_taxe_chauffage_baisse: ~ (gagnant_chauffage_categorie=Perdant) Baisse de pouvoir d'achat du ménage suite à hausse taxe fioul et gaz redistribuée à tous (seuils à 15/40/70/110/160 €/an /UC) - Q158"
  names(s)[68] <<- "elasticite_chauffage_perso"
  label(s[[68]]) <<- "elasticite_chauffage_perso: ~ Réduction de la conso de fioul et gaz du ménage suite à augmentation du prix de 30% (0% - Je n'en consomme déjà presque pas/0% - Je suis contraint sur tous mes déplacements/de 0% à 10%/de 10% à 20%/de 20% à 30%/+ de 30% - Je changerais largement mes habitudes de déplacement) - Q160"
  names(s)[69] <<- "elasticite_chauffage"
  label(s[[69]]) <<- "elasticite_chauffage: ~ Réduction moyenne de la conso de fioul et gaz des Français suite à augmentation du prix de 30% (de 0% à 3%/de 3% à 10%/de 10% à 20%/de 20% à 30%/+ de 30%) - Q163"
  names(s)[70] <<- "gagnant_categorie"
  label(s[[70]]) <<- "gagnant_categorie: Ménage Gagnant/Non affecté/Perdant par hausse taxe carbone redistribuée à tous (+110€/an /adulte, +13/15% gaz/fioul, +0.11/13 €/L diesel/essence) - Q164"
  names(s)[71] <<- "gain_taxe_hausse"
  label(s[[71]]) <<- "gain_taxe_hausse: (gagnant_categorie=Gagnant) Hausse de pouvoir d'achat du ménage suite à hausse taxe carbone redistribuée à tous (seuils à 20/40/60/80 €/an /UC) - Q165"
  names(s)[72] <<- "gain_taxe_baisse"
  label(s[[72]]) <<- "gain_taxe_baisse: (gagnant_categorie=Perdant) Baisse de pouvoir d'achat du ménage suite à hausse taxe carbone redistribuée à tous (seuils à 30/70/120/190/280 €/an /UC) - Q166"
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
  names(s)[91] <<- "gagnant_feedback_categorie"
  label(s[[91]]) <<- "gagnant_feedback_categorie: ~ info si le ménage est gagnant/perdant - Ménage Gagnant/Non affecté/Perdant par hausse taxe carbone redistribuée à tous (+110€/an /adulte, +13/15% gaz/fioul, +0.11/13 €/L diesel/essence) - Q63"
  names(s)[92] <<- "taxe_feedback_approbation"
  label(s[[92]]) <<- "taxe_feedback_approbation: ~ info si le ménage est gagnant/perdant - Approbation d'une hausse de la taxe carbone compensée (+110€/an /adulte, +13/15% gaz/fioul, +0.11/13 €/L diesel/essence) - Q64"
  names(s)[93] <<- "benefices_CC_f"
  label(s[[93]]) <<- "benefices_CC_f: * Participe à la lutte contre le changement climatique - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[94] <<- "benefices_sante_f"
  label(s[[94]]) <<- "benefices_sante_f: * Réduit les effets néfastes de la pollution sur la santé - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[95] <<- "benefices_circulation_f"
  label(s[[95]]) <<- "benefices_circulation_f: * Réduit les embouteillages - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[96] <<- "benefices_revenu_f"
  label(s[[96]]) <<- "benefices_revenu_f: * Augmente mon pouvoir d'achat - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[97] <<- "benefices_pauvres_f"
  label(s[[97]]) <<- "benefices_pauvres_f: * Augmente le pouvoir d'achat des plus modestes - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[98] <<- "benefices_independance_f"
  label(s[[98]]) <<- "benefices_independance_f: * Favorise l'indépendance de la France aux importations d'énergie fossile - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[99] <<- "benefices_enjeu_f"
  label(s[[99]]) <<- "benefices_enjeux_f: * Prépare l'économie aux enjeux de demain - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[100] <<- "benefices_aucun_f"
  label(s[[100]]) <<- "benefices_aucun_f: * Pour aucune de ces raisons - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[101] <<- "benefices_autre_choix_f"
  label(s[[101]]) <<- "benefices_autre_choix_f: * Autre (préciser) - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[102] <<- "benefices_autre_f"
  label(s[[102]]) <<- "benefices_autre_f: * Champ libre - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q66"
  names(s)[103] <<- "problemes_inefficace_f"
  label(s[[103]]) <<- "problemes_inefficace_f: * Est inefficace pour réduire la pollution - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[104] <<- "problemes_alternatives_f"
  label(s[[104]]) <<- "problemes_alternatives_f: * Les alternatives sont insuffisantes ou trop chères - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[105] <<- "problemes_ruraux_f"
  label(s[[105]]) <<- "problemes_ruraux_f: * Pénalise les milieux ruraux - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[106] <<- "problemes_revenu_f"
  label(s[[106]]) <<- "problemes_revenu_f: * Diminue mon pouvoir d'achat - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[107] <<- "problemes_pauvres_f"
  label(s[[107]]) <<- "problemes_pauvres_f: * Diminue le pouvoir d'achat de certains ménages modestes - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[108] <<- "problemes_economie_f"
  label(s[[108]]) <<- "problemes_economie_f: * Nuit à l'économie et à l'emploi - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[109] <<- "problemes_pretexte_f" # TODO: vérifier recodage marche bien, cf. survey_old
  label(s[[109]]) <<- "problemes_pretexte_f: * Est un prétexte pour augmenter les impôts - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[110] <<- "problemes_aucun_f"
  label(s[[110]]) <<- "problemes_aucun_f: * Pour aucune de ces raisons - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[111] <<- "problemes_autre_choix_f"
  label(s[[111]]) <<- "problemes_autre_choix_f: * Autre (préciser) - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[112] <<- "problemes_autre_f"
  label(s[[112]]) <<- "problemes_autre_f: * Champ libre - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q67"
  names(s)[113] <<- "gagnant_progressif_categorie"
  label(s[[113]]) <<- "gagnant_progressif_categorie: ~ info que taxe est progressive - Ménage Gagnant/Non affecté/Perdant par hausse taxe carbone redistribuée à tous (+110€/an /adulte, +13/15% gaz/fioul, +0.11/13 €/L diesel/essence) - Q185"
  names(s)[114] <<- "taxe_progressif_approbation"
  label(s[[114]]) <<- "taxe_progressif_approbation: ~ info que taxe est progressive - Approbation d'une hausse de la taxe carbone compensée (+110€/an /adulte, +13/15% gaz/fioul, +0.11/13 €/L diesel/essence) - Q186"
  names(s)[115] <<- "benefices_CC_p"
  label(s[[115]]) <<- "benefices_CC_p: * Participe à la lutte contre le changement climatique - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[116] <<- "benefices_sante_p"
  label(s[[116]]) <<- "benefices_sante_p: * Réduit les effets néfastes de la pollution sur la santé - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[117] <<- "benefices_circulation_p"
  label(s[[117]]) <<- "benefices_circulation_p: * Réduit les embouteillages - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[118] <<- "benefices_revenu_p"
  label(s[[118]]) <<- "benefices_revenu_p: * Augmente mon pouvoir d'achat - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[119] <<- "benefices_pauvres_p"
  label(s[[119]]) <<- "benefices_pauvres_p: * Augmente le pouvoir d'achat des plus modestes - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[120] <<- "benefices_independance_p"
  label(s[[120]]) <<- "benefices_independance_p: * Favorise l'indépendance de la France aux importations d'énergie fossile - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[121] <<- "benefices_enjeu_p"
  label(s[[121]]) <<- "benefices_enjeux_p: * Prépare l'économie aux enjeux de demain - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[122] <<- "benefices_aucun_p"
  label(s[[122]]) <<- "benefices_aucun_p: * Pour aucune de ces raisons - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[123] <<- "benefices_autre_choix_p"
  label(s[[123]]) <<- "benefices_autre_choix_p: * Autre (préciser) - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[124] <<- "benefices_autre_p"
  label(s[[124]]) <<- "benefices_autre_p: * Champ libre - Bénéfices d'une taxe carbone compensée (maximum trois réponses) - Q187"
  names(s)[125] <<- "problemes_inefficace_p"
  label(s[[125]]) <<- "problemes_inefficace_p: * Est inefficace pour réduire la pollution - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[126] <<- "problemes_alternatives_p"
  label(s[[126]]) <<- "problemes_alternatives_p: * Les alternatives sont insuffisantes ou trop chères - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[127] <<- "problemes_ruraux_p"
  label(s[[127]]) <<- "problemes_ruraux_p: * Pénalise les milieux ruraux - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[128] <<- "problemes_revenu_p"
  label(s[[128]]) <<- "problemes_revenu_p: * Diminue mon pouvoir d'achat - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[129] <<- "problemes_pauvres_p"
  label(s[[129]]) <<- "problemes_pauvres_p: * Diminue le pouvoir d'achat de certains ménages modestes - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[130] <<- "problemes_economie_p"
  label(s[[130]]) <<- "problemes_economie_p: * Nuit à l'économie et à l'emploi - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[131] <<- "problemes_pretexte_p"
  label(s[[131]]) <<- "problemes_pretexte_p: * Est un prétexte pour augmenter les impôts - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[132] <<- "problemes_aucun_p"
  label(s[[132]]) <<- "problemes_aucun_p: * Pour aucune de ces raisons - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[133] <<- "problemes_autre_choix_p"
  label(s[[133]]) <<- "problemes_autre_choix_p: * Autre (préciser) - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[134] <<- "problemes_autre_p"
  label(s[[134]]) <<- "problemes_autre_p: * Champ libre - Indésirabilités d'une taxe carbone compensée (maximum trois réponses) - Q188"
  names(s)[135] <<- "gagnant__20_categorie"
  label(s[[135]]) <<- "gagnant__20_categorie: Le répondant estime que son ménage serait gagnant/non affecté/perdant par hausse taxe carbone redistribuée aux 20% des plus modestes (+550€/an/adulte concerné) - Q117"
  names(s)[136] <<- "taxe__20_approbation"
  label(s[[136]]) <<- "taxe__20_approbation: Le répondant approuverait une hausse de la taxe carbone redistribuée aux 20% des plus modestes (+550€/an/adulte concerné) - Q115"
  names(s)[137] <<- "gagnant_20_30_categorie"
  label(s[[137]]) <<- "gagnant_20_30_categorie: Le répondant estime que son ménage serait gagnant/non affecté/perdant par hausse taxe carbone redistribuée aux 20% ou 30% des plus modestes (+550€ ou +360€ /an/adulte concerné) - Q120"
  names(s)[138] <<- "taxe_20_30_approbation"
  label(s[[138]]) <<- "taxe_20_30_approbation: Le répondant approuverait une hausse de la taxe carbone redistribuée aux 20% ou 30% des plus modestes (+550€ ou +360€/an/adulte concerné) - Q121"
  names(s)[139] <<- "gagnant_30_40_categorie"
  label(s[[139]]) <<- "gagnant_30_40_categorie: Le répondant estime que son ménage serait gagnant/non affecté/perdant par hausse taxe carbone redistribuée aux 30% ou 40% des plus modestes (+360€ ou +270€ /an/adulte concerné) - Q169"
  names(s)[140] <<- "taxe_30_40_approbation"
  label(s[[140]]) <<- "taxe_30_40_approbation: Le répondant approuverait une hausse de la taxe carbone redistribuée aux 30% ou 40% des plus modestes (+360€ ou +270€/an/adulte concerné) - Q124"
  names(s)[141] <<- "gagnant_40_50_categorie"
  label(s[[141]]) <<- "gagnant_40_50_categorie: Le répondant estime que son ménage serait gagnant/non affecté/perdant par hausse taxe carbone redistribuée aux 40% ou 50% des plus modestes (+270€ ou +220€ /an/adulte concerné) - Q170"
  names(s)[142] <<- "taxe_40_50_approbation"
  label(s[[142]]) <<- "taxe_40_50_approbation: Le répondant approuverait une hausse de la taxe carbone redistribuée aux 40% ou 50% des plus modestes (+270€ ou +220€/an/adulte concerné) - Q127"
  names(s)[143] <<- "gagnant_50_70_categorie"
  label(s[[143]]) <<- "gagnant_50_70_categorie: Le répondant estime que son ménage serait gagnant/non affecté/perdant par hausse taxe carbone redistribuée aux 50% des plus modestes (+220€ /an/adulte concerné) - Q171"
  names(s)[144] <<- "taxe_50_70_approbation"
  label(s[[144]]) <<- "taxe_50_70_approbation: Le répondant approuverait une hausse de la taxe carbone redistribuée aux 50% des plus modestes (+220€/an/adulte concerné) - Q130"
  names(s)[145] <<- "gagnant_70__categorie"
  label(s[[145]]) <<- "gagnant_70__categorie: Le répondant estime que son ménage serait gagnant/non affecté/perdant par hausse taxe carbone redistribuée aux 20% ou 30% ou 40% ou 50% des plus modestes (+550€ ou 360€ ou 270€ ou 220€ /an/adulte concerné) - Q172"
  names(s)[146] <<- "taxe_70__approbation"
  label(s[[146]]) <<- "taxe_70__approbation: Le répondant approuverait une hausse de la taxe carbone redistribuée aux 20% ou 30% ou 40% ou 50% des plus modestes (+550€ ou 360€ ou 270€ ou 220€ /an/adulte concerné) - Q133"
  names(s)[147] <<- "si_pauvres"
  label(s[[147]]) <<- "si_pauvres: un versement pour les 50% de Français les plus modestes (ceux gagnant moins de 1670€/mois) - Approbation l'augmentation de la taxe carbone si les recettes étaient utilisées pour financer ... (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) - Q53"
  names(s)[148] <<- "si_compensee"
  label(s[[148]]) <<- "si_compensee: un versement à tous les Français - Approbation l'augmentation de la taxe carbone si les recettes étaient utilisées pour financer ... (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) - Q53"
  names(s)[149] <<- "si_contraints"
  label(s[[149]]) <<- "si_contraints: une compensation pour les ménages contraints dans leur consommation de produits pétroliers - Approbation l'augmentation de la taxe carbone si les recettes étaient utilisées pour financer ... (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) - Q53"
  names(s)[150] <<- "si_baisse_cotsoc"
  label(s[[150]]) <<- "si_baisse_cotsoc: une baisse des cotisations sociales - Approbation l'augmentation de la taxe carbone si les recettes étaient utilisées pour financer ... (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) - Q53"
  names(s)[151] <<- "si_baisse_tva"
  label(s[[151]]) <<- "si_baisse_tva: une baisse de la TVA - Approbation l'augmentation de la taxe carbone si les recettes étaient utilisées pour financer ... (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) - Q53"
  names(s)[152] <<- "si_baisse_deficit"
  label(s[[152]]) <<- "si_baisse_deficit: une baisse du déficit public - Approbation l'augmentation de la taxe carbone si les recettes étaient utilisées pour financer ... (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) - Q53"
  names(s)[153] <<- "si_renovation"
  label(s[[153]]) <<- "si_renovation: la rénovation thermique des bâtiments - Approbation l'augmentation de la taxe carbone si les recettes étaient utilisées pour financer ... (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) - Q53"
  names(s)[154] <<- "si_renouvelables"
  label(s[[154]]) <<- "si_renouvelables: des énergies renouvelables (éoliennes, solaire, etc.) - Approbation l'augmentation de la taxe carbone si les recettes étaient utilisées pour financer ... (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) - Q53"
  names(s)[155] <<- "si_transports"
  label(s[[155]]) <<- "si_transports: des transports non polluants - Approbation l'augmentation de la taxe carbone si les recettes étaient utilisées pour financer ... (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) - Q53"
  names(s)[156] <<- "test_qualite"
  label(s[[156]]) <<- "test_qualite: Merci de sélectionner 'Un peu' (Pas du tout/Un peu/Beaucoup/Complètement/NSP) - Q177"
  names(s)[157] <<- "taxe_kerosene"
  label(s[[157]]) <<- "taxe_kerosene: Le répondant serait favorable (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) à une taxe sur le kérosène (aviation) - Q74"
  names(s)[158] <<- "taxe_viande"
  label(s[[158]]) <<- "taxe_viande: Le répondant serait favorable (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) à une taxe sur la viande rouge - Q74"
  names(s)[159] <<- "normes_isolation"
  label(s[[159]]) <<- "normes_isolation: Le répondant serait favorable (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) à des normes plus strictes sur l'isolation pour les nouveaux bâtiments - Q74"
  names(s)[160] <<- "normes_vehicules"
  label(s[[160]]) <<- "normes_vehicules: Le répondant serait favorable (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) à des normes plus strictes sur la pollution des nouveaux véhicules - Q74"
  names(s)[161] <<- "controle_technique"
  label(s[[161]]) <<- "controle_technique: Le répondant serait favorable (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) à des normes plus strictes sur la pollution lors du contrôle technique - Q74"
  names(s)[162] <<- "interdiction_polluants"
  label(s[[162]]) <<- "interdiction_polluants: Le répondant serait favorable (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) à l'interdiction des véhicules polluants dans les centre-villes - Q74"
  names(s)[163] <<- "peages_urbains"
  label(s[[163]]) <<- "peages_urbains: Le répondant serait favorable (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) à l'instauration de péages urbains - Q74"
  names(s)[164] <<- "fonds_mondial"
  label(s[[164]]) <<- "fonds_mondial: Le répondant serait favorable (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) à une contribution pour un fond mondial pour le climat - Q74"
  names(s)[165] <<- "rattrapage_diesel"
  label(s[[165]]) <<- "rattrapage_diesel: Le répondant serait favorable (Oui tout à fait/Oui plutôt/Indifférent ou NSP/Non pas vraiment/Non pas du tout) au rattrapage de la fiscalité du diesel sur celle de l'essence (on explique que pour des raisons historiques le diesel est moins taxé) - Q140"
  names(s)[166] <<- "parle_CC"
  label(s[[166]]) <<- "parle_CC: Fréquence à laquelle le répondant parle du changement climatique (Plusieurs fois par mois / par an / Presque jamais / NSP) - Q60"
  names(s)[167] <<- "cause_CC"
  label(s[[167]]) <<- "cause_CC: Cause principale du changement climatique selon le répondant (N'est pas une réalité / Causes naturelles / Activité humaine / NSP) - Q1"
  names(s)[168] <<- "ges_CO2" # TODO: majuscules (tva aussi)
  label(s[[168]]) <<- "ges_CO2: Le répondant pense que le CO2 participe au réchauffement climatique - Q48"
  names(s)[169] <<- "ges_CH4"
  label(s[[169]]) <<- "ges_CH4: Le répondant pense que le CH4 participe au réchauffement climatique - Q48"
  names(s)[170] <<- "ges_O2"
  label(s[[170]]) <<- "ges_O2: Le répondant pense que l'oxygène participe au réchauffement climatique - Q48"
  names(s)[171] <<- "ges_pm"
  label(s[[171]]) <<- "ges_pm: Le répondant pense que les particules fines participent au réchauffement climatique - Q48"
  names(s)[172] <<- "ges_boeuf"
  label(s[[172]]) <<- "ges_boeuf: Le répondant pense que la consommation d'un steak haché de boeuf émet environ 20 fois plus de GES que deux portions de pâtes - Q179"
  names(s)[173] <<- "ges_nucleaire"
  label(s[[173]]) <<- "ges_nucleaire: Le répondant pense que l'électricité produite au nucléaire émet eviron 20 fois plus de GES que celle produite par les éoliennes - Q179"
  names(s)[174] <<- "ges_avion"
  label(s[[174]]) <<- "ges_avion: Le répondant pense qu'une place dans un trajet Bordeaux - Nice émet environ20 fois plus de GES en avion qu'en train - Q179"
  names(s)[175] <<- "effets_CC"
  label(s[[175]]) <<- "effets_CC: Le répondant pense qu'en l'absence de mesures ambitieuse, les effets du changement climatiques seraient (Insignifiants / Faibles / Graves / Désastreux / Cataclysmiques / NSP) - Q5"
  names(s)[176] <<- "region_CC"
  label(s[[176]]) <<- "region_CC: Le répondant pense que le changement climatique aura les plus grandes conséquences en Inde/Europe/autant les deux - Q181"
  names(s)[177] <<- "generation_CC_1960"
  label(s[[177]]) <<- "generation_CC_1960: Le répondant estime qu'en France les générations nés dans les années 1960 seront gravement affectées par le changeent climatique - Q71"
  names(s)[178] <<- "generation_CC_1990"
  label(s[[178]]) <<- "generation_CC_1990: Le répondant estime qu'en France les générations nés dans les années 1990 seront gravement affectées par le changeent climatique - Q71"
  names(s)[179] <<- "generation_CC_2020"
  label(s[[179]]) <<- "generation_CC_2020: Le répondant estime qu'en France les générations nés dans les années 2020 seront gravement affectées par le changeent climatique - Q71"
  names(s)[180] <<- "generation_CC_2050"
  label(s[[180]]) <<- "generation_CC_2050: Le répondant estime qu'en France les générations nés dans les années 2050 seront gravement affectées par le changeent climatique - Q71"
  names(s)[181] <<- "generation_CC_aucune"
  label(s[[181]]) <<- "generation_CC_aucune: Le répondant estime qu'en France les générations nées dans les années 1960, 1990, 2020 et 2050 ne seront pas gravement affectées par le changement climatique - Q71"
  names(s)[182] <<- "responsable_CC_chacun"
  label(s[[182]]) <<- "responsable_CC_chacun: Le répondant estime que chacun d'entre nous est responsabe du changement climatique - Q6"
  names(s)[183] <<- "responsable_CC_riches"
  label(s[[183]]) <<- "responsable_CC_riches: Le répondant estime que les plus riches sont responsables du changement climatique - Q6"
  names(s)[184] <<- "responsable_CC_govts"
  label(s[[184]]) <<- "responsable_CC_govts: Le répondant estime que les gouvernements sont responsables du changement climatique - Q6"
  names(s)[185] <<- "responsable_CC_etranger"
  label(s[[185]]) <<- "responsable_CC_etranger: Le répondant estime que certains pays étrangers sont responsables du changement climatique - Q6"
  names(s)[186] <<- "responsable_CC_passe"
  label(s[[186]]) <<- "responsable_CC_passe: Le répondant estime que les générations passées sont responsables du changement climatique - Q6"
  names(s)[187] <<- "responsable_CC_nature"
  label(s[[187]]) <<- "responsable_CC_nature: Le répondant estime que des causes naturelles sont responsables du changement climatique - Q6"
  names(s)[188] <<- "emission_cible"
  label(s[[188]]) <<- "emission_cible: Le répondant estime que les émissions de CO2 en France (actuellement à 10t/pers./an) devraient être ramenées à Xt/pers/an pour contenir le réchauffement climatique à +2 degrés si tous les pays faisaient de même - Q47"
  names(s)[189] <<- "enfant_CC"
  label(s[[189]]) <<- "enfant_CC: Le changement climatique a, a eu ou aura une influence dans la décision du répondant de faire un enfant (Oui/Non/NSP) - Q58"
  names(s)[190] <<- "enfant_CC_pour_lui"
  label(s[[190]]) <<- "enfant_CC_pour_lui: Le changement climatique a, a eu ou aura une influence dans la décision du répondant de faire un enfant parce qu'il ne veut pas que l'enfant vive dans un monde dévasté - Q59"
  names(s)[191] <<- "enfant_CC_pour_CC"
  label(s[[191]]) <<- "enfant_CC_pour_CC: Le changement climatique a, a eu ou aura une influence dans la décision du répondant de faire un enfant parce qu'il ne veut pas que son enfant aggrave le changement climatique - Q59"
  names(s)[192] <<- "changer_si_politiques"
  label(s[[192]]) <<- "changer_si_politiques: Le répondant serait prêt à changer son mode de vie pour lutter contre le changement climatique si les politiques allaient dans ce sens - Q76"
  names(s)[193] <<- "changer_si_moyens"
  label(s[[193]]) <<- "changer_si_moyens: Le répondant serait prêt à changer son mode de vie pour lutter contre le changement climatique s'il en avait les moyens financiers - Q76"
  names(s)[194] <<- "changer_si_tous"
  label(s[[194]]) <<- "changer_si_tous: Le répondant serait prêt à changer son mode de vie pour lutter contre le changement climatique si tout le monde en faisait autant - Q76"
  names(s)[195] <<- "changer_non_riches"
  label(s[[195]]) <<- "changer_non_riches: Le répondant ne serait pas prêt à changer son mode de vie pour lutter contre le changement climatique car seuls les plus riches doivent changer leur mode de vie - Q76"
  names(s)[196] <<- "changer_non_interet"
  label(s[[196]]) <<- "changer_non_interet: Le répondant ne serait pas prêt à changer son mode de vie pour lutter contre le changement climatique car s'oppose à son intérêt personnel - Q76"
  names(s)[197] <<- "changer_non_negation"
  label(s[[197]]) <<- "changer_non_negation: Le répondant ne serait pas prêt à changer son mode de vie pour lutter contre le changement climatique car il pense que le changement climatique n'est pas un vrai problème - Q76"
  names(s)[198] <<- "changer_deja_fait"
  label(s[[198]]) <<- "changer_deja_fait: Le répondant ne serait pas prêt à changer son mode de vie pour lutter contre le changement climatique car il a déjà adopté un mode de vie soutenable - Q76"
  names(s)[199] <<- "changer_essaie"
  label(s[[199]]) <<- "changer_essaie: Le répondant essaie de changer son mode de vie pour lutter contre le changement climatique mais a du mal à changer ses habitudes - Q76"
  names(s)[200] <<- "mode_vie_ecolo"
  label(s[[200]]) <<- "mode_vie_ecolo: Dans l'hypothèse où tous les Etats du monde se mettraient d'accord pour lutter contre le changement climatique, mettraient à contribution les plus riches, et si la France investissait dans les transports non polluants, le répondant serait prêt (Oui/Non/NSP) à adopter un mode de vie écologique - Q51"
  names(s)[201] <<- "fume"
  label(s[[201]]) <<- "fume: Le répondant fume régulièrement (Oui/Non) - Q191"
  names(s)[202] <<- "schiste_approbation"
  label(s[[202]]) <<- "schiste_approbation: Après avoir été informé des avantages du gaz de schiste vis-à-vis de la réduction des émissions, de son effet négatif sur la qualité de l'eau à l'échelle locale, et des potentiels d'exploitation dans son département, le répondant serait favorable (Oui/Non/NSP) à son exploitation - Q197"
  names(s)[203] <<- "schiste_avantage"
  label(s[[203]]) <<- "schiste_avantage: Le répondant considère que le principal avantage de l'exploitation du gaz de schiste serait la lutte contre le changement climatique / la création d'emploi dans les départements concernés / aucune de ces deux raisons - Q199"
  names(s)[204] <<- "schiste_CC"
  label(s[[204]]) <<- "schiste_cc: Le répondant estime que l'idée que le gaz de schiste permettrait de lutter contre le changement climatique est valable / malvenue / il ne sait pas - Q198"
  names(s)[205] <<- "transports_distance_choix"
  label(s[[205]]) <<- "transports_distance_choix: Le répondant peut estimer la distance de l'arrêt de transport en commun le plus proche de chez lui en minutes de marche, ou il ne sait pas - Q42"
  names(s)[206] <<- "transports_distance"
  label(s[[206]]) <<- "transports_distance: L'arrêt de transport en commun le plus proche de chez le répondant est à X minutes de marche - Q42"
  names(s)[207] <<- "transports_frequence"
  label(s[[207]]) <<- "transports_frequence: Le moyen de transports en commun le plus proche de chez le répondant passe en moyenne moins de trois fois par jours / entre quatre fois par jour et une fois par heure / une ou deux fois par heure / plus que trois fois par heure / NSP - Q43"
  names(s)[208] <<- "transports_avis"
  label(s[[208]]) <<- "ransports_avis: Le répondant estime l'offre de transports en commun là où il habite comme étant satisfaisante / convenable / limitée / insuffisante / NSP - Q41"
  names(s)[209] <<- "transports_travail"
  label(s[[209]]) <<- "transports_travail: Le répondant utilise principalement (la voiture/les TC/la marche ou le vélo/un deux roues motorisé/le covoiturage/non conerné) pour ses trajets domiciles-travail (ou études) - Q39"
  names(s)[210] <<- "transports_courses"
  label(s[[210]]) <<- "transports_courses: Le répondant utilise principalement (la voiture/les TC/la marche ou le vélo/un deux roues motorisé/le covoiturage/non conerné) pour faire ses courses - Q39"
  names(s)[211] <<- "transports_loisirs"
  label(s[[211]]) <<- "transports_loisirs~ Le répondant utilise principalement (la voiture/les TC/la marche ou le vélo/un deux roues motorisé/le covoiturage/non conerné) pour ses loisirs (hors vacances) - Q39"
  names(s)[212] <<- "transports_travail_commun"
  label(s[[212]]) <<- "transports_travail_commun: Sans changer de logement ni de lieu de travail, il serait possible pour le répondant prenant sa voiture (Non/Oui mais ça l'embetterait/Oui ça ne lui poserait pas de grande difficulté/NSP) de prendre les transports en commun pour ses trajets domicile-travail - Q40"
  names(s)[213] <<- "transports_travail_actif"
  label(s[[213]]) <<- "transports_travail_actif: Sans changer de logement ni de lieu de travail, il serait possible pour le répondant prenant sa voiture (Non/Oui mais ça l'embetterait/Oui ça ne lui poserait pas de grande difficulté/NSP) d'effectuer ses trajets domicile-travail en marchant ou en vélo - Q40"
  names(s)[214] <<- "interet_politique"
  label(s[[214]]) <<- "interet_politique: Le répondant est intéressé par la politique (Presque pas/Un peu/Beaucoup) - Q32"
  names(s)[215] <<- "extr_gauche"
  label(s[[215]]) <<- "extr_gauche: Le répondant se considère comme étant d'extrême gauche - Q34"
  names(s)[216] <<- "gauche"
  label(s[[216]]) <<- "gauche: Le répondant se considère comme étant de gauche - Q34"
  names(s)[217] <<- "centre"
  label(s[[217]]) <<- "centre: Le répondant se considère comme étant du centre - Q34"
  names(s)[218] <<- "droite"
  label(s[[218]]) <<- "droite: Le répondant se considère comme étant de droite - Q34"
  names(s)[219] <<- "extr_droite"
  label(s[[219]]) <<- "extr_droite: Le répondant se considère comme étant d'extrême droite - Q34"
  names(s)[220] <<- "conservateur"
  label(s[[220]]) <<- "conservateur: Le répondant se considère comme étant conservateur - Q34"
  names(s)[221] <<- "liberal"
  label(s[[221]]) <<- "liberal: Le répondant se considère comme étant libéral - Q34"
  names(s)[222] <<- "humaniste"
  label(s[[222]]) <<- "humaniste: Le répondant se considère comme étant humaniste - Q34"
  names(s)[223] <<- "patriote"
  label(s[[223]]) <<- "patriote: Le répondant se considère comme étant patriote - Q34"
  names(s)[224] <<- "apolitique"
  label(s[[224]]) <<- "apolitique: Le répondant se considère comme étant apolitique - Q34"
  names(s)[225] <<- "ecologiste"
  label(s[[225]]) <<- "ecologiste: Le répondant se considère comme étant écologiste - Q34"
  names(s)[226] <<- "actualite"
  label(s[[226]]) <<- "actualite: Le répondant se tient principalement informé de l'actualité via la télévision / la presse (écrite ou en ligne) / les réseaux sociaux / la radio - Q182"
  names(s)[227] <<- "gilets_jaunes_dedans"
  label(s[[227]]) <<- "gilets_jaunes_dedans: Le répondant déclare faire partie des gilets jaunes - Q35"
  names(s)[228] <<- "gilets_jaunes_soutien"
  label(s[[228]]) <<- "gilets_jaunes_soutien: Le répondant soutient les gilets jaunes - Q35"
  names(s)[229] <<- "gilets_jaunes_compris"
  label(s[[229]]) <<- "gilets_jaunes_compris: Le répondant comprend les gilets jaunes - Q35"
  names(s)[230] <<- "gilets_jaunes_oppose"
  label(s[[230]]) <<- "gilets_jaunes_oppose: Le répondant est opposé aux gilets jaunes - Q35"
  names(s)[231] <<- "gilets_jaunes_NSP"
  label(s[[231]]) <<- "gilets_jaunes_NSP: Le répondant ne sait pas s'il fait partie / s'il soutient / s'il comprend / s'il s'oppose aux gilets jaunes - Q35"
  names(s)[232] <<- "transferts_inter_a"
  label(s[[232]]) <<- "transferts_inter_a: ~ Transferts internationaux - approbation (Approuveriez-vous le transfert de 5% des revenus des pays riches aux pays pauvres ?: Oui/Non/NSP) - Q47"
  names(s)[233] <<- "transferts_inter_a_info"
  label(s[[233]]) <<- "transferts_inter_a_info: ~ Transferts internationaux avec information sur Aide Publique au Développement (0.3% PIB) - approbation du transfert de 5% des revenus des pays riches aux pays pauvres ? - Q200"
  names(s)[234] <<- "premier_clic_depenses"
  label(s[[234]]) <<- ""
  names(s)[235] <<- "dernier_clic_depenses"
  label(s[[235]]) <<- ""
  names(s)[236] <<- "duree_depenses"
  label(s[[236]]) <<- ""
  names(s)[237] <<- "nombre_clics_depenses"
  label(s[[237]]) <<- ""
  names(s)[238] <<- "depenses_confiant"
  label(s[[238]]) <<- "depenses_confiant: À quel point êtes-vous confiant.e dans le fait que l'évolution des dépenses publiques que vous venez de proposer serait souhaitable ? Assez/Pas vraiment confiant.e/NSP - Q198"
  names(s)[239] <<- "compris_depenses"
  label(s[[239]]) <<- "compris_depenses: Avez-vous compris le graphique et les curseurs interactifs [concernant les dépenses publiques] ? Oui/Non/Bug - Q200"
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
  label(s[[273]]) <<- "ID"
  names(s)[274] <<- "duree"
  label(s[[274]]) <<- "duree: Durée de complétion du questionnaire (pn secondes)"
  names(s)[275] <<- "exclu"
  label(s[[275]]) <<- "exclu: Vide si tout est ok (Screened/QuotaMet sinon)"
  names(s)[276] <<- "taille_agglo"
  label(s[[276]]) <<- "taille_agglo: Taille d'agglomération: [1;5]=rural/-20k/20-100k/+100k/Région parisienne - embedded data"
  names(s)[277] <<- "region"
  label(s[[277]]) <<- "region: Région calculée à partir du code postal: 9 nouvelles régions de l'hexagone + autre (ARA/Est/Ouest/Centre/Nord/IDF/SO/Occ/PACA/autre)"
  names(s)[278] <<- "schiste_traite"
  label(s[[278]]) <<- "schiste_traite: Département du répondant potentiellement concerné par l'exploitation du gaz de schiste - Q197 embedded data"
  names(s)[279] <<- "gaz"
  label(s[[279]]) <<- "gaz: Indicatrice que chauffage = 'Gaz de ville' ou 'Butane, propane, gaz en citerne'"
  names(s)[280] <<- "fioul"
  label(s[[280]]) <<- "fioul: Indicatrice que chauffage = 'Fioul, mazout, pétrole'"
  names(s)[281] <<- "nb_vehicules"
  label(s[[281]]) <<- "nb_vehicules: Nombre de véhicules motorisés dont dispose le ménage - Q37"
  names(s)[282] <<- "hausse_depenses"
  label(s[[282]]) <<- "hausse_depenses: Hausse des dépenses énergétiques simulées pour le ménage, suite à la taxe (élasticité de 0.4/0.2 pour carburants/chauffage)"
  names(s)[283] <<- "simule_gagnant"
  label(s[[283]]) <<- "simule_gagnant: Indicatrice sur la prédiction que le ménage serait gagnant avec la taxe compensée, d'après nos simulations"
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
  label(s[[298]]) <<- "info_CC: Indicatrice aléatoire que l'information sur le changement climatique a été présentée (150k morts/an monde, tendance +5/+8°C en 2100/2250, +2°C techniquement possible, sinon extinctions, intensification catastrophes, +270 M subissant inondations en 2100, + de conflits et migrations)"
  names(s)[299] <<- "info_PM"
  label(s[[299]]) <<- "info_PM: Indicatrice aléatoire que l'information sur les particules fines a été présentée (48k morts/an en France, - 9 mois d'espérance vie, réduire conso fuel -> réduire pbs particules fines"
  names(s)[300] <<- "variante_monetaire"
  label(s[[300]]) <<- "variante_monetaire: Indicatrice aléatoire que les questions sur les perdant_/gagnant_ de la taxe portent sur le pouvoir d'achat ou non (0: quels seraient les gagnants-perdant/1: quelles catégories gagneraient-perdraient en pouvoir d'achat)"
  names(s)[301] <<- "cible20"
  label(s[[301]]) <<- "cible20: Indicatrice aléatoire que la réforme ciblée compense les 20% les plus modestes (20/30/40/50)"
  names(s)[302] <<- "cible30"
  label(s[[302]]) <<- "cible30: Indicatrice aléatoire que la réforme ciblée compense les 30% les plus modestes (20/30/40/50)"  
  names(s)[303] <<- "cible40"
  label(s[[303]]) <<- "cible40: Indicatrice aléatoire que la réforme ciblée compense les 40% les plus modestes (20/30/40/50)"
  names(s)[304] <<- "cible50"
  label(s[[304]]) <<- "cible50: Indicatrice aléatoire que la réforme ciblée compense les 50% les plus modestes (20/30/40/50)"
  names(s)[305] <<- "progressivite_feedback_avec_info"
  label(s[[305]]) <<- "progressivite_feedback_avec_info: ~ Une hausse de la taxe carbone compensée avantagerait les plus modestes - après information sur la progressivité - Q208"
  names(s)[306] <<- "progressivite_feedback_sans_info"
  label(s[[306]]) <<- "progressivite_feedback_sans_info: ~ Une hausse de la taxe carbone compensée avantagerait les plus modestes - sans information sur la progressivité - Q207"
  names(s)[307] <<- "progressivite_progressif"
  label(s[[307]]) <<- "progressivite_progressif: Une hausse de la taxe carbone compensée avantagerait les plus modestes - après information sur la progressivité - Q206"
  names(s)[308] <<- "apres_modifs"
  label(s[[308]]) <<- "apres_modifs: Indicatrice de la seconde moitié de l'échantillon, avec une info sur l'efficacité contre la pollution et des questions et informations sur la progressivité (progressivite_feedback_avec/sans_info, progressivite_progressif), et une reformulation des questions transferts_inter (aide publique au développement des Français)" # Les scientifiques s'accordent à dire qu'une taxe carbone serait efficace pour diminuer la pollution.
  names(s)[309] <<- "aide_2p" # à partir de 03/03, 10h45 (Est Coast)
  label(s[[309]]) <<- "aide_2p: Indicatrice du troisième tiers de l'échantillon, où la question sur l'aide publique au développement propose un montant de 2% du revenu des Français (au lieu de 5%)"
  names(s)[310] <<- "aide_non_autonomie_ni"
  label(s[[310]]) <<- "aide_non_autonomie_ni: * Les pays pauvres s'en sortiront mieux par eux-mêmes qu'avec notre aide - raison de ne pas approuver transferts_inter (à 2%: aide_2p=1) - info_transferts_inter=F, Q209"
  names(s)[311] <<- "aide_non_priorite_ni"
  label(s[[311]]) <<- "aide_non_priorite_ni: * La dépense publique doit servir en priorité aux services publics et aux Français - raison de ne pas approuver transferts_inter (à 2%: aide_2p=1) - info_transferts_inter=F, Q209"
  names(s)[312] <<- "aide_non_etats_ni"
  label(s[[312]]) <<- "aide_non_etats_ni: * Je serais favorable si l'aide allait directement aux plus pauvres, et pas aux États - raison de ne pas approuver transferts_inter (à 2%: aide_2p=1) - info_transferts_inter=F, Q209"
  names(s)[313] <<- "aide_non_global_ni"
  label(s[[313]]) <<- "aide_non_global_ni: * Je serais favorable si tous les pays riches contribuaient autant que la France - raison de ne pas approuver transferts_inter (à 2%: aide_2p=1) - info_transferts_inter=F, Q209"
  names(s)[314] <<- "aide_non_trop_ni"
  label(s[[314]]) <<- "aide_non_trop_ni: * Je serais favorable avec un montant plus faible : 2% c'est trop - raison de ne pas approuver transferts_inter (à 2%: aide_2p=1) - info_transferts_inter=F, Q209"
  names(s)[315] <<- "aide_non_autonomie_i"
  label(s[[315]]) <<- "aide_non_autonomie_i: * Les pays pauvres s'en sortiront mieux par eux-mêmes qu'avec notre aide - raison de ne pas approuver transferts_inter (à 2%: aide_2p=1) - info_transferts_inter=T, Q210"
  names(s)[316] <<- "aide_non_priorite_i"
  label(s[[316]]) <<- "aide_non_priorite_i: * La dépense publique doit servir en priorité aux services publics et aux Français - raison de ne pas approuver transferts_inter (à 2%: aide_2p=1) - info_transferts_inter=T, Q210"
  names(s)[317] <<- "aide_non_etats_i"
  label(s[[317]]) <<- "aide_non_etats_i: * Je serais favorable si l'aide allait directement aux plus pauvres, et pas aux États - raison de ne pas approuver transferts_inter (à 2%: aide_2p=1) - info_transferts_inter=T, Q210"
  names(s)[318] <<- "aide_non_global_i"
  label(s[[318]]) <<- "aide_non_global_i: * Je serais favorable si tous les pays riches contribuaient autant que la France - raison de ne pas approuver transferts_inter (à 2%: aide_2p=1) - info_transferts_inter=T, Q210"
  names(s)[319] <<- "aide_non_trop_i"
  label(s[[319]]) <<- "aide_non_trop_i: * Je serais favorable avec un montant plus faible : 2% c'est trop - raison de ne pas approuver transferts_inter (à 2%: aide_2p=1) - info_transferts_inter=T, Q210"

  s <<- s[,c(1,2,7,20:319)]
}

convert_s <- function() {
  # lab <- label(s$csp)
  # s$csp <<- factor(s$csp, levels=c(levels(s$csp), "Cadres", "Indépendants", "Ouvriers", 'Inactifs', "Professions intermédiaires", "Retraités", "Employés", "Agriculteurs"))
  # s$csp <<- as.character(s$csp)
  s$csp[grepl("cadre",s$csp)] <<- "Cadre"
  s$csp[grepl("Artisan",s$csp)] <<- "Indépendant"
  s$csp[grepl("iaire",s$csp)] <<- "Intermédiaire"
  s$csp[grepl("etrait",s$csp)] <<- "Retraité"
  s$csp[grepl("Employ",s$csp)] <<- "Employé"
  s$csp[grepl("Agricul",s$csp)] <<- "Agriculteur"
  s$csp[grepl("Ouvrier",s$csp)] <<- "Ouvrier"
  s$csp[grepl("Inactif",s$csp)] <<- "Inactif"
  # label(s$csp) <<- lab
  # s$csp <<- as.factor(s$csp)
  
  for (i in 1:length(s)) {
    # levels(s[[i]]) <<- c(levels(s[[i]]), "NSP")
    s[[i]][s[[i]] == "NSP (Ne sais pas, ne se prononce pas)"] <<- "NSP"
    s[[i]][s[[i]] == "NSP (Ne sait pas, ne se prononce pas)"] <<- "NSP"
    s[[i]][s[[i]] == "NSP (Ne sais pas, ne se prononce pas)."] <<- "NSP"
    s[[i]][s[[i]] == "NSP (Ne sait pas, ne se prononce pas)."] <<- "NSP"
    s[[i]][s[[i]] == "NSP (Ne sais pas, ne souhaite pas répondre)"] <<- "NSP"
    s[[i]][s[[i]] == "NSP (Ne sait pas, ne veut pas répondre)"] <<- "NSP"
    s[[i]][s[[i]] == "NSP (Ne veut pas répondre)"] <<- "NSP"
  }
  
  s$variante_transferts_inter[!is.na(s$transferts_inter_a) | !is.na(s$transferts_inter_a_info)] <<- "a"
  s$transferts_inter <<- NA
  s$transferts_inter_info[!is.na(s$transferts_inter_a_info)] <<- TRUE
  s$transferts_inter_info[!is.na(s$transferts_inter_a)] <<- FALSE
  s$transferts_inter[!is.na(s$transferts_inter_a_info)] <<- s$transferts_inter_a_info[!is.na(s$transferts_inter_a_info)]
  s$transferts_inter[!is.na(s$transferts_inter_a)] <<- s$transferts_inter_a[!is.na(s$transferts_inter_a)]
  label(s$variante_transferts_inter) <<- "variante_transferts_inter: Variante dans la formulation de transferts_inter; s/i/c/a: simple/intermédiaire/complète/approbation: (argument pauvreté,esponsabilité climatique des pays riches)/complète (i+ argument colonisation, esclavage)/pour ou contre un transfert de 5% des revenus des pays riches vers les pays pauvres (seulement en vague 2); Vague 1: curseur/NSP, Vague 2: champ de saisie (quelques données manquantes) - Q5,Q6,Q78"
  label(s$transferts_inter) <<- "transferts_inter: Approbation d'un transfert de 5% des revenus des pays riches vers les pays pauvres"
  # s$transferts_inter <<- as.item(as.numeric(s$transferts_inter), missing.values=-1, annotation="transferts_inter: Transferts internationaux, variantes (simple) avec curseur 0-20% (s) ou champ (Quelle % des revenus des pays riches devrait être transférée aux pays pauvres ?) - Q73,91")

  s$mauvaise_qualite <<- 0 # 99% if we exclude those from revenu, 92% otherwise
  s$mauvaise_qualite[n(s$revenu) > n(s$rev_tot)] <<- 1 + s$mauvaise_qualite[n(s$revenu) > n(s$rev_tot)] # 164
  s$mauvaise_qualite[n(s$revenu) > 10000] <<- 1 + s$mauvaise_qualite[n(s$revenu) > 10000] # 58
  s$mauvaise_qualite[n(s$rev_tot) > 10000] <<- 1 + s$mauvaise_qualite[n(s$rev_tot) > 10000] # 55
  s$revenu <<- clean_number(s$revenu, high_numbers='divide')
  s$rev_tot <<- clean_number(s$rev_tot, high_numbers='divide')
  for (i in c( # TODO: check number outliers 
     "revenu", "rev_tot", "taille_menage", "nb_adultes", "nb_14_et_plus", "duree", "variation_aide", "depense_aide", "km_0", "km_1", "km_2", "conso_1", "conso_2", "surface", "emission_cible",
      "depense_totale", "depense_sante", "depense_retraites", "depense_protection", "depense_education", "depense_recherche", "depense_loisirs", 
     "depense_infrastructures", "depense_justice", "depense_armee", "depense_securite", "depense_aide", "variation_totale", "variation_aide",
     "variation_sante", "variation_retraites", "variation_protection", "recette_totale", "variation_recette", "budget_equilibre", "regle_or",
     "variation_education", "variation_recherche", "variation_loisirs", "variation_infrastructures", "variation_justice", "variation_armee", "variation_securite",
     "transports_distance", "duree_info_CC", "duree_info_CC_PM", "duree_info_PM", "duree_no_info", "duree_depenses", "duree_champ_libre", 
     "hausse_chauffage", "hausse_depenses", "hausse_diesel", "hausse_essence", "nb_vehicules", "en_position_0", "en_position_1", "en_position_2", "en_position_3", 
     "en_position_4", "en_position_5", "en_position_6", "en_position_7", "en_position_8", "en_position_9", "en_position_10"
              )) {
    lab <- label(s[[i]])
    s[[i]] <<- as.numeric(as.vector(s[[i]]))
    label(s[[i]]) <<- lab
  }
  
  s$mauvaise_qualite[s$taille_menage < s$nb_adultes | s$taille_menage < s$nb_14_et_plus] <<- 1.3 + s$mauvaise_qualite[s$taille_menage < s$nb_adultes | s$taille_menage < s$nb_14_et_plus] # 15
  s$mauvaise_qualite[s$taille_menage > 12] <<- 1.3 + s$mauvaise_qualite[s$taille_menage > 12] # 10
  s$mauvaise_qualite[s$nb_14_et_plus > 10] <<- 1 + s$mauvaise_qualite[s$nb_14_et_plus > 10] # 2
  s$mauvaise_qualite[s$km > 10^6] <<- 1 + s$mauvaise_qualite[s$km > 10^6] # 1
  s$mauvaise_qualite[s$surface < 9] <<- 1 + s$mauvaise_qualite[s$surface < 9] # 6
  s$mauvaise_qualite[s$surface >= 1000] <<- 1 + s$mauvaise_qualite[s$surface >= 1000] # 4
  label(s$mauvaise_qualite) <<- "mauvaise_qualite: Indicatrice d'une réponse aberrante à revenu, taille_menage, nb_14_et_plus, km ou surface."
  s$duree_info_courte[n(s$info_CC) + n(s$info_PM) > 0] <<- FALSE # 15%
  s$duree_info_courte[s$duree_info_CC < 5 | s$duree_info_PM < 5 | s$duree_info_CC_PM < 5] <<- T # 327
  label(s$duree_info_courte) <<- "duree_info_courte: Indicatrice d'un temps de lecture inférieur à 5 secondes pour les informations du début."

  for (j in c("taxe_efficace", "rattrapage_diesel", "enfant_CC", "mode_vie_ecolo", "schiste_approbation", 
              "transferts_inter_a", "transferts_inter_a_info", "transferts_inter", "taxe_approbation",
              "taxe_feedback_approbation", "taxe_progressif_approbation", "taxe__20_approbation", "taxe_20_30_approbation",
              "taxe_30_40_approbation", "taxe_40_50_approbation", "taxe_50_70_approbation", "taxe_70__approbation",
              "progressivite_feedback_sans_info", "progressivite_feedback_avec_info", "progressivite_progressif"
              )) {
    s[j][[1]] <<- as.item(as.character(s[j][[1]]),
                labels = structure(c("","Non","NSP","Oui"), names = c("NA","Non","NSP","Oui")), 
                missing.values = c("","NSP"), annotation=attr(s[j][[1]], "label"))
  }
  
  for (j in c("mode_chauffage", "chauffage", "parle_CC", "cause_CC", "effets_CC", "transports_frequence",
              "schiste_CC", "transports_avis", "transports_travail_actif", "transports_travail_commun"
              # "perte_relative_tva", "perte_relative_fuel", "perte_relative_chauffage", "interet_politique",
              )) {
    if (j %in% c("mode_chauffage", "chauffage", "schiste_CC", "cause_CC")) s[capitalize(j)] <<- s[j][[1]]
    # s[j][[1]] <<- as.item(as.character(s[j][[1]]),
    #             labels = structure(levels(factor(s[j][[1]])), names = levels(factor(s[j][[1]]))),
    #             missing.values = c("NSP", ""), annotation=paste(attr(s[j][[1]], "label"), "(char)")) # TODO: pb with numbers=T
    s[j][[1]] <<- as.item(as.factor(s[j][[1]]),
                          missing.values = c("NSP", ""), annotation=paste(attr(s[j][[1]], "label"), "(char)")) # TODO: pb with numbers=T
  }
 # TODO: as.item region_CC, gagnant_fuel_categorie, gagnant_chauffage_categorie, gagnant_categorie, gagnant_feedback_categorie, gagnant_progressif_categorie, gagnant_cible_categorie?  

  for (j in names(s)) {
    if (j!="peages_urbains" & grepl('_perdant_|_gagnant_|benefices_|problemes_|ges_|responsable_|generation_CC|enfant_CC_pour|changer_|gilets_jaunes_|apres_modif|aide_non_|aide_2p|ecologiste|conservateur|liberal|patriote|humaniste|apolitique', j)) {
      s[[j]][s[[j]]!=""] <<- TRUE
      s[[j]][is.na(s[[j]])] <<- FALSE
    }
  }

  for (k in c("perte_relative_tva", "perte_relative_fuel", "perte_relative_chauffage")) {
    temp <-  2 * (s[[k]]=="Oui, beaucoup plus") + (s[[k]]=="Oui, un peu plus") - (s[[k]]=="Non, un peu moins") - 2 * (s[[k]]=="Non, beaucoup moins")
    s[[k]] <<- as.item(temp, labels = structure(c(-2:2),
                          names = c("Beaucoup moins","Un peu moins","= Moyenne","Un peu plus","Beaucoup plus")),
                          # names = c("Non, beaucoup moins","Non, un peu moins","Autant que la moyenne","Oui, un peu plus","Oui, beaucoup plus")),
                        annotation=Label(s[[k]]))
  }

  for (k in c(132:140,142:149)) {
    temp <-  2 * (s[k][[1]]=="Oui, tout à fait") + (s[k][[1]]=="Oui, plutôt") - (s[k][[1]]=="Non, pas vraiment") - 2 * (s[k][[1]]=="Non, pas du tout")
    s[k][[1]] <<- as.item(temp, labels = structure(c(-2:2),
                          names = c("Pas du tout","Pas vraiment","Indifférent/NSP","Plutôt","Tout à fait")),
                          # names = c("Non, pas du tout","Non, pas vraiment","Indifférent ou Ne sais pas","Oui, plutôt","Oui, tout à fait")),
                        annotation=Label(s[k][[1]]))
  }

  temp <- (s$parle_CC=='Plusieurs fois par an') + 2*(s$parle_CC=='Plusieurs fois par mois') - (s$parle_CC=="NSP")
  s$parle_CC <<- as.item(temp, labels = structure(c(-1:2),
                          names = c("NSP","Presque jamais","Plusieurs fois par an","Plusieurs fois par mois")),
                        missing.values = -1, annotation=Label(s$parle_CC))

  temp <- grepl("Faibles", s$effets_CC) + 2*grepl("Graves", s$effets_CC) + 3*grepl("Désastreux", s$effets_CC) + 4*grepl("Cataclysmiques", s$effets_CC) - (s$effets_CC=="NSP")
  s$effets_CC <<- as.item(temp, labels = structure(c(-1:4),
                          names = c("NSP","Insignifiants","Faibles","Graves","Désastreux","Cataclysmiques")),
                          # names = c("NSP","Insignifiants, voire bénéfiques","Faibles, car les humains sauraient vivre avec","Graves, car il y aurait plus de catastrophes naturelles","Désastreux, les modes de vie seraient largement altérés","Cataclysmiques, l'humanité disparaîtrait")),
                        missing.values = -1, annotation=Label(s$effets_CC))

  temp <- (s$transports_frequence=="Entre quatre fois par jour et une fois par heure") + 2*(s$transports_frequence=="Une ou deux fois par heure") + 3*(s$transports_frequence=="Plus que trois fois par heure") - (s$transports_frequence=="NSP")
  s$transports_frequence <<- as.item(temp, labels = structure(c(-1:3),
                          names = c("NSP","< 3/jour","1/h - 4/jour","1/h - 2/h","> 3/heure")),
                          # names = c("NSP","Moins de trois fois par jour","Entre quatre fois par jour et une fois par heure","Une ou deux fois par heure","Plus que trois fois par heure")),
                        missing.values = -1, annotation=Label(s$transports_frequence))

  temp <- (s$transports_avis=="Limitée, mais suffisante") + 2*(s$transports_avis=="Convenable, mais devrait être accrue") + 3*(s$transports_avis=="Satisfaisante") - (s$transports_avis=="NSP")
  s$transports_avis <<- as.item(temp, labels = structure(c(-1:3),
                          names = c("NSP","Insuffisante","Limitée","Convenable","Satisfaisante")),
                          # names = c("NSP","Insuffisante","Limitée, mais suffisante","Convenable, mais devrait être accrue","Satisfaisante")),
                        missing.values = -1, annotation=Label(s$transports_avis))
  
  labels(s$transports_travail_commun) <<- c("Non"="Non", "NSP"="NSP", "Oui, aucun pb"="Oui, ça ne me poserait pas de grande difficulté", "Oui, embêtant"="Oui, mais ça m'embêterait")
  labels(s$transports_travail_actif) <<- c("Non"="Non", "NSP"="NSP", "Oui, aucun pb"="Oui, ça ne me poserait pas de grande difficulté", "Oui, embêtant"="Oui, mais ça m'embêterait")
  s$Transports_travail_commun <<- s$transports_travail_commun
  s$Transports_travail_actif <<- s$transports_travail_actif
  s$Transports_travail_commun[is.na(s$Transports_travail_commun)] <<- "Non concerné"
  s$Transports_travail_actif[is.na(s$Transports_travail_actif)] <<- "Non concerné"
  s$Transports_travail_actif <<- as.item(as.character(s$Transports_travail_actif), missing.values=c('NSP', "Non concerné"), annotation="Transports_travail_actif: (transports_travail_actif sans NA) Sans changer de logement ni de lieu de travail, il serait possible pour le répondant prenant sa voiture d'effectuer ses trajets domicile-travail en marchant ou en vélo (Non/Oui mais ça l'embêterait/Oui ça ne lui poserait pas de grande difficulté/NSP)")
  s$Transports_travail_commun <<- as.item(as.character(s$Transports_travail_commun), missing.values=c('NSP', "Non concerné"), annotation="Transports_travail_commun: (transports_travail_commun sans NA) Sans changer de logement ni de lieu de travail, il serait possible pour le répondant prenant sa voiture de prendre les transports en commun pour ses trajets domicile-travail (Non/Oui mais ça l'embêterait/Oui ça ne lui poserait pas de grande difficulté/NSP)")
  s$Transports_distance <<- s$transports_distance
  s$Transports_distance[is.na(s$Transports_distance)] <<- mean(s$transports_distance, na.rm=T)
  s$Transports_distance <<- as.item(n(s$Transports_distance), missing.values = mean(s$transports_distance, na.rm=T), annotation="Transports_distance: (transports_distance sans NA) L'arrêt de transport en commun le plus proche de chez le répondant est à X minutes de marche")

  # TODO: récupérer le vrai âge à partir de ID_age_dep_device.csv 
  temp <- 20.90*(s$age == "18 à 24 ans") + 29.61*(s$age == "25 à 34 ans") + 42.14*(s$age == "35 à 49 ans") + 56.84*(s$age == "50 à 64 ans") + 75.43*(s$age == "65 ans ou plus")
  s$age <<- as.item(temp, labels = structure(c(20.90, 29.61, 42.14, 56.84, 75.43), names = c("18-24", "25-34", "35-49", "50-64", "65+")), annotation=Label(s$age))
  # s$Age <<- (s$age == "18 à 24 ans") + 2*(s$age == "25 à 34 ans") + 3.3*(s$age == "35 à 49 ans") + 4.6*(s$age == "50 à 64 ans") + 7*(s$age == "65 ans ou plus")
  s$taille_agglo <<- as.item(as.numeric(s$taille_agglo), labels = structure(1:5, names = c("rural", "-20k", "20-100k", "+100k", "Paris")), annotation=Label(s$taille_agglo))  

  s$Diplome <<- (s$diplome == "Brevet des collèges") + 2*(s$diplome=="CAP ou BEP") + 3*(s$diplome=="Baccalauréat") + 4*(s$diplome=="Bac +2 (BTS, DUT, DEUG, écoles de formation sanitaires et sociales...)") + 5*(s$diplome=="Bac +3 (licence...)") + 6*(s$diplome=="Bac +5 ou plus (master, école d'ingénieur ou de commerce, doctorat, médecine, maîtrise, DEA, DESS...)") - (s$diplome=="NSP (Ne se prononce pas)")
  s$diplome4 <<- as.item(pmin(pmax(s$Diplome, 1), 4), labels = structure(1:4, names = c("Aucun diplôme ou brevet", "CAP ou BEP", "Baccalauréat", "Supérieur")), annotation=Label(s$diplome))  
  # s$diplome4 <<- as.character(s$diplome)
  # s$diplome4[s$Diplome<2] <<- "Aucun diplôme ou brevet"
  # s$diplome4[s$Diplome>3] <<- "Supérieur"
 
  # labels(s$mode_chauffage) <<- c("individuel"="Chauffage individuel", "collectif"="Chauffage collectif", "NSP"="NSP")
  # labels(s$chauffage) <<- c("Gaz réseau"="Gaz de ville", "Gaz bouteille"="Butane, propane, gaz en citerne", "Fioul"="Fioul, mazout, pétrole", "Électricité"="Électricité", "Bois, solaire..."="Bois, solaire, géothermie, aérothermie (pompe à chaleur)", "Autre"="Autre", "NSP"="NSP")
  # labels(s$schiste_CC) <<- c("malvenue"="Elle est malvenue : il faudrait mettre fin aux émissions, pas seulement les ralentir", "valable"="Elle est valable : toute baisse des émissions va dans la bonne direction", "NSP"="NSP")
  # labels(s$cause_CC) <<- c("n'existe pas"="n'est pas une réalité", "naturel"="est principalement dû à la variabilité naturelle du climat", "anthropique"="est principalement dû à l'activité humaine", "NSP"="NSP")
  s$mode_chauffage <<- relabel(s$mode_chauffage, c("Chauffage individuel"="individuel", "Chauffage collectif"="collectif", "NSP"="NSP"))
  s$chauffage <<- relabel(s$chauffage, c("Gaz de ville"="Gaz réseau", "Butane, propane, gaz en citerne"="Gaz bouteille", "Fioul, mazout, pétrole"="Fioul", "Électricité"="Électricité", "Bois, solaire, géothermie, aérothermie (pompe à chaleur)"="Bois, solaire...", "Autre"="Autre", "NSP"="NSP"))
  s$schiste_CC <<- relabel(s$schiste_CC, c("Elle est malvenue : il faudrait mettre fin aux émissions, pas seulement les ralentir"="malvenue", "Elle est valable : toute baisse des émissions va dans la bonne direction"="valable", "NSP"="NSP"))
  s$cause_CC <<- relabel(s$cause_CC, c("n'est pas une réalité"="n'existe pas", "est principalement dû à la variabilité naturelle du climat"="naturel", "est principalement dû à l'activité humaine"="anthropique", "NSP"="NSP"))
  s$Compris_depenses <<- as.character(s$compris_depenses)
  s$Compris_depenses[is.na(s$Compris_depenses)] <<- "NA"
  s$compris_depenses <<- as.item(as.character(s$compris_depenses),
                                 labels = structure(c("", "Oui","Non","Bug: le graphique ne s'est pas affiché correctement."), names = c("NA", "Oui","Non","Bug")), annotation=attr(s$compris_depenses, "label"))

  s$enfant_CC[is.na(s$enfant_CC)] <<- "NSP"
  s$enfant_CC_pour_CC[s$enfant_CC=='Non'] <<- 'Non concerné'
  s$enfant_CC_pour_CC <<- as.item(as.character(s$enfant_CC_pour_CC), labels = structure(c(T, FALSE, 'Non concerné'), names=c('TRUE', 'FALSE', 'Non concerné')),
                                 missing.values='Non concerné', annotation=Label(s$enfant_CC_pour_CC))
  s$enfant_CC_pour_lui[s$enfant_CC=='Non'] <<- 'Non concerné'
  s$enfant_CC_pour_lui <<- as.item(as.character(s$enfant_CC_pour_lui), labels = structure(c(T, FALSE, 'Non concerné'), names=c('TRUE', 'FALSE', 'Non concerné')),
                                 missing.values='Non concerné', annotation=Label(s$enfant_CC_pour_lui))  
  
  s$gauche_droite <<- pmax(-2,pmin(2,-2 * grepl("extrême gauche", s$extr_gauche) - grepl("De gauche", s$gauche) + grepl("De droite", s$droite) + 2 * grepl("extrême droite", s$extr_droite)))
  is.na(s$gauche_droite) <<- (s$gauche_droite == 0) & !grepl("centre", s$centre)
  s$Gauche_droite <<- as.factor(s$gauche_droite)
  s$gauche_droite <<- as.item(as.numeric(as.vector(s$gauche_droite)), labels = structure(c(-2:2),
                          names = c("Extrême gauche","Gauche","Centre","Droite","Extrême droite")), annotation="gauche_droite:échelle de -2 (extr_gauche) à +2 (extr_droite) - Orientation politique (Comment vous définiriez-vous ? Plusieurs réponses possibles: (D'extrême) gauche/Du centre/(D'extrême) droite/Libéral/Humaniste/Patriote/Apolitique/Écologiste/Conservateur (champ libre)/NSP)") 
  levels(s$Gauche_droite) <<- c("Extreme-left", "Left", "Center", "Right", "Extreme-right", "Indeterminate")
  s$Gauche_droite[is.na(s$Gauche_droite)] <<- "Indeterminate"
  
  temp <- Label(s$interet_politique)
  s$interet_politique <<- 1*(s$interet_politique=='Un peu') + 2*(s$interet_politique=='Beaucoup')
  s$interet_politique <<- as.item(s$interet_politique, labels=structure(c(0:2), names=c('Presque pas', 'Un peu', 'Beaucoup')), annotation=temp)

  # s$gilets_jaunes[s$gilets_jaunes_NSP==T] <<- -99
  s$gilets_jaunes[s$gilets_jaunes_compris==T] <<- 0 # total à 115%
  s$gilets_jaunes[s$gilets_jaunes_oppose==T] <<- -1 # 2 oppose et soutien en même temps
  s$gilets_jaunes[s$gilets_jaunes_soutien==T] <<- 1
  s$gilets_jaunes[s$gilets_jaunes_dedans==T] <<- 2
  label(s$gilets_jaunes) <<- "gilets_jaunes: -1: s'oppose / 0: comprend sans soutenir ni s'opposer / 1: soutient / 2: fait partie des gilets jaunes (gilets_jaunes_compris/oppose/soutien/dedans/NSP)" 
  s$gilets_jaunes <<- as.item(n(s$gilets_jaunes), labels = structure(c(-1:2), names=c('oppose', 'comprend', 'soutient', 'est_dedans')), 
                             annotation=attr(s$gilets_jaunes, "label"))
  
  # temp <- label(s$diplome)
  # s$diplome <<- factor(s$diplome, c("","Aucun diplôme","Brevet des collèges","CAP ou BEP","Baccalauréat","Bac +2 (BTS, DUT, DEUG, écoles de formation sanitaires et sociales...)","Bac +3 (licence...)","Bac +5 ou plus (master, école d'ingénieur ou de commerce, doctorat, médecine, maîtrise, DEA, DESS...)","NSP (Ne se prononce pas)") )
  # label(s$diplome) <<- temp
  # temp <- label(s$interet_politique)
  # s$interet_politique <<- factor(s$interet_politique, c("","Beaucoup","Un peu","Presque pas (ou pas du tout)","NSP","NSP (Je ne veux pas répondre)","Q20 - À quel point êtes-vous intéressé·e par la politique ?"))
  # label(s$interet_politique) <<- temp

  s$revenu_conjoint <<- s$rev_tot - s$revenu
  s$revdisp <<- round((s$rev_tot -  irpp(s$rev_tot, s$nb_adultes, s$taille_menage)))
  s$uc <<- uc(s$taille_menage, s$nb_14_et_plus)
  s$niveau_vie <<- s$revdisp / s$uc

  # s$age <<- as.factor(as.character(s$age))
  # s$Region <<- as.factor(as.character(s$region)) 
  # s$taille_agglo <<- as.factor(gsub("[[:alpha:] ]", "", s$taille_agglo))
  # s <<- s[s$taille_agglo!="%1%",] 

  # pourquoi 91 missing regions? Sûrement un bug du Javascript côté utilisateur (certains types de device/navigateur?): 56% ont un bug sur compris_depenses contre 3% pour les autres
	region_code <- function(code) {
	  reg <- "autre"
	  regions <- list(
      "ARA" = c('01', '03', '07', '15', '26', '38', '42', '43', '63', '69', '73', '74'),
    	"Est" = c('21', '25', '39', '58', '70', '71', '89', '90', '08', '10', '51', '52', '54', '55', '57', '67', '68', '88'),
    	"Ouest" = c('22', '29', '35', '56', '14', '27', '50', '61', '76' ),
    	"Centre" = c('18', '28', '36', '37', '41', '45', '44', '49', '53', '72', '85'),
    	"Nord" = c('02', '59', '60', '62', '80'),
    	"IDF" = c('75', '77', '78', '91', '92', '93', '94', '95'),
    	"SO" = c('16', '17', '19', '23', '24', '33', '40', '47', '64', '79', '86', '87'),
    	"Occ" = c('09', '11', '12', '30', '31', '32', '34', '46', '48', '65', '66', '81', '82'),
    	"PACA" = c( '04', '05', '06', '13', '83', '84')
	  )
  	for (i in 1:9) if (as.numeric(code) %in% as.numeric(regions[[i]])) reg <- names(regions)[i]
	  return(reg)
	} # TODO: pourquoi Centre excède de 20% le quota? Pourquoi y a-t-il aussi des excès dee quotas dans taille_agglo?
  region_dep <- rep("", 95)
  for (i in 1:95) region_dep[i] <- region_code(i)
  s$region <<- "autre"
  s$region[as.numeric(substr(s$code_postal, 1, 2)) %in% 1:95] <<- region_dep[as.numeric(substr(s$code_postal, 1, 2))]
  # TODO: missing schiste_traite? dep_traites <- c(01, 02, 04, 06, 07, 08, 10, 11, 12, 13, 24, 25, 26, 30, 31, 32, 33, 34, 38, 39, 40, 45, 46, 48, 51, 54, 55, 57, 59, 60, 62, 64, 65, 67, 68, 69, 71, 73, 74, 77, 78, 82, 83, 84, 89, 90, 91, 93, 94, 95)
  s$nb_vehicules <<- (s$nb_vehicules_texte=='Un') + 2*(s$nb_vehicules_texte=='Deux ou plus')

  s$variante_partielle <<- 'NA'
  s$variante_partielle[!is.na(s$gagnant_chauffage_categorie)] <<- 'c'
  s$variante_partielle[!is.na(s$gagnant_fuel_categorie)] <<- 'f'
  label(s$variante_partielle) <<- "variante_partielle: Variante aléatoire (c/f) de la taxe partielle sur le chauffage ou les carburants (=fuel)"
  s$perte_relative_partielle[s$variante_partielle=='c'] <<- s$perte_relative_chauffage[s$variante_partielle=='c']
  s$perte_relative_partielle[s$variante_partielle=='f'] <<- s$perte_relative_fuel[s$variante_partielle=='f']
  label(s$perte_relative_partielle) <<- "perte_relative_partielle: Une hausse des taxes sur variante_partielle (chauffage ou fuel) ferait perdre plus à votre ménage que la moyenne (Oui, beaucoup/un peu plus/Autant que la moyenne/Non, un peu/beaucoup moins/NSP) - Q155, 162"

    # s$gain_fuel <- NA
  s$gain_fuel[s$gagnant_fuel_categorie=='Non affecté' & s$variante_partielle=='f'] <<- 0
  s$gain_fuel[s$gagnant_fuel_categorie=='Gagnant' & s$variante_partielle=='f'] <<- 1 + as.numeric(gsub("\\D*", "", sub("\\sà.*", "", sub("\\D*", "", s$gain_taxe_fuel_hausse[s$gagnant_fuel_categorie=='Gagnant' & s$variante_partielle=='f']))))/25
  s$gain_fuel[s$gagnant_fuel_categorie=='Perdant' & s$variante_partielle=='f'] <<- - 1 - as.numeric(gsub("\\D*", "", sub("\\sà.*", "", sub("\\D*", "", s$gain_taxe_fuel_baisse[s$gagnant_fuel_categorie=='Perdant' & s$variante_partielle=='f']))))/25
  label(s$gain_fuel) <<- "gain_fuel: Catégorie de gain-perte de pouvoir d'achat par UC, suite à hausse taxe carburants compensée, dans [-6;5] (seuils: -160/-110/-70/-40/-15/0/10/20/30/40)"
  
  s$gain_chauffage[s$gagnant_chauffage_categorie=='Non affecté' & s$variante_partielle=='c'] <<- 0
  s$gain_chauffage[s$gagnant_chauffage_categorie=='Gagnant' & s$variante_partielle=='c'] <<- 1 + as.numeric(gsub("\\D*", "", sub("\\sà.*", "", sub("\\D*", "", s$gain_taxe_chauffage_hausse[s$gagnant_chauffage_categorie=='Gagnant' & s$variante_partielle=='c']))))/25
  s$gain_chauffage[s$gagnant_chauffage_categorie=='Perdant' & s$variante_partielle=='c'] <<- - 1 - as.numeric(gsub("\\D*", "", sub("\\sà.*", "", sub("\\D*", "", s$gain_taxe_chauffage_baisse[s$gagnant_chauffage_categorie=='Perdant' & s$variante_partielle=='c']))))/25
  label(s$gain_chauffage) <<- "gain_fuel: Catégorie de gain-perte de pouvoir d'achat par UC, suite à hausse taxe chauffage compensée, dans [-6;5] (seuils: -160/-110/-70/-40/-15/0/10/20/30/40)"

  s$gagnant_partielle_categorie[s$variante_partielle=='c'] <<- s$gagnant_chauffage_categorie[s$variante_partielle=='c'] 
  s$gagnant_partielle_categorie[s$variante_partielle=='f'] <<- s$gagnant_fuel_categorie[s$variante_partielle=='f']
  label(s$gagnant_partielle_categorie) <<- "gagnant_partielle_categorie: Ménage Gagnant/Non affecté/Perdant par hausse taxe partielle (chauffage ou fuel) compensée, dans [-6;5] (seuils: -160/-110/-70/-40/-15/0/10/20/30/40) (gagnant_fuel_categorie/chauffage)"
  s$gain_partielle[s$variante_partielle=='c'] <<- s$gain_chauffage[s$variante_partielle=='c'] 
  s$gain_partielle[s$variante_partielle=='f'] <<- s$gain_fuel[s$variante_partielle=='f']
  label(s$gain_partielle) <<- "gain_partielle: Catégorie de gain-perte de pouvoir d'achat par UC, suite à hausse taxe partielle (chauffage ou fuel) compensée, dans [-6;5] (seuils: -160/-110/-70/-40/-15/0/10/20/30/40) (gain_fuel/chauffage)"
  
  s$gain[s$gagnant_categorie=='Non affecté' & s$variante_partielle!='NA'] <<- 0
  s$gain[s$gagnant_categorie=='Gagnant' & s$variante_partielle!='NA'] <<- 1 + as.numeric(gsub("\\D*", "", sub("\\sà.*", "", sub("\\D*", "", s$gain_taxe_hausse[s$gagnant_categorie=='Gagnant' & s$variante_partielle!='NA']))))/50
  s$gain[s$gagnant_categorie=='Perdant' & s$variante_partielle!='NA'] <<- - 1 - as.numeric(gsub("\\D*", "", sub("\\sà.*", "", sub("\\D*", "", s$gain_taxe_baisse[s$gagnant_categorie=='Perdant' & s$variante_partielle!='NA']))))/50
  label(s$gain) <<- "gain: Catégorie de gain-perte de pouvoir d'achat par UC, suite à hausse taxe carbone compensée, dans [-6;5] (seuils: -280/-190/-120/-70/-30/0/20/40/60/80)"

  s$gain_echelle <<- s$gain
  s$gain_fuel_echelle <<- s$gain_fuel
  s$gain_chauffage_echelle <<- s$gain_chauffage
  s$gain_partielle_echelle <<- s$gain_fuel
  s$gain_partielle_echelle[!is.na(s$gain_chauffage_echelle)] <<- s$gain_chauffage_echelle[!is.na(s$gain_chauffage_echelle)]
  label(s$gain) <<- "gain: Catégorie de gain-perte de pouvoir d'achat par UC, suite à hausse taxe carbone compensée, dans [-6;5] (seuils: -280/-190/-120/-70/-30/0/20/40/60/80)"
  label(s$gain_chauffage_echelle) <<- "gain_chauffage_echelle: Catégorie de gain-perte de pouvoir d'achat par UC, suite à hausse taxe chauffage compensée, dans [-6;5] (seuils: -160/-110/-70/-40/-15/0/10/20/30/40)"
  label(s$gain_fuel_echelle) <<- "gain_fuel_echelle: Catégorie de gain-perte de pouvoir d'achat par UC, suite à hausse taxe carburants compensée, dans [-6;5] (seuils: -160/-110/-70/-40/-15/0/10/20/30/40)"
  label(s$gain_partielle_echelle) <<- "gain_partielle_echelle: Catégorie de gain-perte de pouvoir d'achat par UC, suite à hausse taxe partielle (carburants ou chauffage) compensée, dans [-6;5] (seuils: -160/-110/-70/-40/-15/0/10/20/30/40)"
  
  # cf. consistency_belief_losses.py pour les imputations
  s$gain_min <<- -1000*(s$gain==-6) - 280*(s$gain==-5) - 190*(s$gain==-4) - 120*(s$gain==-3) - 70*(s$gain==-2) - 30*(s$gain==-1) + 0*(s$gain==1) + 20*(s$gain==2) + 40*(s$gain==3) + 60*(s$gain==4) + 80*(s$gain==5)
  s$gain_max <<- -280*(s$gain==-6) - 190*(s$gain==-5) - 120*(s$gain==-4) - 70*(s$gain==-3) - 30*(s$gain==-2) - 0*(s$gain==-1) + 20*(s$gain==1) + 40*(s$gain==2) + 60*(s$gain==3) + 80*(s$gain==4) + 2000*(s$gain==5)
  s$gain_fuel_min <<- -1000*(s$gain_fuel==-6) - 160*(s$gain_fuel==-5) - 110*(s$gain_fuel==-4) - 70*(s$gain_fuel==-3) - 40*(s$gain_fuel==-2) - 15*(s$gain_fuel==-1) + 0*(s$gain_fuel==1) + 10*(s$gain_fuel==2) + 20*(s$gain_fuel==3) + 30*(s$gain_fuel==4) + 40*(s$gain_fuel==5)
  s$gain_fuel_max <<- -160*(s$gain_fuel==-6) - 110*(s$gain_fuel==-5) - 70*(s$gain_fuel==-4) - 40*(s$gain_fuel==-3) - 15*(s$gain_fuel==-2) - 0*(s$gain_fuel==-1) + 10*(s$gain_fuel==1) + 20*(s$gain_fuel==2) + 30*(s$gain_fuel==3) + 40*(s$gain_fuel==4) + 1000*(s$gain_fuel==5)
  s$gain_chauffage_min <<- -1000*(s$gain_chauffage==-6) - 160*(s$gain_chauffage==-5) - 110*(s$gain_chauffage==-4) - 70*(s$gain_chauffage==-3) - 40*(s$gain_chauffage==-2) - 15*(s$gain_chauffage==-1) + 0*(s$gain_chauffage==1) + 10*(s$gain_chauffage==2) + 20*(s$gain_chauffage==3) + 30*(s$gain_chauffage==4) + 40*(s$gain_chauffage==5)
  s$gain_chauffage_max <<- -160*(s$gain_chauffage==-6) - 110*(s$gain_chauffage==-5) - 70*(s$gain_chauffage==-4) - 40*(s$gain_chauffage==-3) - 15*(s$gain_chauffage==-2) - 0*(s$gain_chauffage==-1) + 10*(s$gain_chauffage==1) + 20*(s$gain_chauffage==2) + 30*(s$gain_chauffage==3) + 40*(s$gain_chauffage==4) + 1000*(s$gain_chauffage==5)
  temp <- -405.55*(s$gain==-6) - 224.25*(s$gain==-5) - 147.91*(s$gain==-4) - 92.83*(s$gain==-3) - 48.28*(s$gain==-2) - 13.72*(s$gain==-1) + 10.39*(s$gain==1) + 30.36*(s$gain==2) + 49.96*(s$gain==3) + 69.72*(s$gain==4) + 106.89*(s$gain==5) #  - 1.66*(s$gain==0)
  s$gain <<- as.item(temp, labels = structure(c(-405.55, -224.25, -147.91, -92.83, -48.28, -13.72, 0, 10.39, 30.36, 49.96, 69.72, 106.89), names = c("<-280", "-280_-190", "-190_-120", "-120_-70", "-70_-30", "-30_0", "0", "0_20", "20_40", "40_60", "60_80", ">80")), annotation=Label(s$gain))
  temp <- NA
  temp[!is.na(s$gain_fuel)] <- (-248.76*(s$gain_fuel==-6) - 131.21*(s$gain_fuel==-5) - 87.36*(s$gain_fuel==-4) - 53.27*(s$gain_fuel==-3) - 25.49*(s$gain_fuel==-2) - 7.51*(s$gain_fuel==-1) + 5.17*(s$gain_fuel==1) + 14.73*(s$gain_fuel==2) + 24.85*(s$gain_fuel==3) + 34.89*(s$gain_fuel==4) + 57.42*(s$gain_fuel==5))[!is.na(s$gain_fuel)] #  - 1.17*(s$gain_fuel==0)
  s$gain_fuel <<- as.item(temp, labels = structure(c(-248.76, -131.21, -87.36, -53.27, -25.49, -7.51, 0, 5.17, 14.73, 24.85, 34.89, 57.42), names = c("<-160", "-160_-110", "-110_-70", "-70_-40", "-40_-15", "-15_0", "0", "0_10", "10_20", "20_30", "30_40", ">40")), annotation=Label(s$gain_fuel))
  temp <- NA
  temp[!is.na(s$gain_chauffage)] <- (-262.07*(s$gain_chauffage==-6) - 132.69*(s$gain_chauffage==-5) - 87.05*(s$gain_chauffage==-4) - 53.65*(s$gain_chauffage==-3) - 26.57*(s$gain_chauffage==-2) - 7.20*(s$gain_chauffage==-1) + 4.53*(s$gain_chauffage==1) + 15.44*(s$gain_chauffage==2) + 25.26*(s$gain_chauffage==3) + 35.66*(s$gain_chauffage==4) + 54.67*(s$gain_chauffage==5))[!is.na(s$gain_chauffage)] #  - 1.34*(s$gain_chauffage==0)
  s$gain_chauffage <<- as.item(temp, labels = structure(c(-262.07, -132.69, -87.05, -53.65, -26.57, -7.20, 0, 4.53, 15.44, 25.26, 35.66, 54.67), names = c("<-160", "-160_-110", "-110_-70", "-70_-40", "-40_-15", "-15_0", "0", "0_10", "10_20", "20_30", "30_40", ">40")), annotation=Label(s$gain_chauffage))

  s$Elasticite_chauffage <<- as.numeric(gsub("\\D*", "", sub("\\sà.*", "", sub("\\D*", "", s$elasticite_chauffage))))
  s$Elasticite_chauffage <<- (s$Elasticite_chauffage==0)*1.5 + (s$Elasticite_chauffage==3)*6.5 + (s$Elasticite_chauffage>3)*(s$Elasticite_chauffage + 5) # Take the average of thresholds, take 40% for >30%
  s$Elasticite_chauffage <<- - round(s$Elasticite_chauffage / 30, 2) # converts into elasticity
  label(s$Elasticite_chauffage) <<- "Elasticite_chauffage: Élasticité-prix des dépenses de chauffage des Français, calculée en prenant la valeur moyenne des intervalles proposées (seuils à 0/3/10/20/30% pour une hausse de 30%)"

  s$Elasticite_chauffage_perso <<- as.numeric(gsub("\\D*", "", sub("\\sà.*", "", sub("\\D*", "", s$elasticite_chauffage_perso))))
  s$Elasticite_chauffage_perso <<- (s$Elasticite_chauffage_perso==0)*1.5 + (s$Elasticite_chauffage_perso==3)*6.5 + (s$Elasticite_chauffage_perso>3)*(s$Elasticite_chauffage_perso + 5) # Take the average of thresholds, take 40% for >30%
  s$Elasticite_chauffage_perso <<- - round(s$Elasticite_chauffage_perso / 30, 2) # converts into elasticity
  label(s$Elasticite_chauffage_perso) <<- "Elasticite_chauffage_perso: Élasticité-prix des dépenses de chauffage du ménage, calculée en prenant la valeur moyenne des intervalles proposées (seuils à 0/3/10/20/30% pour une hausse de 30%)"

  s$Elasticite_fuel <<- as.numeric(gsub("\\D*", "", sub("\\sà.*", "", sub("\\D*", "", s$elasticite_fuel))))
  s$Elasticite_fuel <<- (s$Elasticite_fuel==0)*1.5 + (s$Elasticite_fuel==3)*6.5 + (s$Elasticite_fuel>3)*(s$Elasticite_fuel + 5) # Take the average of thresholds, take 40% for >30%
  s$Elasticite_fuel <<- - round(s$Elasticite_fuel / 30, 2) # converts into elasticity
  label(s$Elasticite_fuel) <<- "Elasticite_fuel: Élasticité-prix des dépenses de carburants des Français, calculée en prenant la valeur moyenne des intervalles proposées (seuils à 0/3/10/20/30% pour une hausse de 0.5€/L)"

  s$Elasticite_fuel_perso <<- as.numeric(gsub("\\D*", "", sub("\\sà.*", "", sub("\\D*", "", s$elasticite_fuel_perso))))
  s$Elasticite_fuel_perso <<- (s$Elasticite_fuel_perso==0)*1.5 + (s$Elasticite_fuel_perso==3)*6.5 + (s$Elasticite_fuel_perso>3)*(s$Elasticite_fuel_perso + 5) # Take the average of thresholds, take 40% for >30%
  s$Elasticite_fuel_perso <<- - round(s$Elasticite_fuel_perso / 30, 2) # converts into elasticity
  label(s$Elasticite_fuel_perso) <<- "Elasticite_fuel_perso: Élasticité-prix des dépenses de carburants du ménage, calculée en prenant la valeur moyenne des intervalles proposées (seuils à 0/3/10/20/30% pour une hausse de 0.5€/L)"
  
  s$elasticite_partielle <<- s$elasticite_chauffage
  s$elasticite_partielle[!is.na(s$elasticite_fuel)] <<- s$elasticite_fuel[!is.na(s$elasticite_fuel)]
  label(s$elasticite_partielle) <<- "elasticite_partielle: Réduction de la conso de fioul et gaz OU de carburants des Français suite à augmentation du prix de 30% (0% - Je n'en consomme déjà presque pas/0% - Je suis contraint sur tous mes déplacements/de 0% à 10%/de 10% à 20%/de 20% à 30%/+ de 30% - Je changerais largement mes habitudes de déplacement)"

  s$Elasticite_partielle <<- s$Elasticite_chauffage
  s$Elasticite_partielle[!is.na(s$Elasticite_fuel)] <<- s$Elasticite_fuel[!is.na(s$Elasticite_fuel)]
  label(s$Elasticite_partielle) <<- "Elasticite_partielle: Élasticité-prix des dépenses de fioul et gaz OU de carburants des Français, calculée en prenant la valeur moyenne des intervalles proposées"

  s$elasticite_partielle_perso <<- s$elasticite_chauffage_perso
  s$elasticite_partielle_perso[!is.na(s$elasticite_fuel_perso)] <<- s$elasticite_fuel_perso[!is.na(s$elasticite_fuel_perso)]
  label(s$elasticite_partielle_perso) <<- "elasticite_partielle_perso: Réduction de la conso de fioul et gaz OU de carburants du ménage suite à augmentation du prix de 30% (0% - Je n'en consomme déjà presque pas/0% - Je suis contraint sur tous mes déplacements/de 0% à 10%/de 10% à 20%/de 20% à 30%/+ de 30% - Je changerais largement mes habitudes de déplacement)"

  s$Elasticite_partielle_perso <<- s$Elasticite_chauffage_perso
  s$Elasticite_partielle_perso[!is.na(s$Elasticite_fuel_perso)] <<- s$Elasticite_fuel_perso[!is.na(s$Elasticite_fuel_perso)]
  label(s$Elasticite_partielle_perso) <<- "Elasticite_partielle_perso: Élasticité-prix des dépenses de fioul et gaz OU de carburants du ménage, calculée en prenant la valeur moyenne des intervalles proposées"
  
  s$variante_taxe_info <<- "p"
  s$variante_taxe_info[s$gagnant_feedback_categorie!=""] <<- "f"
  label(s$variante_taxe_info) <<- "variante_taxe_info: (f/p) Variante aléatoire entre f: feedback (=simulation, 2/3) et p: progressivité (1/3), dans l'information donnée sur la taxe compensée avant de demander à nouveau le gain et l'approbation, ainsi que les bénéfices et problèmes"
  for (v in c('CC', 'sante', 'circulation', 'revenu', 'pauvres', 'independance', 'enjeu', 'aucun', 'autre_choix', 'autre')) {
    s[[paste('benefices_', v, sep="")]] <<- s[paste('benefices_', v, '_p', sep="")][[1]]
    s[[paste('benefices_', v, sep="")]][s$variante_taxe_info=='f'] <<- s[paste('benefices_', v, '_f', sep="")][[1]][s$variante_taxe_info=='f']
    label(s[[paste('benefices_', v, sep="")]]) <<- sub("_f: \\* ", ": ", label(s[paste('benefices_', v, '_f', sep="")][[1]]))
  }
  for (v in c('inefficace', 'alternatives', 'ruraux', 'revenu', 'pauvres', 'economie', 'pretexte', 'aucun', 'autre_choix', 'autre')) {
    s[[paste('problemes_', v, sep="")]] <<- s[paste('problemes_', v, '_p', sep="")][[1]]
    s[[paste('problemes_', v, sep="")]][s$variante_taxe_info=='f'] <<- s[paste('problemes_', v, '_f', sep="")][[1]][s$variante_taxe_info=='f']
    label(s[[paste('problemes_', v, sep="")]]) <<- sub("_f: \\* ", ": ", label(s[paste('problemes_', v, '_f', sep="")][[1]]))
  }
  s$gagnant_info_categorie <<- s$gagnant_feedback_categorie
  s$gagnant_info_categorie[!is.na(s$gagnant_progressif_categorie)] <<- s$gagnant_progressif_categorie[!is.na(s$gagnant_progressif_categorie)]
  label(s$gagnant_info_categorie) <<- "gagnant_info_categorie: après info simule_gagnant et/ou progressivité: Ménage Gagnant/Non affecté/Perdant par hausse taxe carbone redistribuée à tous (+110€/an /adulte, +13/15% gaz/fioul, +0.11/13 €/L diesel/essence)"
  
  s$taxe_info_approbation <<- s$taxe_feedback_approbation
  s$taxe_info_approbation[!is.na(s$taxe_progressif_approbation)] <<- s$taxe_progressif_approbation[!is.na(s$taxe_progressif_approbation)]
  annotation(s$taxe_info_approbation) <<- "taxe_info_approbation: après info simule_gagnant et/ou progressivité - Approbation d'une hausse de la taxe carbone compensée (+110€/an /adulte, +13/15% gaz/fioul, +0.11/13 €/L diesel/essence)"
  
  s$cible[s$cible20==1] <<- '20'
  s$cible[s$cible30==1] <<- '30'
  s$cible[s$cible40==1] <<- '40'
  s$cible[s$cible50==1] <<- '50'
  s$cible <<- relevel(as.factor(s$cible), '50')
  label(s$cible) <<- "cible: Ciblage du recyclage de la hausse de la taxe carbone, attribué en fonction du revenu du répondant et de son ménage (20/30/40/50% les plus modestes)" 

  # TODO: manage to use tidyverse without erasing observations
  # s <<- merge(s, s %>% gather(key = temp, value = gain_cible, c(gagnant__20_categorie, gagnant_20_30_categorie, gagnant_30_40_categorie, gagnant_40_50_categorie, gagnant_50_70_categorie, gagnant_70__categorie)) %>% filter(gain_cible != "") %>% select(-temp), sort=FALSE) # Warning: attributes are not identical across measure variables; they will be dropped
  # label(s$gain_cible) <<- "gain_cible: Ménage Gagnant/Non affecté/Perdant avec hausse taxe carbone à recyclage ciblé (cible: 20/30/40/50% les plus modestes)"
  # s <<- merge(s, s %>% gather(key = temp, value = taxe_cible_approbation, c(taxe__20_approbation, taxe_20_30_approbation, taxe_30_40_approbation, taxe_40_50_approbation, taxe_50_70_approbation, taxe_70__approbation)) %>% filter(taxe_cible_approbation != "") %>% select(-temp), sort=FALSE)
  # label(s$taxe_cible_approbation) <<- "taxe_cible_approbation: Approbation d'une hausse de la taxe carbone compensée par recyclage ciblé (cible: 20/30/40/50% les plus modestes)"
  # 
  #   # s <<- s %>% gather(key = temp, value = km, c(km_0, km_1, km_2)) %>% filter(km != "") %>% select(-temp) # erases km_0, km_1, km_2
  # s <<- merge(s, s %>% gather(key = temp, value = km, c(km_0, km_1, km_2)) %>% filter(km != "") %>% select(-temp), sort=FALSE) # Warning: attributes are not identical across measure variables; they will be dropped
  # label(s$km) <<- "km: Nombre de kilomètres parcourus lors des 12 derniers mois en voiture ou moto (par le répondant pour nb_vehicules=0, par les véhicules sinon)"
  # 
  # s <<- merge(s, s %>% gather(key = temp, value = conso, c(conso_1, conso_2)) %>% filter(conso != "") %>% select(-temp), sort=FALSE)
  # label(s$conso) <<- "conso:  Consommation moyenne du véhicule (en litres aux 100 km)"

  s$gagnant_cible_categorie[!is.na(s$gagnant__20_categorie)] <<- s$gagnant__20_categorie[!is.na(s$gagnant__20_categorie)]
  s$gagnant_cible_categorie[!is.na(s$gagnant_20_30_categorie)] <<- s$gagnant_20_30_categorie[!is.na(s$gagnant_20_30_categorie)]
  s$gagnant_cible_categorie[!is.na(s$gagnant_30_40_categorie)] <<- s$gagnant_30_40_categorie[!is.na(s$gagnant_30_40_categorie)]
  s$gagnant_cible_categorie[!is.na(s$gagnant_40_50_categorie)] <<- s$gagnant_40_50_categorie[!is.na(s$gagnant_40_50_categorie)]
  s$gagnant_cible_categorie[!is.na(s$gagnant_50_70_categorie)] <<- s$gagnant_50_70_categorie[!is.na(s$gagnant_50_70_categorie)]
  s$gagnant_cible_categorie[!is.na(s$gagnant_70__categorie)] <<- s$gagnant_70__categorie[!is.na(s$gagnant_70__categorie)]
  label(s$gagnant_cible_categorie) <<- "gagnant_cible_categorie: Ménage Gagnant/Non affecté/Perdant avec hausse taxe carbone à recyclage ciblé (cible: 20/30/40/50% les plus modestes)"
  s$taxe_cible_approbation[!is.na(s$taxe__20_approbation)] <<- s$taxe__20_approbation[!is.na(s$taxe__20_approbation)]
  s$taxe_cible_approbation[!is.na(s$taxe_20_30_approbation)] <<- s$taxe_20_30_approbation[!is.na(s$taxe_20_30_approbation)]
  s$taxe_cible_approbation[!is.na(s$taxe_30_40_approbation)] <<- s$taxe_30_40_approbation[!is.na(s$taxe_30_40_approbation)]
  s$taxe_cible_approbation[!is.na(s$taxe_40_50_approbation)] <<- s$taxe_40_50_approbation[!is.na(s$taxe_40_50_approbation)]
  s$taxe_cible_approbation[!is.na(s$taxe_50_70_approbation)] <<- s$taxe_50_70_approbation[!is.na(s$taxe_50_70_approbation)]
  s$taxe_cible_approbation[!is.na(s$taxe_70__approbation)] <<- s$taxe_70__approbation[!is.na(s$taxe_70__approbation)]
  label(s$taxe_cible_approbation) <<- "taxe_cible_approbation: Approbation d'une hausse de la taxe carbone compensée par recyclage ciblé (cible: 20/30/40/50% les plus modestes)"

  s$categorie_cible[!is.na(s$taxe__20_approbation)] <<- '_20'
  s$categorie_cible[!is.na(s$taxe_20_30_approbation)] <<- '20_30'
  s$categorie_cible[!is.na(s$taxe_30_40_approbation)] <<- '30_40'
  s$categorie_cible[!is.na(s$taxe_40_50_approbation)] <<- '40_50'
  s$categorie_cible[!is.na(s$taxe_50_70_approbation)] <<- '50_70'
  s$categorie_cible[!is.na(s$taxe_70__approbation)] <<- '70_'
  label(s$categorie_cible) <<- "categorie_cible: Catégorie de revenu du répondant (et de son ménage) taxe à recyclage ciblé - Catégories: percentile <20/20-30/30-40/40-50/50-70/>70, sachant que revenu_conjoint détermine la catégorie quand percentile de revenu > 70 (780/1140/1430/1670/2220)"

  s$traite_cible <<- (s$cible==20)*(s$revenu<780) + (s$cible==30)*(s$revenu<1140) + (s$cible==40)*(s$revenu<1430) + (s$cible==50)*(s$revenu<1670)
  s$traite_cible_conjoint <<- (s$nb_adultes > 1)* ((s$cible==20)*(s$revenu_conjoint<780) + (s$cible==30)*(s$revenu_conjoint<1140) + (s$cible==40)*(s$revenu_conjoint<1430) + (s$cible==50)*(s$revenu_conjoint<1670))
  label(s$traite_cible) <<- "traite_cible: Indicatrice de transfert reçu par le répondant lors de la hausse de la taxe carbone avec compensation ciblée. Montants: 550/360/270/220 €/an/adulte pour cible à 20/30/40/50"
  label(s$traite_cible_conjoint) <<- "traite_cible_conjoint: Indicatrice de transfert reçu par le conjoint du répondant lors de la hausse de la taxe carbone avec compensation ciblée. Montants: 550/360/270/220 €/an/adulte pour cible à 20/30/40/50"
  s$versement_cible <<- (s$traite_cible + s$traite_cible_conjoint) * ((s$cible==20)*550 + (s$cible==30)*360 + (s$cible==40)*270 + (s$cible==50)*220)
  s$versement_cible_sans_conjoint <<- (s$traite_cible) * ((s$cible==20)*550 + (s$cible==30)*360 + (s$cible==40)*270 + (s$cible==50)*220)
  label(s$versement_cible) <<- "versement_cible: Versement annuel reçu par le ménage du répondant lors de la hausse de la taxe carbone avec compensation ciblée. Montants: 550/360/270/220 €/an/parent pour cible à 20/30/40/50"
  label(s$versement_cible_sans_conjoint) <<- "versement_cible_sans_conjoint: Versement annuel reçu par le répondant lors de la hausse de la taxe carbone avec compensation ciblée. Montants: 550/360/270/220 €/an/adulte pour cible à 20/30/40/50"
  s$versement <<- 110 * s$nb_adultes
  label(s$versement) <<- "versement: Versement annuel reçu par le ménage suite à une hausse de la taxe carbone compensée (110 * nb_adultes)"
  
  s$km[!is.na(s$km_0)] <<- s$km_0[!is.na(s$km_0)]
  s$km[!is.na(s$km_1)] <<- s$km_1[!is.na(s$km_1)]
  s$km[!is.na(s$km_2)] <<- s$km_2[!is.na(s$km_2)]
  label(s$km) <<- "km: Nombre de kilomètres parcourus lors des 12 derniers mois en voiture ou moto (par le répondant pour nb_vehicules=0, par les véhicules sinon)"
 
  s$conso[!is.na(s$conso_1)] <<- s$conso_1[!is.na(s$conso_1)]
  s$conso[!is.na(s$conso_2)] <<- s$conso_2[!is.na(s$conso_2)]
  s$conso[is.na(s$conso)] <<- (6.39 + 7.31) / 2
  label(s$conso) <<- "conso:  Consommation moyenne du véhicule (en litres aux 100 km)"

  s$gaz <<- grepl('gaz', s$chauffage, ignore.case = T)
  s$fioul <<- grepl('fioul', s$chauffage, ignore.case = T)
  s$hausse_chauffage <<- -55.507189 + s$gaz * 124.578484 + s$fioul * 221.145441 + s$surface * 0.652174  
	s$hausse_diesel[s$nb_vehicules == 0] <<- (0.5*(6.39/100) * s$km * 1.4 * (1 - 0.4) * 0.090922)[s$nb_vehicules == 0] # share_diesel * conso * km * price * (1-elasticite) * price_increase
	s$hausse_diesel[s$nb_vehicules == 1] <<- ((s$fuel_1=='Diesel') * (s$conso/100) * s$km * 1.4 * (1 - 0.4) * 0.090922)[s$nb_vehicules == 1]
  s$hausse_diesel[s$nb_vehicules == 2] <<- (((s$fuel_2_1=='Diesel')*2/3 + (s$fuel_2_2=='Diesel')/3) * (s$conso/100) * s$km * 1.4 * (1 - 0.4) * 0.090922)[s$nb_vehicules == 2]
	s$hausse_essence[s$nb_vehicules == 0] <<- (0.5*(7.31/100) * s$km * 1.45 * (1 - 0.4) * 0.076128)[s$nb_vehicules == 0] # share_diesel * conso * km * price * (1-elasticite) * price_increase
	s$hausse_essence[s$nb_vehicules == 1] <<- ((s$fuel_1!='Diesel') * (s$conso/100) * s$km * 1.45 * (1 - 0.4) * 0.076128)[s$nb_vehicules == 1]
  s$hausse_essence[s$nb_vehicules == 2] <<- (((s$fuel_2_1!='Diesel')*2/3 + (s$fuel_2_2!='Diesel')/3) * (s$conso/100) * s$km * 1.45 * (1 - 0.4) * 0.076128)[s$nb_vehicules == 2]
  s$hausse_carburants <<- s$hausse_diesel + s$hausse_essence
  label(s$hausse_carburants) <<- "hausse_carburant: Hausse des dépenses de carburants simulées pour le ménage, suite à la taxe (élasticité de 0.4) (hausse_diesel + hausse_essence)"
  s$hausse_depenses <<- s$hausse_carburants + s$hausse_chauffage
  s$diesel <<- (!is.na(s$fuel_1) & (s$fuel_1=='Diesel')) | (!is.na(s$fuel_2_2) & ((s$fuel_2_1=='Diesel') | (s$fuel_2_2=='Diesel')))
  s$essence <<- (!is.na(s$fuel_1) & (s$fuel_1=='Essence')) | (!is.na(s$fuel_2_2) & ((s$fuel_2_1=='Essence') | (s$fuel_2_2=='Essence')))
  label(s$diesel) <<- "diesel: Indicatrice de la possession d'un véhicule diesel par le ménage (fuel_1 ou fuel_2_1 ou fuel_2_2 = 'Diesel')"
  label(s$essence) <<- "essence: Indicatrice de la possession d'un véhicule à essence par le ménage (fuel_1 ou fuel_2_1 ou fuel_2_2 = 'Essence')"

  s$simule_gain <<- 16.1 + s$nb_adultes * 110 - s$hausse_depenses
  s$simule_gain_repondant <<- 16.1 + 110 - s$hausse_depenses
  label(s$simule_gain) <<- "simule_gain: Gain net annuel simulé pour le ménage du répondant suite à une hausse de taxe carbone compensée: 16.1 + nb_adultes * 110 - hausse_depenses"
  label(s$simule_gain_repondant) <<- "simule_gain_repondant: Gain net annuel simulé pour le répondant (sans tenir compte du potentiel versement reçu par les autres adultes du ménage) suite à une hausse de taxe carbone compensée: 116.1 - hausse_depenses"
  s$simule_gain_cible <<- s$versement_cible - s$hausse_depenses
  s$simule_gain_cible_sans_conjoint <<- s$versement_cible - s$hausse_depenses
  label(s$simule_gain_cible) <<- "simule_gain_cible: Gain net simulé pour le ménage du répondant suite à une hausse de taxe carbone avec compensation ciblée: versement_cible - hausse_depenses"
  label(s$simule_gain_cible_sans_conjoint) <<- "simule_gain_cible_sans_conjoint: Gain net simulé pour le répondant (sans tenir compte du potentiel versement reçu par son conjoint) suite à une hausse de taxe carbone avec compensation ciblée: versement_cible - hausse_depenses"
  s$simule_gagnant[is.na(s$simule_gagnant)] <<- 1*(s$simule_gain[is.na(s$simule_gagnant)] > 0)
  
  s$progressivite[!is.na(s$progressivite_feedback_sans_info)] <<- s$progressivite_feedback_sans_info[!is.na(s$progressivite_feedback_sans_info)]
  s$progressivite[!is.na(s$progressivite_feedback_avec_info)] <<- s$progressivite_feedback_avec_info[!is.na(s$progressivite_feedback_avec_info)]
  s$progressivite[!is.na(s$progressivite_progressif)] <<- s$progressivite_progressif[!is.na(s$progressivite_progressif)]
  label(s$progressivite) <<- "progressivite: ~ Une hausse de la taxe carbone compensée avantagerait les plus modestes (réunion des trois variante_progressivite: prog/fb_info/fb_no_info où seule fb_no_info est sans information préalable sur la progressivité) - Q206-208"
  s$variante_progressivite[!is.na(s$progressivite_feedback_sans_info)] <<- "fb_no_info"
  s$variante_progressivite[!is.na(s$progressivite_feedback_avec_info)] <<- "fb_info"
  s$variante_progressivite[s$variante_taxe_info=='p'] <<- "prog" # !is.na(s$progressivite_progressif) | 
  s$variante_progressivite[s$variante_taxe_info=='f' & s$apres_modifs==FALSE] <<- 'fb_no_info' 
  label(s$variante_progressivite) <<- "variante_progressivite: prog/fb_info/fb_no_info Variante aléatoire du bloc de questions où figure 'progressivite'. prog: info sur la progressivité / fb: feedback sur le statut gagnant/perdant simulé, info/no_info: avec/sans info sur la progressivité de la mesure" # , seulement pour apres_modifs=T
  s$info_progressivite <<- FALSE
  s$info_progressivite[s$variante_taxe_info=='p' | s$variante_progressivite=='fb_info'] <<- T
  label(s$info_progressivite) <<- "info_progressivite: Indicatrice qu'a été montrée l'information que la hausse de la taxe carbone compensée avantagerait les plus modestes"

  s$variante_monetaire[is.na(s$variante_monetaire)] <<- 0 # concerne seulement une observation. Évite des complications inutiles.
  
  s$age_18_24 <<- 1*(s$age == '18 à 24 ans')
  s$age_25_34 <<- 1*(s$age == '25 à 34 ans')
  s$age_35_49 <<- 1*(s$age == '35 à 49 ans')
  s$age_50_64 <<- 1*(s$age == '50 à 64 ans')
  s$age_65_plus <<- 1*(s$age == '65 ans ou plus')
  
  s$score_ges <<- 1 * (s$ges_CO2 == TRUE) + 1*(s$ges_CH4 == TRUE) + 1*(s$ges_O2 == FALSE) + 1*(s$ges_pm == FALSE)
  label(s$score_ges) <<- "score_ges: Somme des bonnes réponses au questionnaire gaz à effet de serre (ges_O2/CH4/pm/CO2)"
  s$score_climate_call <<- 1*(s$ges_avion == TRUE) + 1*(s$ges_boeuf == TRUE) + 1*(s$ges_nucleaire == FALSE)
  label(s$score_climate_call) <<- "score_climate_call: Somme des bonnes réponses au questionnaire Climate Call (avion-train / boeuf-pates / nucleaire-eolien) ges_avion/boeuf/nucleaire"  

  s$duree_info[s$info_CC==1 & s$info_PM==1] <<- s$duree_info_CC_PM[s$info_CC==1 & s$info_PM==1]
  s$duree_info[s$info_CC==0 & s$info_PM==1] <<- s$duree_info_PM[s$info_CC==0 & s$info_PM==1]
  s$duree_info[s$info_CC==1 & s$info_PM==0] <<- s$duree_info_CC[s$info_CC==1 & s$info_PM==0]
  s$duree_info[s$info_CC==0 & s$info_PM==0] <<- s$duree_no_info[s$info_CC==0 & s$info_PM==0]
  label(s$duree_info) <<- "duree_info: Temps de soumission - Ancrage (information procurée ou non au début sur changement climatique ou particules fines) (duree_info_CC/PM/CC_PM/no_info)"
  
  for (v in c("autonomie", "priorite", "etats", "global", "trop")) {
    s[[paste("aide_non", v, sep="_")]] <<- NA
    s[[paste("aide_non", v, sep="_")]][!is.na(s$transferts_inter_info) & s$transferts_inter_info==T & s$aide_2p==T] <<- s[[paste("aide_non", v, "i", sep="_")]][!is.na(s$transferts_inter_info) & s$transferts_inter_info==T & s$aide_2p==T]
    s[[paste("aide_non", v, sep="_")]][!is.na(s$transferts_inter_info) & s$transferts_inter_info==FALSE & s$aide_2p==T] <<- s[[paste("aide_non", v, "ni", sep="_")]][!is.na(s$transferts_inter_info) & s$transferts_inter_info==FALSE & s$aide_2p==T]
    label(s[[paste("aide_non", v, sep="_")]]) <<- Label(s[[paste("aide_non", v, "i", sep="_")]])
  }
  
  s$revenu_decile <- 1 + 1 * ((s$revenu > 237) + (s$revenu > 789) + (s$revenu > 1151) + (s$revenu > 1436) + (s$revenu > 1677) + (s$revenu > 1927) + (s$revenu > 2231) + (s$revenu > 2657) + (s$revenu > 3462))
  s$revenu_quintile <- 1 + 1 * ((s$revenu > 789) + (s$revenu > 1436) + (s$revenu > 1927) + (s$revenu > 2657))
  
  categories_depenses <- c("sante", "retraites", "protection", "education", "recherche", "loisirs", "infrastructures", "justice", "armee", "securite", "aide")
  # for (i in 0:10) s[[paste('dep', i, 'en_position', sep='_')]] <<- NA
  for (i in 0:10) {
    s[[paste('dep', i, 'en_position', sep='_')]] <<- (s$en_position_1==i) + 2*(s$en_position_2==i)  + 3*(s$en_position_3==i)  + 4*(s$en_position_4==i)  + 5*(s$en_position_5==i)  + 6*(s$en_position_6==i)  + 7*(s$en_position_7==i)  + 8*(s$en_position_8==i)  + 9*(s$en_position_9==i)  + 10*(s$en_position_10==i) 
    label(s[[paste('dep', i, 'en_position', sep='_')]]) <<- paste(paste('dep', i, 'en_position', sep='_'), ": Position à laquelle est affichée la catégorie de dépense ", i, "(", categories_depenses[i], ") (cf. en_position_i)", sep="")
    # for (o in 1:nrow(s)) {
    #   j <- s[[paste('en_position', i, sep='_')]][o]
    #   if (!is.na(j)) s[[paste('dep', j, 'en_position', sep='_')]][o] <<- i
    #   s[[paste('dep', j, 'en_position', sep='_')]][!is.na(s$en_position_0)] <- 
    # }
  }
  
  s <<- s[, -c(78:97, 100:119, 294:303)]
  # TODO: qualité, connaissances CC, opinions CC
}
# convert_s()
# prepare_s(exclude_screened=FALSE, exclude_speeder=FALSE, only_finished=FALSE)
# sa <- s
# prepare_s()

weighting_s <- function(data, printWeights = T) { # cf. google sheet
  d <- data
  d$region[is.na(d$region)] <- 'autre'
  d$taille_agglo <- as.numeric(d$taille_agglo)
  # d$csp <- factor(d$csp)
  # d$region <- factor(d$region)
  # levels(d$csp) <- c(levels(d$csp),"missing")
  # levels(d$region) <- c(levels(d$region),"missing")
  # levels(d$taille_agglo) <- c(levels(d$taille_agglo),"missing")
  # levels(d$sexe) <- c(levels(d$sexe),"missing")
  # d$csp[is.na(d$csp) | d$csp=="" | d$csp=="NSP"] <- "missing"
  # d$taille_agglo[is.na(d$taille_agglo)] <- "missing"
  # d$sexe[d$sexe=="" | d$sexe=="Autre"] <- "missing"

  unweigthed <- svydesign(ids=~1, data=d)
  sexe <- data.frame(sexe = c("Féminin", "Masculin"), Freq=nrow(d)*c(0.516,0.484)) # http://www.insee.fr/fr/themes/detail.asp?ref_id=bilan-demo&reg_id=0&page=donnees-detaillees/bilan-demo/pop_age2.htm
  csp <- data.frame(csp = c("Inactif", "Ouvrier", "Cadre", "Indépendant", "Intermédiaire", "Retraité", "Employé", "Agriculteur"),
                    Freq=nrow(d)*c(0.1244,0.1214,0.0943,0.0341,0.1364,0.3279,0.1535,0.008))
  region <- data.frame(region = c("autre","ARA", "Est", "Nord", "IDF", "Ouest", "SO", "Occ", "Centre", "PACA"), 
                       Freq=nrow(d)*c(0.001,0.124,0.129,0.093,0.189,0.103,0.093,0.091,0.099,0.078))
  age <- data.frame(age = c("18-24", "25-34", "35-49", "50-64", "65+"), 
                    Freq=nrow(d)*c(0.117,0.147,0.242,0.242,0.252)) # Données/estim-pop-reg-sexe...
  taille_agglo <- data.frame(taille_agglo = c(1:5), Freq=nrow(d)*c(0.2166,0.1710,0.1408,0.3083,0.1633))
  # revenu <- data.frame(revenu = c(), Freq=nrow(d)*c())
  diplome4 <- data.frame(diplome4 = c("Aucun diplôme ou brevet", "CAP ou BEP", "Baccalauréat", "Supérieur"),  # http://webcache.googleusercontent.com/search?q=cache:rUvf6u0uCnEJ:www.insee.fr/fr/themes/tableau.asp%3Freg_id%3D0%26ref_id%3Dnattef07232+&cd=1&hl=fr&ct=clnk&gl=fr&lr=lang_en%7Clang_es%7Clang_fr
                        Freq=nrow(d)*c(0.301, 0.246, 0.168, 0.285))

  if (length(which(is.na(d$taille_agglo)))>0) raked <- rake(design= unweigthed, sample.margins = list(~sexe,~diplome4,~region,~csp,~age),
                population.margins = list(sexe,diplome4,region,csp,age))    
  else raked <- rake(design= unweigthed, sample.margins = list(~sexe,~diplome4,~taille_agglo,~region,~csp,~age),
                population.margins = list(sexe,diplome4,taille_agglo,region,csp,age)) 

  if (printWeights) {    print(summary(weights(raked))  )
    print(sum( weights(raked) )^2/(length(weights(raked))*sum(weights(raked)^2)) ) # <0.5 : problématique   
    print( length(which(weights(raked)<0.25 | weights(raked)>4))/ length(weights(raked)))
  }
  return(weights(trimWeights(raked, lower=0.25, upper=4, strict=TRUE)))
}

prepare_s <- function(exclude_speeder=TRUE, exclude_screened=TRUE, only_finished=TRUE, only_known_agglo=T) { # , exclude_quotas_full=TRUE
  # setwd("/home/adrien/Google Drive/Economie/Travail/enquete/codes")
  # setwd("C:/Users/a.fabre/Google Drive/Economie/Travail/enquete/codes")
  # pes <<- read.csv("fin.csv", sep=";")
  # s <<- read.delim("politique.tsv", fileEncoding="UTF-16")
  # f_data <- read.delim("fin.tsv", fileEncoding="UTF-16")
  s <<- read_csv("survey.csv")
  for (i in 1:length(s)) { label(s[[i]]) <<- toString(s[i][[1]][1]) } # Use the first line to create variable names labels then remove it - to run only once # TODO: bug
  s <<- s[-c(1,2),c(1:91,94:115,117:235,241,247:310,313:319,92,93,116,311,312,236:240,242:246)]

  # if (exclude_screened) { s <<- s[s$Q_TerminateFlag=="",] } # remove Screened
  # if (exclude_speeder) { s <<- s[n(s$`Duration (in seconds)`) > 540,] } # remove speedest
  # if (exclude_quotas_full) { s <<- s[s[101][[1]] %in% c(1:5),]  } # remove those with a problem for the taille d'agglo
  # if (exclude_quotas_full) { s <<- s[s$Q_TerminateFlag=="",]  } # remove those with a problem for the taille d'agglo
  # if (only_finished) { s <<- s[as.vector(s$Finished)=="True",] }
  
  relabel_and_rename_s()
  
  print(paste(length(which(s$exclu=="QuotaMet")), "QuotaMet"))
  s$fini[s$exclu=="QuotaMet" | is.na(s$revenu)] <<- "False" # To check the number of QuotaMet that shouldn't have incremented the quota, comment this line and: decrit(s$each_strate[s$exclu=="QuotaMet" & s$csp=="Employé" & !grepl("2019-03-04 07", s$date)])
  if (exclude_screened) { s <<- s[is.na(s$exclu),] } # remove Screened
  if (exclude_speeder) { s <<- s[as.numeric(as.vector(s$duree)) > 420,] } # remove speedest /!\ was 540 before 22-02-11:00 (EST Coast time)
  # if (exclude_quotas_full) { s <<- s[s[101][[1]] %in% c(1:5),]  } # remove those with a problem for the taille d'agglo
  # if (exclude_quotas_full) { s <<- s[s$Q_TerminateFlag=="",]  } # remove those with a problem for the taille d'agglo
  if (only_finished) { s <<- s[s$fini=="True",] }
  
  agglos <- read.csv2('agglos.csv')
  names(agglos) <- c("id", "taille_agglo2")
  s <<- merge(s, agglos, by="id", all.x=T)
  s$taille_agglo[is.na(s$taille_agglo)] <<- s$taille_agglo2[is.na(s$taille_agglo)]
  s <<- s[, which(names(s)!="taille_agglo2")]
  print(paste(length(which(is.na(s$taille_agglo))), "tailles d'agglo sont manquantes"))
  # id_agglo_manquante <- s$id[is.na(s$taille_agglo)]
  # write.csv(id_agglo_manquante, "ID_agglo_manquante.csv")
  if (only_known_agglo) s <<- s[!is.na(s$taille_agglo),]
  
  convert_s() 
  
  s$sample <<- "a"
  s$sample[s$fini=="True"] <<- "e"
  s$sample[s$fini=="True" & n(s$duree) > 540] <<- "p"
  s$sample[s$fini=="True" & n(s$duree) > 540 & s$test_qualite=='Un peu'] <<- "f" # "q"? excluded because out of quotas
  s$sample[s$fini=="True" & n(s$duree) > 540 & s$exclu==""] <<- "r"
  
  # s <<- s[-which(is.element(s$id, s$id[duplicated(s$id)]) & !duplicated(s$id)),] # TODO: check duplicates

  s$weight <<- weighting_s(s)
}

prepare_s(exclude_screened=FALSE, exclude_speeder=FALSE, only_finished=T) # TODO: let only_finished = FALSE
sa <- s
# prepare_s(exclude_screened=FALSE, exclude_speeder=FALSE)
# se <- s
# prepare_s(exclude_screened=FALSE)
# sp <- s

prepare_s()

write.csv(s, "survey_prepared.csv")

# write.csv(s[,c("id", "taille_agglo", "sexe", "age", "diplome4", "region", "csp")], "IDs.csv")

# ids <- read.csv("C:/Users/a.fabre/Downloads/IDs.csv", header=FALSE)
# ids <- as.vector(ids$V1[-1])
# sid <- s[s$id %in% ids,]
# decrit(n(sid$duree)/60)
# length(which(n(sid$duree) > 7*60))
# decrit(sid$test_qualite)
