##### 1. Probabilité de se penser gagnant/perdant #####
# Description des variables
decrit(s$gagnant_feedback_categorie, weight = s$weight)
decrit(s$gagnant_categorie, weight = s$weight)
decrit(s$simule_gagnant, weight = s$weight)

# Si feedback gagnant : 47.9% se disent perdants / 27.9% gagnants / 24.2% gagnants
decrit(s$gagnant_feedback_categorie[s$simule_gagnant == 1], weight = s$weight[s$simule_gagnant == 1])
decrit(s$gagnant_categorie[s$simule_gagnant == 1], weight = s$weight[s$simule_gagnant == 1])

# Si feedback perdant : 85% se disent perdants / 12.9% gagnants / 2.2% gagnants
decrit(s$gagnant_feedback_categorie[s$simule_gagnant == 0], weight = s$weight[s$simule_gagnant == 0])
decrit(s$gagnant_categorie[s$simule_gagnant == 0], weight = s$weight[s$simule_gagnant == 0])

##### 2. Probabilité de changer d'avis sur ses gains/pertes #####
# Si feedback gagnant et pensait être perdant : 30% changent d'avis (devient non-affecté ou gagnant)
decrit((s$simule_gagnant == 1) & (s$gagnant_categorie == 'Perdant')) # 634 répondants
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Perdant')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Perdant')])
decrit((s$simule_gagnant == 1) & (s$gagnant_categorie != 'Gagnant')) # 855 répondants
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie != 'Gagnant')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie != 'Gagnant')])

# Si feedback gagnant et pensait être gagnant : 23% changent d'avis (devient non-affecté ou perdant)
decrit((s$simule_gagnant == 1) & (s$gagnant_categorie == 'Gagnant')) # 151 répondants
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Gagnant')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Gagnant')])
decrit((s$simule_gagnant == 1) & (s$gagnant_categorie != 'Perdant')) # 372 répondants
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie != 'Perdant')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie != 'Perdant')])

# Si feedback gagnant et pensait être non affecté :
decrit((s$simule_gagnant == 1) & (s$gagnant_categorie == 'Non affecté')) # 221 répondants
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Non affecté')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Non affecté')])

# Si feedback perdant et pensait être perdant : 5% changent d'avis (devient non-affecté ou gagnant)
decrit((s$simule_gagnant == 0) & (s$gagnant_categorie == 'Perdant')) # 247 répondants
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Perdant')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Perdant')])
decrit((s$simule_gagnant == 0) & (s$gagnant_categorie != 'Gagnant')) # 300 répondants
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie != 'Gagnant')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie != 'Gagnant')])

# Si feedback perdant et pensait être gagnant : 87% changent d'avis (devient non-affecté ou perdant)
decrit((s$simule_gagnant == 0) & (s$gagnant_categorie == 'Gagnant')) # 25 répondants
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Gagnant')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Gagnant')])
decrit((s$simule_gagnant == 0) & (s$gagnant_categorie != 'Perdant')) # 78 répondants
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie != 'Perdant')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie != 'Perdant')])

# Si feedback perdant et pensait être non affecté :
decrit((s$simule_gagnant == 0) & (s$gagnant_categorie == 'Non affecté')) # 53 répondants
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Non affecté')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Non affecté')])


##### 3. Probabilité d'approuver ou non #####
# ...


##### 4. Probabilité de changer d'avis sur l'approbation #####
# ...


##### 5. Adaptation Bayesienne biaisée des croyances (aka. "on se fait Bayeser") #####

# TODO: que faire avec non affectés ? les exclure ? les mettre avec gagnants ?
proba_gagner <- 0.70
proba_perdre <- 0.30
proba_simule_gagnant <- sum(s$weight[s$simule_gagnant==1 & s$variante_taxe_info=='f'])/sum(s$weight[s$variante_taxe_info=='f']) # 0.77
proba_simule_perdant <- sum(s$weight[s$simule_gagnant==0 & s$variante_taxe_info=='f'])/sum(s$weight[s$variante_taxe_info=='f']) # 0.23
proba_simule_gagnant_quand_cru_gagnant <- (sum(s$weight[s$simule_gagnant==1 & s$gagnant_categorie=='Gagnant' & s$variante_taxe_info=='f']) # 0.88
                                           /sum(s$weight[s$gagnant_categorie=='Gagnant' & s$variante_taxe_info=='f']))
proba_simule_non_gagnant_quand_cru_non_gagnant <- (sum(s$weight[s$simule_gagnant==0 & s$gagnant_categorie!='Gagnant' & s$variante_taxe_info=='f']) # 0.25
                                           /sum(s$weight[s$gagnant_categorie!='Gagnant' & s$variante_taxe_info=='f']))
proba_simule_perdant_quand_cru_perdant <- (sum(s$weight[s$simule_gagnant==0 & s$gagnant_categorie=='Perdant' & s$variante_taxe_info=='f']) # 0.28
                                           /sum(s$weight[s$gagnant_categorie=='Perdant' & s$variante_taxe_info=='f']))
proba_simule_non_perdant_quand_cru_non_perdant <- (sum(s$weight[s$simule_gagnant==1 & s$gagnant_categorie!='Perdant' & s$variante_taxe_info=='f']) # 0.85
                                           /sum(s$weight[s$gagnant_categorie!='Perdant' & s$variante_taxe_info=='f']))
proba_cru_gagnant_quand_simule_gagnant <- (sum(s$weight[s$simule_gagnant==1 & s$gagnant_categorie=='Gagnant' & s$variante_taxe_info=='f']) # 0.15
                                           /sum(s$weight[s$simule_gagnant==1 & s$variante_taxe_info=='f']))
proba_cru_perdant_quand_simule_perdant <- (sum(s$weight[s$simule_gagnant==0 & s$gagnant_categorie=='Perdant' & s$variante_taxe_info=='f']) # 0.78
                                           /sum(s$weight[s$simule_gagnant==0 & s$variante_taxe_info=='f']))
proba_cru_gagnant <- sum(s$weight[s$gagnant_categorie=='Gagnant' & s$variante_taxe_info=='f'])/sum(s$weight[s$variante_taxe_info=='f']) # 0.13
proba_cru_perdant <- sum(s$weight[s$gagnant_categorie=='Perdant' & s$variante_taxe_info=='f'])/sum(s$weight[s$variante_taxe_info=='f']) # 0.65

probas <- c()
for (cru in c('Perdant', 'Gagnant')) {
  probas[paste('cru', tolower(cru), sep='_')] <- length(which(s$gagnant_categorie==cru & s$variante_taxe_info=='f'))/length(which(s$variante_taxe_info=='f'))
  probas[paste('cru_non', tolower(cru), sep='_')] <- length(which(s$gagnant_categorie!=cru & s$variante_taxe_info=='f'))/length(which(s$variante_taxe_info=='f'))
  for (sim in c(0, 1)) {
    sim_nom <- 'perdant'
    if (sim==1) sim_nom <- 'gagnant'
    for (i in c('', 'non')) {
      for (j in c('', 'non')) {
        probas[gsub('__', '_', paste('simule', j, sim_nom, sep='_'))] <- (length(which((j=='non' | s$simule_gagnant==sim) & (j=='' | s$simule_gagnant!=sim) & 
                                                                      s$variante_taxe_info=='f'))/length(which(s$variante_taxe_info=='f'))) # TODO
        probas[gsub('__', '_', paste('simule', i, sim_nom, 'quand_cru', j, tolower(cru), sep='_'))] <- (length(which(s$variante_taxe_info=='f' & 
          (i=='non' | s$gagnant_categorie==cru) & (j=='non' | s$simule_gagnant==sim) & (i=='' | s$gagnant_categorie!=cru) & (j=='' | s$simule_gagnant!=sim)))/
            length(which(s$variante_taxe_info=='f' & (i=='non' | s$gagnant_categorie==cru) & (i=='' | s$gagnant_categorie!=cru))))
        probas[gsub('__', '_', paste('cru', i, tolower(cru), 'quand_simule', j, sim_nom, sep='_'))] <- (length(which(s$variante_taxe_info=='f' & 
          (i=='non' | s$gagnant_categorie==cru) & (j=='non' | s$simule_gagnant==sim) & (i=='' | s$gagnant_categorie!=cru) & (j=='' | s$simule_gagnant!=sim)))/
            length(which(s$variante_taxe_info=='f' & (j=='non' | s$simule_gagnant==sim) & (j=='' | s$simule_gagnant!=sim))))
      }
    }
  }
}

proba_gagnant_coherente_Bayes <- (5/6)*probas['simule_gagnant']/probas['simule_gagnant_quand_cru_gagnant'] # 0.72
probas['cru_gagnant_quand_simule_gagnant']
# Hypothèses: ils nous croient sur le 5/6, connaissent la proba de (simulés) gagnants et la proba conditionnelle de simulés gagnants lorsque subjectivement
# C'est un effet lié à l'info donnée (gagnant dans 5/6 cas où simule_gagnant), donc relatif au groupe de simulés gagnants vs. perdants.
# Une prior d'être gagnant à 73% aurait été cohérente avec Bayes et avec nos hypothèses, mais ils ne s'estiment gagnant qu'à 15%
# La somme des priors d'être gagnant et non gagnant n'a pas de raison de valoir 1, car elle ne concerne pas le même groupe de gens: 
# les uns sont simulés gagnants et les autres simulés perdants


proba_non_gagnant_coherente_Bayes <- (5/6)*probas['simule_non_gagnant']/probas['simule_non_gagnant_quand_cru_non_gagnant'] # 0.77
proba_perdant_coherente_Bayes <- (5/6)*probas['simule_perdant']/probas['simule_perdant_quand_cru_perdant'] # 0.70
proba_non_perdant_coherente_Bayes <- (5/6)*probas['simule_non_perdant']/probas['simule_non_perdant_quand_cru_non_perdant'] # 0.76

proba_cru_perdant_quand_simule_perdant = 5/6 = proba_cru_gagnant_quand_simule_gagnant

probas
probas_gagnant_coherente_Bayes <- (5/6)*probas['simule_gagnant']/probas['simule_gagnant_quand_cru_gagnant'] # 0.73
proba_non_gagnant_coherente_Bayes <- (5/6)*probas['simule_perdant']/probas['simule_non_gagnant_quand_cru_non_gagnant'] # 0.77
