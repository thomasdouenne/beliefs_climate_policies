##### 1. Probabilité de se penser gagnant/perdant #####
# Description des variables
decrit(s$gagnant_feedback_categorie, weight = s$weight)
decrit(s$gagnant_categorie, weight = s$weight)
decrit(s$simule_gagnant, weight = s$weight)

# Si feedback gagnant : 47.9% se disent perdants / 27.9% non affectés / 24.2% gagnants
decrit(s$gagnant_feedback_categorie[s$simule_gagnant == 1], weight = s$weight[s$simule_gagnant == 1])
decrit(s$gagnant_categorie[s$simule_gagnant == 1], weight = s$weight[s$simule_gagnant == 1])

# Si feedback perdant : 85% se disent perdants / 12.9% non affectés / 2.2% gagnants
decrit(s$gagnant_feedback_categorie[s$simule_gagnant == 0], weight = s$weight[s$simule_gagnant == 0])
decrit(s$gagnant_categorie[s$simule_gagnant == 0], weight = s$weight[s$simule_gagnant == 0])

# TODO: refaire résultats en excluant les simule_gagnant_interaction != simule_gagnant_interaction
##### 2. Matrices de transition
decrit(s$gagnant_categorie[(s$simule_gagnant == 1)], weight = s$weight[(s$simule_gagnant == 1)])
decrit(s$gagnant_categorie[(s$simule_gagnant == 0)], weight = s$weight[(s$simule_gagnant == 0)])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1)], weight = s$weight[(s$simule_gagnant == 1)])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0)], weight = s$weight[(s$simule_gagnant == 0)])

# Si feedback gagnant et pensait être gagnant/ non affecté / perdant
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Gagnant')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Gagnant')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Non affecté')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Non affecté')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Perdant')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Perdant')])

# Si feedback perdant et pensait être gagnant/ non affecté / perdant
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Gagnant')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Gagnant')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Non affecté')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Non affecté')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Perdant')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Perdant')])

# Même chose chez les personnes approuvant la taxe :
decrit(s$gagnant_categorie[(s$simule_gagnant == 1) & (s$taxe_approbation == 'Oui')], weight = s$weight[(s$simule_gagnant == 1) & (s$taxe_approbation == 'Oui')])
decrit(s$gagnant_categorie[(s$simule_gagnant == 0) & (s$taxe_approbation == 'Oui')], weight = s$weight[(s$simule_gagnant == 0) & (s$taxe_approbation == 'Oui')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$taxe_approbation == 'Oui')], weight = s$weight[(s$simule_gagnant == 1) & (s$taxe_approbation == 'Oui')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$taxe_approbation == 'Oui')], weight = s$weight[(s$simule_gagnant == 0) & (s$taxe_approbation == 'Oui')])

# Si feedback gagnant et pensait être gagnant/ non affecté / perdant
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Gagnant') & (s$taxe_approbation == 'Oui')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Gagnant') & (s$taxe_approbation == 'Oui')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Non affecté') & (s$taxe_approbation == 'Oui')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Non affecté') & (s$taxe_approbation == 'Oui')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Perdant') & (s$taxe_approbation == 'Oui')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Perdant') & (s$taxe_approbation == 'Oui')])

# Si feedback perdant et pensait être gagnant/ non affecté / perdant
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Gagnant') & (s$taxe_approbation == 'Oui')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Gagnant') & (s$taxe_approbation == 'Oui')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Non affecté') & (s$taxe_approbation == 'Oui')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Non affecté') & (s$taxe_approbation == 'Oui')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Perdant') & (s$taxe_approbation == 'Oui')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Perdant') & (s$taxe_approbation == 'Oui')])

# Même chose chez les personnes acceptant la taxe :
decrit(s$gagnant_categorie[(s$simule_gagnant == 1) & (s$taxe_approbation != 'Non')], weight = s$weight[(s$simule_gagnant == 1) & (s$taxe_approbation != 'Non')])
decrit(s$gagnant_categorie[(s$simule_gagnant == 0) & (s$taxe_approbation != 'Non')], weight = s$weight[(s$simule_gagnant == 0) & (s$taxe_approbation != 'Non')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$taxe_approbation != 'Non')], weight = s$weight[(s$simule_gagnant == 1) & (s$taxe_approbation != 'Non')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$taxe_approbation != 'Non')], weight = s$weight[(s$simule_gagnant == 0) & (s$taxe_approbation != 'Non')])

# Si feedback gagnant et pensait être gagnant/ non affecté / perdant
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Gagnant') & (s$taxe_approbation != 'Non')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Gagnant') & (s$taxe_approbation != 'Non')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Non affecté') & (s$taxe_approbation != 'Non')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Non affecté') & (s$taxe_approbation != 'Non')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Perdant') & (s$taxe_approbation != 'Non')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Perdant') & (s$taxe_approbation != 'Non')])

# Si feedback perdant et pensait être gagnant/ non affecté / perdant
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Gagnant') & (s$taxe_approbation != 'Non')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Gagnant') & (s$taxe_approbation != 'Non')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Non affecté') & (s$taxe_approbation != 'Non')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Non affecté') & (s$taxe_approbation != 'Non')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Perdant') & (s$taxe_approbation != 'Non')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Perdant') & (s$taxe_approbation != 'Non')])

# Même chose chez les personnes n'acceptant pas la taxe :
decrit(s$gagnant_categorie[(s$simule_gagnant == 1) & (s$taxe_approbation == 'Non')], weight = s$weight[(s$simule_gagnant == 1) & (s$taxe_approbation == 'Non')])
decrit(s$gagnant_categorie[(s$simule_gagnant == 0) & (s$taxe_approbation == 'Non')], weight = s$weight[(s$simule_gagnant == 0) & (s$taxe_approbation == 'Non')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$taxe_approbation == 'Non')], weight = s$weight[(s$simule_gagnant == 1) & (s$taxe_approbation == 'Non')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$taxe_approbation == 'Non')], weight = s$weight[(s$simule_gagnant == 0) & (s$taxe_approbation == 'Non')])

# Si feedback gagnant et pensait être gagnant/ non affecté / perdant
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Gagnant') & (s$taxe_approbation == 'Non')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Gagnant') & (s$taxe_approbation == 'Non')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Non affecté') & (s$taxe_approbation == 'Non')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Non affecté') & (s$taxe_approbation == 'Non')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Perdant') & (s$taxe_approbation == 'Non')], weight = s$weight[(s$simule_gagnant == 1) & (s$gagnant_categorie == 'Perdant') & (s$taxe_approbation == 'Non')])

# Si feedback perdant et pensait être gagnant/ non affecté / perdant
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Gagnant') & (s$taxe_approbation == 'Non')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Gagnant') & (s$taxe_approbation == 'Non')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Non affecté') & (s$taxe_approbation == 'Non')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Non affecté') & (s$taxe_approbation == 'Non')])
decrit(s$gagnant_feedback_categorie[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Perdant') & (s$taxe_approbation == 'Non')], weight = s$weight[(s$simule_gagnant == 0) & (s$gagnant_categorie == 'Perdant') & (s$taxe_approbation == 'Non')])


##### 3. Probabilité de changer d'avis sur ses gains/pertes #####
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

probas <- c()
for (cru in c('Perdant', 'Gagnant')) {
  probas[paste('cru', tolower(cru), sep='_')] <- length(which(s$gagnant_categorie==cru & s$variante_taxe_info=='f'))/length(which(s$variante_taxe_info=='f'))
  probas[paste('cru_non', tolower(cru), sep='_')] <- length(which(s$gagnant_categorie!=cru & s$variante_taxe_info=='f'))/length(which(s$variante_taxe_info=='f'))
  probas[paste('feedback_cru', tolower(cru), sep='_')] <- length(which(s$gagnant_feedback_categorie==cru & s$variante_taxe_info=='f'))/length(which(s$variante_taxe_info=='f'))
  probas[paste('feedback_cru_non', tolower(cru), sep='_')] <- length(which(s$gagnant_feedback_categorie!=cru & s$variante_taxe_info=='f'))/length(which(s$variante_taxe_info=='f'))
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
        probas[gsub('__', '_', paste('simule', i, sim_nom, 'quand_feedback_cru', j, tolower(cru), sep='_'))] <- (length(which(s$variante_taxe_info=='f' & 
          (i=='non' | s$gagnant_feedback_categorie==cru) & (j=='non' | s$simule_gagnant==sim) & (i=='' | s$gagnant_feedback_categorie!=cru) & (j=='' | s$simule_gagnant!=sim)))/
            length(which(s$variante_taxe_info=='f' & (i=='non' | s$gagnant_feedback_categorie==cru) & (i=='' | s$gagnant_categorie!=cru))))
        probas[gsub('__', '_', paste('cru', i, tolower(cru), 'quand_simule', j, sim_nom, sep='_'))] <- (length(which(s$variante_taxe_info=='f' & 
          (i=='non' | s$gagnant_categorie==cru) & (j=='non' | s$simule_gagnant==sim) & (i=='' | s$gagnant_categorie!=cru) & (j=='' | s$simule_gagnant!=sim)))/
            length(which(s$variante_taxe_info=='f' & (j=='non' | s$simule_gagnant==sim) & (j=='' | s$simule_gagnant!=sim))))
        probas[gsub('__', '_', paste('feedback_cru', i, tolower(cru), 'quand_simule', j, sim_nom, sep='_'))] <- (length(which(s$variante_taxe_info=='f' & 
          (i=='non' | s$gagnant_feedback_categorie==cru) & (j=='non' | s$simule_gagnant==sim) & (i=='' | s$gagnant_feedback_categorie!=cru) & (j=='' | s$simule_gagnant!=sim)))/
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


probas
probas['cru_gagnant'] # 0.14
probas['feedback_cru_gagnant'] # 0.19
probas['feedback_cru_gagnant_quand_simule_gagnant'] # 0.24
probas_gagnant_coherente_Bayes <- as.numeric((5/6)*probas['simule_gagnant']/probas['simule_gagnant_quand_cru_gagnant']) # 0.73
probas_gagnant_coherente_Bayes # 0.73
probas['feedback_cru_gagnant_quand_simule_gagnant']*probas['simule_gagnant']/probas['simule_gagnant_quand_feedback_cru_gagnant'] # 0.19
probas['cru_gagnant_quand_simule_gagnant']*probas['simule_gagnant']/probas['simule_gagnant_quand_feedback_cru_gagnant'] # 0.21
probas['feedback_cru_gagnant_quand_simule_gagnant']*probas['simule_gagnant']/probas['simule_gagnant_quand_cru_gagnant'] # 0.14
probas['cru_gagnant_quand_simule_gagnant']*probas['simule_gagnant']/probas['simule_gagnant_quand_cru_gagnant'] # 0.19

probas['cru_perdant'] # 0.66
probas['feedback_cru_perdant'] # 0.57
probas['feedback_cru_perdant_quand_simule_gagnant'] # 0.48
proba_perdant_coherente_Bayes <- as.numeric((5/6)*probas['simule_perdant']/probas['simule_perdant_quand_cru_perdant']) # 0.70
proba_perdant_coherente_Bayes # 0.72
probas['cru_perdant_quand_simule_perdant']*probas['simule_perdant']/probas['simule_perdant_quand_cru_perdant'] # 0.66

proba_non_gagnant_coherente_Bayes <- (5/6)*probas['simule_non_gagnant']/probas['simule_non_gagnant_quand_cru_non_gagnant'] # 0.77
proba_non_perdant_coherente_Bayes <- (5/6)*probas['simule_non_perdant']/probas['simule_non_perdant_quand_cru_non_perdant'] # 0.76


##### 6. Régressions pour estimer effet feedback et asymétrie
s$update <- 1*(((s$gagnant_feedback_categorie != 'Perdant') - (s$gagnant_categorie != 'Perdant')) != 0)
s$update <- 1*(s$gagnant_feedback_categorie != s$gagnant_categorie)

summary(lm(((gagnant_feedback_categorie != 'Perdant') - (gagnant_categorie != 'Perdant')) ~ simule_gagnant, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f']))
summary(lm(((gagnant_feedback_categorie != 'Gagnant') - (gagnant_categorie != 'Gagnant')) ~ simule_gagnant, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f']))

summary(lm(update ~ simule_gagnant * gagnant_categorie, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f']))
probitmarg <- probitmfx(update ~ simule_gagnant * gagnant_categorie, data = s[s$variante_taxe_info=='f',], atmean = TRUE)
probitmarg

summary(lm(update ~ simule_gagnant, data=s[s$variante_taxe_info=='f',], weights = s$weight[s$variante_taxe_info=='f']))
# s$gagnant_categorie <- relevel(as.factor(s$gagnant_categorie), 'Non affecté')
summary(lm(((gagnant_feedback_categorie != 'Perdant') - (gagnant_categorie != 'Perdant')) ~ gagnant_categorie, data=s[s$simule_gagnant==1,], weights = s$weight[s$simule_gagnant==1]))
summary(lm(((gagnant_feedback_categorie != 'Gagnant') - (gagnant_categorie != 'Gagnant')) ~ gagnant_categorie, data=s[s$simule_gagnant==0,], weights = s$weight[s$simule_gagnant==0]))

summary(lm(((gagnant_feedback_categorie == 'Gagnant') - (gagnant_categorie == 'Gagnant')) ~ gagnant_categorie, data=s[s$simule_gagnant==1,], weights = s$weight[s$simule_gagnant==1]))
summary(lm(((gagnant_feedback_categorie == 'Perdant') - (gagnant_categorie == 'Perdant')) ~ gagnant_categorie, data=s[s$simule_gagnant==0,], weights = s$weight[s$simule_gagnant==0]))

summary(lm(((gagnant_feedback_categorie == 'Perdant')) ~ (gagnant_categorie == 'Gagnant') + (gagnant_categorie == 'Perdant'), data=s[s$simule_gagnant==0,], weights = s$weight[s$simule_gagnant==0]))
summary(lm(((gagnant_feedback_categorie == 'Gagnant')) ~ (gagnant_categorie == 'Gagnant') + (gagnant_categorie == 'Perdant'), data=s[s$simule_gagnant==1,], weights = s$weight[s$simule_gagnant==1]))

summary(lm(update ~ gagnant_categorie, data=s[s$simule_gagnant==1,], weights = s$weight[s$simule_gagnant==1]))
summary(lm(update ~ gagnant_categorie, data=s[s$simule_gagnant==0,], weights = s$weight[s$simule_gagnant==0]))
# Interpréter tout ça


library(stargazer)
library(broom)

feedback_lose <- lm(((gagnant_feedback_categorie == 'Perdant')) ~ (gagnant_categorie == 'Gagnant') + (gagnant_categorie == 'Perdant'), data=s[s$simule_gagnant==0,], weights = s$weight[s$simule_gagnant==0])
summary(feedback_lose)
feedback_win <- lm(((gagnant_feedback_categorie == 'Gagnant')) ~ (gagnant_categorie == 'Gagnant') + (gagnant_categorie == 'Perdant'), data=s[s$simule_gagnant==1,], weights = s$weight[s$simule_gagnant==1])
summary(feedback_win)

stargazer(feedback_lose, feedback_win, type="latex",
          dep.var.labels=c("G = G"),
          covariate.labels=c("Estimated winner", "Estimated loser"))

decrit((s[s$simule_gagnant==1,]$gagnant_categorie == 'Gagnant'), weights=s[s$simule_gagnant==1,]$weight)
decrit((s[s$simule_gagnant==1,]$gagnant_categorie == 'Perdant'), weights=s[s$simule_gagnant==1,]$weight)

decrit((s[s$simule_gagnant==0,]$gagnant_categorie == 'Gagnant'), weights=s[s$simule_gagnant==0,]$weight)
decrit((s[s$simule_gagnant==0,]$gagnant_categorie == 'Perdant'), weights=s[s$simule_gagnant==0,]$weight)

var((s[s$simule_gagnant==1,]$gagnant_categorie == 'Gagnant'))
var((s[s$simule_gagnant==1,]$gagnant_categorie == 'Perdant'))
cov((s[s$simule_gagnant==1,]$gagnant_categorie == 'Gagnant'), (s[s$simule_gagnant==1,]$gagnant_categorie == 'Perdant'))

var((s[s$simule_gagnant==0,]$gagnant_categorie == 'Gagnant'))
var((s[s$simule_gagnant==0,]$gagnant_categorie == 'Perdant'))
cov((s[s$simule_gagnant==0,]$gagnant_categorie == 'Gagnant'), (s[s$simule_gagnant==0,]$gagnant_categorie == 'Perdant'))


##### 7. Use binomial law to compute confidence intervals around share of respondents
library("Hmisc")
# Simulés gagnants :
x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Gagnant' & s$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Gagnant',]$weight)
binconf(x = x, n = n)

x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Non affecté' & s$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Non affecté',]$weight)
binconf(x = x, n = n)

x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Perdant' & s$gagnant_feedback_categorie == 'Gagnant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==1 & s$gagnant_categorie == 'Perdant',]$weight)
binconf(x = x, n = n)

# Simulés perdants :
x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Gagnant' & s$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Gagnant',]$weight)
binconf(x = x, n = n)

x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Non affecté' & s$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Non affecté',]$weight)
binconf(x = x, n = n)

x = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Perdant' & s$gagnant_feedback_categorie == 'Perdant',]$weight)
n = sum(s[s$variante_taxe_info == 'f' & s$simule_gagnant==0 & s$gagnant_categorie == 'Perdant',]$weight)
binconf(x = x, n = n)
