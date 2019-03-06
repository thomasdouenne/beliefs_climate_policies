##### 1. Probabilité de se penser gagnant/perdant
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

##### 2. Probabilité de changer d'avis sur ses gains/pertes
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


##### 3. Probabilité d'approuver ou non
# ...


##### 4. Probabilité de changer d'avis sur l'approbation
# ...
