### Info progressivité ###
s_pro <- subset(s, !is.na(taxe_progressif_approbation))

s_pro$dummy_approbation_avant <- 1*(s_pro$taxe_approbation=='Oui')
s_pro$dummy_approbation_apres <- 1*(s_pro$taxe_progressif_approbation=='Oui')

decrit(s_pro$dummy_approbation_avant)
decrit(s_pro$dummy_approbation_apres)

s_pro$diff_avant_apres <- s_pro$dummy_approbation_apres - s_pro$dummy_approbation_avant
decrit(s_pro$diff_avant_apres)
