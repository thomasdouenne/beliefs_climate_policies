set more off
clear
*#delimit ;

ssc install ivreg2
ssc install weakivtest
ssc install avar
ssc install ranktest
net from http://www.stata.com/users/bpoi/ 
net install condivreg

*insheet using "C:\Users\thoma\Documents\Github\beliefs_climate_policies\code\survey_prepared.csv", delimiter(";")
insheet using "/var/www/beliefs_climate_policies/code/survey_prepared.csv", delimiter(";")

*** Environmental effectiveness
encode csp, gen(ecsp)
encode taille_agglo, gen(etaille_agglo)
encode statut_emploi, gen(estatut_emploi)
encode region, gen(eregion)
encode actualite, gen(eactualite)
encode diplome4, gen(ediplome4)
encode gagnant_categorie, gen(egagnant_categorie)
encode prog_na, gen(eprog_na)

gen y_var1 = taxe_approbation != "Non"
gen x_var1 = taxe_efficace != "Non"
gen y_var2 = taxe_approbation == "Oui"
gen x_var2 = taxe_efficace == "Oui"
gen z_var1 = apres_modifs == "TRUE"
gen nb_adultes_1 = nb_adultes == 1
gen masculin = sexe == "Masculin"
gen fumeur = fume == "Oui"
destring nb_14_et_plus uc nb_adultes, replace dpcomma

*ivregress 2sls y_var (x_var = info_cc z_var1)
*ivregress 2sls y_var revenu (x_var = info_cc)
*ivregress 2sls y_var i.egagnant_categorie i.ediplome4 i.ecsp i.etaille_agglo i.estatut_emploi i.eregion i.eactualite revenu revenu2 revenu_conjoint revenu_conjoint2 simule_gain simule_gain2 nb_adultes_1 fumeur masculin nb_14_et_plus nb_adultes uc taille_menage niveau_vie age_18_24 age_25_34 age_35_49 age_50_64 (x_var = z_var1 info_cc)

* Not no without controls
ivreg2 y_var1 (x_var1 = z_var1 info_cc), robust
*weakivtest, level(0.05)
*This test does not converge (unable to compute critical values)

* Not no with controls
ivreg2 y_var1 revenu revenu2 revenu_conjoint revenu_conjoint2 nb_adultes_1 simule_gain simule_gain2 i.egagnant_categorie masculin i.estatut_emploi i.ecsp i.eregion ediplome4 taille_menage nb_14_et_plus nb_adultes fumeur i.eactualite etaille_agglo uc niveau_vie age_18_24 age_25_34 age_35_49 age_50_64 (x_var1 = z_var1 info_cc), robust
weakivtest, level(0.05)

* Yes with controls
ivreg2 y_var2 revenu revenu2 revenu_conjoint revenu_conjoint2 nb_adultes_1 simule_gain simule_gain2 i.egagnant_categorie masculin i.estatut_emploi i.ecsp i.eregion ediplome4 taille_menage nb_14_et_plus nb_adultes fumeur i.eactualite etaille_agglo uc niveau_vie age_18_24 age_25_34 age_35_49 age_50_64 (x_var2 = z_var1 info_cc)  [pweights=weight]
weakivtest, level(0.05)

** LIML
* Not no without controls
ivreg2 y_var1 (x_var1 = z_var1 info_cc), robust liml

* Not no with controls
ivreg2 y_var1 revenu revenu2 revenu_conjoint revenu_conjoint2 nb_adultes_1 simule_gain simule_gain2 i.egagnant_categorie masculin i.estatut_emploi i.ecsp i.eregion ediplome4 taille_menage nb_14_et_plus nb_adultes fumeur i.eactualite etaille_agglo uc niveau_vie age_18_24 age_25_34 age_35_49 age_50_64 (x_var1 = z_var1 info_cc), robust liml

* Yes without controls
ivreg2 y_var2 (x_var2 = z_var1 info_cc), robust liml

* Yes with controls
ivreg2 y_var2 revenu revenu2 revenu_conjoint revenu_conjoint2 nb_adultes_1 simule_gain simule_gain2 i.egagnant_categorie masculin i.estatut_emploi i.ecsp i.eregion ediplome4 taille_menage nb_14_et_plus nb_adultes fumeur i.eactualite etaille_agglo uc niveau_vie age_18_24 age_25_34 age_35_49 age_50_64 (x_var2 = z_var1 info_cc) [pweights=weight], robust liml /*partial(i.eregion) */



* not No ~ Yes without controls
ivreg2 y_var1 (x_var2 = z_var1 info_cc), robust liml

* not no ~ Yes with controls
ivreg2 y_var1 revenu revenu2 revenu_conjoint revenu_conjoint2 nb_adultes_1 simule_gain simule_gain2 i.egagnant_categorie masculin i.estatut_emploi i.ecsp i.eregion ediplome4 taille_menage nb_14_et_plus nb_adultes fumeur i.eactualite etaille_agglo uc niveau_vie age_18_24 age_25_34 age_35_49 age_50_64 (x_var2 = z_var1 info_cc) [pweights=weight], robust liml

** Conditional Likelihood Ratio (CLR) confidence sets (Moreira, 2003)
* Not no without controls: infinite confidence sets => cannot conclude that beta is identified (pi = 0 possible)
condivreg y_var1 (x_var1 = z_var1 info_cc)
condivreg y_var1 (x_var1 = z_var1 info_cc), level(85)
condivreg y_var1 (x_var1 = z_var1 info_cc), level(42)
condivreg y_var1 (x_var1 = info_cc) 
condivreg y_var1 (x_var1 = z_var1)

** Not no with controls
*condivreg y_var1 revenu revenu2 revenu_conjoint revenu_conjoint2 nb_adultes_1 simule_gain simule_gain2 i.egagnant_categorie masculin i.estatut_emploi i.ecsp i.eregion ediplome4 taille_menage nb_14_et_plus nb_adultes fumeur i.eactualite etaille_agglo uc niveau_vie age_18_24 age_25_34 age_35_49 age_50_64 (x_var1 = z_var1 info_cc)


*** Self-interest
gen x_var_si_1 = gagnant_cible_categorie != "Perdant"
gen y_var_si_1 = taxe_cible_approbation != "Non"
gen x_var_si_2 = gagnant_feedback_categorie != "Perdant"
gen y_var_si_2 = taxe_feedback_approbation != "Non"
gen traite_cible_interaction = traite_cible * traite_cible_conjoint
gen taxe_approbation_nsp = taxe_approbation == "NSP"
gen hausse_dep_uc = hausse_depenses_interaction / uc

* Target without controls (F-test : 43.321)
ivreg2 y_var_si_1 i.cible revenu revenu2 revenu_conjoint revenu_conjoint2 nb_adultes_1 (x_var_si_1 = traite_cible traite_cible_conjoint traite_cible_interaction)
weakivtest, level(0.05)

* Target with controls (F-test : 37.728)
ivreg2 y_var_si_1 i.cible y_var1 x_var1 x_var2 taxe_approbation_nsp hausse_dep_uc i.eprog_na revenu revenu2 revenu_conjoint revenu_conjoint2 nb_adultes_1 simule_gain simule_gain2 i.egagnant_categorie masculin i.estatut_emploi i.ecsp i.eregion ediplome4 taille_menage nb_14_et_plus nb_adultes fumeur i.eactualite etaille_agglo uc niveau_vie age_18_24 age_25_34 age_35_49 age_50_64 (x_var_si_1 = traite_cible traite_cible_conjoint traite_cible_interaction)
weakivtest, level(0.05)

* Firth Logit vs Logit
*logit y_var_si_1 i.x_var_si_1 i.cible i.y_var1 i.x_var1 i.x_var2 i.taxe_approbation_nsp hausse_dep_uc i.eprog_na revenu revenu2 revenu_conjoint revenu_conjoint2 nb_adultes_1 simule_gain simule_gain2 i.egagnant_categorie i.masculin i.estatut_emploi i.ecsp i.eregion i.ediplome4 i.taille_menage nb_14_et_plus nb_adultes i.fumeur i.eactualite etaille_agglo uc niveau_vie i.age_18_24 i.age_25_34 i.age_35_49 i.age_50_64
*firthlogit y_var_si_1 i.x_var_si_1 i.cible i.y_var1 i.x_var1 i.x_var2 i.taxe_approbation_nsp hausse_dep_uc i.eprog_na revenu revenu2 revenu_conjoint revenu_conjoint2 nb_adultes_1 simule_gain simule_gain2 i.egagnant_categorie i.masculin i.estatut_emploi i.ecsp i.eregion i.ediplome4 i.taille_menage nb_14_et_plus nb_adultes i.fumeur i.eactualite etaille_agglo uc niveau_vie i.age_18_24 i.age_25_34 i.age_35_49 i.age_50_64

* Feedback without controls (F-test : 37.966)
drop if variante_taxe_info != "f"
ivreg2 y_var_si_2 simule_gain simule_gain2 (x_var_si_2 = simule_gagnant)
weakivtest, level(0.05)

* Feedback with controls (F-test : 58.696)
ivreg2 y_var_si_2 y_var1 x_var1 x_var2 taxe_approbation_nsp i.eprog_na revenu revenu2 revenu_conjoint revenu_conjoint2 nb_adultes_1 simule_gain simule_gain2 i.egagnant_categorie masculin i.estatut_emploi i.ecsp i.eregion ediplome4 taille_menage nb_14_et_plus nb_adultes fumeur i.eactualite etaille_agglo uc niveau_vie age_18_24 age_25_34 age_35_49 age_50_64 (x_var_si_2 = simule_gagnant)
weakivtest, level(0.05)
