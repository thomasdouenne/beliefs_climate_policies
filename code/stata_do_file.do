*ssc install weakivtest
*ssc install avar
clear
insheet using "C:\Users\thoma\Documents\Github\beliefs_climate_policies\code\survey_prepared.csv", delimiter(";")

*encode taxe_approbation, generate(y_var)
*encode taxe_efficace, generate(x_var)
*encode apres_modifs, generate(z_var1)
*encode info_cc, generate(z_var2)

gen y_var = taxe_approbation != "Non"
gen x_var = taxe_efficace != "Non"
gen z_var1 = apres_modifs == "TRUE" 

ivregress 2sls y_var (x_var = z_var1 info_cc)
weakivtest
