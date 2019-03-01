# -*- coding: utf-8 -*-

from __future__ import division

import pandas as pd
import seaborn
import statsmodels.formula.api as smf

from model_reforms_data.prepare_dataset import prepare_dataset
from model_reforms_data.gains_losses_data import compute_gains_losses
from model_reforms_data.standardize_data_bdf_ptc import variables_names_bdf_to_ptc, \
    create_new_variables_bdf_ptc, compute_gains_nets_uc, compare_objective_subjective_beliefs_gains, \
    impute_barycentre_in_bins, impute_average_bdf_in_bins, extrapolate_distribution_bcp_from_bdf

seaborn.set_palette(seaborn.color_palette("Set1", 2))


""" Load samples - BdF/BCP """
df_hh = prepare_dataset()
try:
    df_ptc = pd.read_csv(r'C:\Users\thoma\Documents\Github\beliefs_climate_policies\code\survey_prepared.csv')
except:
    df_ptc = pd.read_csv(r'C:\Users\t.douenne\Documents\Github\beliefs_climate_policies\code\survey_prepared.csv')

df_hh = compute_gains_losses(df_hh)
df_hh = compute_gains_nets_uc(df_hh)
df_hh = variables_names_bdf_to_ptc(df_hh)
(df_hh, df_ptc) = create_new_variables_bdf_ptc(df_hh, df_ptc)
df_hh['weight'] = 1

df_ptc['weight'] = 1
df_ptc['gain_taxe_carbone'] = df_ptc['gain']


""" Define sub-samples to study """
variable = 'gaz'
sign = '=='
value = None
try:
    df_hh = df_hh.query('{0} {1} {2}'.format(variable, sign, value))
    df_ptc = df_ptc.query('{0} {1} {2}'.format(variable, sign, value))
    print "### We have selected a sub-sample from the original datasets:"
except:
    print "### Estimation on full samples:"


""" Choose which function to perform """
plot_step_distribution = True
plot_kde = False
test_imputationn_methods = False # The two methods provide quite similar results
regress = False


""" Compare distributions BdF/BCP """

energy = 'taxe_carbone'

if plot_step_distribution == True:
    df_to_plot = compare_objective_subjective_beliefs_gains(df_hh, df_ptc, energy, True)    
if plot_kde == True:
    extrapolate_distribution_bcp_from_bdf(df_hh, df_ptc, energy, bw_size = 0.4)


""" Imputate numerical values for net gains for BCP """
df_ptc = impute_barycentre_in_bins(df_hh, df_ptc) 
df_ptc = impute_average_bdf_in_bins(df_hh, df_ptc) 

if test_imputationn_methods == True:
    print "Sample means:", df_ptc['gain_net_numeric_uc_{}'.format(energy)].mean(), \
        df_ptc['gain_net_numeric_barycentre_uc_{}'.format(energy)].mean()
    for i in range(-6,6):
        print i, "Average BdF: ", df_ptc.query('gain_{0} == {1}'.format(energy, i))['gain_net_numeric_uc_{}'.format(energy)].mean(), \
            "/ Barycentre method:", df_ptc.query('gain_{0} == {1}'.format(energy, i))['gain_net_numeric_barycentre_uc_{}'.format(energy)].mean()


""" Regress expected net gains on various households characteristics """
if regress == True:
    data = df_ptc
    
    data['revenu_2'] = data['revenu'] ** 2
    data['rev_tot_2'] = data['rev_tot'] ** 2
    
    ols_net_gains = smf.ols(formula = 'gain_net_numeric_uc_{} ~ \
        revenu + revenu_2 + rev_tot + rev_tot_2 + fioul + gaz + taille_menage + surface + \
        age_18_24 + age_25_34 + age_35_49 + age_50_64'.format(energy),
        data = data).fit() # To do : add more variables
    print ols_net_gains.summary()
