# -*- coding: utf-8 -*-

from __future__ import division

import pandas as pd
import seaborn
import statsmodels.formula.api as smf
import numpy as np

from model_reforms_data.prepare_dataset import prepare_dataset
from model_reforms_data.gains_losses_data import compute_gains_losses
from model_reforms_data.standardize_data_bdf_ptc import variables_names_bdf_to_ptc, \
    create_new_variables_bdf_ptc, compute_gain_net_uc, compare_objective_subjective_beliefs_gain, \
    impute_barycentre_in_bins, impute_average_bdf_in_bins, extrapolate_distribution_bcp_from_bdf, \
    save_dataframes_kernel_density, compute_effort_rate_decile

seaborn.set_palette(seaborn.color_palette([u'blue', u'red']))


""" Load samples - BdF/BCP """
df_bdf = prepare_dataset()
df_ptc = pd.read_csv(r'code/survey_prepared.csv', sep = ";", decimal = ',')
#try:
#    df_ptc = pd.read_csv(r'../code/survey_prepared.csv')
#except:
#    df_ptc = pd.read_csv(r'..\code\survey_prepared.csv')

df_bdf = compute_gains_losses(df_bdf)
df_bdf = compute_gain_net_uc(df_bdf)
df_bdf = variables_names_bdf_to_ptc(df_bdf)
#(df_bdf, df_ptc) = create_new_variables_bdf_ptc(df_bdf, df_ptc)
#df_bdf['weight'] = 1

#df_ptc['weight'] = 1
df_ptc['gain_taxe_carbone_echelle'] = df_ptc['gain_echelle']


""" Define policy to study """ # chauffage, fuel ou taxe_carbone
energy = 'taxe_carbone'


""" Define sub-samples to study """ # Put None to variable, value or sign to get full sample
variable = 'gaz'
sign = '=='
value = None
try:
    df_bdf = df_bdf.query('{0} {1} {2}'.format(variable, sign, value))
    df_ptc = df_ptc.query('{0} {1} {2}'.format(variable, sign, value))
    print "### We have selected a sub-sample from the original datasets:"
except:
    print "### Estimation on full samples:"


""" Choose which function to perform """ # True to compute and display results, False otherwise
plot_step_distribution = False
plot_kde = False
save_kde_data = False
test_imputation_methods = False # The two methods provide quite similar results
regress = False
number_winners = True
effort_rate = False
prop_constrained = True


""" Compare distributions BdF/BCP """ # pdf and CDF net gains of both samples (step function or Kernel density estimation)

if plot_step_distribution == True:
    df_to_plot = compare_objective_subjective_beliefs_gain(df_bdf, df_ptc, energy, True)    
if plot_kde == True:
    extrapolate_distribution_bcp_from_bdf(df_bdf, df_ptc, energy, bw_size = 0.3, vector = True)
if save_kde_data == True:
    save_dataframes_kernel_density(df_bdf, df_ptc)

""" Imputate numerical values for net gain for BCP """ # Two different imputation methods: provide very similar results
df_ptc = impute_barycentre_in_bins(df_bdf, df_ptc) 
df_ptc = impute_average_bdf_in_bins(df_bdf, df_ptc)

if test_imputation_methods == True:
    print "Sample means:", df_ptc['gain_net_numeric_uc_{}'.format(energy)].mean(), \
        df_ptc['gain_net_numeric_barycentre_uc_{}'.format(energy)].mean()
    for i in range(-6,6):
        print i, "Average BdF: ", df_ptc.query('gain_{0}_echelle == {1}'.format(energy, i))['gain_net_numeric_uc_{}'.format(energy)].mean(), \
            "/ Barycentre method:", df_ptc.query('gain_{0}_echelle == {1}'.format(energy, i))['gain_net_numeric_barycentre_uc_{}'.format(energy)].mean()


""" Regress expected net gain on various households characteristics """
if regress == True:
    data = df_ptc
    
    data['revenu_2'] = data['revenu'] ** 2
    data['rev_tot_2'] = data['rev_tot'] ** 2
    
    ols_net_gain = smf.ols(formula = 'gain_net_numeric_uc_{} ~ \
        revenu + revenu_2 + rev_tot + rev_tot_2 + fioul + gaz + taille_menage + surface + \
        age_18_24 + age_25_34 + age_35_49 + age_50_64'.format(energy),
        data = data).fit() # To do : add more variables
    print ols_net_gain.summary()
    # Results are barely sensitive to choice of imputation method for numeric net gain


""" Number of winners and average expected net gains"""
if number_winners == True:
    print float((df_ptc['gain_net_numeric_uc_fuel'] * df_ptc['weight']).sum()) / (df_ptc['weight'] * (1-1*np.isnan(df_ptc['gain_fuel_echelle']))).sum()
    print float((df_ptc['gain_net_numeric_uc_chauffage'] * df_ptc['weight']).sum()) / (df_ptc['weight'] * (1-1*np.isnan(df_ptc['gain_chauffage_echelle']))).sum()
    print float((df_ptc['gain_net_numeric_uc_taxe_carbone'] * df_ptc['weight']).sum()) / df_ptc['weight'].sum()
    
    print float((df_bdf['gain_net_numeric_uc_fuel'] * df_bdf['weight']).sum()) / df_bdf['weight'].sum()
    print float((df_bdf['gain_net_numeric_uc_chauffage'] * df_bdf['weight']).sum()) / df_bdf['weight'].sum()
    print float((df_bdf['gain_net_numeric_uc_taxe_carbone'] * df_bdf['weight']).sum()) / df_bdf['weight'].sum()
    
    print float(((df_ptc['gain_net_numeric_uc_fuel'] > 0) * df_ptc['weight']).sum()) / df_ptc['weight'].sum()
    print float(((df_ptc['gain_net_numeric_uc_chauffage'] > 0) * df_ptc['weight']).sum()) / df_ptc['weight'].sum()
    print float(((df_ptc['gain_net_numeric_uc_taxe_carbone'] > 0) * df_ptc['weight']).sum()) / df_ptc['weight'].sum()
    
    print float(((df_bdf['gain_net_numeric_uc_fuel'] > 0) * df_bdf['weight']).sum()) / df_bdf['weight'].sum()
    print float(((df_bdf['gain_net_numeric_uc_chauffage'] > 0) * df_bdf['weight']).sum()) / df_bdf['weight'].sum()
    print float(((df_bdf['gain_net_numeric_uc_taxe_carbone'] > 0) * df_bdf['weight']).sum()) / df_bdf['weight'].sum()


""" Effort rate decile """
if effort_rate == True:
    compute_effort_rate_decile(df_bdf, energy)

""" Proportion of households more constrained than average """
if prop_constrained == True:
    mean_transport_expenditure_increase = (df_bdf['transport_expenditures_increase'] * df_bdf['weight']).sum() / df_bdf['weight'].sum()
    print float(((df_bdf['transport_expenditures_increase'] < mean_transport_expenditure_increase) * df_bdf['weight']).sum()) / df_bdf['weight'].sum()
    mean_housing_expenditure_increase = (df_bdf['transport_expenditures_increase'] * df_bdf['weight']).sum() / df_bdf['weight'].sum()
    print float(((df_bdf['housing_expenditures_increase'] < mean_housing_expenditure_increase) * df_bdf['weight']).sum()) / df_bdf['weight'].sum()
