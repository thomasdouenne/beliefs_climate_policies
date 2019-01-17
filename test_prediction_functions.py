# -*- coding: utf-8 -*-

# We select a subsample of households from BdF, and apply our different methods to determine the feedbacks to give
# We will then compare the goodness of fit of these methods.
# There is a very large downward bias in our regression, we clearly overestimate the losses

from __future__ import division
import time

start = time.time()

from model_reforms_data.gains_losses_data import compute_gains_losses
from model_reforms_data.gains_losses_per_categ import match_households_per_categ
from model_reforms_data.prepare_dataset import prepare_dataset
from model_reforms_data.prepare_dataset_housing import prepare_dataset_housing
from model_reforms_data.regression_feedback import compute_gains_losses_housing, \
    regress_ols_housing_expenditures_increase, predict_winner_looser_housing
from model_reforms.diesel_standard_example import diesel_example
from model_reforms.domestic_fuel_standard_example import domestic_fuel_example
from model_reforms.gas_standard_example import natural_gas_example
from model_reforms.gasoline_standard_example import gasoline_example
from probability_to_win_housing import compute_probability_to_win


df_bdf = prepare_dataset_housing('bdf')
df_bdf = compute_gains_losses_housing(df_bdf)
df_bdf = df_bdf.sample(200) # Select a random sample

df_enl = prepare_dataset_housing('enl')
df_enl = compute_gains_losses_housing(df_enl)

df_bdf['winner_housing'] = 0 + 1 * (df_bdf['housing_expenditures_increase'] < 55 * df_bdf['nb_beneficiaries'])
df_enl['winner_housing'] = 0 + 1 * (df_enl['housing_expenditures_increase'] < 55 * df_enl['nb_beneficiaries'])

results_regressions = predict_winner_looser_housing(df_enl)
    
hh_info = dict()
df_bdf['predict_proba_logit'] = 0.0
df_bdf['predict_proba_probit'] = 0.0
df_bdf['predict_proba_ols'] = 0.0

for index in df_bdf.iterrows():
    i = index[0]
    hh_info['nb_beneficiaries'] = df_bdf['nb_beneficiaries'][i]
    hh_info['consumption_units'] = df_bdf['consumption_units'][i]
    hh_info['domestic_fuel'] = df_bdf['domestic_fuel'][i]
    hh_info['natural_gas'] = df_bdf['natural_gas'][i]
    hh_info['accommodation_size'] = df_bdf['accommodation_size'][i]
    hh_info['hh_income'] = df_bdf['hh_income'][i]
    hh_info['hh_income_2'] = hh_info['hh_income'] ** 2
    hh_info['Intercept'] = 1
    
    hh_info['age_18_24'] = df_bdf['age_18_24'][i]
    hh_info['age_25_34'] = df_bdf['age_25_34'][i]
    hh_info['age_35_49'] = df_bdf['age_35_49'][i]
    hh_info['age_50_64'] = df_bdf['age_50_64'][i]

    dict_loss = compute_probability_to_win(df_enl, hh_info, results_regressions)
    
    for method in ['logit', 'probit', 'ols']:
        df_bdf['predict_proba_{}'.format(method)][i] = dict_loss['predict_proba_{}'.format(method)]

for method in ['logit', 'probit', 'ols']:
    df_bdf['winner_from_regression_{}'.format(method)] = \
        0 + 1 * (df_bdf['predict_proba_{}'.format(method)] > 0.5)

    df_bdf['mistake_{}'.format(method)] = \
        1 * ((df_bdf['winner_housing'] - df_bdf['winner_from_regression_{}'.format(method)]) != 0)

    print "Share of mistakes with {}:".format(method), (float(len(df_bdf.query('mistake_{} == 1'.format(method)))) / len(df_bdf))


end = time.time()
print "###"
print "Computation time:", end - start, "seconds"

# How to solve this issue?
print "###"

print "Winning probability from BdF:", df_bdf['winner_housing'].mean()
print "Winning probability from EL:", df_enl['winner_housing'].mean()
print "Winning probability from Logit:", df_bdf['winner_from_regression_logit'].mean()
print "Winning probability from Probit:", df_bdf['winner_from_regression_probit'].mean()
print "Winning probability from OLS:", df_bdf['winner_from_regression_ols'].mean()

print "###"

print "Avg housing exp increase from BdF:", df_bdf['housing_expenditures_increase'].mean()
print "Avg housing exp increase from EL:", df_enl['housing_expenditures_increase'].mean() # Almost the same (same inflators)

print "###"

print "Avg income from BdF:", df_bdf['hh_income'].mean()
print "Avg income from EL:", df_enl['hh_income'].mean() # Not the same at all (different definitions)

print "###"

print "Avg acc. size from BdF:", df_bdf['accommodation_size'].mean()
print "Avg acc.size from EL:", df_enl['accommodation_size'].mean()

print "###"

print "Avg c.u. from BdF:", df_bdf['consumption_units'].mean()
print "Avg c.u. from EL:", df_enl['consumption_units'].mean()

print "###"

print "Avg natural gas from BdF:", df_bdf['natural_gas'].mean()
print "Avg natural gas from EL:", df_enl['natural_gas'].mean()

print "###"

print "Avg domestic fuel from BdF:", df_bdf['domestic_fuel'].mean()
print "Avg domestic fuel from EL:", df_enl['domestic_fuel'].mean()

print "###"

print "Avg domestic fuel from BdF:", df_bdf['nb_beneficiaries'].mean()
print "Avg domestic fuel from EL:", df_enl['nb_beneficiaries'].mean()
