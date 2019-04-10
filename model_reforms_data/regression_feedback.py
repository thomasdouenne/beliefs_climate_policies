# -*- coding: utf-8 -*-

# Careful : use nb of adults instead of cu.

from __future__ import division

import statsmodels.formula.api as smf
import numpy as np
import pandas as pd
from sklearn import tree


from prepare_dataset_housing import prepare_dataset_housing
from define_tax_incidence_data import *


def compute_gains_losses_housing(df_hh):

    initial_variables = df_hh.columns.tolist()    
    for element in ['domestic_fuel', 'natural_gas_variable']:
    
        """ Fix parameters : """
        
        i = 0.8
        current_carbon_price = 44.6 # Carbon tax in 2018
        adjusted_carbon_price = 44.6 + 50 # Carbon tax that we simulate
            
        if element == 'domestic_fuel':
            current_price = 0.859 # This is roughly the value of domestic fuel prices
            e = -0.15
            carbon_intensity = 0.00265
            initial_excise_tax = 0.038 # This is roughly the value of the TICPE without carbon tax, but I need to check more precisly
        else:
            current_price = 0.0651 # For someone in zone 3 that use gas for heating
            e = -0.15
            carbon_intensity = 0.000182
            initial_excise_tax = 0.0003 # Cf. excel, level of the TICGN if carbon price was null
        
        """ Compute remaining parameters : """
        adjusted_carbon_tax = carbon_tax(adjusted_carbon_price, carbon_intensity)
        current_carbon_tax = carbon_tax(current_carbon_price, carbon_intensity)
        
        adjusted_excise_tax = excise_tax(adjusted_carbon_tax, initial_excise_tax)
        current_excise_tax = excise_tax(current_carbon_tax, initial_excise_tax)
        
        new_final_price = final_price_adjusted(current_price, i, adjusted_excise_tax, current_excise_tax)
        growth_final_price = variation_final_price(i, current_price, adjusted_excise_tax, current_excise_tax)
        
        current_price_without_tax = price_without_tax(current_price, current_excise_tax)
        new_price_without_tax = price_without_tax(new_final_price, adjusted_excise_tax)
    
    
        """ Compute tax incidence : """
        df_hh['{}_quantity'.format(element)] = df_hh['{}_expenditures'.format(element)] / current_price
        df_hh = adjusted_quantity_data(e, growth_final_price, df_hh, element)
    
        df_hh = adjusted_expenditures_data(e, growth_final_price, df_hh, element)
        df_hh['{}_expenditures_increase'.format(element)] = df_hh['{}_adjusted_expenditures'.format(element)] - df_hh['{}_expenditures'.format(element)]
    
        df_hh = taxes_data(current_price_without_tax, current_excise_tax, df_hh, element)
        df_hh = taxes_data(new_price_without_tax, adjusted_excise_tax, df_hh, '{}_adjusted'.format(element))
        df_hh['{}_tax_increase'.format(element)] = df_hh['{}_adjusted_taxes'.format(element)] - df_hh['{}_taxes'.format(element)]
    
    
    df_hh['housing_expenditures_increase'] = (
        df_hh['domestic_fuel_expenditures_increase'] + df_hh['natural_gas_variable_expenditures_increase']
        )
    df_hh['housing_tax_increase'] = (
        df_hh['domestic_fuel_tax_increase'] + df_hh['natural_gas_variable_tax_increase']
        )

    try:
        df_hh['nb_adults'] = df_hh['nb_persons'] - df_hh['nb_children']
    except:
        df_hh['nb_adults'] = df_hh['plus_18']
    df_hh['nb_beneficiaries'] = 2 - 1 * (df_hh['nb_adults'] == 1)
    
    return df_hh[initial_variables + ['housing_expenditures_increase'] + ['housing_tax_increase'] + ['nb_beneficiaries']]


def regress_ols_housing_expenditures_increase(df_hh):

    df_hh['hh_income_2'] = df_hh['hh_income'] ** 2
    df_hh['accommodation_size_2'] = df_hh['accommodation_size'] ** 2

    regression_ols = smf.ols(formula = 'housing_expenditures_increase ~ \
        natural_gas + domestic_fuel + accommodation_size',
#        natural_gas : accommodation_size + domestic_fuel + domestic_fuel : accommodation_size - 1',
        data = df_hh).fit()
    
    alpha = 90.
    prediction_intervals = regression_ols.get_prediction(df_hh, weights=1).summary_frame(alpha=(100-alpha)/100)
    print 'average prediction interval size (at ', alpha, '%):',  round(np.mean(prediction_intervals['obs_ci_upper'] - prediction_intervals['obs_ci_lower'])), '(variance:', np.round(np.var(prediction_intervals['obs_ci_upper'] - prediction_intervals['obs_ci_lower']),3), ')'
    print 'proportion with loss prediction interval above 0 (resp. below 0):', np.round(sum(prediction_intervals['obs_ci_lower'] > 0)/len(prediction_intervals['obs_ci_upper']), 3), sum(prediction_intervals['obs_ci_upper'] < 0)/len(prediction_intervals['obs_ci_upper'])
    print 'proportion with loss prediction interval above 110 (resp. below 110):', np.round(sum(prediction_intervals['obs_ci_lower'] > 110)/len(prediction_intervals['obs_ci_upper']), 3), sum(prediction_intervals['obs_ci_upper'] < 110)/len(prediction_intervals['obs_ci_upper'])
    print 'proportion with zero housing expenditure increase:', np.round(sum(df_hh['housing_expenditures_increase'] == 0)/len(df_hh['housing_expenditures_increase']), 3)
#    print prediction_intervals    
    
    pd.DataFrame({'obj': df_hh['housing_expenditures_increase'], 'fit':prediction_intervals['mean'], 'nb_adultes': df_hh['nb_beneficiaries']}).to_csv('prediction housing expenditures.csv')

#    import statsmodels.api as sm
#    import matplotlib.pyplot as plt
#    X = np.column_stack((df_hh['natural_gas']*df_hh['accommodation_size'], df_hh['domestic_fuel'], df_hh['domestic_fuel']* df_hh['accommodation_size']))
#    X = sm.add_constant(X)
#    ols = sm.OLS(df_hh['housing_expenditures_increase'], X).fit()
#    loess = sm.nonparametric.lowess([1*(x < 110) for x in ols.predict(X)], - df_hh['housing_expenditures_increase'], delta=50)
##    N = len(df_hh['housing_expenditures_increase'])
##    ma = np.convolve(x, np.ones((N,))/N, mode='valid')
#    plt.plot(loess[:,0], loess[:,1], 'g-', lw=3)
#    plt.ylim((0,1))
#    plt.xlim((-300, 0))
#    plt.grid(True)
##    print "probability that housing exp. increase < 110 in function of prediction in housing gain"
##    print sum(x > 110 for x in ols.predict(X))/len(ols.predict(X))
    return regression_ols


def predict_winner_looser_housing(df_hh):

    df_hh['hh_income_2'] = df_hh['hh_income'] ** 2

    df_hh['winner'] = 0 + 1 * (df_hh['housing_expenditures_increase'] < 55 * df_hh['nb_beneficiaries'])

    variables = ['hh_income', 'hh_income_2', 'consumption_units', 'nb_beneficiaries', 'natural_gas',
        'domestic_fuel', 'accommodation_size', 'age_18_24', 'age_25_34', 'age_35_49', 'age_50_64']

    logit = smf.Logit(df_hh['winner'], df_hh[variables]).fit()
    
    probit = smf.Probit(df_hh['winner'], df_hh[variables]).fit()

    ols = smf.ols(formula = 'winner ~ \
        natural_gas + domestic_fuel + \
        accommodation_size + age_18_24 + age_25_34 + age_35_49 + age_50_64',
        data = df_hh).fit()

    clf = tree.DecisionTreeClassifier(max_depth=3)
    clf = clf.fit(df_hh[variables], df_hh['winner'])
#    regr = tree.DecisionTreeRegressor(max_depth=3)
#    regr.fit(df_hh[variables], df_hh['winner'])
    
    return logit, probit, ols, clf


if __name__ == "__main__":
    df_hh = prepare_dataset_housing('enl')
    df_hh = compute_gains_losses_housing(df_hh)
    regression_ols = regress_ols_housing_expenditures_increase(df_hh)

    print regression_ols.summary()
    
    logit_winner = predict_winner_looser_housing(df_hh)[0]
    probit_winner = predict_winner_looser_housing(df_hh)[1]
    ols_winner = predict_winner_looser_housing(df_hh)[2]
    
    #print logit_winner.summary()
    #print probit_winner.summary()
    #print ols_winner.summary()
