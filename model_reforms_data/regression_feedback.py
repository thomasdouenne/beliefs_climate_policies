# -*- coding: utf-8 -*-

# Careful : use nb of adults instead of cu.

from __future__ import division

import statsmodels.formula.api as smf


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

    regression_ols = smf.ols(formula = 'housing_expenditures_increase ~ \
        hh_income + hh_income_2 + consumption_units + nb_beneficiaries + natural_gas + domestic_fuel + \
        accommodation_size + age_18_24 + age_25_34 + age_35_49 + age_50_64',
        data = df_hh).fit()

    return regression_ols


def predict_winner_looser_housing(df_hh):

    df_hh['hh_income_2'] = df_hh['hh_income'] ** 2

    df_hh['winner'] = 0 + 1 * (df_hh['housing_expenditures_increase'] < 55 * df_hh['nb_beneficiaries'])

    variables = ['hh_income', 'hh_income_2', 'consumption_units', 'nb_beneficiaries', 'natural_gas',
        'domestic_fuel', 'accommodation_size', 'age_18_24', 'age_25_34', 'age_35_49', 'age_50_64']

    logit = smf.Logit(df_hh['winner'], df_hh[variables]).fit()
    
    probit = smf.Probit(df_hh['winner'], df_hh[variables]).fit()

    ols = smf.ols(formula = 'winner ~ \
        hh_income + hh_income_2 + consumption_units + nb_beneficiaries + natural_gas + domestic_fuel + \
        accommodation_size + age_18_24 + age_25_34 + age_35_49 + age_50_64',
        data = df_hh).fit()

    return logit, probit, ols


if __name__ == "__main__":
    df_hh = prepare_dataset_housing('enl')
    df_hh = compute_gains_losses_housing(df_hh)
    regression_ols = regress_ols_housing_expenditures_increase(df_hh)

    print regression_ols.summary()
    
    logit_winner = predict_winner_looser_housing(df_hh)[0]
    probit_winner = predict_winner_looser_housing(df_hh)[1]
    ols_winner = predict_winner_looser_housing(df_hh)[2]
    
    print logit_winner.summary()
    print probit_winner.summary()
    print ols_winner.summary()
