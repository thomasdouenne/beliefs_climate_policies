# -*- coding: utf-8 -*-

# Compute tax incidence depending on parameters' values.

from __future__ import division


from prepare_dataset import prepare_dataset
from define_tax_incidence_data import *


def compute_gains_losses(df_hh):
    """ load dataset """
    #df_hh = df_hh[['diesel_expenditures'] + ['domestic_fuel_expenditures'] + ['gasoline_expenditures'] + ['natural_gas_expenditures']]
    
    initial_variables = df_hh.columns.tolist()      
    for element in ['gasoline', 'diesel', 'domestic_fuel', 'natural_gas_variable']:
    
        """ Fix parameters : """
        
        i = 0.8
        current_carbon_price = 44.6 # Carbon tax in 2018
        adjusted_carbon_price = 44.6 + 50 # Carbon tax that we simulate
            
        if element == 'gasoline':
            current_price = 1.441 # This is roughly the value of gasoline prices
            e = -0.0
            carbon_intensity = 0.002286
            initial_excise_tax = 0.6069 - 0.026 # This is roughly the value of the TICPE without carbon tax, but I need to check more precisly
        elif element == 'diesel':
            current_price = 1.399 # This is roughly the value of diesel prices
            e = -0.0
            carbon_intensity = 0.002651
            initial_excise_tax = 0.4284 + 2*0.026 # This is roughly the value of the TICPE without carbon tax, but I need to check more precisly
        elif element == 'domestic_fuel':
            current_price = 0.859 # This is roughly the value of domestic fuel prices
            e = -0.0
            carbon_intensity = 0.00265
            initial_excise_tax = 0.038 # This is roughly the value of the TICPE without carbon tax, but I need to check more precisly
        else:
            current_price = 0.0651 # For someone in zone 3 that use gas for heating
            e = -0.0
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
    
    
    df_hh['transport_expenditures_increase'] = (
        df_hh['gasoline_expenditures_increase'] + df_hh['diesel_expenditures_increase']
        )
    df_hh['housing_expenditures_increase'] = (
        df_hh['domestic_fuel_expenditures_increase'] + df_hh['natural_gas_variable_expenditures_increase']
        )
    df_hh['total_expenditures_increase'] = (
        df_hh['transport_expenditures_increase'] + df_hh['housing_expenditures_increase']
        )
    df_hh['transport_tax_increase'] = (
        df_hh['gasoline_tax_increase'] + df_hh['diesel_tax_increase']
        )
    df_hh['housing_tax_increase'] = (
        df_hh['domestic_fuel_tax_increase'] + df_hh['natural_gas_variable_tax_increase']
        )
    df_hh['total_tax_increase'] = (
        df_hh['transport_tax_increase'] + df_hh['housing_tax_increase']
        )
    
    try:
        df_hh['nb_adults'] = df_hh['nb_persons'] - df_hh['nb_children']
    except:
        df_hh['nb_adults'] = df_hh['plus_18']
    df_hh['nb_beneficiaries'] = 2 - 1 * (df_hh['nb_adults'] == 1)
    
    return df_hh[initial_variables + ['housing_expenditures_increase'] + ['housing_tax_increase'] +
        ['transport_expenditures_increase'] + ['transport_tax_increase'] +
        ['total_expenditures_increase'] + ['total_tax_increase'] + ['nb_beneficiaries']]


if __name__ == "__main__":
    df_hh = prepare_dataset()
    df_hh = compute_gains_losses(df_hh)
    revenue_from_tax_transports = (df_hh['hh_weight'] * df_hh['transport_tax_increase']).sum()
    revenue_from_tax_housing = (df_hh['hh_weight'] * df_hh['housing_tax_increase']).sum()
    revenue_from_tax_total = (df_hh['hh_weight'] * df_hh['total_tax_increase']).sum()

    print "Average loss in purchasing power :", df_hh['total_expenditures_increase'].mean()
    print "Average additional taxes paid :", df_hh['total_tax_increase'].mean()
    print "Revenue from trannsport fuels tax policy in billions euros :", revenue_from_tax_transports / 1e09
    print "Revenue from housing energies tax policy in billions euros :", revenue_from_tax_housing / 1e09
    print "Revenue from the total policy in billions euros :", revenue_from_tax_total / 1e09

    print ((df_hh['transport_expenditures_increase'] - 60 * df_hh['nb_beneficiaries']) / df_hh['consumption_units']).quantile(0.956)
    print ((df_hh['housing_expenditures_increase'] - 50 *df_hh['nb_beneficiaries']) / df_hh['consumption_units']).quantile(0.934)
    print ((df_hh['total_expenditures_increase'] - 110 *df_hh['nb_beneficiaries']) / df_hh['consumption_units']).quantile(0.953)
    
    df_s = df_hh.query('consumption_units < 1.5')
    df_m = df_hh.query('consumption_units > 1.4').query('consumption_units < 2')
    df_l = df_hh.query('consumption_units > 1.9').query('consumption_units < 2.4')
    df_xl = df_hh.query('consumption_units > 2.3')
    
    print ((df_s['total_expenditures_increase'] - 110 *df_s['nb_beneficiaries']) / df_s['consumption_units']).quantile(0.9)
    print ((df_m['total_expenditures_increase'] - 110 *df_m['nb_beneficiaries']) / df_m['consumption_units']).quantile(0.9)
    print ((df_l['total_expenditures_increase'] - 110 *df_l['nb_beneficiaries']) / df_l['consumption_units']).quantile(0.9)
    print ((df_xl['total_expenditures_increase']- 110 *df_xl['nb_beneficiaries']) / df_xl['consumption_units']).quantile(0.9)