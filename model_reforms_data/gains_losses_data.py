# -*- coding: utf-8 -*-

# Compute tax incidence depending on parameters' values.

from __future__ import division


from prepare_dataset import prepare_dataset
from define_tax_incidence_data import *


def compute_gains_losses(df_hh):
    """ load dataset """
    #df_hh = df_hh[['diesel_expenditures'] + ['domestic_fuel_expenditures'] + ['gasoline_expenditures'] + ['natural_gas_expenditures']]
    
    for element in ['gasoline', 'diesel', 'domestic_fuel', 'natural_gas']:
    
        """ Fix parameters : """
        
        i = 0.9
        current_carbon_price = 44.6 # Carbon tax in 2018
        adjusted_carbon_price = 44.6 + 50 # Carbon tax that we simulate
            
        if element == 'gasoline':
            current_price = 1.5 # This is roughly the value of gasoline prices
            e = -0.3
            carbon_intensity = 0.002286
            initial_excise_tax = 0.6069 - 0.026 # This is roughly the value of the TICPE without carbon tax, but I need to check more precisly
        elif element == 'diesel':
            current_price = 1.45 # This is roughly the value of diesel prices
            e = -0.3
            carbon_intensity = 0.002651
            initial_excise_tax = 0.4284 + 2*0.026 # This is roughly the value of the TICPE without carbon tax, but I need to check more precisly
        elif element == 'domestic_fuel':
            current_price = 0.9 # This is roughly the value of domestic fuel prices
            e = -0.15
            carbon_intensity = 0.00265
            initial_excise_tax = 0.1 # This is roughly the value of the TICPE without carbon tax, but I need to check more precisly
        else:
            current_price = 0.07 # to change
            e = -0.15
            carbon_intensity = 0.000205
            initial_excise_tax = 0 # to chance
        
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
        df_hh['domestic_fuel_expenditures_increase'] + df_hh['natural_gas_expenditures_increase']
        )
    df_hh['total_expenditures_increase'] = (
        df_hh['transport_expenditures_increase'] + df_hh['housing_expenditures_increase']
        )
    df_hh['total_tax_increase'] = (
        df_hh['gasoline_tax_increase'] + df_hh['diesel_tax_increase']
        + df_hh['domestic_fuel_tax_increase'] + df_hh['natural_gas_tax_increase']
        )
    
    return df_hh

if __name__ == "__main__":
    df_hh = prepare_dataset()
    df_hh = compute_gains_losses(df_hh)
    print df_hh['total_expenditures_increase'].mean(), df_hh['total_tax_increase'].mean()
