# -*- coding: utf-8 -*-
"""
Created on Tue Jan 15 09:58:11 2019

@author: thoma
"""



import pandas as pd


def gas_expenditure_without_fixed_price(df_hh): # We impute the contract that gives highest consumption
    df_hh['natural_gas_variable_expenditures_base'] = df_hh['natural_gas_expenditures'] - 97.80
    df_hh['natural_gas_variable_expenditures_b0'] = df_hh['natural_gas_expenditures'] - 110.04
    df_hh['natural_gas_variable_expenditures_b1_b2i'] = df_hh['natural_gas_expenditures'] - 250.44

    df_hh['natural_gas_quantity_base'] = df_hh['natural_gas_variable_expenditures_base'] / 0.0999
    df_hh['natural_gas_quantity_b0'] = df_hh['natural_gas_variable_expenditures_b0'] / 0.0859
    df_hh['natural_gas_quantity_b1_b2i'] = df_hh['natural_gas_variable_expenditures_b1_b2i'] / 0.0651
    
    df_hh['natural_gas_variable_expenditures'] = (
            df_hh['natural_gas_variable_expenditures_base']
            * (df_hh['natural_gas_quantity_base'] + 1e-06 > df_hh['natural_gas_quantity_b0'])
            * (df_hh['natural_gas_quantity_base'] + 1e-06 > df_hh['natural_gas_quantity_b0'])
            + df_hh['natural_gas_variable_expenditures_b0']
            * (df_hh['natural_gas_quantity_b0'] + 1e-06 > df_hh['natural_gas_quantity_base'])
            * (df_hh['natural_gas_quantity_b0'] + 1e-06 > df_hh['natural_gas_quantity_b1_b2i'])
            + df_hh['natural_gas_variable_expenditures_b1_b2i']
            * (df_hh['natural_gas_quantity_b1_b2i'] + 1e-06 > df_hh['natural_gas_quantity_base'])
            * (df_hh['natural_gas_quantity_b1_b2i'] + 1e-06 > df_hh['natural_gas_quantity_b0'])
            )
    df_hh['natural_gas_variable_expenditures'] = 0 + df_hh['natural_gas_variable_expenditures'] * (df_hh['natural_gas_variable_expenditures'] > 0)
        
    return df_hh


def prepare_dataset_enl():
    # Load data with all households and relevant characteristics
    try:
        df_hh = pd.read_csv(r'C:\Users\t.douenne\Documents\Data\assets\matching\matching_enl\data_matching_enl.csv')
    except:
        df_hh = pd.read_csv(r'C:\Users\thoma\Documents\Data\assets\matching\matching_enl\data_matching_enl.csv')


    # Change column names to english
    df_hh.rename(
                columns = {
                    'agepr' : 'age_hh_representative',
                    'depenses_combustibles_liquides' : 'domestic_fuel_expenditures',
                    'depenses_gaz_ville' : 'natural_gas_expenditures',
                    'fioul' : 'domestic_fuel',
                    'gaz_ville' : 'natural_gas',
                    'niveau_vie_decile' : 'income_decile',
                    'ocde10' : 'consumption_units',
                    'pondmen' : 'hh_weight',
                    'revtot' : 'hh_income',
                    'surfhab_d' : 'accommodation_size'
                    },
                inplace = True,
                )

    # Inflate expenditures and resources to represent 2018 as closely as possible
    df_hh['domestic_fuel_expenditures'] = (6040.0 / 7866) * df_hh['domestic_fuel_expenditures']
    df_hh['natural_gas_expenditures'] = (12987.0 / 15403) * df_hh['natural_gas_expenditures']
    df_hh['hh_income'] = 1 * df_hh['hh_income']

    df_hh = gas_expenditure_without_fixed_price(df_hh)

    # Keep only some variables :
    df_hh = df_hh[['domestic_fuel_expenditures'] + ['natural_gas_expenditures'] + ['natural_gas_variable_expenditures'] +
        ['hh_weight'] + ['income_decile'] + ['hh_income'] + ['domestic_fuel'] + ['natural_gas'] +
        ['consumption_units'] + ['age_hh_representative'] + ['accommodation_size']]

    return df_hh


if __name__ == "__main__":
    df_hh = prepare_dataset_enl()