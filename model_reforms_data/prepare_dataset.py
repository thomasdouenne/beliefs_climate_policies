# -*- coding: utf-8 -*-

# In this script we build a database ready to work with
# We translate variables names in english for the ease of non-french readers
# We inflate variables using aggregate data to better represent the year 2018


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


def prepare_dataset():
    # Load data with all households and relevant characteristics
    try:
        df_hh = pd.read_csv(r'C:\Users\t.douenne\Documents\Data\assets\data_menages.csv')
    except:
        try:
            df_hh = pd.read_csv(r'../code/data_menages.csv')
        except:
            try:
                df_hh = pd.read_csv(r'code/data_menages.csv')
            except:
                df_hh = pd.read_csv(r'C:\Users\thoma\Documents\Data\assets\data_menages.csv')
    
    
    # Change column names to english
    df_hh.rename(
                columns = {
                    'age_vehicule' : 'age_vehicle',
                    'agepr' : 'age_hh_representative',
                    'aides_logement' : 'housing benefits',
                    'combustibles_liquides' : 'domestic_fuel',
                    'depenses_carburants_corrigees': 'transport_energy_expenditures',
                    'depenses_combustibles_liquides': 'domestic_fuel_expenditures',
                    'depenses_diesel_corrigees' : 'diesel_expenditures',
                    'depenses_electricite' : 'electricity_expenditures',
                    'depenses_energie_thermique' : 'thermal_energy_expenditures',
                    'depenses_energies_logement' : 'housing_energy_expenditures',
                    'depenses_energies_totales' : 'total_energy_expenditures',
                    'depenses_essence_corrigees' : 'gasoline_expenditures',
                    'depenses_gaz_liquefie' : 'liquefied_gas_expenditures',
                    'depenses_gaz_ville' : 'natural_gas_expenditures',
                    'distance_routiere_hebdomadaire_teg' : 'weekly_road_distance_home_to_work',
                    'duree_moyenne_trajet_aller_retour_teg' : 'avg_duration_home_to_work',
                    'gaz_ville' : 'natural_gas',
                    'identifiant_menage' : 'hh_id',
                    'isolation_fenetres' : 'windows_isolation',
                    'isolation_murs' : 'walls_isolation',
                    'log_indiv': 'individual_housing',
                    'majorite_double_vitrage' : 'majority_double_glazing',
                    'nactifs' : 'nb_active',
                    'nenfants' : 'nb_children',
                    'npers' : 'nb_persons',
                    'niveau_vie_decile' : 'income_decile',
                    'ocde10' : 'consumption_units',
                    'ouest_sud' : 'west_sud',
                    'pondmen' : 'hh_weight',
                    'quantites_combustibles_liquides' : 'domestic_fuel_quantity',
                    'quantites_diesel' : 'diesel_quantity',
                    'quantites_electricite_selon_compteur' : 'electricity_quantity',
                    'quantites_essence' : 'gasoline_quantity',
                    'quantites_gaz_final' : 'gas_quantity',
                    'quantites_gaz_liquefie' : 'liquefied_gas_quantity',
                    'revtot' : 'hh_income',
                    'rev_disp_loyerimput': 'hh_disposable_income',
                    'situacj' : 'situation_hh_second',
                    'situapr' : 'situation_hh_representative',
                    'strate' : 'urbanisation',
                    'surfhab_d' : 'accommodation_size'
                    },
                inplace = True,
                )
    
    
    # Inflate expenditures data to represent 2018 as closely as possible
    df_hh['diesel_expenditures'] = (36916.0 / 33412) * df_hh['diesel_expenditures'] # Not specific to diesel
    df_hh['gasoline_expenditures'] = (36916.0 / 33412) * df_hh['gasoline_expenditures'] # Not specific to diesel
    df_hh['domestic_fuel_expenditures'] = (6040.0 / 4710) * df_hh['domestic_fuel_expenditures']
    df_hh['natural_gas_expenditures'] = (12987.0 / 12560) * df_hh['natural_gas_expenditures']
    
    # Inflate quantity data to represent 2018 as closely as possible
    df_hh['diesel_quantity'] = 1 * df_hh['diesel_quantity']
    df_hh['gasoline_quantity'] = 1 * df_hh['gasoline_quantity']
    df_hh['domestic_fuel_quantity'] = 1 * df_hh['domestic_fuel_quantity']
    df_hh['gas_quantity'] = 1 * df_hh['gas_quantity']

    # Inflate households resources :
    #df_hh['total_expenditures'] = 1 * df_hh['total_expenditures']
    df_hh['hh_income'] = 1 * df_hh['hh_income']

    df_hh = gas_expenditure_without_fixed_price(df_hh)
  
    # Keep only some variables :
    df_hh = df_hh[['distance'] + ['diesel_expenditures'] + ['domestic_fuel_expenditures'] + ['gasoline_expenditures'] +
        ['natural_gas_expenditures'] + ['natural_gas_variable_expenditures'] + ['income_decile'] +
        ['hh_income'] + ['hh_disposable_income'] + ['domestic_fuel'] + ['natural_gas'] + ['age_hh_representative'] +
        ['accommodation_size'] + ['consumption_units'] + ['nb_children'] + ['nb_persons'] + ['hh_weight'] +
        ['hh_id']]

    return df_hh


if __name__ == "__main__":
    df_hh = prepare_dataset()
