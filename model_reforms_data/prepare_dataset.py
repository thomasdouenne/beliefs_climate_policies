# -*- coding: utf-8 -*-

# In this script we build a database ready to work with
# We translate variables names in english for the ease of non-french readers
# We inflate variables using aggregate data to better represent the year 2018


import pandas as pd


def prepare_dataset():
    # Load data with all households and relevant characteristics
    try:
        df_hh = pd.read_csv(r'C:\Users\t.douenne\Documents\Data\assets\data_menages.csv')
    except:
        df_hh = pd.read_csv(r'C:\Users\thoma\Documents\Data\assets\data_menages.csv')
    
    
    # Change column names to english
    df_hh.rename(
                columns = {
                    'age_vehicule' : 'age_vehicle',
                    'agepr' : 'age_hh_representative',
                    'aides_logement' : 'housing benefits',
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
                    'isolation_fenetres' : 'windows_isolation',
                    'isolation_murs' : 'walls_isolation',
                    'log_indiv': 'individual_housing',
                    'majorite_double_vitrage' : 'majority_double_glazing',
                    'nactifs' : 'nb_active',
                    'nenfants' : 'nb_children',
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
                    'rev_disp_loyerimput' : 'disposable_income_imputed_rent',
                    'situacj' : 'situation_hh_second',
                    'situapr' : 'situation_hh_representative',
                    'strate' : 'urbanisation',
                    'surfhab_d' : 'accommodation_size'
                    },
                inplace = True,
                )
    
    
    # Inflate expenditures data to represent 2018 as closely as possible
    df_hh['diesel_expenditures'] = 1 * df_hh['diesel_expenditures']
    df_hh['gasoline_expenditures'] = 1 * df_hh['gasoline_expenditures']
    df_hh['domestic_fuel_expenditures'] = 1 * df_hh['domestic_fuel_expenditures']
    df_hh['natural_gas_expenditures'] = 1 * df_hh['natural_gas_expenditures']
    
    # Inflate quantity data to represent 2018 as closely as possible
    df_hh['diesel_quantity'] = 1 * df_hh['diesel_quantity']
    df_hh['gasoline_quantity'] = 1 * df_hh['gasoline_quantity']
    df_hh['domestic_fuel_quantity'] = 1 * df_hh['domestic_fuel_quantity']
    df_hh['gas_quantity'] = 1 * df_hh['gas_quantity']

    # Keep only some variables :
    df_hh = df_hh[['diesel_expenditures'] + ['domestic_fuel_expenditures'] + ['gasoline_expenditures'] +
                  ['consumption_units'] + ['hh_weight'] + ['income_decile']]

    return df_hh
