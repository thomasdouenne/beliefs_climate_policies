# -*- coding: utf-8 -*-

from __future__ import division

import pandas as pd
import numpy as np
import scipy.stats as stats
import random

from utils import graph_builder_bar_percent


def variables_names_bdf_to_ptc(df_bdf):
    df_bdf.rename(
            columns = {
                'age_hh_representative' : 'age_hh_representative',
                'domestic_fuel_expenditures' : 'domestic_fuel_expenditures',
                'natural_gas_expenditures' : 'natural_gas_expenditures',
                'domestic_fuel' : 'fioul',
                'natural_gas' : 'gaz',
                'hh_id' : 'hh_id',
                'income_decile' : 'income_decile',
                'nb_children' : 'nb_children',
                'nb_persons' : 'taille_menage',
                'consumption_units' : 'uc',
                'hh_weight' : 'weight',
                'hh_income' : 'rev_tot',
                'accommodation_size' : 'surface',
                },
            inplace = True,
            )

    return df_bdf


def create_new_variables_bdf_ptc(df_bdf, df_ptc):
    df_bdf['ni_fioul_ni_gaz'] = 1 * ((df_bdf['fioul'] + df_bdf['gaz']) == 0)
    df_ptc['ni_fioul_ni_gaz'] = 1 * ((df_ptc['fioul'] + df_ptc['gaz']) == 0)
  
    return df_bdf, df_ptc


def compute_gain_net_uc(df_bdf):
    df_bdf['gain_net_numeric_uc_fuel'] = \
        (60 * df_bdf['nb_beneficiaries'] - df_bdf['transport_expenditures_increase']) / df_bdf['consumption_units']
    df_bdf['gain_net_numeric_uc_chauffage'] = \
        (50 * df_bdf['nb_beneficiaries'] - df_bdf['housing_expenditures_increase']) / df_bdf['consumption_units']
    df_bdf['gain_net_numeric_uc_taxe_carbone'] = \
        (110 * df_bdf['nb_beneficiaries'] - df_bdf['total_expenditures_increase']) / df_bdf['consumption_units']

    for energy in ['chauffage', 'fuel']:
        df_bdf['gain_{}'.format(energy)] = 0 + (
                - 6 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] < -160)
                - 5 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] >= -160) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] < -110)
                - 4 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] >= -110) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] < -70)
                - 3 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] >= -70) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] < -40)
                - 2 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] >= -40) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] < -15)
                - 1 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] >= -15) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] < -0)
                + 1 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] >= 0) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] <= 10)
                + 2 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] > 10) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] <= 20)
                + 3 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] > 20) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] <= 30)
                + 4 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] > 30) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] <= 40)
                + 5 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] > 40)
                )
    for energy in ['taxe_carbone']:
        df_bdf['gain_{}'.format(energy)] = 0 + (
                - 6 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] < -280)
                - 5 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] >= -280) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] < -190)
                - 4 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] >= -190) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] < -120)
                - 3 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] >= -120) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] < -70)
                - 2 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] >= -70) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] < -30)
                - 1 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] >= -30) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] < -0)
                + 1 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] >= 0) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] <= 20)
                + 2 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] > 20) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] <= 40)
                + 3 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] > 40) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] <= 60)
                + 4 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] > 60) * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] <= 80)
                + 5 * (df_bdf['gain_net_numeric_uc_{}'.format(energy)] > 80)
                )

    return df_bdf


def compare_objective_subjective_beliefs_gain(df_bdf, df_survey, energy = 'taxe_carbone', cumulative = False):
    
    df_to_plot = pd.DataFrame(index = range(-6,6),
        columns = ['Objective_gain', 'Objective_gain_cumulative', 'Subjective_gain', 'Subjective_gain_cumulative'])
        
    for i in range(-6,6):
        df_to_plot['Objective_gain'][i] = df_bdf.query('gain_{0} == {1}'.format(energy, i))['weight'].sum() / df_bdf['weight'].sum()
        if i == -6:
            df_to_plot['Objective_gain_cumulative'][i] = 0 + df_to_plot['Objective_gain'][i]
        else:
            df_to_plot['Objective_gain_cumulative'][i] = df_to_plot['Objective_gain_cumulative'][i-1] + df_to_plot['Objective_gain'][i]

    df_survey.dropna(subset = ['gain_{}'.format(energy)], inplace = True)
    for i in range(-6,6):
        df_to_plot['Subjective_gain'][i] = df_survey.query('gain_{0} == {1}'.format(energy, i))['weight'].sum() / df_survey['weight'].sum()
        if i == -6:
            df_to_plot['Subjective_gain_cumulative'][i] = 0 + df_to_plot['Subjective_gain'][i]
        else:
            df_to_plot['Subjective_gain_cumulative'][i] = df_to_plot['Subjective_gain_cumulative'][i-1] + df_to_plot['Subjective_gain'][i]

    df_to_plot['Subjective_gain'][-1] = df_to_plot['Subjective_gain'][-1] + 0.5 * df_to_plot['Subjective_gain'][0]
    df_to_plot['Subjective_gain'][1] = df_to_plot['Subjective_gain'][1] + 0.5 * df_to_plot['Subjective_gain'][0]        
    df_to_plot = df_to_plot.drop(0)
    
    if cumulative == True:
        df_to_plot[['Objective_gain_cumulative'] + ['Subjective_gain_cumulative']].plot(drawstyle="steps", linewidth=2)
    else:
        graph_builder_bar_percent(df_to_plot[['Objective_gain'] + ['Subjective_gain']])
    
    return df_to_plot


def impute_barycentre_in_bins(df_bdf, df_ptc): # Maybe add weights for bins size
    for energy in ['chauffage', 'fuel']:
        df_ptc['gain_net_numeric_barycentre_uc_{}'.format(energy)] = 0 + (
                df_bdf.query('gain_{} == -6'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == -6)
                + ((-160 * df_ptc.query('gain_{} == -6'.format(energy))['weight'].sum() - 110 * df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -6'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -5)
                + ((-110 * df_ptc.query('gain_{} == -5'.format(energy))['weight'].sum() - 70 * df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -5'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -4)
                + ((-70 * df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum() - 40 * df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -3)
                + ((-40 * df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum() - 15 * df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -2)
                + ((-15 * df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum() - 0 * df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -1)
                + 0 * (df_ptc['gain_{}'.format(energy)] == 0)
                + ((0 * df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum() + 10 * df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 1)
                + ((10 * df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum() + 20 * df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 2)
                + ((20 * df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum() + 30 * df_ptc.query('gain_{} == 4'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 4'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 3)
                + ((30 * df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum() + 40 * df_ptc.query('gain_{} == 5'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 5'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 4)
                + df_bdf.query('gain_{} == 5'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == 5)
                )
    for energy in ['taxe_carbone']:
        df_ptc['gain_net_numeric_barycentre_uc_{}'.format(energy)] = 0 + (
                df_bdf.query('gain_{} == -6'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == -6)
                + ((-280 * df_ptc.query('gain_{} == -6'.format(energy))['weight'].sum() - 190 * df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -6'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -5)
                + ((-190 * df_ptc.query('gain_{} == -5'.format(energy))['weight'].sum() - 120 * df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -5'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -4)
                + ((-120 * df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum() - 70 * df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -3)
                + ((-70 * df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum() - 30 * df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -2)
                + ((-30 * df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum() - 0 * df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -1)
                + 0 * (df_ptc['gain_{}'.format(energy)] == 0)
                + ((0 * df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum() + 20 * df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 1)
                + ((20 * df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum() + 40 * df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 2)
                + ((40 * df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum() + 60 * df_ptc.query('gain_{} == 4'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 4'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 3)
                + ((60 * df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum() + 80 * df_ptc.query('gain_{} == 5'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 5'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 4)
                + df_bdf.query('gain_{} == 5'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == 5)
                )

    return df_ptc


def impute_average_bdf_in_bins(df_bdf, df_ptc): # More consistent with method used for KDE estimation
    # Maybe we should first discard outliers from BdF
    for energy in ['chauffage', 'fuel', 'taxe_carbone']:
        df_ptc['gain_net_numeric_uc_{}'.format(energy)] = 0 + (
                df_bdf.query('gain_{} == -6'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == -6)
                + df_bdf.query('gain_{} == -5'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == -5)
                + df_bdf.query('gain_{} == -4'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == -4)
                + df_bdf.query('gain_{} == -3'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == -3)
                + df_bdf.query('gain_{} == -2'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == -2)
                + df_bdf.query('gain_{} == -1'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == -1)
                + (
                    0.5 * df_bdf.query('gain_{} == -1'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean()
                    + 0.5 * df_bdf.query('gain_{} == 1'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean()
                    ) * (df_ptc['gain_{}'.format(energy)] == 0)
                + df_bdf.query('gain_{} == 1'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == 1)
                + df_bdf.query('gain_{} == 2'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == 2)
                + df_bdf.query('gain_{} == 3'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == 3)
                + df_bdf.query('gain_{} == 4'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == 4)
                + df_bdf.query('gain_{} == 5'.format(energy))['gain_net_numeric_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == 5)
                )

    return df_ptc


def extrapolate_distribution_bcp_from_bdf(df_bdf, df_ptc, energy = 'taxe_carbone', bw_size = 0.25):
    df_to_plot = pd.DataFrame(index = range(0,10000),
        columns = ['subjective_gain_category_{}'.format(energy), 'subjective_gain_numeric_{}'.format(energy)])
    df_to_plot = df_to_plot.reset_index()
    df_to_plot['subjective_gain_category_{}'.format(energy)] = 0
    df_to_plot['subjective_gain_numeric_{}'.format(energy)] = 0.0
    df_ptc_energy = df_ptc.dropna(subset = ['gain_{}'.format(energy)])
    hh_index = 0
    for i in range(-6,6):
        hh_index_old = hh_index
        hh_index = hh_index_old + float(df_ptc_energy.query('gain_{0} == {1}'.format(energy, i))['weight'].sum()) / df_ptc_energy['weight'].sum() * len(df_to_plot)
        df_to_plot['subjective_gain_category_{}'.format(energy)] = df_to_plot['subjective_gain_category_{}'.format(energy)] + i * (df_to_plot['index'] >= hh_index_old) * (df_to_plot['index'] < hh_index)
    
        if i != 0:
            local_hh = df_bdf.query('gain_{0} == {1}'.format(energy, i))
        else:
            local_hh = df_bdf.query('gain_{} <= 1'.format(energy)).query('gain_{} >= -1'.format(energy))
        local_hh = local_hh.sort_values(by=['gain_net_numeric_uc_{}'.format(energy)])
        # parametric fit: assume normal distribution
        loc_param, scale_param = stats.norm.fit(local_hh['gain_net_numeric_uc_{}'.format(energy)])
        param_density = stats.norm.cdf(local_hh['gain_net_numeric_uc_{}'.format(energy)], loc=loc_param, scale=scale_param)

        array_size = int(hh_index) - int(hh_index_old)
        random_array = np.random.randint(1,100001, size = array_size)
        random_array = np.array([random_array,] * 1).astype(float) / 100000
        local_hh_matrix = np.array([local_hh['gain_net_numeric_uc_{}'.format(energy)],] * array_size)
        
        density_array = np.array([param_density,] * array_size)
        index_matrix = density_array - random_array.T
        
        index = (np.abs(index_matrix)).argmin(1)
        array_values = local_hh_matrix[0][index]
        
        df_to_plot['subjective_gain_numeric_{}'.format(energy)][int(hh_index_old):int(hh_index)] = array_values

    # Cut extreme values for more readable figures
    df_bdf_limited = df_bdf.query('gain_net_numeric_uc_{} > -300'.format(energy))
    plot_2 = df_bdf_limited['gain_net_numeric_uc_{}'.format(energy)].plot.density(bw_method = bw_size)
    df_to_plot_limited = df_to_plot.query('subjective_gain_numeric_{} > -300'.format(energy))
    plot_1 = df_to_plot_limited['subjective_gain_numeric_{}'.format(energy)].plot.density(bw_method = bw_size)
    
    return plot_1, plot_2
