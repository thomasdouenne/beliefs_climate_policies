# -*- coding: utf-8 -*-

# To do : find how to compute a non-parametric density function from these bins (somethig like KDE for aggregated data)

from __future__ import division

import numpy as np
import pandas as pd

from model_reforms_data.prepare_dataset_housing import prepare_dataset_housing, merge_transport_data
from model_reforms_data.prepare_dataset import prepare_dataset
from model_reforms_data.regression_feedback import compute_gains_losses_housing, \
    regress_ols_housing_expenditures_increase
from model_reforms_data.gains_losses_data import compute_gains_losses
from model_reforms_data.standardize_data_bdf_ptc import variables_names_bdf_to_ptc, \
    create_new_variables_bdf_ptc, compute_gains_nets_uc
from utils import graph_builder_bar_percent


def estimate_kde(df_hh, df_survey, bdw):
    df_survey['gain_monetaire'] = 0 + (
            - 280 * 1.5 * (df_survey['gain_taxe_carbone'] == -6)
            - (280 + 190) / 2 * (df_survey['gain_taxe_carbone'] == -5)
            - (190 + 120) / 2 * (df_survey['gain_taxe_carbone'] == -4)
            - (120 + 70) / 2 * (df_survey['gain_taxe_carbone'] == -3)
            - (70 + 30) / 2 * (df_survey['gain_taxe_carbone'] == -2)
            - (30 + 0) / 2 * (df_survey['gain_taxe_carbone'] == -1)
            + (0 + 20) / 2 * (df_survey['gain_taxe_carbone'] == 1)
            + (20 + 40) / 2 * (df_survey['gain_taxe_carbone'] == 2)
            + (40 + 60) / 2 * (df_survey['gain_taxe_carbone'] == 3)
            + (60 + 80) / 2 * (df_survey['gain_taxe_carbone'] == 4)
            + (80 + 100) / 2 * (df_survey['gain_taxe_carbone'] == 5)
            )
    
    kde_1 = df_survey['gain_monetaire'].plot.kde(bw_method = bdw)
    kde_2 = df_hh['gain_net_uc_taxe_carbone'].plot.kde(bw_method = bdw)

    return kde_1, kde_2

def compare_objective_subjective_beliefs_gains(df_hh, df_survey, energy, cumulative = False):
    
    df_to_plot = pd.DataFrame(index = range(-6,6),
        columns = ['Objective_gains', 'Objective_gains_cumulative', 'Subjective_gains', 'Subjective_gains_cumulative'])
        
    for i in range(-6,6):
        df_to_plot['Objective_gains'][i] = df_hh.query('gain_{0} == {1}'.format(energy, i))['weight'].sum() / df_hh['weight'].sum()
        if i == -6:
            df_to_plot['Objective_gains_cumulative'][i] = 0 + df_to_plot['Objective_gains'][i]
        else:
            df_to_plot['Objective_gains_cumulative'][i] = df_to_plot['Objective_gains_cumulative'][i-1] + df_to_plot['Objective_gains'][i]

    df_survey.dropna(subset = ['gain_{}'.format(energy)], inplace = True)
    for i in range(-6,6):
        df_to_plot['Subjective_gains'][i] = df_survey.query('gain_{0} == {1}'.format(energy, i))['weight'].sum() / df_survey['weight'].sum()
        if i == -6:
            df_to_plot['Subjective_gains_cumulative'][i] = 0 + df_to_plot['Subjective_gains'][i]
        else:
            df_to_plot['Subjective_gains_cumulative'][i] = df_to_plot['Subjective_gains_cumulative'][i-1] + df_to_plot['Subjective_gains'][i]

    df_to_plot['Subjective_gains'][-1] = df_to_plot['Subjective_gains'][-1] + 0.5 * df_to_plot['Subjective_gains'][0]
    df_to_plot['Subjective_gains'][1] = df_to_plot['Subjective_gains'][1] + 0.5 * df_to_plot['Subjective_gains'][0]        
    df_to_plot = df_to_plot.drop(0)
    
    if cumulative == True:
        #plot = graph_builder_bar_percent(df_to_plot[['Objective_gains_cumulative'] + ['Subjective_gains_cumulative']])
        df_to_plot[['Objective_gains_cumulative'] + ['Subjective_gains_cumulative']].plot(drawstyle="steps", linewidth=2)
    else:
        graph_builder_bar_percent(df_to_plot[['Objective_gains'] + ['Subjective_gains']])
        #df_to_plot[['Objective_gains'] + ['Subjective_gains']].plot(drawstyle="steps", linewidth=2)
    
    return df_to_plot


if __name__ == "__main__":
    df_hh = prepare_dataset()
    df_ptc = pd.read_csv(r'C:\Users\thoma\Documents\Github\beliefs_climate_policies\code\survey_prepared.csv')

    df_hh = compute_gains_losses(df_hh)
    df_hh = compute_gains_nets_uc(df_hh)
    df_hh = variables_names_bdf_to_ptc(df_hh)
    (df_hh, df_ptc) = create_new_variables_bdf_ptc(df_hh, df_ptc)
    df_hh['weight'] = 1
    
    df_ptc['weight'] = 1
    df_ptc['gain_taxe_carbone'] = df_ptc['gain']

    # Comparison on sub-samples:
    variable = None
    sign = '=='
    value = 1
    try:
        df_hh = df_hh.query('{0} {1} {2}'.format(variable, sign, value))
        df_ptc_1 = df_ptc.query('{0} {1} {2}'.format(variable, sign, value))
    except:
        pass

    df_to_plot = compare_objective_subjective_beliefs_gains(df_hh, df_ptc, 'chauffage', True)
    #kde = estimate_kde(df_hh, df_ptc, 0.6)
