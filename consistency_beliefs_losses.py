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

def compare_objective_subjective_beliefs_gains(df_hh, df_survey, energy):
    
    df_to_plot = pd.DataFrame(index = range(-6,6), columns = ['Objective_gains', 'Subjective_gains'])
        
    if energy != 'taxe_carbone':
        df_hh['gain_{}'.format(energy)] = 0 + (
                - 6 * (df_hh['gain_net_uc_{}'.format(energy)] < -160)
                - 5 * (df_hh['gain_net_uc_{}'.format(energy)] >= -160) * (df_hh['gain_net_uc_{}'.format(energy)] < -110)
                - 4 * (df_hh['gain_net_uc_{}'.format(energy)] >= -110) * (df_hh['gain_net_uc_{}'.format(energy)] < -70)
                - 3 * (df_hh['gain_net_uc_{}'.format(energy)] >= -70) * (df_hh['gain_net_uc_{}'.format(energy)] < -40)
                - 2 * (df_hh['gain_net_uc_{}'.format(energy)] >= -40) * (df_hh['gain_net_uc_{}'.format(energy)] < -15)
                - 1 * (df_hh['gain_net_uc_{}'.format(energy)] >= -15) * (df_hh['gain_net_uc_{}'.format(energy)] < -0)
                + 1 * (df_hh['gain_net_uc_{}'.format(energy)] >= 0) * (df_hh['gain_net_uc_{}'.format(energy)] <= 10)
                + 2 * (df_hh['gain_net_uc_{}'.format(energy)] > 10) * (df_hh['gain_net_uc_{}'.format(energy)] <= 20)
                + 3 * (df_hh['gain_net_uc_{}'.format(energy)] > 20) * (df_hh['gain_net_uc_{}'.format(energy)] <= 30)
                + 4 * (df_hh['gain_net_uc_{}'.format(energy)] > 30) * (df_hh['gain_net_uc_{}'.format(energy)] <= 40)
                + 5 * (df_hh['gain_net_uc_{}'.format(energy)] > 40)
                )
    else:
        df_hh['gain_{}'.format(energy)] = 0 + (
                - 6 * (df_hh['gain_net_uc_{}'.format(energy)] < -280)
                - 5 * (df_hh['gain_net_uc_{}'.format(energy)] >= -280) * (df_hh['gain_net_uc_{}'.format(energy)] < -190)
                - 4 * (df_hh['gain_net_uc_{}'.format(energy)] >= -190) * (df_hh['gain_net_uc_{}'.format(energy)] < -120)
                - 3 * (df_hh['gain_net_uc_{}'.format(energy)] >= -120) * (df_hh['gain_net_uc_{}'.format(energy)] < -70)
                - 2 * (df_hh['gain_net_uc_{}'.format(energy)] >= -70) * (df_hh['gain_net_uc_{}'.format(energy)] < -30)
                - 1 * (df_hh['gain_net_uc_{}'.format(energy)] >= -30) * (df_hh['gain_net_uc_{}'.format(energy)] < -0)
                + 1 * (df_hh['gain_net_uc_{}'.format(energy)] >= 0) * (df_hh['gain_net_uc_{}'.format(energy)] <= 20)
                + 2 * (df_hh['gain_net_uc_{}'.format(energy)] > 20) * (df_hh['gain_net_uc_{}'.format(energy)] <= 40)
                + 3 * (df_hh['gain_net_uc_{}'.format(energy)] > 40) * (df_hh['gain_net_uc_{}'.format(energy)] <= 60)
                + 4 * (df_hh['gain_net_uc_{}'.format(energy)] > 60) * (df_hh['gain_net_uc_{}'.format(energy)] <= 80)
                + 5 * (df_hh['gain_net_uc_{}'.format(energy)] > 80)
                )
        
    for i in range(-6,6):
        df_to_plot['Objective_gains'][i] = df_hh.query('gain_{0} == {1}'.format(energy, i))['weight'].sum() / df_hh['weight'].sum()
   
    df_survey.dropna(subset = ['gain_{}'.format(energy)], inplace = True)
    for i in range(-6,6):
        df_to_plot['Subjective_gains'][i] = df_survey.query('gain_{0} == {1}'.format(energy, i))['weight'].sum() / df_survey['weight'].sum()
    df_to_plot['Subjective_gains'][-1] = df_to_plot['Subjective_gains'][-1] + 0.5 * df_to_plot['Subjective_gains'][0]
    df_to_plot['Subjective_gains'][1] = df_to_plot['Subjective_gains'][1] + 0.5 * df_to_plot['Subjective_gains'][0]        
    df_to_plot = df_to_plot.drop(0)
    
    plot = graph_builder_bar_percent(df_to_plot)
    
    return df_to_plot, plot


if __name__ == "__main__":
    df_hh = prepare_dataset()
    df_hh = compute_gains_losses(df_hh)
    df_hh['weight'] = 1
    df_hh['gain_net_uc_fuel'] = \
        (60 * df_hh['nb_beneficiaries'] - df_hh['transport_expenditures_increase']) / df_hh['consumption_units']
    df_hh['gain_net_uc_chauffage'] = \
        (50 * df_hh['nb_beneficiaries'] - df_hh['housing_expenditures_increase']) / df_hh['consumption_units']
    df_hh['gain_net_uc_taxe_carbone'] = \
        (110 * df_hh['nb_beneficiaries'] - df_hh['total_expenditures_increase']) / df_hh['consumption_units']
    
    df_ptc = pd.read_csv(r'C:\Users\thoma\Documents\Github\beliefs_climate_policies\code\survey_prepared.csv')
    df_ptc['weight'] = 1
    df_ptc['gain_taxe_carbone'] = df_ptc['gain']

    df_to_plot = compare_objective_subjective_beliefs_gains(df_hh, df_ptc, 'fuel')[0]
    #kde = estimate_kde(df_hh, df_survey, 0.6)
