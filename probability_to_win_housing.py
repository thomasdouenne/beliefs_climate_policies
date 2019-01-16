# -*- coding: utf-8 -*-

# Careful : go to nb of adults
# Compute proba for probit model
# Deal with people that consume both gas and domestic fuel

from __future__ import division

import numpy as np

from model_reforms_data.prepare_dataset_housing import prepare_dataset_housing
from model_reforms_data.regression_feedback import compute_gains_losses_housing, \
    predict_winner_looser_housing


def compute_probability_to_win(df_hh, hh_info, results_regressions):
    if results_regressions is None:
        results_regressions = predict_winner_looser_housing(df_hh)
    
    if hh_info['heating'] == 'natural_gas': # Think about the case where tehy consume both
        hh_info['natural_gas'] = 1
        hh_info['domestic_fuel'] = 0
    elif hh_info['heating'] == 'domestic_fuel':
        hh_info['natural_gas'] = 0
        hh_info['domestic_fuel'] = 1
    else:
        hh_info['natural_gas'] = 0
        hh_info['domestic_fuel'] = 0

    hh_info['hh_income_2'] = hh_info['hh_income'] ** 2
    hh_info['Intercept'] = 1
    
    dict_winner = dict()
    for method in ['logit', 'probit', 'ols']:

        if method == 'logit':    
            estimation_winner = results_regressions[0]
        elif method == 'probit':  
            estimation_winner = results_regressions[1]
        else:
            estimation_winner = results_regressions[2]
        params = estimation_winner.params
        params = params.to_frame().T
        explanatory_vars = params.columns.tolist()

        dict_winner['predict_proba_{}'.format(method)] = 0

        for var in explanatory_vars:
            dict_winner['predict_proba_{}'.format(method)] += hh_info[var] * params[var][0]

        if method == 'logit':
            dict_winner['predict_proba_logit'] = np.exp(dict_winner['predict_proba_logit'])
            dict_winner['predict_proba_logit'] = dict_winner['predict_proba_logit'] / (1 + dict_winner['predict_proba_logit'])
    
    return dict_winner


if __name__ == "__main__":
    
    df_hh = prepare_dataset_housing('enl')
    df_hh = compute_gains_losses_housing(df_hh)
    
    hh_info = dict()
    hh_info['consumption_units'] = 1.8
    hh_info['heating'] = 'natural_gas'
    hh_info['accommodation_size'] = 80
    hh_info['hh_income'] = 36000
    hh_info['age_18_24'] = 0
    hh_info['age_25_34'] = 1
    hh_info['age_35_49'] = 0
    hh_info['age_50_64'] = 0

    dict_winner = compute_probability_to_win(df_hh, hh_info, None)
