# -*- coding: utf-8 -*-

# Careful : go to nb of adults
# Compute proba for probit model
# Deal with people that consume both gas and domestic fuel

from __future__ import division

import numpy as np
from scipy.stats import norm

from model_reforms_data.prepare_dataset_housing import prepare_dataset_housing
from model_reforms_data.regression_feedback import compute_gains_losses_housing, \
    predict_winner_looser_housing


def compute_probability_to_win_housing(df_hh, df_estimation):
    results_regressions = predict_winner_looser_housing(df_estimation)
    
    df_hh['hh_income_2'] = df_hh['hh_income'] ** 2
    df_hh['Intercept'] = 1
    
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

        df_hh['predict_proba_{}'.format(method)] = 0

        for var in explanatory_vars:
            df_hh['predict_proba_{}'.format(method)] += df_hh[var] * params[var][0]

        if method == 'logit':
            df_hh['predict_proba_logit'] = np.exp(df_hh['predict_proba_logit'])
            df_hh['predict_proba_logit'] = df_hh['predict_proba_logit'] / (1 + df_hh['predict_proba_logit'])

        if method == 'probit':
            df_hh['predict_proba_probit'] = norm.cdf(df_hh['predict_proba_probit'])

    return df_hh


if __name__ == "__main__":
    
    df_hh = prepare_dataset_housing('bdf')
    df_estimation = prepare_dataset_housing('enl')

    df_hh = compute_gains_losses_housing(df_hh)
    df_estimation = compute_gains_losses_housing(df_estimation)
    
    df_hh['winner'] = 0 + 1 * (df_hh['housing_expenditures_increase'] < 55 * df_hh['nb_beneficiaries'])
    
    df_hh = compute_probability_to_win_housing(df_hh, df_estimation)

    for method in ['logit', 'probit', 'ols']:
        df_hh['predicted_winner_{}'.format(method)] = 0 + 1 * (df_hh['predict_proba_{}'.format(method)] > 0.5)
        df_hh['mistake'] = \
            1 * ((df_hh['winner'] - df_hh['predicted_winner_{}'.format(method)]) != 0)
        
        print "Share of mistakes with {}:".format(method), (float(len(df_hh.query('mistake == 1'))) / len(df_hh))
        print "Probability predict that a loser wins with {}:".format(method), float(len(df_hh.query('winner == 0').query('predicted_winner_{} == 1'.format(method)))) / len(df_hh.query('winner == 0'))
        print "Probability predict that a winner loses with {}:".format(method), float(len(df_hh.query('winner == 1').query('predicted_winner_{} == 0'.format(method)))) / len(df_hh.query('winner == 1'))
