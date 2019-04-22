# -*- coding: utf-8 -*-

# Careful : go to nb of adults
# Compute proba for probit model
# Deal with people that consume both gas and domestic fuel

from __future__ import division

import numpy as np

from model_reforms_data.prepare_dataset_housing import prepare_dataset_housing
from model_reforms_data.regression_feedback import compute_gains_losses_housing, \
    predict_winner_looser_housing

from scipy.stats import norm
from sklearn import tree

def compute_probability_to_win_housing(df_hh, df_estimation):
    results_regressions = predict_winner_looser_housing(df_estimation)
    
    df_hh['hh_income_2'] = df_hh['hh_income'] ** 2
    df_hh['natural_gas:accommodation_size'] = df_hh['accommodation_size'] * df_hh['natural_gas']
    df_hh['domestic_fuel:accommodation_size'] = df_hh['accommodation_size'] * df_hh['domestic_fuel']
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
    
    df_hh['winner'] = 0 + 1 * (df_hh['housing_expenditures_increase'] < 50 * df_hh['nb_beneficiaries'])
    
    df_hh = compute_probability_to_win_housing(df_hh, df_estimation)

    for method in ['logit', 'probit', 'ols']:
        df_hh['predicted_winner_{}'.format(method)] = 0 + 1 * (df_hh['predict_proba_{}'.format(method)] > 0.5)
        df_hh['mistake_{}'.format(method)] = \
            1 * ((df_hh['winner'] - df_hh['predicted_winner_{}'.format(method)]) != 0)
        
        print "Share of mistakes with {}:".format(method), (float(sum(df_hh['mistake_{}'.format(method)])) / len(df_hh))
        print "Probability predict that a loser wins with {}:".format(method), float(len(np.where((df_hh['winner']==0) & (df_hh['predicted_winner_{}'.format(method)] == 1))[0])) / len(np.where(df_hh['winner']==0)[0])
        print "Probability predict that a winner loses with {}:".format(method), float(len(np.where((df_hh['winner']==1) & (df_hh['predicted_winner_{}'.format(method)] == 0))[0])) / len(np.where(df_hh['winner']==1)[0])
        
    logit, probit, ols, clf = predict_winner_looser_housing(df_estimation)
    params = logit.params.to_frame().T.columns.tolist()
    df_hh['predicted_winner_tree'] = 0 + 1 * (clf.predict(df_hh[params]) > 0.5)
    df_hh['mistake_tree'] = 1 * (df_hh['winner'] != df_hh['predicted_winner_tree'])
    
    print "Share of mistakes with tree", (float(sum(df_hh['mistake_tree'])) / len(df_hh))
    print "Probability predict that a loser wins with tree", float(len(np.where((df_hh['winner']==0) & (df_hh['predicted_winner_tree'] == 1))[0])) / len(np.where(df_hh['winner']==0)[0])
    print "Probability predict that a winner loses with tree", float(len(np.where((df_hh['winner']==1) & (df_hh['predicted_winner_tree'] == 0))[0])) / len(np.where(df_hh['winner']==1)[0])
