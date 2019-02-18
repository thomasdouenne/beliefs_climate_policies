# -*- coding: utf-8 -*-

# Careful : go to nb of adults
# Compute proba for probit model
# Deal with people that consume both gas and domestic fuel

from __future__ import division

import numpy as np

from model_reforms_data.prepare_dataset_housing import prepare_dataset_housing, merge_transport_data
from model_reforms_data.prepare_dataset import prepare_dataset
from model_reforms_data.regression_feedback import compute_gains_losses_housing, \
    regress_ols_housing_expenditures_increase
from model_reforms_data.gains_losses_data import compute_gains_losses


def compute_probability_to_win_from_ols_regression(df_hh, df_estimation):
    results_regressions = regress_ols_housing_expenditures_increase(df_estimation)
    
    df_hh['hh_income_2'] = df_hh['hh_income'] ** 2
    df_hh['Intercept'] = 1
    
    params = results_regressions.params
    params = params.to_frame().T
    explanatory_vars = params.columns.tolist()

    df_hh['predicted_housing_expenditures_increase'] = 0

    for var in explanatory_vars:
        df_hh['predicted_housing_expenditures_increase'] += df_hh[var] * params[var][0]

    df_hh['predicted_total_expenditures_increase'] = (
            df_hh['predicted_housing_expenditures_increase']
            + df_hh['transport_expenditures_increase']
            )

    return df_hh


if __name__ == "__main__":
    df_hh = prepare_dataset_housing('bdf')
    df_transports = prepare_dataset()
    df_hh = merge_transport_data(df_hh, df_transports)
    df_hh = compute_gains_losses(df_hh)
        
    ########### Reprendre ici
    
    df_estimation = prepare_dataset_housing('enl')
    df_estimation = compute_gains_losses_housing(df_estimation)
    
    df_hh['winner'] = 0 + 1 * (df_hh['total_expenditures_increase'] < 132 * df_hh['nb_beneficiaries'])
    
    df_hh = compute_probability_to_win_from_ols_regression(df_hh, df_estimation)
    df_hh['predicted_winner'] = 0 + 1 * (df_hh['predicted_total_expenditures_increase'] < 132 * df_hh['nb_beneficiaries'])
    
    df_hh['mistake'] = \
        1 * ((df_hh['winner'] - df_hh['predicted_winner']) != 0)
    
    print "Share of mistakes:", (float(len(df_hh.query('mistake == 1'))) / len(df_hh))
    print "Probability predict that a loser wins:", float(len(df_hh.query('winner == 0').query('predicted_winner == 1'))) / len(df_hh.query('winner == 0'))
    print "Probability predict that a winner loses:", float(len(df_hh.query('winner == 1').query('predicted_winner == 0'))) / len(df_hh.query('winner == 1'))

    print df_hh['predicted_winner'].mean() 
    print df_hh['winner'].mean()