# -*- coding: utf-8 -*-

# To do : find how to compute a non-parametric density function from these bins (somethig like KDE for aggregated data)

from __future__ import division

import pandas as pd

from model_reforms_data.prepare_dataset import prepare_dataset
from model_reforms_data.gains_losses_data import compute_gains_losses
from utils import graph_builder_bar_percent


def compare_elasticities(df_survey, energy):

    df_survey.dropna(subset = ['Elasticite_{}'.format(energy)], inplace = True)
    df_survey['difference_elasticities'] = df_survey['Elasticite_{}'.format(energy)] - df_survey['Elasticite_{}_perso'.format(energy)]

    print df_survey['Elasticite_{}'.format(energy)].mean()
    print df_survey['Elasticite_{}_perso'.format(energy)].mean()
    print df_survey['difference_elasticities'].mean()

    return df_to_plot


if __name__ == "__main__":
    df_survey = pd.read_csv(r'C:\Users\thoma\Documents\Github\beliefs_climate_policies\code\survey_prepared.csv')
    print df_survey['Elasticite_chauffage']
    df_survey['weight'] = 1
    energy = 'chauffage'
