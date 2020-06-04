#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Thu Feb  7 23:56:02 2019

@author: adrien
"""

# Careful : use nb of adults instead of cu.

from __future__ import division
from sklearn import tree
import graphviz 

import statsmodels.formula.api as smf


from prepare_dataset_housing import prepare_dataset_housing
from regression_feedback import *
from define_tax_incidence_data import *


if __name__ == "__main__":
    df_hh = prepare_dataset_housing('enl')
    df_hh = compute_gains_losses_housing(df_hh)

    df_hh['hh_income_2'] = df_hh['hh_income'] ** 2
    df_hh['heating_oil'] = df_hh['domestic_fuel']

    df_hh['winner'] = 0 + 1 * (df_hh['housing_expenditures_increase'] < 55 * df_hh['nb_beneficiaries'])

    variables = ['hh_income', 'hh_income_2', 'consumption_units', 'nb_beneficiaries', 'natural_gas',
        'heating_oil', 'accommodation_size', 'age_18_24', 'age_25_34', 'age_35_49', 'age_50_64']

    clf = tree.DecisionTreeClassifier(max_depth=3)
    clf = clf.fit(df_hh[variables], df_hh['winner'])
    regr = tree.DecisionTreeRegressor(max_depth=3)
    regr.fit(df_hh[variables], df_hh['winner'])
    
    dot_data = tree.export_graphviz(clf, feature_names = variables, class_names = ['lose', 'win'], leaves_parallel=True, impurity=False, proportion=True, rounded=True, special_characters=True) # filled = True, 
    graph = graphviz.Source(dot_data)
    graph
    graph.render('decision_tree', format='png')
    clf.predict(df_hh[variables])