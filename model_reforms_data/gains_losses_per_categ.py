# -*- coding: utf-8 -*-

# Make sure categories are all large enough (pay attention to hh with large nb of cu)
# Switch to revtot variable for income
# Swicth to number of persons in the hh and take intervals for high numbers

from __future__ import division

import pandas as pd

from prepare_dataset import prepare_dataset
from gains_losses_data import compute_gains_losses


def match_households_per_categ(df_hh, cu, heating, size, hh_income):
    
    # Select household size
    df_hh = df_hh.query('consumption_units == {}'.format(cu))

    # Select energy for heating
    df_hh = df_hh.query('{} == 1'.format(heating))

    # Select accomodation size : up to plus or minus 30% than the hh
    df_hh = df_hh.query('accommodation_size > 0.7 * {}'.format(size))
    df_hh = df_hh.query('accommodation_size < 1.3 * {}'.format(size))
    
    # Select age category
    
    # Select income
    df_hh = df_hh.query('disposable_income_imputed_rent > 0.7 * {}'.format(hh_income * 12))
    df_hh = df_hh.query('disposable_income_imputed_rent < 1.3 * {}'.format(hh_income * 12))

    return df_hh


if __name__ == "__main__":
    df_hh = prepare_dataset()

    consumption_units = 1.8
    heating = 'natural_gas'
    accommodation_size = 80
    hh_income = 2500

    df_hh = match_households_per_categ(df_hh, consumption_units, heating, accommodation_size, hh_income)
    
    print df_hh['{}_expenditures'.format(heating)].mean()
    
    
    #for cu in [1, 1.3, 1.5, 1.6, 1.8, 1.9, 2, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3, 3.1, 3.2, 3.3, 3.4, 3.5, 3.6]:
    #    print cu, len(df_hh.query('consumption_units == {}'.format(cu)))
