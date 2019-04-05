
from __future__ import division

import pandas as pd

from prepare_dataset import prepare_dataset
from define_tax_incidence_data import quantity_data, adjusted_quantity_data, co2_emissions_data, adjusted_co2_emissions_data


df_hh = prepare_dataset()


reduced_emissions = 0
for energy in ['diesel', 'gasoline', 'domestic_fuel', 'gas']:
    
    if energy == 'diesel':
        co2_content = 0.00265
        e = -0.4
        price = 1.4
        price_variation = 0.0907
    elif energy == 'gasoline':
        e = -0.4
        co2_content = 0.00228
        price = 1.45
        price_variation = 0.0756
    elif energy == 'domestic_fuel':
        e = -0.4
        co2_content = 0.00265
        price = 0.859
        price_variation = 0.1481
    else:
        e = -0.4
        co2_content = 0.000181
        price = 0.0651
        price_variation = 0.1334

    
    df_hh = adjusted_quantity_data(e, price_variation, df_hh, energy)
    df_hh = co2_emissions_data(co2_content, df_hh, energy)
    df_hh = adjusted_co2_emissions_data(co2_content, df_hh, energy)
    reduced_emissions += (df_hh['{}_adjusted_co2_emissions'.format(energy)] * df_hh['hh_weight']).sum() - (df_hh['{}_co2_emissions'.format(energy)] * df_hh['hh_weight']).sum()
    print "Reduction in CO2 emissions {} :".format(energy), \
        (df_hh['{}_adjusted_co2_emissions'.format(energy)] * df_hh['hh_weight']).sum() - (df_hh['{}_co2_emissions'.format(energy)] * df_hh['hh_weight']).sum()

print "Sum of CO2 emissions reduced:", reduced_emissions
