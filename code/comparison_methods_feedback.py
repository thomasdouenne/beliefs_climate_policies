# -*- coding: utf-8 -*-

# We select a subsample of households from BdF, and apply our different methods to determine the feedbacks to give
# We will then compare the goodness of fit of these methods.

from __future__ import division
import time

from model_reforms_data.gains_losses_data import compute_gains_losses
from model_reforms_data.gains_losses_per_categ import match_households_per_categ
from model_reforms_data.prepare_dataset import prepare_dataset
from model_reforms_data.prepare_dataset_housing import prepare_dataset_housing
from model_reforms_data.regression_feedback import compute_gains_losses_housing, regress_ols_housing_expenditures_increase
from model_reforms.diesel_standard_example import diesel_example
from model_reforms.domestic_fuel_standard_example import domestic_fuel_example
from model_reforms.gas_standard_example import natural_gas_example
from model_reforms.gasoline_standard_example import gasoline_example
from computations_feedback_regression import loss_purchasing_power_from_regression

start = time.time()

diesel_price = 1.40
gasoline_price = 1.45

df_bdf = prepare_dataset()
df_bdf = compute_gains_losses(df_bdf)
df_bdf = df_bdf.sample(1000) # Select a random sample

df_enl = prepare_dataset_housing('enl')
df_enl = compute_gains_losses_housing(df_enl)

regression_ols = regress_ols_housing_expenditures_increase(df_enl)   
    
hh_info = dict()
df_bdf['housing_expenditures_increase_from_regression'] = 0

for index in df_bdf.iterrows():
    i = index[0]
    hh_info['consumption_units'] = df_bdf['consumption_units'][i]
    if df_bdf['domestic_fuel'][i] == 1:
        hh_info['heating'] = 'domestic_fuel'
    else:
        hh_info['heating'] = 'natural_gas'
    hh_info['accommodation_size'] = df_bdf['accommodation_size'][i]
    hh_info['hh_income'] = df_bdf['hh_income'][i]
    
    hh_info['nb_vehicles'] = 2
    hh_info['energy_first_vehicle'] = 'gasoline'
    hh_info['energy_second_vehicle'] = 'diesel'
    hh_info['avg_fuel_consumption'] = 8 / 100
    hh_info['nb_kilometers'] = 20000

    hh_info['age_18_24'] = 0 + 1 * (df_bdf['age_hh_representative'][i] > 17) * (df_bdf['age_hh_representative'][i] < 25)
    hh_info['age_25_34'] = 0 + 1 * (df_bdf['age_hh_representative'][i] > 24) * (df_bdf['age_hh_representative'][i] < 35)
    hh_info['age_35_49'] = 0 + 1 * (df_bdf['age_hh_representative'][i] > 34) * (df_bdf['age_hh_representative'][i] < 50)
    hh_info['age_50_64'] = 0 + 1 * (df_bdf['age_hh_representative'][i] > 49) * (df_bdf['age_hh_representative'][i] < 65)

    dict_loss = loss_purchasing_power_from_regression(df_enl, hh_info, regression_ols)
    df_bdf['housing_expenditures_increase_from_regression'][i] = dict_loss['housing']


end = time.time()
print end - start

r_squared_from_regressed_data = (
        ((df_bdf['housing_expenditures_increase_from_regression'] - df_bdf['housing_expenditures_increase'].mean())**2).sum() /
        ((df_bdf['housing_expenditures_increase'] - df_bdf['housing_expenditures_increase'].mean())**2).sum()
        )
print r_squared_from_regressed_data
