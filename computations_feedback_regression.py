# -*- coding: utf-8 -*-

# Careful : if the person does not match anyone, we get a Nan
# Deal with people that consume both gas and domestic fuel
# Update avg fuel consumption

from __future__ import division

import pandas as pd
import time

from model_reforms_data.prepare_dataset_housing import prepare_dataset_housing
from model_reforms_data.regression_feedback import compute_gains_losses_housing, regress_ols_housing_expenditures_increase
from model_reforms.diesel_standard_example import diesel_example
from model_reforms.gasoline_standard_example import gasoline_example


def loss_purchasing_power_from_regression(df_hh, hh_info, regression_ols):
    diesel_price = 1.4
    gasoline_price = 1.45    
    
    if hh_info['nb_vehicles'] == 0: # If we don't know ther vehicle, we impute distance half and half for diesel and gasoline
        diesel_expenditures = (hh_info['nb_kilometers'] / 2) * (6.39 / 100) * diesel_price # avg fuel consumption from Statista
        gasoline_expenditures = (hh_info['nb_kilometers'] / 2) * (7.31 / 100) * gasoline_price # avg fuel consumption from Statista
    
    if hh_info['nb_vehicles'] == 1:
        if hh_info['energy_first_vehicle'] == 'diesel':
            diesel_expenditures = hh_info['nb_kilometers'] * hh_info['avg_fuel_consumption'] * diesel_price
            gasoline_expenditures = 0
        if hh_info['energy_first_vehicle'] == 'gasoline':
            gasoline_expenditures = hh_info['nb_kilometers'] * hh_info['avg_fuel_consumption'] * gasoline_price
            diesel_expenditures = 0
    
    if hh_info['nb_vehicles'] == 2: # If the hh has several vehicles, we assume 2/3 of the distance is made with the main one
        if hh_info['energy_first_vehicle'] == 'diesel':
            diesel_expenditures = (hh_info['nb_kilometers'] * 2 / 3) * hh_info['avg_fuel_consumption'] * diesel_price
            gasoline_expenditures = 0
        else:
            gasoline_expenditures = (hh_info['nb_kilometers'] * 2 / 3) * hh_info['avg_fuel_consumption'] * gasoline_price
            diesel_expenditures = 0
        if hh_info['energy_second_vehicle'] == 'diesel':
            diesel_expenditures = diesel_expenditures + (hh_info['nb_kilometers'] * 1 / 3) * hh_info['avg_fuel_consumption'] * diesel_price
            gasoline_expenditures = gasoline_expenditures
        else:
            gasoline_expenditures = gasoline_expenditures + (hh_info['nb_kilometers'] * 1 / 3) * hh_info['avg_fuel_consumption'] * gasoline_price
            diesel_expenditures = diesel_expenditures  
    
    """ Estimation of increase in housing energy expenditures from regression """

    if hh_info['heating'] == 'natural_gas': # Think about the case where tehy consume both
        hh_info['natural_gas'] = 1
        hh_info['domestic_fuel'] = 0
    else:
        hh_info['natural_gas'] = 0
        hh_info['domestic_fuel'] = 1    

    hh_info['hh_income_2'] = hh_info['hh_income'] ** 2

    if regression_ols == None:
        regression_ols = regress_ols_housing_expenditures_increase(df_hh)

    params = regression_ols.params
    params = params.to_frame().T
    explanatory_vars = params.columns.tolist()
    
    housing_variation_expenditures = 0
    for var in explanatory_vars:
        if var == 'Intercept':
            housing_variation_expenditures = params[var][0]
        else:
            housing_variation_expenditures = housing_variation_expenditures + hh_info[var] * params[var][0]
    

    dict_diesel = diesel_example(diesel_expenditures)
    dict_gasoline = gasoline_example(gasoline_expenditures)
    
    dict_loss = dict()
    dict_loss['housing'] = housing_variation_expenditures
    dict_loss['diesel'] = dict_diesel['variation_expenditures']
    dict_loss['gasoline'] = dict_gasoline['variation_expenditures']
    dict_loss['transports'] = dict_loss['diesel'] + dict_loss['gasoline']
    dict_loss['total'] = dict_loss['housing'] + dict_loss['transports']

    return dict_loss

if __name__ == "__main__":
        
    df_hh = prepare_dataset_housing('enl')
    df_hh = compute_gains_losses_housing(df_hh)

    hh_info = dict()

    hh_info['consumption_units'] = 1.5
    hh_info['heating'] = 'natural_gas'
    hh_info['accommodation_size'] = 80
    hh_info['hh_income'] = 2500
    
    hh_info['nb_vehicles'] = 2
    hh_info['energy_first_vehicle'] = 'gasoline'
    hh_info['energy_second_vehicle'] = 'diesel'
    hh_info['avg_fuel_consumption'] = 8 / 100
    hh_info['nb_kilometers'] = 20000
    hh_info['age_18_24'] = 0
    hh_info['age_25_34'] = 0
    hh_info['age_35_49'] = 0
    hh_info['age_50_64'] = 1
        
    dict_loss = loss_purchasing_power_from_regression(df_hh, hh_info, None)

    print "Loss purchasing power from housing : ", dict_loss['housing']
    print "Loss purchasing power from transports : ", dict_loss['transports']
    print "Total loss purchasing power : ", dict_loss['total']
