# -*- coding: utf-8 -*-

# Careful : if the person does not match anyone, we get a Nan
# Deal with people that consume both gas and domestic fuel
# Update avg fuel consumption

from __future__ import division

import pandas as pd

from model_reforms_data.gains_losses_per_categ import match_households_per_categ
from model_reforms_data.prepare_dataset import prepare_dataset
from model_reforms_data.prepare_dataset_enl import prepare_dataset_enl
from model_reforms.diesel_standard_example import diesel_example
from model_reforms.domestic_fuel_standard_example import domestic_fuel_example
from model_reforms.gas_standard_example import natural_gas_example
from model_reforms.gasoline_standard_example import gasoline_example


def loss_purchasing_power(df_hh, consumption_units, heating, accommodation_size, hh_income, nb_vehicles,
                          energy_first_vehicle, energy_second_vehicle, avg_fuel_consumption, nb_kilometers):
    if nb_vehicles == 0: # If we don't know ther vehicle, we impute distance half and half for diesel and gasoline
        diesel_expenditures = (nb_kilometers / 2) * (6.39 / 100) * diesel_price # avg fuel consumption from Statista
        gasoline_expenditures = (nb_kilometers / 2) * (7.31 / 100) * gasoline_price # avg fuel consumption from Statista
    
    if nb_vehicles == 1:
        if energy_first_vehicle == 'diesel':
            diesel_expenditures = nb_kilometers * avg_fuel_consumption * diesel_price
            gasoline_expenditures = 0
        if energy_first_vehicle == 'gasoline':
            gasoline_expenditures = nb_kilometers * avg_fuel_consumption * gasoline_price
            diesel_expenditures = 0
    
    if nb_vehicles == 2: # If the hh has several vehicles, we assume 2/3 of the distance is made with the main one
        if energy_first_vehicle == 'diesel':
            diesel_expenditures = (nb_kilometers * 2 / 3) * avg_fuel_consumption * diesel_price
            gasoline_expenditures = 0
        else:
            gasoline_expenditures = (nb_kilometers * 2 / 3) * avg_fuel_consumption * gasoline_price
            diesel_expenditures = 0
        if energy_second_vehicle == 'diesel':
            diesel_expenditures = diesel_expenditures + (nb_kilometers * 1 / 3) * avg_fuel_consumption * diesel_price
            gasoline_expenditures = gasoline_expenditures
        else:
            gasoline_expenditures = gasoline_expenditures + (nb_kilometers * 1 / 3) * avg_fuel_consumption * gasoline_price
            diesel_expenditures = diesel_expenditures  
    
    """ Match households to guess housing energy expenditures """
    df_hh = match_households_per_categ(df_hh, consumption_units, heating, accommodation_size, hh_income)
    if heating == 'natural_gas':
        housing_mean_expenditures = df_hh['{}_variable_expenditures'.format(heating)].mean()
    else:
        housing_mean_expenditures = df_hh['{}_expenditures'.format(heating)].mean()
    
    """ Compute households transport expenditures from information """
        
    if heating == 'natural_gas':
        dict_housing = natural_gas_example(housing_mean_expenditures)
    else:
        dict_housing = domestic_fuel_example(housing_mean_expenditures)
    dict_diesel = diesel_example(diesel_expenditures)
    dict_gasoline = gasoline_example(gasoline_expenditures)
    
    dict_loss = dict()
    dict_loss['housing'] = dict_housing['variation_expenditures']
    dict_loss['diesel'] = dict_diesel['variation_expenditures']
    dict_loss['gasoline'] = dict_gasoline['variation_expenditures']
    dict_loss['transports'] = dict_loss['diesel'] + dict_loss['gasoline']
    dict_loss['total'] = dict_loss['housing'] + dict_loss['transports']

    return dict_loss

if __name__ == "__main__":
    
    df_hh = prepare_dataset_enl()

    consumption_units = 1.5
    heating = 'natural_gas'
    accommodation_size = 80
    hh_income = 2500
    
    nb_vehicles = 2
    energy_first_vehicle = 'gasoline'
    energy_second_vehicle = 'diesel'
    avg_fuel_consumption = 8 / 100
    nb_kilometers = 20000
    
    diesel_price = 1.45
    gasoline_price = 1.5
    
    dict_loss = loss_purchasing_power(df_hh, consumption_units, heating, accommodation_size, hh_income, nb_vehicles,
                                      energy_first_vehicle, energy_second_vehicle, avg_fuel_consumption, nb_kilometers)
    
    print "Loss purchasing power from housing : ", dict_loss['housing']
    print "Loss purchasing power from transports : ", dict_loss['transports']
    print "Total loss purchasing power : ", dict_loss['total']
