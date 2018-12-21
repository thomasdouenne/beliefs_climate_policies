# -*- coding: utf-8 -*-

# Compute households expenditures after the policy (+50€/tCO2 on all energies except electricity)

from __future__ import division


from sc_1_prepare_dataset import prepare_dataset


additional_carbon_price = 50 # in euro per tCO2
diesel_carbon_intensity = 0.00265 # in tCO2 per liter (deduced from code des douanes art 265)
gasoline_carbon_intensity = 0.00229 # in tCO2 per liter (deduced from code des douanes art 265)
domestic_fuel_carbon_intensity = 0.00265 # in tCO2 per liter (deduced from code des douanes art 265)

diesel_additional_tax = additional_carbon_price * diesel_carbon_intensity
gasoline_additional_tax = additional_carbon_price * gasoline_carbon_intensity
domestic_fuel_additional_tax = additional_carbon_price * domestic_fuel_carbon_intensity

diesel_initial_price = 1.45 # in euro per liter
gasoline_initial_price = 1.5 # in euro per liter
domestic_fuel_initial_price = 0.95 # in euro per liter

diesel_price_elas = -0.45
gasoline_price_elas = -0.45
domestic_fuel_price_elas = -0.2

def adjusted_expenditures():
    
    # load dataset
    df_hh = prepare_dataset()

    # Compute for each household the cost of the policy
    df_hh['diesel_expenditures_adjusted'] = df_hh['diesel_expenditures'] * (
            1 + (1 + diesel_price_elas)*(diesel_additional_tax / diesel_initial_price)
            )
    df_hh['gasoline_expenditures_adjusted'] = df_hh['gasoline_expenditures'] * (
            1 + (1 + gasoline_price_elas)*(gasoline_additional_tax / gasoline_initial_price)
            )
    df_hh['domestic_fuel_expenditures_adjusted'] = df_hh['domestic_fuel_expenditures'] * (
            1 + (1 + domestic_fuel_price_elas)*(domestic_fuel_additional_tax / domestic_fuel_initial_price)
            )
    
    df_hh['diesel_expenditures_increase'] = (
        df_hh['diesel_expenditures_adjusted'] - df_hh['diesel_expenditures']
        )
    df_hh['gasoline_expenditures_increase'] = (
        df_hh['gasoline_expenditures_adjusted'] - df_hh['gasoline_expenditures']
        )
    df_hh['domestic_fuel_expenditures_increase'] = (
        df_hh['domestic_fuel_expenditures_adjusted'] - df_hh['domestic_fuel_expenditures']
        )
    df_hh['transport_energy_expenditures_increase'] = (
        df_hh['diesel_expenditures_increase'] + df_hh['gasoline_expenditures_increase']
        )
    df_hh['housing_energy_expenditures_increase'] = (
        df_hh['domestic_fuel_expenditures_increase']
        )
    df_hh['total_energy_expenditures_increase'] = (
        df_hh['transport_energy_expenditures_increase'] + df_hh['housing_energy_expenditures_increase']
        )

    return df_hh


if __name__ == "__main__":
    df_hh = adjusted_expenditures()

    print "expenditures after tax / before tax :"
    print df_hh['gasoline_expenditures_adjusted'].mean(), "/", df_hh['gasoline_expenditures'].mean()
    print df_hh['diesel_expenditures_adjusted'].mean(), '/', df_hh['diesel_expenditures'].mean()
    print df_hh['domestic_fuel_expenditures_adjusted'].mean(), '/', df_hh['domestic_fuel_expenditures'].mean()
