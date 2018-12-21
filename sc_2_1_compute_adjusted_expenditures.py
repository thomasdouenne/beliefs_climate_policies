# -*- coding: utf-8 -*-

# Compute households expenditures after the policy (+50€/tCO2 on all energies except electricity)

from __future__ import division


from sc_1_prepare_dataset import prepare_dataset


def adjusted_expenditures():
    
    # load dataset
    df_hh = prepare_dataset()

    # Set price and policy parameters
    add_carbon_price = 50 # in euro per tCO2
    carbon_intensity_essence = 0.00228 # in tCO2 per liter
    carbon_intensity_diesel = 0.00265 # in tCO2 per liter
    carbon_intensity_fioul = 0.00266 # in tCO2 per liter
    initial_price_ttc_essence = 1.5 # in euro per liter
    initial_price_ttc_diesel = 1.45 # in euro per liter
    initial_price_ttc_fioul = 0.95 # in euro per liter
    price_elasticity_essence = -0.45
    price_elasticity_diesel = -0.45
    price_elasticity_fioul = -0.2
    
    
    # Compute for each household the cost of the policy
    df_hh['diesel_expenditures_adjusted'] = df_hh['diesel_expenditures'] * (
            1 + (1 + price_elasticity_diesel)*((add_carbon_price * carbon_intensity_diesel) / initial_price_ttc_diesel)
            )
    df_hh['gasoline_expenditures_adjusted'] = df_hh['gasoline_expenditures'] * (
            1 + (1 + price_elasticity_essence)*((add_carbon_price * carbon_intensity_essence) / initial_price_ttc_essence)
            )
    df_hh['domestic_fuel_expenditures_adjusted'] = df_hh['domestic_fuel_expenditures'] * (
            1 + (1 + price_elasticity_fioul)*((add_carbon_price * carbon_intensity_fioul) / initial_price_ttc_fioul)
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
