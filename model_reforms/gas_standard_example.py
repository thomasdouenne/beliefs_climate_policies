# -*- coding: utf-8 -*-

# Here we construct a very standard exemple to study the tax incidence on one consumer
# consuming 1000€ of natural gas per year, and not having a car.

# The case of natural gas is complicated since it is composed of a two-part tariff (fixed price plus price per unit)
# As a result, this energy is subject to about 4 different taxes (TIC, VAT, CTA, TICGN wichich itself includes CSPG and CTSSG)

# Need to change incorrect parameter values / in particular, determine which vat rate applies, and what are the other taxes levied on consumption
# Note that maybe we don't care if these taxes are not earned by the government

# Check current parameters are correct, in particular that there is no mistake between kWh and MWh

from __future__ import division


from define_tax_incidence import *


def natural_gas_example(expenditures):
    
    dict_gas = dict()
    # Set common parameters :
    vat = 0.2 # ???
    e_housing = -0.2
    i = 0.8
    
    current_price = 0.0651 # For someone in zone 3 that use gas for heating
    
    fixed_price_contract = 150
    
    old_carbon_tax = 44.6 # Carbon tax in 2018
    new_carbon_tax = 44.6 + 50 # Carbon tax that we simulate
    carbon_intensity = 0.000181 # Need to check
    
    initial_excise_tax = 0.0003 # Need to check
    
    # Compute tax rates :
    new_carbon_tax = carbon_tax(new_carbon_tax, carbon_intensity)
    old_carbon_tax = carbon_tax(old_carbon_tax, carbon_intensity)
    #print "Carbon tax (before/after)", old_carbon_tax, "/", new_carbon_tax
    
    new_excise_tax = excise_tax(new_carbon_tax, initial_excise_tax)
    old_excise_tax = excise_tax(old_carbon_tax, initial_excise_tax)
    #print "Excise tax (before/after)", old_excise_tax, "/", new_excise_tax
    
    
    # Compute prices :
    new_final_price = final_price_adjusted(current_price, i, new_excise_tax, old_excise_tax)
    final_price_variation = variation_final_price(i, current_price, new_excise_tax, old_excise_tax)
    print "Final price (before/after)", current_price, "/", new_final_price
    
    current_price_without_tax = price_without_tax(current_price, old_excise_tax)
    new_price_without_tax = price_without_tax(new_final_price, new_excise_tax)
    #print "Price without tax (before/after)", current_price_without_tax, "/", new_price_without_tax
    
    
    # Compute quantities :
    current_quantity = quantity(current_price, expenditures - fixed_price_contract)
    new_quantity = adjusted_quantity(current_quantity, e_housing, final_price_variation)
    #print "Quantities (before/after)", current_quantity, "/", new_quantity
    
    
    # Compute expenditures :
    new_expenditures = adjusted_expenditures(expenditures - fixed_price_contract, e_housing, final_price_variation) + fixed_price_contract
    variation_expenditures = new_expenditures - expenditures
    #print "Expenditures (before/after)", expenditures, "/", new_expenditures
    
    
    # Compute taxes paid :
    current_taxes = taxes(current_price_without_tax, current_quantity, old_excise_tax)
    new_taxes = taxes(new_price_without_tax, new_quantity, new_excise_tax)
    #print "Taxes paid (before/after)", current_taxes, "/", new_taxes

    dict_gas['new_expenditures'] = new_expenditures
    dict_gas['variation_expenditures'] = variation_expenditures
    dict_gas['new_quantity'] = new_quantity
    dict_gas['current_quantity'] = current_quantity
    dict_gas['current_taxes'] = current_taxes
    dict_gas['new_taxes'] = new_taxes

    return dict_gas


if __name__ == "__main__":
    expenditures = 1000
    example = natural_gas_example(expenditures)
