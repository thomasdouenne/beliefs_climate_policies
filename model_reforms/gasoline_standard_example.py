# -*- coding: utf-8 -*-

# Here we construct a very standard exemple to study the tax incidence on one consumer
# consuming 1000€ of gasoline per year, and using electricity only in housing.

from __future__ import division


from define_tax_incidence import *


def gasoline_example(expenditures):

    dict_gasoline = dict()
    # Set common parameters :
    vat = 0.2
    e_transports = -0.4 # -0.3
    i = 0.8 # 0.9
    
    current_price = 1.45 # 1.441 # This is roughly the value of gasoline prices
        
    old_carbon_tax = 44.6 # Carbon tax in 2018
    new_carbon_tax = 44.6 + 50 # Carbon tax that we simulate
    carbon_intensity = 0.002286 # Carbon content of gasoline (deduced from art 265 code des douanes)
    
    initial_excise_tax = 0.6069 - 0.026 # This is roughly the value of the TICPE without carbon tax, but I need to check more precisly
    
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
    #print "Final price (before/after)", current_price, "/", new_final_price
    
    current_price_without_tax = price_without_tax(current_price, old_excise_tax)
    new_price_without_tax = price_without_tax(new_final_price, new_excise_tax)
    #print "Price without tax (before/after)", current_price_without_tax, "/", new_price_without_tax
    
    
    # Compute quantities :
    current_quantity = quantity(current_price, expenditures)
    new_quantity = adjusted_quantity(current_quantity, e_transports, final_price_variation)
    #print "Quantities (before/after)", current_quantity, "/", new_quantity
    
    
    # Compute expenditures :
    new_expenditures = adjusted_expenditures(expenditures, e_transports, final_price_variation)
    variation_expenditures = new_expenditures - expenditures
    #print "Expenditures (before/after)", expenditures, "/", new_expenditures
    
    
    # Compute taxes paid :
    current_taxes = taxes(current_price_without_tax, current_quantity, old_excise_tax)
    new_taxes = taxes(new_price_without_tax, new_quantity, new_excise_tax)
    #print "Taxes paid (before/after)", current_taxes, "/", new_taxes


    dict_gasoline['new_expenditures'] = new_expenditures
    dict_gasoline['variation_expenditures'] = variation_expenditures
    dict_gasoline['new_quantity'] = new_quantity
    dict_gasoline['current_quantity'] = current_quantity
    dict_gasoline['current_taxes'] = current_taxes
    dict_gasoline['new_taxes'] = new_taxes

    return dict_gasoline


if __name__ == "__main__":
    expenditures = 1000
    example = gasoline_example(expenditures)