# -*- coding: utf-8 -*-

# Here we construct a very standard exemple to study the tax incidence on one consumer
# consuming 1000€ of domestic fuel per year, and not having a car.

# Need to change incorrect parameter values

from __future__ import division


from define_tax_incidence import *

# Set common parameters :
vat = 0.2
e_transports = -0.4
i = 0.9

current_price = 1.45 # This is roughly the value of gasoline prices

expenditures = 1000

old_carbon_tax = 44.6 # Carbon tax in 2018
new_carbon_tax = 44.6 + 50 # Carbon tax that we simulate
carbon_intensity = 0.002651 # Carbon content of gasoline (deduced from art 265 code des douanes)

initial_excise_tax = 0.4284 + 2*0.026 # This is roughly the value of the TICPE without carbon tax, but I need to check more precisly

# Compute tax rates :
new_carbon_tax = carbon_tax(new_carbon_tax, carbon_intensity)
old_carbon_tax = carbon_tax(old_carbon_tax, carbon_intensity)
print "Carbon tax (before/after)", old_carbon_tax, "/", new_carbon_tax

new_excise_tax = excise_tax(new_carbon_tax, initial_excise_tax)
old_excise_tax = excise_tax(old_carbon_tax, initial_excise_tax)
print "Excise tax (before/after)", old_excise_tax, "/", new_excise_tax


# Compute prices :
new_final_price = final_price_adjusted(current_price, i, new_excise_tax, old_excise_tax)
variation_final_price = variation_final_price(i, current_price, new_excise_tax, old_excise_tax)
print "Final price (before/after)", current_price, "/", new_final_price

current_price_without_tax = price_without_tax(current_price, old_excise_tax)
new_price_without_tax = price_without_tax(new_final_price, new_excise_tax)
print "Price without tax (before/after)", current_price_without_tax, "/", new_price_without_tax


# Compute quantities :
current_quantity = quantity(current_price, expenditures)
adjusted_quantity = adjusted_quantity(current_quantity, e_transports, variation_final_price)
print "Quantities (before/after)", current_quantity, "/", adjusted_quantity


# Compute expenditures :
adjusted_expenditures = adjusted_expenditures(expenditures, e_transports, variation_final_price)
print "Expenditures (before/after)", expenditures, "/", adjusted_expenditures


# Compute taxes paid :
current_taxes = taxes(current_price_without_tax, current_quantity, old_excise_tax)
new_taxes = taxes(new_price_without_tax, adjusted_quantity, new_excise_tax)
print "Taxes paid (before/after)", current_taxes, "/", new_taxes
