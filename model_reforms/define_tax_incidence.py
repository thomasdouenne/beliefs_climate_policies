# -*- coding: utf-8 -*-

# Define functions that enable to compute tax incidence

from __future__ import division


# Set common parameters :
vat = 0.2

def carbon_tax(carbon_price, carbon_intensity):
    carbon_tax = carbon_price * carbon_intensity
    
    return carbon_tax


def excise_tax(carbon_tax, initial_excise_tax):
    excise_tax = initial_excise_tax + carbon_tax
    
    return excise_tax


def price_without_tax(final_price, excise_tax):
    price_without_tax = final_price / (1 + vat) - excise_tax
    
    return price_without_tax


def final_price_adjusted(final_price_current, i, new_excise_tax, old_excise_tax):
    final_price_adjusted = final_price_current + i * (new_excise_tax - old_excise_tax) * (1 + vat)
    
    return final_price_adjusted
    

def variation_final_price(i, final_price_current, new_excise_tax, old_excise_tax):
    variation_final_price = i * (new_excise_tax - old_excise_tax) * (1 + vat) / final_price_current

    return variation_final_price


def quantity(price, expenditures):
    quantity = expenditures / price
    
    return quantity


def adjusted_quantity(initial_quantity, e, variation_final_price):
    variation_quantity = initial_quantity * (1 + e * variation_final_price)
    
    return variation_quantity


def adjusted_expenditures(initial_expenditures, e, variation_final_price):
    variation_expenditures = initial_expenditures * (1 + (1 + e) * variation_final_price)
    
    return variation_expenditures


def taxes(price_without_tax, quantity, excise_tax):
    taxes = vat * price_without_tax * quantity + excise_tax * (1 + vat) * quantity
    
    return taxes
