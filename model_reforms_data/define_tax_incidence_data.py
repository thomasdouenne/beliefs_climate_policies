# -*- coding: utf-8 -*-

# Define functions that enable to compute tax incidence

from __future__ import division


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


def quantity_data(price, data, key):
    data['{}_quantity'.format(key)] = data['{}_expenditures'.format(key)] / price
    
    return data


def co2_emissions_data(co2_content, data, key):
    data['{}_co2_emissions'.format(key)] = co2_content * data['{}_quantity'.format(key)]
    
    return data


def adjusted_quantity_data(e, variation_final_price, data, key):
    data['{}_adjusted_quantity'.format(key)] = data['{}_quantity'.format(key)] * (1 + e * variation_final_price)
    
    return data


def adjusted_expenditures_data(e, variation_final_price, data, key):
    data['{}_adjusted_expenditures'.format(key)] = \
        data['{}_expenditures'.format(key)] * (1 + (1 + e) * variation_final_price)
    
    return data


def adjusted_co2_emissions_data(co2_content, data, key):
    data['{}_adjusted_co2_emissions'.format(key)] = co2_content * data['{}_adjusted_quantity'.format(key)]
    
    return data


def taxes_data(price_without_tax, excise_tax, data, key):
    data['{}_taxes'.format(key)] = data['{}_quantity'.format(key)] * (vat * price_without_tax + excise_tax * (1 + vat))
    
    return data


def adjusted_taxes_data(new_price_without_tax, new_excise_tax, data, key):
    data['{}_adjusted_taxes'.format(key)] = data['{}_adjusted_quantity'.format(key)] * (vat * new_price_without_tax + new_excise_tax * (1 + vat))
    
    return data
