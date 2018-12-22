# -*- coding: utf-8 -*-

# Plot tax incidence before and after transfers

from __future__ import division

import pandas as pd
import matplotlib.pyplot as plt

from prepare_dataset import prepare_dataset
from gains_losses_data import compute_gains_losses


def graph_builder_bar(graph):
    axes = graph.plot(
        kind = 'bar',
        )
    plt.axhline(0, color = 'b')
    axes.legend(
        bbox_to_anchor = (1.5, 1.05),
        )
    return plt.show()

# Load dataset
df_hh = prepare_dataset()
df_hh = compute_gains_losses(df_hh)

# Compute total revenue from policy
tax_revenue_total = (df_hh['total_tax_increase'] * df_hh['hh_weight']).sum() / 1000000
tax_revenue_per_uc = (df_hh['total_tax_increase'] * df_hh['hh_weight']).sum() / (df_hh['consumption_units'] * df_hh['hh_weight']).sum()
avg_loss_per_uc = (df_hh['total_expenditures_increase'] * df_hh['hh_weight']).sum() / (df_hh['consumption_units'] * df_hh['hh_weight']).sum()


def incidence_decile(data, targeted, amount): # tareted = number of deciles compensated / amount = size of compensation
    df_deciles = pd.DataFrame(index = range(1, 11),
            columns = ['income_decile', 'disposable_income_imputed_rent_cu', 'total_expenditures_increase_cu',
                       'transport_expenditures_increase_cu', 'housing_expenditures_increase_cu']
            )
    for i in range(1,11):
        df_deciles['income_decile'][i] = i
        df_deciles['disposable_income_imputed_rent_cu'][i] = (
            df_hh.query('income_decile == {}'.format(i))['disposable_income_imputed_rent'].mean() / df_hh.query('income_decile == {}'.format(i))['consumption_units'].mean()
            )
        df_deciles['total_expenditures_increase_cu'][i] = (
            df_hh.query('income_decile == {}'.format(i))['total_expenditures_increase'].mean() / df_hh.query('income_decile == {}'.format(i))['consumption_units'].mean()
            )
        df_deciles['transport_expenditures_increase_cu'][i] = (
            df_hh.query('income_decile == {}'.format(i))['transport_expenditures_increase'].mean() / df_hh.query('income_decile == {}'.format(i))['consumption_units'].mean()
            )
        df_deciles['housing_expenditures_increase_cu'][i] = (
            df_hh.query('income_decile == {}'.format(i))['housing_expenditures_increase'].mean() / df_hh.query('income_decile == {}'.format(i))['consumption_units'].mean()
            )
    
    df_deciles['loss_purchasing_power_after_transfer_cu'] = df_deciles['total_expenditures_increase_cu']
    for i in range(1, targeted + 1):
        df_deciles['loss_purchasing_power_after_transfer_cu'][i] = df_deciles['loss_purchasing_power_after_transfer_cu'][i] - amount
    
    df_deciles['total_expenditures_flat_transfer_cu'] = df_deciles['total_expenditures_increase_cu'] - tax_revenue_per_uc
    
    df_deciles['effort_rate_before_transfers'] = df_deciles['total_expenditures_increase_cu'] / df_deciles['disposable_income_imputed_rent_cu']
    df_deciles['effort_rate_after_transfers'] = df_deciles['loss_purchasing_power_after_transfer_cu'] / df_deciles['disposable_income_imputed_rent_cu']
    
    return df_deciles


if __name__ == "__main__":
    df_deciles = incidence_decile(df_hh, 5, 75)
    graph_builder_bar(df_deciles['transport_expenditures_increase_cu'])
    graph_builder_bar(df_deciles['housing_expenditures_increase_cu'])
    graph_builder_bar(df_deciles['total_expenditures_increase_cu'])
    graph_builder_bar(df_deciles['loss_purchasing_power_after_transfer_cu'])
    graph_builder_bar(df_deciles['effort_rate_before_transfers'])
    graph_builder_bar(df_deciles['effort_rate_after_transfers'])
