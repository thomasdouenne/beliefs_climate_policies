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

# Households losses in purchasing power (increase in expenditures) prior revenue-recycling:
df_deciles = pd.DataFrame(index = range(1, 11),
        columns = ['income_decile', 'total_expenditures_increase_cu',
                   'transport_expenditures_increase_cu', 'housing_expenditures_increase_cu']
        )
for i in range(1,11):
    df_deciles['income_decile'][i] = i
    df_deciles['total_expenditures_increase_cu'][i] = (
        df_hh.query('income_decile == {}'.format(i))['total_expenditures_increase'].mean() / df_hh.query('income_decile == {}'.format(i))['consumption_units'].mean()
        )
    df_deciles['transport_expenditures_increase_cu'][i] = (
        df_hh.query('income_decile == {}'.format(i))['transport_expenditures_increase'].mean() / df_hh.query('income_decile == {}'.format(i))['consumption_units'].mean()
        )
    df_deciles['housing_expenditures_increase_cu'][i] = (
        df_hh.query('income_decile == {}'.format(i))['housing_expenditures_increase'].mean() / df_hh.query('income_decile == {}'.format(i))['consumption_units'].mean()
        )

graph_builder_bar(df_deciles['transport_expenditures_increase_cu'])
graph_builder_bar(df_deciles['housing_expenditures_increase_cu'])
graph_builder_bar(df_deciles['total_expenditures_increase_cu'])


# Households losses in purchasing power after flat-transfer:
df_deciles['total_expenditures_flat_transfer_cu'] = df_deciles['total_expenditures_increase_cu'] - tax_revenue_per_uc
graph_builder_bar(df_deciles['total_expenditures_flat_transfer_cu'])
