# -*- coding: utf-8 -*-

from __future__ import division


def variables_names_bdf_to_ptc(df_hh):
    df_hh.rename(
            columns = {
                'age_hh_representative' : 'age_hh_representative',
                'domestic_fuel_expenditures' : 'domestic_fuel_expenditures',
                'natural_gas_expenditures' : 'natural_gas_expenditures',
                'domestic_fuel' : 'fioul',
                'natural_gas' : 'gaz',
                'hh_id' : 'hh_id',
                'income_decile' : 'income_decile',
                'nb_children' : 'nb_children',
                'nb_persons' : 'taille_menage',
                'consumption_units' : 'uc',
                'hh_weight' : 'weight',
                'hh_income' : 'rev_tot',
                'accommodation_size' : 'surface',
                },
            inplace = True,
            )

    return df_hh


def create_new_variables_bdf_ptc(df_hh, df_ptc):
    df_hh['ni_fioul_ni_gaz'] = 1 * ((df_hh['fioul'] + df_hh['gaz']) == 0)
    df_ptc['ni_fioul_ni_gaz'] = 1 * ((df_ptc['fioul'] + df_ptc['gaz']) == 0)
  
    return df_hh, df_ptc


def compute_gains_nets_uc(df_hh):
    df_hh['gain_net_uc_fuel'] = \
        (60 * df_hh['nb_beneficiaries'] - df_hh['transport_expenditures_increase']) / df_hh['consumption_units']
    df_hh['gain_net_uc_chauffage'] = \
        (50 * df_hh['nb_beneficiaries'] - df_hh['housing_expenditures_increase']) / df_hh['consumption_units']
    df_hh['gain_net_uc_taxe_carbone'] = \
        (110 * df_hh['nb_beneficiaries'] - df_hh['total_expenditures_increase']) / df_hh['consumption_units']

    for energy in ['chauffage', 'fuel']:
        df_hh['gain_{}'.format(energy)] = 0 + (
                - 6 * (df_hh['gain_net_uc_{}'.format(energy)] < -160)
                - 5 * (df_hh['gain_net_uc_{}'.format(energy)] >= -160) * (df_hh['gain_net_uc_{}'.format(energy)] < -110)
                - 4 * (df_hh['gain_net_uc_{}'.format(energy)] >= -110) * (df_hh['gain_net_uc_{}'.format(energy)] < -70)
                - 3 * (df_hh['gain_net_uc_{}'.format(energy)] >= -70) * (df_hh['gain_net_uc_{}'.format(energy)] < -40)
                - 2 * (df_hh['gain_net_uc_{}'.format(energy)] >= -40) * (df_hh['gain_net_uc_{}'.format(energy)] < -15)
                - 1 * (df_hh['gain_net_uc_{}'.format(energy)] >= -15) * (df_hh['gain_net_uc_{}'.format(energy)] < -0)
                + 1 * (df_hh['gain_net_uc_{}'.format(energy)] >= 0) * (df_hh['gain_net_uc_{}'.format(energy)] <= 10)
                + 2 * (df_hh['gain_net_uc_{}'.format(energy)] > 10) * (df_hh['gain_net_uc_{}'.format(energy)] <= 20)
                + 3 * (df_hh['gain_net_uc_{}'.format(energy)] > 20) * (df_hh['gain_net_uc_{}'.format(energy)] <= 30)
                + 4 * (df_hh['gain_net_uc_{}'.format(energy)] > 30) * (df_hh['gain_net_uc_{}'.format(energy)] <= 40)
                + 5 * (df_hh['gain_net_uc_{}'.format(energy)] > 40)
                )
    for energy in ['taxe_carbone']:
        df_hh['gain_{}'.format(energy)] = 0 + (
                - 6 * (df_hh['gain_net_uc_{}'.format(energy)] < -280)
                - 5 * (df_hh['gain_net_uc_{}'.format(energy)] >= -280) * (df_hh['gain_net_uc_{}'.format(energy)] < -190)
                - 4 * (df_hh['gain_net_uc_{}'.format(energy)] >= -190) * (df_hh['gain_net_uc_{}'.format(energy)] < -120)
                - 3 * (df_hh['gain_net_uc_{}'.format(energy)] >= -120) * (df_hh['gain_net_uc_{}'.format(energy)] < -70)
                - 2 * (df_hh['gain_net_uc_{}'.format(energy)] >= -70) * (df_hh['gain_net_uc_{}'.format(energy)] < -30)
                - 1 * (df_hh['gain_net_uc_{}'.format(energy)] >= -30) * (df_hh['gain_net_uc_{}'.format(energy)] < -0)
                + 1 * (df_hh['gain_net_uc_{}'.format(energy)] >= 0) * (df_hh['gain_net_uc_{}'.format(energy)] <= 20)
                + 2 * (df_hh['gain_net_uc_{}'.format(energy)] > 20) * (df_hh['gain_net_uc_{}'.format(energy)] <= 40)
                + 3 * (df_hh['gain_net_uc_{}'.format(energy)] > 40) * (df_hh['gain_net_uc_{}'.format(energy)] <= 60)
                + 4 * (df_hh['gain_net_uc_{}'.format(energy)] > 60) * (df_hh['gain_net_uc_{}'.format(energy)] <= 80)
                + 5 * (df_hh['gain_net_uc_{}'.format(energy)] > 80)
                )

    return df_hh


def impute_barycentre_in_bins(df_ptc, df_bdf):
    for energy in ['chauffage', 'fuel']:
        df_ptc['gain_net_numeric_uc_{}'.format(energy)] = 0 + (
                df_bdf.query('gain_{} == -6'.format(energy))['gain_net_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == -6)
                + ((-160 * df_ptc.query('gain_{} == -6'.format(energy))['weight'].sum() - 110 * df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -6'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -5)
                + ((-110 * df_ptc.query('gain_{} == -5'.format(energy))['weight'].sum() - 70 * df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -5'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -4)
                + ((-70 * df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum() - 40 * df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -3)
                + ((-40 * df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum() - 15 * df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -2)
                + ((-15 * df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum() - 0 * df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -1)
                + 0 * (df_ptc['gain_{}'.format(energy)] == 0)
                + ((0 * df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum() + 10 * df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 1)
                + ((10 * df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum() + 20 * df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 2)
                + ((20 * df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum() + 30 * df_ptc.query('gain_{} == 4'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 4'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 3)
                + ((30 * df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum() + 40 * df_ptc.query('gain_{} == 5'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 5'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 4)
                + df_bdf.query('gain_{} == 5'.format(energy))['gain_net_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == 5)
                )
    for energy in ['taxe_carbone']:
        df_ptc['gain_net_numeric_uc_{}'.format(energy)] = 0 + (
                df_bdf.query('gain_{} == -6'.format(energy))['gain_net_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == -6)
                + ((-280 * df_ptc.query('gain_{} == -6'.format(energy))['weight'].sum() - 190 * df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -6'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -5)
                + ((-190 * df_ptc.query('gain_{} == -5'.format(energy))['weight'].sum() - 120 * df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -5'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -4)
                + ((-120 * df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum() - 70 * df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -4'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -3)
                + ((-70 * df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum() - 30 * df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -3'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -2)
                + ((-30 * df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum() - 0 * df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -2'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == -1)
                + 0 * (df_ptc['gain_{}'.format(energy)] == 0)
                + ((0 * df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum() + 20 * df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == -1'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 1)
                + ((20 * df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum() + 40 * df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == 1'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 2)
                + ((40 * df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum() + 60 * df_ptc.query('gain_{} == 4'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == 2'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 4'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 3)
                + ((60 * df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum() + 80 * df_ptc.query('gain_{} == 5'.format(energy))['weight'].sum()) / (df_ptc.query('gain_{} == 3'.format(energy))['weight'].sum() + df_ptc.query('gain_{} == 5'.format(energy))['weight'].sum())) * (df_ptc['gain_{}'.format(energy)] == 4)
                + df_bdf.query('gain_{} == 5'.format(energy))['gain_net_uc_{}'.format(energy)].mean() * (df_ptc['gain_{}'.format(energy)] == 5)
                )

    return df_ptc
