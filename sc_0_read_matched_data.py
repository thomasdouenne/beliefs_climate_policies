# -*- coding: utf-8 -*-

from __future__ import division

import os
import pkg_resources

from openfisca_france_indirect_taxation.surveys import SurveyScenario
from openfisca_france_indirect_taxation.examples.calage_bdf_cn_energy import get_inflators_by_year_energy
from openfisca_france_indirect_taxation.almost_ideal_demand_system.elasticites_aidsills import get_elasticities_aidsills

assets_directory = os.path.join(
    pkg_resources.get_distribution('openfisca_france_indirect_taxation').location
    )


inflators_by_year = get_inflators_by_year_energy(rebuild = False)
year = 2016
data_year = 2011
#elasticities = get_elasticities(data_year)
elasticities = get_elasticities_aidsills(data_year, True)
inflation_kwargs = dict(inflator_by_variable = inflators_by_year[year])


# Homogeneous + SL :
#elasticities['elas_price_1_1'] = -0.466
#elasticities['elas_price_2_2'] = -0.214

# Homogeneous, no SL :
#elasticities['elas_price_1_1'] = -0.440
#elasticities['elas_price_2_2'] = -0.139

survey_scenario = SurveyScenario.create(
    #elasticities = elasticities,
    inflation_kwargs = inflation_kwargs,
    reform_key = 'officielle_2018_in_2016',
    year = year,
    data_year = data_year
    )

simulated_variables = [
    'quantites_gaz_liquefie',
    'quantites_electricite_selon_compteur',
    'quantites_gaz_final',
    'quantites_essence',
    'quantites_diesel',
    'quantites_combustibles_liquides',
    'depenses_carburants_corrigees',
    'depenses_combustibles_liquides',
    'depenses_combustibles_solides',
    'depenses_diesel_corrigees',
    'depenses_electricite',
    'depenses_energies_logement',
    'depenses_energie_thermique',
    'depenses_energies_totales',
    'depenses_essence_corrigees',
    'depenses_gaz_ville',
    'depenses_gaz_liquefie',
    'strate',
    'niveau_vie_decile',
    'pondmen',
    'combustibles_liquides',
    'gaz_ville',
    'agepr',
    'isolation_murs',
    'isolation_fenetres',
    'majorite_double_vitrage',
    'nactifs',
    'nenfants',
    'ocde10',
    'rev_disp_loyerimput',
    'situacj',
    'situapr',
    'log_indiv',
    'bat_av_49',
    'bat_49_74',
    'ouest_sud',
    'surfhab_d',
    'aides_logement',
    'typmen',
    'distance',
    'distance_routiere_hebdomadaire_teg',
    'duree_moyenne_trajet_aller_retour_teg',
    'age_vehicule',
    'stalog',
    'situapr',
    ]

data_menages = survey_scenario.create_data_frame_by_entity(simulated_variables, period = year)['menage']

data_menages.to_csv(os.path.join(assets_directory, 'openfisca_france_indirect_taxation', 'assets',
    'quaids', 'data_menages.csv'), sep = ',')
