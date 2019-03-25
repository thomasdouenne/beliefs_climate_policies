# -*- coding: utf-8 -*-

# To do : use weights

from __future__ import division

import pandas as pd

from model_reforms_data.prepare_dataset import prepare_dataset

try:
    df_ptc = pd.read_csv(r'C:\Users\thoma\Documents\Github\beliefs_climate_policies\code\survey_prepared.csv')
except:
    df_ptc = pd.read_csv(r'C:\Users\t.douenne\Documents\Github\beliefs_climate_policies\code\survey_prepared.csv')
df_bdf = prepare_dataset()


# Energies : les gens ne consomment pas assez de fioul ou de gaz dans notre sondage par rapport à BdF
print 'Fioul:', 'PTC:', df_ptc['fioul'].mean(), " / BDF:", df_bdf['domestic_fuel'].mean()
print 'Gaz:', 'PTC:', df_ptc['gaz'].mean(), " / BDF:", df_bdf['natural_gas'].mean()


# KM parcourus : à peu près OK, légèrement plus dispersé dans notre sondage
for i in [.1, .2, .3, .4, .5, .6, .7, .8, .9, .95]:
    print i, "PTC:", df_ptc['km'].quantile(i), " / BDF:", df_bdf['distance'].quantile(i)


# Revenu : Pas si loin des seuils supposés pour chaque décile
for i in [.1, .2, .3, .4, .5, .6, .7, .8, .9]:
    print i, "PTC:", df_ptc['revenu'].quantile(i)


# Revenu ménage : les gens sont beaucoup trop pauvres dans notre sondage par rapport à BdF
for i in [.05, .1, .25, .5, .75, .9, .95]:
    print i, "PTC:", df_ptc['rev_tot'].quantile(i), " / BDF:", (df_bdf['hh_income'].quantile(i) / 12)


# Taille logement : quasi parfait
for i in [.05, .1, .25, .5, .75, .9, .95]:
    print i, "PTC:", df_ptc['surface'].quantile(i), " / BDF:", df_bdf['accommodation_size'].quantile(i)


# Taille ménage : quasi parfait
for i in [.05, .1, .175, .25, .35, .5, .6, .75, .9, .95]:
    print i, "PTC:", df_ptc['taille_menage'].quantile(i), " / BDF:", df_bdf['nb_persons'].quantile(i)


# Age : les gens sont beaucoup trop jeunes dans notre sondage par rapport à BdF
df_bdf['age_18_24'] = 1 * (df_bdf['age_hh_representative'] <= 24)
df_bdf['age_25_34'] = 1 * (df_bdf['age_hh_representative'] >= 25) * (df_bdf['age_hh_representative'] <= 34)
df_bdf['age_35_49'] = 1 * (df_bdf['age_hh_representative'] >= 35) * (df_bdf['age_hh_representative'] <= 49)
df_bdf['age_50_64'] = 1 * (df_bdf['age_hh_representative'] >= 50) * (df_bdf['age_hh_representative'] <= 64)
df_bdf['age_65_plus'] = 1 * (df_bdf['age_hh_representative'] >= 65)

print '18-24:', 'PTC:', df_ptc['age_18_24'].mean(), " / BDF:", df_bdf['age_18_24'].mean()
print '25-34:', 'PTC:', df_ptc['age_25_34'].mean(), " / BDF:", df_bdf['age_25_34'].mean()
print '35-49:', 'PTC:', df_ptc['age_35_49'].mean(), " / BDF:", df_bdf['age_35_49'].mean()
print '50-64:', 'PTC:', df_ptc['age_50_64'].mean(), " / BDF:", df_bdf['age_50_64'].mean()
print '65-:', 'PTC:', df_ptc['age_65_plus'].mean(), " / BDF:", df_bdf['age_65_plus'].mean()

# Occupation
