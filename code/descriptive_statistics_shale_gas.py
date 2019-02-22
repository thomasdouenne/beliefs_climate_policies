# -*- coding: utf-8 -*-
"""
Created on Fri Feb 22 10:00:19 2019

@author: thoma
"""

import pandas as pd

df_survey = pd.read_csv(r'C:\Users\thoma\Documents\Data\assets\data_qualtrics.csv')
df_survey = df_survey[2:]

liste_variables = df_survey.columns.tolist()


""" Timing of the survey """

df_survey['temps'] = df_survey['Duration (in seconds)'].astype(int)
print "Average duration in minutes:", df_survey.temps.mean() / 60
print "Median duration in minutes:", df_survey.temps.quantile(0.5) / 60


""" Shale gas - full sample """

df_survey['approuve_schiste'] = 0 + 1 * (df_survey['Q197'] == 'Oui')
print "Average number of people in favor of shale gas extraction:", df_survey['approuve_schiste'].mean()

df_survey['schiste_bon_pour_climat_nsp'] = 0 + 1 * (df_survey['Q198'] == 'NSP (Ne sait pas, ne se prononce pas)')
df_survey['schiste_bon_pour_climat_non'] = 0 + 1 * (df_survey['Q198'].str[:17] == 'Elle est malvenue')
df_survey['schiste_bon_pour_climat_oui'] = 0 + 1 * (df_survey['Q198'].str[:16] == 'Elle est valable')
print "Average number of people who don't know whether shale gas is good for climate:", \
    df_survey['schiste_bon_pour_climat_nsp'].mean()
print "Average number of people who disagree that shale gas is good for climate:", \
    df_survey['schiste_bon_pour_climat_non'].mean()
print "Average number of people who agree that shale gas is good for climate:", \
    df_survey['schiste_bon_pour_climat_oui'].mean()

df_survey['schiste_avantage_climat'] = 0 + 1 * (df_survey['Q199'].str[:27] == 'Cela permettrait de limiter')
df_survey['schiste_avantage_emploi'] = 0 + 1 * (df_survey['Q199'].str[:22] == u'Cela permettrait de cr')
df_survey['schiste_avantage_aucun'] = 0 + 1 * (df_survey['Q199'].str[:6] == 'Aucune')
print "Average number of people who think the main benefit of shale gas is its positive impact on climate:", \
    df_survey['schiste_avantage_climat'].mean()
print "Average number of people who think the main benefit of shale gas is its positive impact on job creation:", \
    df_survey['schiste_avantage_emploi'].mean()
print "Average number of people who think shale gas benefits neither for climate nor for job creation:", \
    df_survey['schiste_avantage_aucun'].mean()


""" Shale gas - agree vs disagree """

df_schiste_oui = df_survey.query('approuve_schiste == 1')
print "Average number of people who think the main benefit of shale gas is its positive impact on climate among those who agree for extraction:", \
    df_schiste_oui['schiste_avantage_climat'].mean()
print "Average number of people who think the main benefit of shale gas is its positive impact on job creation among those who agree for extraction:", \
    df_schiste_oui['schiste_avantage_emploi'].mean()
print "Average number of people who think shale gas benefits neither for climate nor for job creation among those who agree for extraction:", \
    df_schiste_oui['schiste_avantage_aucun'].mean()
del df_schiste_oui

df_schiste_non = df_survey.query('approuve_schiste == 0')
print "Average number of people who think the main benefit of shale gas is its positive impact on climate among those who disgree for extraction:", \
    df_schiste_non['schiste_avantage_climat'].mean()
print "Average number of people who think the main benefit of shale gas is its positive impact on job creation among those who disagree for extraction:", \
    df_schiste_non['schiste_avantage_emploi'].mean()
print "Average number of people who think shale gas benefits neither for climate nor for job creation among those who disagree for extraction:", \
    df_schiste_non['schiste_avantage_aucun'].mean()
del df_schiste_non


""" Shale gas - treated vs non treated """

df_survey['gas_treated'] = 0 + 1 * (df_survey['gas_treated'] == '1')
df_schiste_treated = df_survey.query('gas_treated == 1')
df_schiste_not_treated = df_survey.query('gas_treated == 0')

print df_schiste_treated['approuve_schiste'].mean()
print df_schiste_not_treated['approuve_schiste'].mean()

print df_schiste_treated['schiste_avantage_emploi'].mean()
print df_schiste_not_treated['schiste_avantage_emploi'].mean()

print df_schiste_treated['schiste_avantage_climat'].mean()
print df_schiste_not_treated['schiste_avantage_climat'].mean()
