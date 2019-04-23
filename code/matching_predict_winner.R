library("StatMatch")
# Import data
df_donor_enl <- read.csv(file = "C:/Users/thoma/Documents/Github/beliefs_climate_policies/df_donor_enl.csv", header = -1, sep=";")
df_receiver_bdf <- read.csv(file = "C:/Users/thoma/Documents/Github/beliefs_climate_policies/df_receiver_bdf.csv", header = -1, sep=";")

# Compute matching:
out.nnd <- NND.hotdeck(
  data.rec = df_receiver_bdf, data.don = df_donor_enl,
  match.vars = c("accommodation_size", "hh_income", "hh_income_2"),# "consumption_units", "age_18_24", "age_25_34", "age_35_49", "age_50_64"),
  don.class = c("natural_gas", "domestic_fuel"), # TODO: Should it be here or in the match.vars? Necessary to reduce memory needs in computations
  dist.fun = "Gower"
)

# Create fused file
fused.nnd.m <- create.fused(
  data.rec = df_receiver_bdf, data.don = df_donor_enl,
  mtc.ids = out.nnd$mtc.ids,
  z.vars = c("housing_expenditures_increase_to_impute")
)

fused.nnd.m$predicted_winner_matching <- 1 * (fused.nnd.m$transport_expenditures_increase + fused.nnd.m$housing_expenditures_increase_to_impute < 110 * fused.nnd.m$nb_beneficiaries)
1 - sum(fused.nnd.m$winner * fused.nnd.m$predicted_winner_matching) / sum(fused.nnd.m$winner)
1 - sum((fused.nnd.m$winner == 0) * (fused.nnd.m$predicted_winner_matching == 0)) / sum((fused.nnd.m$winner == 0))
mean(fused.nnd.m$winner != fused.nnd.m$predicted_winner_matching)
# With accommodation size only : 18.6%
# With accommodation size and income (squared) : 18.0%
# With many more variables : 17.7%