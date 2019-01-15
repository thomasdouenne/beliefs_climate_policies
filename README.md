# beliefs_climate_policies
This is a research project on beliefs regarding climate change and climate policies, in particular with respect to their distributive effects


# To do:
    # Change color graphs
    # Update inflators for BdF using 2017 values
    # Change income variable to "revtot" for BdF
    # Account for fixed-part of contracts for natural gas
    # Improve matching quality for feedbacks
    # Do simulations over regressivness of VAT
    # Test other methods for feedback (probit, linear regression, distance matching, etc.)


# How to use these files?
- This repository is divided into two parts: one that requires data, one that does not.

- The one that does not - called "model_reforms" - first defines several objects that are then used to compute the incidence
of a policy on an household purchaisng power (how much expenditures increase?), and the amount paid in taxes.
To obtain these numbers, one simply needs to enter some parameters and say how much this household spend in energies.

- The other - called "model_reforms_data" - applies the same formulas to actual households taken from surveys.
Currently the survey Budget de Famille 2011 (BdF 2011) is used. In addition to household level tax incidence, this is also
useful to compute the total revenue from the policy.

- On top of these two parts, the script "computations_feedback" can be used to estimate how much an household,
given some basic information relative to his energy consumption, should be impacted by the policy. The data from
"model_reforms_data" are used to match any household to a set of households from BdF 2011. The average expenditures
of these households is then taken and applied to the formulas provided by "model_reforms" to compute the tax incidence
on the household to whom whe want to give feedbacks.
