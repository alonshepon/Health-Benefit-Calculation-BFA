# Health impacts of increased aquatic foods production

## Overview

The is the GitHub repository for the health impacts analysis for the following paper:

* Golden CD*, Koehn JZ*, Shepon A*, Passarelli S*, Free CM*, Viana DF*, Matthey H, Eurich JG, Gephart JA, Fluet-Chouinnard E, Nyboer EA, Lynch AJ, Kjellevold M, Bromage S, Charlebois P, Barange M, Vannuccini S, Cao L, Kleisner KM, Rimm EB, Danaei G, DeSisto C, Kelahan H, Fiorella KJ, Little DC, Allison EH, Fanzo J, Thilsted SH (2021) **Aquatic foods to nourish nations.** Nature: https://doi.org/10.1038/s41586-021-03917-1. * Contributed equally

Please contact Chris Golden (golden@hsph.harvard.edu) with questions about the paper and Chris Free (cfree14@gmail.com) or Alon Shepon (alonshepon@tauex.tau.ac.il) with questions about the repository.

## Repository structure

The repository is organized using the following structure:

* data.............. folder containing the data to support the health impacts analysis
* code............. folder containing code to fit intake distributions and calculate inadequate intakes (SEVs)
* output........... folder containing the output of the health impacts analysis
* figures.......... folder containing figures for the manuscript
* tables........... folder containing tables for the manuscript

The following folder of data is not posted due to its large size: "data/intakes/processed/". Email Chris Free (cfree14@gmail.com) if you require access.

## Other resources

The other GitHub repositories in support of this paper are availble here:

* Aquatic foods nutrient database: https://github.com/zachkoehn/aquatic_foods_nutrient_database
* Diversity disaggregation: https://github.com/cg0lden/Fisheries-Nutrition-Modeling
* Subnational distribution data: https://github.com/cg0lden/subnational_distributions_BFA
* SPADE analysis: https://github.com/cg0lden/subnational_distributions_BFA

## Methods overview

Calculation of Burden of disease for changes in fish consumption
The GBD project is the most comprehensive methodology to assess health burden across countries, age and sex groups and various forms of disease and risks. The GBD project has calculated the burden of malnutrition for vitamin A, zinc, iron (and other nutrient deficiencies) and low seafood consumption (PUFA->heart disease) (Murray 2020, Afshin 2019 and James 2018) - four risk factors associated with fish. While not comprehensive of all outcomes associated with consuming fish (in the health literature) e.g. omega n-3 impact on child development), it seems to include both micronutrient contribution and low omega n-3 derived from fish. Using the DALYs metrics, our results can be compared to other GBD global metrics results, enabling us to assess the magnitude of health burdens associated with reduced or increased fish consumption in future alternatives. Last, because fish is an important animal sourced food, increased consumption of fish is likely to result in changes in red meat consumption. Red meat consumption constitutes an important health burden, which can also be assessed in order to capture the full burdens of the above analysis

calculating fish-associated burden of disease (DALYs) in 2030 for omega n-3
Build GBD dataset. As a first step, we coalesced the GBD historical data for all countries per age-sex groups and for the specific risks and causes associated with fish consumption that exist in the GBD database, namely low omega n-3 consumption.

Extract and upload population data from present till 2030 per age-sex location group.

Using that historical data we extrapolated the burden of disease in year 2030 for low seafood consumption (hereafter DALY2030) for each age-sex-location using a moving regression. The following code also plots the results per country (population adjusted).

Deriving average micronutrient intakes from the Aglink Cosimo: This FAO model will simulate consumption of fish in future scenarios based on price elasticities, supply and demand curves and will output average nutrient intakes for reference and alternative futures per age-sex-location group.

Deriving national level distribution of micronutrients from SPADE: This stage will be done using the SPADE software (https://www.rivm.nl/en/spade) which analyzes existing HCES data and translates 24h recall into average distributions. Currently we will have estimates for a handful of countries, in all continents, from which we will infer on the distributions for all countries.

Calulate relative risk curves for omega n-3. RR curves For omega n-3 are derived from the new GBD 2020 spline curves (As a comparison, the Thomsen paper used the Mozaffarian and Rimm 2006 curve and simplified it to a descending linear curve, until a consumption level of 250 mg/cap/d EPA+DHA).

Deriving age-sex-location changes in DALYs of seafood omega n-3 burdens due to perturbations in 2030. We compute the changes in DALYs (changes in health burden) per age-sex-location for omega n-3 as response to moving from the reference scenario (BAU) to the alternative-perturbed scenario (alt) in year 2030 using the following equation:
ΔDALYa,s,l,r=DALY2030a,s,l,r∗(SEValta,s,l,r/SEVBAUa,s,l,r−1)

Where DALY2030,a,s,l,r is the DALY per age (a), sex (s), location (l) and risk (r) value for year 2030 derived above, and SEValta,s,l,r and SEVrefa,s,l,r are the population level average weighted exposure to risk for the alternative (“perturbed”) and reference (“baseline”) scenarios, respectively. SEVs are summary exposure values and they reflect excess average weighted prevelance of exposure (or inadequancy in this case). A SEV of 0 means that the population is not exposed to any risk, while a value of 1 means the entire population is exposed to a risk. This value is equal to:
SEVBAU/alta,s,l,r=∫(IBAU/alta,s,l,r∗RRa,s,l,r)

Usually SEV is derived by dividing the above term with the maximal risk Rmax; however in our calculation the maximal risk is 1 (no consumption). As consumption increases, RR effectively decreases.

calculating fish-associated changes in risk (SEVs) associated with micronutrients intake changes for year 2030
For the micronutrient calculation, we calculated RR distributions by taking country level EAR values and constructing a descending risk curve based on the CDF (cumulative distribution function) of a normal distribution. IOM suggest using a CV of 10%-15% when there is no sufficient data on the requirements. As for intakes, these are derived by taking the average full-diets micronutrients from the FAO model (for both scenarios) and building a distributions around them based on the shape deduced from SPADE; using Monte Carlo we can then assess how changes in the tail of this distribution affect the overall calcs.

Multiplying calculated intake distribution with the above micronutrient RR curves and integrating will result in SEV per age-sex-location-micronutrient, which is actually the prevelance of micronutrient deficiency in that group. This method of calculating the prevalence of inadequacy is basically the probability method for calculating deficiency on a population-level. As the above reference indicates, as long as the requirement distribution is symmetrical (not for iron) the results are insensitive to the shape and SD of the requirement curve. The following equation details the calculation of SEV for micronutrient intakes for the baseline (BAU) and alternative (alt) scenario

SEVBAU/alta,s,l,m=∫(IBAU/alta,s,l,m∗RRa,s,l,m)

Overall risk of micronutrient deficiencies resulting from the perturbation in fish consumption can be either the average of all risks (micronutrients, marked as r) examined for each country per age(a)-sex(s)-location(l) groups or the maximum risk:
SEVa,s,l=average(SEVa,s,l,m).
or
SEVBAU/alta,s,l=max(SEVBAU/alta,s,l,r).

Therefore the change (in percentage) in micronutrient deficiency prevalence for each sex-age-location-micronutient group following a perturbation (increase/decrease consumption of fish) will be

ΔSEVc,a,s,r=(SEValtc,a,s,r/SEVrefc,a,s,r−1)∗100
