## Code

### Description
This directory contains all code for translating Butler's risk index into R, calculating risk of the black-grass strategies, and analysis and plotting.

### File descriptions

- **arable_plant_riskscore.Rmd** - this file calculates risk scores for *arable broadleaf plant* species according to Butler's risk index. It's an R markdown file containing a detailed walk-through with explanations of Butler's method. The function to calculate the risk scores is `calc_plant_riskscore()`, or `calc_std_plant_riskscore()` for standardised risk scores. See script for more details.

- **butterfly_bee_riskscore.Rmd** - this file calculates risk scores for *butterfly* and *bee* species according to Butler's risk index. It's an R markdown file containing a detailed walk-through with explanations of Butler's method. The functions to calculate the risk scores are `calc_bee_riskscore()` and `calc_std_bee_riskscore()` for bees and `calc_butterfly_riskscore()` and `calc_std_butterfly_riskscore()` for butterflies. See script for more details.

- **mammals_riskscore.Rmd** - this file calculates risk scores for *mammal* species according to Butler's risk index. It's an R markdown file containing a detailed walk-through with explanations of Butler's method. The function to calculate the risk scores is `calc_mammal_riskscore()`, or `calc_std_mammal_riskscore()` for standardised risk scores. See script for more details.
black_grass_risk.R          

- **riskscore_functions.R** - this file contains some generic functions necessary for all taxa risk score calculations

- **black_grass_risk.R** - this file sources all the individual taxon risk score scripts and calculates risk scores for the black-grass management strategies. It then analyses the results and creates plots.
