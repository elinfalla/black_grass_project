## Data

### Description
This directory contains all input files required for Butler's risk index, as well as Butler's risk score validation files and information about the black-grass strategies


### File descriptions

#### Species' resource requirements
- **Arable_plant_requirements.xlsx** - data on resource requirements for survival of broadleaf arable plants per species, as defined by Butler et al. (2009), including germination and flowering periods, and habitat, nitrogen and moisture preferences. Entered as binary data (with the exception of moisture and nitrogen Ellenberg scores).

- **Butterfly_requirements.xlsx** - data on resource requirements for survival of butterflies per species, as defined by Butler et al. (2009), including species' habitats, activity periods and generations per year. Entered as binary data.

- **Pollinator_requirements.xlsx** - data on resource requirements for survival of bees per species, as defined by Butler et al. (2009), including species' habitats, activity periods, generations per year and nesting habitats. Entered as binary data.

- **Mammal_requirements.xlsx** - data on resource requirements for survival of mammals per species, as defined by Butler et al. (2009), including species' habitats, diet/feeding preferences and nesting habitats. Entered as binary data.

#### Farm management changes
- **change.xlsx** - data on how the farm management changes outlined by Butler et al. (2009) affect the species' resources. Entered as binary data. Used to validate my code, to check I get the same risk scores as Butler.

- **change_BG.xlsx** - data on how the farm management changes created by the black-grass strategies (and BAU strategy) affect the species' resources. Entered as binary data. Used to calculate risk scores associated with changing management to the black-grass strategies.

#### Forage plants

- **butterfly_forage_plants.xlsx** - data on the forage plant families of each butterfly species, for both the adult and larval stage. Entered as binary data.

- **forage_plant_location.xlsx** - data on the locations of the forage plant families of the bee and butterfly species. Entered as binary data.

#### Misc.

- **butler_plants_risk.xlsx** - data containing the broadleaf arable plant risk scores as calculated by Butler et al. (2009). Used for validation.


### Directory descriptions

- **Butler_validation** - contains Butler's original files where he calculated the risk scores for each taxon as well as supplementary information from the publication (Butler et al. 2009). Used for validation of my risk score scripts, to ensure I had the same scores.

- **black_grass_info** - contains 3 files containing detailed information on the farm management strategies used in the analysis. These files were what I used to decide the elements of risk of the strategies for each taxa.
  - **BAU_InputData.xlsx** - data on the business-as-usual (BAU) strategy, which was used as a baseline to compare the black-grass strategies to.
  - **Farmer Focus strategies_01-03-2021.xlsx** - data on all the black-grass management strategies created by the BGRI farmer focus group, including the subset of strategies I focussed on for my analysis.
  - **initcondit.pptx** - Graphic showing the breakdown of how the black-grass stategies were organised per region, soil type and density-resistance level.
