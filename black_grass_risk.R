rm(list=ls())

library(tidyverse)
library(readr)
library(data.table)
library(lme4)
library(lmtest)

knitr::purl("butterfly_bee_riskscore.Rmd")
source("butterfly_bee_riskscore.R") 
#file.remove("butterfly_bee_riskscore.R")
# NOTE: Ensure that the variable 'input' is set to 'both' in butterfly_bee_riskscore.Rmd

knitr::purl("mammals_riskscore.Rmd")
source("mammals_riskscore.R")
#file.remove("mammals_riskscore.R")

knitr::purl("arable_plant_riskscore.Rmd")
source("arable_plant_riskscore.R")
#file.remove("arable_plant_riskscore.R")

# load farm management strategy datasets
mamm_man_strats <- read_xlsx("change_BG.xlsx", sheet="Mammals")
plant_man_strats <- read_xlsx("change_BG.xlsx", sheet="Plants")
bee_man_strats <- read_xlsx("change_BG.xlsx", sheet="Bees")
butt_man_strats <- read_xlsx("change_BG.xlsx", sheet="Butterflies")

# replace NAs with 0s in all datasets
mamm_man_strats <- NA_to_zero(mamm_man_strats)
plant_man_strats <- NA_to_zero(plant_man_strats)
bee_man_strats <- NA_to_zero(bee_man_strats)
butt_man_strats <- NA_to_zero(butt_man_strats)

# define strategy names
strategy_names <- c("historical", # historical -> BAU
                 "HDHR-N-h", #  BAU -> High-Density High-Resistance black-grass (BG), North, heavy soil
                 "LDHR-N-h", #  BAU -> Low-Density High-Resistance BG, North, heavy soil
                 "LDLR-N-h") #  BAU -> Low-Density Low-Resistance BG, North, heavy soil

# initialise risk scores dataframe
all_riskscores_df <- data.frame(Species = character(),
                                Taxon = character(),
                                Strategy = character(),
                                Year = integer(),
                                Component = character(),
                                Risk_score = integer())

# define number of rotations (years) per strategy
num_yrs <- 6

##### BROADLEAF ARABLE PLANTS RISK SCORES ######

# for each strategy, calculate risk scores and append to all_riskscores_df
for (strategy in strategy_names) {
  
  # select the strategy's rows from management strategies
  man_strats <- plant_man_strats[plant_man_strats$transition %like% strategy,]
  
  # calculate risk scores, per component per year of the strategy
  plant_risk <- calc_plant_riskscore(plants, man_strats)

  # convert risk scores to long format from wide
  plant_risk_long <- reshape2::melt(plant_risk[,-ncol(plant_risk)], id.vars="species")
  num_rows <- nrow(plant_risk_long)
  
  # set strategy name (i.e. change name of historical strat to 'BAU')
  if (strategy == "historical") {
    strat_name <- "BAU"
  } else {
    strat_name <- strategy
  }
  
  # create dataframe of strategy data ready to add to main risk score dataframe
  plant_data_to_add <- data.frame(
    Species = plant_risk_long$species,
    Taxon = rep("Broadleaf plant", num_rows),
    Strategy = rep(strat_name, num_rows),
    Year = rep(1:num_yrs, rep(num_rows/num_yrs, num_yrs)), 
    Component = plant_risk_long$variable,
    Risk_score = plant_risk_long$value
    )
  
  # add data as rows in main risk score dataframe
  all_riskscores_df <- dplyr::bind_rows(all_riskscores_df, plant_data_to_add)
  
}

##### BUTTERFLY RISK SCORES ######

# for each strategy, calculate risk scores and append to all_riskscores_df
for (strategy in strategy_names) {
  
  # select the strategy's rows from management strategies
  man_strats <- butt_man_strats[butt_man_strats$transition %like% strategy,]
  
  # calculate risk scores, per component per year of the strategy
  butt_risk <- calc_butterfly_riskscore(butterfly, butterfly_plants, butt_plant_locs, man_strats)
  
  # convert risk scores to long format from wide
  butt_risk_long <- reshape2::melt(butt_risk[,-ncol(butt_risk)], id.vars="species")
  num_rows <- nrow(butt_risk_long)
  
  # set strategy name (i.e. change name of historical strat to 'BAU')
  if (strategy == "historical") {
    strat_name <- "BAU"
  } else {
    strat_name <- strategy
  }
  
  # create dataframe of strategy data ready to add to main risk score dataframe
  butt_data_to_add <- data.frame(
    Species = butt_risk_long$species,
    Taxon = rep("Butterfly", num_rows),
    Strategy = rep(strat_name, num_rows),
    Year = rep(1:num_yrs, rep(num_rows/num_yrs, num_yrs)), 
    Component = butt_risk_long$variable,
    Risk_score = butt_risk_long$value
  )
  
  # add data as rows in main risk score dataframe
  all_riskscores_df <- dplyr::bind_rows(all_riskscores_df,butt_data_to_add)
  
}
  
##### BEE RISK SCORES ######

# for each strategy, calculate risk scores and append to all_riskscores_df
for (strategy in strategy_names) {
  
  # select the strategy's rows from management strategies
  man_strats <- bee_man_strats[bee_man_strats$transition %like% strategy,]
  
  # calculate risk scores, per component per year of the strategy
  bee_risk <- calc_bee_riskscore(bee, bee_plants, bee_plant_locs, man_strats)
  
  # convert risk scores to long format from wide
  bee_risk_long <- reshape2::melt(bee_risk[,-ncol(bee_risk)], id.vars="species")
  num_rows <- nrow(bee_risk_long)
  
  # set strategy name (i.e. change name of historical strat to 'BAU')
  if (strategy == "historical") {
    strat_name <- "BAU"
  } else {
    strat_name <- strategy
  }
  
  # create dataframe of strategy data ready to add to main risk score dataframe
  bee_data_to_add <- data.frame(
    Species = bee_risk_long$species,
    Taxon = rep("Bee", num_rows),
    Strategy = rep(strat_name, num_rows),
    Year = rep(1:num_yrs, rep(num_rows/num_yrs, num_yrs)), 
    Component = bee_risk_long$variable,
    Risk_score = bee_risk_long$value
  )
  
  # add data as rows in main risk score dataframe
  all_riskscores_df <- dplyr::bind_rows(all_riskscores_df,bee_data_to_add)
  
}

##### MAMMAL RISK SCORES ######

# for each strategy, calculate risk scores and append to all_riskscores_df
for (strategy in strategy_names) {
  
  # select the strategy's rows from management strategies
  man_strats <- mamm_man_strats[mamm_man_strats$transition %like% strategy,]
  
  # calculate risk scores, per component per year of the strategy
  mamm_risk <- calc_mammal_riskscore(mammals, man_strats)
  
  # convert risk scores to long format from wide
  mamm_risk_long <- reshape2::melt(mamm_risk[,-ncol(mamm_risk)], id.vars="species")
  num_rows <- nrow(mamm_risk_long)
  
  # set strategy name (i.e. change name of historical strat to 'BAU')
  if (strategy == "historical") {
    strat_name <- "BAU"
  } else {
    strat_name <- strategy
  }
  
  # create dataframe of strategy data ready to add to main risk score dataframe
  mamm_data_to_add <- data.frame(
    Species = mamm_risk_long$species,
    Taxon = rep("Mammal", num_rows),
    Strategy = rep(strat_name, num_rows),
    Year = rep(1:num_yrs, rep(num_rows/num_yrs, num_yrs)), 
    Component = mamm_risk_long$variable,
    Risk_score = mamm_risk_long$value
  )
  
  # add data as rows in main risk score dataframe
  all_riskscores_df <- dplyr::bind_rows(all_riskscores_df,mamm_data_to_add)
  
}

## DATA SUMMARY

risk_per_strat <- all_riskscores_df %>%
  group_by(Species, Strategy, Taxon) %>%
  summarise(Risk_score = sum(Risk_score))

## MODEL FIT

# model1 <- glmer(Risk_score ~ Strategy + (1|Species), data=risk_per_strat, family="gaussian")
# summary(model1)
# 
# model2 <- glmer(Risk_score ~ Strategy*Taxon + (1|Species), data=risk_per_strat, family="gaussian")
# summary(model2)
# 
# model3 <- glmer(Risk_score ~ Strategy + Taxon + (1|Species), data=risk_per_strat, family="gaussian")
# summary(model3)

anova(model1, model2)
anova(model2, model3)

model1_l <- lmer(Risk_score ~ Strategy + (1|Species), data=risk_per_strat)
summary(model1_l)

model2_l <- lmer(Risk_score ~ Strategy*Taxon + (1|Species), data=risk_per_strat)
summary(model2_l)

model3_l <- lmer(Risk_score ~ Strategy + Taxon + (1|Species), data=risk_per_strat)
summary(model3_l)

null_model <- lmer(Risk_score ~ 1 + (1|Species), data=risk_per_strat)
summary(null_model)

anova(model1_l, model3_l)
anova(model2_l, model3_l)
anova(null_model, model1)
