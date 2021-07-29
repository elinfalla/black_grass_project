rm(list=ls())

library(tidyverse)
library(readr)
library(data.table)
library(lme4)
library(lmtest)

knitr::purl("butterfly_bee_riskscore.Rmd")
source("butterfly_bee_riskscore.R") 
file.remove("butterfly_bee_riskscore.R")
# NOTE: Ensure that the variable 'input' is set to 'both' in butterfly_bee_riskscore.Rmd

knitr::purl("mammals_riskscore.Rmd")
source("mammals_riskscore.R")
file.remove("mammals_riskscore.R")

knitr::purl("arable_plant_riskscore.Rmd")
source("arable_plant_riskscore.R")
file.remove("arable_plant_riskscore.R")

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
                                Component_transition = character(),
                                Risk_score = numeric())

# define number of rotations (years) per strategy and risk component names
num_yrs <- 6
risk_components <- c("crop", 
                     "sowing", 
                     "tillage", 
                     "glyphosate", 
                     "BG herbicide", 
                     "pesticide", 
                     "fertiliser")

# define number of resource requirements (overlap parameters) per taxon
num_mamm_reqs <- 4
num_bee_reqs <- 3
num_butt_reqs <- 4
num_plant_reqs <- 4

##### BROADLEAF ARABLE PLANTS RISK SCORES ######

# for each strategy, calculate risk scores and append to all_riskscores_df
for (strategy in strategy_names) {
  
  # select the strategy's rows from management strategies
  man_strats <- plant_man_strats[plant_man_strats$transition %like% strategy,]
  
  # calculate risk scores, per component per year of the strategy
  plant_risk <- calc_plant_riskscore(plants, man_strats)
  num_species <- nrow(plant_risk)

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
    Num_reqs = rep(num_plant_reqs, num_rows),
    Strategy = rep(strat_name, num_rows),
    Year = rep(1:num_yrs, each=num_rows/num_yrs), 
    Component = rep(rep(risk_components, each=num_species), num_yrs),
    Component_transition = plant_risk_long$variable,
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
  num_species <- nrow(butt_risk)
  
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
    Num_reqs = rep(num_butt_reqs, num_rows),
    Strategy = rep(strat_name, num_rows),
    Year = rep(1:num_yrs, each=num_rows/num_yrs), 
    Component = rep(rep(risk_components, each=num_species), num_yrs),
    Component_transition = butt_risk_long$variable,
    Risk_score = butt_risk_long$value
  )
  
  # add data as rows in main risk score dataframe
  all_riskscores_df <- dplyr::bind_rows(all_riskscores_df, butt_data_to_add)
  
}
  
##### BEE RISK SCORES ######

# for each strategy, calculate risk scores and append to all_riskscores_df
for (strategy in strategy_names) {
  
  # select the strategy's rows from management strategies
  man_strats <- bee_man_strats[bee_man_strats$transition %like% strategy,]
  
  # calculate risk scores, per component per year of the strategy
  bee_risk <- calc_bee_riskscore(bee, bee_plants, bee_plant_locs, man_strats)
  num_species <- nrow(bee_risk)
  
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
    Num_reqs = rep(num_bee_reqs, num_rows),
    Strategy = rep(strat_name, num_rows),
    Year = rep(1:num_yrs, each=num_rows/num_yrs), 
    Component = rep(rep(risk_components, each=num_species), num_yrs),
    Component_transition = bee_risk_long$variable,
    Risk_score = bee_risk_long$value
  )
  
  # add data as rows in main risk score dataframe
  all_riskscores_df <- dplyr::bind_rows(all_riskscores_df, bee_data_to_add)
  
}

##### MAMMAL RISK SCORES ######

# for each strategy, calculate risk scores and append to all_riskscores_df
for (strategy in strategy_names) {
  
  # select the strategy's rows from management strategies
  man_strats <- mamm_man_strats[mamm_man_strats$transition %like% strategy,]
  
  # calculate risk scores, per component per year of the strategy
  mamm_risk <- calc_mammal_riskscore(mammals, man_strats)
  num_species <- nrow(mamm_risk)
  
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
    Num_reqs = rep(num_mamm_reqs, num_rows),
    Strategy = rep(strat_name, num_rows),
    Year = rep(1:num_yrs, each=num_rows/num_yrs), 
    Component = rep(rep(risk_components, each=num_species), num_yrs),
    Component_transition = mamm_risk_long$variable,
    Risk_score = mamm_risk_long$value
  )
  
  # add data as rows in main risk score dataframe
  all_riskscores_df <- dplyr::bind_rows(all_riskscores_df, mamm_data_to_add)
  
}

### FUNCTION TO CALCULATE RELATIVE RISK SCORES
# given the dataframe, name of the risk score column, and number of BG strategies
get_relative_risk <- function(df, risk_col, num_BG_strats) {
  
  BAU <- df %>%
    filter(Strategy == "BAU")
  
  rep_BAU <- BAU %>%
    slice(rep(1:n(), num_BG_strats)) # repeat BAU rows for the number of other strats there are

  BG_strats <- df %>%
    filter(!(Strategy == "BAU"))

  relative_risk <- BG_strats %>% pull(risk_col) - rep_BAU %>% pull(risk_col)
  
  df$Relative_risk <- c(BAU %>% pull(risk_col), relative_risk)
  
  return(df)
  
}

## FUNCTION TO EXTRACT RISK SCORES AND SE FROM LMER OUTPUT
# num_levels refers to the number of levels in the non-taxon categorical variable

extract_lmer_means <- function(model, num_levels) {
  
  fix_eff <- fixef(model)
  reference1 <- fix_eff[[1]]
  reference2 <- fix_eff[[1 + num_levels]]
  reference3 <- fix_eff[[2 + num_levels]]
  reference4 <- fix_eff[[3 + num_levels]]
  
  j <- 0
  for (i in 1:length(fix_eff)) {
    if (i == 1) {
      means <- reference1
    } 
    else if (i > 1 & i <= num_levels) { # use reference 1
      means <- c(means, fix_eff[[i]] + reference1)
    }
    else if (i == (num_levels + 1)) { # add in other references
      means <- c(means, reference2, reference3, reference4)
    }
    else if (i > (num_levels + 3)) { # is interactions
      j <- j + 1
      
      if (j > 0 & j < num_levels) {
        means <- c(means, fix_eff[[i]] + reference2)
      }
      else if (j >= num_levels & j < num_levels*2 - 1) {
        means <- c(means, fix_eff[[i]] + reference3)
      }
      else {
        means <- c(means, fix_eff[[i]] + reference4)
      }
    }
  }
  std_err <- sqrt(diag(vcov(model, useScale = FALSE)))
  
  return(data.frame(means, std_err))
}


## DATA SUMMARIES

risk_per_strat <- all_riskscores_df %>%
  group_by(Species, Strategy, Taxon, Num_reqs) %>%
  summarise(Risk_score = sum(Risk_score))

risk_per_year <- all_riskscores_df %>%
  group_by(Species, Strategy, Taxon, Year) %>%
  summarise(Risk_score = sum(Risk_score))

risk_per_component <- all_riskscores_df %>%
  group_by(Strategy, Species, Taxon, Component, Year) %>%
  summarise(Risk_score = sum(Risk_score))

risk_per_component <- get_relative_risk(risk_per_component, "Risk_score", 3)

## MODEL FIT

### LOG RISK SCORES
risk_per_strat[,"Risk_score"] <- risk_per_strat[,"Risk_score"] + 0.00000001
risk_per_strat[,"Risk_score"] <- log(risk_per_strat[,"Risk_score"])

# risk_per_year[,"Risk_score"] <- risk_per_year[,"Risk_score"] + 0.00000001
# risk_per_year[,"Risk_score"] <- log(risk_per_year[,"Risk_score"])

#risk_per_component[,"Relative_risk"] <- risk_per_component[,"Relative_risk"] + 0.00000001
#risk_per_component[,"Relative_risk"] <- log(risk_per_component[,"Relative_risk"])

#### taxon effects
null_model <- lmer(Risk_score ~ 1 + (1|Species), data=risk_per_strat)

model_simple <- lmer(Risk_score ~ Strategy + (1|Species), data=risk_per_strat)

model_taxon <- lmer(Risk_score ~ Strategy + Taxon + (1|Species), data=risk_per_strat)

model_tax_int <- lmer(Risk_score ~ Strategy*Taxon + (1|Species), data=risk_per_strat)


# taxon effects ANOVA
anova(null_model, model_simple) # *** signif
anova(model_simple, model_taxon) # *** signif
anova(model_taxon, model_taxon_int) # *** signif

#### year effects

# # separate year risks into strategies and analyse separately
# risk_per_yr_HDHR <- risk_per_year[risk_per_year$Strategy == "HDHR-N-h",]
# risk_per_yr_LDHR <- risk_per_year[risk_per_year$Strategy == "LDHR-N-h",]
# risk_per_yr_LDLR <- risk_per_year[risk_per_year$Strategy == "LDLR-N-h",]
# 
# model_yr_null <- lmer(Risk_score ~ 1 + (1|Species), data=risk_per_yr_HDHR)
# 
# model_yr_simple <- lmer(Risk_score ~ as.factor(Year) + (1|Species), data=risk_per_yr_HDHR)
# 
# model_yr_strat <- lmer(Risk_score ~ Taxon + as.factor(Year) + (1|Species), data=risk_per_yr_HDHR)
# 
# model_yr_int <- lmer(Risk_score ~ Taxon*as.factor(Year) + (1|Species), data=risk_per_yr_HDHR)
# summary(model_yr_int)

# # year effects anova / likelihood ratio test
# anova(model_yr_null, model_yr_simple) # *** signif
# anova(model_yr_simple, model_yr_strat) # *** signif
# anova(model_yr_strat, model_yr_int) # *** signif


## component effects
model_comp_null <- lmer(Relative_risk ~ 1 + (1|Species), data=risk_per_component[risk_per_component$Strategy != "BAU",])
summary(model_comp_null)

model_comp <- lmer(Relative_risk ~ Component + (1|Species), data=risk_per_component[risk_per_component$Strategy != "BAU",])
summary(model_comp)

model_comp_tax <- lmer(Relative_risk ~ Component + Taxon + (1|Species), data=risk_per_component[risk_per_component$Strategy != "BAU",])

model_comp_tax_int <- lmer(Relative_risk ~ Component*Taxon + (1|Species), data=risk_per_component[risk_per_component$Strategy != "BAU",])

model_comp_HDHR <- lmer(Relative_risk ~ Component*Taxon + (1|Species), data=risk_per_component[risk_per_component$Strategy == "HDHR-N-h",])
model_comp_LDHR <- lmer(Relative_risk ~ Component*Taxon + (1|Species), data=risk_per_component[risk_per_component$Strategy == "LDHR-N-h",])
model_comp_LDLR <- lmer(Relative_risk ~ Component*Taxon + (1|Species), data=risk_per_component[risk_per_component$Strategy == "LDLR-N-h",])

####  PLOTTING

### strat*taxon plot

model_tax_int_df <- extract_lmer_means(model_tax_int, 4) # 4 is num levels in 'strategy' fixed effect
names(model_tax_int_df) <- c("Mean_risk", "Std_error")

model_tax_int_df$Strategy <- c("BAU", "HDHR", "LDHR", "LDLR", rep("BAU", 3), rep (c("HDHR", "LDHR", "LDLR"), 3))
model_tax_int_df$Taxon <- c(rep("Bees", 4), "Broadleaf plants", "Butterflies", "Mammals", rep(c("Broadleaf plants", "Butterflies", "Mammals"), each=3))
model_tax_int_df$Estimate <- as.vector(fixef(model_tax_int))

# order dataframe alphabetically by strategysu
#model_tax_int_df <- model_tax_int_df[order(model_tax_int_df$Strategy),]

# add in relative risk
#model_tax_int_df <- get_relative_risk(model_tax_int_df, "Mean_risk", 3)

## plot model
p <- ggplot(data = model_tax_int_df %>% filter(!(Strategy == "BAU")), 
            aes(x = Strategy, y = Estimate, ymin = (Estimate - Std_error), ymax = (Estimate + Std_error))) +
  geom_pointrange() +
  ylab("Estimate") +
  coord_flip() +
  facet_wrap(~Taxon) +
  geom_hline(yintercept = 0, lty=2)

p

### COMPONENTS PLOT

# extract risk scores + SE
components_df <- extract_lmer_means(model_comp_tax_int, 7) # 7 is num levels in 'component' fixed effect
names(components_df) <- c("Rel_risk", "Std_error")

# add taxon and component columns
components_df$Taxon <- c(rep("Bees", 7), "Broadleaf plants", "Butterflies", "Mammals", rep(c("Broadleaf plants", "Butterflies", "Mammals"), each=6))

components <- c("BG herbicide", "Crop", "Fertiliser", "Glyphosate", "Pesticide", "Sowing", "Tillage")
components_df$Component <- c(components, rep(components[1], 3), rep(components[-1], 3))
  
# plot components * taxon

comp_plot <- ggplot(data = components_df, 
            aes(x = Component, 
                y = Rel_risk, 
                ymin = (Rel_risk - Std_error), 
                ymax = (Rel_risk + Std_error))) +
  geom_pointrange() +
  ylab("Mean relative risk score") +
  coord_flip() +
  facet_wrap(~Taxon) +
  geom_hline(yintercept = 0, lty=2) +
  ylim(-0.3,0.3)

comp_plot

# plot individually for each BG strategy

### HDHR
components_df_HDHR <- extract_lmer_means(model_comp_HDHR, 7) # 7 is num levels in 'component' fixed effect
names(components_df_HDHR) <- c("Rel_risk", "Std_error")

components_df_HDHR$Taxon <- components_df$Taxon
components_df_HDHR$Component <- components_df$Component

comp_plot_HDHR <- ggplot(data = components_df_HDHR, 
                    aes(x = Component, 
                        y = Rel_risk, 
                        ymin = (Rel_risk - Std_error), 
                        ymax = (Rel_risk + Std_error))) +
  geom_pointrange() +
  ylab("Mean relative risk score HDHR") +
  coord_flip() +
  facet_wrap(~Taxon) +
  geom_hline(yintercept = 0, lty=2) +
  ylim(-0.3, 0.3)

comp_plot_HDHR


### LDHR
components_df_LDHR <- extract_lmer_means(model_comp_LDHR, 7) # 7 is num levels in 'component' fixed effect
names(components_df_LDHR) <- c("Rel_risk", "Std_error")

components_df_LDHR$Taxon <- components_df$Taxon
components_df_LDHR$Component <- components_df$Component

comp_plot_LDHR <- ggplot(data = components_df_LDHR, 
                         aes(x = Component, 
                             y = Rel_risk, 
                             ymin = (Rel_risk - Std_error), 
                             ymax = (Rel_risk + Std_error))) +
  geom_pointrange() +
  ylab("Mean relative risk score LDHR") +
  coord_flip() +
  facet_wrap(~Taxon) +
  geom_hline(yintercept = 0, lty=2) + 
  ylim(-0.3, 0.3)

comp_plot_LDHR

### LDLR
components_df_LDLR <- extract_lmer_means(model_comp_LDLR, 7) # 7 is num levels in 'component' fixed effect
names(components_df_LDLR) <- c("Rel_risk", "Std_error")

components_df_LDLR$Taxon <- components_df$Taxon
components_df_LDLR$Component <- components_df$Component

comp_plot_LDLR <- ggplot(data = components_df_LDLR, 
                         aes(x = Component, 
                             y = Rel_risk, 
                             ymin = (Rel_risk - Std_error), 
                             ymax = (Rel_risk + Std_error))) +
  geom_pointrange() +
  ylab("Mean relative risk score LDLR") +
  coord_flip() +
  facet_wrap(~Taxon) +
  geom_hline(yintercept = 0, lty=2) +
  ylim(-0.3, 0.3)

comp_plot_LDLR




