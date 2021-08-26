rm(list=ls())

library(tidyverse)
library(readr)
library(data.table)
library(lme4)
library(lmtest)
library(colorspace)
library(readxl) # for reading in Excel sheets
library(scales)
library(gridExtra)
library(emmeans) #estimated marginal means - post-hoc analysis
library(RColorBrewer)

### SET WORKING DIRECTORY TO CODE FOLDER BEFORE RUNNING

source("riskscore_functions.R") # functions needed to calculate scores

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
mamm_man_strats <- read_xlsx("../data/change_BG.xlsx", sheet="Mammals")
plant_man_strats <- read_xlsx("../data/change_BG.xlsx", sheet="Plants")
bee_man_strats <- read_xlsx("../data/change_BG.xlsx", sheet="Bees")
butt_man_strats <- read_xlsx("../data/change_BG.xlsx", sheet="Butterflies")

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

write.csv(all_riskscores_df, "full_bg_risk_output.csv", row.names = F)
#all_riskscores_df <- read_csv("full_bg_risk_output.csv")

### FUNCTION TO CALCULATE RELATIVE RISK SCORES
# given the dataframe, name of the risk score column, and number of BG strategies
get_relative_risk <- function(df, risk_col, num_BG_strats) {
  
  BAU <- df %>%
    filter(Strategy == "BAU")
  
  # repeat BAU rows for the number of other strats there are
  rep_BAU <- do.call("rbind", replicate(num_BG_strats, BAU, simplify = FALSE)) 

  BG_strats <- df %>%
    filter(!(Strategy == "BAU"))

  # relative risk is the black-grass risk - BAU risk
  relative_BG_risk <- (BG_strats %>% pull(risk_col)) - (rep_BAU %>% pull(risk_col))
  
  relative_risk_w_BAU <- c(BAU %>% pull(risk_col), relative_BG_risk)
  
  return(relative_risk_w_BAU)
  
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


## 

risk_per_strat <- all_riskscores_df %>%
  group_by(Species, Strategy, Taxon, Num_reqs) %>%
  summarise(Risk_score = sum(Risk_score))




## MODEL FIT

### log risk scores
risk_per_strat[,"Risk_score"] <- risk_per_strat[,"Risk_score"] + 0.00000001
risk_per_strat[,"Risk_score"] <- log(risk_per_strat[,"Risk_score"])

#### forward step-wise selection
null_model <- lmer(Risk_score ~ 1 + (1|Species), data=risk_per_strat)

model_simple <- lmer(Risk_score ~ Strategy + (1|Species), data=risk_per_strat)

model_taxon <- lmer(Risk_score ~ Strategy + Taxon + (1|Species), data=risk_per_strat)

model_tax_int <- lmer(Risk_score ~ Strategy*Taxon + (1|Species), data=risk_per_strat)

# ANOVA
anova(null_model, model_simple) # *** signif
anova(model_simple, model_taxon) # *** signif
anova(model_taxon, model_tax_int) # *** signif

# post-hoc tests
model_emm <- emmeans(model_tax_int, ~ Strategy|Taxon)
model_con <- contrast(model_emm, method = "trt.vs.ctrl", by = "Taxon")

### rerun models with different reference levels for figure
# butterfly
risk_per_strat_butt <- within(risk_per_strat, Taxon <- relevel(factor(Taxon), ref = "Butterfly"))
model_tax_int_butt <- lmer(Risk_score ~ Strategy*Taxon + (1|Species), data=risk_per_strat_butt)

# plants
risk_per_strat_plant <- within(risk_per_strat, Taxon <- relevel(factor(Taxon), ref = "Broadleaf plant"))
model_tax_int_plant <- lmer(Risk_score ~ Strategy*Taxon + (1|Species), data=risk_per_strat_plant)  

# mammals
risk_per_strat_mamm <- within(risk_per_strat, Taxon <- relevel(factor(Taxon), ref = "Mammal"))
model_tax_int_mamm <- lmer(Risk_score ~ Strategy*Taxon + (1|Species), data=risk_per_strat_mamm)  


### prepare data to plot
extract_estimates <- function(model) {
  HDHR_est <- fixef(model)[[2]]
  LDHR_est <-fixef(model)[[3]]
  LDLR_est <- fixef(model)[[4]]
  
  estimates <- c(HDHR_est, LDHR_est, LDLR_est)
  std_err <- sqrt(diag(vcov(model, useScale = FALSE)))[2:4]
  CI <- std_err * 1.96
  
  return(data.frame(Strategy = c("HDHR", "LDHR", "LDLR"),
                    Estimate = estimates,
                    Std_error = std_err,
                    CI = CI))
  
}

bee_estimates <- extract_estimates(model_tax_int)
bee_estimates$Taxon <- rep("Bee", 3)

butt_estimates <- extract_estimates(model_tax_int_butt)
butt_estimates$Taxon <- rep("Butterfly", 3)

plant_estimates <- extract_estimates(model_tax_int_plant)
plant_estimates$Taxon <- rep("Broadleaf plant", 3)

mamm_estimates <-  extract_estimates(model_tax_int_mamm)
mamm_estimates$Taxon <- rep("Mammal", 3)

all_model_estimates <- bind_rows(bee_estimates,
                                 butt_estimates,
                                 plant_estimates,
                                 mamm_estimates)

# set colour of each point. red = riskier than BAU, green = less risk, black = NS
all_model_estimates$Colour <- c(rep("green3", 7), "black", "red", rep("green3", 3))

## plot model
tax_levels <- c("Bee", "Butterfly", "Broadleaf plant", "Mammal")

# make dataframe for annotations
annotations <- data.frame(Taxon = rep(tax_levels, each = 3),
                          label = c("t = -10.5", #HDHR bee
                                    "t = -7.50", #LDHR bee
                                    "t = -3.37", #LDLR bee
                                    "t = -7.09", #HDHR butt
                                    "t = -4.40", #LDHR butt
                                    "t = -2.02", #LDLR butt
                                    "t = -17.2", #HDHR plant
                                    "t = 1.61", #LDHR plant
                                    "t = 4.74", #LDLR plant
                                    "t = -24.0", #HDHR mamm
                                    "t = -6.35", #LDHR mamm
                                    "t = -4.49" #LDLR mamm
                                    ),
                          y = all_model_estimates$Estimate,
                          x = rep(c(1,2,3), times=4))

pdf("../../write_up/Figures/risk_comparison.pdf"#, width=9, height=7
    )

total_risk_plot <- 
  ggplot(data = all_model_estimates, 
       aes(x = Strategy, y = Estimate, ymin = Estimate - (CI), ymax = Estimate + (CI))) +
  geom_hline(yintercept = 0, lty=2, colour = "grey40") +
  geom_pointrange(colour = all_model_estimates$Colour) +
  labs(y = "\nModel-estimated difference in risk (log) from BAU",
       x = "Black-grass management strategy\n") +
  scale_x_discrete(labels = c("HD-HR", "LD-HR", "LD-LR")) +
  scale_y_continuous(limits = c(-0.155, 0.05)) +
  coord_flip() +
  facet_wrap(~factor(Taxon, levels = tax_levels)) +

  geom_label(aes(label=label, x=x, y=y), data = annotations, inherit.aes = F, 
             nudge_x = 0.2, size = 3.9, label.size = NA) +
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold", size = 14),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12))

total_risk_plot

dev.off()


### STANDARDISE RISK SCORES for other analyses ----

# standardise plant risk scores
all_plant_riskscores_df <- all_riskscores_df %>%
  filter(Taxon == "Broadleaf plant")

all_std_plant_riskscores <- calc_std_plant_riskscore(all_plant_riskscores_df,
                                                     risk_col = "Risk_score")

# standardise bee risk scores
all_bee_riskscores_df <- all_riskscores_df %>%
  filter(Taxon == "Bee")

all_std_bee_riskscores <- calc_std_bee_riskscore(all_bee_riskscores_df,
                                                     risk_col = "Risk_score")

# standardise butterfly risk scores
all_butt_riskscores_df <- all_riskscores_df %>%
  filter(Taxon == "Butterfly")

all_std_butt_riskscores <- calc_std_butterfly_riskscore(all_butt_riskscores_df,
                                                        risk_col = "Risk_score")

# standardise mammal risk scores
all_mamm_riskscores_df <- all_riskscores_df %>%
  filter(Taxon == "Mammal")

all_std_mamm_riskscores <- calc_std_mammal_riskscore(all_mamm_riskscores_df,
                                                        risk_col = "Risk_score")

# row bind them into one df again
all_std_riskscores_df <- dplyr::bind_rows(all_std_plant_riskscores,
                                          all_std_bee_riskscores,
                                          all_std_butt_riskscores,
                                          all_std_mamm_riskscores)

### RISK PER SPECIES AND MEAN RISK ----

### predict risk scores from model

risk_per_species <- all_std_riskscores_df %>%
  group_by(Species, Taxon, Strategy) %>%
  summarise(Risk_score = sum(Risk_score)) # get total risk score for each species
  
mean_risk <- risk_per_species %>%
  group_by(Taxon, Strategy) %>%
  summarise(Mean_risk_score = mean(Risk_score)) # find mean of all species per strat per taxon
mean_risk$BAU_score <- c(rep(4.23, 4), rep(8.58, 4), rep(4.01, 4), rep(4.41, 4))

mean_risk <- mean_risk %>%  
  mutate(Percentage_change = (Mean_risk_score / BAU_score) * 100 - 100)

## plot distribution of species risk scores

pdf("../../write_up/Figures/species_risk_dist.pdf")
species_risk <- ggplot(risk_per_species, aes(x = Strategy, y = Risk_score, fill = Taxon)) +
  geom_boxplot() +
  facet_wrap(~factor(Taxon, levels=tax_levels)) +
  scale_x_discrete(labels = c("BAU", "HD-HR", "LD-HR", "LD-LR")) +
  theme_bw() +
  theme(#panel.grid = element_blank(), # remove grid lines
        strip.background = element_blank(), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold", size = 14),
        legend.position = "none",
        axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 10)) +
  labs(x = "\nFarm management strategy",
       y = "Standardised risk score\n")

species_risk
dev.off()

risk_per_species[which.max(
  risk_per_species %>%
  filter(Taxon == "Broadleaf plant") %>%
  pull(Risk_score)
), "Species"]


## FUNCTION TO CALCULATE STANDARD ERROR ----
se <- function(x) sqrt(var(x)/length(x))

### COMPONENTS PLOT ----
risk_per_component <- all_std_riskscores_df %>%
  group_by(Strategy, Taxon, Species, Component) %>% 
  summarise(Risk_score = sum(Risk_score)) %>% # find risk score per species per component
  group_by(Strategy, Taxon, Component) %>% 
  summarise(Risk_score = mean(Risk_score)) # find mean species risk per component
risk_per_component$Relative_risk <- get_relative_risk(risk_per_component, "Risk_score", 3)

# find relative risk for all species individually so can get SE
species_risk_per_component <- all_std_riskscores_df %>%
  group_by(Strategy, Taxon, Species, Component) %>%
  summarise(Risk_score = sum(Risk_score)) # get total risk per species, per component per strat per taxa
species_risk_per_component$Relative_risk <- get_relative_risk(species_risk_per_component, "Risk_score", 3)

species_risk_per_component <- species_risk_per_component %>%
  group_by(Strategy, Taxon, Component) %>%
  summarise(Std_error = se(Relative_risk)) # find SE of relative risk per taxa, strat, component

# add SE to risk_per_component to plot
risk_per_component$Std_error <- species_risk_per_component$Std_error

## set up variables for plotting
component_colours <- c( 
          "crop" = "goldenrod2", 
          "fertiliser" = "navyblue", 
          "glyphosate" = "dodgerblue",
          "pesticide" = "blue2",
          "sowing" = "yellow",
          "BG herbicide" = "lightskyblue",
          "tillage" = "gold")
component_order <- c("BG herbicide", "glyphosate", "pesticide", "fertiliser", "sowing", "tillage", "crop")
component_titles <- c("BG herbicides", "Glyphosate", "Pesticides", "Fertilisers", "Sowing", "Tillage", "Crop choice")

pdf("../../write_up/Figures/components_proportions.pdf", width=8, height=9)

comp_bar <- ggplot(data = risk_per_component, aes(x = Strategy, y = Risk_score, fill = factor(Component, levels = component_order))) +
  geom_col(position = "fill") +
  scale_x_discrete(labels = c("BAU", "HD-HR", "LD-HR", "LD-LR")) +
  facet_wrap(~factor(Taxon, levels=tax_levels), scales="free") +
  theme_bw() +
  theme(panel.grid = element_blank(), # remove grid lines
        strip.background = element_blank(), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold", size = 14),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 10)) + 
  scale_fill_manual(values = component_colours,
                    labels = component_titles) +
  labs(fill = "Agricultural\ncomponent", 
       y = "Mean proportion of standardised risk score\n", 
       x = "\nFarm management strategy")
comp_bar

dev.off()

pdf("../../write_up/Figures/components_rel_risk.pdf")

rel_comp <- ggplot(data=risk_per_component[risk_per_component$Strategy != "BAU",], aes(x = Component, y = Relative_risk, fill = Strategy)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Relative_risk - Std_error, ymax = Relative_risk + Std_error), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~factor(Taxon, levels = tax_levels), scales = "free_x") +
  theme_bw() +
  theme(panel.grid = element_blank(), # remove grid lines
        strip.background = element_blank(), 
        strip.placement = "outside",
        strip.text = element_text(face = "bold", size = 14),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 10, angle = 400, vjust = 0.9, hjust = 1)) + # rotate tick text
  geom_vline(xintercept = seq(0.5, length(risk_components), by = 1), color="gray", size=.5, alpha=.5) + # set vertical lines between x groups
  geom_hline(yintercept = 0, color="gray", size=.5, alpha=.5) +
  scale_x_discrete(labels = c("BG herbicides", "Crop choice", "Fertilisers", "Glyphosate", "Pesticides", "Sowing", "Tillage")) +
  scale_fill_discrete_sequential(palette = "ag_Sunset", 
                                   labels = c("HD-HR", "LD-HR", "LD-LR")) +
  labs(fill = "Black-grass strategy",
       x = "\nAgricultural component",
       y = "Relative standardised risk\n")
rel_comp

dev.off()

# same plot using proportions not raw scores
risk_per_component$Relative_prop <- get_relative_risk(risk_per_component, "Proportion_risk", 3)

pdf("../../write_up/Figures/components_rel_prop.pdf")

prop_rel_comp <-
  ggplot(data=risk_per_component[risk_per_component$Strategy != "BAU",], aes(x = Component, y = Relative_prop, fill = Strategy)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Taxon) +
  theme_bw() +
  theme(panel.grid = element_blank()) + # remove grid lines
  geom_vline(xintercept = seq(0.5, length(risk_components), by = 1), color="gray", size=.5, alpha=.5) + # set vertical lines between x groups
  geom_hline(yintercept = 0, color="gray", size=.5, alpha=.5) +
  theme(axis.text.x = element_text(angle = 400, vjust = 0.9, hjust = 1)) +
  labs(y = "Relative proportion")
prop_rel_comp

dev.off()
  
  
  
  
# can i do difference in proportions like that? work out proportion for BAU and BG strat and take one away from other?

## could make colours by proportion size rather than component - might have to label the components

# could group by known risk to biodiveristy - all chemicals would come under increased agrochems,
# could put tillage and sowing together
# e.g blue means agrochemicals and make them diff shades and stack them together

#proportion of risk due to the different crop categories
# could split crop bar into crop transition types e.g. flowering->cereal




### CROP TYPES YEAR PLOT ----

# extract vector of crop transitions per year per strategy per taxon
crop_transitions <- all_std_riskscores_df %>%
  group_by(Taxon, Strategy, Year, Component, Component_transition) %>%
  summarise(Risk_score = sum(Risk_score)) %>%
  filter(Component == "crop") %>%
  mutate(Component_transition = gsub("wheat -> 18 month fallow", "wheat -> fallow", Component_transition),
         Component_transition = gsub("oil seed rape", "OSR", Component_transition),
         Component_transition = gsub("osr", "OSR", Component_transition),
         Component_transition = gsub(".1", " (2)", Component_transition),
         Component_transition = gsub("wheat -> beans", "wheat -> bean", Component_transition)
         ) %>%
  pull(Component_transition)

risk_per_crop <- all_std_riskscores_df %>%
  group_by(Strategy, Taxon, Species, Year) %>% # needs to be ordered by strategy for relative risk score
  summarise(Risk_score = sum(Risk_score)) %>% # find risk score per species per year
  group_by(Strategy, Taxon, Year) %>% 
  summarise(Risk_score = mean(Risk_score))# %>% # find mean species risk per year
  #ungroup()

risk_per_crop$Relative_risk = get_relative_risk(risk_per_crop, "Risk_score", 3)

# find relative risk for all species individually so can get SE
species_risk_per_crop <- all_std_riskscores_df %>%
  group_by(Strategy, Taxon, Species, Year) %>%
  summarise(Risk_score = sum(Risk_score)) # get total risk per species, per year per strat per taxa
species_risk_per_crop$Relative_risk <- get_relative_risk(species_risk_per_crop, "Risk_score", 3)

species_risk_per_crop <- species_risk_per_crop %>%
  group_by(Strategy, Taxon, Year) %>%
  summarise(Std_error = se(Relative_risk)) # find SE of relative risk per taxa, strat, component

# add SE to risk_per_component to plot
risk_per_crop$Std_error <- species_risk_per_crop$Std_error


risk_per_crop <- risk_per_crop %>%
  arrange(Taxon) %>% # need ordered by taxon to add crop transitions
  add_column(Crop_transition = crop_transitions) %>%
  filter(!(Strategy == "BAU")) %>% # remove BAU as interested in BG relative riskscores
  arrange(Strategy)

risk_per_crop <- risk_per_crop %>%
  mutate(Crop_transition = gsub("->", "\u2192", Crop_transition, fixed = T), # use unicode arrow symbol
         
    Crop_type_transition = case_when( # get types of crop transition
    Crop_transition == "wheat \u2192 fallow" ~ "cereal → none",
    Crop_transition == "wheat \u2192 maize" | 
      Crop_transition == "no change (wheat)" |
      Crop_transition == "no change (wheat) (2)" |
      Crop_transition == "wheat \u2192 barley" |
      Crop_transition == "wheat \u2192 barley (2)" ~ "cereal \u2192 cereal",
    Crop_transition == "OSR \u2192 barley" | 
      Crop_transition == "OSR \u2192 wheat" ~ "flowering \u2192 cereal",
    Crop_transition == "wheat \u2192 OSR" | 
      Crop_transition == "wheat \u2192 bean" ~ "cereal \u2192 flowering",
    Crop_transition == "OSR \u2192 bean" | 
      Crop_transition == "OSR \u2192 bean (2)" | 
      Crop_transition == "no change (OSR)" ~ "flowering \u2192 flowering"))
  

strat_labels <- c("HD-HR", "LD-HR", "LD-LR")
names(strat_labels) <- c("HDHR-N-h", "LDHR-N-h", "LDLR-N-h")

cairo_pdf("../../write_up/Figures/crop_year_bar.pdf", width = 7, height = 9, 
          family = "Arial")

crop_bar <- 
  ggplot(data = risk_per_crop, aes(x = Crop_transition, y = Relative_risk, fill = Crop_type_transition)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_hline(yintercept = seq(-1, 0.5, by = 0.25), 
             color="grey", 
             size=.5, 
             alpha=.5) +
  geom_bar(position = "stack", stat = "identity") +
  geom_errorbar(aes(ymin = Relative_risk - Std_error, ymax = Relative_risk + Std_error), width=.2,
                position=position_dodge(.9)) +
  facet_grid(factor(Taxon, levels = tax_levels) ~ Strategy, scales = "free_x",
             labeller = labeller(Strategy = strat_labels)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 400, vjust = 0.9, hjust = 1),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(face = "bold", size = 14),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 13)) +
  labs(x = "\nCrop transition",
       y = "Mean relative standardised risk\n",
       fill = "Crop type transition") +
  guides(fill = guide_legend(nrow = 2, byrow = T)) + # legend on 2 rows
  scale_fill_discrete_sequential(palette = "Viridis")

crop_bar

dev.off()
## categorise based on type of conversion? eg. cereal -> flowering, grass -> arable
      ## PROBABLY CLEARER TO GROUP BY TYPE ON X AXIS, CAN ALSO COLOUR
## include 'no change' ones??? they do often still convey risk
## CHANGE PLOT SO IS A COMPARISON BETWEEN BAU AND BG STRATS?? E.G. WHEAT-> BARLEY FOR BAU COMPARED TO FOR BG STRSATS?
## OR JUST NOT INCLUDE BAU?? esp if doing relative risk score

## LDHR - wheat -> barley, plants affected disproportionately. they are affected more by weed suppression
# as it affects all species not just those using them as a resource like for other taxa.
# same for osr-> wheat and osr->barley, may be why hdhr not significant

# LDHR = wheat->wheat, wheat->barley, osr->bean, x2
## COULD POTENTIALLY DO A MODEL TO SEE CONTRIBUTION OF CROP TRANSITIONS TO RISK SCORE?
# risk ~ crop_transition_type

crop_transitions_df <- data.frame(Crop_transition = crop_transitions[1:(num_yrs*length(strategy_names))])
crop_count <- crop_transitions_df %>% 
  count(Crop_transition)

crop_count_bar <- ggplot(data = crop_count, aes(x = Crop_transition, y = n)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 400, vjust = 0.9, hjust = 1))
#crop_count_bar # probably not necessary to show

### COMPONENT TRANSITIONS
comp_categories <- all_std_riskscores_df %>%
  group_by(Strategy, Year, Component, Component_transition) %>%
  summarise(Risk_score = sum(Risk_score)) %>%
  filter(!(Component_transition == "wheat -> beans")) %>%
  mutate(Component_transition = gsub("selective BG herbicides -> none", "no", Component_transition),
         Component_transition = gsub("osr", "OSR", Component_transition),
         Component_transition = gsub("fertiliser -> none", "no", Component_transition),
         Component_transition = gsub("autumn sowing -> no sowing", "none", Component_transition),
         Component_transition = gsub("inversion tillage -> no tillage", "none", Component_transition),
         Component_transition = gsub("[a-zA-Z ]+ -> ", "", Component_transition),
         Component_transition = gsub(".[0-9]", "", Component_transition),
         Component_transition = gsub("no change (", "", Component_transition, fixed = T),
         Component_transition = gsub(")", "", Component_transition, fixed = T),
         Component_transition = gsub("selective BG herbicides", "yes", Component_transition, fixed=T),
         Component_transition = gsub("fertiliser", "yes", Component_transition, fixed=T),
         Component_transition = gsub("no autumn glyphosate", "no", Component_transition, fixed=T),
         Component_transition = gsub("autumn glyphosate", "yes", Component_transition, fixed=T),
         Component_transition = gsub("glyphosate", "yes", Component_transition, fixed=T),
         Component_transition = gsub("high pesticides", "yes", Component_transition, fixed=T),
         Component_transition = gsub("no pesticides", "no", Component_transition, fixed=T),
         Component_transition = gsub("pesticides", "yes", Component_transition, fixed=T),
         Component_transition = gsub(" sowing", "", Component_transition, fixed=T),
         Component_transition = gsub(" tillage", "", Component_transition, fixed=T),
         Component_transition = gsub(", autumn", "", Component_transition, fixed=T),
         Component_transition = gsub(" applied", "", Component_transition, fixed=T),
         Component_transition = gsub(" month ", "", Component_transition, fixed=T),
         Component_transition = gsub("beans", "bean", Component_transition, fixed=T),
         Component_transition = gsub("oil seed rape", "OSR", Component_transition, fixed=T)) %>%
  filter(!(Component_transition == "light autumn" | 
             Component_transition == "subsoil autumn" | 
             Component_transition == "inversion autumn")) %>%
  group_by(Strategy, Component) %>%
  dplyr::count(Component_transition)

pdf("../../write_up/Figures/component_categories.pdf", width = 8, height = 11)

all_comp_cat_plots <- lapply(1:length(component_order), function(i)
  ggplot(data = comp_categories[comp_categories$Component == component_order[i],], aes(x = Strategy, y = n, fill = Component_transition)) +
    geom_col() +
    theme_bw() +
    
    scale_y_continuous("Number of years") + 
    ggtitle(component_titles[i]) +
    theme(legend.position = "right",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          panel.grid = element_blank(),  # remove grid lines
          axis.text.x = element_text(size = 10)
    ) +
    scale_x_discrete(labels = c("BAU", "HD-HR", "LD-HR", "LD-LR")) +
    #scale_fill_discrete_qualitative(palette = "Dark 3")
    scale_fill_brewer(palette = "Set1")
  )

blank_plot <- ggplot() + theme_void()


lay <- rbind(c(1,1,2,2),
             c(3,3,4,4),
             c(5,5,6,6),
             c(7,7,8,8))


# arrange them with legend at the bottom
gridExtra::grid.arrange(all_comp_cat_plots[[1]],
                        all_comp_cat_plots[[2]],
                        all_comp_cat_plots[[3]],
                        all_comp_cat_plots[[4]],
                        all_comp_cat_plots[[5]],
                        all_comp_cat_plots[[6]],
                        all_comp_cat_plots[[7]],
                        blank_plot,
                        layout_matrix = lay,
                        heights = (c(2.5,2.5,2.5,2.5)))

dev.off()


### **OLD** COMPONENTS PLOT ----

# extract risk scores + SE
# components_df <- extract_lmer_means(model_comp_tax_int, 7) # 7 is num levels in 'component' fixed effect
# names(components_df) <- c("Rel_risk", "Std_error")
# 
# # add taxon and component columns
# components_df$Taxon <- c(rep("Bees", 7), "Broadleaf plants", "Butterflies", "Mammals", rep(c("Broadleaf plants", "Butterflies", "Mammals"), each=6))
# 
# components <- c("BG herbicide", "Crop", "Fertiliser", "Glyphosate", "Pesticide", "Sowing", "Tillage")
# components_df$Component <- c(components, rep(components[1], 3), rep(components[-1], 3))
#   
# # plot components * taxon
# 
# comp_plot <- ggplot(data = components_df, 
#             aes(x = Component, 
#                 y = Rel_risk, 
#                 ymin = (Rel_risk - Std_error), 
#                 ymax = (Rel_risk + Std_error))) +
#   geom_pointrange() +
#   ylab("Mean relative risk score") +
#   coord_flip() +
#   facet_wrap(~Taxon) +
#   geom_hline(yintercept = 0, lty=2) +
#   ylim(-0.3,0.3)
# 
# comp_plot
# 
# # plot individually for each BG strategy
# 
# ### HDHR
# components_df_HDHR <- extract_lmer_means(model_comp_HDHR, 7) # 7 is num levels in 'component' fixed effect
# names(components_df_HDHR) <- c("Rel_risk", "Std_error")
# 
# components_df_HDHR$Taxon <- components_df$Taxon
# components_df_HDHR$Component <- components_df$Component
# 
# comp_plot_HDHR <- ggplot(data = components_df_HDHR, 
#                     aes(x = Component, 
#                         y = Rel_risk, 
#                         ymin = (Rel_risk - Std_error), 
#                         ymax = (Rel_risk + Std_error))) +
#   geom_pointrange() +
#   ylab("Mean relative risk score HDHR") +
#   coord_flip() +
#   facet_wrap(~Taxon) +
#   geom_hline(yintercept = 0, lty=2) +
#   ylim(-0.3, 0.3)
# 
# comp_plot_HDHR
# 
# 
# ### LDHR
# components_df_LDHR <- extract_lmer_means(model_comp_LDHR, 7) # 7 is num levels in 'component' fixed effect
# names(components_df_LDHR) <- c("Rel_risk", "Std_error")
# 
# components_df_LDHR$Taxon <- components_df$Taxon
# components_df_LDHR$Component <- components_df$Component
# 
# comp_plot_LDHR <- ggplot(data = components_df_LDHR, 
#                          aes(x = Component, 
#                              y = Rel_risk, 
#                              ymin = (Rel_risk - Std_error), 
#                              ymax = (Rel_risk + Std_error))) +
#   geom_pointrange() +
#   ylab("Mean relative risk score LDHR") +
#   coord_flip() +
#   facet_wrap(~Taxon) +
#   geom_hline(yintercept = 0, lty=2) + 
#   ylim(-0.3, 0.3)
# 
# comp_plot_LDHR
# 
# ### LDLR
# components_df_LDLR <- extract_lmer_means(model_comp_LDLR, 7) # 7 is num levels in 'component' fixed effect
# names(components_df_LDLR) <- c("Rel_risk", "Std_error")
# 
# components_df_LDLR$Taxon <- components_df$Taxon
# components_df_LDLR$Component <- components_df$Component
# 
# comp_plot_LDLR <- ggplot(data = components_df_LDLR, 
#                          aes(x = Component, 
#                              y = Rel_risk, 
#                              ymin = (Rel_risk - Std_error), 
#                              ymax = (Rel_risk + Std_error))) +
#   geom_pointrange() +
#   ylab("Mean relative risk score LDLR") +
#   coord_flip() +
#   facet_wrap(~Taxon) +
#   geom_hline(yintercept = 0, lty=2) +
#   ylim(-0.3, 0.3)
# 
# comp_plot_LDLR
# 
# 
# 
# ### **OLD** YEAR PLOT ----
# 
# risk_per_year <- all_std_riskscores_df %>%
#   group_by(Strategy, Taxon, Year) %>%
#   summarise(Risk_score = sum(Risk_score))
# 
# pdf("../../write_up/Figures/year_bar.pdf")
# 
# year_bar <- ggplot(data = risk_per_year, aes(x = Strategy, y = Risk_score, fill = as.factor(Year))) +
#   geom_bar(position = "stack", stat = "identity") +
#   facet_wrap(~Taxon, scales="free")
# year_bar
# ## change to mean risk score???? same for components plot???
# 
# dev.off()

### **OLD** CROP AND FARMLAND RELIANCE PLOTS ----

# number of species using cropped (/arable) area as a habitat for each taxon

# work out number of bee species using cropped arable as either nesting or foraging habitat
bee_CA_hab_only <- bee %>%
  select(CA_hab, `CA_N_Bare ground on banks, paths and tracks`)

total_bee_CA_hab <- apply(bee_CA_hab_only, 1, function(row) row[1] | row[2])

# work out number of mammal species using cropped area as either nesting or feeding habitat
mamm_CA_hab_only <- mammals %>%
  select(C_hab, C_N_AG, C_N_BG)

total_mamm_CA_hab <- apply(mamm_CA_hab_only, 1, function(row) row[1] | row[2] | row[3])

# work out proportion of species reliant on cropped area for each taxon
bee_arable_reliance <- sum(total_bee_CA_hab) / nrow(bee)
butt_ad_arable_reliance <- sum(butterfly$Ad_CA_hab) / nrow(butterfly)
butt_lar_arable_reliance <- sum(butterfly$L_CA_hab) / nrow(butterfly)
mamm_crop_reliance <- sum(total_mamm_CA_hab) / nrow(mammals)
plant_crop_reliance <- sum(plants$C_hab) / nrow(plants)

crop_reliance_df <- data.frame(Taxon = c("Bee", "Butterfly adult", "Butterfly larvae",
                                         "Mammal", "Broadleaf plant"),
                               Crop_reliance = c(bee_arable_reliance,
                                                 butt_ad_arable_reliance,
                                                 butt_lar_arable_reliance,
                                                 mamm_crop_reliance,
                                                 plant_crop_reliance))

pdf("../../write_up/Figures/crop_reliance.pdf")

crop_rel_bar <- ggplot(data = crop_reliance_df, aes(x = Taxon, y = Crop_reliance, fill=Taxon)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("yellow2", "darkgreen", "red", "red", "blue")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "\nTaxonomic group",
       y = "Proportion reliant on crop habitat\n") +
  scale_x_discrete(labels = c("Bee", "Broadleaf plant", "Butterfly (adult)", "Butterfly (larva)", "Mammal"))

crop_rel_bar
# get icons above each bar showing which taxa they are

dev.off()

# mean farmland reliance score
bee_farm_reliance <- sum(bee$Reliance) / nrow(bee)
butt_ad_farm_reliance <- sum(butterfly$Ad_reliance) / nrow(butterfly)
butt_lar_farm_reliance <- sum(butterfly$L_reliance) / nrow(butterfly)
mamm_farm_reliance <- sum(mammals$Reliance) / nrow(mammals)
plant_farm_reliance <- sum(plants$Reliance) / nrow(plants)

farm_reliance_df <- data.frame(Taxon = c("Bee", "Butterfly adult", "Butterfly larvae",
                                         "Mammal", "Broadleaf plant"),
                               Farm_reliance = c(bee_farm_reliance,
                                                 butt_ad_farm_reliance,
                                                 butt_lar_farm_reliance,
                                                 mamm_farm_reliance,
                                                 plant_farm_reliance))

farm_bar <- ggplot(data = farm_reliance_df, aes(x = Taxon, y = Farm_reliance, fill = Taxon)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("yellow2", "darkgreen", "red", "red", "blue")) +
  theme(legend.position = "none")
#farm_bar  # don't need a graph, could just show this in a table, or in the text


## bees have lower reliance on farmland, but what about those that rely on cropped areas?
## could do plot of mean farm reliance for those reliant on cropped area - may not be useful


