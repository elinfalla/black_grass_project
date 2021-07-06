rm(list=ls())

library(tidyverse)
library(readr)

knitr::purl("butterfly_bee_riskscore.Rmd")
load("butterfly_bee_riskscore.R")
#file.remove("butterfly_bee_riskscore.R")

knitr::purl("mammals_riskscore.Rmd")
load("mammals_riskscore.R")
#file.remove("mammals_riskscore.R")

knitr::purl("arable_plant_riskscore.Rmd")
load("arable_plant_riskscore.R")
#file.remove("arable_plant_riskscore.R")

## then calculate risk scores...