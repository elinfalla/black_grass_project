# Script to calculate risk_scores associated with changes to farm management for bumblebees and mammals
# method is from Butler, S. J. et al. A cross-taxonomic index for quantifying the health of farmland biodiversity. J. Appl. Ecol. 46, 1154-1162 (2009).

# load packages----
rm(list = ls())#clean the environment
library(readxl)# for reading in Excel sheets
library(tidyverse) # for data manipulation (package includes dplyr, tidyr, stringr....)
library(ggpubr)


# set working directory ----

setwd("~/Biodiversity_risk_R")

# ****************************************
# *********** BUMBLEBEES *********** ----
# ****************************************

# Load bumblebees data----

fpl <- read_xlsx("forage_plant_location.xlsx", 1)# forage plant locations

fmc <- read_xlsx("change.xlsx", 1)# farm management changes

poll <- read_xlsx("Pollinator matrix.xlsx", 1)# pollinator data, including resource requirements of bumblebees

fmc <- fmc %>% 
  mutate_all(~replace(., is.na(.), 0))# The farm management spreadsheet contains blanks. These are read in as NA. Convert NAs to zeroes.

bb <- poll %>% 
  replace(is.na(.), 0) %>% 
  filter(grepl('Bombus', species))%>% 
  rename(
    # rename forage habitat columns so they are nore meaningful:
    f_hedge = H_fhab, 
    f_margin = M_fhab,
    f_arablefields = CA_fhab,
    f_grassfields = CG_fhab
  ) %>%
  # replace the spaces in the species names with underscores:
  mutate(species = gsub(" ", "_", species)) %>% 
  select(-c("Celasteraceae", "Cistaceae", "Crassulaceae", "Crops", "Ericaceae", "Globulariaceae", "Plumbaginaceae", "Polemoniaceae", "Saxifragaceae") ) 

# remove plants from the pollinator matrix which don't grow in the UK:
# "Cistaceae" - Mediterranean region
# "Crassulaceae" - very unlikely to be found in farmland
# "Crops" - not a plant family
# "Ericaceae" - very unlikely to be found in farmland
# "Globulariaceae" - not in the UK
# "Plumbaginaceae" - grow by the sea
# "Polemoniaceae" - Probably garden escape. Damp and rocky habitats, often close to habitation, damp rocky habitats. So unlikely in UK farmland.
# "Saxifragaceae" - unlikely in most of UK farmland.
# Also remove Celasteraceae as it's not used by any of the pollinator species


# re-order plant family columns alphabetically for ease of inspection:

bb <- bb[,c(names(bb)[which(colnames(bb)=="species"):which(colnames(bb)=="CG_damp plant")],sort(names(bb)[which(colnames(bb)=="Fabaceae"):which(colnames(bb)=="Balsaminaceae")]))]

#function to calculate the risk score of bumblebees----

riskscore <- function(scenario){
  
  # create df 'riskbom' ----
  # this is a new df for calculating risk score
  
  riskbom = setNames(data.frame(matrix(ncol = 14, nrow = length(bb$species))), 
                     c("species", "G", "Gt", "H", "Pt", "A", "F.", "Ft", "B", "N", "Hn", "Nt", "R", "risk_score"))
  
  riskbom$species = bb$species
  
  # Create df 'bb_activity' ----
  
  # To calculate many of the variables, we need to create a dataframe of bumblebee activity from the bumblebee dataframe.
  # Pull out columns relating to activity periods, then create new summary columns 'early', 'mid' and 'late'. 
  # Activity periods:  
  # Early = April-May, mid = June-July, late = August-September.
  
  bb_activityperiods <- bb %>% 
    
    # pull out just the columns we want
    
    select("species","generations",matches("early|mid|late"))%>%
    
    # sum the number of habitats used early-season
    mutate(earlycount = rowSums(.[grep("early", names(.))], na.rm = TRUE),
           
           # sum the number of habitats used mid-season
           midcount = rowSums(.[grep("mid", names(.))], na.rm = TRUE),
           
           # sum the number of habitats used late-season
           latecount = rowSums(.[grep("late", names(.))], na.rm = TRUE))%>% 
    
    # assign 1 if species is active early-season, 0 if not
    mutate(early = replace(earlycount, earlycount>1, 1),
           
           # assign 1 if species is active mid-season, 0 if not   
           mid = replace(midcount, midcount>1, 1),
           
           # assign 1 if species is active late-season, 0 if not
           late = replace(latecount, latecount>1, 1))
  
  # Now create a dataframe containing only the columns 'early', 'mid' and 'late'.
  bb_activity <- bb_activityperiods %>% 
    
    # pull out just the columns we want
    select("species","early","mid","late")
  
  
  
  # Pt = risk score associated with reduced foraging activity potential
  # i) Pt = Gt / (G*H)----
  
  # ********************
  #   G ----
  # ********************
  
  # G = total number of life cycle components (i.e. sum of the number of generations in all activity periods)
  
  riskbom$G = with(bb,generations[match(riskbom$species,species)]) 
  
  
  # ********************
  #   Gt ----
  # ********************
  
  # extract columns containing information about activity periods
  
  # first from the bumblebee matrix
  bb_tpa  <- bb %>% 
    
    # extract the first column, plus any columns containing info on which time periods are affected by the change
    select(1,matches("early|mid|late"))
  
  
  # then from the change matrix
  c_tpa <- fmc %>% 
    
    # choose the change you're investigating
    filter(agric_change == scenario)%>% 
    
    # extract the first column, plus any columns containing info on which time periods are affected by the change
    select(1,matches("early|mid|late"))
  
  bb_tpa$Gt = apply(bb_tpa[,match(colnames(c_tpa[-1])[which(apply(c_tpa[-1], 2, function(x) any(x == -1)))],names(bb_tpa)),drop=FALSE], 1, sum)
  
  riskbom$Gt = with(bb_tpa,Gt[match(riskbom$species,species)])
  
  
  # ********************
  # H----
  # ********************
  
  bb_hab <- bb %>%  
    
    # Select the 1st column ('species') plus forage habitat column (f_hedge, f_margin, f_arablefields,  f_grassfields)
    select(1,matches("f_")) %>% 
    
    # for each species, sum how many of the possible habitats are used by that species
    mutate(H = rowSums(.[2:5]) 
    )
  
  riskbom$H = with(bb_hab,H[match(riskbom$species,species)])
  
  
  # ********************
  # Pt----
  # ********************  
  
  riskbom$Pt = ifelse(
    is.nan(riskbom$Gt / (riskbom$G * riskbom$H)) | is.infinite(riskbom$Gt / (riskbom$G * riskbom$H)) ==T, 
    0, 
    riskbom$Gt / (riskbom$G * riskbom$H)) # Pt = Gt / (G*H)
  
  
  
  # Ft = risk score associated with reduced forage plant availability
  
  # ii) Ft = A/F----
  
  
  # ********************
  #   F ----
  # ********************
  
  # F = points of coincidence between habitat use by a species and the location of its forage plants.
  
  # Create dataframes to work on.
  
  # remove row Cannabaceae from df forage_plant_location as they are wind pollinated
  # remove row Celasteraceae from df forage_plant_location as I can't find any recored of Bombus spp foraging on it
  # remove row Cruciferae from df forage_plant_location as it's just another name for Brassicaceae
  # remove row Ulmaceae from df forage_plant_location as they are wind pollinated (https://www.sciencedirect.com/topics/agricultural-and-biological-sciences/ulmaceae)
  
  
  fploc <- fpl %>% 
    
    filter(!plantfamily %in% c("Cannabaceae", "Celasteraceae", "Cruciferae", "Ulmaceae"))%>% 
    
    # extract forage plant locations relevant to bumblebees
    select("plantfamily":"grassfields" ) 
  
  # make bumblebee forage plant df
  #bb_fpl <- bb[, c(1,which(colnames(bb)=="Aceraceae"):which(colnames(bb)=="Violaceae"))] # extract bumblebee forage plant data
  bb_fpl <- bb %>% 
    select(c("species","Aceraceae":"Violaceae")) 
  
  # make bumblebee forage habitat df
  bb_fhab = bb%>%
    select(1,matches("f_"))%>%
    rename(
      # rename forage habitat columns so they match those in bb_fplhab
      hedge = f_hedge,
      margin = f_margin,
      arablefields = f_arablefields,
      grassfields = f_grassfields
    )
  
  # check that there are the same number of plant families in both the forage plant location df (fploc) and the bb forage plants df (bb_fpl)
  plants_bbforageplants <- colnames(bb_fpl[-1])
  plants_bbforageplants <- sort(plants_bbforageplants)
  plants_location <- fploc$plantfamily
  length(plants_bbforageplants)  # 50
  length(plants_location)  # 50
  setdiff(plants_location,plants_bbforageplants) # check all the plant families are the same in each dataset (should return zero if the same)
  
  
  riskbom$F. <- sapply(seq_len(nrow(riskbom)), function(i){
    sum(fploc[match(colnames(bb_fpl)[which(bb_fpl[i,] == 1)],fploc$plantfamily) , 
              match(colnames(bb_fhab)[which(bb_fhab[i,] == 1)], colnames(fploc))])
  }
  )
  
  
  
  # ********************
  #   A ----
  # ********************
  
  # A = number of points of coincidence between the impact on and species' use of forage plant families
  
  # Pull out forage habitats from the farm management change (fmc) dataframe. Any forage habitats affected will have a -1 in the column.
  
  mc_fha <- fmc %>% 
    
    filter(agric_change == scenario)%>%
    
    select(1,matches("fhab"))%>%
    
    # rename forage habitat columns so they match the column names in the forage plant location dataframe (fploc)
    rename(
      hedge = H_fhab, 
      margin = M_fhab,
      arablefields = CA_fhab,
      grassfields = CG_fhab
    ) 
  
  
  # Find out which forage plants grow in the affected habitats
  
  fploc$affects = apply(fploc[,match(colnames(mc_fha[-1])[which(apply(mc_fha[-1], 2, function(x) any(x == -1)))],names(fploc)),drop=FALSE], 1, sum)
  
  
  
  # Now get the plant families with a 1 in the aut_sow_affects column of df fploc.
  # Use these names to lookup the columns (plant families) in bb_fpl dataframe that are affected,
  # then ascribe a 1 to the bumblebee species that use those plants.
  
  # Subset the bb_fpl df by the plant families affected and calculate rowsums
  
  
  fploverlaps <- bb_fpl %>% 
    select(species,match(as.vector(t(fploc[which(fploc[,6]==1), 1])),colnames(bb_fpl)))%>% 
    transmute(species, A = rowSums(select(., -species)))
  
  riskbom$A = with(fploverlaps,A[match(riskbom$species,species)]) 
  
  
  # ********************
  #   Ft ----
  # ********************  
  
  riskbom$Ft = ifelse(is.nan(riskbom$A / riskbom$F.)==T, 0, riskbom$A / riskbom$F.)# Ft = A / F.
  
  
  # Nt = risk score associated with reduced nesting site availability
  # iii) Nt = B/(N*Hn)----
  
  
  # ********************
  #   B ---- 
  # ********************
  
  # B = number of points of coincidence between the impact on and species' use of nest sites 
  
  
  mc_nha <- fmc %>% 
    
    filter(agric_change == scenario)%>%
    
    select(1,matches("_N_"))%>%
    
    mutate (H_N_count = H_N_Versatile + H_N_Aerial + H_N_Bare,
            M_N_count = M_N_Versatile + M_N_Extensive + M_N_Open + M_N_Bare,
            CA_N_count = CA_N_Bare,
            CG_N_count = CG_N_Versatile + CG_N_Extensive + CG_N_Open + CG_N_Bare)%>% 
    
    mutate(H_N = replace(H_N_count, H_N_count < 0, -1),
           M_N = replace(M_N_count, M_N_count < 0, -1),
           CA_N = replace(CA_N_count, CA_N_count < 0, -1),
           CG_N = replace(CG_N_count, CG_N_count < 0, -1))%>% 
    
    select (agric_change, H_N, M_N, CA_N, CG_N)%>%
    
    rename( n_hedge = H_N,
            n_margin = M_N,
            n_arablefields = CA_N,
            n_grassfields = CG_N)
  
  bb_nhab <- bb %>%
    
    select (species, matches("_N_"))%>%
    
    mutate (H_N_count = H_N_Versatile + H_N_Aerial + H_N_Bare,
            M_N_count = M_N_Versatile + M_N_Extensive + M_N_Open + M_N_Bare,
            CA_N_count = CA_N_Bare,
            CG_N_count = CG_N_Versatile + CG_N_Extensive + CG_N_Open + CG_N_Bare)%>% 
    
    mutate(H_N = replace(H_N_count, H_N_count > 0, 1),
           M_N = replace(M_N_count, M_N_count > 0, 1),
           CA_N = replace(CA_N_count, CA_N_count > 0, 1),
           CG_N = replace(CG_N_count, CG_N_count > 0, 1))%>% 
    
    select (species, H_N, M_N, CA_N, CG_N)%>%
    
    rename( n_hedge = H_N,
            n_margin = M_N,
            n_arablefields = CA_N,
            n_grassfields = CG_N)
  
  bb_nhab$B = apply(bb_nhab[,match(colnames(mc_nha[-1])[which(apply(mc_nha[-1], 2, function(x) any(x == -1)))],names(bb_nhab)),drop=FALSE], 1, sum)
  
  riskbom$B = with(bb_nhab, B[match(riskbom$species, species)])
  
  
  # ********************
  #   N ----
  # ********************
  
  # N = number of nest sites used
  
  
  bb_nsite <- bb %>%
    
    select (species, matches("_N_"))%>%
    
    mutate( Versatile_count = H_N_Versatile + M_N_Versatile + CG_N_Versatile,
            Aerial_count = H_N_Aerial,
            Bare_count = H_N_Bare + M_N_Bare + CA_N_Bare + CG_N_Bare,
            Extensive_count = M_N_Extensive + CG_N_Extensive,
            Open_count = M_N_Open + CG_N_Open)%>%
    
    mutate(Versatile = replace(Versatile_count, Versatile_count > 0, 1),
           Aerial = replace(Aerial_count, Aerial_count > 0, 1),
           Bare= replace(Bare_count, Bare_count > 0, 1),
           Extensive= replace(Extensive_count, Extensive_count > 0, 1),
           Open= replace(Open_count, Open_count > 0, 1))%>%
    
    select (species, Versatile, Aerial, Bare, Extensive, Open)
  
  
  bb_nsite$nsite_count = rowSums(bb_nsite[,2:6])
  
  riskbom$N = with(bb_nsite,nsite_count[match(riskbom$species,species)])
  
  
  
  # ********************
  #   Hn ----
  # ********************
  
  bb_nhab$nhab_count = rowSums(bb_nhab[,2:5])
  
  riskbom$Hn = with(bb_nhab,nhab_count[match(riskbom$species,species)])
  
  
  
  # ********************
  #   Nt ----
  # ********************
  
  
  riskbom$Nt = ifelse(is.nan(riskbom$B / (riskbom$N * riskbom$Hn))==T, 0, riskbom$B / (riskbom$N * riskbom$Hn))  # Nt = B/(N*Hn)
  
  
  # ********************  
  # iv) R ----  
  # ********************  
  
  riskbom$R = with(bb,Reliance[match(riskbom$species,species)]) 
  
  
  # ******************** 
  # Risk score (bumblebees)
  # ******************** 
  
  
  # Risk score (bumblebees)= ( Pt + Ft + Nt ) / R----
  
  
  riskbom$risk_score <- ifelse(is.na( (riskbom$Pt + riskbom$Ft + riskbom$Nt)/riskbom$R)==T, 0, (riskbom$Pt + riskbom$Ft + riskbom$Nt)/riskbom$R )
  
  return(as.data.frame(riskbom$risk_score))
  
}



#input the scenarios need to be examined----
scenario <- c("BAU1_ww_spr-aut", "BAU1_ww_inc-chem", "BAU2_ww_spr-aut",
              "BAU2_ww_inc-chem", "BAU3_wosr_spr-aut","BAU3_wosr_inc-chem", 
              "BAU4_ww_spr-aut", "BAU4_ww_inc-chem", "BAU5_ww_spr-aut", 
              "BAU5_ww_inc-chem", "BAU6_wosr_spr-aut", "BAU6_wosr_inc-chem", 
              
              "Ai_yr1_spr-lin_spr-aut", "Ai_yr1_spr-lin_inc-chem", "Ai_yr2_spr-beans_spr-aut", 
              "Ai_yr2_spr-beans_inc-chem", "Ai_yr3_ww_spr-aut", "Ai_yr3_ww_inc-chem",
              "Ai_yr4_spr-oats_spr-aut", "Ai_yr4_spr-oats_inc-chem", "Ai_yr5_ww_spr-aut", 
              "Ai_yr5_ww_inc-chem", "Ai_yr6_spr-lin_spr-aut", "Ai_yr6_spr-lin_inc-chem",
              
              "Bi_yr1_ww_spr-aut", "Bi_yr1_ww_inc-chem", "Bi_yr2_spr-beans_spr-aut",
              "Bi_yr2_spr-beans_inc-chem", "Bi_yr3_ww_spr-aut","Bi_yr3_ww_inc-chem", 
              "Bi_yr4_w-barley_spr-aut", "Bi_yr4_w-barley_inc-chem", "Bi_yr5_wosr_spr-aut", 
              "Bi_yr5_wosr_inc-chem", "Bi_yr6_ww_spr-aut", "Bi_yr6_ww_inc-chem",
              
              "Ci_yr1_ww_spr-aut", "Ci_yr1_ww_inc-chem", "Ci_yr2_wosr_spr-aut",
              "Ci_yr2_wosr_inc-chem", "Ci_yr3_ww_spr-aut","Ci_yr3_ww_inc-chem", 
              "Ci_yr4_ww_spr-aut", "Ci_yr4_ww_inc-chem", "Ci_yr5_wosr_spr-aut", 
              "Ci_yr5_wosr_inc-chem", "Ci_yr6_ww_spr-aut", "Ci_yr6_ww_inc-chem",
              
              "CONTWW_yr1_ww_spr-aut", "CONTWW_yr1_ww_inc-chem", "CONTWW_yr2_ww_spr-aut",
              "CONTWW_yr2_ww_inc-chem", "CONTWW_yr3_ww_spr-aut","CONTWW_yr3_ww_inc-chem", 
              "CONTWW_yr4_ww_spr-aut", "CONTWW_yr4_ww_inc-chem", "CONTWW_yr5_ww_spr-aut", 
              "CONTWW_yr5_ww_inc-chem", "CONTWW_yr6_ww_spr-aut", "CONTWW_yr6_ww_inc-chem")




#run each scenario through the risk-score function and obtain the risk score----

risk_score <- as.data.frame(lapply(scenario, riskscore))

names(risk_score) <- scenario

species <- as.data.frame(bb$species)

riskpoll_each_yr <- as.data.frame(cbind(species,risk_score))

#calculate the average risk score of the six year rotation
riskpoll_each_yr$BAU = apply(riskpoll_each_yr[,2:13],1,mean)
riskpoll_each_yr$Ai = apply(riskpoll_each_yr[,14:25],1,mean)
riskpoll_each_yr$Bi = apply(riskpoll_each_yr[26:37],1,mean)
riskpoll_each_yr$Ci = apply(riskpoll_each_yr[38:49],1,mean)
riskpoll_each_yr$CWW = apply(riskpoll_each_yr[50:61],1,mean) 

#obtian the df only contains average risk score of each strategy
riskpoll <- riskpoll_each_yr %>% select("bb$species","BAU","Ai","Bi","Ci","CWW")

#write.csv(riskpoll, "riskpoll.csv")


#create df riskpoll_cal ready for data analysis----

risk_BAU <- data.frame(species = bb$species, strategy = rep("BAU", length(bb$species)), 
                       
                       risk_score = 2*riskpoll$BAU)

risk_Ai <- data.frame(species = bb$species, strategy = rep("Ai", length(bb$species)), 
                      
                      risk_score = 2*riskpoll$Ai)

risk_Bi <- data.frame(species = bb$species, strategy = rep("Bi", length(bb$species)), 
                      
                      risk_score = 2*riskpoll$Bi)

risk_Ci <- data.frame(species = bb$species, strategy = rep("Ci", length(bb$species)), 
                      
                      risk_score = 2*riskpoll$Ci)

risk_CWW <- data.frame(species = bb$species, strategy = rep("CWW", length(bb$species)), 
                       
                       risk_score = 2*riskpoll$CWW)


riskpoll_cal <- as.data.frame (rbind (risk_BAU, risk_Ai, risk_Bi, risk_Ci, risk_CWW))

riskpoll_cal$strategy <- factor(riskpoll_cal$strategy)




# ****************************************
# *********** MAMMALS *********** ----
# ****************************************


# load mammals data ----

fmc_mm <- read_xlsx("change.xlsx", 3)# farm management changes for mammals

mal <- read_xlsx("Mammal matrix.xlsx", 1)# mammal data,including resource requirements of mammals

# The spreadsheet contains blanks. These are read in as NA. Convert NAs to zeroes.
fmc_mm <- fmc_mm %>% 
  mutate_all(~replace(., is.na(.), 0)) 

mal <- mal %>% 
  mutate_all(~replace(., is.na(.), 0))

#function to calculate the risk score of mammals----
riskscore_mm <- function(scenario_mm){
  
  riskmal = setNames(data.frame(matrix(ncol = 12, nrow = length(mal$Species))), 
                     c("species", "A", "B", "D", "F", "Dt", "C1", "C2", "N", "Nt", "R", "risk_score"))
  
  riskmal$species = mal$Species #set the species in the riskmal df as the species in the mammal matrix
  
  
  
  # Risk score (mammals)= (Dt+ Nt)/R
  
  #where Dt is the risk score associated with reduced food abundance or availability, 
  #Nt is the risk score associated with reduced breeding success and R is the species' reliance on farmland habitat.  
  
  
  
  #i) Dt = A/(D*F) + B/F	----
  
  
  # ********************
  # A----
  # ********************
  
  #A = number of points of coincidence between the impact on and species' use of dietary components
  
  
  #subset fmc_mm (change matrix) in order to calculate A
  
  
  A_change_mal <- fmc_mm %>% 
    
    filter(agric_change == scenario_mm)%>% #select the agric_change we are interested in 
    
    mutate(Diet_BG = C_BG + M_BG + H_BG + R_BG,
           Diet_AG = C_AG + M_AG + H_AG + R_AG)%>% #add up all the _BG _AG
    
    mutate(Diet_BG_present = replace(Diet_BG, Diet_BG < 0, -1),
           Diet_AG_present = replace(Diet_AG, Diet_AG < 0, -1))%>% #assign -1 if it is affected negatively by the ag change
    
    select(1, matches("Diet_BG|Diet_AG|_seeds|_plant|_vertebrate|_AQ")) #select the column related to dietary components
  
  
  mal_diet = mal %>% 
    
    mutate(Diet_BG = C_BG + M_BG + H_BG + R_BG,
           Diet_AG = C_AG + M_AG + H_AG + R_AG)%>% #add up the _BG _AG in the mammal matrix
    
    mutate(Diet_BG_present = replace(Diet_BG, Diet_BG > 0, 1),
           Diet_AG_present = replace(Diet_AG, Diet_AG > 0, 1)) #assign 1 if a species uses this dietary component
  
  
  mal_diet$A = apply(mal_diet[,match(colnames(A_change_mal[-1])[which(apply(A_change_mal[-1], 2, function(x) any(x == -1)))],names(mal_diet)),drop=FALSE], 1, sum)
  
  
  riskmal$A = with(mal_diet, A[match(riskmal$species, Species)])
  
  
  
  # ******************** 
  # B----
  # ********************
  
  #B = number of points of coincidence between the impact on and species' use of foraging habitat components
  
  B_change_mal <- fmc_mm %>% 
    
    filter(agric_change == scenario_mm)%>% #select the agric_change we are interested in 
    
    select(1, matches("_hab")) #select the column related to the foraging habitat components
  
  mal$B = apply(mal[,match(colnames(B_change_mal[-1])[which(apply(B_change_mal[-1], 2, function(x) any(x == -1)))],names(mal)),drop=FALSE], 1, sum)
  
  riskmal$B = with(mal, B[match(riskmal$species, Species)])
  
  
  
  # ********************  
  # D----
  # ********************
  
  #D = total number of dietary components used by the species 
  
  
  D_mal = mal %>% mutate(D_BG_count = C_BG + M_BG + H_BG + R_BG,
                         D_AG_count = C_AG + M_AG + H_AG + R_AG,
                         D_seeds_count = C_seeds + M_seeds + H_seeds + R_seeds,
                         D_plant_count = C_plant + M_plant + H_plant + R_plant,
                         D_vertebrate_count = C_vertebrate + M_vertebrate + H_vertebrate + R_vertebrate,
                         D_AQ_count = R_AQ) %>% #add up the dietary components in different habitat
    
    mutate(D_BG = replace(D_BG_count, D_BG_count > 0, 1),
           D_AG = replace(D_AG_count, D_AG_count > 0, 1),
           D_seeds = replace(D_seeds_count, D_seeds_count > 0, 1),
           D_plant = replace(D_plant_count, D_plant_count > 0, 1),
           D_vertebrate = replace(D_vertebrate_count, D_vertebrate_count > 0, 1),
           D_AQ = replace(D_AQ_count, D_AQ_count > 0, 1)) %>% #assign 1 if a species uses this dietary component
    
    mutate(D = D_BG + D_AG + D_seeds + D_plant + D_vertebrate + D_AQ)
  
  
  riskmal$D = with(D_mal, D[match(riskmal$species, Species)])
  
  
  # ********************  
  # F----
  # ********************
  
  #F = total number of foraging habitat components used by the species
  
  
  F_mal = mal %>% mutate (F_hab = C_hab + M_hab + H_hab + R_hab)
  
  
  riskmal$F = with(F_mal,F_hab[match(riskmal$species, Species)])
  
  
  
  # ********************  
  # Dt----
  # ********************
  
  riskmal$Dt = ifelse(is.nan(riskmal$A/(riskmal$D*riskmal$F) + riskmal$B/riskmal$F)==T, 0, riskmal$A/(riskmal$D*riskmal$F) + riskmal$B/riskmal$F) 
  #Dt = A/(D*F) + B/F	
  
  
  
  #ii) Nt = C1/N + C2/N	----
  
  
  #where C1 and C2 = number of points of coincidence 
  #between potential impact on 
  #and the species' use of nesting habitat components 
  #if impact is through reduced success in existing habitat and loss of habitat respectively and 
  #N = number of nesting habitat components used by the species.
  
  # ********************
  # C1----
  # ********************
  
  C1_change_mal <- fmc_mm %>% 
    
    
    filter(agric_change == scenario_mm)%>% #select the agric_change we are interested in 
    
    
    
    select(1, matches("AG_S|BG_S"))%>% #select the column related to reduced success in existing habitat
    
    rename(C_N_AG = C_N_AG_S,
           C_N_BG = C_N_BG_S,
           M_N_AG = M_N_AG_S,
           M_N_BG = M_N_BG_S,
           H_N_AG = H_N_AG_S,
           H_N_BG = H_N_BG_S,
           R_N_AG = R_N_AG_S,
           R_N_BG = R_N_BG_S) #rename them in order to match the column name of the mammal matrix. eg if C_N_AG_S is affected, and the mammal species use C_N_AG as nesting habitat component then C1 is 1
  
  
  mal$C1 = apply(mal[,match(colnames(C1_change_mal[-1])[which(apply(C1_change_mal[-1], 2, function(x) any(x == -1)))],names(mal)),drop=FALSE], 1, sum)
  
  riskmal$C1 = with(mal, C1[match(riskmal$species, Species)]) 
  
  
  
  
  
  # ********************  
  # C2----
  # ********************
  
  C2_change_mal <- fmc_mm %>% 
    
    
    filter(agric_change == scenario_mm)%>% #select the agric_change we are interested in 
    
    
    select(1, ends_with("N_AG"), ends_with("N_BG")) # select the column that related to the loss of habitat
  
  
  
  mal$C2 = apply(mal[,match(colnames(C2_change_mal[-1])[which(apply(C2_change_mal[-1], 2, function(x) any(x == -1)))],names(mal)),drop=FALSE], 1, sum)
  
  riskmal$C2 = with(mal, C2[match(riskmal$species, Species)]) 
  
  
  
  
  # ******************** 
  # N----
  # ********************
  
  N_mal = mal %>% mutate(C_N_count = C_N_AG + C_N_BG,
                         M_N_count = M_N_AG + M_N_BG,
                         H_N_count = H_N_AG + H_N_BG,
                         R_N_count = R_N_AG + R_N_BG)%>%
    
    mutate(C_N = replace(C_N_count, C_N_count > 0, 1),
           M_N = replace(M_N_count, M_N_count > 0, 1),
           H_N = replace(H_N_count, H_N_count > 0, 1),
           R_N = replace(R_N_count, R_N_count > 0, 1))%>% #assign 1 if a species uses this nesting habitat component
    
    mutate(N = C_N + M_N + H_N + R_N)
  
  riskmal$N = with(N_mal,N[match(riskmal$species, Species)])
  
  
  # ********************
  # Nt----
  # ********************
  riskmal$Nt = ifelse(is.nan(riskmal$C1/riskmal$N + riskmal$C2/riskmal$N)==T, 0, riskmal$C1/riskmal$N + riskmal$C2/riskmal$N) 
  #Nt = C1/N + C2/N	
  
  
  # ********************
  # iii) R----
  # ********************
  
  riskmal$R = with(mal, Reliance[match(riskmal$species, Species)])
  
  
  # Risk score (mammals)= ( Dt+ Nt ) / R----
  riskmal$risk_score = ifelse(is.nan((riskmal$Dt + riskmal$Nt) / riskmal$R)==T, 0, (riskmal$Dt + riskmal$Nt) / riskmal$R) 
  
  return(as.data.frame(riskmal$risk_score))
  
}

#input the scenarios need to be examined----
scenario_mm <- c("BAU1_ww_spr-aut", "BAU1_ww_inc-chem", "BAU2_ww_spr-aut",
                 "BAU2_ww_inc-chem", "BAU3_wosr_spr-aut","BAU3_wosr_inc-chem", 
                 "BAU4_ww_spr-aut", "BAU4_ww_inc-chem", "BAU5_ww_spr-aut", 
                 "BAU5_ww_inc-chem", "BAU6_wosr_spr-aut", "BAU6_wosr_inc-chem", 
                 
                 "Ai_yr1_spr-lin_spr-aut", "Ai_yr1_spr-lin_inc-chem", "Ai_yr2_spr-beans_spr-aut", 
                 "Ai_yr2_spr-beans_inc-chem", "Ai_yr3_ww_spr-aut", "Ai_yr3_ww_inc-chem",
                 "Ai_yr4_spr-oats_spr-aut", "Ai_yr4_spr-oats_inc-chem", "Ai_yr5_ww_spr-aut", 
                 "Ai_yr5_ww_inc-chem", "Ai_yr6_spr-lin_spr-aut", "Ai_yr6_spr-lin_inc-chem",
                 
                 "Bi_yr1_ww_spr-aut", "Bi_yr1_ww_inc-chem", "Bi_yr2_spr-beans_spr-aut",
                 "Bi_yr2_spr-beans_inc-chem", "Bi_yr3_ww_spr-aut","Bi_yr3_ww_inc-chem", 
                 "Bi_yr4_w-barley_spr-aut", "Bi_yr4_w-barley_inc-chem", "Bi_yr5_wosr_spr-aut", 
                 "Bi_yr5_wosr_inc-chem", "Bi_yr6_ww_spr-aut", "Bi_yr6_ww_inc-chem",
                 
                 "Ci_yr1_ww_spr-aut", "Ci_yr1_ww_inc-chem", "Ci_yr2_wosr_spr-aut",
                 "Ci_yr2_wosr_inc-chem", "Ci_yr3_ww_spr-aut","Ci_yr3_ww_inc-chem", 
                 "Ci_yr4_ww_spr-aut", "Ci_yr4_ww_inc-chem", "Ci_yr5_wosr_spr-aut", 
                 "Ci_yr5_wosr_inc-chem", "Ci_yr6_ww_spr-aut", "Ci_yr6_ww_inc-chem",
                 
                 "CONTWW_yr1_ww_spr-aut", "CONTWW_yr1_ww_inc-chem", "CONTWW_yr2_ww_spr-aut",
                 "CONTWW_yr2_ww_inc-chem", "CONTWW_yr3_ww_spr-aut","CONTWW_yr3_ww_inc-chem", 
                 "CONTWW_yr4_ww_spr-aut", "CONTWW_yr4_ww_inc-chem", "CONTWW_yr5_ww_spr-aut", 
                 "CONTWW_yr5_ww_inc-chem", "CONTWW_yr6_ww_spr-aut", "CONTWW_yr6_ww_inc-chem")


#run each scenario through the risk-score function and obtain the risk score----

risk_score_mm <- as.data.frame(lapply(scenario_mm, riskscore_mm))

names(risk_score_mm) <- scenario_mm

species_mm <- as.data.frame(mal$Species)

riskmal_each_yr <- as.data.frame(cbind(species_mm,risk_score_mm))

riskmal_each_yr$BAU =  apply(riskmal_each_yr[,2:13],1,mean)
riskmal_each_yr$Ai = apply(riskmal_each_yr[,14:25],1,mean)
riskmal_each_yr$Bi = apply(riskmal_each_yr[26:37],1,mean)
riskmal_each_yr$Ci = apply(riskmal_each_yr[38:49],1,mean)
riskmal_each_yr$CWW = apply(riskmal_each_yr[50:61],1,mean)


riskmal <- riskmal_each_yr %>% select("mal$Species","BAU","Ai","Bi","Ci","CWW")

#write.csv(riskmal, "riskmal.csv")

#create df riskmal_cal ready for data analysis----

risk_BAU_mm <- data.frame(species = mal$Species, strategy = rep("BAU", length(mal$Species)), 
                          
                          risk_score = 2*riskmal$BAU)

risk_Ai_mm <- data.frame(species = mal$Species, strategy = rep("Ai", length(mal$Species)), 
                         
                         risk_score = 2*riskmal$Ai)

risk_Bi_mm <- data.frame(species = mal$Species, strategy = rep("Bi", length(mal$Species)), 
                         
                         risk_score = 2*riskmal$Bi)

risk_Ci_mm <- data.frame(species = mal$Species, strategy = rep("Ci", length(mal$Species)), 
                         
                         risk_score = 2*riskmal$Ci)

risk_CWW_mm <- data.frame(species = mal$Species, strategy = rep("CWW", length(mal$Species)), 
                          
                          risk_score = 2*riskmal$CWW)


riskmal_cal <- as.data.frame (rbind (risk_BAU_mm, risk_Ai_mm, risk_Bi_mm, risk_Ci_mm, risk_CWW_mm))

riskmal_cal$strategy <- factor(riskmal_cal$strategy)







#Kruskal-Wallis----

kruskal.test(risk_score ~ strategy, data = riskmal_cal)
kruskal.test(risk_score ~ strategy, data = riskpoll_cal)

#data visualization----

bb_group <- data.frame(group = rep("bumblebees", length(riskpoll_cal$risk_score))) 

bb_cal <- as.data.frame (cbind(bb_group, riskpoll_cal))                          


mm_group <- data.frame(group = rep("mammals", length(riskmal_cal$risk_score))) 

mm_cal <- as.data.frame (cbind(mm_group, riskmal_cal))                          

bb_mm_cal <- as.data.frame(rbind(bb_cal, mm_cal))




bb_mm_boxplot <- ggboxplot(bb_mm_cal, x = "strategy", y = "risk_score", 
                           facet.by = "group",
                           ylab = "risk score",
                           bxp.errorbar = T,
                           title = "risk scores of different farm management strategies for bumblebees and mammals")+
  
  labs(subtitle = "Kruskal-Wallis test: df = 4, p = 0.27 (bumblebees); df = 4, p = 0.82 (mammals)")


bb_mm_boxplot


