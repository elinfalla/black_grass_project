calculate_G_H <- function(data) {
  
  # create output dataframe
  output <- data.frame(H = rep(NA, nrow(data)),
                       G = rep(NA, nrow(data)))
  
  # calculate H
  output[,"H"] <- data %>%
    select(hab_start : hab_end) %>%  # select habitat columns
    select(-c(hab_start, hab_end)) %>%  # remove marker cols
    mutate(Cropped = CA_hab | CG_hab) %>%  # create cropped area col and remove CG and CA
    select(-c(CA_hab, CG_hab)) %>%
    rowSums(.)
  
  # calculate G (select generations col of respective df)
  output[,"G"] <- data %>%
    select(gens_start : gens_end) %>%
    rowSums(.)
  
  return(output)
}

calculate_Gt <- function(data, strat) {
  
  # select relevant cols of strat
  activity_effects <- strat %>%
    select(activity_start : activity_end) %>%
    select(-c(activity_start, activity_end)) # remove start and end cols
  
  activity <- data %>%
    select(activity_start : activity_end) %>%
    select(-c(activity_start, activity_end))
  
  gens_per_period <- data %>%
    select(gens_start : gens_end) %>%
    select(-c(gens_start, gens_end))
  
  early <- gens_per_period %>%
    select(contains("early")) %>%
    rowSums(.)
  mid <- gens_per_period %>%
    select(contains("mid")) %>%
    rowSums(.)
  late <- gens_per_period %>%
    select(contains("late")) %>%
    rowSums(.)
  
  all_gens_per_period <- data.frame(rep(data.frame(early, mid, late), 4))
  
  # Check columns of diets_habs and strat are the same (if not aligned, overlap calculation will not be correct)
  aligned <- identical(colnames(activity), colnames(activity_effects))
  
  if (aligned == F) {
    warning(paste0("WARNING: Butterfly data and management strategy columns do not align/are not the same - Gt calculations will be incorrect. Lifestage = ", i, ", 1=adult, 2=larvae"))
  }
  
  # find overlap
  overlap <- as.data.frame(t(apply(activity, 1, function(row) row & activity_effects))) # NOTE: THIS ASSUMES THE COLUMNS ALIGN
  names(overlap) <- names(activity_effects)
  
  Cropped <- t(apply(overlap, 1, function(row) row[7:9] | row[10:12] ))
  
  truncated_overlap <- as_tibble(overlap) %>%
    select(-contains(c("CA","CG"))) %>%
    cbind(., Cropped)
  
  all_gens_affected <- truncated_overlap * all_gens_per_period
  
  Gt <- rowSums(all_gens_affected)
  
  return(Gt)
  
}

calculate_Pt <- function(data, strat) {
  
  G_H <- calculate_G_H(data)
  G <- G_H$G
  H <- G_H$H
  
  Gt <- calculate_Gt(data, strat)
  
  # CALCULATE Pt
  Pt = Gt / (G * H)
  
  # replace infinite or NA values with 0
  Pt <- 
    ifelse(
      is.nan(Pt) | is.infinite(Pt) == T,  # test
      0, # if yes
      Pt  # if no
    )
  
  return(Pt)
}

calculate_A_F <- function(data, plants, plant_locs, strat) {
  
  # PUT IN CHECK THAT COLUMNS ALIGN
  
  # select hab + damp cols of change data
  hab_effects <- strat %>%
    select(hab_start:hab_end, damp_start:damp_end)
  
  # pull plant names from plant_locs and plants, to check they are the same
  plant_locs_names <- plant_locs %>% pull(plantfamily)
  plants_names <- colnames(plants %>% 
                             select(species_start:species_end) %>% 
                             select(-c(species_start, species_end)))
  
  # check plant names are the same, print warning if not
  if (identical(plant_locs_names, plants_names) == F) {
    warning("WARNING: columns of butterfly forage plants are not the same as rows of plant locations data, i.e. plant species are not the same. Calculations may be incorrect.")
    
    print("Cols of butterfly plant data:")
    print(paste(plants_names))
    print(paste("Rows of plant locations data:", plant_locs_names))
  }
  
  # initialise output dataframe
  output <- data.frame(F = rep(NA, nrow(data)),
                       A = rep(NA, nrow(data)))
  
  # subset appropriate butterfly requirements data to habitat and damp cols
  habs <-  data %>% 
    select(hab_start:hab_end, damp_start:damp_end)
  
  # select plant species cols from butterfly forage plants data
  butterfly_plants <- plants %>% 
    select(species_start : species_end) %>%
    select(-c(species_start, species_end)) # remove marker cols
  
  # find overlap between species habitat use and effects on habitat
  overlap <- as.data.frame(t(apply(habs, 1, function(row) row & hab_effects)))
  colnames(overlap) <- colnames(habs)
  
  # split effects on species into general and damp effects
  general_effects <- overlap %>% 
    select(hab_start : hab_end) %>%
    select(-c(hab_start, hab_end))  # remove marker cols
  
  damp_effects <- overlap %>% 
    select(damp_start : damp_end) %>%
    select(-c(damp_start, damp_end))  # remove marker cols
  
  # calculate A and F for each species
  for(species in 1:nrow(habs)) {
    
    sp_general_effects <- general_effects[species,]
    sp_damp_effects <- damp_effects[species,]
    
    # remove species name col of plant locs (so columns align with habs)
    no_name_plant_locs <- plant_locs[,-1] 
    
    # subset plant locs to only include plants the species forages on
    sp_plant_locs <- no_name_plant_locs[which(butterfly_plants[species,] == 1),]
    
    # if species has no forage plants, print warning and set A and F to 0
    if (nrow(sp_plant_locs) == 0) { 
      warning(paste("WARNING: Species", species, "has 0 forage plants. Al = 0
                    "))
      output[species, lifestage] <- 0
      output[species, lifestage+2] <- 0
      next  # go to next iteration of for loop
    }
    
    # CALCULATE F
    
    # subset habitat to current species
    sp_habs <- habs %>%
      select(hab_start : hab_end) %>%
      select(-c(hab_start, hab_end))
    sp_habs <- sp_habs[species,]
    
    # remove 'cropped area' habitat (col) so the sp_plant_loc cols align with sp_habs (species habitat)
    no_crops_plant_locs <- sp_plant_locs %>%
      select(-croppedarea)
    
    # subset sp_plant_locs to only habitats occupied by the species
    all_forage_plants <- no_crops_plant_locs[,which(sp_habs != 0)]
    
    # if both grass and arable fields are selected (are part of species habitat), then remove them and replace with 'croppedarea' col
    if (has_name(all_forage_plants, "grassfields") && has_name(all_forage_plants, "arablefields")) {
      all_forage_plants <- all_forage_plants %>%
        select(-c(grassfields, arablefields)) %>%
        cbind(., sp_plant_locs$croppedarea)
    }
    output[species, "F"] <- sum(all_forage_plants)
    
    # CALCULATE A
    
    # find all plants affected by general and damp hab effects
    sp_effects <- list(sp_general_effects, sp_damp_effects)
    plants_affected <- c(NA, NA)
    
    for (effect in 1:length(sp_effects)) {
      
      plant_locs_affected <- sp_plant_locs[,which(sp_effects[[effect]] != 0)]
      
      if (ncol(plant_locs_affected) == 0) {
        
        plants_affected[effect] <- 0
        next  # if no plants affected, skip to next iteration (next effect)
      }
      
      # if both grass and arable fields are affected, then remove them and replace with 'croppedarea' col
      if (has_name(plant_locs_affected, "grassfields") && has_name(plant_locs_affected, "arablefields")) {
        plant_locs_affected <- plant_locs_affected %>%
          select(-c(grassfields, arablefields)) %>%
          cbind(., sp_plant_locs$croppedarea)
      } 
      else if (hab_effects$CG_hab != 0 & hab_effects$CA_hab != 0 & effect == 1){ #  if strat affects all crops (not damp)
        if (sum(has_name(plant_locs_affected, c("grassfields", "arablefields"))) > 0) { # if sp is on at least 1 crop
          
          if (sum(sp_habs) > 1) { # if sp is also on at least 1 other hab
            
            plant_locs_affected <- plant_locs_affected %>% # do same as above - used cropped area col
              select(-contains(c("grassfields", "arablefields"))) %>%
              cbind(., sp_plant_locs$croppedarea)  ## IS THIS TRUE FOR DAMP PLANTS TOO??
          }
        }
      }
      
      if (effect == 2) {  # if damp effects, need to subset to only damp plants
        damp_plants_affected <- plant_locs_affected[sp_plant_locs$likesdamp == "yes",]
        
        plants_affected[effect] <- sum(damp_plants_affected)
      }
      else {
        plants_affected[effect] <- sum(plant_locs_affected)
      }
      
    }
    # sum plants affected by general and damp effects to get total effects
    total_plants_affected <- sum(plants_affected)
    
    output[species, "A"] <- total_plants_affected
  }
  
  return(output)
}

calculate_Ft <- function(data, plants, plant_locs, strat) {
  
  A_F <- calculate_A_F(data, plants, plant_locs, strat)
  A <- A_F$A
  F <- A_F$F
  
  # CALCULATE Ft
  Ft <- A / F
  
  # replace infinite or NA values with 0
  Ft <- 
    ifelse(
      is.nan(Ft) | is.infinite(Ft) == T,  # test
      0, # if yes
      Ft  # if no
    )
  
  return(Ft)
}
