# Script to calculate risk scores associated with changes to farm management for butterflies
# method is from Butler, S. J. et al. A cross-taxonomic index for quantifying the health of farmland biodiversity. J. Appl. Ecol. 46, 1154-1162 (2009).


# load packages and data ----

rm(list = ls())
suppressMessages(library("XLConnect"))
suppressMessages(library(dplyr))
library(stringr)

setwd("C:/Users/varah.a/Desktop/Documents/Black Grass Resistance Initiative/Biodiversity/Risk")
#bb = loadWorkbook("bumblebee forage x habitat.xlsx") # load forage x location info for bumblebee species # requires library("XLConnect")
#sy = readWorksheet(bb, sheet = "bombus_sylvarum") # get Bombus sylvarum
ac = loadWorkbook("agric_change.xlsx") # agricultural change impacts
aut_sow = readWorksheet(ac, sheet = "aut_sow")

# read all sheets of a workbook into a list of dataframes using library XLConnect
importWorksheets <- function(filename) {
  # filename: name of Excel file
  workbook <- loadWorkbook(filename)
  sheet_names <- getSheets(workbook)
  names(sheet_names) <- sheet_names
  sheet_list <- lapply(sheet_names, function(.sheet){
    readWorksheet(object=workbook, .sheet)})
}

resrq <- importWorksheets("C:/Users/varah.a/Desktop/Documents/Black Grass Resistance Initiative/Biodiversity/Risk/resource requirements all spp.xlsx")
list2env(resrq,envir=.GlobalEnv) # converts all the list objects into dataframes

forhab <- importWorksheets("C:/Users/varah.a/Desktop/Documents/Black Grass Resistance Initiative/Biodiversity/Risk/forage x habitat.xlsx")
list2env(forhab,envir=.GlobalEnv) # converts all the list objects into dataframes


# create df 'risk' ----

# this is a new df for calculating risk score, including both bb and butterflies
# create spp list of bb and butt
#spp = rbind(Bombus_spp_list,Butterfly_spp_list)
#spp$taxon =  ifelse(grepl("Bombus", spp$species), "bumblebee", "butterfly")
#risk = setNames(data.frame(matrix(ncol = 15, nrow = length(spp$species))), 
#                c("species", "taxon", "G", "Gt", "H", "Pt", "A", "F.", "Ft", "B", "N", "Hn", "Nt", "R", "risk_score"))
#risk$species = spp$species
#risk$taxon = with(spp,taxon[match(risk$species,species)]) 


# do it for just butterflies first

# Risk score = (( Pta + Fta) / Ra + ((Ptl + Ftl_ / Rl ))) # now has separate parts for adult and larval stages

riskbutt = setNames(data.frame(matrix(ncol = 18, nrow = length(Butterfly_spp_list$species))), 
                c("species", 
                  "Ga", "Gta", "Ha", "Pta",  "Gl", "Gtl", "Hl", "Ptl",
                  "Aa", "Fa", "Fta",  "Al", "Fl", "Ftl", 
                  "Ra", "Rl", 
                  "risk_score"))

riskbutt$species = Butterfly_spp_list$species


# create lists of dataframes of all species, and ensure relevant columns are numeric ----

allspp.list <- list(Ochlodes_sylvanus,      Thymelicus_lineola,     Thymelicus_sylvestris,  Lycaena_phlaeas,       
                    Celastrina_argiolus,    Polyommatus_icarus,     Satyrium_w_album,       Thecla_betulae,        
                    Aglais_io,              Aglais_urticae,         Euphydryas_aurina,      Polygonum_c_album,     
                    Aphantopus_hyperantus,  Coenonympha_pamphilus,  Lasiommata_megera,      Maniola_jurtina,       
                    Melanargia_galathea,    Pararge_aegeria,        Pyronia_tithonus,       Gonepteryx_rhamni,     
                    Anthocharis_cardamines, Pieris_brassicae,       Pieris_napi,            Pieris_rapae,
                    Bombus_distinguendus,   Bombus_hortorum,        Bombus_humilis,         Bombus_jonellus,      
                    Bombus_lapidarius,      Bombus_lucorum,         Bombus_muscorum,        Bombus_pascuorum,
                    Bombus_pratorum,        Bombus_ruderarius,      Bombus_ruderatus,       Bombus_soroeensis,    
                    Bombus_sylvarum,        Bombus_terrestris )


allspp.list.num <- lapply(allspp.list, function(x) x[colnames(x[-1])] <- sapply(x[colnames(x[-1])],as.numeric) ) # convert relevant columns to numeric for all species dfs
# get warnings but I can use this anyway....

buttspp.list <- list(Ochlodes_sylvanus,      Thymelicus_lineola,     Thymelicus_sylvestris,  Lycaena_phlaeas,  # make list of all the butterfly dataframes     
                     Celastrina_argiolus,    Polyommatus_icarus,     Satyrium_w_album,       Thecla_betulae,        
                     Aglais_io,              Aglais_urticae,         Euphydryas_aurina,      Polygonum_c_album,     
                     Aphantopus_hyperantus,  Coenonympha_pamphilus,  Lasiommata_megera,      Maniola_jurtina,       
                     Melanargia_galathea,    Pararge_aegeria,        Pyronia_tithonus,       Gonepteryx_rhamni,     
                     Anthocharis_cardamines, Pieris_brassicae,       Pieris_napi,            Pieris_rapae )

buttspp.list.num <- lapply(buttspp.list, function(x) x[colnames(x[-1])] <- sapply(x[colnames(x[-1])],as.numeric) ) # first, make the relevant columns numeric in all dfs in the list


# Pta & Ptl ----

# Pt = risk score associated with reduced foraging activity potential
# Pta = Gta / (Ga*Ha)
# Ptl = Gtl / (Ga*Hl)


    # Ga & Gl ----

# Ga = total number of adult life cycle components (i.e. sum of the number of generations in all activity periods)
riskbutt$Ga = with(activity[which(activity$lifestage == "a"),] ,generations[match(riskbutt$species,species)]) 

# Gl = total number of larval life cycle components (i.e. sum of the number of generations in all activity periods)
riskbutt$Gl = with(activity[which(activity$lifestage == "l"),] ,generations[match(riskbutt$species,species)]) 


    # Gta & Gtl ----

# Gta = number of generations of a species' adult stage active in the activity periods affected
adultactivity = activity[which(activity$lifestage == "a"),] # subset for adult butterflies first
adultactivity$Gta = apply(adultactivity[ , match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(adultactivity)),drop=FALSE], 1, sum)
riskbutt$Gta = with(adultactivity,Gta[match(riskbutt$species,species)]) 

# Gtl = number of generations of a species' larval stage active in the activity periods affected
larvalactivity = activity[which(activity$lifestage == "l"),] # subset for larvae
larvalactivity$Gtl = apply(larvalactivity[ , match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(adultactivity)),drop=FALSE], 1, sum)
riskbutt$Gtl = with(larvalactivity,Gtl[match(riskbutt$species,species)]) 


    # Ha & Hl ----

# Ha = number of habitat components used by the adult stage of the species
habbutt <- hab[-grep("Bombus", hab$species), ] # subset hab to get just the butterflies out
library(stringr)
habbutt$Ha <- Reduce(`+`, lapply(habbutt[-1], 
                                function(x) lengths(str_extract_all(x, "a")) # sum instances of "a" for each species (i.e. across rows)
                                )
                     ) 
riskbutt$Ha = with(habbutt,Ha[match(riskbutt$species,species)]) # add Ha into riskbutt df

# Hl = number of habitat components used by the larval stage of the species
habbutt$Hl <- Reduce(`+`, lapply(habbutt[-1], function(x) lengths(str_extract_all(x, "l")))) # sum instances of "a" for each species (i.e. across rows)
riskbutt$Hl = with(habbutt,Hl[match(riskbutt$species,species)]) # add Ha into riskbutt df


    # Pta & Ptl ----

# Pt = risk score associated with reduced foraging activity potential

# Pta = Gta / (Ga*Ha)
riskbutt$Pta = ifelse(is.nan(riskbutt$Gta / (riskbutt$Ga * riskbutt$Ha))==T,
                      0,
                      riskbutt$Gta / (riskbutt$Ga * riskbutt$Ha)) 

# Ptl = Gtl / (Ga*Hl)
riskbutt$Ptl = ifelse(is.nan(riskbutt$Gtl / (riskbutt$Gl * riskbutt$Hl))==T,
                      0,
                      riskbutt$Gtl / (riskbutt$Gl * riskbutt$Hl)) 



# Fta & Ftl ----

# Ft = risk score associated with reduced forage plant availability
# Ft = A/F

    # Fa & Fl ----
# Fa = points of coincidence between adult stage habitat and the location of its forage plants
# ****** NB this isn't fool-proof as it's dependent on the 'plantfamily' column being ordered the ******
# ****** same in all the species files and in df 'forage_plant_location' ******
# ****** and on the columns being in the same order in the forage_plant_location and all the species files  ******
# ****** and on exactly the same plant families being present in all files. ******

# 1. check all species files agains forage_plant_location:

# a) do they have the same column names in same order?

#columnresult <- lapply(buttspp.list, function(x) identical(names(forage_plant_location[1:7]), names(x)))
#all(columnresult==TRUE) # are all the elements in columnresult 'TRUE'?
all(lapply(buttspp.list, function(x) identical(names(forage_plant_location[1:7]), names(x)))==TRUE) # if they are all TRUE, then all dataframes have matching column names

# b) is the plantfamily column identical in all files?

#identical(forage_plant_location[[1]], Ochlodes_sylvanus[[1]] ) # checks whether column 1 (i.e. plant family) is the same
#rowres <- lapply(df.list, function(x) identical(forage_plant_location[[1]], x[[1]] ) ) # checks this for every dataframe in the list of dataframes
#all(rowres==TRUE) # asks if every result from the previous line is TRUE
all(lapply(buttspp.list, function(x) identical(forage_plant_location[[1]], x[[1]] ) )==TRUE)


# 2. If all OK, calculate F

# ******NB***********
# if the rows and columns of forage_plant_location and the insect species dfs aren't in the same order 
# then the code below won't give the right result

riskbutt$Fa <- unlist(lapply(buttspp.list, function(x) length(forage_plant_location[x=="a" | x=="a,l"])))
riskbutt$Fl <- unlist(lapply(buttspp.list, function(x) length(forage_plant_location[x=="l" | x=="a,l"])))



    # Aa & Al ----


# Aa = number of points of coincidence between the impact on and species' adult stage use of forage plant families

adultactivity = activity[which(activity$lifestage == "a"),] # subset for adult butterflies first

riskbutt$Aa <- unlist(lapply(buttspp.list.num, function(x) ifelse(adultactivity[rownames(adultactivity)[[1]],match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])<0)],names(adultactivity))] == 0,
                                                                  0,
                                                                  colSums(data.frame(colSums(x[match(str_remove(aut_sow[grep('fhab_', aut_sow$resource),][which(apply(aut_sow[grep('fhab_', aut_sow$resource),], 1, function(r) any(r %in% -1))),1],"fhab_"),names(x))], na.rm=TRUE)))
                                                                  )
                             )
                      )

# Al = number of points of coincidence between the impact on and species' larval stage use of forage plant families

larvalactivity = activity[which(activity$lifestage == "l"),] # subset for adult butterflies first

riskbutt$Al <- unlist(lapply(buttspp.list.num, function(x) ifelse(larvalactivity[rownames(larvalactivity)[[1]],match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])<0)],names(larvalactivity))] == 0,
                                                                  0,
                                                                  colSums(data.frame(colSums(x[match(str_remove(aut_sow[grep('fhab_', aut_sow$resource),][which(apply(aut_sow[grep('fhab_', aut_sow$resource),], 1, function(r) any(r %in% -1))),1],"fhab_"),names(x))], na.rm=TRUE)))
                                                                  )
                             )
                      )


# how I worked out A ----

# 1

# a) Which time period does it impact? (find the columns in 'aut_sow' that have -1 in them)
#colnames(aut_sow[-1])[which(colSums(aut_sow[-1])<0)] # this returns any column names in 'aut_sow' (apart from the first column) that sum to less than zero; i.e., which contain a -1 (so where autumn sowing has an impact)
#match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])<0)],names(activity)) # this matches the column names returned above (e.g. 'late') with the column names in 'activity', and returns the column index of the matching column in 'activity'


# b) Is the insect active in that/those time period(s)?
# i.e. see if Bombus sylvarum has a 1 in the column in 'activity' returned above 
#activity[rownames(activity)[[1]],match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])<0)],names(activity))] 
# this returns the value in the column of 'activity' returned above, for the first row of 'activity', which is Bombus sylvarum
# if the value returned is not zero, then the insect is active in the period concerned


# 2

# If the insect is active in the period concerned, work out if its forage habitat is affected.

# First change relevant columns in the species files to numeric (they are all character and contain strings 'NA') so can later do rowSums on columns
###not run: Bombus_sylvarum[relevant columns] <- sapply(Bombus_sylvarum[relevant columns],as.numeric) 
#Bombus_sylvarum[colnames(Bombus_sylvarum[-1])] <- sapply(Bombus_sylvarum[colnames(Bombus_sylvarum[-1])],as.numeric) # check result: sapply(Bombus_sylvarum, class) 
#colSums(Bombus_sylvarum[-1], na.rm=TRUE)


# a) Which habitat is affected?

# which foraging resource in 'aut_sow' does it impact? (find the rows that have -1 in them)
#aut_sow_forageresource <- aut_sow[grep('fhab_', aut_sow$resource),] # grep returns the rows in the 'resource' column which contain foraging habitat
# if I wanted to look up values other than -1 I could do it this way:
###not run: aut_sow_forageresource[which(apply(aut_sow_forageresource, 1, function(r) any(r %in% c(-1,1)))),1] 

# get just the name of the habitat, without prefix 'fhab_'
#library(stringr)
###not run: forage_res_affected <- aut_sow_forageresource[which(apply(aut_sow_forageresource, 1, function(r) any(r %in% -1))),1] # indexes row(s) returned by the which() statement, and column number 1, to get the name(s) of the habitat(s) affected
###not run: forage_res_affected <- str_remove(forage_res_affected, "fhab_") # removes the string 'fhab_'
#forage_res_affected <- str_remove(aut_sow_forageresource[which(apply(aut_sow_forageresource, 1, function(r) any(r %in% -1))),1],"fhab_") # indexes row(s) returned by the which() statement, and column number 1, to get the name(s) of the habitat(s) affected
# returns "arablefields"

# b) does the bee forage in these habitats?

# If the bee forages in those locations there will be 1's in these columns
#match(forage_res_affected,names(Bombus_sylvarum)) # returns the index of the columns in B.sylv that match the affected forage resource
#colSums(Bombus_sylvarum[match(forage_res_affected,names(Bombus_sylvarum))], na.rm=TRUE) # sums the columns returned by the match - if the bee is affected, the sum should be >0

#forage_affected_sums <- data.frame(colSums(Bombus_sylvarum[match(forage_res_affected,names(Bombus_sylvarum))], na.rm=TRUE)) 
###not run: names(forage_affected_sums)[names(forage_affected_sums) == "colSums.Bombus_sylvarum.match.forage_res_affected..names.Bombus_sylvarum...."] <- "A"
# or rename column by column index
#names(forage_affected_sums)[1]<-"A" # rename the column as A

#df <- data.frame(colname = "B_sy",A=colSums(forage_affected_sums))
#print(df, row.names=F)


# then put it in an ifelse statement so that if the answer to 1 is zero, the value ascribed is zero, and if not, A is calculated.
#ifelse(activity[rownames(adultactivity)[[1]],match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])<0)],names(adultactivity))] == 0,
#       0,
#       colSums(data.frame(colSums(Bombus_sylvarum[match(str_remove(aut_sow[grep('fhab_', aut_sow$resource),][which(apply(aut_sow[grep('fhab_', aut_sow$resource),], 1, function(r) any(r %in% -1))),1],"fhab_"),names(Bombus_sylvarum))], na.rm=TRUE)))
#)


    # Fta & Ftl ----

riskbutt$Fta <- ifelse( (riskbutt$Aa / riskbutt$Fa) == "NaN", 0, riskbutt$Aa / riskbutt$Fa )
riskbutt$Ftl <- ifelse( (riskbutt$Al / riskbutt$Fl) == "NaN", 0, riskbutt$Al / riskbutt$Fl )


# R ----

farmland_reliance[1:14,3] <- 0 # replace NAs with zero

farmland_reliance$reliance_larval <- as.numeric(farmland_reliance$reliance_larval) # convert reliance_larval column from character to numeric

riskbutt$Ra = with(farmland_reliance,reliance_adult[match(riskbutt$species,species)]) # adds Ra into df 'risk'
riskbutt$Rl = with(farmland_reliance,reliance_larval[match(riskbutt$species,species)]) # adds Rl into df 'risk'

# Risk ----

# Risk score = (( riskbutt$Pta + riskbutt$Fta ) / riskbutt$Ra) + (( riskbutt$Ptl + riskbutt$Ftl ) / riskbutt$Rl)

riskbutt$risk_score <- (( riskbutt$Pta + riskbutt$Fta ) / riskbutt$Ra) + (( riskbutt$Ptl + riskbutt$Ftl ) / riskbutt$Rl)






# ----
## attempts to calculate A ----

# attempt 3
which(Bombus_sylvarum$hedge %in% 1) # these give the indices for the 1s in hedge column of sy
# now ask whether these locations in forage plants df are 1 as well
identical(names(Bombus_sylvarum[[1]]), names(forage_plant_location[[1]]) )

length(which(forage_plant_location$hedge[which(Bombus_sylvarum$hedge %in% 1)]==1))
length(which(forage_plant_location$margin[which(Bombus_sylvarum$margin %in% 1)]==1))
length(which(forage_plant_location$arablefields[which(Bombus_sylvarum$arablefields %in% 1)]==1))
length(which(forage_plant_location$grassfields[which(Bombus_sylvarum$grassfields %in% 1)]==1))


risk$G = with(activity,generations[match(risk$species,species)]) # G = total number of life cycle components (i.e. sum of the number of generations in all activity periods)
# Gt = number of generations of a species active in the activity periods affected
#apply(activity[Bombus_sylvarum, match(aut_sow[-1][which(colSums(aut_sow[-1])==-1)],names(activity)),drop=FALSE], 1, sum)
# which columns in aut_sow have -1 in them?
colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)] # finds the column names (apart from the first column) that contain a -1 anywhere in them
# now use these columns to see if Bombus_sylvarum is active in those periods 
# (i.e. if the row for Bombus_sylvarum has 1s in those columns)
affected <- match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity)) # finds the column index/indices in 'activity' that match column/s containing -1 in 'aut_sow'
#4,5


# do it by summing?
activity$A <- activity[,affected[1]] + activity[,affected[2]] + activity[,affected[3]]
#data$col3 <- data$col1 + data$col2

# need to loop the following through the species in 'activity' df.
length(which(activity[1,match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity)) ] ==1)==TRUE) # this just does it for the first row of 'activity'



# this adds an activity column but it's adding the Bombus sylvarum result to every row
activity$A4 <- by(activity, 1:nrow(activity), function(row) length(activity[1,match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity)) ] ==1))

activity[1]



spp <- list(activity$species)
#risk$species <- as.factor(risk$species)
#str(risk)

result <- data.frame(matrix(nrow = length(activity$species), ncol = 2))
colnames(result) <- c("spp", "A")


# this is adding the Bombus sylvarum result to every row:
for (i in 1:nrow(activity)) {
  activity$A <- length(activity[i,match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity)) ] ==1)
  #result[i, 1] <- i
  #result[i, 2] <- A
}



for (i in 1:nrow(activity)) {
  activity$A5 <- length(which(activity[1,match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity)) ] ==1)==TRUE)
  #result[i, 1] <- i
  #result[i, 2] <- A
}



# > d
#name plate value1 value2
#1    A    P1      1    100
#2    B    P2      2    200
#3    C    P3      3    300
#species early mid late
# B_sy       0   1    1 

#f <- function(x, output) {
#  wellName <- x[1]
#  plateName <- x[2]
#  wellID <- 1
#  print(paste(wellID, x[3], x[4], sep=","))
#  cat(paste(wellID, x[3], x[4], sep=","), file= output, append = T, fill = T)
#}
#apply(activity, 1, f, output = 'outputfile')

rm(f)
f <- function(x) {
  #species <- x[1]
  A <- 1
  print(length(which(activity[,match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity)) ] ==1)==TRUE) )
  #cat(length(x[,match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(x)) ] ==1), file= output, append = T, fill = T)
}
apply(activity, 1, f) # 1 applies over rows, 2 applies over columns


sapply(activity, f, output = 'outputfile')
sapply(activity, f)

sapply(activity, function(row) length(activity[match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity)) ] ==1))
# b1$q <- sapply(b1$Ac, function(x){prod((R - x - Sfi) / (R - Sfi))})
sapply(activity[3:5], function(x){length(activity[match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity)) ] ==1)})


# this adds an activity column but it gives the value 28 to every row
for (i in 1:nrow(activity)) { activity$A <- length(which(activity[i,match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity)) ] ==1)==TRUE)}





# attempt 2

# Gt = number of generations of a species active in the activity periods affected
#activity$Gt = apply(activity[, match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity)),drop=FALSE], 1, sum)
# colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)] # finds the column names (apart from the first column) that contain a -1 anywhere in them
# match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity)) # finds the column index/indices in 'activity' that match column/s containing -1 in 'aut_sow'
# then apply the 'sum' function to those columns, summing across rows. 
# drop=FALSE is necessary because the apply function doesn't work on vectors (if only one column is returned by match, then it's a vector/it's one dimension, rather than being a matrix)
# set drop= FALSE to disable conversion from a matrix to a vector in these circumstances.
#risk$Gt = with(activity,Gt[match(risk$species,species)]) 


aut_sow_fhab<-aut_sow[grep("fhab", aut_sow$resource),] # returns df with just the rows from the agricultural change df that relate to foraging hab


forageareas_Bsylv <- colnames(Bombus_sylvarum[-1])[which(colSums(Bombus_sylvarum[-1])>0)] # "hedge"       "margin"      "grassfields"

# need to add these to the risk spreadsheet for each spp
# and can then use them to see if there are any -1 values in rows containing those text strings in the aut_sow_fhab sheet

risk$forageareas <- c(as.factor(colnames(Bombus_distinguendus[-1])[which(colSums(Bombus_distinguendus[-1])>0)]),
                      colnames(Bombus_hortorum[-1])[which(colSums(Bombus_hortorum[-1])>0)],
                      colnames(Bombus_humilis[-1])[which(colSums(Bombus_humilis[-1])>0)],
                      colnames(Bombus_jonellus[-1])[which(colSums(Bombus_jonellus[-1])>0)],
                      colnames(Bombus_lapidarius[-1])[which(colSums(Bombus_lapidarius[-1])>0)],
                      colnames(Bombus_lucorum[-1])[which(colSums(Bombus_lucorum[-1])>0)],
                      colnames(Bombus_muscorum[-1])[which(colSums(Bombus_muscorum[-1])>0)],
                      colnames(Bombus_pascuorum[-1])[which(colSums(Bombus_pascuorum[-1])>0)],
                      colnames(Bombus_pratorum[-1])[which(colSums(Bombus_pratorum[-1])>0)],
                      colnames(Bombus_ruderarius[-1])[which(colSums(Bombus_ruderarius[-1])>0)],
                      colnames(Bombus_ruderatus[-1])[which(colSums(Bombus_ruderatus[-1])>0)],
                      colnames(Bombus_soroeensis[-1])[which(colSums(Bombus_soroeensis[-1])>0)],
                      colnames(Bombus_sylvarum[-1])[which(colSums(Bombus_sylvarum[-1])>0)],
                      colnames(Bombus_terrestris[-1])[which(colSums(Bombus_terrestris[-1])>0)]
)



which(colSums(Bombus_sylvarum[-1])>0)
#hedge      margin grassfields 
#1           2           4 

colnames(activity)[match(colnames(aut_sow_fhab[-1])[which(colSums(aut_sow_fhab[-1])==-1)],names(activity))]

#colnames(Bombus_sylvarum)[match(colnames(Bombus_sylvarum[-1])[which(colSums(Bombus_sylvarum[-1])>0)],names(aut_sow))]

# If forageact = 1, then look up which rows of aut_sow = 1 and then extract the name of the row (just the bit after "fhab_") 
which(apply(aut_sow_fhab, 1, function(r) any(r %in% -1)))


# attempt 1 


aut_sow_fhab<-aut_sow[grep("fhab", aut_sow$resource),] # returns df with just the rows from the agricultural change df that relate to foraging hab

# is an overlap between a sp's activity period and the foraging period affected by the agricultural change
#activity$forageact <- activity[, match(colnames(aut_sow_fhab[-1])[which(colSums(aut_sow_fhab[-1])==-1)],names(activity))] 

#activity$colindex <- ifelse(activity$forageact >0, 
#                            match(colnames(aut_sow_fhab[-1])[which(colSums(aut_sow_fhab[-1])==-1)],names(activity)), 
#                            0)

activity$lookup_col <- ifelse(activity[, match(colnames(aut_sow_fhab[-1])[which(colSums(aut_sow_fhab[-1])==-1)],names(activity))] >0, 
                              colnames(activity)[match(colnames(aut_sow_fhab[-1])[which(colSums(aut_sow_fhab[-1])==-1)],names(activity))], 
                              "NA")


# then look in that column in df 'aut_sow_fhab' and return rows where the value is -1
colnames(activity)[match(colnames(aut_sow_fhab[-1])[which(colSums(aut_sow_fhab[-1])==-1)],names(activity))]

aut_sow_fhab$resource[aut_sow_fhab$x==-1]




# If forageact = 1, then look up which rows of aut_sow = 1 and then extract the name of the row (just the bit after "fhab_") 
which(apply(aut_sow_fhab, 1, function(r) any(r %in% -1)))

# for each species, which period is affected?
# this gives a vector of column indeces (columns are 'early', 'mid' or 'late') in df 'activity' for the overlapping periods when a sp is active and when it forages
activity$colindex <- match(colnames(aut_sow_fhab[-1])[which(colSums(aut_sow_fhab[-1])==-1)],names(activity))


# then look in that column in df 'aut_sow_fhab' and return rows where the value is -1
colnames(activity)[match(colnames(aut_sow_fhab[-1])[which(colSums(aut_sow_fhab[-1])==-1)],names(activity))]


#affected <- match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity)) # finds the column index/indices in 'activity' that match column/s containing -1 in 'aut_sow'
#4,5


# do it by summing?
activity$A <- activity[,affected[1]] + activity[,affected[2]] + activity[,affected[3]]


activity %>% 
  filter_at(vars(colnames(activity)[match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity))]))#, 


# this isn't the right calculation for A - it is only part of it ----
activity <- activity %>%
  mutate(A = rowSums(.[ match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity)) ]))

risk$A <- with(activity,A[match(risk$species,species)]) ## add values of A into risk

risk$Ft <- risk$A / risk$F.


# NOTES ON HOW I DID STUFF ----
    # Gt = number of generations of a species active in the activity periods affected
#activity$Gt = apply(activity[, match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity)),drop=FALSE], 1, sum)
    # colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)] # finds the column names (apart from the first column) that contain a -1 anywhere in them
    # match(colnames(aut_sow[-1])[which(colSums(aut_sow[-1])==-1)],names(activity)) # finds the column index/indices in 'activity' that match column/s containing -1 in 'aut_sow'
    # then apply the 'sum' function to those columns, summing across rows. 
    # drop=FALSE is necessary because the apply function doesn't work on vectors (if only one column is returned by match, then it's a vector/it's one dimension, rather than being a matrix)
    # set drop= FALSE to disable conversion from a matrix to a vector in these circumstances.
#risk$Gt = with(activity,Gt[match(risk$species,species)]) 





###### works, but don't need as done it another way: ----
# same column names? ----
samecolnames <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      print('Warning: Names are not the same')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('Names are identical')
    }
  }
}
samecolnames(forage_plant_location[1:7],Thymelicus_lineola)

#colnames(Thymelicus_lineola)
#colnames(forage_plant_location)

#forage_plant_location <- forage_plant_location[order(forage_plant_location$plantfamily),] # order the df by plantfamily column  

#identical(names(forage_plant_location[1:7]), names(Thymelicus_lineola)) # checks whether column names are the same

# F ----

# here's how I worked out the solution to calculate F:

# I started with along-winded way:
riskbutt$Fa <- c(length(forage_plant_location[Ochlodes_sylvanus=="a" | Ochlodes_sylvanus=="a,l"]), #0
                 length(forage_plant_location[Thymelicus_lineola=="a" | Thymelicus_lineola=="a,l"]), #0
                 length(forage_plant_location[Thymelicus_sylvestris=="a" | Thymelicus_sylvestris=="a,l"]), #0
                 length(forage_plant_location[Lycaena_phlaeas=="a" | Lycaena_phlaeas=="a,l"]), #0
                 length(forage_plant_location[Celastrina_argiolus=="a" | Celastrina_argiolus=="a,l"]), #0
                 length(forage_plant_location[Polyommatus_icarus=="a" | Polyommatus_icarus=="a,l"]), #0
                 length(forage_plant_location[Satyrium_w_album=="a" | Satyrium_w_album=="a,l"]), #0
                 length(forage_plant_location[Thecla_betulae=="a" | Thecla_betulae=="a,l"]), #0
                 length(forage_plant_location[Aglais_io=="a" | Aglais_io=="a,l"]), #0
                 length(forage_plant_location[Aglais_urticae=="a" | Aglais_urticae=="a,l"]), #0
                 length(forage_plant_location[Euphydryas_aurina=="a" | Euphydryas_aurina=="a,l"]), #0
                 length(forage_plant_location[Polygonum_c_album=="a" | Polygonum_c_album=="a,l"]), #0
                 length(forage_plant_location[Aphantopus_hyperantus=="a" | Aphantopus_hyperantus=="a,l"]),
                 length(forage_plant_location[Coenonympha_pamphilus=="a" | Coenonympha_pamphilus=="a,l"]),
                 length(forage_plant_location[Lasiommata_megera=="a" | Lasiommata_megera=="a,l"]),
                 length(forage_plant_location[Maniola_jurtina=="a" | Maniola_jurtina=="a,l"]),
                 length(forage_plant_location[Melanargia_galathea=="a" | Melanargia_galathea=="a,l"]),
                 length(forage_plant_location[Pararge_aegeria=="a" | Pararge_aegeria=="a,l"]),
                 length(forage_plant_location[Pyronia_tithonus=="a" | Pyronia_tithonus=="a,l"]),
                 length(forage_plant_location[Gonepteryx_rhamni=="a" | Gonepteryx_rhamni=="a,l"]),
                 length(forage_plant_location[Anthocharis_cardamines=="a" | Anthocharis_cardamines=="a,l"]),
                 length(forage_plant_location[Pieris_brassicae=="a" | Pieris_brassicae=="a,l"]),
                 length(forage_plant_location[Pieris_napi=="a" | Pieris_napi=="a,l"]),
                 length(forage_plant_location[Pieris_rapae=="a" | Pieris_rapae=="a,l"])
                 )

# then I applied the function 'length(forage_plant_location[df=="a" | df=="a,l"])'
# to the list of butterfly dataframes I created manually
Fa <- lapply(df.list, function(x) length(forage_plant_location[x=="a" | x=="a,l"]))
# then I unlisted Fa to give a vector
unlist(Fa)
# and I can add this into the risk dataframe


# try to do F in a loop and make it foolproof (none of this worked): ----
# try 1

spp2 <- list(Bombus_spp_list$bbspecies)
df = NULL
for (k in 1:length(spp2)) {
  
  name = spp2[k]
  print(name)
  Sys.sleep(1) 
  F_bb <- length(forage_plant_location[spp2[[k]]==1])
  print(F_bb)
  Sys.sleep(1)
  #df = rbind(df, data.frame(name,F_bb))
  #Sys.sleep(1)
  
}


# try 2
str(risk)
risk$species <- as.factor(risk$species)
str(risk)

result <- data.frame(matrix(nrow = length(risk$species), ncol = 2))
colnames(result) <- c("spp", "F")
for (i in 1:length(spp)) {
  F_bb <- length(forage_plant_location[spp[[i]]==1])
  result[i, 1] <- i
  result[i, 2] <- F_bb
}
result


# try 3
#library(dplyr)

spp <- data.frame(species = as.factor(Bombus_spp_list$bbspecies))
spp <- spp %>% 
  mutate(species = factor(species, levels = c("Bombus_distinguendus",
                                              "Bombus_hortorum",
                                              "Bombus_humilis",
                                              "Bombus_jonellus",
                                              "Bombus_lapidarius",
                                              "Bombus_lucorum",
                                              "Bombus_muscorum",
                                              "Bombus_pascuorum",
                                              "Bombus_pratorum",
                                              "Bombus_ruderarius",
                                              "Bombus_ruderatus",
                                              "Bombus_soroeensis",
                                              "Bombus_sylvarum",
                                              "Bombus_terrestris")))



df = NULL
for(i in seq_along(levels(spp$species))){
  name = levels(spp$species)[i]
  #name = spp[i]
  print(name)
  Sys.sleep(1) 
  F_bb <- length(forage_plant_location[levels(spp$species)[i]==1])
  print(F_bb)
  Sys.sleep(1)
  #df = rbind(df, data.frame(name,F_bb))
  #Sys.sleep(1)
  
}


# try 4

#speciesid<-colnames(forage_plant_location)
spp3 <- Bombus_spp_list$bbspecies
for (k in 1:length(spp3)) {
  name = spp3[k]
  print(name)
  F_bb <- length(forage_plant_location[spp3[k]==1])
  print(F_bb)
}


# try 5
for (k in 1:length(spp3)) {
  name = spp3[k]
  
  print(name)
}

length(forage_plant_location[Bombus_sylvarum==1])


#df = NULL
#for (k in 1:length(spp)) {
#  
#  name = spp2[k]
#  print(name)
#  Sys.sleep(1) 
#  F_bb <- length(forage_plant_location[spp[k]==1])
#  print(F_bb)
#  Sys.sleep(1)
#  df = rbind(df, data.frame(name,F))
#  Sys.sleep(1)
#  
#}




for(i in 1:length(risk$species)){
  risk[i,F] <- length(forage_plant_location[i[,]==1])
  
}




# read all sheets of workbook into a list of dataframes using library readxl ----
suppressMessages(library(readxl))
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
rr <- read_excel_allsheets("C:/Users/varah.a/Desktop/Documents/Black Grass Resistance Initiative/Biodiversity/Risk/resource requirements all spp.xlsx")
list2env(rr,envir=.GlobalEnv) # converts all the list objects into dataframes


