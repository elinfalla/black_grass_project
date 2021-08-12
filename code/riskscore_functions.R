
## FUNCTION USED BY ALL TAXA'S RISK SCORE SCRIPTS IN ORDER TO CHECK THE COLUMNS OF 
## THE MANAGEMENT CHANGE AND SPECIES' REQUIREMENT SPREADSHEETS ALIGN (I.E. ARE THE SAME)
## SO THAT OVERLAP CALCULATIONS ARE CORRECT

check_aligned <- function(data1_cols, data2_cols) {
  
  aligned <- identical(data1_cols, data2_cols)
  
  if (aligned == F) {
    warning("Columns of management change and species' requirements datasets are not aligned - calculations may not be correct.")
  }
}

## FUNCTION TO REPLACE ALL NAs WITH 0s IN A DATASET

NA_to_zero <- function(dataset) {
  dataset <- dataset %>% 
    mutate_all(~replace(., is.na(.), 0))
  
  return(dataset)
}