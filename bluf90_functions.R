
prepare_parMale <- function() {
  sink("blupf90_male.par", type="output")
  writeLines("#blupf90 parametar file
DATAFILE
 blupf90_male.txt
TRAITS
 6
WEIGHT(S)

RESIDUAL_VARIANCE
 270
EFFECT
 4 cross numer
EFFECT
 1 cross numer
RANDOM
 animal
FILE
pedData_male.txt
FILE_POS
 1 2 3
(CO)VARIANCES
 20
OPTION sol se
OPTION saveG
")
  sink()
}



prepare_parFemale <- function() {
  sink("blupf90_female.par", type="output")
  writeLines("#blupf90 parametar file
DATAFILE
 blupf90_female.txt
TRAITS
 6
WEIGHT(S)

RESIDUAL_VARIANCE
 270
EFFECT
 4 cross numer
EFFECT
 1 cross numer
RANDOM
 animal
FILE
 pedData_female.txt                                                            
FILE_POS
 1 2 3
(CO)VARIANCES
 20
OPTION sol se
OPTION saveG
")
  sink()
}



getEBV <- function(){ 
  # Read 'solutions' file
  solutions_data <- read.table("solutions", skip = 1)
  
  filtered_solutions <- solutions_data[solutions_data$V2 == 2, c("V3", "V4")]
  
  # Sort the filtered solutions by the first column
  sorted_solutions <- filtered_solutions[order(filtered_solutions$V3), ]
  
  # Write the sorted solutions to 'sol.temp' file
  write.table(sorted_solutions, file = "sol.temp", quote = FALSE, row.names = FALSE, col.names = FALSE)
  
  # Read 'renadd02.ped' file, select the first and tenth columns
  ids_data <- read.table("renadd02.ped", header = FALSE)
  selected_ids <- ids_data[c(1, 10)]
  
  # Sort the selected columns by the first column
  sorted_ids <- selected_ids[order(selected_ids$V1), ]
  
  # Write the sorted selected columns to 'ids.temp' file
  write.table(sorted_ids, file = "ids.temp", quote = FALSE, row.names = FALSE, col.names = FALSE)
  
  # Join the 'ids.temp' and 'sol.temp' files on the first column
  renumbered_original_EB <- merge(sorted_ids, sorted_solutions, by.x = "V1", by.y = "V3")
  
  # Write the result to 'renumbered_original_EB' file
  write.table(renumbered_original_EB, file = "renumbered_original_EB", quote = FALSE, row.names = FALSE, col.names = FALSE)
  
  return(renumbered_original_EB)
}





