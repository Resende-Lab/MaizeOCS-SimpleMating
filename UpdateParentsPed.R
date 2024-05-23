#------------------------------------------------------------------------------
####----------- Updating the parents
#------------------------------------------------------------------------------

###----  Model for parents using BLUPf90
source("RunningBLUPf90.R")


###---- For male parents
df = ebv[[1]][ebv[[1]]$V10 %in% MaleDH@id,]
dfMale2 = df[order(df$V10),] 
MaleDH@ebv = as.matrix(dfMale2$V4)

MaleParents = MaleDH

###---- For female parents
df = ebv[[2]][ebv[[2]]$V10 %in% FemaleDH@id,]
dfFemale2 = df[order(df$V10),] 
FemaleDH@ebv = as.matrix(dfFemale2$V4)

FemaleParents = FemaleDH



