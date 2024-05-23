######################################################################
###--------- This script runs a pedigree model using blupf90
##---------------------------------------

ebv = list()

#------------------------------------------------------------------------------
####----------- 1. Male population
#------------------------------------------------------------------------------

# Prepare prediction dataset for seedlings
dataPed =   rbind(pedM,
                  data.frame(Ind = MaleDH@id,
                             Sire = MaleDH@father,
                             Dam = MaleDH@mother,
                             Year = year,
                             Stage = rep("MaleDH",MaleDH@nInd),
                             Pheno = c(MaleDH@pheno[,1]))) 


# As factor
dataPed$Ind = as.factor(dataPed$Ind)
dataPed$Dam = as.factor(dataPed$Dam)
dataPed$Sire = as.factor(dataPed$Sire)
dataPed$Year = as.factor(dataPed$Year)
dataPed$Stage = as.factor(dataPed$Stage)

# Pedigree file
pedInf0 = dataPed[,c(1:3)]
pedInf = unique(pedInf0)

sireInf= data.frame(Ind = pedInf[,2],
                    Sire = 0,
                    Dam = 0)

damInf= data.frame(Ind = pedInf[,3],
                   Sire = 0,
                   Dam = 0)

# Cleaning
inf0 = rbind(sireInf, damInf)
inf1 = unique(inf0)

# Repeated entry
inf2 = inf1[!inf1$Ind %in% pedInf$Ind,]
rownames(inf2) = NULL

#Putting together
pedFinal = rbind(inf2, pedInf)

pedData = pedFinal[order(pedFinal$Ind),]
colnames(pedData) = NULL


#------------------------------------------------------------------------------
####----------- Run renumf90 and BLUf90 for male pop
#------------------------------------------------------------------------------


#-------------------- Run renumf90
dirCur = getwd()

# Creating the folder for pedfiles
if(!dir.exists("3.Pedfiles")){
  dir.create("3.Pedfiles")
}

setwd("3.Pedfiles")

# Folder for the reps and storage
if(!dir.exists(paste0("Folder_",rep))){
  dir.create(paste0("Folder_",rep))
}

setwd(paste0("Folder_",rep))

# Creating datasets
write.table(dataPed, file = "blupf90_male.txt", row.names = F, quote = F, col.names = F)
write.table(pedData, file = "pedData_male.txt", row.names = F, quote = F, col.names=F)

# Parameter file
prepare_parMale()

# renumf90
system(command = "echo blupf90_male.par | /apps/blupf90/20190531/bin/renumf90 | tee renum.log")


#--------------------- Run blupf90
system(command = "echo renf90.par | /apps/blupf90/20190531/bin/blupf90 | tee blup.log")


# Save the blups
ebv[[1]] <- getEBV()


# Back to root directory
setwd(dirCur)


#------------------------------------------------------------------------------
####-----------  For female population
#------------------------------------------------------------------------------

# Prepare prediction dataset for seedlings
datPedFem =   rbind(pedF,
                    data.frame(Ind = FemaleDH@id,
                               Sire = FemaleDH@father,
                               Dam = FemaleDH@mother,
                               Year = year,
                               Stage = rep("FemaleDH",FemaleDH@nInd),
                               Pheno = c(FemaleDH@pheno[,1])))


# Construct numerator relationship matrix A
datPedFem$Ind = as.factor(datPedFem$Ind)
datPedFem$Dam = as.factor(datPedFem$Dam)
datPedFem$Year = as.factor(datPedFem$Year)
datPedFem$Stage = as.factor(datPedFem$Stage)
datPedFem$Sire = as.factor(datPedFem$Sire)


#Unique model
tmp_naive = unique(datPedFem[,1:3])
tmp_naive2 = tmp_naive[order(tmp_naive$Ind),]


sireInf= data.frame(Ind = tmp_naive2[,2],
                    Sire = 0,
                    Dam = 0)

damInf= data.frame(Ind = tmp_naive2[,3],
                   Sire = 0,
                   Dam = 0)

# Cleaning
infPed = unique(rbind(sireInf, damInf))

# Repeated entry
inf2 = infPed[!infPed$Ind %in% tmp_naive2$Ind,]
rownames(inf2) = NULL

#Putting together
pedFinalFem = rbind(inf2, infPed)

pedDataFem = pedFinalFem[order(pedFinalFem$Ind),]
colnames(pedDataFem) = NULL


#------------------------------------------------------------------------------
####----------- Run renumf90 and BLUf90 for female pop
#------------------------------------------------------------------------------

#-------------------- Run renumf90
dirCur = getwd()

# Creating the folder for pedfiles
if(!dir.exists("3.Pedfiles")){
  dir.create("3.Pedfiles")
}

setwd("3.Pedfiles")

# Folder for the reps and storage
if(!dir.exists(paste0("Folder_",rep))){
  dir.create(paste0("Folder_",rep))
}

setwd(paste0("Folder_",rep))


# Creating datasets
write.table(datPedFem, file = "blupf90_female.txt", row.names = F, quote = F, col.names = F)
write.table(pedDataFem, file = "pedData_female.txt", row.names = F, quote = F, col.names=F)

# Parameter file
prepare_parFemale()

# renumf90
system(command = "echo blupf90_female.par | /apps/blupf90/20190531/bin/renumf90 | tee renum.log")


#--------------------- Run blupf90
system(command = "echo renf90.par | /apps/blupf90/20190531/bin/blupf90 | tee blup.log")

ebv[[2]] <- getEBV()

# Back to root directory
setwd(dirCur)
