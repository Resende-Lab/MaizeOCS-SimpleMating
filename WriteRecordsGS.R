cat(" Writing records \n")
# NOTE: Training records are collected with a sliding-window process.
# Cumulation of the records starts in the burn-in year 'startTrainPop' for GS
# Once the burn-in period is over, the sliding-window process removes the oldest
# records.
# For pedigree-based model, we tracked the information since year 1.


######------------------------------------------------
##>>>>>  For GS
######------------------------------------------------

if(year >= startTrainPop){

######--------------------------------------------
##>>>>>  pull the snps from male heterotic group

  if (year == 13) {
    male_yt1 = MaleYT1
    male_yt1@fixEff <- as.integer(rep(paste0(year,1L),nInd(male_yt1)))

    male_yt2 = MaleYT2
    male_yt2@fixEff <- as.integer(rep(paste0(year,3L),nInd(male_yt2)))

    trainPop = c(male_yt1, male_yt2)

  } else if (year == 14 | year == 15 ) {
    male_yt1 = MaleYT1
    male_yt1@fixEff <- as.integer(rep(paste0(year,1L),nInd(male_yt1)))

    male_yt2 = MaleYT2
    male_yt2@fixEff <- as.integer(rep(paste0(year,3L),nInd(male_yt2)))

    trainPop = c(trainPop,male_yt1, male_yt2)

  } else {
    male_yt1 = MaleYT1
    male_yt1@fixEff <- as.integer(rep(paste0(year,1L),nInd(male_yt1)))

    male_yt2 = MaleYT2
    male_yt2@fixEff <- as.integer(rep(paste0(year,3L),nInd(male_yt2)))

    trainPop <- trainPop[-c(1:(male_yt1@nInd+male_yt2@nInd))]
    trainPop = c(trainPop, male_yt1, male_yt2)

  }

######--------------------------------------------
##>>>>>  pull the snps from female heterotic group



  if (year == 13) {
    female_yt1 = FemaleYT1
    female_yt1@fixEff <- as.integer(rep(paste0(year,1L),nInd(female_yt1)))

    female_yt2 = FemaleYT2
    female_yt2@fixEff <- as.integer(rep(paste0(year,3L),nInd(female_yt2)))

    trainPopF = c(female_yt1, female_yt2)

  } else if (year == 14 | year == 15 ) {
    female_yt1 = FemaleYT1
    female_yt1@fixEff <- as.integer(rep(paste0(year,1L),nInd(female_yt1)))

    female_yt2 = FemaleYT2
    female_yt2@fixEff <- as.integer(rep(paste0(year,3L),nInd(female_yt2)))

    trainPopF = c(trainPopF,female_yt1, female_yt2)

  } else {
    female_yt1 = FemaleYT1
    female_yt1@fixEff <- as.integer(rep(paste0(year,1L),nInd(female_yt1)))

    female_yt2 = FemaleYT2
    female_yt2@fixEff <- as.integer(rep(paste0(year,3L),nInd(female_yt2)))

    trainPopF <- trainPopF[-c(1:(female_yt1@nInd+female_yt2@nInd))]
    trainPopF = c(trainPopF, female_yt1, female_yt2)

  }

}

 

######------------------------------------------------
##>>>>>  For pedigree-based models
######------------------------------------------------

######------------------------------------------------
##>>>>>  pull the pedigree information from male heterotic group

  if(year==1){
    
    pedM1 = data.frame(Ind = c(MaleYT1@id),
                       Sire = c(MaleYT1@father),
                       Dam = c(MaleYT1@mother),
                       Year = year,
                       Stage = c(rep("MaleYT1",MaleYT1@nInd)),
                       Pheno = c(MaleYT1@pheno[,1]))
    
    pedM2 = data.frame(Ind = c(MaleF1@id),
                       Sire = c(MaleF1@father),
                       Dam = c(MaleF1@mother),
                       Year = year,
                       Stage = c(rep("MaleF1",MaleF1@nInd)),
                       Pheno = c(MaleF1@pheno[,1]))
    
    pedM = rbind(pedM1, pedM2)
    
  } else{
    
    pedM2 = data.frame(Ind = c(MaleYT1@id),
                       Sire = c(MaleYT1@father),
                       Dam = c(MaleYT1@mother),
                       Year = year,
                       Stage = c(rep("MaleYT1",MaleYT1@nInd)),
                       Pheno = c(MaleYT1@pheno[,1]))
    
    pedM1 = data.frame(Ind = c(MaleF1@id),
                       Sire = c(MaleF1@father),
                       Dam = c(MaleF1@mother),
                       Year = year,
                       Stage = c(rep("MaleF1",MaleF1@nInd)),
                       Pheno = c(MaleF1@pheno[,1]))
    
    pedM = rbind(pedM, pedM1, pedM2)
    
  }

######------------------------------------------------
##>>>>>  pull the pedigree information from female heterotic group

  if(year==1){
    
    pedF1 = data.frame(Ind = c(FemaleYT1@id),
                       Sire = c(FemaleYT1@father),
                       Dam = c(FemaleYT1@mother),
                       Year = year,
                       Stage = c(rep("FemaleYT1",FemaleYT1@nInd)),
                       Pheno = c(FemaleYT1@pheno[,1]))
    
    pedF2 = data.frame(Ind = c(FemaleF1@id),
                       Sire = c(FemaleF1@father),
                       Dam = c(FemaleF1@mother),
                       Year = year,
                       Stage = c(rep("FemaleF1",FemaleF1@nInd)),
                       Pheno = c(FemaleF1@pheno[,1]))
    
    pedF = rbind(pedF1, pedF2)
    
  } else {
    
    pedF1 = data.frame(Ind = c(FemaleYT1@id),
                       Sire = c(FemaleYT1@father),
                       Dam = c(FemaleYT1@mother),
                       Year = year,
                       Stage = c(rep("FemaleYT1",FemaleYT1@nInd)),
                       Pheno = c(FemaleYT1@pheno[,1]))
    
    pedF2 = data.frame(Ind = c(FemaleF1@id),
                       Sire = c(FemaleF1@father),
                       Dam = c(FemaleF1@mother),
                       Year = year,
                       Stage = c(rep("FemaleF1",FemaleF1@nInd)),
                       Pheno = c(FemaleF1@pheno[,1]))
    
    pedF = rbind(pedF, pedF1, pedF2)
  }


