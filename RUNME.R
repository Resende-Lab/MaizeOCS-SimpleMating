#==================================================================================
# This represents an alternative to implementation of scenarios accounting for
# optimal cross selection via SimpleMating package
#
# This example was created based on Powell et al. (2021) and used to implement the
# simulation in the paper describing SimpleMating.
#
#
# Marco Antonio Peixoto
# deamorimpeixotom@ufl.edu
# https://marcopxt.github.io/
#


###>>>-------------------------------------------------
###> 1. Setting the base population and initial variables
###>>>-------------------------------------------------
rm(list=ls())

setwd("/blue/mresende/share/marcopxt/OCS/Hybrid/13DHOCS") #*** Change it here

require('AlphaSimR')
require('BGLR')
require('AGHmatrix')
require(SimpleMating)

options(echo=TRUE)
args = commandArgs(trailingOnly=TRUE)
rep <- as.numeric(args[1])

#---- 2. Setting the global parameters and creating files for results
# Load global parameters
source("GlobalParameters.R")

# Creating the files for record the results
MeanG_pop = matrix(NA, 35)
VarG_pop  = matrix(NA, 35)
GenicVG_pop = matrix(NA, 35)

#---- 3. Creating parents and filling the initial pipeline

# Create initial parents and set testers and hybrid parents
source("CreateParents.R")

# Fill breeding pipeline with unique individuals from initial parents
source("FillPipeline.R")

# p-values for GxY effects
P = runif(burninYears+futureYears)


###>>>-------------------------------------------------
###> 2. Burn-in period
###>>>-------------------------------------------------

# 2.1 Cycle years
  for(year in 1:burninYears){ #Change to any number of desired years
    cat("Working on year:",year,"\n")
    p = P[year]
    source("UpdateParents.R")  # Pick new parents based on last year's data
    source("UpdateTesters.R")  # Pick new testers and hybrid parents
    source("AdvancePheno.R")   # Advances yield trials by a year
    source("WriteRecordsGS.R") # Write records for GS predictions
    source("UpdateResults.R")  # Track summary data

  }

# 2.2 Save burn-in to load later use
save.image(paste0("BURNIN_",rep,".RData"))

###>>>-------------------------------------------------
###> 3. Scenario 1 - PS program
###>>>-------------------------------------------------
# 3.0 Loading the scenarios
load(paste0("BURNIN_",rep,".RData"))

# 3.1 Looping through the years

cat("Working on Scenario 1\n")
for(year in (burninYears+1):(burninYears+20)){

  cat("Working on year:",year,"\n")
  p = P[year]
  source("UpdateParents.R") #Pick new parents based on last year's data
  source("UpdateTesters.R") #Pick new testers and hybrid parents
  source("AdvancePheno.R") #Advances yield trials by a year
  source("UpdateResults.R") #Track summary data

}

# 3.2 Recording results
output1 = data.frame(rep=rep(rep, 35),
                     scenario=rep("DH", 35),
                     MeanG_pop,
                     VarG_pop,
                     GenicVG_pop,
                     stringsAsFactors=FALSE)

# 3.3 Saving the results as RDS
saveRDS(output1,paste0("Results_DH_",rep,".rds"))

###>>>-------------------------------------------------
###> 4. Scenario 2 - DHGS
###>>>-------------------------------------------------
# 4.0 Loading the scenarios
load(paste0("BURNIN_",rep,".RData"))

# 4.1 Looping through the years

cat("Working on Scenario 2\n")
for(year in (burninYears+1):(burninYears+20)){

  if(year==(burninYears+1)){

    gsmodel = RRBLUP(trainPop, use = 'pheno',useReps = TRUE )
    gsmodelF = RRBLUP(trainPopF, use = 'pheno',useReps = TRUE )

  }
  cat("Working on year:",year,"\n")

  p = P[year]
  source("UpdateParents_GS.R") #Pick new parents based on last year's data
  source("UpdateTesters.R") #Pick new testers and hybrid parents
  source("AdvanceDHGS.R") #Advances yield trials by a year
  source('WriteRecordsGS.R') # Fill data for next cycle GS models
  source("UpdateResultsGS.R") #Track summary data
}


# 4.2 Recording results
output2 = data.frame(rep=rep(rep, 35),
                     scenario=rep("DHGS", 35),
                     MeanG_pop,
                     VarG_pop,
                     GenicVG_pop,
                     stringsAsFactors=FALSE)

# 4.3 Saving the results as RDS
saveRDS(output2,paste0("Results_DHGS_",rep,".rds"))

###>>>-------------------------------------------------
###> 5. Scenario 3 DHGS with optimization on usefulness
###>>>-------------------------------------------------
# 5.0 Loading the scenarios
load(paste0("BURNIN_",rep,".RData"))

# 5.1 Looping through the years
cat("Working on Scenario 3\n")
for(year in (burninYears+1):(burninYears+20)){

  if(year==(burninYears+1)){
    gsmodel = RRBLUP(trainPop, use = 'pheno',useReps = TRUE )
    gsmodelF = RRBLUP(trainPopF, use = 'pheno',useReps = TRUE )

  }
  nPar = 3
  cat("Working on year:",year,"\n")
  p = P[year]
  source("UpdateParents_all.R")   #Pick new parents based on last year's data
  source("UpdateTesters.R")       #Pick new testers and hybrid parents
  source("AdvanceGSU.R")          #Advances yield trials by a year
  source('WriteRecordsGS.R')      #Fill data for next cycle GS models
  source("UpdateResultsMate.R")     #Track summary data
}


# 5.2 Recording results
output3 = data.frame(rep=rep(rep, 35),
                     scenario=rep("OCS1", 35),
                     MeanG_pop,
                     VarG_pop,
                     GenicVG_pop,
                     stringsAsFactors=FALSE)

# 5.3 Saving the results as RDS
saveRDS(output3,paste0("Results_OCS1_",rep,".rds"))


###>>>-------------------------------------------------
###> 6. Scenario 4 DHGS with optimization using mean parental average
###>>>-------------------------------------------------
# 6.0 Loading the scenarios
load(paste0("BURNIN_",rep,".RData"))

# 6.1 Looping through the years
cat("Working on Scenario 3\n")
for(year in (burninYears+1):(burninYears+20)){

  if(year==(burninYears+1)){
    gsmodel = RRBLUP(trainPop, use = 'pheno',useReps = TRUE)
    gsmodelF = RRBLUP(trainPopF, use = 'pheno',useReps = TRUE)

  }

  cat("Working on year:",year,"\n")
  p = P[year]
  source("UpdateParents_all.R")     #Pick new parents based on last year's data
  source("UpdateTesters.R")         #Pick new testers and hybrid parents
  source("AdvanceGSMPA.R")          #Advances yield trials by a year
  source('WriteRecordsGS.R')        #Fill data for next cycle GS models
  source("UpdateResultsMate.R")     #Track summary data
}

# 6.2 Recording results
output4 = data.frame(rep=rep(rep, 35),
                     scenario=rep("MeanPAve", 35),
                     MeanG_pop,
                     VarG_pop,
                     GenicVG_pop,
                     stringsAsFactors=FALSE)


# 6.3 Saving the results as RDS
saveRDS(output4,paste0("Results_MeanPAve_",rep,".rds"))

###>>>-------------------------------------------------
###> 7. Scenario 3 Pedigree with optimization
###>>>-------------------------------------------------

# 7.0 Loading the scenarios
load(paste0("BURNIN_",rep,".RData"))
source("bluf90_functions.R")

# 7.1 Looping through the years
cat("Working on Scenario 3\n")
for(year in (burninYears+1):(burninYears+20)){

  cat("Working on year:",year,"\n")

  p = P[year]
  source("UpdateParentsPed.R") #Pick new parents based on last year's data
  source("UpdateTesters.R")       #Pick new testers and hybrid parents
  source("AdvancePed.R")          #Advances yield trials by a year
  source('WriteRecordsGS.R')      #Fill data for next cycle GS models
  source("UpdateResultsMate.R")       #Track summary data

}

# 7.2 Recording results
output5 = data.frame(rep=rep(rep, 35),
                     scenario=rep("OCSped", 35),
                     MeanG_pop,
                     VarG_pop,
                     GenicVG_pop,
                     stringsAsFactors=FALSE)

# 7.3 Saving the results as RDS
saveRDS(output5,paste0("Results_OCSped_",rep,".rds"))

###>>>-------------------------------------------------
###> 6. Removing the temporary files
###>>>-------------------------------------------------

# # Delete tmp file
# file.remove(paste0("BURNIN_",rep,"_S.RData"))


