################################################################
###########----------- Mating program -------------
################################################################
require(AGHmatrix)

########>>>>>>>--------------------
########> FOR MALE CANDIDATES
########>>>>>>>--------------------

###------------------------------------------------------------------------
###              Construct pedigree for male
###------------------------------------------------------------------------

##>>>-----------  Pedigree
# Generating the information for the remaining parents
# Getting the information
pedM.tmp =   rbind(pedM,
                   data.frame(Ind = MaleDH@id,
                              Sire = MaleDH@father,
                              Dam = MaleDH@mother,
                              Year = year,
                              Stage = rep("MaleDH",MaleDH@nInd),
                              Pheno = c(MaleDH@pheno[,1])))


pedInf0 = pedM.tmp[,c(1:3)]
pedInf = unique(pedInf0)

sireInf= data.frame(Ind = pedInf$Sire,
                    Sire = 0,
                    Dam = 0)

damInf= data.frame(Ind = pedInf$Dam,
                   Sire = 0,
                   Dam = 0)

# Cleaning
inf0 = rbind(sireInf, damInf)
inf1 = unique(inf0)

# Repeated entry
inf2 = inf1[!inf1$Ind %in% pedInf$Ind,]

#Putting together
ped_final = rbind(inf2, pedInf)
tmp_naive2 = ped_final[order(ped_final$Ind),]

# Matrix
AMat.M = Amatrix(tmp_naive2)

#------------------------------------------------------------------------------
####----------- 1. Criteria for selection
#------------------------------------------------------------------------------
# Criteria
CriterionM = data.frame(ID = MaleParents@id,
                        Criterion = MaleParents@ebv)

# For males v
Indiv2keep = MaleParents@id

# Cutting the matrix for account only genotypes in the relationship matrix
AMat_male = AMat.M[rownames(AMat.M)%in%Indiv2keep,
                   colnames(AMat.M)%in%Indiv2keep]  

#------------------------------------------------------------------------------
####----------- 2. Plan crosses
#------------------------------------------------------------------------------

MatePlan = planCross(TargetPop = colnames(AMat_male),
                     MateDesign = 'half')

all_cross = getMPA(MatePlan = MatePlan,
                   Criterion = CriterionM,
                   K = AMat_male)

#------------------------------------------------------------------------------
####----------- 3. Optimization
#------------------------------------------------------------------------------
# 5.1 Crosses selected
maxGainPlan = selectCrosses(data = all_cross,
                            n.cross = 40,
                            max.cross = 3,
                            min.cross = 1,
                            culling.pairwise.k = 1.75, 
                            max.cross.to.search = 1e+10)

# 5.2 Mating plan
OCSPlan1 = maxGainPlan[[2]]
opt.planM = as.matrix(OCSPlan1[,c(1,2)])



########>>>>>>>--------------------
########> FOR FEMALE CANDIDATES
########>>>>>>>--------------------



###------------------------------------------------------------------------
###              Construct pedigree for female
###------------------------------------------------------------------------

##>>>-----------  Pedigree
# Generating the information for the remaining parents

pedF.tmp =   rbind(pedF,
                   data.frame(Ind = FemaleDH@id,
                              Sire = FemaleDH@father,
                              Dam = FemaleDH@mother,
                              Year = year,
                              Stage = rep("FemaleDH",FemaleDH@nInd),
                              Pheno = c(FemaleDH@pheno[,1])))

pedInf0 = pedF.tmp[,c(1:3)]
pedInf = unique(pedInf0)


sireInf= data.frame(Ind = pedInf$Sire,
                    Sire = 0,
                    Dam = 0)

damInf= data.frame(Ind = pedInf$Dam,
                   Sire = 0,
                   Dam = 0)

# Cleaning
inf0 = rbind(sireInf, damInf)
inf1 = unique(inf0)

# Repeated entry
inf2 = inf1[!inf1$Ind %in% pedInf$Ind,]

#Putting together and organizing
ped_final = rbind(inf2, pedInf)
tmp_naive2 = ped_final[order(ped_final$Ind),]

# Matrix
AMat.F=Amatrix(tmp_naive2)


#------------------------------------------------------------------------------
####----------- 1. Criteria for selection and individuals rel matrix
#------------------------------------------------------------------------------
# Criteria
CriterionFe =data.frame(ID = FemaleParents@id,
                        Criterion = FemaleParents@ebv)


# For females AMat
Indiv2keep = FemaleParents@id

# Cutting the matrix for account only genotypes in the relationship matrix
AMat_female = AMat.F[rownames(AMat.F)%in%Indiv2keep,
                     colnames(AMat.F)%in%Indiv2keep]  

#------------------------------------------------------------------------------
####----------- 2. Plan crosses and estimating MPA
#------------------------------------------------------------------------------

MatePlanFe = planCross(TargetPop = colnames(AMat_female),
                       MateDesign = 'half')


# Estimating MPA
planMate = getMPA(MatePlan = MatePlanFe,
                  Criterion = CriterionFe,
                  K = AMat_female)

#------------------------------------------------------------------------------
####----------- 3. Optimization
#------------------------------------------------------------------------------
# 5.1 Crosses selected
maxGainPlan = selectCrosses(data = planMate,
                            n.cross = 40,
                            max.cross = 3, 
                            min.cross = 1,
                            culling.pairwise.k = 1.75, 
                            max.cross.to.search = 1e+10) 

# 5.2 Mating plan
OCSPlan1 = maxGainPlan[[2]]
opt.planF = as.matrix(OCSPlan1[,c(1,2)])



