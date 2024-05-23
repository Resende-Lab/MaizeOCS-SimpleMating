
########>>>>>>>--------------------
########> FOR MALE CANDIDATES
########>>>>>>>--------------------

#------------------------------------------------------------------------------
####----------- 1. Additive effects
#------------------------------------------------------------------------------
# 1.1 Access SNP genotype data
Markers = pullSnpGeno(MaleParents)

MaleParents = setEBV(MaleParents, gsmodel)

#------------------------------------------------------------------------------
####----------- 2. Thinning
#------------------------------------------------------------------------------
# 3.1 Potential parents - SNPs
relMat = Gmatrix(Markers)

#------------------------------------------------------------------------------
####----------- 3. Mean Parental average
#------------------------------------------------------------------------------
# 3.1 Plan
CrossPlan = planCross(TargetPop=MaleParents@id[1:100],
                     MateDesign = 'half')

Crit = data.frame(Id = MaleParents@id,
                  Crit = MaleParents@ebv)

# 3.2 Single trait mean parental average
ST_mpa = getMPA(MatePlan = CrossPlan,
                Criterion = Crit,
                K=relMat)

#------------------------------------------------------------------------------
####----------- 4. Criterion
#------------------------------------------------------------------------------
# 4.1 Crosses selected
maxGainPlan = selectCrosses(data = ST_mpa, 
                            n.cross = 40,
                            max.cross = 3, 
                            min.cross = 1,
                            culling.pairwise.k = 0.5,
                            max.cross.to.search = 1e+10) 

# 4.2 Mating plan
opt.planM1 = maxGainPlan[[2]]
opt.planM = as.matrix(opt.planM1[,c(1,2)])


########>>>>>>>--------------------
########> FOR FEMALE CANDIDATES
########>>>>>>>--------------------

#------------------------------------------------------------------------------
####----------- 1. Additive effects
#------------------------------------------------------------------------------
# 1.1 Access SNP genotype data
MarkersF = pullSnpGeno(FemaleParents)

FemaleParents = setEBV(FemaleParents, gsmodelF)

#------------------------------------------------------------------------------
####----------- 2. Thinning
#------------------------------------------------------------------------------
# 3.1 Potential parents - SNPs
relMatF = Gmatrix(MarkersF)

#------------------------------------------------------------------------------
####----------- 3. Mean Parental average
#------------------------------------------------------------------------------
# 3.1 Plan
CrossPlanF = planCross(TargetPop=FemaleParents@id,
                       MateDesign = 'half')

CritF = data.frame(Id = FemaleParents@id,
                  Crit = FemaleParents@ebv)

# 3.2 Single trait mean parental average
FE_mpa = getMPA(MatePlan = CrossPlanF,
                Criterion = CritF,
                K=relMatF)

#------------------------------------------------------------------------------
####----------- 4. Criterion
#------------------------------------------------------------------------------
# 4.1 Crosses selected
maxGainPlan = selectCrosses(data = FE_mpa, 
                            n.cross = 40,
                            max.cross = 3, 
                            min.cross = 1,
                            culling.pairwise.k = 0.5,
                            max.cross.to.search = 1e+10) 

# 4.2 Mating plan
opt.planF1 = maxGainPlan[[2]]
opt.planF = as.matrix(opt.planF1[,c(1,2)])

