################################################################
###########----------- Mating program -------------
################################################################
require("SimpleMating")

#------------------------------------------------------------------------------
####----------- 1. Population map for recomb Matrix
#------------------------------------------------------------------------------
#----------
# Markers map
#---------- 

snp_pos1 = getSnpMap(snpChip=1, simParam=SP)
snp_pos = snp_pos1[,c(2, 4, 1)]; colnames(snp_pos) = c("chr","pos",'mkr')
snp_pos$pos = snp_pos$pos*100; rownames(snp_pos) = NULL


#----
map = data.frame(CHROMOSOME=as.integer(snp_pos$chr),
                 POSITION=snp_pos$pos,
                 MARKER=snp_pos$mkr)



########>>>>>>>--------------------
########> FOR MALE CANDIDATES
########>>>>>>>--------------------

#------------------------------------------------------------------------------
####----------- 1. Additive effects
#------------------------------------------------------------------------------
# 1.1 Access SNP genotype data
Markers = pullSnpGeno(MaleParents)

MaleParents = setEBV(MaleParents, gsmodel)

addEff = gsmodel@gv[[1]]@addEff


#------------------------------------------------------------------------------
####----------- 2. Relationship matrix
#------------------------------------------------------------------------------
# 3.1 Potential parents - SNPs
relMatMale = Gmatrix(Markers)

#------------------------------------------------------------------------------
####----------- 4. Usefulness
#------------------------------------------------------------------------------
# 4.1 Plan
plan = planCross(TargetPop=colnames(relMatMale),
                 MateDesign = 'half')

# 4.2 Model
Usef_plan = getUsefA(MatePlan = plan,
                    Markers =  Markers,
                    addEff = addEff,
                    Map.In = map,
                    K = relMatMale,
                    propSel = 0.05,
                    Type = 'DH',
                    Generation = 1)


#------------------------------------------------------------------------------
####----------- 5. Criterion
#------------------------------------------------------------------------------
# 5.1 Crosses selected
maxGainPlan = selectCrosses(data = Usef_plan[[2]], 
                            n.cross = 40,
                            max.cross = 3,
                            min.cross = 1,
                            culling.pairwise.k = 0,
                            max.cross.to.search = 1e+10) 



# 5.2 Mating plan
opt.planM1 = maxGainPlan[[2]]
opt.planM = as.matrix(opt.planM1[,c(1,2)])

########>>>>>>>--------------------
########> FOR FEMALE CANDIDATES
########>>>>>>>--------------------

#------------------------------------------------------------------------------
####----------- 1. Additive effects
#------------------------------------------------------------------------------
# 1.1 Access SNP genotype data
Markers_female = pullSnpGeno(FemaleParents)

FemaleParents = setEBV(FemaleParents, gsmodelF)

addEff_F = as.vector(gsmodelF@gv[[1]]@addEff)


#------------------------------------------------------------------------------
####----------- 2. Relationship matrix
#------------------------------------------------------------------------------

# 3.1 Potential parents
relMatF = Gmatrix(Markers_female)

#------------------------------------------------------------------------------
####----------- 3. Usefulness
#------------------------------------------------------------------------------

# 3.1 Plan
planF = planCross(TargetPop=colnames(relMatF),
                  MateDesign = 'half')


# 3.2 Model
Usef_planF = getUsefA(MatePlan = planF,
                   Markers =  Markers_female,
                   addEff = addEff_F,
                   Map.In = map,
                   K = relMatF,
                   propSel = 0.05,
                   Type = 'DH',
                   Generation = 1)

#------------------------------------------------------------------------------
####----------- 4. Criterion
#------------------------------------------------------------------------------
# 4.1 Crosses selected
maxGainPlan = selectCrosses(data = Usef_planF[[2]], 
                            n.cross = 40,
                            max.cross = 3, 
                            min.cross = 1,
                            culling.pairwise.k = 0,
                            max.cross.to.search = 1e+10) 

# 4.2 Mating plan
opt.planF1 = maxGainPlan[[2]]
opt.planF = as.matrix(opt.planF1[,c(1,2)])




