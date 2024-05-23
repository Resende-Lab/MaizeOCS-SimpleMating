#####-----------------------------------------------
#Track population, hybrid and parents performances
#####-----------------------------------------------

###>>>-------- 1. Population
femPar = FemaleParents[FemaleParents@id %in% c(opt.planF[,1],opt.planF[,2])]
malePar = MaleParents[MaleParents@id %in% c(opt.planM[,1],opt.planM[,2])]

popname = c(malePar, femPar)

# Parameters
genPa = genParam(popname)

###>>>-------- 2. Paramenters
MeanG_pop[year] = genPa$mu
VarG_pop[year]  = genPa$varG
GenicVG_pop[year]  = genPa$genicVarG






