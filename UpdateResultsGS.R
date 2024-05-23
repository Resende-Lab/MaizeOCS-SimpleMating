#####-----------------------------------------------
#Track population, hybrid and parents performances
#####-----------------------------------------------

###>>>----- Populatin
popname = c(MaleParents, FemaleParents)
genPa = genParam(popname)

###>>>-------- 1. Paramenters
MeanG_pop[year] = genPa$mu
VarG_pop[year]  = genPa$varG
GenicVG_pop[year]  = genPa$genicVarG






