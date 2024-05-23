# Male parents
MaleDH = setEBV(MaleDH, gsmodel)
MaleParents = selectInd(MaleDH, nParents, use = 'ebv')

# Female parents
FemaleDH = setEBV(FemaleDH, gsmodelF)
FemaleParents = selectInd(FemaleDH, nParents, use = 'ebv')


