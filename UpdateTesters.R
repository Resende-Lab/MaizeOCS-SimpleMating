# Update the testers for the new cycle
#

# 1. Replace oldest hybrid parent with parent of best hybrid from YT6

# Male
bestHybridM = selectInd(MaleHybridYT5, 1)
bestMaleInbred = bestHybridM@mother

MaleElite = c(MaleElite[-1],MaleInbredYT5[bestMaleInbred])

# Female
bestHybridF = selectInd(FemaleHybridYT5, 1)
bestFemaleInbred = bestHybridF@mother

FemaleElite = c(FemaleElite[-1],FemaleInbredYT5[bestFemaleInbred])


# 2. Update testers
MaleTester1 = MaleElite[1:nTester1]
FemaleTester1 = FemaleElite[1:nTester1]

MaleTester2 = MaleElite[1:nTester2]
FemaleTester2 = FemaleElite[1:nTester2]



