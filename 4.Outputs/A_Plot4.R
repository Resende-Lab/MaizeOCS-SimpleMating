################################################################################
##### Plotting the results of the paper:
##### SimpleMating:  R-package for prediction and optimization of breeding crosses using genomic selection


#======================================================
###>>>---- 1. Packages and environment
#======================================================
# Environment
rm(list=ls())
setwd("...") #*** Change to your work directory
dirD = getwd()

# Packages
require(dplyr)
library(data.table)
library(plyr)
library(ggplot2)
library(tidyverse)
require(ggpubr)

#======================================================
###>>>---- 2. Loading the result files
#======================================================

# files
files = list.files(paste0(dirD), pattern = 'Results_')

rawData = list()

# Loop
for(i in 1:length(files)){
  
  temp = data.frame(Year=1:35, readRDS(files[i]),
                    stringsAsFactors=FALSE)
  
  names(temp) <- c("Year","rep", "Scenario",
                   'MeanG_pop',
                   'VarG_pop',
                   'GenicVG_pop')
  
  rawData[[i]] = temp
}

rawData = rbindlist(rawData) #collapse all Scenarios into one data frame

# Changing to better plot the results
rawData$Year = rawData$Year-15
table(rawData$Scenario, rawData$rep)

# better names for scenarios
dfFinal$Scenario[dfFinal$Scenario == "OCS1"] <- "OCS"
dfFinal$Scenario[dfFinal$Scenario == "DH"] <- "PS"
dfFinal$Scenario[dfFinal$Scenario == "DHGS"] <- "GS"

#======================================================
###>>>---- Population mean
#======================================================
dfMean = dfFinal
dfMean = dfMean[dfMean$Year >= 0,]
dfMean$MeanG_pop = dfMean$MeanG_pop - dfMean$MeanG_pop[dfMean$Year == 0] 


#------------------ mean
PopMean = ddply(dfMean,c("Year","Scenario"), summarize,
                mean = mean(MeanG_pop),
                se = sd(MeanG_pop)/sqrt(20))

# Adding a nice title
PopMean$name = 'Genetic performance' 

#Plot order
PopMean$Scenario <- factor(PopMean$Scenario, levels = c("PS", "GS","OCS", "OCSped",  'MeanPAve'))


#Plot
Mean = ggplot(PopMean,aes(x=Year,y=mean,color=Scenario))+
  facet_grid(~name) +
  geom_line(aes(color=Scenario,linetype=Scenario),size=1)+
  geom_ribbon(aes(x=Year,ymin=mean-se,ymax=mean+se, fill=Scenario),alpha=0.05,linetype=0)+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank(),
        plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size=26),
        legend.position = c(0.1,0.85),
        legend.key = element_blank(),
        legend.key.width =   unit(3, 'cm'),
        axis.text.x =  element_text(size = 18,colour = "black"),
        axis.text.y.left = element_text(size = 18,colour = "black"),
        axis.text.y.right =  element_blank(),
        axis.title = element_text(size = 20, face = "bold"),
        strip.text = element_blank())+ #element_text(face = "bold", size = 38, colour = 'blue'))+
  scale_linetype_manual(values = c("longdash", "twodash",'twodash', "solid" ,'solid', 'solid'))+
  scale_color_manual(values = c('#CC9900','#FF3333','blue',   '#ACA4E2',"#E495A5"))+
  scale_fill_manual(values = c('#CC9900','#FF3333','blue',  '#ACA4E2', "#E495A5"))+
  guides(linetype = guide_legend(override.aes = list(size = 2)))+
  scale_x_continuous("Year",limits=c(0, 20))+
  scale_y_continuous("Genetic mean",expand = c(0, 0), limits = c(-1,NA))


#======================================================
###>>>---- Standardize graph
#======================================================
dfStd = dfFinal

# Standardizing the genetic mean
dfStd$MeanG_pop = scale(dfStd$MeanG_pop)

## standard deviation
PopMeanstd = ddply(dfStd,c("Year","Scenario"), summarize,
                   MeanG2 = sd(MeanG_pop),
                   se = sd(MeanG_pop)/sqrt(20))


# Adding a nice title
PopMeanstd$name = 'Genetic std' 

#Plot order
PopMeanstd$Scenario <- factor(PopMeanstd$Scenario, levels = c("PS", "GS","OCS", "OCSped",  'MeanPAve'))


#Plot
stdVar = ggplot(PopMeanstd,aes(x=Year,y=MeanG2,color=Scenario))+
  facet_grid(~name) +
  geom_line(aes(color=Scenario,linetype=Scenario),size=1)+
  geom_ribbon(aes(x=Year,ymin=MeanG2-se,ymax=MeanG2+se, fill=Scenario),alpha=0.05,linetype=0)+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank(),
        plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size=26),
        legend.position = c(0.1,0.85),
        legend.key = element_blank(),
        legend.key.width =   unit(3, 'cm'),
        axis.text.x =  element_text(size = 18,colour = "black"),
        axis.text.y.left = element_text(size = 18,colour = "black"),
        axis.text.y.right =  element_blank(),
        axis.title = element_text(size = 20, face = "bold"),
        strip.text = element_blank())+ #element_text(face = "bold", size = 38, colour = 'blue'))+
  scale_linetype_manual(values = c("longdash", "twodash",'twodash', "solid" ,'solid', 'solid'))+
  scale_color_manual(values = c('#CC9900','#FF3333','blue',   '#ACA4E2',"#E495A5", 'grey90'))+
  scale_fill_manual(values = c('#CC9900','#FF3333','blue',  '#ACA4E2', "#E495A5", 'grey90'))+
  guides(linetype = guide_legend(override.aes = list(size = 2)))+
  scale_x_continuous("Year",limits=c(0,20))+
  scale_y_continuous("Genetic standard deviation",expand = c(0, 0), limits = c(0.15,0.58))



#======================================================
###>>>---- Genic variance
#======================================================

#------------------ variance
PopGenVar = ddply(dfFinal,c("Year","Scenario"), summarize,
                  mean = mean(GenicVG_pop),
                  se = sd(GenicVG_pop)/sqrt(20))

# Adding a nice title
PopGenVar$Measure = 'Genic variance'

#Plot order
PopGenVar$Scenario <- factor(PopGenVar$Scenario, levels = c("PS", "GS","OCS", "OCSped",  'MeanPAve'))


# Plot
GenVar=ggplot(PopGenVar,aes(x=Year,y=mean,color=Scenario))+
  facet_grid(~Measure) +
  geom_line(aes(color=Scenario,linetype=Scenario),size=1)+
  geom_ribbon(aes(x=Year,ymin=mean-se,ymax=mean+se, fill=Scenario),alpha=0.05,linetype=0)+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_blank(),
        plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size=26),
        legend.position = c(0.8,0.85),
        legend.key.width =   unit(3, 'cm'),
        axis.text.x =  element_text(size = 18,colour = "black"),
        axis.text.y.left = element_text(size = 18,colour = "black"),
        axis.text.y.right =  element_blank(),
        axis.title = element_text(size = 20, face = "bold"),
        strip.text = element_blank())+#element_text(face = "bold", size = 38, colour = 'blue'))+
  scale_linetype_manual(values = c("longdash", "twodash",'twodash', "solid" ,'solid'))+
  scale_color_manual(values = c('#CC9900','#FF3333','blue',   '#ACA4E2',"#E495A5"))+
  scale_fill_manual(values = c('#CC9900','#FF3333','blue',   '#ACA4E2',"#E495A5"))+
  guides(linetype = guide_legend(override.aes = list(size = 2)))+
  scale_x_continuous("Year",limits=c(0,20))+
  scale_y_continuous("Genic variance",expand = c(0, 0), limits = c(11, 18))




# b
tiff("1.Figure_6.tiff", width = 18, height = 6, units = 'in', res = 300)
ggpubr::ggarrange(Mean,stdVar, GenVar,  # list of plots
                  labels = c('(A)', '(B)', '(C)'), # labels
                  font.label = list(size = 28, 
                                    color = "black", face = "bold", family = NULL),
                  common.legend = T, # COMMON LEGEND
                  legend = "bottom", # legend position
                  align = "hv", # Align them both, horizontal and vertical
                  heights = 2,
                  nrow = 1,
                  ncol = 3)  # number of rows
dev.off()