


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### Extending assingment inference to colormap visualizations
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ####


#This document is in the process of being cleaned/tidied up.


# Load in packages --------------------------------------------------------

library(tidyverse)
library(psych)
library(dplyr)
library(lmSupport)
library(stringr)
library(ggplot2)
library(reshape2)
library(lme4)
library(psych)
library(car)
library(aod)
library(lmerTest)





# EXPERIMENT 1 - COLORMAPS INTERPRETATIONS --------------------------------



dall2 = dmap1

#get averages with error bars
dsumPlot3 <- dall2 %>%
  group_by(conceptStr, assocDiffStr) %>%
  summarise(
    sd = sd(propDark, na.rm = TRUE),
    meanPropDark = mean(propDark),
    N = length(subjectID),
    se = sd/sqrt(N)
  )
dsumPlot3

#plot as high vs. low assoc diff maps for shade vs. sunshine with error bars
plot1b  = ggplot(dsumPlot3, aes(x = assocDiffStr, y = meanPropDark, color = conceptStr)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = meanPropDark-se, ymax = meanPropDark+se), width = .3, size = .5) + 
  labs(title = "selecting darker side",
       x = "assoc Difference group",
       y = "mean Prop. select dark side") +
  ylim(0, 1)
plot1b



#Get averages for each individual colormap
dsumPlot2 <- dall2 %>%
  group_by(conceptStr, meanAssocDiff, ID, lightrgb, darkrgb) %>%
  summarise(
    meanPropDark = mean(propDark),
  )
dsumPlot2



#Get averages for each individual colormap
dsumPlot2 <- dall2 %>%
  group_by(conceptStr, meanAssocDiff, ID, lightrgb, darkrgb) %>%
  summarise(
    sd = sd(propDark, na.rm = TRUE),
    meanPropDark = mean(propDark),
    N = length(subjectID),
    se = sd/sqrt(N)
  )
dsumPlot2


#do some sorting and organizing so that have the correct colors for plotting
map_Set = dsumPlot2 %>% arrange(ID)
mapSun = map_Set[map_Set$conceptStr == "sunshine",]
mapShade = map_Set[map_Set$conceptStr == "shade",]
mapSun2 = mapSun %>% arrange(ID)
mapShade2 = mapShade %>% arrange(ID)
combo = rbind(mapSun2, mapShade2)
lightrgbArray_combo <- dplyr::pull(combo, lightrgb)
darkrgbArray_combo <- dplyr::pull(combo, darkrgb)


#plot as continuous  assoc diff for shade (triangles) vs. sunshine (circles) 
plot3 = ggplot() +
  geom_errorbar(data = mapSun, aes(x = meanAssocDiff, y = meanPropDark, ymin = meanPropDark-se, ymax = meanPropDark+se), width = .1, size = .5) + 
  geom_errorbar(data = mapShade, aes(x = meanAssocDiff, y = meanPropDark, ymin = meanPropDark-se, ymax = meanPropDark+se), width = .1, size = .5) + 
  scale_fill_manual(values= lightrgbArray_combo) + 
  geom_point(data = mapSun, aes(x = meanAssocDiff, y = meanPropDark,  color=factor(ID), fill = factor(ID)), shape=21, size = 3, stroke = 3) +  
  geom_point(data = mapShade , aes(x = meanAssocDiff, y = meanPropDark, color=factor(ID), fill = factor(ID)), shape=24, size = 3, stroke = 3) +  
  scale_colour_manual(values= darkrgbArray_combo) +
  ylim(0,1) +
  xlim(0,1) +
  coord_fixed()+
  labs(title = "selecting darker side",
       x = "mean assoc difference",
       y = "mean Prop. select dark side") 
plot3


#plot as continuous  assoc diff for shade (triangles) vs. sunshine (circles) 
plot3 = ggplot() +
  geom_point(data = mapSun, aes(x = meanAssocDiff, y = meanPropDark,  color=factor(ID), fill = factor(ID)), shape=21, size = 3, stroke = 3) +  
  geom_point(data = mapShade , aes(x = meanAssocDiff, y = meanPropDark, color=factor(ID), fill = factor(ID)), shape=24, size = 3, stroke = 3) +  
  scale_fill_manual(values= lightrgbArray_combo) + 
  scale_colour_manual(values= darkrgbArray_combo) +
  ylim(0,1) +
  xlim(0,1) +
  coord_fixed()+
  labs(title = "selecting darker side",
       x = "mean assoc difference",
       y = "mean Prop. select dark side") 
plot3


varDescribeBy(dall2$propDark, dall2$conditionProto)

# plot with respect to selecting the LEFT side ----------------------------

d2$darkSideC =varRecode(d2$darkSide, c(0,1), c(-.5, .5))
d2$darkSideC = as.factor(d2$darkSideC)
d2$signedMeanAssocDiff = ifelse(d2$prompt == "shade", d2$diffRatingShade2, -1*d2$diffRatingSun)

d2$AssociatedSide = ifelse(d2$prompt == "shade" & d2$darkSide == 0, 0,
                           ifelse(d2$prompt == "shade" & d2$darkSide == 1, 1, 
                                  ifelse(d2$prompt == "sunshine" & d2$darkSide == 1, 0, 
                                         ifelse(d2$prompt == "sunshine" & d2$darkSide == 0, 1, 999))))

#multiple the association differences (both currenlty positive) by whether the more associated side was the left vs.right
d2$sideAssociationDiff = ifelse(d2$prompt == "shade", d2$diffRatingShade2*d2$associatedSide, 
                                ifelse(d2$prompt == "sunshine", d2$diffRatingSun*d2$associatedSide, 999))


# Analyses ----------------------------------------------------------------


# predicting whichSideChosen from assocDiffgroup X concept. 
# DV = whichSide
  #  propSelectedDark side  (lmer) [range: 0 to 1]
  # for a given trial, which side. (glmer) [0 or 1] 

dall$assocDiffC <- varRecode(dall$assocDiff, c(0,1), c(-.5,.5))
dall$conceptC <- varRecode(dall$concept, c(0,1), c(.5, -.5))
dall$darkSideC <- varRecode(dall$darkSide, c(0,1), c(-.5, .5))
dall$darkSideC2 = varRecode(dall$darkSide, c(0,1), c(-1, 1))

dall$meanAssocDiffC <- dall$meanAssocDiff - mean(dall$meanAssocDiff)


#new analysiss
dall$signedMeanAssocDiff = ifelse(dall$prompt == "shade", dall$diffRatingShade2, -1*dall$diffRatingSun)

dall$AssociatedSide = ifelse(dall$prompt == "shade" & dall$darkSide == 0, 0,
                           ifelse(dall$prompt == "shade" & dall$darkSide == 1, 1, 
                                  ifelse(dall$prompt == "sunshine" & dall$darkSide == 1, 0, 
                                         ifelse(dall$prompt == "sunshine" & dall$darkSide == 0, 1, 999))))

dall$AssociatedSideC = varRecode(dall$AssociatedSide, c(0,1), c(-1,1))

#multiple the association differences (both currenlty positive) by whether the more associated side was the left vs.right
dall$sideAssociationDiff = ifelse(dall$prompt == "shade", dall$diffRatingShade2*dall$AssociatedSideC, 
                                ifelse(dall$prompt == "sunshine", dall$diffRatingSun*dall$AssociatedSideC, 999))
dall$sideAssocDiffNorm = round(2*(((dall$sideAssociationDiff - min(dall$sideAssociationDiff))/(max(dall$sideAssociationDiff)-min(dall$sideAssociationDiff))))-1,4)




mNew2 <- glmer(sidePress ~ darkSideC2+sideAssocDiffNorm + (1|subjectID), data = dall, family = "binomial")
summary(mNew2)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        0.04257    0.07297   0.583     0.56    
#darkSideC2         1.32985    0.08601  15.461   <2e-16 ***
# sideAssocDiffNorm  4.51181    0.21267  21.215   <2e-16 ***



# EXPERIMENT 2 - COLORMAPS INTERPRETATIONS --------------------------------


#merge so that propDark appears with all the rest of the data
dall = merge(dsum, d2, by = "subjectID")
dall2 = dall[!duplicated(dall$subjectID),] #655 subjects


#Get averages for each individual colormap
dsumPlot2 <- dall2 %>%
  group_by(meanAssocDiff, SemDist, oldSemDist, ID, lightrgb, darkrgb) %>%
  summarise(
    sd = sd(propDark, na.rm = TRUE),
    meanPropDark = mean(propDark),
    N = length(subjectID),
    se = sd/sqrt(N)
  )
dsumPlot2

#correlation of new semantic distance and mean prop dark
cor(dsumPlot2$SemDist*-1, dsumPlot2$meanPropDark)
# -.93

corr.test(dsumPlot2$SemDist*-1, dsumPlot2$meanPropDark)
#r(16) = .93

#correlation of association difference and mean prop dark
cor(dsumPlot2$meanAssocDiff, dsumPlot2$meanPropDark)
#-.82


#do some sorting and organizing so that have the correct colors for plotting
mapSun = dsumPlot2 %>% arrange(ID)
lightrgbArray_combo <-  dplyr::pull(mapSun, lightrgb)
darkrgbArray_combo <- dplyr::pull(mapSun, darkrgb)


varDescribeBy(dall2$propDark, dall2$conditionProto)


#PLOT AS SIGNED SEMANTIC DISTANCE from endpoints (LOT SUN - NO SUN)
plotSDnew = ggplot() +
  geom_errorbar(data = mapSun, aes(x = SemDist*-1, y = meanPropDark, ymin = meanPropDark-se, ymax = meanPropDark+se), width = .06, size = .4) + 
  scale_fill_manual(values=  c("#d5b811", "#d0e942", "#fba714","#f7db7c","#fba714","#d0b85a","#aac510","#fcdb42",
                               "#4dc7e8", "#eddcad", "#39f6e0", "#dddddd", "#e6a8b7", "#dddddd", "#b9b9b9", "#b9b9b9"))+ 
  geom_point(data = mapSun, aes(x = SemDist*-1, y = meanPropDark, color=factor(ID), fill = factor(ID)), shape=21, size =4, stroke = 3) +  
  scale_colour_manual(values = c("#3b3b3b", "#777777", "#3b3b3b","#777777","#2e3086","#1c3d61","#5e2b3a","#077acc","#512d5f",
                                 "#a553c8","#777777","#3b8378","#184415","#0e8a19","#600b84","#72005e"))+ 
  ylim(0,1) +
  xlim(-1,.1) +
  coord_fixed()+
  labs(title = "selecting darker side - New SD",
       x = "Semantic distance (lot sun - no sun)",
       y = "mean Prop. select dark side") 
plotSDnew



#PLOT AS CONTINUOUS ASSOCIATION DIFFERENCE
plotAssoc = ggplot() +
  geom_errorbar(data = mapSun, aes(x = meanAssocDiff*-1, y = meanPropDark, ymin = meanPropDark-se, ymax = meanPropDark+se), width = .06, size = .4) + 
  scale_fill_manual(values=  c("#d5b811", "#d0e942", "#fba714","#f7db7c","#fba714","#d0b85a","#aac510","#fcdb42",
                               "#4dc7e8", "#eddcad", "#39f6e0", "#dddddd", "#e6a8b7", "#dddddd", "#b9b9b9", "#b9b9b9"))+ 
  geom_point(data = mapSun, aes(x = meanAssocDiff*-1, y = meanPropDark, color=factor(ID), fill = factor(ID)), shape=21, size =4, stroke = 3) +  
  scale_colour_manual(values = c("#3b3b3b", "#777777", "#3b3b3b","#777777","#2e3086","#1c3d61","#5e2b3a","#077acc","#512d5f",
                                 "#a553c8","#777777","#3b8378","#184415","#0e8a19","#600b84","#72005e"))+ 
  ylim(0,1) +
  xlim(-1,.1) +
  coord_fixed()+
  labs(title = "selecting darker side",
       x = "Sunshine assoc. difference",
       y = "mean Prop. select dark side") 
plotAssoc






# EXPERIMENT 3 - COLORMAPS INTERPRETATIONS --------------------------------

d4 = read.csv("environ-semanticDistances-all-matlabOutput-fullSet.csv") #fulll

d4 = read.csv("environ-semanticDistances-all-matlabOutput-trainSet2.csv")# train

d4 = read.csv("environ-semanticDistances-all-matlabOutput-testSet2.csv") #test



#add darkness 
dDarkness = read.csv("CEnviron-meanDarknessRatings-4.csv" )

d4 = merge(dDarkness, d4, by =c("xPair", "concept", "lightrgb", "darkrgb"))

#coded wrt to left vs. right
d4$semDistAssocSS = d4$semDistAssoc * d4$associatedSide
d4$semDistAssocSSC = d4$semDistAssocSS- mean(d4$semDistAssocSS)
d4$semDistOviSS = d4$SemDistOvi * d4$darkSideC3
d4$darknessSS = d4$meanDarkness * d4$darkSideC3 

#norm data to be between 1 and -1 for models
d4$semDistAssocSSNorm = round(2*(((d4$semDistAssocSS - min(d4$semDistAssocSS))/(max(d4$semDistAssocSS)-min(d4$semDistAssocSS))))-1,4)
d4$semDistOviSSNorm = round(2*(((d4$semDistOviSS - min(d4$semDistOviSS))/(max(d4$semDistOviSS)-min(d4$semDistOviSS))))-1,4)
d4$darknessSSNorm = round(2*(((d4$darknessSS - min(d4$darknessSS))/(max(d4$darknessSS)-min(d4$darknessSS))))-1,4)

#coded wrt to more associated color (lighter vs. darker)
d4$semDistAssocSignAssocSide = ifelse(d4$meanAssocDiff < 0, d4$semDistAssoc*-1, d4$semDistAssoc)


keep = c("subjectID", "concept", "xPair", "sidePress", "oviT", "oviTOuter", "oviTInner","seDark", "seLeft", "meanAssocDiff", "assocOuter1", "assocOuter2", "assocInner1", "assocInner2","meanPropLeft","meanPropDark", "darkSideC3","lightrgb", "darkrgb", "semDistAssocSSNorm", "semDistOviSSNorm", "semDistAssocSignAssocSide", "darknessSSNorm")
d5 = d4[keep]

dAllTrials = d5
dEachSide = d5 %>% distinct(concept, xPair, darkSideC3, .keep_all = T)
dEachXPair = d5 %>% distinct(concept, xPair, .keep_all = T)
dEachBanner = dEachXPair[dEachXPair$xPair == 262 | dEachXPair$xPair == 1019,]



#add darkness 
dDarkness = read.csv("CEnviron-meanDarknessRatings-4.csv" )

#load data file
dWeight = read.csv("weightPairsm1.csv")

#select data frame
dEachXPairComboC = dEachXPair %>% 
  select( subjectID, concept, xPair, meanPropDark,  semDistAssocSSNorm, seDark, oviT, oviTOuter, oviTInner, semDistOviSSNorm, assocOuter1, assocOuter2, assocInner1, assocInner2, lightrgb, darkrgb) %>%
  distinct(subjectID, concept, xPair, .keep_all = TRUE)

#merge 
dWeightsA = merge(dEachXPairComboC, dWeight)
dWeights = merge(dDarkness, dWeightsA, by =c("xPair", "concept"))

#get outer and inner weights
dWeights$Outer1m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocOuter1) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 1))
dWeights$Outer2m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocOuter2) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 1))
dWeights$Inner1m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocInner1) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 0))
dWeights$Inner2m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocInner2) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 0))
dWeights$comboSign = ifelse((dWeights$Inner1m1 + dWeights$Inner2m1 ) < (dWeights$Outer1m1 + dWeights$Outer2m1), 1,-1)


#divide weights by sum of the weights
dWeights$Outer1m1c = dWeights$Outer1m1 
dWeights$Outer2m1c = dWeights$Outer2m1
dWeights$Inner1m1c = dWeights$Inner1m1
dWeights$Inner2m1c = dWeights$Inner2m1

#write.csv(dWeights, "environ_semanticDistance_combo_MatlabInput-m1_weightCheck-trainSet-darkness2-noDivide.csv")


#load from matlab
dwc = read.csv("environ_semanticDistance_combo_MatlabOutput-m1_weightCheck-trainSet-darkness2-noDivide.csv")
dwc$semDistComboSign = dwc$semDistComboM1*dwc$comboSign

dwc5 = dwc
dwc5$darkSideWeight = dwc5$darkSideWeightm1 
dwc5$semDistComboM1 = dwc5$semDistComboSign

#linear comparison
dwc5$lineX = (dwc5$meanPropDark - .5)/.5
dwc5$se1 = (dwc5$lineX - dwc5$semDistComboM1)^2



weightsIce = dwc5[dwc5$concept == "ice",]
#Ice
plotIceWeights = ggplot() +
  geom_point(data = weightsIce, aes(x =semDistComboM1, y =meanPropDark), shape=19, size =2) +  
  theme(legend.position = "none")+
  ylim(0,1) +
  xlim(-1,1)+
  facet_wrap(~darkSideWeight)+
  geom_abline(intercept = .5, slope = .5)+
  coord_fixed()+
  labs(title = "weight pairs- mean prop dark-ice",
       x = "semDistCombo",
       y = "meanPropDark") 
plotIceWeights


weightsFire = dwc5[dwc5$concept == "fire",]
plotFireWeights = ggplot() +
  geom_point(data = weightsFire, aes(x =semDistComboM1, y =meanPropDark), shape=19, size =2) +  
  theme(legend.position = "none")+
  ylim(0,1) +
  xlim(-1,1)+
  facet_wrap(~darkSideWeight)+
  geom_abline(intercept = .5, slope = .5)+
  coord_fixed()+
  labs(title = "weight pairs- mean prop dark-fire",
       x = "semDistCombo",
       y = "meanPropDark") 
plotFireWeights

weightsWater = dwc5[dwc5$concept == "water",]
plotWaterWeights = ggplot() +
  geom_point(data = weightsWater, aes(x =semDistComboM1, y =meanPropDark), shape=19, size =2) +  
  theme(legend.position = "none")+
  ylim(0,1) +
  xlim(-1,1)+
  facet_wrap(~darkSideWeight)+
  geom_abline(intercept = .5, slope = .5)+
  coord_fixed()+
  labs(title = "weight pairs- mean prop dark-water",
       x = "semDistCombo",
       y = "meanPropDark") 
plotWaterWeights



#Group by concept and weightpair to get mse
dwc6 <- dwc5 %>%
  group_by(concept, darkSideWeight) %>%
  summarise(
    mse1 = mean(se1)
  )  
dwc7  = merge(dwc5, dwc6, by = c("concept","darkSideWeight"))


dwc7 = dwc7 %>% 
  select( concept, xPair,darkSideWeight, semDistComboM1, mse1) %>%
  distinct(concept, darkSideWeight,.keep_all = TRUE)

dwc7$darkSideWeight2 = as.numeric(dwc7$darkSideWeight)

dwc8<- dwc7 %>%
  group_by(darkSideWeight) %>%
  summarise(
    mseAve = mean(mse1)
  )  
dwc8$darkSideWeight2 = as.numeric(dwc8$darkSideWeight)

#all concepts
plotSdwc7allm1 = ggplot() +
  geom_point(data = dwc7, aes(x = darkSideWeight2, y =mse1, shape=factor(concept), color = factor(concept)), size =3, alpha = .3) +  
  geom_point(data = dwc8, aes(x = darkSideWeight2, y =mseAve), size =3, shape = 16) +  
  scale_shape_manual(values = c(17,15, 18))+
  scale_color_manual(values = c("red", "cyan", "blue"))+
  ylim(0,1) +
  xlim(0,1)+
  coord_fixed()+
  labs(title = "MSE for all concepts",
       x = "weight for darkSide  (weight for assoc SD = 1-darkSide)",
       y = "mean squared error") 
plotSdwc7allm1

#current weight on darkSIde = .30, with mse = .209
#new best = .30, mse = .19



#plot steps for 3 weight pairs of interest (0,.3,1)
weightSets = dwc5[dwc5$darkSideWeightm1 == 0 |  dwc5$darkSideWeightm1 == .3 |dwc5$darkSideWeightm1 == 1 ,]

#write.csv(weightSets, "trainSet.csv")
weightSets$lightrgb = as.character(weightSets$lightrgb_x)
weightSets$darkrgb = as.character(weightSets$darkrgb_x)

plotSetWeights = ggplot() +
  geom_point(data = weightSets, aes(x =semDistComboM1, y =meanPropDark), shape=19, size =2) +  
  theme(legend.position = "none")+
  ylim(0,1) +
  xlim(-1,1)+
  facet_wrap(~darkSideWeight)+
  geom_abline(intercept = .5, slope = .5)+
  coord_fixed()+
  labs(title = "weight pairs- mean prop dark",
       x = "semDistCombo",
       y = "meanPropDark") 
plotSetWeights





# Assignment inference: progressive weights  MODEL 1 TESTING -------------------------------


#add darkness 
dDarkness = read.csv("CEnviron-meanDarknessRatings-4.csv" )

#load data file
dWeight = read.csv("weightPairsm1testing.csv")

#select data frame
dEachXPairComboC = dEachXPair %>% 
  select( subjectID, concept, xPair, meanPropDark,  semDistAssocSSNorm, seDark, oviT, oviTOuter, oviTInner, semDistOviSSNorm, assocOuter1, assocOuter2, assocInner1, assocInner2, lightrgb, darkrgb) %>%
  distinct(subjectID, concept, xPair, .keep_all = TRUE)

#merge 
dWeightsA = merge(dEachXPairComboC, dWeight)
dWeights = merge(dDarkness, dWeightsA, by =c("xPair", "concept"))

#get outer and inner weights
dWeights$Outer1m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocOuter1) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 1))
dWeights$Outer2m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocOuter2) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 1))
dWeights$Inner1m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocInner1) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 0))
dWeights$Inner2m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocInner2) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 0))
dWeights$comboSign = ifelse((dWeights$Inner1m1 + dWeights$Inner2m1 ) < (dWeights$Outer1m1 + dWeights$Outer2m1), 1,-1)


#divide weights by sum of the weights
dWeights$Outer1m1c = dWeights$Outer1m1
dWeights$Outer2m1c = dWeights$Outer2m1 
dWeights$Inner1m1c = dWeights$Inner1m1 
dWeights$Inner2m1c = dWeights$Inner2m1

#write.csv(dWeights, "environ_semanticDistance_combo_MatlabInput-m1_weightCheck-testSet-darkness2-noDivide.csv")


#load from matlab
dwc = read.csv("environ_semanticDistance_combo_MatlabOutput-m1_weightCheck-testSet-darkness2-noDivide.csv")
dwc$semDistComboSign = dwc$semDistComboM1*dwc$comboSign

dwc5 = dwc
dwc5$darkSideWeight = dwc5$darkSideWeightm1 
dwc5$semDistComboM1 = dwc5$semDistComboSign

#linear comparison
dwc5$lineX = (dwc5$meanPropDark - .5)/.5
dwc5$se1 = (dwc5$lineX - dwc5$semDistComboM1)^2


#Group by concept and weightpair to get mse
dwc6 <- dwc5 %>%
  group_by(xPair, darkSideWeight, concept) %>%
  summarise(
    mse1 = mean(se1)
  )  

dwc7m1 <- dwc6 %>%
  group_by(darkSideWeight) %>%
  summarise(
    sdWeight = sd(mse1, na.rm =T), 
    mseAve = mean(mse1),
    nWeight = 63,
    seWeight = sdWeight/sqrt(nWeight)
  )  


#all concepts
plotSdwc7allm1 = ggplot() +
  geom_point(data = dwc7m1, aes(x = darkSideWeight, y =mseAve), size =3, shape = 16) + 
  geom_errorbar(data = dwc7m1, aes(x = darkSideWeight, y =mseAve, ymin = mseAve-seWeight, ymax=mseAve+seWeight), width = .06, size = .4) +  
  scale_color_manual(values = c("red", "purple", "blue"))+
  ylim(0,1) +
  xlim(-.05,1.05)+
  coord_fixed()+
  labs(title = "MSE for all concepts",
       x = "weight for darkSide  (weight for assoc SD = 1-darkSide)",
       y = "mean squared error") 
plotSdwc7allm1


#current weight on darkSIde = .30, with mse = .209 (training)
#test best: .213  #new= .19
#test for assoc only: .416  #.42
#test for dark only: .685 #.61


dwc8concept <- dwc6 %>%
  group_by(concept, darkSideWeight) %>%
  summarise(
    sdWeight = sd(mse1, na.rm =T), 
    mseAve = mean(mse1),
    nWeight = 63,
    seWeight = sdWeight/sqrt(nWeight)
  )  



#all concepts
plotSdwc8allm1 = ggplot() +
  geom_point(data = dwc8concept, aes(x = darkSideWeight, y =mseAve, color = concept), size =3, shape = 16, alpha = .5) + 
  geom_errorbar(data = dwc8concept, aes(x = darkSideWeight, y =mseAve, ymin = mseAve-seWeight, ymax=mseAve+seWeight, color = concept), width = .06, size = .4, alpha = .5) +  
  geom_point(data = dwc7m1, aes(x = darkSideWeight, y =mseAve), size =4, shape = 16) + 
  geom_errorbar(data = dwc7m1, aes(x = darkSideWeight, y =mseAve, ymin = mseAve-seWeight, ymax=mseAve+seWeight), width = .08, size = .5) +  
  scale_color_manual(values = c("red", "cyan", "blue"))+
  ylim(0,1.05) +
  xlim(-.05,1.05)+
  coord_fixed()+
  labs(title = "MSE for all concepts",
       x = "weight for darkSide  (weight for assoc SD = 1-darkSide)",
       y = "mean squared error") 
plotSdwc8allm1



weightSets = dwc5
weightSets$lightrgb = as.character(weightSets$lightrgb_x)
weightSets$darkrgb = as.character(weightSets$darkrgb_x)


plotSetWeights = ggplot() +
  geom_point(data = weightSets, aes(x =semDistComboM1, y =meanPropDark), shape=19, size =2) +  
  theme(legend.position = "none")+
  ylim(0,1) +
  xlim(-1,1)+
  facet_wrap(~darkSideWeight)+
  geom_abline(intercept = .5, slope = .5)+
  coord_fixed()+
  labs(title = "weight pairs- mean prop dark",
       x = "semDistCombo",
       y = "meanPropDark") 
plotSetWeights



weightSetsIce = weightSets[weightSets$concept == "ice",]
weightSetsFire = weightSets[weightSets$concept == "fire",]
weightSetsWater = weightSets[weightSets$concept == "water",]

weightSetsOpt = weightSets[weightSets$darkSideWeight ==.3,]

#all items. 
weightSetsOpt <- weightSetsOpt %>% arrange(semDistComboM1)
weightSetsOpt= tibble::rowid_to_column(weightSetsOpt, "plotID")
lightrgbArray_combo <-  dplyr::pull(weightSetsOpt, lightrgb)
darkrgbArray_combo <- dplyr::pull(weightSetsOpt, darkrgb)

plotSetWeightsOpt = ggplot() +
  geom_errorbar(data = weightSetsOpt, aes(x = semDistComboM1, y = meanPropDark, ymin = meanPropDark-seDark, ymax = meanPropDark+seDark), width = .06, size = .4) + 
  geom_point(data = weightSetsOpt, aes(x = semDistComboM1, y = meanPropDark, color=factor(plotID), fill = factor(plotID)), shape=21, size =2, stroke = 2) +  
  theme(legend.position = "none")+
  scale_fill_manual(values = lightrgbArray_combo)+ 
  scale_colour_manual(values = darkrgbArray_combo)+ 
  ylim(0,1) +
  xlim(-1,1)+
  facet_wrap(~concept)+
  geom_abline(intercept = .5, slope = .5)+
  coord_fixed()+
  labs(title = "weight pairs- mean prop dark-opt",
       x = "semDistCombo",
       y = "meanPropDark") 
plotSetWeightsOpt


#ice
weightSetsIce <- weightSetsIce %>% arrange(semDistComboM1)
weightSetsIce3 = weightSetsIce[weightSetsIce$darkSideWeight == .3,]
corr.test(weightSetsIce3$semDistComboM1, weightSetsIce3$meanPropDark)
# .55

#fire
weightSetsFire <- weightSetsFire %>% arrange(semDistComboM1)
weightSetsFire3 = weightSetsFire[weightSetsFire$darkSideWeight == .3,]
corr.test(weightSetsFire3$semDistComboM1, weightSetsFire3$meanPropDark)
# .83


#water
weightSetsWater <- weightSetsWater %>% arrange(semDistComboM1)
weightSetsWater3 = weightSetsWater[weightSetsWater$darkSideWeight == .3,]
corr.test(weightSetsWater3$semDistComboM1, weightSetsWater3$meanPropDark)
#.72


# #t-test to compare mse for best pair versus dark only vs. assoc  --------


#switch from long to wide
dAssocOnly = dwc6[dwc6$darkSideWeight == 0,]
dDarkOnly = dwc6[dwc6$darkSideWeight == 1,]
dComboOnly = dwc6[dwc6$darkSideWeight == .3,]

dAssocMean = mean(dAssocOnly$mse1)
dDarkMean = mean(dDarkOnly$mse1)
dComboMean = mean(dComboOnly$mse1)

#compare best to assoc only 
t.test(dComboOnly$mse1, dAssocOnly$mse1, var.equal = T)
#t = -2.5462, df = 124, p-value = 0.01212

t.test(dComboOnly$mse1, dDarkOnly$mse1, var.equal = T)
#t = -3.538 df = 124, p-value = 0.001






# SUPPLEMENTAL  -----------------------------------------------------------

# Experiment 3 ------------------------------------------------------------


#use full data set
dAllTrials


m1n <- glmer(sidePress ~ semDistAssocSSNorm+darknessSSNorm+ (semDistAssocSSNorm+darknessSSNorm|subjectID), data = dAllTrials, family = "binomial")
summary(m1n)
#(Intercept)        -0.33791    0.05342  -6.326 2.51e-10 ***
#semDistAssocSSNorm 16.29173    0.97549  16.701  < 2e-16 ***
#darknessSSNorm     14.59987    1.08705  13.431  < 2e-16 ***


