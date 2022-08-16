

############################################################################################
### Unifying effects of Direct and Relational Associations for Visual Communication
###
### Melissa A. Schoenlein, Johnny Campos, Kevin J. Lande, Laurent Lessard, Karen B. Schloss
############################################################################################




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



# EXPERIMENT 1 - COLORMAPS INTERPRETATIONS --------------------------------

setwd("C:/Users/melan/Dropbox/Research/Manuscripts/Submitted/SunshineMaps-manuscript/VIS/finalAnalysesCode/dataSets/colormapInterpretations")
dexp1maps = read.csv("exp1maps1.csv")


# exp1: analyses ----------------------------------------------------------------


#create & center variables 
dexp1maps$assocDiffC <- varRecode(dexp1maps$assocDiff, c(0,1), c(-.5,.5))
dexp1maps$conceptC <- varRecode(dexp1maps$concept, c(0,1), c(.5, -.5))
dexp1maps$darkSideC = varRecode(dexp1maps$darkSide, c(0,1), c(-1, 1))
dexp1maps$meanAssocDiffC <- dexp1maps$meanAssocDiff - mean(dexp1maps$meanAssocDiff)
dexp1maps$signedMeanAssocDiff = ifelse(dexp1maps$prompt == "shade", dexp1maps$diffRatingShade2, -1*dexp1maps$diffRatingSun)
dexp1maps$AssociatedSide = ifelse(dall$prompt == "shade" & dexp1maps$darkSide == 0, 0,
                           ifelse(dexp1maps$prompt == "shade" & dexp1maps$darkSide == 1, 1, 
                                  ifelse(dexp1maps$prompt == "sunshine" & dexp1maps$darkSide == 1, 0, 
                                         ifelse(dexp1maps$prompt == "sunshine" & dexp1maps$darkSide == 0, 1, 999))))
dexp1maps$AssociatedSideC = varRecode(dexp1maps$AssociatedSide, c(0,1), c(-1,1))


#multiple the association differences (both currenlty positive) by whether the more associated side was the left vs.right
dexp1maps$sideAssociationDiff = ifelse(dexp1maps$prompt == "shade", dexp1maps$diffRatingShade2*dexp1maps$AssociatedSideC, 
                                ifelse(dexp1maps$prompt == "sunshine", dexp1maps$diffRatingSun*dexp1maps$AssociatedSideC, 999))
dexp1maps$sideAssocDiffNorm = round(2*(((dexp1maps$sideAssociationDiff - min(dexp1maps$sideAssociationDiff))/(max(dexp1maps$sideAssociationDiff)-min(dexp1maps$sideAssociationDiff))))-1,4)


#Model predicting side press from which side was dark & which side was more associated and by how much
mExp1maps <- glmer(sidePress ~ darkSideC+sideAssocDiffNorm + (1|subjectID), data = dexp1maps, family = "binomial")
summary(mExp1maps)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        0.04257    0.07297   0.583     0.56    
#darkSideC         1.32985    0.08601  15.461   <2e-16 ***
# sideAssocDiffNorm  4.51181    0.21267  21.215   <2e-16 ***


# exp1: plots -------------------------------------------------------------

#Get averages for each individual colormap
dexp1mapsMean <- dexp1maps %>%
  group_by(conceptStr, meanAssocDiff, ID, lightrgb, darkrgb) %>%
  summarise(
    sd = sd(propDark, na.rm = TRUE),
    meanPropDark = mean(propDark),
    N = length(subjectID),
    se = sd/sqrt(N)
  )
dexp1mapsMean


#do some sorting and organizing so that have the correct colors for plotting
map_Set = dexp1mapsMean %>% arrange(ID)
mapSun = map_Set[map_Set$conceptStr == "sunshine",]
mapShade = map_Set[map_Set$conceptStr == "shade",]
mapSun2 = mapSun %>% arrange(ID)
mapShade2 = mapShade %>% arrange(ID)
combo = rbind(mapSun2, mapShade2)
lightrgbArray_combo <- dplyr::pull(combo, lightrgb)
darkrgbArray_combo <- dplyr::pull(combo, darkrgb)
lightrgbArray_combo2 <- as.character(lightrgbArray_combo)
darkrgbArray_combo2 <- as.character(darkrgbArray_combo)


#plot as continuous  assoc diff for shade (triangles) vs. sunshine (circles) 
plotExp1maps = ggplot() +
  geom_errorbar(data = mapSun, aes(x = meanAssocDiff*-1, y = meanPropDark, ymin = meanPropDark-se, ymax = meanPropDark+se), width = .1, size = .5) + 
  geom_errorbar(data = mapShade, aes(x = meanAssocDiff, y = meanPropDark, ymin = meanPropDark-se, ymax = meanPropDark+se), width = .1, size = .5) + 
  scale_fill_manual(values= lightrgbArray_combo2) + 
  geom_point(data = mapSun, aes(x = meanAssocDiff*-1, y = meanPropDark,  color=factor(ID), fill = factor(ID)), shape=21, size = 3, stroke = 3) +  
  geom_point(data = mapShade , aes(x = meanAssocDiff, y = meanPropDark, color=factor(ID), fill = factor(ID)), shape=24, size = 3, stroke = 3) +  
  scale_colour_manual(values= darkrgbArray_combo2) +
  ylim(0,1) +
  xlim(-1,1) +
  coord_fixed()+
  labs(title = "exp1maps - association difference",
          x = "signed association difference",
          y = "mean prop. select darker side") 
plotExp1maps





# EXPERIMENT 2 - COLORMAPS INTERPRETATIONS --------------------------------

setwd("C:/Users/melan/Dropbox/Research/Manuscripts/Submitted/SunshineMaps-manuscript/VIS/finalAnalysesCode/dataSets/colormapInterpretations")
dexp2maps = read.csv("exp2maps.csv")


# exp2: analyses  -------------------------------------------------------------


#current dataset includes each trial for each subject (10 rows per subj)

#Add variables for getting signed semantic distance
dexp2maps$Outer1 = dexp2maps$lightAssoc_noSun
dexp2maps$Outer2 = dexp2maps$darkAssoc_lotSun
dexp2maps$Inner1 = dexp2maps$darkAssoc_noSun
dexp2maps$Inner2 = dexp2maps$lightAssoc_lotSun
dexp2maps$assign = ifelse((dexp2maps$Inner1 + dexp2maps$Inner2 ) < (dexp2maps$Outer1 + dexp2maps$Outer2), 1,-1)
dexp2maps$assignSemDist = dexp2maps$assign * dexp2maps$SemDist
dexp2maps$signMeanAssocDiff = dexp2maps$meanAssocDiff*-1

#remove to only 1 row per subject to use the propdark
dexp2mapsSubj = dexp2maps[!duplicated(dexp2maps$subjectID),] #655 subjects


#Get averages for each individual colormap
dexp2mapsMean <- dexp2mapsSubj %>%
  group_by(signMeanAssocDiff,  assignSemDist, ID, lightrgb, darkrgb) %>%
  summarise(
    sd = sd(propDark, na.rm = TRUE),
    meanPropDark = mean(propDark),
    N = length(subjectID),
    se = sd/sqrt(N)
  )
dexp2mapsMean


#correlation of association difference and mean prop dark
corr.test(dexp2mapsMean$signMeanAssocDiff, dexp2mapsMean$meanPropDark)
#r(14) = .82

#correlation of semantic distance and mean prop dark
corr.test(dexp2mapsMean$assignSemDist, dexp2mapsMean$meanPropDark)
#r(14) = .94




#remove 2 bad scales
dexp2mapsMeanCl = dexp2mapsMean[dexp2mapsMean$ID != 143, ]
dexp2mapsMeanCl = dexp2mapsMeanCl[dexp2mapsMeanCl$ID != 90, ]


#correlation of association difference and mean prop dark
corr.test(dexp2mapsMeanCl$signMeanAssocDiff, dexp2mapsMeanCl$meanPropDark)
#r(12) = .85

#correlation of semantic distance and mean prop dark
corr.test(dexp2mapsMeanCl$assignSemDist, dexp2mapsMeanCl$meanPropDark)
#r(12) = .97




# exp2: plots -------------------------------------------------------------


#do some sorting and organizing so that have the correct colors for plotting
mapSun = dexp2mapsMean %>% arrange(ID)
lightrgbArray_combo <-  dplyr::pull(mapSun, lightrgb)
darkrgbArray_combo <- dplyr::pull(mapSun, darkrgb)


#PLOT AS CONTINUOUS ASSOCIATION DIFFERENCE
plotExp2mapsAssoc = ggplot() +
  geom_errorbar(data = mapSun, aes(x = signMeanAssocDiff, y = meanPropDark, ymin = meanPropDark-se, ymax = meanPropDark+se), width = .06, size = .4) + 
  scale_fill_manual(values=  c("#d5b811", "#d0e942", "#fba714","#f7db7c","#fba714","#d0b85a","#aac510","#fcdb42",
                               "#4dc7e8", "#eddcad", "#39f6e0", "#dddddd", "#e6a8b7", "#dddddd", "#b9b9b9", "#b9b9b9"))+ 
  geom_point(data = mapSun, aes(x = signMeanAssocDiff, y = meanPropDark, color=factor(ID), fill = factor(ID)), shape=21, size =4, stroke = 3) +  
  scale_colour_manual(values = c("#3b3b3b", "#777777", "#3b3b3b","#777777","#2e3086","#1c3d61","#5e2b3a","#077acc","#512d5f",
                                 "#a553c8","#777777","#3b8378","#184415","#0e8a19","#600b84","#72005e"))+ 
  ylim(0,1) +
  xlim(-1,1) +
  coord_fixed()+
  labs(title = "exp2maps - association difference",
       x = "signed association difference",
       y = "mean prop. select darker side") 
plotExp2mapsAssoc


#PLOT AS SIGNED SEMANTIC DISTANCE from endpoints (LOT SUN - NO SUN) - old
plotExp2mapsSemDist = ggplot(data = mapSun, aes(x = assignSemDist, y = meanPropDark)) +
  geom_errorbar(data = mapSun, aes(x = assignSemDist, y = meanPropDark, ymin = meanPropDark-se, ymax = meanPropDark+se), width = .06, size = .4) + 
  scale_fill_manual(values=  c("#d5b811", "#d0e942", "#fba714","#f7db7c","#fba714","#d0b85a","#aac510","#fcdb42",
                               "#4dc7e8", "#eddcad", "#39f6e0", "#dddddd", "#e6a8b7", "#dddddd", "#b9b9b9", "#b9b9b9"))+ 
  geom_point(aes(color=factor(ID), fill = factor(ID)), shape=21, size =4, stroke = 3) +  
  scale_colour_manual(values = c("#3b3b3b", "#777777", "#3b3b3b","#777777","#2e3086","#1c3d61","#5e2b3a","#077acc","#512d5f",
                                 "#a553c8","#777777","#3b8378","#184415","#0e8a19","#600b84","#72005e"))+ 
  geom_smooth(method = "lm", se = T)+
  ylim(0,1) +
  xlim(-1,1) +
  coord_fixed()+
  labs(title = "exp2maps - semantic distance",
       x = "signed semantic distance",
       y = "mean prop. select darker side") 
plotExp2mapsSemDist





# EXPERIMENT 3 - COLORMAPS INTERPRETATIONS --------------------------------

setwd("C:/Users/melan/Dropbox/Research/Manuscripts/Submitted/SunshineMaps-manuscript/VIS/finalAnalysesCode/dataSets/colormapInterpretations")

#load mean darkness ratings
dDarkness = read.csv("exp3maps-darknessRatings.csv" )

#load weight paris for training
dWeightTrain = read.csv("weightPairsTraining.csv")

#load weight paris for testing
dWeightTest = read.csv("weightPairsTesting.csv")


# exp3: training set ----------------------------------------------------------

#training set
dsubjSet = read.csv("environ-split-trainingSet.csv")
d2c = dsubjSet


#obtain prop select dark for each subject
dsumD <- d2c %>%
  group_by(subjectID, concept, xPair) %>%
  summarise(
    totalDark = sum(selectDark),
    propDark = totalDark/10,
  )
dsumD



dsumDave <- dsumD %>%
  group_by(concept, xPair) %>%
  summarise(
    sdDark = sd(propDark, na.rm = TRUE),
    meanPropDark = mean(propDark),
    Ndark = length(subjectID),
    seDark = sdDark/sqrt(Ndark)
  )
dsumDave


dsumL <- d2c %>%
  group_by(subjectID, concept, xPair, darkSide) %>%
  summarise(
    totalLeft = sum(sidePress),
    propLeft = totalLeft/5
  )
dsumL

dsumLAve <- dsumL %>%
  group_by(concept, xPair, darkSide) %>%
  summarise(
    sdLeft = sd(propLeft, na.rm = TRUE),
    meanPropLeft = mean(propLeft),
    Nleft = length(subjectID),
    seLeft = sdLeft/sqrt(Nleft)
  )
dsumL


#merge so that propDark appears with all the rest of the data
dall = merge(dsumD, d2c, by = c("subjectID", "concept", "xPair"))
dall = merge(dsumDave, dall, by = c("concept", "xPair"))
dall = merge(dsumL, dall, by = c("subjectID", "concept", "xPair", "darkSide"))
dall = merge(dsumLAve, dall, by = c( "concept", "xPair", "darkSide"))

d2 = dall  #full set without averages by subject 

#update factors for plotting WRT to left side and with OVI
d2$darkSideC =varRecode(d2$darkSide2, c(0,1), c(-.5, .5))  
d2$darkSideC = as.factor(d2$darkSideC) 
d2$meanAssocDiff = d2$meanAssocDiff *-1
d2$lightrgb = as.character(d2$lightrgb)
d2$darkrgb = as.character(d2$darkrgb)


#DARK IS MORE BIAS  dark on left (1) versus right (-1)
d2$darkSideC2 = as.numeric(d2$darkSide2)
d2$darkSideC3 = varRecode(d2$darkSideC2, c(0,1), c(-1, 1)) #-1 dark right; 1 dark left


#ASSOCIATION STRENGTH more associated side on left (1) or right (-1)
#strenght of associated side (near -1 is more associated side on the right, with a strong association difference; 
#near + 1 is more associated on the left, with a a strong association difference, 
# near  +.05 more associated on left, but smalll association difference)
d2$meanAssocDiffNoSign = ifelse(d2$meanAssocDiff < 0, d2$meanAssocDiff*-1, d2$meanAssocDiff)
d2$associatedSide = ifelse(d2$meanAssocDiff > 0 & d2$darkSideC3 == 1, 1,
                           ifelse(d2$meanAssocDiff > 0 & d2$darkSideC3 == -1,-1, 
                                  ifelse(d2$meanAssocDiff < 0 & d2$darkSideC3 == 1, -1,
                                         ifelse(d2$meanAssocDiff < 0 & d2$darkSideC3 == -1, 1,0))))

d3 = d2
keep = c("subjectID", "concept", "xPair", "sidePress",  "seDark", "seLeft", "meanPropLeft","meanPropDark", "darkSideC3", "associatedSide","meanAssocDiff", "SemDist", "darkAssoc_lot", "darkAssoc_no", "lightAssoc_lot", "lightAssoc_no",  "diffLighting", "lightrgb", "darkrgb")
d3 = d3[keep]

#associations
d3$assocOuter1 = d3$darkAssoc_lot
d3$assocOuter2 = d3$lightAssoc_no
d3$assocInner1 = d3$lightAssoc_lot
d3$assocInner2 = d3$darkAssoc_no

#write.csv(d3, "environ-semanticDistance-MatlabInput-trainSet2.csv")# train






#load in from matlab
d4 = read.csv("exp3maps-trainingSet1.csv")# train

#merge with darkness ratings
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
#d4$semDistAssocSignAssocSide = ifelse(d4$meanAssocDiff < 0, d4$semDistAssoc*-1, d4$semDistAssoc)


#coded wrt to assignProblem being solved
#get outer and inner weights
d4$Outer1 = d4$lightAssoc_no
d4$Outer2 = d4$darkAssoc_lot
d4$Inner1 = d4$darkAssoc_no
d4$Inner2 = d4$lightAssoc_lot
d4$assign = ifelse((d4$Inner1 + d4$Inner2 ) < (d4$Outer1 + d4$Outer2), 1,-1)
d4$assignSemDistAssocSignAssocSide = d4$assign * d4$SemDist
d4$match =  ifelse(d4$xPair == 350 | d4$xPair == 1125, -1, 1)#ifelse(d4$semDistAssocSignAssocSide==d4$assignSemDist, 1, -1)
d4$assignSemDistAssocSS = ifelse(d4$match == 1, d4$semDistAssoc * d4$associatedSide, d4$semDistAssoc * d4$associatedSide*-1)
d4$assignSemDistAssocSSNorm = round(2*(((d4$assignSemDistAssocSS - min(d4$assignSemDistAssocSS))/(max(d4$assignSemDistAssocSS)-min(d4$assignSemDistAssocSS))))-1,4)


keep = c( "concept", "xPair", "meanAssocDiff", "lightAssoc_no", "lightAssoc_lot", "darkAssoc_no", "darkAssoc_lot","meanPropDark","lightrgb", "darkrgb", "semDistAssocSSNorm", "assignSemDistAssocSignAssocSide","semDistAssocSS", "assignSemDistAssocSS", "assignSemDistAssocSSNorm", "match")
dcheck = d4[keep]
dcheck = dcheck[!duplicated(dcheck$assignSemDistAssocSignAssocSide),]
#dcheck2 = dcheck[(dcheck$xPair == 350 | dcheck$xPair == 1125),]


keep = c("subjectID", "concept", "xPair", "sidePress", "seDark", "seLeft", "meanAssocDiff", "assocOuter1", "assocOuter2", "assocInner1", "assocInner2","meanPropLeft","meanPropDark", "darkSideC3","lightrgb", "darkrgb", "semDistAssocSSNorm", "semDistOviSSNorm",  "darknessSSNorm",  "assignSemDistAssocSignAssocSide", "assignSemDistAssocSSNorm")
d5 = d4[keep]

dAllTrials = d5
dEachSide = d5 %>% distinct(concept, xPair, darkSideC3, .keep_all = T)
dEachXPair = d5 %>% distinct(concept, xPair, .keep_all = T)


#color pairs in Figure 1
dEachBanner = dEachXPair[dEachXPair$xPair == 262 | dEachXPair$xPair == 1019,]


#select data frame
dEachXPairComboC = dEachXPair %>% 
  select( subjectID, concept, xPair, meanPropDark,  semDistAssocSSNorm, seDark,  semDistOviSSNorm, assocOuter1, assocOuter2, assocInner1, assocInner2, lightrgb, darkrgb) %>%
  distinct(subjectID, concept, xPair, .keep_all = TRUE)

#merge with training weight sets
dWeightsA = merge(dEachXPairComboC, dWeightTrain)
dWeights = merge(dDarkness, dWeightsA, by =c("xPair", "concept"))


#GET COMBINED SIGNED SEMANTIC DISTANCE
#get outer and inner weights - explain how signing is happening 
dWeights$Outer1m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocOuter1) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 1))
dWeights$Outer2m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocOuter2) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 1))
dWeights$Inner1m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocInner1) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 0))
dWeights$Inner2m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocInner2) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 0))
dWeights$comboSign = ifelse((dWeights$Inner1m1 + dWeights$Inner2m1 ) < (dWeights$Outer1m1 + dWeights$Outer2m1), 1,-1)


dWeights$Outer1m1c = dWeights$Outer1m1 
dWeights$Outer2m1c = dWeights$Outer2m1 
dWeights$Inner1m1c = dWeights$Inner1m1
dWeights$Inner2m1c = dWeights$Inner2m1


#Save file and use as input into matlab function to calculate combined semantic distance
#write.csv(dWeights, "environ_semanticDistance_combo_MatlabInput-m1_weightCheck-trainSet-darkness2-noDivide.csv")



#load output from from matlab
dwc = read.csv("exp3maps-trainingSet2.csv")
dwc$semDistComboSign = dwc$semDistComboM1*dwc$comboSign

dwc5 = dwc
dwc5$darkSideWeight = dwc5$darkSideWeightm1 
dwc5$semDistComboM1 = dwc5$semDistComboSign

#linear comparison
dwc5$lineX = (dwc5$meanPropDark - .5)/.5
dwc5$se1 = (dwc5$lineX - dwc5$semDistComboM1)^2



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


#weight pair with smallest MSE: darksideWeight = .3




# exp3: testing set -------------------------------------------------------

#testing set
dsubjSet = read.csv("environ-split-testingSet.csv")
d2c = dsubjSet


#obtain prop select dark for each subject
dsumD <- d2c %>%
  group_by(subjectID, concept, xPair) %>%
  summarise(
    totalDark = sum(selectDark),
    propDark = totalDark/10,
  )
dsumD



dsumDave <- dsumD %>%
  group_by(concept, xPair) %>%
  summarise(
    sdDark = sd(propDark, na.rm = TRUE),
    meanPropDark = mean(propDark),
    Ndark = length(subjectID),
    seDark = sdDark/sqrt(Ndark)
  )
dsumDave


dsumL <- d2c %>%
  group_by(subjectID, concept, xPair, darkSide) %>%
  summarise(
    totalLeft = sum(sidePress),
    propLeft = totalLeft/5
  )
dsumL

dsumLAve <- dsumL %>%
  group_by(concept, xPair, darkSide) %>%
  summarise(
    sdLeft = sd(propLeft, na.rm = TRUE),
    meanPropLeft = mean(propLeft),
    Nleft = length(subjectID),
    seLeft = sdLeft/sqrt(Nleft)
  )
dsumL


#merge so that propDark appears with all the rest of the data
dall = merge(dsumD, d2c, by = c("subjectID", "concept", "xPair"))
dall = merge(dsumDave, dall, by = c("concept", "xPair"))
dall = merge(dsumL, dall, by = c("subjectID", "concept", "xPair", "darkSide"))
dall = merge(dsumLAve, dall, by = c( "concept", "xPair", "darkSide"))


d2 = dall  #full set without averages by subject 

#update factors for plotting WRT to left side and with OVI
d2$darkSideC =varRecode(d2$darkSide2, c(0,1), c(-.5, .5))  
d2$darkSideC = as.factor(d2$darkSideC) 
d2$meanAssocDiff = d2$meanAssocDiff *-1
d2$lightrgb = as.character(d2$lightrgb)
d2$darkrgb = as.character(d2$darkrgb)


#DARK IS MORE BIAS  dark on left (1) versus right (-1)
d2$darkSideC2 = as.numeric(d2$darkSide2)
d2$darkSideC3 = varRecode(d2$darkSideC2, c(0,1), c(-1, 1)) #-1 dark right; 1 dark left


#ASSOCIATION STRENGTH more associated side on left (1) or right (-1)
#strenght of associated side (near -1 is more associated side on the right, with a strong association difference; 
#near + 1 is more associated on the left, with a a strong association difference, 
# near  +.05 more associated on left, but smalll association difference)
d2$meanAssocDiffNoSign = ifelse(d2$meanAssocDiff < 0, d2$meanAssocDiff*-1, d2$meanAssocDiff)
d2$associatedSide = ifelse(d2$meanAssocDiff > 0 & d2$darkSideC3 == 1, 1,
                           ifelse(d2$meanAssocDiff > 0 & d2$darkSideC3 == -1,-1, 
                                  ifelse(d2$meanAssocDiff < 0 & d2$darkSideC3 == 1, -1,
                                         ifelse(d2$meanAssocDiff < 0 & d2$darkSideC3 == -1, 1,0))))

d3 = d2
keep = c("subjectID", "concept", "xPair", "sidePress",  "seDark", "seLeft", "meanPropLeft","meanPropDark", "darkSideC3", "associatedSide","meanAssocDiff", "SemDist", "darkAssoc_lot", "darkAssoc_no", "lightAssoc_lot", "lightAssoc_no",  "diffLighting", "lightrgb", "darkrgb")
d3 = d3[keep]

#associations
d3$assocOuter1 = d3$darkAssoc_lot
d3$assocOuter2 = d3$lightAssoc_no
d3$assocInner1 = d3$lightAssoc_lot
d3$assocInner2 = d3$darkAssoc_no

#Get semantic distance for associations
#write.csv(d3, "environ-semanticDistance-MatlabInput-testSet2.csv")#test






#load in testing set
d4 = read.csv("exp3maps-testingSet1.csv") #test


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
#d4$semDistAssocSignAssocSide = ifelse(d4$meanAssocDiff < 0, d4$semDistAssoc*-1, d4$semDistAssoc)


#coded wrt to assignProblem being solved
#get outer and inner weights
d4$Outer1 = d4$lightAssoc_no
d4$Outer2 = d4$darkAssoc_lot
d4$Inner1 = d4$darkAssoc_no
d4$Inner2 = d4$lightAssoc_lot
d4$assign = ifelse((d4$Inner1 + d4$Inner2 ) < (d4$Outer1 + d4$Outer2), 1,-1)
d4$assignSemDistAssocSignAssocSide = d4$assign * d4$SemDist
d4$match =  ifelse(d4$xPair == 350 | d4$xPair == 1125, -1, 1)#ifelse(d4$semDistAssocSignAssocSide==d4$assignSemDist, 1, -1)
d4$assignSemDistAssocSS = ifelse(d4$match == 1, d4$semDistAssoc * d4$associatedSide, d4$semDistAssoc * d4$associatedSide*-1)
d4$assignSemDistAssocSSNorm = round(2*(((d4$assignSemDistAssocSS - min(d4$assignSemDistAssocSS))/(max(d4$assignSemDistAssocSS)-min(d4$assignSemDistAssocSS))))-1,4)


keep = c( "concept", "xPair", "meanAssocDiff", "lightAssoc_no", "lightAssoc_lot", "darkAssoc_no", "darkAssoc_lot","meanPropDark","lightrgb", "darkrgb", "semDistAssocSSNorm", "assignSemDistAssocSignAssocSide","semDistAssocSS", "assignSemDistAssocSS", "assignSemDistAssocSSNorm", "match")
dcheck = d4[keep]
dcheck = dcheck[!duplicated(dcheck$assignSemDistAssocSignAssocSide),]
#dcheck2 = dcheck[(dcheck$xPair == 350 | dcheck$xPair == 1125),]


keep = c("subjectID", "concept", "xPair", "sidePress", "oviT", "oviTOuter", "oviTInner","seDark", "seLeft", "meanAssocDiff", "assocOuter1", "assocOuter2", "assocInner1", "assocInner2","meanPropLeft","meanPropDark", "darkSideC3","lightrgb", "darkrgb", "semDistAssocSSNorm", "semDistOviSSNorm",  "darknessSSNorm",  "assignSemDistAssocSignAssocSide", "assignSemDistAssocSSNorm")
d5 = d4[keep]

dAllTrials = d5
dEachSide = d5 %>% distinct(concept, xPair, darkSideC3, .keep_all = T)
dEachXPair = d5 %>% distinct(concept, xPair, .keep_all = T)

dEachBanner = dEachXPair[dEachXPair$xPair == 262 | dEachXPair$xPair == 1019,]


#select data frame
dEachXPairComboC = dEachXPair %>% 
  select( subjectID, concept, xPair, meanPropDark,  semDistAssocSSNorm, seDark, oviT, oviTOuter, oviTInner, semDistOviSSNorm, assocOuter1, assocOuter2, assocInner1, assocInner2, lightrgb, darkrgb) %>%
  distinct(subjectID, concept, xPair, .keep_all = TRUE)


#merge 
dWeightsA = merge(dEachXPairComboC, dWeightTest)
dWeights = merge(dDarkness, dWeightsA, by =c("xPair", "concept"))

#get outer and inner weights
dWeights$Outer1m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocOuter1) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 1))
dWeights$Outer2m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocOuter2) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 1))
dWeights$Inner1m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocInner1) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 0))
dWeights$Inner2m1 = ((dWeights$semDistAssocWeightm1 * dWeights$assocInner2) + (dWeights$meanDarkness * dWeights$darkSideWeightm1 * 0))
dWeights$comboSign = ifelse((dWeights$Inner1m1 + dWeights$Inner2m1 ) < (dWeights$Outer1m1 + dWeights$Outer2m1), 1,-1)


#divide weights by sum of the weights
dWeights$Outer1m1c = dWeights$Outer1m1 #/ (dWeights$semDistAssocWeightm1  + (dWeights$meanDarkness * dWeights$darkSideWeightm1))
dWeights$Outer2m1c = dWeights$Outer2m1 #/ (dWeights$semDistAssocWeightm1  + (dWeights$meanDarkness * dWeights$darkSideWeightm1))
dWeights$Inner1m1c = dWeights$Inner1m1 #/ (dWeights$semDistAssocWeightm1  + (dWeights$meanDarkness * dWeights$darkSideWeightm1))
dWeights$Inner2m1c = dWeights$Inner2m1#/ (dWeights$semDistAssocWeightm1  + (dWeights$meanDarkness * dWeights$darkSideWeightm1))

#write.csv(dWeights, "environ_semanticDistance_combo_MatlabInput-m1_weightCheck-testSet-darkness2-noDivide.csv")

dWeightsBanner = dWeights[dWeights$xPair == 262 | dWeights$xPair == 1019,]
dWeightsBanner = dWeightsBanner[dWeightsBanner$weightPair ==1,]



#load from matlab
dwc = read.csv("exp3maps-testingSet2.csv")
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


#write.csv(dwc5, "testSet.csv")

dwc7m1 <- dwc6 %>%
  group_by(darkSideWeight) %>%
  summarise(
    sdWeight = sd(mse1, na.rm =T), 
    mseAve = mean(mse1),
    nWeight = 63,
    seWeight = sdWeight/sqrt(nWeight)
  )  


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


#MODEL PREDICTING MSE
dwc6$conceptC1= ifelse(dwc6$concept == "fire", 2/3, -1/3)
dwc6$conceptC2= ifelse(dwc6$concept == "water", .5,
                       ifelse(dwc6$concept == "ice",-.5, 0))
dwc6$weightC1= ifelse(dwc6$darkSideWeight ==  .3, 2/3, -1/3)
dwc6$weightC2= ifelse(dwc6$darkSideWeight == 0, .5,
                      ifelse(dwc6$darkSideWeight == 1,-.5, 0))



dwc6$conceptContrast = (cbind(dwc6$conceptC1, dwc6$conceptC2))
dwc6$weightContrast = (cbind(dwc6$weightC1, dwc6$weightC2))


control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6),check.nobs.vs.nRE = "ignore") 

mMSE = lmer(mse1 ~ conceptContrast*weightContrast + (1+weightContrast|xPair), data = dwc6, control = control)
summary(mMSE)
Anova(mMSE, type = 3, test = "F")
#Estimate Std. Error        df t value Pr(>|t|)    
#conceptContrast1                  -0.47503    0.09486  91.75023  -5.008 2.66e-06 ***
#conceptContrast2                   0.07210    0.10616 100.08348   0.679  0.49861    
#weightContrast1                   -0.31601    0.07454  53.54973  -4.240 8.89e-05 ***
#weightContrast2                   -0.18292    0.16106  48.32302  -1.136  0.26166    
#conceptContrast1:weightContrast1   0.35856    0.13937  66.84189   2.573  0.01232 *  
#conceptContrast2:weightContrast1  -0.18871    0.15813  66.15099  -1.193  0.23698    
#conceptContrast1:weightContrast2   0.76587    0.26654 103.14799   2.873  0.00493 ** 
#conceptContrast2:weightContrast2   0.02925    0.29549 107.64724   0.099  0.92132    

#                                     F Df Df.res    Pr(>F)    
#(Intercept)                    56.9655  1 55.255 4.655e-10 ***
#conceptContrast                12.6559  2 60.760 2.542e-05 ***
#weightContrast                  9.1841  2 53.452 0.0003728 ***
#conceptContrast:weightContrast  3.1352  4 65.625 0.0201767 *  



#t-test to compare mse for best pair versus dark only vs. assoc

#switch from long to wide
dAssocOnly = dwc6[dwc6$darkSideWeight == 0,]
dDarkOnly = dwc6[dwc6$darkSideWeight == 1,]
dComboOnly = dwc6[dwc6$darkSideWeight == .3,]

dAssocMean = mean(dAssocOnly$mse1)
dDarkMean = mean(dDarkOnly$mse1)
dComboMean = mean(dComboOnly$mse1)

#compare best to assoc only 
t.test(dComboOnly$mse1, dAssocOnly$mse1, var.equal = T)
#t = -2.3014, df = 124, p-value = 0.02304
#new
#t = -2.5462, df = 124, p-value = 0.01212


t.test(dComboOnly$mse1, dDarkOnly$mse1, var.equal = T)
#t = -3.5522, df = 124, p-value = 0.000541
#new
#t = -3.538 df = 124, p-value = 0.001




weightSets = dwc5
weightSets$lightrgb = as.character(weightSets$lightrgb_x)
weightSets$darkrgb = as.character(weightSets$darkrgb_x)


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
#.55  #new .55


#fire
weightSetsFire <- weightSetsFire %>% arrange(semDistComboM1)
weightSetsFire3 = weightSetsFire[weightSetsFire$darkSideWeight == .3,]
corr.test(weightSetsFire3$semDistComboM1, weightSetsFire3$meanPropDark)
#.85  #new .83


#water
weightSetsWater <- weightSetsWater %>% arrange(semDistComboM1)
weightSetsWater3 = weightSetsWater[weightSetsWater$darkSideWeight == .3,]
corr.test(weightSetsWater3$semDistComboM1, weightSetsWater3$meanPropDark)
#.72  #new - .72














