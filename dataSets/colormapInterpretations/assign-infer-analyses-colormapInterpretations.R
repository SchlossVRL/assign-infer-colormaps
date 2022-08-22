

############################################################################################
### Unifying effects of Direct and Relational Associations for Visual Communication
###
###                           COLORMAP INTERPRETATIONS
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


#load in exp1 maps - assign-infer-colormaps\dataSets\colormapInterpretations folder
dexp1maps = read.csv("exp1maps.csv")


#Model predicting side press from which side was dark & which side was more associated and by how much
mExp1maps <- glmer(sidePress ~ darkSideC+sideAssocDiffNorm + (1|subjectID), data = dexp1maps, family = "binomial")
summary(mExp1maps)


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

#load in exp1 maps - assign-infer-colormaps\dataSets\colormapInterpretations folder
dexp2maps = read.csv("exp2maps.csv")
#Note: To obtain the combined semantic distance, use the matlab file: assign-infer-semanticDistance-main


#Filter to only 1 row per subject to use the propdark
dexp2mapsSubj = dexp2maps[!duplicated(dexp2maps$subjectID),]


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

#correlation of semantic distance and mean prop dark
corr.test(dexp2mapsMean$assignSemDist, dexp2mapsMean$meanPropDark)


#do some sorting and organizing so that have the correct colors for plotting
mapSun = dexp2mapsMean %>% arrange(ID)
lightrgbArray_combo <-  dplyr::pull(mapSun, lightrgb)
darkrgbArray_combo <- dplyr::pull(mapSun, darkrgb)


#PLOT AS CONTINUOUS ASSOCIATION DIFFERENCE
plotExp2mapsAssoc = ggplot() +
  geom_errorbar(data = mapSun, aes(x = signMeanAssocDiff, y = meanPropDark, ymin = meanPropDark-se, ymax = meanPropDark+se), width = .06, size = .4) + 
  scale_fill_manual(values=  c("#d5b811", "#d0e942", "#fba714","#f7db7c","#fba714","#d0b85a","#aac510","#fcdb42",
                               "#4dc7e8",  "#39f6e0", "#dddddd", "#dddddd", "#b9b9b9", "#b9b9b9"))+ 
  geom_point(data = mapSun, aes(x = signMeanAssocDiff, y = meanPropDark, color=factor(ID), fill = factor(ID)), shape=21, size =4, stroke = 3) +  
  scale_colour_manual(values = c("#3b3b3b", "#777777", "#3b3b3b","#777777","#2e3086","#1c3d61","#5e2b3a","#077acc","#512d5f",
                                 "#777777","#3b8378","#0e8a19","#600b84","#72005e"))+ 
  ylim(0,1) +
  xlim(-1,1) +
  coord_fixed()+
  labs(title = "exp2maps - association difference",
       x = "signed association difference",
       y = "mean prop. select darker side") 
plotExp2mapsAssoc


#PLOT AS SIGNED SEMANTIC DISTANCE from endpoints (LOT SUN - NO SUN) 
plotExp2mapsSemDist = ggplot(data = mapSun, aes(x = assignSemDist, y = meanPropDark)) +
  geom_errorbar(data = mapSun, aes(x = assignSemDist, y = meanPropDark, ymin = meanPropDark-se, ymax = meanPropDark+se), width = .06, size = .4) + 
  scale_fill_manual(values=  c("#d5b811", "#d0e942", "#fba714","#f7db7c","#fba714","#d0b85a","#aac510","#fcdb42",
                               "#4dc7e8",  "#39f6e0", "#dddddd",  "#dddddd", "#b9b9b9", "#b9b9b9"))+ 
  geom_point(aes(color=factor(ID), fill = factor(ID)), shape=21, size =4, stroke = 3) +  
  scale_colour_manual(values = c("#3b3b3b", "#777777", "#3b3b3b","#777777","#2e3086","#1c3d61","#5e2b3a","#077acc","#512d5f",
                                 "#777777","#3b8378","#0e8a19","#600b84","#72005e"))+ 
  geom_smooth(method = "lm", se = T)+
  ylim(0,1) +
  xlim(-1,1) +
  coord_fixed()+
  labs(title = "exp2maps - semantic distance",
       x = "signed semantic distance",
       y = "mean prop. select darker side") 
plotExp2mapsSemDist




# EXPERIMENT 3 - COLORMAPS INTERPRETATIONS --------------------------------


#load mean darkness ratings - -  assign-infer-colormaps\dataSets\supplementaMaterial folder
dDarkness = read.csv("exp3maps-darknessRatings-ave.csv" )

#load weight pairs for training -  assign-infer-colormaps\dataSets\colormapInterpretations folder
dWeightTrain = read.csv("weightPairsTraining.csv")

#load weight pairs for testing -  assign-infer-colormaps\dataSets\colormapInterpretations folder
dWeightTest = read.csv("weightPairsTesting.csv")



# exp3: TRAINING SET ------------------------------------------------------

#full training set -  assign-infer-colormaps\dataSets\colormapInterpretations folder
exp3training = read.csv("exp3maps-trainingSet.csv")


keep = c("subjectID", "concept", "xPair", "sidePress", "seDark", "seLeft", "meanAssocDiff", "assocOuter1", "assocOuter2", "assocInner1", "assocInner2","meanPropLeft","meanPropDark", "darkSideC3","lightrgb", "darkrgb", "semDistAssocSSNorm", "darknessSSNorm",  "assignSemDistAssocSSNorm")
exp3training = exp3training[keep]
dEachSide = exp3training %>% distinct(concept, xPair, darkSideC3, .keep_all = T)
dEachXPair = exp3training %>% distinct(concept, xPair, .keep_all = T)

#color pairs in Banner Figure 1 
dEachBanner = dEachXPair[dEachXPair$xPair == 262 | dEachXPair$xPair == 1019,]



#Training set averaged over participants, with combined semantic distance for each weight pair - assign-infer-colormaps\dataSets\colormapInterpretations folder
#Note: To obtain the combined semantic distance, use the matlab file: assign-infer-semanticDistance-main
exp3trainingAve = read.csv("exp3maps-trainingSet-ave.csv")


#Group by concept and weightpair to get MSE
exp3trainingAveGroup <- exp3trainingAve %>%
  group_by(concept, darkSideWeight) %>%
  summarise(
    mse1 = mean(se1)
  )  
exp3trainingAveGroup2 = merge(exp3trainingAve, exp3trainingAveGroup, by = c("concept","darkSideWeight"))


exp3trainingAveGroup2  = exp3trainingAveGroup2  %>% 
  select( concept, xPair,darkSideWeight, semDistComboM1, mse1) %>%
  distinct(concept, darkSideWeight,.keep_all = TRUE)
exp3trainingAveGroup2 $darkSideWeight2 = as.numeric(exp3trainingAveGroup2 $darkSideWeight)

exp3trainingAveGroup3 <- exp3trainingAveGroup2  %>%
  group_by(darkSideWeight) %>%
  summarise(
    mseAve = mean(mse1)
  )  
exp3trainingAveGroup3$darkSideWeight2 = as.numeric(exp3trainingAveGroup3$darkSideWeight)


#Plot each concept and average MSE at each weight pair step.
plotTrainingMSE = ggplot() +
  geom_point(data = exp3trainingAveGroup2, aes(x = darkSideWeight2, y =mse1, shape=factor(concept), color = factor(concept)), size =3, alpha = .3) +  
  geom_point(data = exp3trainingAveGroup3, aes(x = darkSideWeight2, y =mseAve), size =3, shape = 16) +  
  scale_shape_manual(values = c(17,15, 18))+
  scale_color_manual(values = c("red", "cyan", "blue"))+
  ylim(0,1) +
  xlim(0,1)+
  coord_fixed()+
  labs(title = "MSE for all concepts",
       x = "weight for darkSide  (weight for assoc SD = 1-darkSide)",
       y = "mean squared error") 
plotTrainingMSE





# exp3: TESTING SET -------------------------------------------------------

#testing set -  assign-infer-colormaps\dataSets\colormapInterpretations folder
exp3testing = read.csv("exp3maps-testingSet.csv")

keep = c("subjectID", "concept", "xPair", "sidePress", "seDark", "seLeft", "meanAssocDiff", "assocOuter1", "assocOuter2", "assocInner1", "assocInner2","meanPropLeft","meanPropDark", "darkSideC3","lightrgb", "darkrgb", "semDistAssocSSNorm",  "darknessSSNorm", "assignSemDistAssocSSNorm")
exp3testing = exp3testing[keep]
dEachSide = exp3testing %>% distinct(concept, xPair, darkSideC3, .keep_all = T)
dEachXPair = exp3testing %>% distinct(concept, xPair, .keep_all = T)

#color pairs in Banner Figure 1 
dEachBanner = dEachXPair[dEachXPair$xPair == 262 | dEachXPair$xPair == 1019,]



#Testing set averaged over participants, with combined semantic distance for each weight pair-  assign-infer-colormaps\dataSets\colormapInterpretations folder
#Note: To obtain the combined semantic distance, use the matlab file: assign-infer-semanticDistance-main
exp3testingAve = read.csv("exp3maps-testingSet-ave.csv")


#Group by concept and weight pair to get mse
exp3testingAveGroup <- exp3testingAve  %>%
  group_by(xPair, darkSideWeight, concept) %>%
  summarise(
    mse1 = mean(se1))  

exp3testingAveGroup2 <- exp3testingAveGroup %>%
  group_by(darkSideWeight) %>%
  summarise(
    sdWeight = sd(mse1, na.rm =T), 
    mseAve = mean(mse1),
    nWeight = 63,
    seWeight = sdWeight/sqrt(nWeight))  


exp3testingAveGroup3 <- exp3testingAveGroup %>%
  group_by(concept, darkSideWeight) %>%
  summarise(
    sdWeight = sd(mse1, na.rm =T), 
    mseAve = mean(mse1),
    nWeight = 63,
    seWeight = sdWeight/sqrt(nWeight))  



#Plot MSE for each concept and average, for weight on all dark-is-more, all direct associations, and the optimal pair
plotTestMSE = ggplot() +
  geom_point(data = exp3testingAveGroup3, aes(x = darkSideWeight, y =mseAve, color = concept), size =3, shape = 16, alpha = .5) + 
  geom_errorbar(data = exp3testingAveGroup3, aes(x = darkSideWeight, y =mseAve, ymin = mseAve-seWeight, ymax=mseAve+seWeight, color = concept), width = .06, size = .4, alpha = .5) +  
  geom_point(data = exp3testingAveGroup2, aes(x = darkSideWeight, y =mseAve), size =4, shape = 16) + 
  geom_errorbar(data = exp3testingAveGroup2, aes(x = darkSideWeight, y =mseAve, ymin = mseAve-seWeight, ymax=mseAve+seWeight), width = .08, size = .5) +  
  scale_color_manual(values = c("red", "cyan", "blue"))+
  ylim(0,1.05) +
  xlim(-.05,1.05)+
  coord_fixed()+
  labs(title = "MSE for all concepts - testing",
       x = "weight for darkSide  (weight for assoc SD = 1-darkSide)",
       y = "mean squared error") 
plotTestMSE




#MODEL PREDICTING MSE
exp3testingAveGroup$conceptC1= ifelse(exp3testingAveGroup$concept == "fire", 2/3, -1/3)
exp3testingAveGroup$conceptC2= ifelse(exp3testingAveGroup$concept == "water", .5,
                       ifelse(exp3testingAveGroup$concept == "ice",-.5, 0))
exp3testingAveGroup$weightC1= ifelse(exp3testingAveGroup$darkSideWeight ==  .3, 2/3, -1/3)
exp3testingAveGroup$weightC2= ifelse(exp3testingAveGroup$darkSideWeight == 0, .5,
                      ifelse(exp3testingAveGroup$darkSideWeight == 1,-.5, 0))
exp3testingAveGroup$conceptContrast = (cbind(exp3testingAveGroup$conceptC1, exp3testingAveGroup $conceptC2))
exp3testingAveGroup$weightContrast = (cbind(exp3testingAveGroup$weightC1, exp3testingAveGroup $weightC2))

#model
control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6),check.nobs.vs.nRE = "ignore") 
mMSE = lmer(mse1 ~ conceptContrast*weightContrast + (1+weightContrast|xPair), data = exp3testingAveGroup, control = control)
summary(mMSE)
Anova(mMSE, type = 3, test = "F")



#T-TESTS TO COMPARE MSE FOR OPTIMAL PAIR VS. DARK-ONLY VS. DIRECT ASSOCIATIONS ONLY
dAssocOnly = exp3testingAveGroup[exp3testingAveGroup$darkSideWeight == 0,]
dAssocMean = mean(dAssocOnly$mse1)

dDarkOnly = exp3testingAveGroup[exp3testingAveGroup$darkSideWeight == 1,]
dDarkMean = mean(dDarkOnly$mse1)

dComboOnly = exp3testingAveGroup[exp3testingAveGroup$darkSideWeight == .3,]
dComboMean = mean(dComboOnly$mse1)

#Compare optimal to direct associations only
t.test(dComboOnly$mse1, dAssocOnly$mse1, var.equal = T)

#Compare optimal to dark-is-more bias only
t.test(dComboOnly$mse1, dDarkOnly$mse1, var.equal = T)


#Organize for plotting the optimal weight pair
weightSets = exp3testingAve
weightSets$lightrgb = as.character(weightSets$lightrgb)
weightSets$darkrgb = as.character(weightSets$darkrgb)
weightSetsOpt = weightSets[weightSets$darkSideWeight ==.3,]
weightSetsOpt <- weightSetsOpt %>% arrange(semDistComboM1)
weightSetsOpt= tibble::rowid_to_column(weightSetsOpt, "plotID")
lightrgbArray_combo <-  dplyr::pull(weightSetsOpt, lightrgb)
darkrgbArray_combo <- dplyr::pull(weightSetsOpt, darkrgb)


#Plot mean proportion of times darker side was selected as a function of combined signed semantic distance
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
  labs(title = "weight pairs- mean prop dark-optimal pair",
       x = "combined signed semantic distance",
       y = "mean prop. darker side selected") 
plotSetWeightsOpt



#Fire
weightSetsFire = weightSets[weightSets$concept == "fire",]
weightSetsFire <- weightSetsFire %>% arrange(semDistComboM1)
weightSetsFire3 = weightSetsFire[weightSetsFire$darkSideWeight == .3,]
corr.test(weightSetsFire3$semDistComboM1, weightSetsFire3$meanPropDark)


#Ice
weightSetsIce = weightSets[weightSets$concept == "ice",]
weightSetsIce <- weightSetsIce %>% arrange(semDistComboM1)
weightSetsIce3 = weightSetsIce[weightSetsIce$darkSideWeight == .3,]
corr.test(weightSetsIce3$semDistComboM1, weightSetsIce3$meanPropDark)


#Water
weightSetsWater = weightSets[weightSets$concept == "water",]
weightSetsWater <- weightSetsWater %>% arrange(semDistComboM1)
weightSetsWater3 = weightSetsWater[weightSetsWater$darkSideWeight == .3,]
corr.test(weightSetsWater3$semDistComboM1, weightSetsWater3$meanPropDark)



