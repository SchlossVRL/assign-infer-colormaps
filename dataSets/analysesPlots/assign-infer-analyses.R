
#Load in packages
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
#setwd ("C:/Users/johnny/OneDrive/Desktop/prep/experiment1_data")
setwd("C:/Users/melan/Dropbox/Research/Experiments/ColormapSunshine-PREP2021/ColormapsAnalyses-sunshineShade8-Exp1")

exp1 <- read.csv("Csunshine-colormapsRawOutput.csv")
mapData = read.csv("final8colormaps.csv")


#condition 0: shade, L38_ID_2, high diff
#condition 1: shade, L38_ID_10, high diff
#condition 2: shade, L38_ID_134, low diff
#condition 3: shade, L38_ID_184, low diff
#condition 4: shade, L50_ID_1, high diff
#condition 5: shade, L50_ID_3, high diff
#condition 6: shade, L50_ID_220, low diff
#condition 7: shade, L50_ID_222, low diff

#condition 8: sunshine, L38_ID_2, high diff
#condition 9: sunshine, L38_ID_10, high diff
#condition 10: sunshine, L38_ID_134, low diff
#condition 11: sunshine, L38_ID_184, low diff
#condition 12: sunshine, L50_ID_1, high diff
#condition 13: sunshine, L50_ID_3, high diff
#condition 14: sunshine, L50_ID_220, low diff
#condition 15: sunshine, L50_ID_222, low diff


#filter to trial_index > 1 so excludes people who didnt finsih
exp1noFinish= exp1[exp1$trial_index == 0, ]
exp1= exp1[exp1$trial_index > 0, ]

exp1count = exp1[!duplicated(exp1$subjectID),]
#190 start



# Color vision  --------------------------------------------------------------------

#Change response to character
exp1$responses = as.character(exp1$responses)
exp1$subjectID = as.character(exp1$subjectID)



# Check for colorblindness
exp1Colorblind <- filter(exp1, str_detect(exp1$responses, "colorDifficulty")) 
exp1Colorblind$colorblind <- ifelse(str_detect(exp1Colorblind$responses, "Yes"), 1,0 )
exp1Colorblind = exp1Colorblind[exp1Colorblind$colorblind == 1,]
exp1Colorblind$subjectID
dontuseColorblind <- data.frame(subjectID = exp1Colorblind$subjectID) #c(4,6), blah = c(1,2))


#Remove subjects reporting "Yes" to both/either colorblindness question
exp1 = anti_join(exp1, dontuseColorblind )
#Removes  3 people

#187 total
dall2 = exp1[!duplicated(exp1$subjectID),]
exp1countAll = exp1[!duplicated(exp1$subjectID),]
#removes 3 total participants





# Age ---------------------------------------------------------------------

d = exp1
#Filter to age rows 
dAge <-  filter(d, str_detect(d$responses, "Age"))

#Get the age values from the string response
dAge$Age <- str_sub(dAge$responses,9, 10) 
keep = c("subjectID", "Age")
Age <- dAge[keep]
Age$Age = as.numeric(Age$Age)
mean(Age$Age)

#Merge with main data
d <- merge(d,Age,by = "subjectID") 



# Gender ------------------------------------------------------------------

#Filter to age rows 
dGender <-  filter(d, str_detect(d$responses, "Gender"))

#Get the age values from the string response
dGender$Gender <- str_sub(dGender$responses,23,26) 
keep = c("subjectID", "Gender")
Gender <- dGender[keep]
female = Gender[Gender$Gender == 'F","' | Gender$Gender == "FEMA" | Gender$Gender == "Fema" |Gender$Gender == "fema" | Gender$Gender == "Woma" | Gender$Gender == "woma" , ]
male = Gender[Gender$Gender == 'M","'| Gender$Gender == 'm","' | Gender$Gender == "MALE" | Gender$Gender == "Male" |Gender$Gender == "male" | Gender$Gender == "MAle" , ]


#Merge with main data
d <- merge(d,Gender, by = "subjectID") 



# creating variables ------------------------------------------------------

d = exp1
d= d[d$trial_type== "image-keyboard-responseMAS-colormaps_jc",]

keep = c("subjectID", "conditionProto", "stimulus","prompt", "key_press", "darkSide", "colormap")
d = d[keep]

d$sidePress = ifelse(d$key_press == 37, 0, 1)  # Left = 0, right = 1. 

d$selectDark =ifelse(d$sidePress == d$darkSide, 1, 0) #select dark = 1, select light = 0. 
d$concept = ifelse(d$conditionProto <= 7, 0,1)  #shade = 0, sunshine = 1
d$assocDiff = ifelse(d$conditionProto <= 0 | d$conditionProto <= 1 | d$conditionProto == 4 | d$conditionProto == 5 | d$conditionProto == 8 |  d$conditionProto == 9 |  d$conditionProto == 12 |  d$conditionProto == 13,  1, 0) #high assoc diff group = 1, low = 0

d$ID = ifelse(d$conditionProto == 0 | d$conditionProto == 8, 2,
              ifelse(d$conditionProto == 1 |d$conditionProto == 9, 10,
                     ifelse(d$conditionProto == 2 | d$conditionProto == 10, 134,
                            ifelse(d$conditionProto == 3 |  d$conditionProto == 11, 184,
                                 ifelse(d$conditionProto == 4 | d$conditionProto == 12, 1,
                                        ifelse(d$conditionProto == 5 | d$conditionProto == 13, 3,
                                               ifelse(d$conditionProto == 6 | d$conditionProto == 14, 220,
                                                      ifelse(d$conditionProto == 7 | d$conditionProto == 15, 222,0)))))))) #high assoc diff group = 1, low = 0




# merge with colormapData -------------------------------------------------

d2 = merge(d, mapData, by = "ID")
d2$diffRatingShade2 = d2$diffRatingShade * - 1
d2$meanAssocDiff = (d2$diffRatingSun + d2$diffRatingShade2)/2

cor(d2$diffRatingSun, d2$diffRatingShade2)

d2$conceptStr = varRecode(d2$concept, c(0,1), c("shade", "sunshine"))
d2$assocDiffStr = varRecode(d2$assocDiff, c(0,1), c("low", "high"))
d2$assocDiffStr = factor(d2$assocDiffStr, levels = c("low", "high"))

# plots  ------------------------------------------------------------------


#obtain prop select dark for each subject
dsum <- d2 %>%
  group_by(subjectID) %>%
  summarise(
    totalDark = sum(selectDark),
    propDark = totalDark/10
    
  )
dsum

#merge so that propDark appears with all the rest of the data
dall = merge(dsum, d2, by = "subjectID")
dall2 = dall[!duplicated(dall$subjectID),]


#obtain average mean prop select dark
dsumPlot <- dall2 %>%
  group_by(conceptStr, assocDiffStr) %>%
  summarise(
    meanPropDark = mean(propDark),
  )
dsumPlot

#plot as high vs. low assoc diff maps for shade vs. sunshine
plot1  = ggplot(dsumPlot, aes(x = assocDiffStr, y = meanPropDark, color = conceptStr)) +
  geom_point(size = 4) +
  labs(title = "selecting darker side",
       x = "assoc Difference group",
       y = "mean Prop. select dark side") +
  ylim(0, 1)
plot1

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

#condition 0: shade, L38_ID_2, high diff  -- 
#condition 1: shade, L38_ID_10, high diff
#condition 2: shade, L38_ID_134, low diff
#condition 3: shade, L38_ID_184, low diff
#condition 4: shade, L50_ID_1, high diff
#condition 5: shade, L50_ID_3, high diff
#condition 6: shade, L50_ID_220, low diff
#condition 7: shade, L50_ID_222, low diff

#condition 8: sunshine, L38_ID_2, high diff
#condition 9: sunshine, L38_ID_10, high diff
#condition 10: sunshine, L38_ID_134, low diff
#condition 11: sunshine, L38_ID_184, low diff
#condition 12: sunshine, L50_ID_1, high diff
#condition 13: sunshine, L50_ID_3, high diff
#condition 14: sunshine, L50_ID_220, low diff
#condition 15: sunshine, L50_ID_222, low diff



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

#code with respect to left_ MElissa
dleft <- d2 %>%
  group_by(subjectID,signedMeanAssocDiff, darkSideC, conceptStr, ID, lightrgb, darkrgb) %>%
  summarise(
    totalLeft = sum(sidePress),
    propLeft = totalLeft/5
  )
dleft

dleftAve <- dleft %>%
  group_by(signedMeanAssocDiff, darkSideC, conceptStr, ID, lightrgb, darkrgb) %>%
  summarise(
    sd = sd(propLeft, na.rm = TRUE),
    meanPropLeft = mean(propLeft),
    N = length(subjectID),
    se = sd/sqrt(N)
  )
dleftAve

#plot data
actualGraph <- ggplot(dleftAve, aes(x= signedMeanAssocDiff, y= meanPropLeft, color = darkSideC)) +
  geom_point(aes(size = 1, shape = factor(conceptStr))) + 
  geom_errorbar(aes(ymax= meanPropLeft+se, ymin= meanPropLeft-se)) +
  geom_smooth()+
  coord_cartesian(ylim= c(-.1, 1.1), expand = F) +
  xlim(-1.05, 1.05)+
  labs(y= 'Proportion times selected left side', 
       x= 'mean association difference')+
  theme_pubr()+
  theme(legend.title = element_blank())+
  theme(text = element_text(size=12))
actualGraph




#plot with correct colors
map_Set = dleftAve %>% arrange(signedMeanAssocDiff)

mapSun = map_Set[map_Set$conceptStr == "sunshine",]
mapShade = map_Set[map_Set$conceptStr == "shade",]
mapSun$plotID = 1:nrow(mapSun)
mapSun$plotID = as.numeric(mapSun$plotID)
mapShade$plotID = 1:nrow(mapShade)
mapShade$plotID = as.numeric(mapShade$plotID)

mapSun2 = mapSun %>% arrange(plotID)
mapShade2 = mapShade %>% arrange(plotID)
combo = rbind(mapSun2, mapShade2)
combo$lightrgb = as.character(combo$lightrgb)
combo$darkrgb = as.character(combo$darkrgb)

lightrgbArray_combo <- dplyr::pull(combo, lightrgb)
darkrgbArray_combo <- dplyr::pull(combo, darkrgb)


actualGraphC2 <- ggplot() +
  geom_smooth(data = dleftAve, aes(x= signedMeanAssocDiff, y= meanPropLeft, group = darkSideC, alpha= factor(darkSideC)))+  
  geom_errorbar(data = mapSun, aes(x= signedMeanAssocDiff, y= meanPropLeft, ymax= meanPropLeft+se, ymin= meanPropLeft-se, alpha= factor(darkSideC)),  width = .1, size = .5) +
   geom_errorbar(data = mapShade, aes(x= signedMeanAssocDiff, y= meanPropLeft, ymax= meanPropLeft+se, ymin= meanPropLeft-se, alpha= factor(darkSideC)),  width = .1, size = .5) +
   geom_point(data = mapSun, aes(x= signedMeanAssocDiff, y= meanPropLeft,  color = factor(plotID), fill = factor(plotID), shape = factor(conceptStr), alpha= factor(darkSideC)), size = 3, stroke = 3) + 
  geom_point(data = mapShade, aes(x= signedMeanAssocDiff, y= meanPropLeft,color = rev(factor(plotID)), fill = rev(factor(plotID)), shape = factor(conceptStr), alpha= factor(darkSideC)),size = 3, stroke = 3) + 
  scale_shape_manual(values = c(21,24))+
  scale_fill_manual(values= lightrgbArray_combo) + 
  scale_colour_manual(values= darkrgbArray_combo) +
  scale_alpha_manual(values = c(.15, .8))+
  coord_cartesian(ylim= c(-.1, 1.1), expand = F) +
  xlim(-1.05, 1.05)+
  labs(y= 'Proportion times selected left side', 
       x= 'mean association difference')+
  theme_pubr()+
  theme(legend.title = element_blank())+
  theme(text = element_text(size=12))
actualGraphC2





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
#dall$sideAssociationDiffC = (dall$sideAssociationDiff) - mean(dall$sideAssociationDiff)
dall$sideAssocDiffNorm = round(2*(((dall$sideAssociationDiff - min(dall$sideAssociationDiff))/(max(dall$sideAssociationDiff)-min(dall$sideAssociationDiff))))-1,4)



#NEW model : 
mNew <- glmer(sidePress ~ darkSideC2+sideAssociationDiff + (1|subjectID), data = dall, family = "binomial")
summary(mNew)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          0.04257    0.07297   0.583     0.56    
#darkSideC2           1.32978    0.08601  15.461   <2e-16 ***
#sideAssociationDiff  5.03361    0.23726  21.215   <2e-16 ***

mNew2 <- glmer(sidePress ~ darkSideC2+sideAssocDiffNorm + (1|subjectID), data = dall, family = "binomial")
summary(mNew2)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        0.04257    0.07297   0.583     0.56    
#darkSideC2         1.32985    0.08601  15.461   <2e-16 ***
# sideAssocDiffNorm  4.51181    0.21267  21.215   <2e-16 ***



# previous modesl ---------------------------------------------------------


#write.csv(dall, "Csunshine-colormapsProcessed.csv")

control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6),check.nobs.vs.nRE = "ignore") 


#this is our MODEL 
mH2 <- glmer(sidePress ~ darkSideC*meanAssocDiffC*conceptC + (1|subjectID), data = dall, family = "binomial")
summary(mH2)
#(Intercept)                        0.06452    0.10329   0.625    0.532    
#darkSideC                          1.78457    0.20658   8.639  < 2e-16 ***
#meanAssocDiffC                     0.29334    0.32998   0.889    0.374    
#conceptC                          -0.17008    0.20658  -0.823    0.410    
#darkSideC:meanAssocDiffC          -4.82114    0.66011  -7.303  2.8e-13 ***
#darkSideC:conceptC                 9.16355    0.41319  22.178  < 2e-16 ***
#meanAssocDiffC:conceptC           -0.20124    0.66009  -0.305    0.760    
#darkSideC:meanAssocDiffC:conceptC 11.66667    1.32078   8.833  < 2e-16 ***


#run separately for shade vs. sunshine
dshade = dall[dall$conceptStr == "shade",]

mHshade <- glmer(sidePress ~ darkSideC*meanAssocDiffC + (1|subjectID), data = dshade, family = "binomial")
summary(mHshade)
#(Intercept)              -0.02052    0.16957  -0.121    0.904    
#darkSideC                 6.36634    0.33913  18.772   <2e-16 ***
#meanAssocDiffC            0.19272    0.54197   0.356    0.722    
#darkSideC:meanAssocDiffC  1.01219    1.08402   0.934   0.350  


dsunshine = dall[dall$conceptStr == "sunshine",]

mHsun <- glmer(sidePress ~ darkSideC*meanAssocDiffC + (1|subjectID), data = dsunshine, family = "binomial")
summary(mHsun)
#(Intercept)                0.1496     0.1180   1.268    0.205    
#darkSideC                 -2.7972     0.2360 -11.855   <2e-16 ***
#meanAssocDiffC             0.3940     0.3766   1.046    0.296    
#darkSideC:meanAssocDiffC -10.6545     0.7531 -14.147   <2e-16 ***


#test if sunshine low assoc is sig greater than chance (does darkSide predict sidepress?
dsunshinelow = dsunshine[dsunshine$assocDiffC < .5,]
mHsunlow <- glmer(sidePress ~ darkSideC + (1|subjectID), data = dsunshinelow, family = "binomial")
summary(mHsunlow)
#darkSideC    0.56561    0.18638   3.035  0.00241 **





# investigating low sunshine maps -----------------------------------------

dsunshinelow2 = dsunshinelow[!duplicated(dsunshinelow$subjectID),]
hist(dsunshinelow2$propDark)

dsunlow2purple = dsunshinelow2[dsunshinelow2$conditionProto == 14,]
hist(dsunlow2purple$propDark)

dsunlow2teal = dsunshinelow2[dsunshinelow2$conditionProto == 10,]
hist(dsunlow2teal$propDark)

dsunlow2pink = dsunshinelow2[dsunshinelow2$conditionProto == 15,]
hist(dsunlow2pink$propDark)

dsunlow2green = dsunshinelow2[dsunshinelow2$conditionProto == 11,]
hist(dsunlow2green$propDark)







# predicting proporiton select darker from assoc vs. concept --------------

#catbound
d2 <- dall %>%
  group_by(conceptC, assocDiffC, subjectID) %>%
  summarise(
    sd = sd(propDark, na.rm = TRUE),
    meanPropDark = mean(propDark),
    N = length(subjectID),
    se = sd/sqrt(N)
  )
d2


m12 <- lm(meanPropDark ~ assocDiffC*conceptC, data = d2)
modelSummary(m12)
#assocDiffC          -0.25831  0.03831 -6.743 1.97e-10 ***
#conceptC             0.65059  0.03831 16.983  < 2e-16 ***
#assocDiffC:conceptC  0.52798  0.07661  6.891 8.63e-11 ***

dsunshine2 = d2[d2$conceptC == -.5,]
dshade2 = d2[d2$conceptC == .5,]

m12sun <- lm(meanPropDark ~ assocDiffC, data = dsunshine2)
modelSummary(m12sun)
#assocDiffC  -0.52230  0.07057 -7.401 5.92e-11 ***

m12shade <- lm(meanPropDark ~ assocDiffC, data = dshade2)
modelSummary(m12shade)
#assocDiffC  0.005682 0.027306  0.208    0.836 




# old ---------------------------------------------------------------------


m1Side <- glmer(selectDark ~ assocDiffC*conceptC + (1|subjectID), data = dall, family = "binomial")
summary(m1Side)
#(Intercept)                 4.837      0.838   5.772 7.83e-09 ***
#assocDiffCenter            -9.241      1.669  -5.536 3.10e-08 ***
#conceptC                  -12.682      1.513  -8.380  < 2e-16 ***
#assocDiffCenter:conceptC  -19.040      3.321  -5.732 9.90e-09 ***

# significnat  p<.05 * , p<.01** , p<.001 *** 

dsunshine3 = dall[dall$conceptC == -.5,]
dshade3 = dall[dall$conceptC == .5,]

m13sun <- glmer(selectDark ~ assocDiffC +(1|subjectID), data = dsunshine3, family = "binomial")
summary(m13sun)
#assocDiffC  -20.7996     2.6862  -7.743 9.69e-15 ***
  
m13shade <-glmer(selectDark ~ assocDiffC +(1|subjectID), data = dshade3, family = "binomial")
summary(m13shade)
#assocDiffC    0.2888     1.5660   0.184    0.854    





m1SideCont <- glmer(selectDark ~ meanAssocDiffC*conceptC + (1|subjectID), data = dall, family = "binomial")
summary(m1SideCont)
#(Intercept)               4.0312     0.8187   4.924 8.49e-07 ***
#meanAssocDiffC          -14.1890     2.8919  -4.906 9.27e-07 ***
#conceptC                 13.6314     1.5583   8.747  < 2e-16 ***
#meanAssocDiffC:conceptC  29.8743     5.8169   5.136 2.81e-07 ***


mH1 <- glmer(sidePress ~ darkSideC + (1|subjectID), data = dall, family = "binomial")
summary(mH1)
#darkSideC    1.04573    0.09568  10.929   <2e-16 ***


