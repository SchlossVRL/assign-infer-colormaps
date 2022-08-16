

############################################################################################
### Unifying effects of Direct and Relational Associations for Visual Communication
###
###                             SUPPLEMENTAL MATERIAL
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



# SUPPLEMENTAL: ADDITIONAL DATA FOR ESTIMATING MERIT ------------------------------




# Supp: ADDITIONAL ANALYSES FOR UNDERSTANDING COLORMAP INTERPRETATIONS ----

# Supp: EXPERIMENT 1 - COLORMAPS INTERPRETATIONS --------------------------------

setwd("C:/Users/melan/Dropbox/Research/Manuscripts/Submitted/SunshineMaps-manuscript/VIS/finalAnalysesCode/dataSets/colormapInterpretations")
dexp1maps =  read.csv("exp1maps1.csv")


dall = dmap1

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





# Supp analyses: Exp3 ------------------------------------------------------------

setwd("C:/Users/melan/Dropbox/Research/Manuscripts/Submitted/SunshineMaps-manuscript/VIS/finalAnalysesCode/dataSets/colormapInterpretations")

#load mean darkness ratings
dDarkness = read.csv("exp3maps-darknessRatings.csv" )

#load weight paris for training
dWeightTrain = read.csv("weightPairsTraining.csv")

#load weight paris for testing
dWeightTest = read.csv("weightPairsTesting.csv")


#See assign-infer-analyses-main.R file for analyses for full model predicting MSE for each
#color scale with fixed effects for 



d4 = read.csv("exp3maps-fullSet.csv") #fulll
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


keep = c( "concept", "xPair", "meanAssocDiff", "lightAssoc_no", "lightAssoc_lot", "darkAssoc_no", "darkAssoc_lot","meanPropDark","lightrgb", "darkrgb", "semDistAssocSSNorm", "semDistAssocSignAssocSide","assignSemDistAssocSignAssocSide","semDistAssocSS", "assignSemDistAssocSS", "assignSemDistAssocSSNorm", "match")
dcheck = d4[keep]
dcheck = dcheck[!duplicated(dcheck$assignSemDistAssocSignAssocSide),]
#dcheck2 = dcheck[(dcheck$xPair == 350 | dcheck$xPair == 1125),]


keep = c("subjectID", "concept", "xPair", "sidePress", "oviT", "oviTOuter", "oviTInner","seDark", "seLeft", "meanAssocDiff", "assocOuter1", "assocOuter2", "assocInner1", "assocInner2","meanPropLeft","meanPropDark", "darkSideC3","lightrgb", "darkrgb", "semDistAssocSSNorm", "semDistOviSSNorm", "semDistAssocSignAssocSide", "darknessSSNorm",  "assignSemDistAssocSignAssocSide", "assignSemDistAssocSSNorm")
d5 = d4[keep]

dAllTrials = d5


#use full data set
dAllTrials


m1assign <- glmer(sidePress ~ assignSemDistAssocSSNorm+darknessSSNorm+ (assignSemDistAssocSSNorm+darknessSSNorm|subjectID), data = dAllTrials, family = "binomial", control = gcontrol)
summary(m1assign)
#(Intercept)               -0.3374     0.1632  -2.067   0.0387 *  
#assignSemDistAssocSSNorm  16.9128     1.3464  12.561   <2e-16 ***
#darknessSSNorm            13.6635     1.2519  10.914   <2e-16 ***


#USE full data set or testing?? 




#correlations between lightness and UW 71 colors for each concept 
