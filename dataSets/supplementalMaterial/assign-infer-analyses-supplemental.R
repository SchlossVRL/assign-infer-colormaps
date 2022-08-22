

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
library(lmerTest)
library(ggpubr)


# SUPPLEMENTAL: MONOTONICITY CHECK ----------------------------------------

#use the regressionLCH-xpairs.csv file plus the files
# allIcePairs.csv and iceMeanAssoc.csv in the regression check


#Load UW-71 color coordinates - housed in assign-infer-colormaps\stimuli\ folder
d71 <- read_csv("UW71coordinates.csv")
d71 = d71 %>%arrange(color_index)


#read in the interpolated color pair coordinates in assign-infer-colormaps\supplementalMaterial\monotonicityRegression
dxPairs = read_csv("RegressionLCH-xPairs.csv")#in this csv file, xPairs corresponds to the unique color pair. colorSeqNum == 1 is the light color of the pair, == 10 is the dark color of the pair. 2-9 are the interpolated color items
dxPairs = tibble::rowid_to_column(dxPairs, "rowNum")


#Change from degrees to radians
dxPairs$Hrad = dxPairs$H * ((pi)/180)


#load concept data - uncomment desired concept file - assign-infer-colormaps\supplementalMaterial\monotonicityRegression
dPairs = read_csv("allIcePairs.csv")
#dPairs = read_csv("allFirePairs.csv")
#dPairs = read_csv("allWaterPairs.csv")
#dPairs = read_csv("allSunshinePairs.csv")
#dPairs = read_csv("allShadePairs.csv")
#dPairs = read_csv("allSoilPairs.csv")
#dPairs = read_csv("allFoliagePairs.csv")

dPairs = dPairs %>%arrange(xPair)


#Load in the mean association ratings - uncomment desired concept file-  assign-infer-colormaps\dataSets\associationRatings\meanAssoc
ConceptAssoc = read_csv("iceMeanAssoc.csv")
#ConceptAssoc = read_csv("fireMeanAssoc.csv")
#ConceptAssoc = read_csv("waterMeanAssoc.csv")
#ConceptAssoc = read_csv("sunMeanAssoc.csv")
#ConceptAssoc = read_csv("shadeMeanAssoc.csv")
#ConceptAssoc = read_csv("soilMeanAssoc.csv")
#ConceptAssoc = read_csv("foliageMeanAssoc.csv")

ConceptAssoc = ConceptAssoc %>%arrange(color_index)
cor(ConceptAssoc$mean_response, ConceptAssoc$L)


#Merge datasets together 
dConcept = merge(d71, ConceptAssoc, c("color_index", "v_index", "L","a","b","C","H","r_rgb","g_rgb","b_rgb","r255","g255","b255","color_hex"))
dConcept = dConcept %>%arrange(color_index)

#Change from degrees to radians
dConcept$Hrad = dConcept$H * ((pi)/180)



#Supp: Regression predicting associations --------------------------------------

#predict mean color-concept association ratings from L, C, sin(h), cos(h), sin(2h), cos(2h)
mConcept = lm(mean_response  ~ L + C + sin(Hrad) + cos(Hrad) + sin(2*Hrad) + cos(2*Hrad), data = dConcept)
summary(mConcept)

constants = data.frame(mConcept$coefficients)
predRating <-  predict(mConcept,dConcept)
cor(dConcept$mean_response, predRating) 
r.squaredLR(mConcept)
plot(dConcept$mean_response, predRating)

#Prepare for plotting
ConceptPred = data.frame(predRating)
ConceptPred2 = tibble::rowid_to_column(ConceptPred, "rowNum")
dConcept$rowNum = dConcept$color_index + 1
ConceptPred2 = merge(dConcept, ConceptPred2, by = "rowNum")
ConceptPred2 = ConceptPred2 %>%arrange(color_index)
colorHex <-  dplyr::pull(ConceptPred2,color_hex)

#Create plot
plotConcept = ggplot(data = ConceptPred2) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - predRating),  y = predRating, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Pred. Assoc - Concept",
       y = " pred. assoc.",
       x = "color")+
  theme(legend.position = "none") 
plotConcept

#Get predictions for interpolated colors
predRating <-  predict(mConcept,dxPairs)
result = data.frame(predRating)
result2 = tibble::rowid_to_column(result, "rowNum")
result2 = merge(dxPairs, result2, by = "rowNum")
result2 = result2 %>% arrange(xPair)


#create table with real endpoint values 
resultReal = result2[result2$colorSeqNum == 1 | result2$colorSeqNum == 10, ]
resultReal = merge(resultReal, dPairs, by = "xPair")
resultReal$realRating = ifelse(resultReal$colorSeqNum == 1, resultReal$lightAssoc, 
                               ifelse(resultReal$colorSeqNum == 10, resultReal$darkAssoc, 0))
resultReal <- resultReal %>% arrange(xPair)


rPair = c(1)
regressTable = data.frame(rPair)
regressLoop = data.frame(rPair)
regressTable = data.frame(matrix(ncol = 4, nrow = 0))
formula = y ~ x


#Loop through to get table with 10 steps per color pair
for (id in 1:1848){
  cp1 = slice(result2, (1+(id-1)*10):((id*10)))
  cp2 = resultReal[resultReal$xPair == id,]
  
  fig =  ggplot() +
    geom_point(data = cp1, aes(x = colorSeqNum, y = predRating), color = "blue") +
    geom_point(data = cp2, aes(x = colorSeqNum, y = realRating), color = "black") +
    geom_line(data = cp2, aes(x= colorSeqNum, y = realRating), color = "black") + 
    stat_smooth(data = cp1, aes(x= colorSeqNum, y = predRating), method = lm, se=FALSE, fullrange = TRUE, formula = formula) +
    stat_regline_equation(data = cp1,  aes(x= colorSeqNum, y = predRating, label =  paste(..adj.rr.label.., sep = "~~~~")),
                          formula = formula, color = "blue") +
    ylim(-.1,1) +
    ggtitle( paste("Concept assoc.- xPair:", id))+
    labs(y = "Predicted rating",
         x = "color seq Num")
  
  fig
  
  output = ggplot_build(fig)$data[[5]]
  
  regressLoop$id = id
  regressLoop$rsquareConcept = output$adj.rr
  
  regressLoop$xPair = cp1$xPair[1]
  regressLoop$xPair2 = cp2$xPair[1]
  
  #update the main table
  regressTable <- rbind(regressTable, regressLoop)
  
  #Save the figure
  #ggsave(fig,filename=paste("ConceptAssoc-",id,".png",sep=""), width = 3, height = 3) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
  
}

regressTable
cp1
cp2
fig

dfullConcept = merge(regressTable, dPairs, by = "xPair")


#Filter to concepts with rsquare greater than or equal to .8 & with Lightness difference > 25
dfullConcept8 = dfullConcept[dfullConcept$rsquareConcept >= .8,]
dfullConceptdiff = dfullConcept[dfullConcept$diffLighting > 25, ]
dfullConcept8diff = dfullConcept8[dfullConcept8$diffLighting > 25, ]


#Repeat for other concepts



# SUPPLEMENTAL: ADDITIONAL DATA FOR ESTIMATING MERIT ---------------------


# Supp: Merit for direct associations -------------------------------------

# See environment endpoint association ratings files & semantic distance matlab script



# Supp: Merit for the dark-is-more bias -----------------------------------


#Read in raw darkness ratings data -  assign-infer-colormaps\supplementalMaterial
d3 = read.csv("exp3maps-darknessRatings.csv" )


#get average for each participants
d4 <- d3 %>%
  group_by(subjectID, xPair) %>%
  summarise(
  meanDarkResponse = mean(darkResponse))

#get average for each xpair (color pair)
d6 <- d4 %>%
  group_by(xPair) %>%
  summarise(
    sdDarkness = sd(meanDarkResponse),
    meanDarkness = mean(meanDarkResponse),
    Ndark = length(subjectID),
    seDarkness = sdDarkness/sqrt(Ndark))


#remove catch trials
d7= d6[d6$xPair != "darknessCheck1" & d6$xPair != "darknessCheck2" & d6$xPair != "darknessCheck3" ,]


#Table d7 now contains the meanDarkness information for each color pair (xPair)






# SUPPLEMENTAL: ADDITIONAL ANALYSES FOR UNDERSTANDING COLORMAP INTERPRETATIONS ----

# Supp: Exp1 - Colormap interpretations -----------------------------------


#Load in exp1maps - housed in assign-infer-colormaps\datasets\colormapInterpretations folder
dexp1maps =  read.csv("exp1maps.csv")


#Model predicting side pressed from which side was dark, mean association difference, and concept
mH2 <- glmer(sidePress ~ darkSideC*meanAssocDiffC*conceptC + (1|subjectID), data = dexp1maps, family = "binomial")
summary(mH2)


#Model for shade
dshade = dexp1maps[dexp1maps$conceptStr == "shade",]
mHshade <- glmer(sidePress ~ darkSideC*meanAssocDiffC + (1|subjectID), data = dshade, family = "binomial")
summary(mHshade)


#Model for sunshine
dsunshine = dexp1maps[dexp1maps$conceptStr == "sunshine",]
mHsun <- glmer(sidePress ~ darkSideC*meanAssocDiffC + (1|subjectID), data = dsunshine, family = "binomial")
summary(mHsun)




# Supp: Exp3 - Colormap interpretations ------------------------------------


#load mean darkness ratings - -  assign-infer-colormaps\dataSets\supplementaMaterial folder
dDarkness = read.csv("exp3maps-darknessRatings-ave.csv" )

#load weight pairs for training -  assign-infer-colormaps\dataSets\colormapInterpretations folder
dWeightTrain = read.csv("weightPairsTraining.csv")

#load weight pairs for testing -  assign-infer-colormaps\dataSets\colormapInterpretations folder
dWeightTest = read.csv("weightPairsTesting.csv")




#load full exp3 set-  assign-infer-colormaps\dataSets\colormapInterpretations folder
dexp3maps = read.csv("exp3maps-fullSet.csv")
dexp3maps = merge(dDarkness, dexp3maps, by =c("xPair", "concept", "lightrgb", "darkrgb"))

#Center mean darkness
dexp3maps$darknessSS = dexp3maps$meanDarkness * dexp3maps$darkSideC3 

#get outer and inner weights
dexp3maps$Outer1 = dexp3maps$lightAssoc_no
dexp3maps$Outer2 = dexp3maps$darkAssoc_lot
dexp3maps$Inner1 = dexp3maps$darkAssoc_no
dexp3maps$Inner2 = dexp3maps$lightAssoc_lot

#code based on solution to the assignment problem
dexp3maps$semDistAssoc = dexp3maps$SemDist
dexp3maps$assign = ifelse((dexp3maps$Inner1 + dexp3maps$Inner2 ) < (dexp3maps$Outer1 + dexp3maps$Outer2), 1,-1)
dexp3maps$assignSemDistAssocSignAssocSide = dexp3maps$assign * dexp3maps$SemDist
dexp3maps$match =  ifelse(dexp3maps$xPair == 350 | dexp3maps$xPair == 1125, -1, 1)
dexp3maps$assignSemDistAssocSS = ifelse(dexp3maps$match == 1, dexp3maps$semDistAssoc * dexp3maps$associatedSide, dexp3maps$semDistAssoc * dexp3maps$associatedSide*-1)

#norm data to be between 1 and -1 for models
dexp3maps$darknessSSNorm = round(2*(((dexp3maps$darknessSS - min(dexp3maps$darknessSS))/(max(dexp3maps$darknessSS)-min(dexp3maps$darknessSS))))-1,4)
dexp3maps$assignSemDistAssocSSNorm = round(2*(((dexp3maps$assignSemDistAssocSS - min(dexp3maps$assignSemDistAssocSS))/(max(dexp3maps$assignSemDistAssocSS)-min(dexp3maps$assignSemDistAssocSS))))-1,4)


#Model predicting side pressed from association sem. dist, and mean darkness
gcontrol=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e6),check.nobs.vs.nRE = "ignore") 
m1assign <- glmer(sidePress ~ assignSemDistAssocSSNorm+darknessSSNorm+ (assignSemDistAssocSSNorm+darknessSSNorm|subjectID), data = dexp3maps, family = "binomial", control = gcontrol)
summary(m1assign)

#Note: See assign-infer-analyses-main.R file for analyses for model predicting MSE


