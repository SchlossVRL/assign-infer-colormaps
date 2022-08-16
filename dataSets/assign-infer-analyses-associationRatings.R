
############################################################################################
### Unifying effects of Direct and Relational Associations for Visual Communication
###
###                             ASSOCIATION RATINGS 
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

color_index <- read_csv("UW71coordinates.csv")


# EXPERIMENT 1 & 2 - ASSOCIATION RATINGS --------------------------------

setwd("C:/Users/melan/Dropbox/Research/Manuscripts/Submitted/SunshineMaps-manuscript/VIS/finalAnalysesCode/dataSets/associationRatings")
dexp1assoc = read.csv("exp1-assoc2.csv")

exp1 = dexp1assoc


#add symbol to rgb to be read correctly in plots
exp1$color_rgb <- sub("^", "#", exp1$color_rgb)



# exp1 & 2: SUNSHINE ----------------------------------------------------------


#table with all relevant sunshine data
sunshines <- exp1 %>% filter(prompt == "sunshine") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(rt, response, prompt, color_rgb, color_index, newresponse) 

#sunshine mean assoc
sunshine_mean_assoc <- sunshines %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
sunshine_mean_assoc = sunshine_mean_assoc[keep]
sunshine_mean_assoc <- merge(sunshine_mean_assoc, color_index)



SunshineAssoc = sunshine_mean_assoc
SunshineAssoc = SunshineAssoc %>%arrange(color_index)
SunshineAssoc$color_hex = as.character(SunshineAssoc$color_hex)
colorHex <-  dplyr::pull(SunshineAssoc,color_hex)

corr.test(SunshineAssoc$mean_response, SunshineAssoc$L)
#r = .71, p<.001


plotSunshine = ggplot(data = SunshineAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - Sunshine",
       y = "mean assoc.",
       x = "color") +
  ylim(0,1)+
  theme(legend.position = "none") 
plotSunshine

#write.csv(sunshine_mean_assoc, "sunMeanAssoc.csv")



# exp1: SHADE  ----------------------------------------------------------

shades <- exp1 %>% filter(prompt == "shade") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(rt, response, prompt, color_rgb, color_index, newresponse)

#shade mean assoc
shade_mean_assoc <- shades %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
shade_mean_assoc = shade_mean_assoc[keep]
shade_mean_assoc <- merge(shade_mean_assoc, color_index)


shadeAssoc = shade_mean_assoc 

corr.test(shadeAssoc$mean_response, shadeAssoc$L)
#-.79, p <.001

shadeAssoc = shadeAssoc %>%arrange(color_index)
shadeAssoc$color_hex = as.character(shadeAssoc$color_hex)
colorHex <-  dplyr::pull(shadeAssoc,color_hex)


plotShade = ggplot(data = shadeAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - Shade",
       y = "mean assoc.",
       x = "color") +
  ylim(0,1)+
  theme(legend.position = "none") 
plotShade


#write.csv(shade_mean_assoc, "shadeMeanAssoc.csv")



# exp1 & 2: SUNSHINE ENDPOINTS  ------------------------------------------------

##  a lot of sunshine  ------------------------------------------------------------------

d = read.csv("exp1-sunEndpointsAssoc.csv")

#table with all relevant data
lotSun<- d %>% filter(prompt == "a lot of sunshine") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(rt, response, prompt, color_rgb, color_index, newresponse)

#lotSun mean assoc
lotSun_mean_assoc <- lotSun %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
lotSun_mean_assoc = lotSun_mean_assoc[keep]
lotSun_mean_assoc <- merge(lotSun_mean_assoc, color_index)

lotSunAssoc = lotSun_mean_assoc


#Hill plot 
lotSunAssoc = lotSunAssoc %>%arrange(color_index)
colorHex <-  dplyr::pull(lotSunAssoc,color_hex)
colorHex = as.character(colorHex)

plotlotSun = ggplot(data = lotSunAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - lotSun",
       y = "mean assoc.",
       x = "color") +
  theme(legend.position = "none") 
plotlotSun


#write.csv(lotSun_mean_assoc, "lotSunMeanAssoc.csv")


# no sunshine  ------------------------------------------------------------------

d = read.csv("exp1-sunEndpointsAssoc.csv")


#table with all relevant data
noSun<- d %>% filter(prompt == "no sunshine") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(rt, response, prompt, color_rgb, color_index, newresponse)

#lotSun mean assoc
noSun_mean_assoc <- noSun %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
noSun_mean_assoc = noSun_mean_assoc[keep]
noSun_mean_assoc <- merge(noSun_mean_assoc, color_index)


noSunAssoc = noSun_mean_assoc
noSunAssoc = noSunAssoc %>%arrange(color_index)
colorHex <-  dplyr::pull(noSunAssoc,color_hex)
colorHex = as.character(colorHex)

#hill plot
plotnoSun = ggplot(data = noSunAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - noSun",
       y = "mean assoc.",
       x = "color")+
  theme(legend.position = "none") 
plotnoSun


#write.csv(noSun_mean_assoc, "noSunMeanAssoc.csv")


# correlation between sunshine endpoint associations ----------------------------------------

corr.test(lotSun_mean_assoc$mean_response, noSun_mean_assoc$mean_response)
# -.971



# exp1: SHADE ENDPOINTS ------------------------------------------------

# a lot of shade ----------------------------------------------------------


d = read.csv("exp1-shadeEndpointsAssoc.csv")

#table with all relevant data
lotShade<- d %>% filter(prompt == "a lot of shade") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(rt, condition, response, prompt, color_rgb, color_index, newresponse)

#lotShade mean assoc
lotShade_mean_assoc <- lotShade %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
lotShade_mean_assoc = lotShade_mean_assoc[keep]
lotShade_mean_assoc <- merge(lotShade_mean_assoc, color_index)

lotShadeAssoc = lotShade_mean_assoc
lotShadeAssoc = lotShadeAssoc %>%arrange(color_index)
lotShadeAssoc$color_hex = as.character(lotShadeAssoc$color_hex)
colorHex <-  dplyr::pull(lotShadeAssoc,color_hex)


plotlotShade = ggplot(data = lotShadeAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - lotShade",
       y = "mean assoc.",
       x = "color") +
  theme(legend.position = "none") 
plotlotShade



#write.csv(lotShade_mean_assoc, "lotShadeMeanAssoc.csv")


#  no shade  ------------------------------------------------------------------


#table with all relevant data
noShade<- d %>% filter(prompt == "no shade") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(rt, condition, response, prompt, color_rgb, color_index, newresponse)

#lotShade mean assoc
noShade_mean_assoc <- noShade %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
noShade_mean_assoc = noShade_mean_assoc[keep]
noShade_mean_assoc <- merge(noShade_mean_assoc, color_index)

noShadeAssoc = noShade_mean_assoc
noShadeAssoc = noShadeAssoc %>%arrange(color_index)
noShadeAssoc$color_hex = as.character(noShadeAssoc$color_hex)
colorHex <-  dplyr::pull(noShadeAssoc,color_hex)


plotnoShade = ggplot(data = noShadeAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - noShade",
       y = "mean assoc.",
       x = "color")+
  theme(legend.position = "none") 
plotnoShade


#write.csv(noShade_mean_assoc, "noShadeMeanAssoc.csv")


# correlation between shade endpoint associations ----------------------------------------


corr.test(lotShade_mean_assoc$mean_response, noShade_mean_assoc$mean_response)
# -.869





# EXPERIMENT 3 - ASSOCIATION RATINGS --------------------------------------


# exp3: GLACIAL ICE  ------------------------------------------------------------------


d = read.csv("exp3-assoc.csv")

#table with all relevant ice data
ice<- d %>% filter(prompt == "glacial ice") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(subjectID, rt, condition, response, prompt, color_rgb, color_index, newresponse)

#ice mean assoc
ice_mean_assoc <- ice %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
ice_mean_assoc = ice_mean_assoc[keep]
ice_mean_assoc <- merge(ice_mean_assoc, color_index)

IceAssoc = ice_mean_assoc
IceAssoc = IceAssoc %>%arrange(color_index)
IceAssoc$color_hex = as.character(IceAssoc$color_hex)
colorHex <-  dplyr::pull(IceAssoc,color_hex)

plotIce = ggplot(data = IceAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - ice",
       y = "mean assoc.",
       x = "color") +
  theme(legend.position = "none") 
plotIce

#write.csv(ice_mean_assoc, "iceMeanAssoc.csv")




# color combinations ------------------------------------------------------

xPair = c(1)
iceLoop = data.frame(xPair)

iceTable = data.frame(xPair)
iceTable = data.frame(matrix(ncol = 10, nrow = 0))


#loop through all possible ice color combinations
for (y in 1:70){
  for (x in 1:(71-y)){
    color1 = slice(ice_mean_assoc, y)
    color2 = slice(ice_mean_assoc, y+x)
    
    
    #if lightness levels are different, then calculate mean rating difference
    if (color1$L != color2$L){
      
      #Color 1 is lighter than color 2
      if (color1$L > color2$L){
        lightcolor = color1
        darkcolor = color2
        
        #Color 2 is lighter than color 2
      }else if (color2$L > color1$L){
        lightcolor = color2
        darkcolor = color1
      }
      
      #update loop table
      iceLoop$y = y
      iceLoop$x = x
      
      iceLoop$diffRating = lightcolor$mean_response - darkcolor$mean_response
      iceLoop$diffLighting = lightcolor$L - darkcolor$L
      
      iceLoop$lightcolorIndex = lightcolor$color_index
      iceLoop$lightrgb = lightcolor$color_rgb
      iceLoop$lightL=   lightcolor$L
      iceLoop$lightA = lightcolor$a
      iceLoop$lightB =lightcolor$b
      iceLoop$lightAssocice = lightcolor$mean_response
      
      
      iceLoop$darkcolorIndex = darkcolor$color_index
      iceLoop$darkrgb = darkcolor$color_rgb
      iceLoop$darkL= darkcolor$L
      iceLoop$darkA = darkcolor$a
      iceLoop$darkB =darkcolor$b
      iceLoop$darkAssocice= darkcolor$mean_response
      
      iceLoop$xPair = xPair
      xPair = xPair+1
      #update the main table
      iceTable <- rbind(iceTable, iceLoop)
      
      
      #if lightness levels are the same, skip
    }else if (color1$L == color2$L){
    }
  }
}

#change xpair to non-integer to be read correctly when plotting
iceTable$xPair <- as.factor(iceTable$xPair)


iceTable2 <- iceTable %>%
  select(diffRating, diffLighting, xPair,  lightAssocice, darkAssocice, darkrgb, lightrgb,
         lightL, lightA, lightB, darkL, darkA, darkB)

iceTable25 = iceTable2[iceTable2$diffLighting >25, ]

#write.csv(iceTable2, "allIcePairs.csv")


# exp3: WILD FIRE ------------------------------------------------------------------

#table with all relevant fire data
fire<- d %>% filter(prompt == "wild fire") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(subjectID, rt, condition, response, prompt, color_rgb, color_index, newresponse)

#fire mean assoc
fire_mean_assoc <- fire %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
fire_mean_assoc = fire_mean_assoc[keep]
fire_mean_assoc <- merge(fire_mean_assoc, color_index)

fireAssoc = fire_mean_assoc
fireAssoc = fireAssoc %>%arrange(color_index)
fireAssoc$color_hex = as.character(fireAssoc$color_hex)

colorHex <-  dplyr::pull(fireAssoc,color_hex)


plotfire = ggplot(data = fireAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - fire",
       y = "mean assoc.",
       x = "color") +
  theme(legend.position = "none") 
plotfire

#write.csv(fire_mean_assoc, "fireMeanAssoc.csv")


# color combinations ------------------------------------------------------

xPair = c(1)
fireLoop = data.frame(xPair)

fireTable = data.frame(xPair)
fireTable = data.frame(matrix(ncol = 10, nrow = 0))


#loop through all possible fire color combinations
for (y in 1:70){
  for (x in 1:(71-y)){
    color1 = slice(fire_mean_assoc, y)
    color2 = slice(fire_mean_assoc, y+x)
    
    
    #if lightness levels are different, then calculate mean rating difference
    if (color1$L != color2$L){
      
      #Color 1 is lighter than color 2
      if (color1$L > color2$L){
        lightcolor = color1
        darkcolor = color2
        
        #Color 2 is lighter than color 2
      }else if (color2$L > color1$L){
        lightcolor = color2
        darkcolor = color1
      }
      
      #update loop table
      fireLoop$y = y
      fireLoop$x = x
      
      fireLoop$diffRating = lightcolor$mean_response - darkcolor$mean_response
      fireLoop$diffLighting = lightcolor$L - darkcolor$L
      
      fireLoop$lightcolorIndex = lightcolor$color_index
      fireLoop$lightrgb = lightcolor$color_rgb
      fireLoop$lightL=   lightcolor$L
      fireLoop$lightA = lightcolor$a
      fireLoop$lightB =lightcolor$b
      fireLoop$lightAssocfire = lightcolor$mean_response
      
      
      fireLoop$darkcolorIndex = darkcolor$color_index
      fireLoop$darkrgb = darkcolor$color_rgb
      fireLoop$darkL= darkcolor$L
      fireLoop$darkA = darkcolor$a
      fireLoop$darkB =darkcolor$b
      fireLoop$darkAssocfire= darkcolor$mean_response
      
      fireLoop$xPair = xPair
      xPair = xPair+1
      #update the main table
      fireTable <- rbind(fireTable, fireLoop)
      
      
      #if lightness levels are the same, skip
    }else if (color1$L == color2$L){
    }
  }
}

#change xpair to non integer to be read correctly when plotting
fireTable$xPair <- as.factor(fireTable$xPair)


fireTable2 <- fireTable %>%
  select(diffRating, diffLighting, xPair,  lightAssocfire, darkAssocfire, darkrgb, lightrgb,
         lightL, lightA, lightB, darkL, darkA, darkB)


#filter to only pairs with >25 lightness difference
fireTable25 = fireTable2[fireTable2$diffLighting >25, ]


#write.csv(fireTable2, "allFirePairs.csv")

# exp3: OCEAN WATER ------------------------------------------------------------------

#table with all relevant water data
water<- d %>% filter(prompt == "ocean water") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(subjectID, rt, condition, response, prompt, color_rgb, color_index, newresponse)

#water mean assoc
water_mean_assoc <- water %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
water_mean_assoc = water_mean_assoc[keep]
water_mean_assoc <- merge(water_mean_assoc, color_index)

WaterAssoc = water_mean_assoc
waterAssoc = waterAssoc %>%arrange(color_index)
waterAssoc$color_hex = as.character(waterAssoc$color_hex)

colorHex <-  dplyr::pull(waterAssoc,color_hex)


plotwater = ggplot(data = waterAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - water",
       y = "mean assoc.",
       x = "color")+
  theme(legend.position = "none") 
plotwater


#write.csv(water_mean_assoc, "waterMeanAssoc.csv")


# color combinations ------------------------------------------------------

xPair = c(1)
waterLoop = data.frame(xPair)

waterTable = data.frame(xPair)
waterTable = data.frame(matrix(ncol = 10, nrow = 0))


#loop through all possible water color combinations
for (y in 1:70){
  for (x in 1:(71-y)){
    color1 = slice(water_mean_assoc, y)
    color2 = slice(water_mean_assoc, y+x)
    
    
    #if lightness levels are different, then calculate mean rating difference
    if (color1$L != color2$L){
      
      #Color 1 is lighter than color 2
      if (color1$L > color2$L){
        lightcolor = color1
        darkcolor = color2
        
        #Color 2 is lighter than color 2
      }else if (color2$L > color1$L){
        lightcolor = color2
        darkcolor = color1
      }
      
      #update loop table
      waterLoop$y = y
      waterLoop$x = x
      
      waterLoop$diffRating = lightcolor$mean_response - darkcolor$mean_response
      waterLoop$diffLighting = lightcolor$L - darkcolor$L
      
      waterLoop$lightcolorIndex = lightcolor$color_index
      waterLoop$lightrgb = lightcolor$color_rgb
      waterLoop$lightL=   lightcolor$L
      waterLoop$lightA = lightcolor$a
      waterLoop$lightB =lightcolor$b
      waterLoop$lightAssocwater = lightcolor$mean_response
      
      
      waterLoop$darkcolorIndex = darkcolor$color_index
      waterLoop$darkrgb = darkcolor$color_rgb
      waterLoop$darkL= darkcolor$L
      waterLoop$darkA = darkcolor$a
      waterLoop$darkB =darkcolor$b
      waterLoop$darkAssocwater= darkcolor$mean_response
      
      waterLoop$xPair = xPair
      xPair = xPair+1
      #update the main table
      waterTable <- rbind(waterTable, waterLoop)
      
      
      #if lightness levels are the same, skip
    }else if (color1$L == color2$L){
    }
  }
}

#change xpair to non integer to be read correctly when plotting
waterTable$xPair <- as.factor(waterTable$xPair)


waterTable2 <- waterTable %>%
  select(diffRating, diffLighting, xPair,  lightAssocwater, darkAssocwater, darkrgb, lightrgb,
         lightL, lightA, lightB, darkL, darkA, darkB)

#filter to only pairs with >25 lightness difference
waterTable25 = waterTable2[waterTable2$diffLighting >25, ]


#write.csv(waterTable2, "allWaterPairs.csv")



# exp3 : TREE FOLIAGE -------------------------------------------------------------

#table with all relevant foliage data
foliage<- d %>% filter(prompt == "tree foliage") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(subjectID, rt, condition, response, prompt, color_rgb, color_index, newresponse)

#foliage mean assoc
foliage_mean_assoc <- foliage %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
foliage_mean_assoc = foliage_mean_assoc[keep]
foliage_mean_assoc <- merge(foliage_mean_assoc, color_index)

foliageAssoc = foliage_mean_assoc
foliageAssoc = foliageAssoc %>%arrange(color_index)
foliageAssoc$color_hex = as.character(foliageAssoc$color_hex)

colorHex <-  dplyr::pull(foliageAssoc,color_hex)


plotfoliage = ggplot(data = foliageAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - foliage",
       y = "mean assoc.",
       x = "color") +
  theme(legend.position = "none") 
plotfoliage

#write.csv(foliage_mean_assoc, "foliageMeanAssoc.csv")


# color combinations ------------------------------------------------------

xPair = c(1)
foliageLoop = data.frame(xPair)

foliageTable = data.frame(xPair)
foliageTable = data.frame(matrix(ncol = 10, nrow = 0))


#loop through all possible foliage color combinations
for (y in 1:70){
  for (x in 1:(71-y)){
    color1 = slice(foliage_mean_assoc, y)
    color2 = slice(foliage_mean_assoc, y+x)
    
    
    #if lightness levels are different, then calculate mean rating difference
    if (color1$L != color2$L){
      
      #Color 1 is lighter than color 2
      if (color1$L > color2$L){
        lightcolor = color1
        darkcolor = color2
        
        #Color 2 is lighter than color 2
      }else if (color2$L > color1$L){
        lightcolor = color2
        darkcolor = color1
      }
      
      #update loop table
      foliageLoop$y = y
      foliageLoop$x = x
      
      foliageLoop$diffRating = lightcolor$mean_response - darkcolor$mean_response
      foliageLoop$diffLighting = lightcolor$L - darkcolor$L
      
      foliageLoop$lightcolorIndex = lightcolor$color_index
      foliageLoop$lightrgb = lightcolor$color_rgb
      foliageLoop$lightL=   lightcolor$L
      foliageLoop$lightA = lightcolor$a
      foliageLoop$lightB =lightcolor$b
      foliageLoop$lightAssocfoliage = lightcolor$mean_response
      
      
      foliageLoop$darkcolorIndex = darkcolor$color_index
      foliageLoop$darkrgb = darkcolor$color_rgb
      foliageLoop$darkL= darkcolor$L
      foliageLoop$darkA = darkcolor$a
      foliageLoop$darkB =darkcolor$b
      foliageLoop$darkAssocfoliage= darkcolor$mean_response
      
      foliageLoop$xPair = xPair
      xPair = xPair+1
      #update the main table
      foliageTable <- rbind(foliageTable, foliageLoop)
      
      
      #if lightness levels are the same, skip
    }else if (color1$L == color2$L){
    }
  }
}

#change xpair to non integer to be read correctly when plotting
foliageTable$xPair <- as.factor(foliageTable$xPair)


foliageTable2 <- foliageTable %>%
  select(diffRating, diffLighting, xPair,  lightAssocfoliage, darkAssocfoliage, darkrgb, lightrgb,
         lightL, lightA, lightB, darkL, darkA, darkB)

#filter to only pairs with >25 lightness difference
foliageTable25 = foliageTable2[foliageTable2$diffLighting >25, ]

#write.csv(foliageTable2, "allFoliagePairs.csv")


# exp3: GROUND SOIL  ------------------------------------------------------------------

#table with all relevant soil data
soil<- d %>% filter(prompt == "ground soil") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(subjectID, rt, condition, response, prompt, color_rgb, color_index, newresponse)

#soil mean assoc
soil_mean_assoc <- soil %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
soil_mean_assoc = soil_mean_assoc[keep]
soil_mean_assoc <- merge(soil_mean_assoc, color_index)

soilAssoc = soil_mean_assoc
soilAssoc = soilAssoc %>%arrange(color_index)
soilAssoc$color_hex = as.character(soilAssoc$color_hex)

colorHex <-  dplyr::pull(soilAssoc,color_hex)


plotsoil = ggplot(data = soilAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - soil",
       y = "mean assoc.",
       x = "color")+
  theme(legend.position = "none") 
plotsoil



#write.csv(soil_mean_assoc, "soilMeanAssoc.csv")

# color combinations ------------------------------------------------------

xPair = c(1)
soilLoop = data.frame(xPair)

soilTable = data.frame(xPair)
soilTable = data.frame(matrix(ncol = 10, nrow = 0))


#loop through all possible soil color combinations
for (y in 1:70){
  for (x in 1:(71-y)){
    color1 = slice(soil_mean_assoc, y)
    color2 = slice(soil_mean_assoc, y+x)
    
    
    #if lightness levels are different, then calculate mean rating difference
    if (color1$L != color2$L){
      
      #Color 1 is lighter than color 2
      if (color1$L > color2$L){
        lightcolor = color1
        darkcolor = color2
        
        #Color 2 is lighter than color 2
      }else if (color2$L > color1$L){
        lightcolor = color2
        darkcolor = color1
      }
      
      #update loop table
      soilLoop$y = y
      soilLoop$x = x
      
      soilLoop$diffRating = lightcolor$mean_response - darkcolor$mean_response
      soilLoop$diffLighting = lightcolor$L - darkcolor$L
      
      soilLoop$lightcolorIndex = lightcolor$color_index
      soilLoop$lightrgb = lightcolor$color_rgb
      soilLoop$lightL=   lightcolor$L
      soilLoop$lightA = lightcolor$a
      soilLoop$lightB =lightcolor$b
      soilLoop$lightAssocsoil = lightcolor$mean_response
      
      
      soilLoop$darkcolorIndex = darkcolor$color_index
      soilLoop$darkrgb = darkcolor$color_rgb
      soilLoop$darkL= darkcolor$L
      soilLoop$darkA = darkcolor$a
      soilLoop$darkB =darkcolor$b
      soilLoop$darkAssocsoil= darkcolor$mean_response
      
      soilLoop$xPair = xPair
      xPair = xPair+1
      #update the main table
      soilTable <- rbind(soilTable, soilLoop)
      
      
      #if lightness levels are the same, skip
    }else if (color1$L == color2$L){
    }
  }
}

#change xpair to non integer to be read correctly when plotting
soilTable$xPair <- as.factor(soilTable$xPair)

soilTable2 <- soilTable %>%
  select(diffRating, diffLighting, xPair,  lightAssocsoil, darkAssocsoil, darkrgb, lightrgb,
         lightL, lightA, lightB, darkL, darkA, darkB)


#filter to only pairs with >25 lightness difference
soilTable25 = soilTable2[soilTable2$diffLighting >25, ]

#write.csv(soilTable2, "allSoilPairs.csv")



# STOP HERE AND RUN COLORMAPS-REGRESSIONCHECK-ENVIRONMENT.r ----------------

#use one of the files made above ("allWaterPairs.csv") to run in 
# the generatingColormaps.r file to create the regressionLCH-xpairs file 
# that includes the LCH coordinates for the interpolated colors. 

#use the regressionLCH-xpairs.csv file plus the above files
# allWaterPairs.csv and waterMeanAssoc.csv in the regression check
