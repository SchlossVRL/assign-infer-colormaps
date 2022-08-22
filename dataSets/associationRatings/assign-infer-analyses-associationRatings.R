
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



#Load UW-71 color coordinates - housed in assign-infer-colormaps\stimuli\ folder
color_index <- read_csv("UW71coordinates.csv")


# EXPERIMENT 1 & 2 - ASSOCIATION RATINGS --------------------------------

#load in exp1-exp2 association ratings - assign-infer-colormaps\dataSets\associationRatings folder
exp1 = read.csv("exp1exp2-assoc2.csv")

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

#Correlation between lightness and association ratings
corr.test(SunshineAssoc$mean_response, SunshineAssoc$L)


#Create plot 
plotSunshine = ggplot(data = SunshineAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - Sunshine",
       y = "mean assoc.",
       x = "color") +
  ylim(0,1)+
  theme(legend.position = "none") 
plotSunshine

#Create file with mean association ratings (averaged over participants)
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
shadeAssoc = shadeAssoc %>%arrange(color_index)
shadeAssoc$color_hex = as.character(shadeAssoc$color_hex)
colorHex <-  dplyr::pull(shadeAssoc,color_hex)

#Correlation between lightness and association ratings
corr.test(shadeAssoc$mean_response, shadeAssoc$L)


#create plot
plotShade = ggplot(data = shadeAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - Shade",
       y = "mean assoc.",
       x = "color") +
  ylim(0,1)+
  theme(legend.position = "none") 
plotShade

#Create file with mean association ratings (averaged over participants)
#write.csv(shade_mean_assoc, "shadeMeanAssoc.csv")






# EXPERIMENT 1 & 2 ASSOCIATION RATINGS ------------------------------------


# exp1 & 2: SUNSHINE ENDPOINTS  ------------------------------------------------

#load in exp1-exp2 association ratings - assign-infer-colormaps\dataSets\associationRatings folder
exp1sunEnds = read.csv("exp1exp2-sunEndpointsAssoc.csv")


##  a lot of sunshine  ------------------------------------------------------------------

#table with all relevant data
lotSun<- exp1sunEnds %>% filter(prompt == "a lot of sunshine") %>% 
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
lotSunAssoc = lotSunAssoc %>%arrange(color_index)
colorHex <-  dplyr::pull(lotSunAssoc,color_hex)
colorHex = as.character(colorHex)

#Create plot
plotlotSun = ggplot(data = lotSunAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - lotSun",
       y = "mean assoc.",
       x = "color") +
  theme(legend.position = "none") 
plotlotSun

#Create file with mean association ratings (averaged over participants)
#write.csv(lotSun_mean_assoc, "lotSunMeanAssoc.csv")


# no sunshine  ------------------------------------------------------------------


#table with all relevant data
noSun<- exp1sunEnds %>% filter(prompt == "no sunshine") %>% 
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

#Create plot
plotnoSun = ggplot(data = noSunAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - noSun",
       y = "mean assoc.",
       x = "color")+
  theme(legend.position = "none") 
plotnoSun


#Create file with mean association ratings (averaged over participants)
#write.csv(noSun_mean_assoc, "noSunMeanAssoc.csv")



# correlation between sunshine endpoint associations ----------------------------------------


corr.test(lotSun_mean_assoc$mean_response, noSun_mean_assoc$mean_response)


# exp1: SHADE ENDPOINTS ------------------------------------------------

#load in exp1shadeEnds association ratings - assign-infer-colormaps\dataSets\associationRatings folder
exp1shadeEnds = read.csv("exp1-shadeEndpointsAssoc.csv")


# a lot of shade ----------------------------------------------------------


#table with all relevant data
lotShade<- exp1shadeEnds %>% filter(prompt == "a lot of shade") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(rt,  response, prompt, color_rgb, color_index, newresponse)

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

#create plot
plotlotShade = ggplot(data = lotShadeAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - lotShade",
       y = "mean assoc.",
       x = "color") +
  theme(legend.position = "none") 
plotlotShade


#Create file with mean association ratings (averaged over participants)
#write.csv(lotShade_mean_assoc, "lotShadeMeanAssoc.csv")


#  no shade  ------------------------------------------------------------------


#table with all relevant data
noShade<- exp1shadeEnds %>% filter(prompt == "no shade") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(rt,  response, prompt, color_rgb, color_index, newresponse)

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

#create plot
plotnoShade = ggplot(data = noShadeAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - noShade",
       y = "mean assoc.",
       x = "color")+
  theme(legend.position = "none") 
plotnoShade

#Create file with mean association ratings (averaged over participants)
#write.csv(noShade_mean_assoc, "noShadeMeanAssoc.csv")


# correlation between shade endpoint associations ----------------------------------------


corr.test(lotShade_mean_assoc$mean_response, noShade_mean_assoc$mean_response)


# EXPERIMENT 3 - ASSOCIATION RATINGS --------------------------------------

#load in exp3 association ratings - assign-infer-colormaps\dataSets\associationRatings folder
exp3 = read.csv("exp3-assoc2.csv")


# exp3: GLACIAL ICE  ------------------------------------------------------------------

#table with all relevant ice data
ice<- exp3 %>% filter(prompt == "glacial ice") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(subjectID, rt, response, prompt, color_rgb, color_index, newresponse)

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

#create plot
plotIce = ggplot(data = IceAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - ice",
       y = "mean assoc.",
       x = "color") +
  theme(legend.position = "none") 
plotIce

#Create file with mean association ratings (averaged over participants)
#write.csv(ice_mean_assoc, "iceMeanAssoc.csv")



# exp3: WILD FIRE ------------------------------------------------------------------

#table with all relevant fire data
fire<- exp3 %>% filter(prompt == "wild fire") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(subjectID, rt, response, prompt, color_rgb, color_index, newresponse)

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

#create plot
plotfire = ggplot(data = fireAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - fire",
       y = "mean assoc.",
       x = "color") +
  theme(legend.position = "none") 
plotfire

#Create file with mean association ratings (averaged over participants)
#write.csv(fire_mean_assoc, "fireMeanAssoc.csv")



# exp3: OCEAN WATER ------------------------------------------------------------------

#table with all relevant water data
water<- exp3 %>% filter(prompt == "ocean water") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(subjectID, rt, response, prompt, color_rgb, color_index, newresponse)

#water mean assoc
water_mean_assoc <- water %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
water_mean_assoc = water_mean_assoc[keep]
water_mean_assoc <- merge(water_mean_assoc, color_index)

waterAssoc = water_mean_assoc
waterAssoc = waterAssoc %>%arrange(color_index)
waterAssoc$color_hex = as.character(waterAssoc$color_hex)
colorHex <-  dplyr::pull(waterAssoc,color_hex)

#Create plot
plotwater = ggplot(data = waterAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - water",
       y = "mean assoc.",
       x = "color")+
  theme(legend.position = "none") 
plotwater

#Create file with mean association ratings (averaged over participants)
#write.csv(water_mean_assoc, "waterMeanAssoc.csv")



# exp3 : TREE FOLIAGE -------------------------------------------------------------

#table with all relevant foliage data
foliage<- exp3 %>% filter(prompt == "tree foliage") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(subjectID, rt, response, prompt, color_rgb, color_index, newresponse)

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

#Create plot
plotfoliage = ggplot(data = foliageAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - foliage",
       y = "mean assoc.",
       x = "color") +
  theme(legend.position = "none") 
plotfoliage

#Create file with mean association ratings (averaged over participants)
#write.csv(foliage_mean_assoc, "foliageMeanAssoc.csv")



# exp3: GROUND SOIL  ------------------------------------------------------------------

#table with all relevant soil data
soil<- exp3 %>% filter(prompt == "ground soil") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(subjectID, rt,  response, prompt, color_rgb, color_index, newresponse)

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

#create plot
plotsoil = ggplot(data = soilAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - soil",
       y = "mean assoc.",
       x = "color")+
  theme(legend.position = "none") 
plotsoil


#Create file with mean association ratings (averaged over participants)
#write.csv(soil_mean_assoc, "soilMeanAssoc.csv")




# EXPERIMENT 3: ENDPOINT ASSOCIATION RATINGS ------------------------------

#load in exp3ends association ratings - assign-infer-colormaps\dataSets\associationRatings folder
exp3Ends = read.csv("exp3-EndpointsAssoc.csv")


# exp3: GLACIAL ICE ENDPOINTS ---------------------------------------------

# a lot of ice ------------------------------------------------------------

#table with all relevant data
lotIce<- exp3Ends %>% filter(prompt == "a lot of glacial ice") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(rt, response, prompt, color_rgb, color_index, newresponse)

#lotIce mean assoc
lotIce_mean_assoc <- lotIce %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
lotIce_mean_assoc = lotIce_mean_assoc[keep]
lotIce_mean_assoc <- merge(lotIce_mean_assoc, color_index)

lotIceAssoc = lotIce_mean_assoc
lotIceAssoc = lotIceAssoc %>%arrange(color_index)
lotIceAssoc$color_hex = as.character(lotIceAssoc$color_hex)
colorHex <-  dplyr::pull(lotIceAssoc,color_hex)

#Create plot
plotlotIce = ggplot(data = lotIceAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - lotIce",
       y = "mean assoc.",
       x = "color") +
  theme(legend.position = "none") 
plotlotIce


#Create file with mean association ratings (averaged over participants)
#write.csv(lotIce_mean_assoc, "lotIceMeanAssoc.csv")


# no ice ------------------------------------------------------------------


#table with all relevant data
noIce<- exp3Ends %>% filter(prompt == "no glacial ice") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(rt, response, prompt, color_rgb, color_index, newresponse)

#lotIce mean assoc
noIce_mean_assoc <- noIce %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
noIce_mean_assoc = noIce_mean_assoc[keep]
noIce_mean_assoc <- merge(noIce_mean_assoc, color_index)

noIceAssoc = noIce_mean_assoc
noIceAssoc = noIceAssoc %>%arrange(color_index)
noIceAssoc$color_hex = as.character(noIceAssoc$color_hex)
colorHex <-  dplyr::pull(noIceAssoc,color_hex)

#Create plot
plotnoIce = ggplot(data = noIceAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - noIce",
       y = "mean assoc.",
       x = "color")+
  theme(legend.position = "none") 
plotnoIce


#Create file with mean association ratings (averaged over participants)
#write.csv(noIce_mean_assoc, "noIceMeanAssoc.csv")


# correlation between ice endpoint associations ----------------------------------------


corr.test(lotIce_mean_assoc$mean_response, noIce_mean_assoc$mean_response)



# exp3: WILD FIRE ENDPOINTS -----------------------------------------------

# a lot of fire -----------------------------------------------------------

#table with all relevant data
lotFire<- exp3Ends %>% filter(prompt == "a lot of wild fire") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(rt, response, prompt, color_rgb, color_index, newresponse)

#lotFire mean assoc
lotFire_mean_assoc <- lotFire %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
lotFire_mean_assoc = lotFire_mean_assoc[keep]
lotFire_mean_assoc <- merge(lotFire_mean_assoc, color_index)

lotFireAssoc = lotFire_mean_assoc
lotFireAssoc = lotFireAssoc %>%arrange(color_index)
lotFireAssoc$color_hex = as.character(lotFireAssoc$color_hex)
colorHex <-  dplyr::pull(lotFireAssoc,color_hex)

#Create plot
plotlotFire = ggplot(data = lotFireAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - lotFire",
       y = "mean assoc.",
       x = "color") +
  theme(legend.position = "none") 
plotlotFire


#Create file with mean association ratings (averaged over participants)
#write.csv(lotFire_mean_assoc, "lotFireMeanAssoc.csv")


# no fire -----------------------------------------------------------------


#table with all relevant data
noFire<- exp3Ends %>% filter(prompt == "no wild fire") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(rt, response, prompt, color_rgb, color_index, newresponse)

#lotFire mean assoc
noFire_mean_assoc <- noFire %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
noFire_mean_assoc = noFire_mean_assoc[keep]
noFire_mean_assoc <- merge(noFire_mean_assoc, color_index)

noFireAssoc = noFire_mean_assoc
noFireAssoc = noFireAssoc %>%arrange(color_index)
noFireAssoc$color_hex = as.character(noFireAssoc$color_hex)
colorHex <-  dplyr::pull(noFireAssoc,color_hex)

#Create plot
plotnoFire = ggplot(data = noFireAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - noFire",
       y = "mean assoc.",
       x = "color")+
  theme(legend.position = "none") 
plotnoFire


#Create file with mean association ratings (averaged over participants)
#write.csv(noFire_mean_assoc, "noFireMeanAssoc.csv")


# correlation between fire endpoint associations ----------------------------------------

corr.test(lotFire_mean_assoc$mean_response, noFire_mean_assoc$mean_response)



# exp3: OCEAN WATER ENDPOINTS ---------------------------------------------

# a lot of water ----------------------------------------------------------

#table with all relevant data
lotWater<- exp3Ends %>% filter(prompt == "a lot of ocean water") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(rt, response, prompt, color_rgb, color_index, newresponse)

#lotWater mean assoc
lotWater_mean_assoc <- lotWater %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
lotWater_mean_assoc = lotWater_mean_assoc[keep]
lotWater_mean_assoc <- merge(lotWater_mean_assoc, color_index)

lotWaterAssoc = lotWater_mean_assoc
lotWaterAssoc = lotWaterAssoc %>%arrange(color_index)
lotWaterAssoc$color_hex = as.character(lotWaterAssoc$color_hex)
colorHex <-  dplyr::pull(lotWaterAssoc,color_hex)

#create plot
plotlotWater = ggplot(data = lotWaterAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - lotWater",
       y = "mean assoc.",
       x = "color") +
  theme(legend.position = "none") 
plotlotWater


#Create file with mean association ratings (averaged over participants)
#write.csv(lotWater_mean_assoc, "lotWaterMeanAssoc.csv")



# no water ----------------------------------------------------------------


#table with all relevant data
noWater<- exp3Ends %>% filter(prompt == "no ocean water") %>% 
  mutate(newresponse = (response+200)/400) %>%
  select(rt, response, prompt, color_rgb, color_index, newresponse)

#lotWater mean assoc
noWater_mean_assoc <- noWater %>% group_by(color_index) %>%
  mutate(mean_response = mean(newresponse)) %>% 
  distinct(color_index, .keep_all = TRUE)

keep = c("color_index", "color_rgb", "mean_response")
noWater_mean_assoc = noWater_mean_assoc[keep]
noWater_mean_assoc <- merge(noWater_mean_assoc, color_index)

noWaterAssoc = noWater_mean_assoc
noWaterAssoc = noWaterAssoc %>%arrange(color_index)
noWaterAssoc$color_hex = as.character(noWaterAssoc$color_hex)
colorHex <-  dplyr::pull(noWaterAssoc,color_hex)

#Create plot
plotnoWater = ggplot(data = noWaterAssoc) +
  geom_bar(stat = "identity", aes(x = reorder(color_index, - mean_response),  y = mean_response, fill = factor(color_index))) +  
  scale_fill_manual(values = colorHex)+
  labs(title = "Mean Assoc - noWater",
       y = "mean assoc.",
       x = "color")+
  theme(legend.position = "none") 
plotnoWater

#Create file with mean association ratings (averaged over participants)
#write.csv(noWater_mean_assoc, "noWaterMeanAssoc.csv")


# correlation between water endpoint associations ----------------------------------------

corr.test(lotWater_mean_assoc$mean_response, noWater_mean_assoc$mean_response)


