# Unifying Effects of Direct and Relational Associations for Visual Communication

## FOLDER: COLORMAP INTERPRETATIONS:  

Data files

`exp1maps.csv`, `exp2maps.csv`, `exp3maps-fullSet.csv`: store the data from colormaps interpretations task for each experiment

`exp3maps-testingSet.csv`, `exp3maps-trainingSet.csv`: store the data from colormaps interpretations task for Exp. 3, separated by training vs. testing data 

`exp3maps-testingSet-ave.csv`, `exp3maps-trainingSet-ave.csv`: store the data from colormaps interpretations task for Exp. 3, separated by training vs. testing data, averaged over participants 

`weightPairsTesting.csv`, `weightPairsTraining.csv`: store the weight pairing combinations used in Exp. 3. 



Analysis and plotting files

`assign-infer-analyses-colormapInterpretations.R`: R script for analyses and plotting data from all 3 experiments colormaps interpretations tasks. Takes files above as inputs.

`assign_infer_semanticDistance_main.m`: MATLAB script for getting semantic distance values, uses `get_semantic_distance.m` function file

---


### Columns of exp2maps files contain:

`subjectID` unique participant code

 `xPair` codes for the unique color-pair (2 colors from the UW 71 colors) used to create a color scale and corresponding colormaps (range: 0-1848)

 `ID`  codes for the unique color-pair for that experiment alone 

`colorPair` (Exp3 files only) code for the unique color-pair for that experiment alone (range 0-63)

`prompt` concept presented

`stimulus` stimulus file name

`diffLighting` difference in L* between two colors

`darkrgb` hex code for dark color

`dark_colorIndex` unique UW71 code for dark color

`lightrgb` hex code for light color

`light_colorIndex` unique UW71 code for light color

`lightAssocSun`  mean association rating for the lighter color with concept sunshine

`lightAssocShade`mean association rating for the lighter color with concept shade

`darkAssocSun` mean association rating for the darker color with concept sunshine

`darkAssocShade`mean association rating for the darker color with concept shade

`diffRatingSun` difference in mean association ratings between two colors with the concept sunshine

`diffRatingShade` difference in mean association ratings between two colors with the concept sunshine
    
`lightL` L* coordinate in CIELAB space for light color

`lightA` A* coordinate in CIELAB space for light color

`lightB` B* coordinate in CIELAB space for light color

`darkL`	L* coordinate in CIELAB space for dark color

`darkA` A* coordinate in CIELAB space for dark color

`darkB` B* coordinate in CIELAB space for dark color

`rsquareSun` Rsquare value identified during the monotonicity check (range: 0-1)

`totalDark` number of times the darker side was selected (range: 0-10)

`propDark` proportion of times the darker side was selected (range: 0-1)

`conditionProto` condition, including which concept and which color pair

`key_press` which key was pressed (left selected = 37, right selected = 39)

`darkSide` which side of the map was darker (left dark = 0, right dark = 1)

`colormap` which unique colormap (0-4)

`sidePress` which side of the map was selected (left selected = 0, right selected = 1)

`selectDark` whether the darker side was selected (dark selected = 1, light selected = 0)

`darkAssoc_lotSun` mean association between darker color and a lot of sunshine (range: 0-1)

`darkAssoc_noSun` mean association between darker color and no sunshine (range: 0-1)

`lightAssoc_lotSun` mean association between lighter color and a lot of sunshine (range: 0-1)

`lightAssoc_noSun` mean association between lighter color and no sunshine (range: 0-1)

`SemDist` semantic distance for endpoint concepts and colors of pair (range: 0-1)

`meanAssocDiff` mean association difference between the lighter and darker color and the concept (range: 0-1)

`darkSideC`which side of the map was darker, centered (left dark = -.5, right dark = .5)

`meanAssocDiffC`  mean association difference between the lighter and darker color and the concept, centered around zero (range: -1 to 1)

`Outer1` merit for darker color with a lot of the concept (range: 0-1)

`Outer2` merit for lighter color with a no of the concept (range: 0-1)

`Inner1` merit for lighter color with a lot of the concept (range: 0-1)

`Inner2` merit for darker color with a no of the concept (range: 0-1)

`assign` solution to the assignment problem (1 = outer, -1 = inner)

`assignSemDist`signed semantic distance, based on solution to assignment problem

`signMeanAssocDiff` signed mean association difference



### Columns of exp1maps file contains those listed above for (exp2maps) and the following:
`conceptStr` concept presented (sunshine or shade)

`conceptC` concept presented (sunshine = -.5, shade = .5)

`AssociatedSideC` which side is more associated (left more associated = -1, right more associated = 1)

`sideAssociationDiff` mean association difference, signed based on which side is more associated

`sideAssocDiffNorm` mean signed association difference, normed to range from -1 to 1

`assocDiffStr` categorical split of whether association difference was low vs. high 


### Columns of exp3maps-full file contains those listed above for (exp2maps) and the following:
`expID` codes for the unique color-pair for that experiment alone 

`plotID` codes for the unique color-pair for that experiment alone, used for plotting order

`concept` which concept presented

`seDark` standard error for mean prop. darker side was selected (ave. over participants)

`seLeft` standard error for mean prop. left side was selected (ave. over participants)

`meanPropLeft` mean proportion of times left side was selected

`meanPropDark` mean proportion of times darker side was selected

`darkSideC3` which side was dark (left dark = -1, right dark = 1)

`associatedSide` which side is more associated (left more associated = -1, right more associated = 1)

`darkAssoc_lot` and `assocOuter1` association between darker color and a lot of the concept

`darkAssoc_no` and `assocInner2` association between darker color and none of the concept

`lightAssoc_lot` and `assocInner1` association between lighter color and a lot of the concept

`lightAssoc_no` and `assocOuter2` association between the lighter color and no of the concept



### Columns of exp3maps-test.csv, exp3maps-train.csv, exp3maps-test-ave.csv & exp3maps-train-ave.csv contain:

`subjectID` unique participant code

 `xPair` codes for the unique color-pair (2 colors from the UW 71 colors) used to create a color scale and corresponding colormaps (range: 0-1848)

 `ID`  codes for the unique color-pair for that experiment alone 

`colorPair` code for the unique color-pair for that experiment alone (range 0-63)

`concept` concept presented

`darkrgb` hex code for dark color

`dark_colorIndex` unique UW71 code for light color

`lightrgb` hex code for light color

`light_colorIndex` unique UW71 code for light color

`sdDarkness` standard deviation for the mean darkness ratings

`meanDarkness` mean darkness ratings (see supplemental material)

`seDarkness` standard error for the mean darkness ratings

`meanPropDark` mean proportion of times the darker side was selected (range: 0-1)

`semDistAssocSSNorm` semantic distance, normed to range from -1 to 1

`seDark` standard error for the mean prop. dark selected

`darkAssoc_lot`, `assocOuter1`, `Outer1` association between darker color and a lot of the concept

`darkAssoc_no`, `assocInner2`, `Inner2` association between darker color and none of the concept

`lightAssoc_lot`, `assocInner1`, `Inner1` association between lighter color and a lot of the concept

`lightAssoc_no`,  `assocOuter2`, `Outer2` association between the lighter color and no of the concept



exp3maps-test.csv & exp3maps-train.csv also contain

`sidePress` which side was selected (left = 0, right = 1)

`seLeft` standard error for mean prop. select left side

`meanPropLeft` mean proportion of times the left side was selected (range: 0-1)

`darkSideC3` which side was dark (left dark = -1, right dark = 1)

`associatedSide` which side is more associated (left more associated = -1, right more associated = 1)

`SemDist` semantic distance for endpoint concepts and colors of pair (range: 0-1)

`meanAssocDiff` mean association difference between the lighter and darker color and the concept (range: 0-1)

`diffLighting` L* difference in lighting between the light and dark color

`semDistAssocSSC` semantic distance, centered around zero

`darknessSS` mean darkness ratings centered around zero
`darknessSSNorm` mean darkness ratings centered, normed to range from - 1 to 1

`assignSemDistAssocSS` semantic distance, centered around zero

`assignSemDistAssocSSNorm` semantic distance, centered, normed to range from -1 to 1



Columns of exp3maps-test-ave.csv & exp3maps-train-ave.csv also contain:

`semDistAssocWeightm1` weight on direct associations

`darkSideWeightm1` and `darkSideWeight` weight on dark-is-more bias

`comboSign` solution to the assignment problem for combined semantic distance (inner = -1, outer = 1)

`Outer1m1c` weighted merit for assignment between a lot of the concept and the darker color

`Outer2m1c` weighted merit for assignment between no of the concept and the lighter color

`Inner1m1c` weighted merit for assignment between a lot of the concept and the lighter color

`Inner2m1c` weighted merit for assignment between no of the concept and the darker color

`semDistComboM1` combined semantic distance (weighted with both semantic distance and dark-is-more bias)

`semDistComboSign` signed combined semantic distance based on solution to the assignment problem

`lineX` value along the line that would demonstrate a perfect fit between mean prop. dark and the predicted responses (based on combined signed semantic distance)

`se1` standard error between the line of perfect fit and the true value




### Columns of weightPairsTraining.csv contains:

`semDistAssocWeightm1` weight on direct associations

`darkSideWeightm1` weight on dark-is-more bias



### Columns of weightPairsTesting.csv contains:

`weightPair` which weight pair (direct only= 0, optimal pair = 1, dark-is-more only = 2)

`semDistAssocWeightm1` weight on direct associations

`darkSideWeightm1` weight on dark-is-more bias
