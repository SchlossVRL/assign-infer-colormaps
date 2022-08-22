# Unifying Effects of Direct and Relational Associations for Visual Communication

## FOLDER: SUPPLEMENTAL MATERIAL:  

Data files

`exp3maps-darknessRatings.csv`: store the data from darkness ratings task for the color pairs from Exp. 3

`exp3maps-darknessRatings-ave.csv`: store the data from darkness ratings task for the color pairs from Exp.3, averaged over participants



Analysis and plotting file

`assign-infer-analyses-supplemental.R`: R script for analyses and plotting data discussed in the Supplemental materials. Takes files above as inputs.



Subfolder-monotonicityRegression:

  `allFirePairs`, `allFoliagePairs`, `allIcePairs`, `allShadePairs`, `allSoilPairs`, `allSunshinePairs`, `allWaterPairs`: stores association data for each possible color pair using the UW 71

  `RegressionLCH-xPairs`: contains the CIELCh color coordinates for each possible color scale generated from each color pair using the UW 71


---


### Columns of exp3maps-darknessRatings file contains:

`subjectID` unique participant code

`Gender` participant's self-reported gender (via free response)

`Age`: participant's self-reported age

`xPair` codes for the unique color-pair (2 colors from the UW 71 colors) used to create a color scale and corresponding colormaps (range: 0-1848)

`colorPair` (Exp3 files only) code for the unique color-pair for that experiment alone (range 0-63)

`trial_index` trial number during experiment (smaller numbers were seen earlier in the experiment)

`rt` response times

`whichSideDark` which side of the square was darker (dark left = 0, dark right = 1)

`selectedDarkSide` whether the darker side was selected (dark selected = 1, light selected = -1)

`response` slider scale rating (range: -200 to 200) of how much darker the darker color was perceived

`newresponse` scaled response (range: -1 to 1)

`unsignedResponse` scaled response without sign (range: 0-1)

`darkResponse` scaled response, signed according to whether darker (+) or lighter (-) side was indicated as darker based on ratings




### Columns of exp3maps-darknessRatings-ave file contains:

`xPair` codes for the unique color-pair (2 colors from the UW 71 colors) used to create a color scale and corresponding colormaps (range: 0-1848)

`colorPair` (Exp3 files only) code for the unique color-pair for that experiment alone (range 0-63)

`concept` which concept was presented

`darkrgb` hex code for dark color

`dark_colorIndex` unique UW71 code for light color

`lightrgb` hex code for light color

`light_colorIndex` unique UW71 code for light color

`meanDarkness` mean darkness rating, averaged over participants

`sdDarkness` standard deviation of the darkness rating

`seDarkness`: standard error of the mean of the darkness rating


