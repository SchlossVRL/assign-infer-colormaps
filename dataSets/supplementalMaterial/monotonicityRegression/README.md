# Unifying Effects of Direct and Relational Associations for Visual Communication

## FOLDER: SUPPLEMENTAL MATERIAL:  

Subfolder-monotonicityRegression:

  `allFirePairs`, `allFoliagePairs`, `allIcePairs`, `allShadePairs`, `allSoilPairs`, `allSunshinePairs`, `allWaterPairs`: stores association data for each possible color pair using the UW 71

  `RegressionLCH-xPairs`: contains the CIELCh color coordinates for each possible color scale generated from each color pair using the UW 71


---

### Columns of allConceptPair files contain:

`xPair` codes for the unique color-pair (2 colors from the UW 71 colors) used to create a color scale and corresponding colormaps (range: 0-1848)

`darkrgb` hex code for dark color

`dark_colorIndex` unique UW71 code for dark color

`lightrgb` hex code for light color

`light_colorIndex` unique UW71 code for light color

`diffLighting` difference in L* between two colors
    
`lightL` L* coordinate in CIELAB space for light color

`lightA` A* coordinate in CIELAB space for light color

`lightB` B* coordinate in CIELAB space for light color

`darkL`	L* coordinate in CIELAB space for dark color

`darkA` A* coordinate in CIELAB space for dark color

`darkB` B* coordinate in CIELAB space for dark color

`lightAssoc`  mean association rating for the lighter color with concept

`darkAssoc`  mean association rating for the darker color with concept

`diffRating` difference in mean association rating between the lighter and darker color



### Columns of RegressionLCH-xPairs file contains:

`xPair` codes for the unique color-pair (2 colors from the UW 71 colors) used to create a color scale and corresponding colormaps (range: 0-1848)

`darkrgb` hex code for dark color of the given sequence

`dark_colorIndex` unique UW71 code for dark color of the given sequence

`lightrgb` hex code for light color of the given sequence

`light_colorIndex` unique UW71 code for light color of the given sequence

`colorSeqNum` which color in the sequence: 1 = lightest, 10 = darkest

`L` L* coordinate in CIELCh space of the specific color for that row

`C` C* coordinate in CIELCh space of the specific color for that row

`h` hue in CIELCh space of the specific color for that row

`diffLighting` difference in L* between two colors

`Hrad` hue converted to radians