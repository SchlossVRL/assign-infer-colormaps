# Unifying Effects of Direct and Relational Associations for Visual Communication

## FOLDER: stimuli
Subfolders for each experiment contain the colormaps showed in that experiment (10 colormaps per color scale; 5 dark on left/ 5 dark on right)

`exp1colorCoordinates.csv`, `exp2colorCoordinates.csv`, `exp3colorCoordinates.csv`: stores the color coordinates for each color pair from each experiment

`UW71coordinates.csv`: stores the color coordinates for the UW-71

`celeryColorCoordinates.csv`: stores the color coordinates for the colors used in the attention check


---

Columns of the UW71coordinates.csv file and celeryColorCoordinates:

 `color_index` codes for the unique color from the UW 71 set of colors - used across data files
 `v_index` secondary codes for the unique color from the UW 71 set of colors
 `color_hex` hex code for the color
 `L` L* coordinate in CIELAB & CIELCh space
 `A` A* coordinate in CIELAB space
 `B` B* coordinate in CIELAB space
 `C` C* coordinate in CIELCh space
 `h` hue coordinate in CIELCh space
 `r_rgb` r (red) coordinate in rgb space (scaled from 0-1)
 `g_rgb` g (green) coordinate in rgb space (scaled from 0-1)
 `b_rgb`blue (blue) coordinate in rgb space (scaled from 0-1)
 `r255` r (red) coordinate in rgb space (scaled from 0-255)
 `g255` g (green) coordinate in rgb space (scaled from 0-255)
 `b255` b (blue) coordinate in rgb space (scaled from 0-255)
Celery also includes `prompt` and `rating`, which code the concept and the mean association rating (from previous data) between celery and the given color


Columns of each of expColorCoordinates.csv files:

 `xPair` codes for the unique color-pair (2 colors from the UW 71 colors) used to create a color scale and corresponding colormaps
 `ID`  codes for the unique color-pair for that experiment alone
`diffLighting` difference in L* between two colors
`diffRatingSun` difference in mean association ratings between two colors with the concept sunshine
`diffRatingShade` difference in mean association ratings between two colors with the concept sunshine
`lightrgb` hex code for light color
`light_colorIndex` unique UW71 code for light color
`lightAssocSun`  mean association rating for the lighter color with concept sunshine
`lightAssocShade`mean association rating for the lighter color with concept shade
`lightL` L* coordinate in CIELAB space for light color
`lightA` A* coordinate in CIELAB space for light color
`lightB` B* coordinate in CIELAB space for light color
`darkrgb` hex code for dark color
`dark_colorIndex` unique UW71 code for light color
`darkL`	L* coordinate in CIELAB space for dark color
`darkA` A* coordinate in CIELAB space for dark color
`darkB` B* coordinate in CIELAB space for dark color
`darkAssocSun` mean association rating for the darker color with concept sunshine
`darkAssocShade`mean association rating for the darker color with concept shade


exp3colorCoordinates.csv also includes the following columns: 
`concept` which concept presented
`colorPair` code for the unique color-pair for that experiment alone
`expID` code for the unique color-pair for that experiment alone 
`plotID` code for unique color-pair used for plotting
`rsquare` Rsquare value identified during the monotonicity check
`diffRating`, `darkAssoc`, and `lightAssoc` code for the corresponding values as described above for sun/shade

