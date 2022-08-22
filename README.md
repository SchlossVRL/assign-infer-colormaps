# Unifying Effects of Direct and Relational Associations for Visual Communication

_Melissa A. Schoenlein, Johnny Campos, Kevin J. Lande, Laurent Lessard, Karen B. Schloss_

People have expectations about how colors map to concepts in visualizations, and they are better at interpreting visualizations
that match their expectations. Traditionally, studies on these expectations (inferred mappings) distinguished distinct factors
relevant for visualizations of categorical vs. continuous information. Studies on categorical information focused on direct associations
(e.g., mangos are associated with yellows) whereas studies on continuous information focused on relational associations (e.g., darker
colors map to larger quantities; dark-is-more bias). We unite these two areas within a single framework of assignment inference.
Assignment inference is the process by which people infer mappings between perceptual features and concepts represented in
encoding systems. Observers infer globally optimal assignments by maximizing the “merit,” or “goodness,” of each possible assignment.
Previous work on assignment inference focused on visualizations of categorical information. We extend this approach to visualizations
of continuous data by (a) broadening the notion of merit to include relational associations and (b) developing a method for combining
multiple (sometimes conflicting) sources of merit to predict people’s inferred mappings. We developed and tested our model on data
from experiments in which participants interpreted colormap data visualizations, representing fictitious data about environmental
concepts (sunshine, shade, wild fire, ocean water, glacial ice). We found both direct and relational associations contribute independently
to inferred mappings. These results can be used to optimize visualization design to facilitate visual communication.

---

Across files:

 `colorID` codes for the unique color from the UW 71 set of colors

 `xPair` codes for the unique color-pair (2 colors from the UW 71 colors) used to create a color scale and corresponding colormaps



## FOLDER: dataSets
### Association ratings
 
 Data files

`exp1exp2assoc2.csv`, `exp3assoc2.csv`: store the individual trial data from the associations ratings tasks

`exp1-shadeEndpointsAssoc.csv`,`exp1exp2-sunEndpointsAssoc.csv`,`exp3-EndpointsAssoc.csv`: store the individual trial data from the endpoint associations ratings tasks


Analysis and plotting file

`assign-infer-analyses-associationRatings.R`: R script for analyses and plotting data from all 3 experiments association ratings tasks. Takes files above as inputs.


Subfolder-meanAssoc:  

`sunMeanAssoc.csv`, `shadeMeanAssoc.csv`, `iceMeanAssoc.csv`, `fireMeanAssoc.csv`, `waterMeanAssoc.csv`: store the mean data from the association ratings task (averaged over participants)

`noSunAssoc.csv`, `noShadeAssoc.csv`, `noFireAssoc.csv`, `noWaterAssoc.csv`, `noIceAssoc.csv`, `lotSunAssoc.csv`, `lotShadeAssoc.csv`, `lotFireAssoc.csv`, `lotWaterAssoc.csv`, `lotIceAssoc.csv`: store the mean data from the endpoint association ratings task for each domain concept (averaged over participants)




### Colormap interpretations:  

Data files

`exp1maps.csv`, `exp2maps.csv`, `exp3maps-fullSet.csv`: store the data from colormaps interpretations task for each experiment

`exp3maps-testingSet.csv`, `exp3maps-trainingSet.csv`: store the data from colormaps interpretations task for Exp. 3, separated by training vs. testing data 

`exp3maps-testingSet-ave.csv`, `exp3maps-trainingSet-ave.csv`: store the data from colormaps interpretations task for Exp. 3, separated by training vs. testing data, averaged over participants 

`weightPairsTesting.csv`, `weightPairsTraining.csv`: store the weight pairing combinations used in Exp. 3. 



Analysis and plotting files

`assign-infer-analyses-colormapInterpretations.R`: R script for analyses and plotting data from all 3 experiments colormaps interpretations tasks. Takes files above as inputs.

`assign_infer_semanticDistance_main.m`: MATLAB script for getting semantic distance values, uses `get_semantic_distance.m` function file



### Supplemental Material

Data files

`exp3maps-darknessRatings.csv`: store the data from darkness ratings task for the color pairs from Exp. 3

`exp3maps-darknessRatings-ave.csv`: store the data from darkness ratings task for the color pairs from Exp.3, averaged over participants



Analysis and plotting file

`assign-infer-analyses-supplemental.R`: R script for analyses and plotting data discussed in the Supplemental materials. Takes files above as inputs.



Subfolder-monotonicityRegression:

  `allFirePairs`, `allFoliagePairs`, `allIcePairs`, `allShadePairs`, `allSoilPairs`, `allSunshinePairs`, `allWaterPairs`: stores association data for each possible color pair using the UW 71

  `RegressionLCH-xPairs`: contains the CIELCh color coordinates for each possible color scale generated from each color pair using the UW 71





## FOLDER: stimuli

Subfolders for each experiment contain the colormaps showed in that experiment (10 colormaps per color scale; 5 dark on left/ 5 dark on right)

`exp1colorCoordinates.csv`, `exp2colorCoordinates.csv`, `exp3colorCoordinates.csv`: stores the color coordinates for each color pair from each experiment

`UW71coordinates.csv`: stores the color coordinates for the UW-71

`celeryColorCoordinates.csv`: stores the color coordinates for the attention check used during the association ratings task
