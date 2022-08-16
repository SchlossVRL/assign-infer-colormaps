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


## FOLDER: dataSets
### Data files 

RATINGS: 

`sunMeanAssoc.csv`, `shadeMeanAssoc.csv`, `iceMeanAssoc.csv`, `fireMeanAssoc.csv`, `waterMeanAssoc.csv`: store the data from the association ratings task

`noSunAssoc.csv`, `noShadeAssoc.csv`, `noFireAssoc.csv`, `noWaterAssoc.csv`, `noIceAssoc.csv`: store the data from the endpoint association ratings task for no each domain concept

`meanDarknessRatings.csv`: store the data from darkness ratings task for the color pairs from Exp. 3



COLORMAP INTERPRETATIONS

`exp1maps.csv`, `exp2maps.csv`, `exp3maps.csv`: store the data from colormaps interpretations task for each experiment


### Analyses and plotting scripts

`assign-infer-analyses.R`: R script for analyses and plotting data from all 3 experiments. Takes files above as inputs.


## FOLDER: stimuli
#Subfolders for each experiment contain the colormaps showed in that experiment (10 colormaps per color scale; 5 dark on left/ 5 dark on right)

`exp1colorCoordinates.csv`, `exp2colorCoordinates.csv`, `exp3colorCoordinates.csv`: stores the color coordinates for each color pair from each experiment
`UW71coordinates.csv`: stores the color coordinates for the UW-71

