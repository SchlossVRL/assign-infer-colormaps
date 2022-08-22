# Unifying Effects of Direct and Relational Associations for Visual Communication

## FOLDER: ASSOCIATION RATINGS:  
`exp1exp2assoc2.csv`, `exp3assoc2.csv`: store the individual trial data from the associations ratings tasks

`exp1-shadeEndpointsAssoc.csv`,`exp1exp2-sunEndpointsAssoc.csv`,`exp3-EndpointsAssoc.csv`: store the individual trial data from the endpoint associations ratings tasks


Analysis and plotting file

`assign-infer-analyses-associationRatings.R`: R script for analyses and plotting data from all 3 experiments association ratings tasks. Takes files above as inputs.


Subfolder-meanAssoc:  

`sunMeanAssoc.csv`, `shadeMeanAssoc.csv`, `iceMeanAssoc.csv`, `fireMeanAssoc.csv`, `waterMeanAssoc.csv`: store the mean data from the association ratings task (averaged over participants)

`noSunAssoc.csv`, `noShadeAssoc.csv`, `noFireAssoc.csv`, `noWaterAssoc.csv`, `noIceAssoc.csv`, `lotSunAssoc.csv`, `lotShadeAssoc.csv`, `lotFireAssoc.csv`, `lotWaterAssoc.csv`, `lotIceAssoc.csv`: store the mean data from the endpoint association ratings task for each domain concept (averaged over participants)


---

### Columns of all files contain:

`subjectID` unique participant code

`trial_index` unique trial number (lower numbers were seen earlier in the experiment)

`rt` response time 

`response` slider response for a given trial representing degree of association with given concept (range: -200 to 200)

`prompt` and `concept` both code for which concept was presented

 `color_rgb` hex code for the color

 `color_index` codes for the unique color from the UW 71 set of colors - used across data files (range 0-70)

`Age` participant's self-reported age

`Gender` participant's self-reported (via free response box) gender
