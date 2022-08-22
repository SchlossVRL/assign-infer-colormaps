
%Uses the get_semantic distance function to calculate the semantic
%distance for the color pairs (either combined semantic distance, 
% or for associations only)

clear; close all;

%% Regression weights 

%Can read in any of the csv files for the colormaps task

%Example: 
d = readtable('exp3maps-trainingSet-ave.csv');

%d3$assocOuter1 = d3$darkAssoc_lot
%d3$assocOuter2 = d3$lightAssoc_no
%d3$assocInner1 = d3$lightAssoc_lot
%d3$assocInner2 = d3$darkAssoc_no

%Update this number based on number of rows in file
nPairs = 1323;

%Concept 1 = a lot of [domain concept]
%(e.g., concept1 = [d.lightAssoc_lotFire, d.darkAssoc_lotFire];)
concept1 = [d.Outer1m1c, d.Inner1m1c]; 

%concept 2=  no [domain concept]
%(e.g., concept2 = [d.lightAssoc_noFire, d.darkAssoc_noFire];)
concept2 = [d.Inner2m1c, d.Outer2m1c];

SemDist = [];

%loop through function to acquire semantic distance for all color pairs
for i = 1:nPairs
    SemDist(i) = get_semantic_distance(concept1(i,:), concept2(i,:));
end
 
d.semDistCombo = SemDist';
%Note this semantic distance is UNsigned. See R Supplemental Material file
% for process of signing based on the solution to the assignment problem.

%% 
% Table d can be saved with Semantic Distance 
writetable(d, "exp3maps-trainingSet-ave-SD.csv");
