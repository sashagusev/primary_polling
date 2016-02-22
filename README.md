# Historic primary poll accuracy

This repository contains data and analysis of historic presidential primary polling and it's concordance with eventual nominees.

## Data details

Data is indexed in a hierarchy of files:
* all.races.tsv : contains a tab delimited list of all races, the party, year, and winner
* (ex: REPS.1960.list) : contains links to each polling result from that race
* (ex: REPS/1958.01.22.out) : contains tab separated polling percentages for that poll
* plot_split.R : Script to read each file in turn and produce graphics.

## Data collection

All polls were downloaded from the Gallup Brain resource using searches for the text “Republican candidate for president” or “Republican nomination for president” (likewise for Democrats) and excluding “second choice” or leaner answers as possible. Races that did not have at least 12 polls were excluded. For each race, candidates that were not queried in >50% of the polls were excluded. Of the remaining candidates, the top five by average poll % were plotted, with the eventual winner always shown in black. 

Though the Gallup archive primarily had monthly polling, polls that were taken in in the same month were plotted separately (and therefore averaged in the fit curve). For the average across all races, the first and last poll results were extended backward/forward to 30 months, and races that did not have a poll in a given month were interpolated from the previous month. In all plots smoothing was done by simple LOESS curve fitting.
