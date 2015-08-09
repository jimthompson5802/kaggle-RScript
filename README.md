Kaggle RScripts Competition
==================================================

## Overview
Source code and visualizations for the competition.

## Repository Directory Descriptions
* **src** - source code for the competition.  Key source code files:
    + **competition_data_extract.R** web scrapes the Kaggle leaderboard for all
    completed competitions.  Run this script first.
    + **top3_player_location_extract.R** Uses data created by competition_data_extract.R
    to determine location of team members on teams placing in the top 3 positions. 
    Run this script second.
    + **kaggle_leaderboard_medal_count.Rmd** creates report on medal count by country.


