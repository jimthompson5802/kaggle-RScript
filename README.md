Kaggle RScripts Competition
==================================================

## Overview
Source code and visualizations for the [competition](https://www.kaggle.com/c/introducing-kaggle-scripts).

## Repository Directory Descriptions
* **src** - source code for the competition.  Key source code files:
    + **competition_data_extract.R** web scrapes the Kaggle leaderboard for all
    completed competitions.  Run this script first.
    + **top3_player_location_extract.R** Uses data created by competition_data_extract.R
    to determine location of team members on teams placing in the top 3 positions. 
    Run this script second.
    + **kaggle_leaderboard_medal_count.Rmd** creates [report](https://www.kaggle.com/jimthompson/introducing-kaggle-scripts/kaggle-competition-medal-count-analysis/notebook) on medal count by country.
    + **winning_team_structure_analysis.Rmd** provides [visualization and analysis](https://www.kaggle.com/jimthompson/introducing-kaggle-scripts/visualizing-kaggle-team-structures/notebook) 
    of Kaggle
    teams placing in the top 3 positions using network analytic techniques.


