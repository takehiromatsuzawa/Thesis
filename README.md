# Thesis

Thesis Objective:

The aims of this study were to predict the average number of points of each player in
the next 5 games by looking at the statistics of an individual player, his team and his opponents
in the previous 10 games. Subsequent to this study, I was able to find important
variables to predict the number of points. In this study, the random forest regression predicted
the average number of points in the next 5 games by mean squared error of 0.0675. 
This paper starts with an exploration of relevant sports statistics and their history. Then the
paper shifts its focus on explaining relevant work by other people (Chapter 1). Next the paper
explains different modern machine algorithms such as neural network regression, random
forest regression and k-nearest neighborhood regression used in this research (Chapter 2).
Then the paper seeks to minimize prediction errors and find significant features with these
methods (Chapter 3). Finally the paper summarizes our main findings (Chapter 4).

Methods:

Ordinary Linear Regression, Lasso Regression, Ridge Regression, Elastic Regression, PCA ,Neural Networks Random Forest, Xgboost

Findings

1. Team shots are more significant than team goals to predict the number of points for
a player.
2. Assists are more significant features than goals to predict the average number of points.
3. Weight and height are not very significant predictors. Most of the NHL scouts consider
weight and height to be important predictors, but if other variables held the same,
weight and height are not as significant as past points in predicting the number of
points in the future.
4. Player Data > Team Data> Opponent Data in terms of importance to predict the average number of points

All codes for data scraping are written in Python. All codes for machine learning are written in R.
https://github.com/takehiromatsuzawa/thesis

Thesis Advisor: 
Professor Kevin Rader 
