This is a branch
# Thesis


Goal creation in ice hockey is complicated and difficult to understand. Unlike baseball, players and puck are always moving and a combination of players produces a goal. A team is always looking for a player who can contribute to team goals. National Hockey League has about 7000 goals per season. This study analyzes goals and assists in NHL to predict the number of goals of each player in the following season by finding most similar players in the past.

I scraped NHL game documents from NHL Hockey Reference to get features of each player. The features include height, weight, draft positions, goals, assists and so on.
I used the following four methods to predict the number of goals of each player.

1. PCA I extracted 23 features in total and used PCA to reduce dimensions to 1 to 6. I predicted the goal of 2016-2017 and figured out which dimension is the best to predict the goal and assists.

2. Linear Regressions I used Lasso, Ridge and Ordinary Linear Regressions to predict the number of goals.

3. KNN I used K-Nearest Neighbors to find out similar players and predict the number of goals

4. Neural Networks I used neural networks to predict the number of goals based on multiple statis- tics.
