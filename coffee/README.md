simulation for 6/23/17 riddler: the coffee pot contest

single-run-results.svg shows the average coffee/turn for a 300 player corpus, over 8e10 total turns. single-run-results-zoom.svg trims players to those who attempt to take at least 0.4 of the pot.

multi-run-results.svg shows the results of an iterated version of the game, where player strategies are resampled each run, with higher weights given to those who perform better. in that simulation, 100 players are used and 1e10 turns are taken. again, multi-run-results.svg trims the plot to players who take at least 0.4 of the pot.

polarized-distribution-results.svg shows the results of a polarized distribution, where the likelyhood of choosing a strategy is proportional to the square of the distance from 0.5. 100 players, 10e8 rounds.

high-end-distribution-results.svg shows the results if approximately 90% of players chose values centered around 0.8, and the remainder choose values centered around 0.95. 100 players, 10e8 rounds.

spoiler-distribution-results.svg shows the effect of a small number of players with choices <0.2, assuming the vast majority of players choose something close to 0.8 or 1.

collusion-distribution shows the effect of a large number of players choosing a value of 0.21 in order to boost the score of players choosing 0.7.
