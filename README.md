# Simulation-of-an-IPL-match-using-Machine-Learning-
Using the ball to ball data available, ipl match is simulated using analysis and machine learning

The simulation of an IPL match with the chosen teams and respective members of the teams is done ball by ball.  This is done by using player data in all formats. The player vs player (batsman vs bowler) data was used. But if a player has not played against another, then clustering is used. The batsman and bowlers profile are clustered separately and the cluster vs cluster probabilities were retrieved and used for the simulation. The effect of a ball play will be chance based but on the number of times a player has scored 1’s, 2’s, 4’s,got out, etc and normalised.
The clustering method use was k-means. The optimal k value was chosen by running over several k values and over a couple of matches. The accuracy got was on average 92% when validated against the IPL match of 2017.

Order Of Execution:
p2p_probabilities
batsman
bowler
clustering
cluster_probabilities
Match_simulation
