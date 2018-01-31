# Dota2 is a free-to-play multiplayer online battle arena (MOBA) video game. Dota 2 is played in matches between two teams of five players, with each team occupying and defending their own separate base on the map. Each of the ten players independentlycontrols a powerful character, known as a "hero" (which they choose at the start of the match), who all have unique abilities and differing styles of play. During a match, players collect experience points and items for their heroes to successful battle with the opposing team's heroes, who attempt to do the same to them. A team wins by being the first to destroy a large structure located in the opposing team's base, called the "Ancient".


You’re given dataset of professional Dota players and their most frequent 10 heroes. The data also includes details about the heros (Kind of Hero (nuker, initiator and so on), their base attack, strength, movement speed). Here both train and test dataset is divided into two dataset(train9.csv & train1.csv and test9.csv & test1.csv).
 
train9.csv and train1.csv contain the user performance for their most frequent 9 heroes and 10th hero respectively. Both train9.csv and train1.csv have below fields.

test9.csv contain the different set of user (different from training set) performance for their most frequent 9 heroes. test9.csv has similar fields as train9.csv. Now, the aim is to predict the performance (kda_ratio) of the same set of users (test users) with the 10th hero which is test1.csv.

We also have "hero_data.csv" which contains information about heros.
