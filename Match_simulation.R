library(readr)

p2p_probabilities<-read.csv('p2p_probabilities.csv')
cluster_probabilities<-read.csv('cluster_probabilities_new.csv')
batting_clusters<-read.csv('BattingClusters.csv')
bowling_clusters<-read.csv('BowlingClusters.csv')

#Preprocessing the data
#set.seed(100)
#current_match <- "bigdatamatchsimulation/match1.txt"
current_match <- "test1.txt"

conn <- file(current_match,open="r")
linn <-readLines(conn)
#print(linn)
team1<-linn[1]
team2<-linn[2]
team1_batting_order<-linn[4:14]
team2_batting_order<-linn[16:26]
#print(team2)
no_of_bowlers<-as.integer(linn[28])

team1_bowling_order<-c(1:20)

for(i in c(1:no_of_bowlers))
{
	bowler<-linn[28+i]
	bowler<-strsplit(bowler," ")
	for(j in 1:length(bowler[[1]])-2)
	{
		team1_bowling_order[as.integer(bowler[[1]][j])]<-paste(bowler[[1]][1],bowler[[1]][2])
	}
}

team2_bowling_order<-c(1:20)
index<-28+as.integer(linn[28])+2
#print(index)
no_of_bowlers<-as.integer(linn[index])
#print(no_of_bowlers)
for(i in c(1:no_of_bowlers))
{
	bowler<-linn[index+i]
	bowler<-strsplit(bowler," ")
	for(j in 1:length(bowler[[1]])-2)
	{
		team2_bowling_order[as.integer(bowler[[1]][j])]<-paste(bowler[[1]][1],bowler[[1]][2])
	}
}
#print(index+no_of_bowlers+2)
team_batting_first<-linn[index+no_of_bowlers+2]
close(conn)
#print(team_batting_first)
match<-c(1:20)
over<-c(1:6)


team_order<-c()
team_order<-append(team_order,team_batting_first)

if(team_batting_first==team1)
{
	team_order<-append(team_order,team2)
}else
{
	team_order<-append(team_order,team1)
}

#print(team_order)



#Match Simulation
score_of_the_other_team<-0
count<-1
while(count<3)
{
if(team_order[count]==team1)
{
batsman1<-team1_batting_order[1]
batsman2<-team1_batting_order[2]
striker<-batsman1

names<-colnames(p2p_probabilities[3:9])
#print(names)

hash_table <- vector(mode="list", length=7)
names(hash_table)<-names
for(i in c(1:7))
{
hash_table[i]<-i-1
}
#print(hash_table)

original_p2p_data<-data.frame()
updated_p2p_data<-data.frame()
original_cluster_data<-data.frame()
updated_cluster_data<-data.frame()


runs<-0
temp<-0
wickets_lost<-0
current_wicket<-2
striker<-batsman1
runs_scored_this_ball<-0

number<-0
if(score_of_the_other_team==0)
{
	score_of_the_other_team<-500
}

for(over_no in match)
{
	for(ball in over)
	{	
		if(wickets_lost!=10 & runs<=score_of_the_other_team)
		{
			random_prob_for_runs<-runif(1,0.2,1)
		if(!identical(integer(0),which(p2p_probabilities$Batsman==striker & p2p_probabilities$Bowler==team2_bowling_order[over_no])))
		{
			#print(striker)
			#print(team2_bowling_order[over_no])
			#if((striker %in% original_p2p_data$Batsman) & (team2_bowling_order[over_no] %in% original_p2p_data$Bowler))
			if(!identical(integer(0),which(updated_p2p_data$Batsman==striker & updated_p2p_data$Bowler==team2_bowling_order[over_no])))
			{
				current_ball<-updated_p2p_data[which(updated_p2p_data$Batsman==striker & updated_p2p_data$Bowler==team2_bowling_order[over_no]),]
				#print('Hello')
				#print(original_p2p_data)
				current_ball_with_original_data<-original_p2p_data[which(original_p2p_data$Batsman==striker & original_p2p_data$Bowler==team2_bowling_order[over_no]),]
				probability<-current_ball$prob_not_out
				updated_probability<-current_ball
				updated_probability$prob_not_out<-(updated_probability$prob_not_out)*current_ball_with_original_data$prob_not_out
				
				if(probability<0.30)
				{
					wickets_lost<-wickets_lost+1
					current_wicket<-current_wicket+1
					if(striker==batsman1)
					{
						batsman1<-team1_batting_order[current_wicket]
						striker<-batsman1
						if(ball==6)
						{
							striker<-batsman2
						}
					}
					else
					{
						batsman2<-team1_batting_order[current_wicket]
						striker<-batsman2
						if(ball==6)
						{
							striker<-batsman1
						}
					}


					print(paste0('Over : ',over_no,'Ball : ',ball,' OUT'))

				}
				else
				{
					#print(current_ball)
					sum_of_probabilities<-sum(current_ball[c(3:9)])
					prev<-current_ball[3]/sum_of_probabilities
					#print('*************************************')
					#print(random_prob_for_runs<prev)
					if(random_prob_for_runs<prev)
					{
						runs_scored_this_ball<-0
						#print('%%%%%%%%%%%%%%%%%%%%')

					}
					else
					{
						for(z in c(4:9))
						{
							present<-current_ball[z]/sum_of_probabilities
							present<-present+prev
							if(random_prob_for_runs>=prev & random_prob_for_runs<present)
							{
								#print(paste0('Im here',z-3))
								runs_scored_this_ball<-(z-3)
								break
							}
							prev<-present
						}
					}
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
				#print(random_prob_for_runs)
				#print(runs_scored_this_ball)
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
	
				runs<-runs+runs_scored_this_ball
				#print(paste0('Over : ',over_no,' Ball : ',ball,' Runs : ',runs_scored_this_ball))
				
				if(runs_scored_this_ball%%2==0)
				{

				}
				else
				{
					if(striker==batsman1)
					{
						striker<-batsman2
					}
					else
					{
						striker<-batsman1
					}
				}

				}



				temp<-temp+1
			}
			else
			{
				current_ball<-p2p_probabilities[which(p2p_probabilities$Batsman==striker & p2p_probabilities$Bowler==team2_bowling_order[over_no]),]
				#print(current_ball)
				original_p2p_data<-rbind(original_p2p_data,current_ball)

				probability<-current_ball$prob_not_out
				updated_probability<-current_ball
				updated_probability$prob_not_out<-(updated_probability$prob_not_out)^2
				#print('&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&')
				#print(original_p2p_data)
				#print('&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&')

				if(probability<0.30)
				{
					wickets_lost<-wickets_lost+1
					current_wicket<-current_wicket+1
					if(striker==batsman1)
					{
						batsman1<-team1_batting_order[current_wicket]
						striker<-batsman1
						if(ball==6)
						{
							striker<-batsman2
						}
					}
					else
					{
						batsman2<-team1_batting_order[current_wicket]
						striker<-batsman2
						if(ball==6)
						{
							striker<-batsman1
						}
					}

					updated_p2p_data<-rbind(do.call(data.frame,updated_p2p_data),updated_probability)
					print(paste0('Over : ',over_no,' Ball : ',ball,' OUT'))

				}
				else
				{

					sum_of_probabilities<-sum(current_ball[c(3:9)])
					prev<-current_ball[3]/sum_of_probabilities
					#print('*************************************')
					#print(random_prob_for_runs<prev)
					if(random_prob_for_runs<prev)
					{
						runs_scored_this_ball<-0
						#print('%%%%%%%%%%%%%%%%%%%%')

					}
					else
					{
						for(z in c(4:9))
						{
							present<-current_ball[z]/sum_of_probabilities
							present<-present+prev
							if(random_prob_for_runs>=prev & random_prob_for_runs<present)
							{
								#print(paste0('Im here',z-3))
								runs_scored_this_ball<-(z-3)
								break
							}
							prev<-present
						}
					}
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
				#print(random_prob_for_runs)
				#print(runs_scored_this_ball)
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
	
				updated_p2p_data<-rbind(do.call(data.frame,updated_p2p_data),updated_probability)

				runs<-runs+runs_scored_this_ball
				print(paste0('Over : ',over_no,' Ball : ',ball,' Runs : ',runs_scored_this_ball))
				if(runs_scored_this_ball%%2==0)
				{

				}
				else
				{
					if(striker==batsman1)
					{
						striker<-batsman2
					}
					else
					{
						striker<-batsman1
					}
				}

				}
				#print(striker)
			}
		}
		else
		{

				batsman_cluster_data<-batting_clusters[which(batting_clusters$Batsman==striker),]
				batsman_cluster<-batsman_cluster_data$cluster_no
				bowler_cluster_data<-bowling_clusters[which(bowling_clusters$Bowler==team2_bowling_order[over_no]),]
				bowler_cluster<-bowler_cluster_data$cluster_no

				#print('---------------------------------------------------------')
				#print(striker)
				#print(team2_bowling_order[over_no])
				#print(which(updated_cluster_data$BatsmanCluster==striker & updated_cluster_data$BowlerCluster==team2_bowling_order[over_no]))
			if(!identical(integer(0),which(updated_cluster_data$BatsmanCluster==batsman_cluster & updated_cluster_data$BowlerCluster==bowler_cluster)))
			{

				current_ball_cluster<-updated_cluster_data[which(updated_cluster_data$Batsman==batsman_cluster & updated_cluster_data$Bowler==bowler_cluster),]

				current_ball_cluster_with_original_data<-original_cluster_data[which(original_cluster_data$Batsman==batsman_cluster & original_cluster_data$Bowler==bowler_cluster),]
				#print(current_ball)

				probability<-current_ball_cluster$prob_not_out
				#print(probability)

				#print(current_ball)
				#print('-----')
				updated_probability<-current_ball_cluster
				#print(updated_probability)
				updated_probability$prob_not_out<-(updated_probability$prob_not_out)*current_ball_cluster_with_original_data$prob_not_out
				
				if(probability<0.30)
				{
					#print('hello')
					wickets_lost<-wickets_lost+1
					current_wicket<-current_wicket+1
					if(striker==batsman1)
					{
						batsman1<-team1_batting_order[current_wicket]
						striker<-batsman1
						if(ball==6)
						{
							striker<-batsman2
						}
					}
					else
					{
						batsman2<-team1_batting_order[current_wicket]
						striker<-batsman2
						if(ball==6)
						{
							striker<-batsman1
						}
					}


					print(paste0('Over : ',over_no,' Ball : ',ball,' OUT'))

				}
				else
				{

					sum_of_probabilities<-sum(current_ball_cluster[c(3:9)])
					prev<-current_ball_cluster[3]/sum_of_probabilities
					#print('*************************************')
					#print(random_prob_for_runs<prev)
					if(random_prob_for_runs<prev)
					{
						runs_scored_this_ball<-0
						#print('%%%%%%%%%%%%%%%%%%%%')

					}
					else
					{
						for(z in c(4:9))
						{
							present<-current_ball_cluster[z]/sum_of_probabilities
							present<-present+prev
							if(random_prob_for_runs>=prev & random_prob_for_runs<present)
							{
								#print(paste0('Im here',z-3))
								runs_scored_this_ball<-(z-3)
								break
							}
							prev<-present
						}
					}
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
				#print(random_prob_for_runs)
				#print(runs_scored_this_ball)
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
	
				index_to_be_updated<-which(updated_cluster_data$Batsman==batsman_cluster & updated_cluster_data$Bowler==bowler_cluster)
				updated_cluster_data[index_to_be_updated,]<-updated_probability
				#print(updated_cluster_data)
				#updated_p2p_data<-updated_p2p_data[!which(updated_p2p_data$Batsman==striker & updated_p2p_data$Bowler==team2_bowling_order[over_no])]

				#updated_p2p_data<-rbind(updated_p2p_data,updated_probability)


				runs<-runs+runs_scored_this_ball
				print(paste0('Over : ',over_no,' Ball : ',ball,' Runs : ',runs_scored_this_ball))
				if(runs_scored_this_ball%%2==0)
				{

				}
				else
				{
					if(striker==batsman1)
					{
						striker<-batsman2
					}
					else
					{
						striker<-batsman1
					}
				}

				}



				temp<-temp+1
			}
			else
			{

				current_ball_cluster<-cluster_probabilities[which(cluster_probabilities$BatsmanCluster==batsman_cluster & cluster_probabilities$BowlerCluster==bowler_cluster),]
				#print(current_ball_cluster)
				original_cluster_data<-rbind(original_cluster_data,current_ball_cluster)
				
				probability<-current_ball_cluster$prob_not_out
				updated_probability<-current_ball_cluster
				updated_probability$prob_not_out<-(updated_probability$prob_not_out)^2


				if(probability<0.30)
				{
					wickets_lost<-wickets_lost+1
					current_wicket<-current_wicket+1
					if(striker==batsman1)
					{
						batsman1<-team1_batting_order[current_wicket]
						striker<-batsman1
						if(ball==6)
						{
							striker<-batsman2
						}
					}
					else
					{
						batsman2<-team1_batting_order[current_wicket]
						striker<-batsman2
						if(ball==6)
						{
							striker<-batsman1
						}
					}

					updated_cluster_data<-rbind(do.call(data.frame,updated_cluster_data),updated_probability)
					print(paste0('Over : ',over_no,' Ball : ',ball,' OUT'))

				}
				else
				{
				#print(current_ball_cluster[3:9])

					sum_of_probabilities<-sum(current_ball_cluster[c(3:9)])
					prev<-current_ball_cluster[3]/sum_of_probabilities
					#print('*************************************')
					#print(random_prob_for_runs<prev)
					if(random_prob_for_runs<prev)
					{
						runs_scored_this_ball<-0
						#print('%%%%%%%%%%%%%%%%%%%%')

					}
					else
					{
						for(z in c(4:9))
						{
							present<-current_ball_cluster[z]/sum_of_probabilities
							present<-present+prev
							if(random_prob_for_runs>=prev & random_prob_for_runs<present)
							{
								#print(paste0('Im here',z-3))
								runs_scored_this_ball<-z-3
								break
							}
							prev<-present
						}
					}
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
				#print(random_prob_for_runs)
				#print(runs_scored_this_ball)
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
	





				updated_cluster_data<-rbind(do.call(data.frame,updated_cluster_data),updated_probability)
				#print('hello world')

				runs<-runs+runs_scored_this_ball
				print(paste0('Over : ',over_no,' Ball : ',ball,' Runs : ',runs_scored_this_ball))
				if(runs_scored_this_ball%%2==0)
				{

				}
				else
				{
					if(striker==batsman1)
					{
						striker<-batsman2
					}
					else
					{
						striker<-batsman1
					}
				}

				}
				#print(striker)
			}





				number<-number+1

		}
		#print(striker)
	}
	else
	{
		#print('Innings Over')
		break 
	}
	}
	#break

	if(wickets_lost==10)
	{
		break
	}
	#break
}
#print(temp)
#print(number)
#print(runs)
#print(wickets_lost)
print(paste0(team1,' have ended their innings with a total score of ',runs))

team1_wickets_lost<-wickets_lost
team1_runs_scored<-runs
score_of_the_other_team<-team1_runs_scored
}
else
{
original_p2p_data<-data.frame()
updated_p2p_data<-data.frame()
original_cluster_data<-data.frame()
updated_cluster_data<-data.frame()


runs_team2<-0
temp<-0
wickets_lost<-0
current_wicket<-2

runs_scored_this_ball<-0
number<-0


batsman1<-team2_batting_order[1]
batsman2<-team2_batting_order[2]
striker<-batsman1
#print(striker)
#print(team1_bowling_order[1])
if(score_of_the_other_team==0)
{
	score_of_the_other_team<-500
}
for(over_no in match)
{
	for(ball in over)
	{	
		if(wickets_lost!=10 & runs_team2<=score_of_the_other_team)
		{
		random_prob_for_runs<-runif(1,0.2,1)
		if(!identical(integer(0),which(p2p_probabilities$Batsman==striker & p2p_probabilities$Bowler==team1_bowling_order[over_no])))
		{
			#print(striker)
			#print(team1_bowling_order[over_no])
			#if((striker %in% original_p2p_data$Batsman) & (team1_bowling_order[over_no] %in% original_p2p_data$Bowler))
			if(!identical(integer(0),which(updated_p2p_data$Batsman==striker & updated_p2p_data$Bowler==team1_bowling_order[over_no])))
			{
				current_ball<-updated_p2p_data[which(updated_p2p_data$Batsman==striker & updated_p2p_data$Bowler==team1_bowling_order[over_no]),]
				#print('Hello')
				#print(original_p2p_data)
				current_ball_with_original_data<-original_p2p_data[which(original_p2p_data$Batsman==striker & original_p2p_data$Bowler==team1_bowling_order[over_no]),]
				probability<-current_ball$prob_not_out
				updated_probability<-current_ball
				updated_probability$prob_not_out<-(updated_probability$prob_not_out)*current_ball_with_original_data$prob_not_out
				#print(current_ball)
				if(probability<0.3)
				{
					wickets_lost<-wickets_lost+1
					current_wicket<-current_wicket+1
					if(striker==batsman1)
					{
						batsman1<-team1_batting_order[current_wicket]
						striker<-batsman1
						if(ball==6)
						{
							striker<-batsman2
						}
					}
					else
					{
						batsman2<-team1_batting_order[current_wicket]
						striker<-batsman2
						if(ball==6)
						{
							striker<-batsman1
						}
					}


					print(paste0('Over : ',over_no,'Ball : ',ball,' OUT'))

				}
				else
				{

					sum_of_probabilities<-sum(current_ball[c(3:9)])
					prev<-current_ball[3]/sum_of_probabilities
					#print('*************************************')
					#print(random_prob_for_runs<prev)
					if(random_prob_for_runs<prev)
					{
						runs_scored_this_ball<-0
						#print('%%%%%%%%%%%%%%%%%%%%')

					}
					else
					{
						for(z in c(4:9))
						{
							present<-current_ball[z]/sum_of_probabilities
							present<-present+prev
							if(random_prob_for_runs>=prev & random_prob_for_runs<present)
							{
								#print(paste0('Im here',z-3))
								runs_scored_this_ball<-(z-3)
								break
							}
							prev<-present
						}
					}
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
				#print(random_prob_for_runs)
				#print(runs_scored_this_ball)
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
	
				runs_team2<-runs_team2+runs_scored_this_ball
				print(paste0('Over : ',over_no,' Ball : ',ball,' Runs : ',runs_scored_this_ball))
				
				if(runs_scored_this_ball%%2==0)
				{

				}
				else
				{
					if(striker==batsman1)
					{
						striker<-batsman2
					}
					else
					{
						striker<-batsman1
					}
				}

				}



				temp<-temp+1
			}
			else
			{
				current_ball<-p2p_probabilities[which(p2p_probabilities$Batsman==striker & p2p_probabilities$Bowler==team1_bowling_order[over_no]),]
				#print(current_ball)
				original_p2p_data<-rbind(original_p2p_data,current_ball)

				probability<-current_ball$prob_not_out
				updated_probability<-current_ball
				updated_probability$prob_not_out<-(updated_probability$prob_not_out)^2
				#print('&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&')
				#print(original_p2p_data)
				#print('&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&')
				#print(current_ball)

				if(probability<0.3)
				{
					wickets_lost<-wickets_lost+1
					current_wicket<-current_wicket+1
					if(striker==batsman1)
					{
						batsman1<-team1_batting_order[current_wicket]
						striker<-batsman1
						if(ball==6)
						{
							striker<-batsman2
						}
					}
					else
					{
						batsman2<-team1_batting_order[current_wicket]
						striker<-batsman2
						if(ball==6)
						{
							striker<-batsman1
						}
					}

					updated_p2p_data<-rbind(do.call(data.frame,updated_p2p_data),updated_probability)
					print(paste0('Over : ',over_no,' Ball : ',ball,' OUT'))

				}
				else
				{

					sum_of_probabilities<-sum(current_ball[c(3:9)])
					prev<-current_ball[3]/sum_of_probabilities
					#print('*************************************')
					#print(current_ball)
					#print(random_prob_for_runs<prev)
					if(random_prob_for_runs<prev)
					{
						runs_scored_this_ball<-0
						#print('%%%%%%%%%%%%%%%%%%%%')

					}
					else
					{
						for(z in c(4:9))
						{
							present<-current_ball[z]/sum_of_probabilities
							present<-present+prev
							if(random_prob_for_runs>=prev & random_prob_for_runs<present)
							{
								#print(paste0('Im here',z-3))
								runs_scored_this_ball<-(z-3)
								break
							}
							prev<-present
						}
					}
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
				#print(random_prob_for_runs)
				#print(runs_scored_this_ball)
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
	
				updated_p2p_data<-rbind(do.call(data.frame,updated_p2p_data),updated_probability)

				runs_team2<-runs_team2+runs_scored_this_ball
				print(paste0('Over : ',over_no,' Ball : ',ball,' Runs : ',runs_scored_this_ball))
				if(runs_scored_this_ball%%2==0)
				{

				}
				else
				{
					if(striker==batsman1)
					{
						striker<-batsman2
					}
					else
					{
						striker<-batsman1
					}
				}

				}
				#print(striker)
			}
		}
		else
		{

				batsman_cluster_data<-batting_clusters[which(batting_clusters$Batsman==striker),]
				batsman_cluster<-batsman_cluster_data$cluster_no
				bowler_cluster_data<-bowling_clusters[which(bowling_clusters$Bowler==team1_bowling_order[over_no]),]
				bowler_cluster<-bowler_cluster_data$cluster_no

				#print('---------------------------------------------------------')
				#print(striker)
				#print(team1_bowling_order[over_no])
				#print(which(updated_cluster_data$BatsmanCluster==striker & updated_cluster_data$BowlerCluster==team1_bowling_order[over_no]))
			if(!identical(integer(0),which(updated_cluster_data$BatsmanCluster==batsman_cluster & updated_cluster_data$BowlerCluster==bowler_cluster)))
			{

				current_ball_cluster<-updated_cluster_data[which(updated_cluster_data$Batsman==batsman_cluster & updated_cluster_data$Bowler==bowler_cluster),]

				current_ball_cluster_with_original_data<-original_cluster_data[which(original_cluster_data$Batsman==batsman_cluster & original_cluster_data$Bowler==bowler_cluster),]
				#print(current_ball)

				probability<-current_ball_cluster$prob_not_out
				#print(probability)

				#print(current_ball_cluster)
				#print('-----')
				updated_probability<-current_ball_cluster
				#print(updated_probability)
				updated_probability$prob_not_out<-(updated_probability$prob_not_out)*current_ball_cluster_with_original_data$prob_not_out
				
				if(probability<0.3)
				{
					#print('hello')
					wickets_lost<-wickets_lost+1
					current_wicket<-current_wicket+1
					if(striker==batsman1)
					{
						batsman1<-team1_batting_order[current_wicket]
						striker<-batsman1
						if(ball==6)
						{
							striker<-batsman2
						}
					}
					else
					{
						batsman2<-team1_batting_order[current_wicket]
						striker<-batsman2
						if(ball==6)
						{
							striker<-batsman1
						}
					}


					print(paste0('Over : ',over_no,' Ball : ',ball,' OUT'))

				}
				else
				{

					sum_of_probabilities<-sum(current_ball_cluster[c(3:9)])
					prev<-current_ball_cluster[3]/sum_of_probabilities
					#print('*************************************')
					#print(current_ball_cluster)
					#print(random_prob_for_runs<prev)
					if(random_prob_for_runs<prev)
					{
						runs_scored_this_ball<-0
						#print('%%%%%%%%%%%%%%%%%%%%')

					}
					else
					{
						for(z in c(4:9))
						{
							present<-current_ball_cluster[z]/sum_of_probabilities
							present<-present+prev
							if(random_prob_for_runs>=prev & random_prob_for_runs<present)
							{
								#print(paste0('Im here',z-3))
								runs_scored_this_ball<-(z-3)
								break
							}
							prev<-present
						}
					}
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
				#print(random_prob_for_runs)
				#print(runs_scored_this_ball)
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
	
				index_to_be_updated<-which(updated_cluster_data$Batsman==batsman_cluster & updated_cluster_data$Bowler==bowler_cluster)
				updated_cluster_data[index_to_be_updated,]<-updated_probability
				#print(updated_cluster_data)
				#updated_p2p_data<-updated_p2p_data[!which(updated_p2p_data$Batsman==striker & updated_p2p_data$Bowler==team1_bowling_order[over_no])]

				#updated_p2p_data<-rbind(updated_p2p_data,updated_probability)


				runs_team2<-runs_team2+runs_scored_this_ball
				print(paste0('Over : ',over_no,' Ball : ',ball,' Runs : ',runs_scored_this_ball))
				if(runs_scored_this_ball%%2==0)
				{

				}
				else
				{
					if(striker==batsman1)
					{
						striker<-batsman2
					}
					else
					{
						striker<-batsman1
					}
				}

				}



				temp<-temp+1
			}
			else
			{

				current_ball_cluster<-cluster_probabilities[which(cluster_probabilities$BatsmanCluster==batsman_cluster & cluster_probabilities$BowlerCluster==bowler_cluster),]
				#print(current_ball_cluster)
				original_cluster_data<-rbind(original_cluster_data,current_ball_cluster)
				
				probability<-current_ball_cluster$prob_not_out
				updated_probability<-current_ball_cluster
				updated_probability$prob_not_out<-(updated_probability$prob_not_out)^2
				#print(current_ball_cluster)

				if(probability<0.3)
				{
					wickets_lost<-wickets_lost+1
					current_wicket<-current_wicket+1
					if(striker==batsman1)
					{
						batsman1<-team1_batting_order[current_wicket]
						striker<-batsman1
						if(ball==6)
						{
							striker<-batsman2
						}
					}
					else
					{
						batsman2<-team1_batting_order[current_wicket]
						striker<-batsman2
						if(ball==6)
						{
							striker<-batsman1
						}
					}

					updated_cluster_data<-rbind(do.call(data.frame,updated_cluster_data),updated_probability)
					print(paste0('Over : ',over_no,' Ball : ',ball,' OUT'))

				}
				else
				{
				#print(current_ball_cluster[3:9])

					sum_of_probabilities<-sum(current_ball_cluster[c(3:9)])
					prev<-current_ball_cluster[3]/sum_of_probabilities
					#print('*************************************')
					#print(random_prob_for_runs<prev)
					if(random_prob_for_runs<prev)
					{
						runs_scored_this_ball<-0
						#print('%%%%%%%%%%%%%%%%%%%%')

					}
					else
					{
						for(z in c(4:9))
						{
							present<-current_ball_cluster[z]/sum_of_probabilities
							present<-present+prev
							if(random_prob_for_runs>=prev & random_prob_for_runs<present)
							{
								#print(paste0('Im here',z-3))
								runs_scored_this_ball<-z-3
								break
							}
							prev<-present
						}
					}
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
				#print(random_prob_for_runs)
				#print(runs_scored_this_ball)
				#print('@@@@@@@@@@@@@@@@@@@@@@@@@@@')
	





				updated_cluster_data<-rbind(do.call(data.frame,updated_cluster_data),updated_probability)
				#print('hello world')

				runs_team2<-runs_team2+runs_scored_this_ball
				print(paste0('Over : ',over_no,' Ball : ',ball,' Runs : ',runs_scored_this_ball))
				if(runs_scored_this_ball%%2==0)
				{

				}
				else
				{
					if(striker==batsman1)
					{
						striker<-batsman2
					}
					else
					{
						striker<-batsman1
					}
				}

				}
				#print(striker)
			}





				number<-number+1

		}
		#print(striker)
	}
	else
	{
		#print('Innings Over')
		break 
	}
	}
	#break

	if(wickets_lost==10)
	{
		break
	}
	#break
}
#print(runs_team2)
#print(runs)
#print('hello there')
#print(runs_team2!=runs)
team2_runs_scored<-runs_team2
team2_wickets_lost<-wickets_lost
score_of_the_other_team<-team2_runs_scored
print(paste0(team2,' have ended their innings with a total score of ',runs_team2))
}
count<-count+1
}

print(paste0(team1,' : ',team1_runs_scored,'/',team1_wickets_lost))
print(paste0(team2,' : ',team2_runs_scored,'/',team2_wickets_lost))

result_of_the_match<-c()
result_of_the_match_data<-data.frame()

if(runs_team2==runs)
{
	result_of_the_match<-append(result_of_the_match,team1)
	result_of_the_match<-append(result_of_the_match,"Draw")
	result_of_the_match<-append(result_of_the_match,team2)
	result_of_the_match<-append(result_of_the_match,"-")
	result_of_the_match<-append(result_of_the_match,"-")
	print(paste0('Match drawn between ',team1,' and ',team2)) 
}else
{
	if(team2_runs_scored>team1_runs_scored)
	{
		result_of_the_match<-append(result_of_the_match,team2)
		result_of_the_match<-append(result_of_the_match,"Won")
		result_of_the_match<-append(result_of_the_match,team1)
		if(team2==team_batting_first)
		{
			diff<-team2_runs_scored-team1_runs_scored
			print(paste0(team2,' win over ',team1,' by ',diff,' runs')) 
			result_of_the_match<-append(result_of_the_match,diff)
			result_of_the_match<-append(result_of_the_match,"runs")
   			write_csv(result_of_the_match,"Results.csv", append = TRUE, col_names = TRUE)
		}
		else
		{
			diff<-10-team2_wickets_lost
			print(paste0(team2,' win over ',team1,' by ',diff,' wickets'))
			result_of_the_match<-append(result_of_the_match,diff)
			result_of_the_match<-append(result_of_the_match,"wickets") 

		}
	}
	else
	{
		result_of_the_match<-append(result_of_the_match,team1)
		result_of_the_match<-append(result_of_the_match,"Won")
		result_of_the_match<-append(result_of_the_match,team2)
		if(team1==team_batting_first)
		{
			diff<-team1_runs_scored-team2_runs_scored
			print(paste0(team1,' win over ',team2,' by ',diff,' runs')) 
			result_of_the_match<-append(result_of_the_match,diff)
			result_of_the_match<-append(result_of_the_match,"runs") 
		}
		else
		{
			diff<-10-team1_wickets_lost
			print(paste0(team1,' win over ',team2,' by ',diff,' wickets')) 
			result_of_the_match<-append(result_of_the_match,diff)
			result_of_the_match<-append(result_of_the_match,"wickets") 
		}

	}
	
}
#print(result_of_the_match)

result_of_the_match_data<-rbind(do.call(data.frame,result_of_the_match_data),result_of_the_match)
names(result_of_the_match_data)<-c("Team1","Status","Team2","By","Runs/Wickets")

results_filename<-"Results_test1.csv"
if(!file.exists(results_filename))
{
	write_csv(result_of_the_match_data,results_filename, append = TRUE, col_names = TRUE)
}else
{
	write_csv(result_of_the_match_data,results_filename, append = TRUE, col_names = FALSE)
}

