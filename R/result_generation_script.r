require(reshape)
require(stringr)
require(cluster)
require(fastICA)
require(bit64)
require(dplyr)
require(fitdistrplus)
require(MASS)

source("network_helpers.R")
source("analysis_helpers.R")

PREF_CATEGORIES <- c("art","game", "health", "hobby","humor", "movie","music","science","sport")
USER_INFO_FILE_HEADERS <- c("twitter_uid","twitter_screenname","n_status","n_follower","n_friend","n_listed")

#################################################
######GET THE DATA###############################
#################################################

load("user_data.rdata")

##we can pull out a bunch of data from here now and go get the network data from Twitter.
##to do so, we run "../python/ego_net_extractor.py"

#Then, we go get the tie strengths using ../python/get_tie_strengths.py
###then run get_alter_connections.py to get  (use this later - when getting alter_connections)

##finally, i got the cosine similarity of the user's 2014 tweets using ../python/get_cosine_sim.py

##these should all probably be (easily) integrated into a single script at some point

##this is basic info output by get_tie_strengths on the user's 2014 tweets
user_2014_tweet_info <- fread("../data/tie_info/node_info.tsv")
setnames(user_2014_tweet_info,c("twitter_uid","tweets_in_2014","n_possible_ties","n_users_mentioned_2014","total_mentions_2014"))

##output of get_tie_strengths
all_ego_ties <- fread("../data/tie_info/tie_strength.tsv")
setnames(all_ego_ties,c("twitter_uid","alter_id","user_alter_2014","alter_user_2014","total_alter_tweets"))
##calculate min to determing strong v weak ties
all_ego_ties$min_str <- with(all_ego_ties,ifelse(user_alter_2014 < alter_user_2014, user_alter_2014, alter_user_2014))

##ties w/ >2 mutual interactions are strong
all_ego_ties$strength <- "w"
all_ego_ties$strength[all_ego_ties$min_str > 2] <- "s"

n_tie_data <- all_ego_ties[,list(strong_ties=sum(strength=='s'), weak_ties=sum(strength=='w'), n_ties=length(strength)),by='twitter_uid']

tie_user_data <- merge(user_data, user_2014_tweet_info, by="twitter_uid",all.y=T)
tie_user_data <- merge(tie_user_data,n_tie_data,by="twitter_uid",all.x=T)

##if the user didn't have any ties, change from NA to 0
tie_user_data$strong_ties[is.na(tie_user_data$strong_ties)] <- 0
tie_user_data$weak_ties[is.na(tie_user_data$weak_ties)] <- 0
tie_user_data$n_ties[is.na(tie_user_data$n_ties)] <- 0

##set user's "total preferences"
tie_user_data$total_pref <- with(tie_user_data,weak+strong)

##messed up sampling of the 0/0 nodes, have to remove a few.
#also get rid of people who weren't very active in 2014
tie_user_data <- tie_user_data[n_checkin >= 10,]
tie_user_data <- tie_user_data[tweets_in_2014 > 50]
tie_user_data <- tie_user_data[n_possible_ties > 0]
tie_user_data <- tie_user_data[n_follower < 5000]

##pretty good fits to negative binomial
plot(fitdist(tie_user_data$n_ties,'nbinom'))
plot(fitdist(tie_user_data$strong_ties,'nbinom'))
plot(fitdist(tie_user_data$weak_ties,'nbinom'))

##cosine similarity and other topical metrics on the pre 2014 data
topical_info <- fread("../data/cosine_sim.tsv")
setnames(topical_info, c("twitter_uid","cosine_sim","n_tweets_prior_2014",
                         "informational_tweet_count", "n_times_retweeted", "n_hashtags_used"))
##BLAH. N times retweeted fail - bug in the code.
tie_user_data <- merge(tie_user_data, topical_info, by="twitter_uid")
##get rid of users we don't have data for
tie_user_data <- tie_user_data[n_tweets_prior_2014 > 1]
##standardize our predictor variables
standardize <- function(x){ (x-mean(x))/(2*sd(x))}
tie_user_data$lc <- with(tie_user_data,standardize(log(n_checkin)))
tie_user_data$ltw <- with(tie_user_data,standardize(log(tweets_in_2014)))
tie_user_data$lfol <- with(tie_user_data,standardize(log(n_follower)))
tie_user_data$lment <- with(tie_user_data,standardize(log(total_mentions_2014)))
tie_user_data$total_pref_st <- with(tie_user_data,standardize(total_pref))
tie_user_data$strong_pref_st <- with(tie_user_data,standardize(strong))
tie_user_data$weak_pref_st <- with(tie_user_data,standardize(weak))
tie_user_data$listed_count_st <- with(tie_user_data,standardize(n_listed))
tie_user_data$cosine_sim_st <- standardize(tie_user_data$cosine_sim)
tie_user_data$info_tweet_st <- standardize(tie_user_data$informational_tweet_count/tie_user_data$n_tweets_prior_2014)
tie_user_data$ht_st <- standardize(tie_user_data$n_hashtags_used/tie_user_data$n_tweets_prior_2014)

###############HYPOTHESES TESTING##################################

theme_set(theme_bw(17))

total <- glm.nb(n_ties~ltw+lc+lment+cosine_sim_st+total_pref_st+info_tweet_st+ht_st, data = tie_user_data)
anova(total,update(total, .~.-info_tweet_st-ht_st-ltw))
total.best <- update(total, .~.-ht_st-ltw)
total_plot <- model_to_plot(total.best)  + scale_x_discrete(labels=rev(c(
  "Log(N Mentions in 2014)","Total Cult. Pref.", "Log(N Checkins)", "Proportion Info Content", "Cosine Similarity of Tweets")))
ggsave("/Users/kjoseph/Dropbox/Kenny/papers/current/fs_lizardo/total_res.png",total_plot,h=3,w=6)


weak <- update(total,weak_ties~.-total_pref_st+weak_pref_st+strong_pref_st)
anova(weak, update(weak, .~.-ht_st))
weak.best <- update(weak, .~.-ht_st)
summary(weak.best)
anova(weak.best, update(weak.best, .~.-strong_pref_st))
weak.best <- update(weak.best, .~.-strong_pref_st)
anova(weak.best, update(weak.best, .~.-info_tweet_st))
weak.best <- update(weak.best, .~.-info_tweet_st)
anova(weak.best, update(weak.best,.~.-lc))
weak.best <- update(weak.best,.~.-lc)
anova(weak.best, update(weak.best,.~.-ltw))
weak.best <- update(weak.best,.~.-ltw)
weak_plot <- model_to_plot(weak.best)  + scale_x_discrete(labels=rev(c(
  "Log(N Mentions in 2014)","Num. Weak Cult. Pref.", "Cosine Similarity of Tweets")))
ggsave("/Users/kjoseph/Dropbox/Kenny/papers/current/fs_lizardo/weak_res.png",weak_plot,h=3,w=6)

strong <- update(total,strong_ties~.-total_pref_st+weak_pref_st+strong_pref_st)
##remove ht
anova(strong, update(strong,.~.-strong_pref_st-weak_pref_st-lc-ht_st))
strong.best <-update(strong,.~.-strong_pref_st-weak_pref_st-lc-ht_st)
strong_plot <- model_to_plot(strong.best)  + scale_x_discrete(labels=rev(c("Log(N Mentions in 2014)",
                                                                           "Log(N Tweets in 2014)",
                                                                           "Proportion Info Content","Cosine Similarity of Tweets")))
ggsave("/Users/kjoseph/Dropbox/Kenny/papers/current/fs_lizardo/strong_res.png",strong_plot,h=3,w=6)

#Now I have to move some data along to get H1, H2
alter_connections <- fread("../data/alter_tie_strength.tsv")
setnames(alter_connections, c("twitter_uid","a1","a2","weight"))
alter_connections <- alter_connections[twitter_uid %in% tie_user_data$twitter_uid]
wt <- tie_user_data[, get_alter_net_stats(twitter_uid,all_ego_ties,alter_connections),by="twitter_uid"]
wtd <- merge(tie_user_data,wt,by="twitter_uid")

wtnb <- glm.nb(n_conn~ltw+lc+lment+cosine_sim_st+info_tweet_st+ht_st+strong_pref_st+weak_pref_st+offset(log(n_ties^2)), data = wtd[n_ties >1])
anova(wtnb, update(wtnb,.~.-lc-strong_pref_st-weak_pref_st-info_tweet_st-lment))
wtnb <- update(wtnb,.~.-lc-strong_pref_st-weak_pref_st-info_tweet_st-lment)

wtnb_plot <- model_to_plot(wtnb) + scale_x_discrete(labels=
                                                      rev(c("Cosine Similarity of Tweets",
                                                            "Hashtag Usage",
                                                            "Log(N Tweets in 2014)")))


ggsave("/Users/kjoseph/Dropbox/Kenny/papers/submitted/fs_lizardo/all_n_conn_res.png",wtnb_plot,h=3,w=6)



wt_cos_plot <- ggplot(tie_user_data[-1497], aes(weak,cosine_sim)) + stat_summary(fun.data="mean_cl_boot")  + stat_smooth(method='lm')  + ylab("Cosine Similarity") + xlab("Number of Weak Cultural Preferences")
ggsave("/Users/kjoseph/Dropbox/Kenny/papers/submitted/fs_lizardo/wt_cos_res.png",wt_cos_plot,h=4,w=7)

st_cos_plot <- ggplot(tie_user_data[-1497], aes(strong,cosine_sim)) + stat_summary(fun.data="mean_cl_boot")  + stat_smooth(method='lm')  + ylab("Cosine Similarity") + xlab("Number of Strong Cultural Preferences")
ggsave("/Users/kjoseph/Dropbox/Kenny/papers/submitted/fs_lizardo/st_cos_res.png",st_cos_plot,h=4,w=7)


tie_user_data$info_prop <- tie_user_data$informational_tweet_count/tie_user_data$n_tweets_prior_2014

##correlation between informational tweet count and tweets in 2014
f <- function(d, i){
  d2 <- d[i,]
  return(cor(d2$info_prop, log(d2$tweets_in_2014), use="pairwise.complete.obs"))
}

bootcorr <- boot(tie_user_data, f, R=10000)
boot.ci(bootcorr, type = "all")


