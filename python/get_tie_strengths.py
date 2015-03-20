
from multiprocessing import Pool
from datetime import datetime
from collections import Counter
import cPickle as pickle
import os, itertools, glob, sys
from casostwitter import general_utils

def get_tie_strength_network(args):
    user_id, directory, api_hook,i = args
    #print 'starting', user_id, i
    user = pickle.load(open(os.path.join(directory,user_id),"rb"))
    user.api_hook = api_hook
    #print 'getting friends and followers'

    if len(user.friend_ids) == 0 and len(user.follower_ids) == 0:
        user.populate_friends()
        user.populate_followers()
        pickle.dump(user,open(os.path.join(directory,user_id),"wb"))
    
    ##get reciprocal followings
    mutual_ff = set.intersection(set(user.friend_ids), set(user.follower_ids))

    #print '\tn mutual:', len(mutual_ff), len(user.friend_ids), len(user.follower_ids)
    ##get all mentions this year, count the number of tweets
    min_date = datetime(2014,1,1)
    mentions_set = set()
    mentions_this_year = Counter()
    total_user_tweets = 0
    for tweet in user.tweets:
        if tweet.created_at > min_date:
            total_user_tweets +=1
            for ent in tweet.mentions:
                mentions_this_year[ent] += 1
                mentions_set.add(ent)

    #print '\tn mentions: ', len(mentions_set), sum(mentions_this_year.values())
    ##ties are those actors that fit both conditions and have also mentioned the user
    possible_tie_set = set.intersection(mutual_ff,mentions_set)
    n_possible_tie = len(possible_tie_set)
    ties = []
    uid_integer = int(user_id)
    for alter_id in possible_tie_set:
        alter_mention_of_user_count = 0
        total_alter_tweets = 0
        if not os.path.exists(os.path.join(directory,str(alter_id))):
            continue

        alter = pickle.load(open(os.path.join(directory,str(alter_id))))
        #print '\tn alter followers: ', alter.followers_count
        #print 'alter tweets: ', len(alter.tweets)
        ##if alter has > 5K followers, ignore
        if alter.followers_count > 5000:
            n_possible_tie -= 1
            continue

        for tweet in alter.tweets:
            
            if tweet.created_at > min_date:
                total_alter_tweets += 1
                if uid_integer in tweet.mentions:
                    alter_mention_of_user_count += 1
        #print 'total alter tweets: ', total_alter_tweets
        #print '\t n alter tweets/mentions of self', total_alter_tweets, alter_mention_of_user_count

        ##if there has been reciprocal interaction in the last year,
        ## we call this a tie and record information about it
        if alter_mention_of_user_count > 0:
            ties.append([user.user_id, alter.user_id,
                         mentions_this_year[alter_id], alter_mention_of_user_count,
                         total_alter_tweets])

    print 'done', user_id, i
    return (ties, [user_id, total_user_tweets, n_possible_tie, len(mentions_set), sum(mentions_this_year.values())])


##read in the data
N_CPU = 50
pool = Pool(processes=N_CPU)

handles = general_utils.get_handles(glob.glob(os.path.join(sys.argv[1],"*.txt")))

users = [line.strip() for line in open("final_user_list.txt")]
handle_set = [handles[i % len(handles)] for i in range(len(users))]

#print get_tie_strength_network([users[0], "../ego_network_data/obj",handle_set[0],0])
#sys.exit(-1)

results = pool.map(get_tie_strength_network,
                  itertools.izip(users,
                                 itertools.repeat("../ego_network_data/obj"),
                                 handle_set,
                                 range(len(users))))

tie_output_file = open("tie_strength.tsv","w")
node_output_file = open("node_info.tsv","w")
for ties, node_info in results:
    for tie in ties:
        tie_output_file.write(general_utils.tab_stringify_newline(tie))
    node_output_file.write(general_utils.tab_stringify_newline(node_info))

tie_output_file.close()
node_output_file.close()
