from multiprocessing import Pool
from datetime import datetime
from collections import Counter, defaultdict
import cPickle as pickle
import os, itertools, glob, sys
from casostwitter import general_utils

def get_user(user_id, api_hook,directory):
    user = pickle.load(open(os.path.join(directory,user_id),"rb"))
    user.api_hook = api_hook

    if len(user.friend_ids) == 0 and len(user.follower_ids) == 0:
        user.populate_friends()
        user.populate_followers()
        pickle.dump(user,open(os.path.join(directory,user_id),"wb"))
    return user

def get_tie_strength_network(args):
    user_id, directory, api_hook,i = args
    #print 'starting', user_id, i
    user = get_user(user_id, api_hook, directory)
    
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

    uid_integer = int(user_id)
    real_user_ties = set()
    for alter_id in possible_tie_set:
        total_alter_tweets = 0
        if not os.path.exists(os.path.join(directory,str(alter_id))):
            continue

        alter = pickle.load(open(os.path.join(directory,str(alter_id))))
        ##if alter has > 5K followers, ignore
        if alter.followers_count > 5000:
            continue

        for tweet in alter.tweets:
            if tweet.created_at > min_date:
                total_alter_tweets += 1
                if uid_integer in tweet.mentions:
                    real_user_ties.add(alter_id)
                    break

    people_alters_mentioned = defaultdict(Counter)
    for alter_id in real_user_ties:
        alter = pickle.load(open(os.path.join(directory,str(alter_id))))
        for tweet in alter.tweets:
            if tweet.created_at > min_date:
                for mention in tweet.mentions:
                    if mention in real_user_ties:
                        people_alters_mentioned[alter_id][mention] += 1

    print 'getting alter connections'
    print people_alters_mentioned

    reciprocal_mention_net = defaultdict(defaultdict)
    ##okay, we here have the full mention network. now we have to find reciprocated mentions
    for id, mention_counter in people_alters_mentioned.items():
        for mentioned_by_id, mention_count in mention_counter.items():
            if id in people_alters_mentioned[mentioned_by_id]:
                ##link weight is minimum of the two counts
                reciprocal_mention_net[id][mentioned_by_id] = min(mention_count,people_alters_mentioned[mentioned_by_id][id])
                del people_alters_mentioned[mentioned_by_id][id]

    final_alter_ties = []
    ##now we have to ensure they're both following each other
    print reciprocal_mention_net
    for alter_id, comentioned_alters in reciprocal_mention_net.items():
        print 'getting alter: ', alter_id, ' who has n possible ties: ', len(comentioned_alters)
        print comentioned_alters
        alter = get_user(alter_id,api_hook,directory)
        mutual_ff = set.intersection(set(alter.friend_ids), set(alter.follower_ids))
        ties = set.intersection(mutual_ff, set(comentioned_alters.keys()))
        for tie in ties:
            final_alter_ties.append([alter_id,tie,comentioned_alters[tie]])

    print final_alter_ties

    print 'done', user_id, i
    return final_alter_ties

##read in the data
N_CPU = 50
pool = Pool(processes=N_CPU)

handles = general_utils.get_handles(glob.glob(os.path.join(sys.argv[1],"*.txt")))

users = [line.strip() for line in open("even_more_final_user_list.txt")]
handle_set = [handles[i % len(handles)] for i in range(len(users))]

print get_tie_strength_network([users[0], "../ego_network_data/obj",handle_set[0],0])
sys.exit(-1)

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
