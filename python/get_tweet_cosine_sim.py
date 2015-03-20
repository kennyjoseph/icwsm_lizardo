__author__ = 'kjoseph'
import cPickle as pickle
from datetime import datetime
from multiprocessing import Pool
import os, sys, itertools
from gensim import corpora,  matutils
from scipy.spatial.distance import pdist
from casostwitter import general_utils

def get_tweet_cosine_sim(args):
    user_id, directory= args
    user = pickle.load(open(os.path.join(directory,user_id),"rb"))
    min_date = datetime(2014,1,1)

    documents = []

    informational_content_count = 0
    n_retweeted = 0
    n_hashtags_used = 0
    for tweet in user.tweets:
        if tweet.created_at < min_date:
            print tweet.
            documents.append(tweet.tokens)
            if tweet.retweeted_user_tweet_count > 0:
                n_retweeted += 1
            if 'RT' in tweet.text or 'HT' in tweet.text or 'MT' in tweet.text or 'via' in tweet.tokens or len(tweet.urls) > 0:
                informational_content_count += 1
            n_hashtags_used += len(tweet.hashtags)
    dictionary = corpora.Dictionary(documents)

    cosine_sim = 0
    if len(documents) >= 0:
        data = [dictionary.doc2bow(doc) for doc in documents if len(doc) > 0]
        numpy_matrix = matutils.corpus2dense(data,num_terms=len(dictionary.keys())).transpose()
        cosine_sim = 1-pdist(numpy_matrix,'cosine').mean()
    print 'n tweets: ', len(documents), ' n tokens: ', len(dictionary.keys())

    return user_id, cosine_sim, len(documents), informational_content_count, n_retweeted, n_hashtags_used

##read in the data
N_CPU = 50
pool = Pool(processes=N_CPU)

get_tweet_cosine_sim(["818379","./"])
sys.exit(-1)


users = [line.strip() for line in open("even_more_final_user_list.txt")]

results = pool.map(get_tweet_cosine_sim,
                  itertools.izip(users,
                                 itertools.repeat("../ego_network_data/obj")))

cosine_output_file = open("cosine_sim.tsv","w")
for result in results:
    cosine_output_file.write(general_utils.tab_stringify_newline(result))

cosine_output_file