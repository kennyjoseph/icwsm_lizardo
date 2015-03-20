import os, sys, codecs, glob
from casostwitter import general_utils
from casostwitter.TwitterUser import get_user_ids_and_sn_data_from_list

CREDS_DIR = '/Users/kjoseph/git/thesis/thesis_python/twitter_login_creds'
USER_IDS_FILE = '/Users/kjoseph/git/foursquare_ccm/data/ids_left.tsv'
OUTPUT_FILE = '/Users/kjoseph/git/foursquare_ccm/data/twitter_user_ids_left2.tsv'


handles = general_utils.get_handles(glob.glob(os.path.join(CREDS_DIR,"*.txt")))
print 'n connections: ', len(handles)

data = set([f.strip() for f in open(USER_IDS_FILE).readlines()])

already_got = set([l.split(",")[1] for l in codecs.open('/Users/kjoseph/git/foursquare_ccm/data/twitter_user_ids_left.tsv',"r","utf8")])

data = [x for x in data.difference(already_got)]

print 'n to get: ', len(data)

user_data = get_user_ids_and_sn_data_from_list(data,handles,False, OUTPUT_FILE)
