import glob,sys, os
from casostwitter import general_utils
from casostwitter.WorkerTwitterEgoNetwork import TwitterEgoNetworkWorker

## fake out script arguments
top_dir = "/usr4/kjoseph/foursquare_ccm/"

sys.argv = ['',
            '/usr3/kjoseph/thesis_python/twitter_login_creds',
            top_dir+'ego_network_data',
            top_dir+'data/twitter_users_for_ego_final.tsv']


if len(sys.argv) != 4:
    print 'usage:  [login_credentials_directory] [output_dir] [user_sn_file]'
    sys.exit(-1)

OUTPUT_DIRECTORY = sys.argv[2]

##get all the handles we have to the api
handles = general_utils.get_handles(glob.glob(os.path.join(sys.argv[1],"*.txt")))

print 'n authed users: ', len(handles)

#user screen names we are interested in
user_id_screenname_pairs = [line.strip().split("\t") for line in open(sys.argv[3]).readlines()]

pickle_dir = OUTPUT_DIRECTORY +"/obj/"
network_dir = OUTPUT_DIRECTORY+"/net/"

general_utils.mkdir_no_err(OUTPUT_DIRECTORY)
general_utils.mkdir_no_err(pickle_dir)
general_utils.mkdir_no_err(network_dir)

general_utils.init_good_sync_manager()

##put data on the queue
request_queue = general_utils.load_request_queue(user_id_screenname_pairs, len(handles))

processes = []
for i in range(len(handles)):
    p = TwitterEgoNetworkWorker(request_queue, handles[i], network_dir, pickle_dir)
    p.start()
    processes.append(p)

try:
    for p in processes:
        p.join()
except KeyboardInterrupt:
    print 'keyboard interrupt'




