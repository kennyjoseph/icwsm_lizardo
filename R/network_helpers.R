require(igraph)
require(tnet)

get_network_stats <- function(screen_name,
                              strong_tie_network=F,
                              relation_type="Mention",
                              plot_network=F,
                              directory="../tmp/all/",
                              strong_tie_weight=1){

  net <- data.table(get_network_for_user(screen_name,relation_type,directory))

  print(paste("The file for ", screen_name, " had :", nrow(net), " rows"))
  if(nrow(net) == 0){
    return(list())
  }
  
  ##remove self-loops
  net <- net[!(net$net_source == screen_name & net$sink == screen_name),]
  
  ##get mean strengths in and out
  mean_tie_in <- mean_cl_boot(net[net$sink==screen_name,"weight",with=F])
  mean_tie_out <- mean_cl_boot(net[net$net_source==screen_name,"weight",with=F])
  
  ##unique actors that sent ties to user
  sent_to_user <-unique(net[net$sink==screen_name,]$net_source)
  all_in_net <- union(sent_to_user,unique(net[net$net_source==screen_name,]$sink))
  
  ##get strong tie info
  strong_from_user <- unique(net[net_source==screen_name & 
                                   sink %in% sent_to_user & 
                                   weight >= strong_tie_weight, ]$sink)
  
  strong_to_user <- unique(net[sink==screen_name & 
                                 net_source %in% sent_to_user & 
                                 weight >= strong_tie_weight, ]$net_source)
  
  strong_all <- intersect(strong_from_user,strong_to_user)
  
  ##only consider those who have a reciprocal tie with the user, otherwise
  ##we're looking at networks of celebs, etc.
  if(strong_tie_network){
    net <- trim_network_including_isolates(net, screen_name,strong_all)
  } else {
    net <- trim_network_including_isolates(net,screen_name,sent_to_user)
  }
  g <- graph.data.frame(net[,c("net_source","sink","weight"),with=F])
  
  ##now, get rid of user himself
  user_vert <- which(V(g)$name == screen_name)
  
  ##get data on user ties
  non_user_ties <- E(g)[from(which(V(g)$name != screen_name)) & 
                          to(which(V(g)$name != screen_name))]
  user_ties <- delete.edges(g,non_user_ties)
  user_ties <- as.undirected(user_ties,mode="collapse",edge.attr.comb=list(weight=geometric_mean))
  mean_tie_recip <- mean_cl_boot(E(user_ties)$weight)
  
  g <- delete.vertices(g,V(g)[user_vert])
  ##edge weight is the geometric mean of the number of interactions between the two actors
  g <- as.undirected(g,mode="collapse",edge.attr.comb=list(weight=geometric_mean))
  ##here, get weighted clustering coefficient
  
  induced_graph <- induced.subgraph(g,V(g)[which(degree(g) !=0)])
  edgelist <- cbind(get.edgelist(induced_graph,names=F), E(g)$weight)
  clust_gm <- -1
  clust_am <- -1
  #if(nrow(edgelist) > 0 & !is.nan(transitivity(induced_graph))){
    
   # g_tnet <- as.tnet(rbind(edgelist,edgelist[,c(2,1,3)]))
  #  clust_gm <- as.numeric(clustering_w(g_tnet,"gm"))
  #  clust_am <- as.numeric(clustering_w(g_tnet,"am"))
  #}
  
  ##Community detection
  #mc <- multilevel.community(g)
  if(plot_network & nrow(net) != 0){
    tryCatch({
      png(filename=paste0("net_img/",screen_name,".png"), height=800, width=600)
      
      plot(mc,g,layout=layout.kamada.kawai,vertex.size=5,vertex.label=NA,edge.width=log(E(g)$weight+1))
    }, error = function(e){
      print(paste("Couldnt plot: ", screen_name))
    }, finally ={
      dev.off()
    })
  }
  
  #comps <- clusters(g, mode="weak")
  
  #n_strong=length(strong_all),
  #n_strong_out=length(strong_from_user),
  #n_strong_in=length(strong_to_user),
  l <- list( 
    n_total=length(all_in_net),
    n_total_recip=length(sent_to_user),
    n_nodes=vcount(g),
    n_edge=ecount(g),
    density = graph.density(g),
    mean_tie_str = mean(E(g)$weight),
    #n_components=comps$no,
    #n_isolates=sum(comps$csize==1),
    #mean_component_size = mean(comps$csize),
    #lwcc_size=max(comps$csize),
    tie_recip = mean_tie_recip[[1]],
    tie_recip_high = mean_tie_recip[[3]],
    tie_recip_low = mean_tie_recip[[2]],
    tie_in = mean_tie_in[[1]],
    tie_out = mean_tie_out[[1]],
    #modularity = max(mc$modularity),
    #clust_am = clust_am,
    #clust_gm = clust_gm
  )
  return(l)
}

get_network_for_user <- function(screen_name, type="all",  directory, remove_ego = F){
  file_name <- file.path(directory,paste0(screen_name,".csv"))
  if(!file.exists(file_name)){
    return(data.table())
  }
  tryCatch({
    net <- read.csv(file_name,header=F,stringsAsFactors=F) 
    net$V1 <- NULL
    names(net) <- c("net_source","sink","weight","type")
    if(type != "all"){
      net <- net[net$type ==type,]
    }
    if(remove_ego){
      net <- net[net$net_source != screen_name & net$sink != screen_name,]
    }
    return(net)
  }, error = function(e){
    print(e)
    return(data.table())
  })
  
  
}


trim_network_including_isolates <- function(net,screen_name,list_to_keep){
  keep_w_user <- c(list_to_keep, screen_name)
  ##have to do it this way to keep isolates
  net <- net[net$net_source %in% keep_w_user & net$sink %in% keep_w_user,]
  return(net)
}



get_sw_data <- function(screen_name,
                        directory="../tmp/out",
                        relation_type="Mention"){
  
  net <- data.table(get_network_for_user(screen_name,relation_type,directory))
  
  print(paste("The file for ", screen_name, " had :", nrow(net), " rows"))
  
  
  ##empty
  if(nrow(net) == 0){
    #return(as.integer(c(-1,-1,-1,-1)))
    return(as.integer(rep(-1,4)))
  }
  
  n_sent <- nrow(net[net$net_source == screen_name])
  
  ##remove self-loops
  net <- net[!(net$net_source == screen_name & net$sink == screen_name),]
  net <- net[!(net$net_source == net$sink)]
  ##unique actors that sent ties to user
  sent_to_user <-unique(net[net$sink==screen_name,]$net_source)
  all_in_net <- intersect(sent_to_user,unique(net[net$net_source==screen_name,]$sink))

  net <- net[net_source==screen_name & sink %in% all_in_net |
             sink == screen_name & net_source %in% all_in_net,]
  net$alter <- ifelse(net$net_source == screen_name,net$sink,net$net_source)

  if(nrow(net) == 0){
    #return(as.integer(c(-1,-1,-1,-1)))
    return(as.integer(c(rep(0,3),n_sent)))
  }
  
  data <- net[,min(weight)*log(max(weight)),by=alter]$V1
  ##these numbers are approximate quantiles
  return(as.integer(c(sum(data > .5 & data < 16),
                      sum(data > 16),
                      length(data),
                      n_sent)))
}


get_alter_net_stats <- function(twitter_id,strong_ego_ties,alter_connections){
  ego_ties <-strong_ego_ties[twitter_uid == twitter_id, ]
  if(nrow(ego_ties) == 0){
    return(data.frame(n_strong_ties=as.integer(0),n_conn=as.integer(0),clust=0.0))
  }
  alter_conn <- alter_connections[twitter_uid == twitter_id & 
                                    a1 %in% ego_ties$alter_id & 
                                    a2 %in% ego_ties$alter_id]
  if(nrow(alter_conn) == 0){
    return(data.frame(n_strong_ties=nrow(ego_ties),n_conn=as.integer(0),clust=0.0))
  }
  g <- graph.data.frame(alter_conn[,c("a1","a2","weight"),with=F],directed=F)
  n_conn <- length(E(g))
  
  g2<- graph.data.frame(ego_ties[,c("twitter_uid","alter_id","min_str"),with=F], directed=F)
  g3 <- graph.union(g,g2)

  E(g3)$weight <- ifelse(is.na(E(g3)$weight), E(g3)$min_str, E(g3)$weight)
  
  edgelist <- cbind(get.edgelist(g3,names=F), E(g3)$weight)
  
  clust <- as.numeric(clustering_w(as.tnet(rbind(edgelist,edgelist[,c(2,1,3)])),"gm"))
  return(data.frame(n_strong_ties=nrow(ego_ties),n_conn=as.integer(n_conn),clust=as.double(clust)))
}


