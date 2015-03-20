
model_to_plot <- function(model,labels=NA){
  est<- data.frame(exp(cbind(est = coef(model), confint(model))))
  est <- est[2:nrow(est),]

  names(est) <- c("estimate","low","high")
  est$name <- rownames(est)
  est$name <- factor(est$name, levels=arrange(est,estimate)$name)
  p <- ggplot(est, aes(name,estimate,ymin=low,ymax=high)) + geom_pointrange()  + geom_hline(y=1,color='red') + xlab("") + ylab("Log(Incidence Rate Ratio)") + coord_flip()
  if(!is.na(labels)){
    p <- p + scale_x_discrete(labels=labels)
  }
  p <- p +scale_y_log10(labels=c(.5,1,5,10),breaks=c(.5,1,5,10),limits=c(0.5,max(est$high)))
  return(p)
  
}

get_coeff <- function(model){
  return(data.frame(exp(cbind(est = coef(model), confint(model)))))
}