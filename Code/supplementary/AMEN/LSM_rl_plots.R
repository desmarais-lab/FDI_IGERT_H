# clear workspace, setting random seed, and setting work directory
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(amen)


#load ERGM results for with
load("LSM_nrm_results.RData")

#check convergence
#for(i in 1:11){
#  beta.mcmc <- as.mcmc(LSMs[[i]]$BETA)
#  print(geweke.diag(beta.mcmc))
#}

confint_ame <- function(x, conf){
  df <- as.data.frame(matrix(nrow= ncol(x),ncol=5))
  colnames(df) <- c("var", "estimate", "error","conf.low", "conf.high")
  df[,1] <- colnames(x)
  n <- nrow(x)-1
  for(r in 1:nrow(df)){df[r,2] <- mean(x[,r])}
  for(r in 1:nrow(df)){df[r,3] <- qt(conf,df=n-1)*sd(x[,r])}
  for(r in 1:nrow(df)){df[r,4] <- df[r,2]-df[r,3]}
  for(r in 1:nrow(df)){df[r,5] <- df[r,2]+df[r,3]}
  return(df)
}





#mean(x[,r])/(-0.862 + sqrt(0.743 - 2.404*log(p)))

summary(x)
#put results into df
years <- 2002:2012
df <- confint_ame(LSMs[[1]]$GOF, conf=.975)
df$year <- 2002
for(i in 2:11){
  dfi <- confint_ame(LSMs[[i]]$GOF, conf=.975)
  dfi$year <- years[i]
  df <- rbind(df, dfi)
}
for(i in 1:11){
  dfi <- confint_ame(LSMs[[i]]$BETA, conf=.975)
  dfi$year <- years[i]
  df <- rbind(df, dfi)
}


#set vars to loop through.
vars <- unique(df$var)

for(i in 1:21){
  df2 <- subset(df, df$var==vars[i])
  name <- ggplot(df2, aes(x=year, y = estimate, ymin= conf.low, ymax= conf.high,
                         colour="#D55E00",legend=TRUE), legend=TRUE)+
    geom_pointrange(size= .5, fatten = 1.25)+ geom_hline(yintercept = 0)+
    scale_x_continuous(breaks= seq(2002,2012,3))+
    xlab("Year") + ylab("Coefficient")+ 
    theme(axis.text  = element_text(family="Times", size=9),
          axis.title  = element_text(family="Times", size=8),
          legend.text  = element_text(family="Times"),
          legend.title  = element_text(family="Times"),  
          plot.title = element_text(family="Times"),
          legend.position="none")
  #create plot object
  ggsave(paste("LSM_rl_plots/",vars[i],".pdf", sep=""), 
         name, device="pdf", width=2.565, height=1.8)
}