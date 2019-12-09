# clear workspace
#rm(list=ls())
#set.seed(19)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

simdata <- read.csv("contagion_simulation.csv")

boot.ci.mean <- function(y,level,nrep){
	values <- numeric(nrep)
	for(i in 1:nrep){
		yi <- y[sample(1:length(y),length(y),rep=T)]
		values[i] <- mean(yi,na.rm=T)
	}
	tail <- (1-level)/2
	quantile(values,c(tail,1-tail),na.rm=T)
}

names(simdata) <- c("mean_edge_val","mean_j_sends", "mean_i_sends", "mean_j_receives", "mean_i_receives")

results_summary <- NULL
for(c in 2:5){
	upper <- c(5,15,max(simdata[,1],na.rm=T))
	lower <- c(min(simdata[,1],na.rm=T),5,15)
	for(u in 1:3){
		y_uc <- simdata[which(simdata[,1]>=lower[u] & simdata[,1] <= upper[u]),c]
		res_uc <- c(c,u,mean(y_uc,na.rm=T),boot.ci.mean(y_uc,level=0.95,nrep=500))
		results_summary <- rbind(results_summary,res_uc)
	}
	print(c)
}

ylims <- c(min(c(results_summary[,3:5])),max(c(results_summary[,3:5])))
x_placement <- (results_summary[,2]-1)*5+results_summary[,1]-1
x_placement <- x_placement-min(x_placement)
xlims <- c(min(x_placement),max(x_placement))

colrs <- c("black","red","blue","grey60")[results_summary[,1]-1]

pchs <- c(19,22,24,25)[results_summary[,1]-1]

  
  pdf("figures/contagion_simulation_results.pdf",height=3.25,width=5.5,family="Times")
  par(las=1,mar=c(4,4,1,1))
  plot(x_placement,100*results_summary[,3],type="n",xaxt="n",ylim=100*ylims,xlim=xlims,xlab="",ylab="percent change in mean edge")
  abline(h=0,lwd=1.5,lty=2,v=c(2.25/7,4.75/7)*xlims[2])
  segments(x0=x_placement,x1=x_placement,y0=100*results_summary[,4],y1=100*results_summary[,5],col=colrs,lwd=2.5)
  points(x_placement,100*results_summary[,3],pch=pchs,col=colrs,bg=colrs)
  text(2.05,-6.5,"0 - 5",xpd=T)
  text(6.5,-6.5,"5 - 15",xpd=T)
  text(11,-6.5,"> 15",xpd=T)
  title(xlab="Expected value of edge i -> j",line=2.25)
  legend("bottomleft",legend=c("sent by j","sent by i","sent to j","sent to i"),col=c("black","red","blue","grey60"),pch=c(19,22,24,25),pt.bg=c("black","red","blue","grey60"),lwd=2,bty="n")
  dev.off()

