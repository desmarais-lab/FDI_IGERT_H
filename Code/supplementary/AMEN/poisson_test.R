# clear workspace, set seed, and set wd
rm(list=ls())

set.seed(19)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


### Read in data
load("../../main/main_cov.Rdata")
load("../../main/main_net.Rdata")

library(amen)
library(statnet)
library(coda) 

y = 6
net <- netlist[[y]] 
n_nodes <- length(net[[3]])
node_attr <- matrix(nrow=n_nodes, ncol= 3) #create matrix of node variables
node_attr[,1] <- get.node.attr(net, attrname ="polity")
node_attr[,2] <- get.node.attr(net, attrname ="trade_opennes")
node_attr[,3] <- get.node.attr(net, attrname ="gdp.pc")
colnames(node_attr) <-c("polity","trade_openness", "gdp.pc")
edge_vals <- net %e% "Value"
net %e% "tValue" <- ceiling(log(edge_vals)*2)
Y <- as.sociomatrix(net, attrname = "tValue") # get adjacency matrix for DV
Xd <- sapply(covlist[[y]], identity, simplify="array")


#Y<-sheep$dom 
#age<-sheep$age 
#x<-sheep$age - mean(sheep$age)

X<-design_array(Xrow=node_attr,Xcol=node_attr,Xdyad=Xd) 

#### Starting values
Z<-log(Y+1) ; diag(Z)<-0

Sab<-diag(2)

R<-1 ; Suv<-diag(2*R) ; U<-V<-matrix(0,nrow(Y),R) 

s2<-1 ; rho<-0

#### Parameter values to be saved 
BETA<-VE<-LL<-NULL
ACC<-matrix(0,nrow(Y),nrow(Y))

#### MCMC 
set.seed(1)
for(s in 1:10000)
{ 
  ## Update AMEN parameters
  
  # update beta, a and b
  tmp<-rbeta_ab_fc(Z,Sab,rho,X,s2,offset=U%*%t(V))
  beta<-tmp$beta ; a<-tmp$a ; b<-tmp$b
  
  # update UV
  tmp<-rUV_fc(Z,U,V,Suv,rho,offset=Xbeta(X,beta)+outer(a,b,"+"))
  U<-tmp$U ; V<-tmp$V
  
  # update Sab
  Sab<-rSab_fc(a,b)
  
  # update Suv
  Suv<-rSuv_fc(U,V)
  
  # update s2
  s2<-rs2_fc(Z,rho,offset=Xbeta(X,beta)+outer(a,b,"+")+U%*%t(V))
  
  # update rho
  rho<-rrho_mh(Z,rho,s2,offset=Xbeta(X,beta)+outer(a,b,"+")+U%*%t(V))
  
  
  
  ## Update Z 
  
  # propose candidate Z 
  Zp<-Z+matrix(rnorm(nrow(Y)^2),nrow(Y),nrow(Y))*sqrt(s2)   

  # compute acceptance ratio 
  EZ<-Xbeta(X,beta)+outer(a,b,"+")+U%*%t(V)
  lr<-ldZgbme(Zp,Y,function(y,z){ dpois(y,exp(z),log=TRUE) },EZ,rho,s2) -
      ldZgbme(Z, Y,function(y,z){ dpois(y,exp(z),log=TRUE) },EZ,rho,s2) 

  # simulate symmetric matrix of (log) uniform rvs
  lh<-matrix(log(runif(nrow(Y)^2)),nrow(Y),nrow(Y))
  lh[lower.tri(lh)]<-t(lh)[lower.tri(lh)]  

  # update dyads for which lr>lh, and keep track of acceptances
  Z[lr>lh]<-Zp[lr>lh] 
  ACC[lr>lh]<-ACC[lr>lh]+1  



  ## Output
  if(s%%25==0) 
  {  
    cat(s,range(ACC/s),"\n") 
    BETA<-rbind(BETA,beta) 
    VE<-rbind(VE,c(s2,rho))    

    par(mfrow=c(1,3),mar=c(3,3,1,1),mgp=c(1.75,.75,0)) 
    matplot(BETA,type="l") 
    LL<-c(LL,sum(dpois(Y,exp(Z),log=TRUE),na.rm=TRUE)) ; plot(LL,type="l")
    hist(ACC/s) 
  }
}


geweke.diag(BETA)



#var1    var2    var3    var4    var5    var6    var7    var8    var9   var10   var11   var12   var13   var14   var15   var16 
#7.0792 -1.0478 -3.9507 -0.3528 -9.6689  1.2392 -2.2643 -1.1986 -3.5923  3.1300  2.6362 -2.1214 -3.3722 -1.6459 -0.2213  6.5602 

