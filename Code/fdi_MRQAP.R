#=============================================================#
# John Schoeneman
# Work Done For: FDI Network Analysis RA-IGERT
# Date: Fall 2016
# Work Done: Network Measures and MRQAP
# Machine: MacPro OSX Yosemite
#=============================================================#


# clear workspace
rm(list=ls())

set.seed(19)

# libraries
library(GERGM)
library(network)
library(igraph)
library(doBy)
library(foreach)
library(doMC)
library(stargazer)


no_cores <- detectCores()
registerDoMC(no_cores)  #change the 2 to your number of CPU cores  

#setwd("/Users/johnpschoeneman/Desktop/ACI")

#load in data
fdi <- read.csv("fdi_sub.csv", stringsAsFactors=FALSE)        #FDI

fdi <- fdi[,-1]

fdi$trade_int_sq = fdi$trade_int^2

######  MRQAP   #######################################################################
#

fdi <- fdi[c("Destination", "Origin","Year", "Value", "contig","comlang_off", "comlang_ethno","colony",
             "comcol", "curcol","dist","Dest.GDP","Origin.GDP","defense.max.x","nonaggression.max.x",
             "neutrality.max.x","entente.max.x","Dest.polity","Origin.polity", "Dest.TO", "Dest.GDP.g",
             "Origin.TO", "Origin.GDP.g", "Origin.pop", "Dest.pv", "Origin.pv", "depth_index", "depth_latent",
             "trade_hco", "trade_int", "trade_int_sq","trade_cap", "trade_mix")]

#"depth_latent",
xvars<-c("contig","comlang_off", "comlang_ethno","colony",
         "comcol", "curcol","dist","Dest.GDP","Origin.GDP","defense.max.x","nonaggression.max.x",
         "neutrality.max.x","entente.max.x","Dest.polity","Origin.polity", "Dest.TO", "Dest.GDP.g",
         "Origin.TO", "Origin.GDP.g", "Origin.pop", "Dest.pv", "Origin.pv", "depth_latent", 
         "trade_hco", "trade_int", "trade_int_sq", "trade_cap", "trade_mix", "Destination", "Origin", "as.factor(Year)")
MODEL<-as.formula(paste(paste("Value"," ~ ", paste(xvars,collapse="+"))))

for(i in 3:32){
fdi[,i] <- as.numeric(fdi[,i])

}
fdi2 <- subset(fdi, is.na(fdi[,12]))

# FE 
fe <- lm(MODEL, data=fdi)
summary(fe)
 
stargazer(fe)

# create Y Permuter for Data
MODEL<-as.formula(paste(paste("dv[,1]"," ~ ", paste(xvars,collapse="+"))))


#im.train   <- foreach(im = d.train$Image, .combine=rbind) %dopar% {
#  as.integer(unlist(strsplit(im, " ")))
results <- matrix(nrow = 1000, ncol= 84)
# Start the clock!
ptm <- proc.time()
for(a in 1:2) {
 

  #permute Y 
  Y <-   foreach(i=2001:2012, .combine=rbind) %dopar% {

    Y_year <- subset(fdi, fdi$Year==i)
    Y_ig <- graph.data.frame(Y_year)
    Y_adj <- get.adjacency(Y_ig,attr='Value', sparse=FALSE)
    n <- nrow(Y_adj)
    
    order <- sample(1:n)
    Y_re <- Y_adj[order,order]+1
    
    g  <- graph.adjacency(Y_re,weighted=TRUE, diag=FALSE)
    df <- get.data.frame(g)
    df$weight <- df$weight-1
    
    
    #rbind(Y, df)
   
  }

  Y <- t(Y)
  dv <- data.frame(Y[,1])
  names(dv)[1] <- "dv1"
  for(j in 2:12){
    dv_r <-data.frame(Y[,j])
    names(dv_r)[1] <- "dv1"
    dv <- rbind(dv, dv_r)
  }
  
  
  
  
  linear.1 <- lm(MODEL, data=fdi)
  
  p1 <- 0
  p2 <- 28
  p3 <- 56
  
  for(covar in 1:28){
    p1 <- p1 + 1
    p2 <- p2 + 1
    p3 <- p3 + 1
    #subtract base model
    base <- coef(summary(ols))[covar] - coef(summary(linear.1))[covar]
    # count for p-values greater than 0
    results[a,p1] <- ifelse(base>0,1,0)
    # count for p-values less than 0
    results[a,p2] <- ifelse(base<0,1,0)
    # count for p-values greater than 0
    results[a,p3] <- ifelse(abs(base)>0,1,0)
    
  }

}
# Stop the clock
proc.time() - ptm


 
# write out as datafile
#write.csv(results, file="mrqap_results.csv")

df <- fdi
# model
mod <- lm(Value ~ trade_int, data = df)

# predicts + interval
newx <- seq(min(df$trade_int), max(df$trade_int), length.out=100)
preds <- predict(mod, newdata = data.frame(trade_int=newx), 
                 interval = 'confidence')


# plot
plot(Value ~ trade_int, data = df, type = 'n')
# add fill
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
# model
abline(mod)
# intervals
lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
lines(newx, preds[ ,2], lty = 'dashed', col = 'red')




#Scale x variables
fdi$trade_int_sc <- fdi$trade_int/max(fdi$trade_int)
fdi$depth_laten_sc <- fdi$depth_latent/max(fdi$depth_latent)
fdi$trade_cap_sc <- fdi$trade_cap/max(fdi$trade_cap)


# make plot
par(mfrow=c(1,1))
plot(x = NULL, y= NULL, xlim=c(0.0, 1.0), ylim=c(0, 8000),
     main = "Linear fit between FDI and PTAs/Trade", xlab = "Scaled Covariates",
     ylab = "FDI (USD, Millions)")
abline(lm(fdi$Value ~ fdi$trade_int_sc), lty = 2)
abline(lm(fdi$Value ~ fdi$depth_laten_sc), lty=3)

legend(2.4, 1, c("Ind", "Ex", "Unst", "True"),bty = "n",
       lty = c(1, 2, 3,1), col= c("black", "black", "black", "red"), merge = TRUE)


c <- ggplot(mtcars, aes(y=wt, x=mpg, colour=factor(cyl)))
c + stat_smooth(method=lm) + geom_point()
c + stat_smooth(method=lm, aes(fill = factor(cyl)))


library(ggplot2)
ggplot() +
  stat_smooth(aes(trade_int_sc, Value, colour='Trade In Intermediate Goods'), fdi) +  
  stat_smooth(aes(depth_laten_sc, Value, colour='PTA Depth'), fdi) + 
  labs(title = "LOWESS Fit",  x = "Scaled Covariates", y ="FDI (USD, Millions)",colour = "Variables")

ggplot() +
  stat_smooth(aes(trade_cap_sc, Value, colour='Trade In Intermediate Goods'), fdi) +  
  stat_smooth(aes(depth_laten_sc, Value, colour='PTA Depth'), fdi) + 
  labs(title = "LOWESS Fit",  x = "Scaled Covariates", y ="FDI (USD, Millions)",colour = "Variables")  





qplot(fdi$trade_int_sc, fdi$Value, geom=c("smooth"))
