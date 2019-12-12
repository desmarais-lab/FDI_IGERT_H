# clear workspace, set seed, set wd
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#load packages
library(Amelia)
library(DataCombine)


#load in data
fdi <- read.csv("zero_panel_stock.csv", stringsAsFactors=FALSE)
fdi <- fdi[,-1]


# fill in treaty data
fdi$defense.max.x <- ifelse(is.na(fdi$defense.max.x), 0, fdi$defense.max.x)
fdi$neutrality.max.x <- ifelse(is.na(fdi$neutrality.max.x), 0, fdi$neutrality.max.x)
fdi$nonaggression.max.x <- ifelse(is.na(fdi$nonaggression.max.x), 0, fdi$nonaggression.max.x)
fdi$entente.max.x <- ifelse(is.na(fdi$entente.max.x), 0, fdi$entente.max.x)


#turn char into numeric
fdi$distw <- as.numeric(fdi$distw)
fdi$distwces <- as.numeric(fdi$distwces)
fdi$Dest.TO <- as.numeric(fdi$Dest.TO)
fdi$Origin.TO <- as.numeric(fdi$Origin.TO)


#create variables, change variables for model
fdi$mass <- fdi$Origin.GDP * fdi$Dest.GDP
fdi$alliance <- fdi$neutrality.max.x + fdi$nonaggression.max.x + 
                fdi$entente.max.x
fdi$alliance <- ifelse(fdi$alliance>0, 1, 0)
fdi$defense.max.x <- ifelse(fdi$defense.max.x>0, 1, 0) 
fdi$Dest.GDPpc <- fdi$Dest.GDP/fdi$Dest.pop
fdi$Origin.GDPpc <- fdi$Origin.GDP/fdi$Origin.pop
fdi$dyadic_trade <- fdi$trade_cap+fdi$trade_hco+fdi$trade_int+fdi$trade_mix


fdi<-fdi[,c(1,2,3,8,9,18,28,32:35,46:51)]

#drop based on missing covariates
fdi_drop <- na.omit(fdi[,c(1:4,6:17)])
a <- unique(fdi_drop$Origin)
fdi <- fdi[fdi$Origin %in% a,]
fdi <- fdi[fdi$Destination %in% a,]
rm(fdi_drop,a)






#   check missingness
#missmap(fdi)


#drop missing values by variables
df_na <- subset(fdi, is.na(fdi$Dest.polity) | is.na(fdi$Dest.TO))
df_na <- (unique(df_na$Destination))
'%!in%' <- function(x,y)!('%in%'(x,y))
fdi <- subset(fdi, fdi$Origin %!in% df_na)
fdi <- subset(fdi, fdi$Destination %!in% df_na)



#fdi_a$alliance <- fdi_a$alliance + fdi_a$defense.max.x
#fdi_a$alliance <- ifelse(fdi_a$alliance>0, 1, 0)
#fdi_a <- fdi_a[,-7]
#add a lag
#fdi_a <- slide(fdi_a, Var = "Value", GroupVar = "dyadid", slideBy = -1)



########## Variable Transformations ##############

#log
fdi$dist <- log(fdi$dist)
fdi$mass <- log(fdi$mass)
fdi$trade_ln <- log(fdi$dyadic_trade+1)
fdi$Origin.GDPpc <- log(fdi$Origin.GDPpc)
fdi$Dest.GDPpc <- log(fdi$Dest.GDPpc)

#function that scales variables
range_0to1 <- function(x){(x-min(x))/(max(x)-min(x))}
#scale continuous variables
for(i in 6:18){
  
  fdi[,i] <- range_0to1(fdi[,i]) 
  
}

# log and round DV, create lag
fdi$Value <- ifelse(fdi$Value<0, 0, fdi$Value)
fdi$Value_ln <- log(fdi$Value+1)
fdi$Value_ln <- round(fdi$Value_ln, digits = 0)
fdi <- slide(fdi, Var = "Value_ln", GroupVar = "dyadid", slideBy = -1)

#write out copy for Amelia 
fdi_a <- fdi
fdi_a <- fdi_a[,-5]


# add network variable for Amelia imputations





#write csv with zero imputations
fdi$Value_ln <- ifelse(is.na(fdi$Value_ln), 0, fdi$Value_ln)
fdi$`Value_ln-1` <- ifelse(is.na(fdi$`Value_ln-1`), 0, fdi$`Value_ln-1`)

write.csv(fdi, file = "zero_stock.csv")


############# Amelia ################


#set id variables for ameilia
ids <-  c("Origin","Destination")

#run imputations
amelia_fit <- amelia(fdi_a, m =2, idvars =ids,
              ts = "Year", cs = c("dyadid"), polytime = 1)



############# Amelia vs Zero ################




fdi_a1 <- amelia_fit$imputations[[1]]
fdi_a1$Value_ln <- ifelse(fdi_a1$Value_ln<0, 0, fdi_a1$Value_ln)
fdi_a1$Value_ln  <- round(fdi_a1$Value_ln, digits = 0)


d1 <- density(rnorm(fdi$Value_ln))
d2 <- density(rnorm(fdi_a1$Value_ln))
plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", xlab = "FDI",
     ylab = "Density", main ="Amelia vs. Just Zeros")
lines(d1, col = "red")
lines(d2, col = "blue")
legend("topright", inset=.05, title="Method", c("Zero", "Amelia"),
        lty=c(1,1),lwd=c(2.5,2.5),col=c("red","blue")) 


summary(fdi$Value_ln)
summary(fdi_a1$Value_ln)




 

# amelia for different levels of q

# q = .25
q25 <- read.csv("q_25.csv", stringsAsFactors=FALSE)
fdi_q25 <- fdi[fdi$Origin %in% q25[,2],]
fdi_q25 <- fdi_q25[fdi_q25$Destination %in% q25[,2],]
fdi_q25a <- fdi_a
fdi_q25a <- fdi_a[fdi_a$Origin %in% q25[,2],]
fdi_q25a <- fdi_q25[fdi_q25$Destination %in% q25[,2],]


#run imputations
amelia_fit25 <- amelia(fdi_q25a, m =2, idvars =ids,
                     ts = "Year", cs = c("dyadid"), polytime = 1)

fdi_q25a1 <- amelia_fit25$imputations[[1]]
fdi_q25a1$Value_ln <- ifelse(fdi_q25a1$Value_ln<0, 0, fdi_q25a1$Value_ln)
fdi_q25a1$Value_ln  <- round(fdi_q25a1$Value_ln, digits = 0)



summary(fdi_q25$Value_ln)
summary(fdi_q25a1$Value_ln)

d1 <- density(rnorm(fdi_q25$Value_ln))
d2 <- density(rnorm(fdi_q25a1$Value_ln))
plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", xlab = "FDI",
     ylab = "Density", main ="Amelia vs. Just Zeros, q=.25")
lines(d1, col = "red")
lines(d2, col = "blue")
legend("topright", inset=.05, title="Method", c("Zero", "Amelia"),
       lty=c(1,1),lwd=c(2.5,2.5),col=c("red","blue")) 









#q = .5
q50 <- read.csv("q_50.csv", stringsAsFactors=FALSE)
fdi_q50 <- fdi[fdi$Origin %in% q50[,2],]
fdi_q50 <- fdi_q50[fdi_q50$Destination %in% q50[,2],]
fdi_q50a <- fdi_q50
fdi_q50$Value <- ifelse(is.na(fdi_q50$Value), 0, fdi_q50$Value)
fdi_q50a <- slide(fdi_q50a, Var = "Value", GroupVar = "dyadid", slideBy = -1)

#run imputations
amelia_fit50 <- amelia(fdi_q50a, m =2, idvars =ids,
                       ts = "Year", cs = c("dyadid"), polytime = 1)

fdi_q50a1 <- amelia_fit50$imputations[[1]]

# check density after converting to negative values to zero, logging, and rounding
fdi_q50$Value <- ifelse(fdi_q50$Value<0, 0, fdi_q50$Value)
fdi_q50$Value_ln <- log(fdi_q50$Value+1)
fdi_q50$Value_ln <- round(fdi_q50$Value_ln, digits = 0)
fdi_q50a1$Value <- ifelse(fdi_q50a1$Value<0, 0, fdi_q50a1$Value)
fdi_q50a1$Value_ln <- log(fdi_q50a1$Value+1)
fdi_q50a1$Value_ln <- round(fdi_q50a1$Value_ln)


summary(fdi_q50$Value_ln)
summary(fdi_q50a1$Value_ln)

d1 <- density(rnorm(fdi_q50$Value_ln))
d2 <- density(rnorm(fdi_q50a1$Value_ln))
plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", xlab = "FDI",
     ylab = "Density", main ="Amelia vs. Just Zeros, q=.50")
lines(d1, col = "red")
lines(d2, col = "blue")
legend("topright", inset=.05, title="Method", c("Zero", "Amelia"),
       lty=c(1,1),lwd=c(2.5,2.5),col=c("red","blue")) 

