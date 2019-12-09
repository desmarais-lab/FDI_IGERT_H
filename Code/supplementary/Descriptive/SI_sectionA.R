# clear workspace
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load packages
library(magic)
library(network)
library(igraph)
library(doBy)
library(plyr)
library(DataCombine)



#load in data
fdi <- read.csv("sub_stock.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]

# create new variable transformations
fdi$trade_ln <- log(fdi$trade_int+fdi$trade_hco+fdi$trade_cap+fdi$trade_mix+1) 
fdi$dyad <- paste(fdi$Destination, fdi$Origin, sep = "")
fdi$mass <- (log(fdi$Dest.GDP*fdi$Origin.GDP))
fdi$dist_ln <- log(fdi$dist)
fdi$Value_ln <- round(log(ifelse(fdi$Value<0, 0, fdi$Value)+1)) 
fdi$Dest.pop_ln <- log(fdi$Dest.pop)
fdi$Origin.pop_ln <- log(fdi$Origin.pop)

#lag DV and GDP (to create growth rates)
fdi <- slide(fdi, Var = "Dest.GDP", GroupVar = "dyad", slideBy = -1)
fdi <- slide(fdi, Var = "Origin.GDP", GroupVar = "dyad", slideBy = -1)
fdi <- slide(fdi, Var = "Value_ln", GroupVar = "dyad", slideBy = -1)

#Create GDP pc (logged) and growth rate
fdi$Origin.GDPpc_ln <- log(fdi$Origin.GDP/fdi$Origin.pop)
fdi$Dest.GDPpc_ln <- log(fdi$Dest.GDP/fdi$Dest.pop)
fdi$Dest.GDPpc <- fdi$Dest.GDP/fdi$Dest.pop
fdi$Origin.GDPg <- (fdi$Origin.GDP-fdi$`Origin.GDP-1`)/fdi$`Origin.GDP-1`
fdi$Dest.GDPg <- (fdi$Dest.GDP-fdi$`Dest.GDP-1`)/fdi$`Dest.GDP-1`


# create alliance dummy
fdi$alliance <- (fdi$nonaggression.max.x + fdi$entente.max.x + fdi$neutrality.max.x)
fdi$alliance <- as.numeric(ifelse(fdi$alliance >0, 1, 0))
#make bit  and defense numeric
fdi$bit_dummy <- as.numeric(fdi$bit_dummy)
fdi$defense.max.x <- as.numeric(fdi$defense.max.x)

#drop 2001 with missing variables
fdi <- na.omit(fdi)

#write csv
#write.csv(fdi, file = "sub_stock.csv")



#plot dependent variable distribution
fdi$Value_h <- ifelse(fdi$Value < 0, 0, fdi$Value)
fdi_col <- subset(fdi, fdi$Value_ln == 0)
fdi_h <- subset(fdi, fdi$Value_ln != 0)
fdi_col$Value  <- 1
fdi_col <- summaryBy(Value~Year, data = fdi_col, FUN=sum)
fdi_col$prop <- fdi_col$Value.sum/15500
#hist_1 <- hist(fdi$Value_h)
#hist_1$counts <- hist_1$counts/12
#plot(hist_1+1, col="lightgray", main = "Distribution of FDI Flows", ylim = c(0,600),
#     xlab="Value, logged (Excludes Zero Values, ~85% of obs.)", ylab = "Per Year")
par(mar = c(5,5,5,5))
boxplot(fdi_h$Value_h~fdi_h$Year, log = "y",col="lightgray", main = "Non-Zero FDI Stocks",yaxt='n',
        xlab="Year", ylab = "FDI Stock (in mil USD)",outline=TRUE, cex=0.5, pch = 16, par(las=1))
axis(2,at=c(1, 100, 10000),labels=c("1","100","10K"))
#mtext("Removed Zeroes from Box-Plots")
par(new = T)
plot(fdi_col$prop, axes=F, xlab=NA, ylab=NA, type="l", col = "#D55E00", pch = 16)
axis(side = 4, col = "#D55E00", col.axis ="#D55E00",col.ticks="#D55E00")
par(las=0)
mtext(side = 4, line = 3, 'Proportion of Zeroes', col ="#D55E00")

rm(fdi_col, fdi_h)





####### Descriptive Stats ##################################################################

# Bar charts giving proportion 1 by year for dichotomous variables


dichot <- summaryBy(bit_dummy + alliance + defense.max.x~Year, data = fdi, FUN=sum)
dichot[,2:4] <- dichot[,2:4]/(length(fdi$Year)/length(unique(fdi$Year)))
# BIT
mp <- barplot(dichot$bit_dummy.sum,  main="Proportion of BIT dyads",
              col="lightgray",axes = FALSE,xlab="Year")
axis(2,at=seq(0, .1, .02),labels=c("0%","2%","4%", "6%", "8%", "10%"))
axis(1,at=mp,labels=dichot$Year)
# Alliance
mp <- barplot(dichot$alliance.sum,  main="Proportion of Alliance Dyads",
              col="lightgray",axes=FALSE,xlab="Year", ylim = c(0,.1))
axis(2,at=seq(0, .1, .02),labels=c("0%","2%","4%", "6%", "8%", "10%"))
axis(1,at=mp,labels=dichot$Year)
#Defense Treaty
mp <- barplot(dichot$defense.max.x.sum,  main="Proportion of Defense Treaty Dyads",
              col="lightgray",axes = FALSE,xlab="Year", ylim = c(0,.1))
axis(2,at=seq(0, .1, .02),labels=c("0%","2%","4%", "6%", "8%", "10%"))
axis(1,at=mp,labels=dichot$Year)

rm(dichot, mp)
# Yearly box-plots for quantitative variables

#Polity
boxplot(fdi$Dest.polity~fdi$Year, col="lightgray", main = "Polity",#yaxt='n',
        xlab="Year", ylab = "Polity Scale", cex=0.5, pch = 16)
#Trade_openness
boxplot(fdi$Dest.TO~fdi$Year, col="lightgray", main = "Trade Openness", yaxt='n',
        xlab="Year", ylab = "IM/EX as % of GDP ", cex=0.5, pch = 16)
axis(2,at=seq(0, 400, 100),labels=c("0%","100%","200%", "300%", "400%"))
#Population
boxplot(fdi$Dest.pop~fdi$Year, log = "y",col="lightgray", main = "Population", yaxt='n',
        xlab="Year", ylab = "Population in Millions", cex=0.5, pch = 16)
axis(2,at=c(0, 1, 100, 1000),labels=c("0","1","100","1K"))
#gdp.pc
boxplot(fdi$Dest.GDPpc~fdi$Year, col="lightgray", main = "GDP pc", yaxt='n',
        xlab="Year", ylab = "GDP per capita", cex=0.5, pch = 16)
axis(2,at=c(0, 50000, 100000, 150000),labels=c("0","50K","100K","150K"))
#Mass
boxplot(fdi$mass~fdi$Year, col="lightgray", main = "Mass", #yaxt='n',
        xlab="Year", ylab = "GDP Product of Dyad, Logged", cex=0.5, pch = 16)
axis(2,at=c(0, 50000, 100000, 150000),labels=c("0","50K","100K","150K"))
#Distance
boxplot(fdi$dist~fdi$Year, col="lightgray", main = "Distance", yaxt='n',
        xlab="Year", ylab = "Distance, Miles", cex=0.5, pch = 16)
axis(2,at=c(0, 5000, 10000, 15000, 20000),labels=c("0","5K","10K","15K", "20K"))
#Trade
boxplot(fdi$trade_ln~fdi$Year, col="lightgray", main = "Trade", #yaxt='n',
        xlab="Year", ylab = "Value of Trade USD in mil, logged", cex=0.5, pch = 16)
axis(2,at=c(0, 50000, 100000, 150000),labels=c("0","50K","100K","150K"))


#clean_stock.csv has world totals