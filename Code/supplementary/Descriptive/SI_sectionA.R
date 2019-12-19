# clear workspace
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load packages
library(doBy)
library(ggplot2)
library(xtable)
#load in data
fdi <- read.csv("../../main/subset_stock2.csv", stringsAsFactors=FALSE) #FDI
fdi <- fdi[,-1]

########### Correlation Matrix #################
source("correlation_function.R")
var_list <- c(6:8, 10,12:16,19,25)
corstars(fdi[,var_list], method=c("pearson"), removeTriangle=c("upper"),result=c("latex"))
#p < .0001 ‘****’; p < .001 ‘***’, p < .01 ‘**’, p < .05 ‘*’
########### Distribution Plots #################

#plot dependent variable distribution
fdi$Value_h <- ifelse(fdi$Value < 0, 0, fdi$Value)
fdi_col <- subset(fdi, fdi$Value_ln == 0)
fdi_h <- subset(fdi, fdi$Value_ln != 0)
fdi_col$Value  <- 1
n <- length(unique(fdi$Origin))
fdi_col <- summaryBy(Value~Year, data = fdi_col, FUN=sum)
fdi_col$prop <- fdi_col$Value.sum/(n*(n-1))
#hist_1 <- hist(fdi$Value_h)
#hist_1$counts <- hist_1$counts/12
#plot(hist_1+1, col="lightgray", main = "Distribution of FDI Flows", ylim = c(0,600),
#     xlab="Value, logged (Excludes Zero Values, ~85% of obs.)", ylab = "Per Year")
pdf("descriptive_plots/fdi_stock_zeroes_boxplot.pdf",height=4.25,width=5.5,family="Times")
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
dev.off()
rm(fdi_col, fdi_h)


####### Descriptive Stats ##################################################################

# Bar charts giving proportion 1 by year for dichotomous variables

dichot <- summaryBy(bit_dummy + alliance + defense.max.x + both_oecd~Year, data = fdi, FUN=sum)
dichot[,2:5] <- dichot[,2:5]/(length(fdi$Year)/length(unique(fdi$Year)))
# BIT
pdf("descriptive_plots/BIT_barplot.pdf",height=4.25,width=5.5,family="Times")
mp <-barplot(dichot$bit_dummy.sum,  main="Proportion of BIT dyads",
              col="lightgray",axes = FALSE,xlab="Year")
axis(2,at=seq(0, .1, .02),labels=c("0%","2%","4%", "6%", "8%", "10%"))
axis(1,at=mp,labels=dichot$Year)
dev.off()
# Alliance
pdf("descriptive_plots/alliance_barplot.pdf",height=4.25,width=5.5,family="Times")
mp <-barplot(dichot$alliance.sum,  main="Proportion of Alliance Dyads",
              col="lightgray",axes=FALSE,xlab="Year", ylim = c(0,.1))
axis(2,at=seq(0, .1, .02),labels=c("0%","2%","4%", "6%", "8%", "10%"))
axis(1,at=mp,labels=dichot$Year)
dev.off()
#Defense Treaty
pdf("descriptive_plots/defense_barplot.pdf",height=4.25,width=5.5,family="Times")
mp <-barplot(dichot$defense.max.x.sum,  main="Proportion of Defense Treaty Dyads",
              col="lightgray",axes = FALSE,xlab="Year", ylim = c(0,.1))
axis(2,at=seq(0, .1, .02),labels=c("0%","2%","4%", "6%", "8%", "10%"))
axis(1,at=mp,labels=dichot$Year)
dev.off()
#OECD pairs
pdf("descriptive_plots/OECDpair_barplot.pdf",height=4.25,width=5.5,family="Times")
mp <-barplot(dichot$both_oecd.sum,  main="Proportion of Defense Treaty Dyads",
        col="lightgray",axes = FALSE,xlab="Year", ylim = c(0,.1))
axis(2,at=seq(0, .1, .02),labels=c("0%","2%","4%", "6%", "8%", "10%"))
axis(1,at=mp,labels=dichot$Year)
dev.off()

rm(dichot,mp)
# Yearly box-plots for quantitative variables

#Polity
pdf("descriptive_plots/polity_boxplot.pdf",height=4.25,width=5.5,family="Times")
boxplot(fdi$Dest.polity~fdi$Year, col="lightgray", main = "Polity",#yaxt='n',
        xlab="Year", ylab = "Polity Scale", cex=0.5, pch = 16)
dev.off()
#Trade_openness
pdf("descriptive_plots/TO_boxplot.pdf",height=4.25,width=5.5,family="Times")
boxplot(fdi$Dest.TO~fdi$Year, col="lightgray", main = "Trade Openness", yaxt='n',
        xlab="Year", ylab = "IM/EX as % of GDP ", cex=0.5, pch = 16)
axis(2,at=seq(0, 400, 100),labels=c("0%","100%","200%", "300%", "400%"))
dev.off()
#gdp.pc
pdf("descriptive_plots/GDPpc_boxplot.pdf",height=4.25,width=5.5,family="Times")
boxplot(fdi$Dest.GDPpc~fdi$Year, col="lightgray", main = "GDP pc", yaxt='n',
        xlab="Year", ylab = "GDP per capita in USD, logged", cex=0.5, pch = 16)
axis(2,at=seq(0,12,2),labels=c("0","2","4","6","8","10","12"))
dev.off()
#Mass
pdf("descriptive_plots/Mass_boxplot.pdf",height=4.25,width=5.5,family="Times")
boxplot(fdi$mass~fdi$Year, col="lightgray", main = "Mass", #yaxt='n',
        xlab="Year", ylab = "GDP Product of Dyad, logged", cex=0.5, pch = 16)
axis(2,at=c(0, 50000, 100000, 150000),labels=c("0","50K","100K","150K"))
dev.off()
#Distance
pdf("descriptive_plots/dist_boxplot.pdf",height=4.25,width=5.5,family="Times")
boxplot(fdi$dist~fdi$Year, col="lightgray", main = "Distance", yaxt='n',
        xlab="Year", ylab = "Distance in Miles, logged", cex=0.5, pch = 16)
axis(2,at=c(0, 2.5, 5, 7.5, 10),labels=c("0","2.5","5","7.5", "10"))
dev.off()
#Trade
pdf("descriptive_plots/Trade_boxplot.pdf",height=4.25,width=5.5,family="Times")
boxplot(fdi$trade_ln~fdi$Year, col="lightgray", main = "Bilateral Trade Volumne", #yaxt='n',
        xlab="Year", ylab = "Value of Trade USD in mil, logged", cex=0.5, pch = 16)
axis(2,at=c(0, 50000, 100000, 150000),labels=c("0","50K","100K","150K"))
dev.off()
#PTA Depth
pdf("descriptive_plots/ptadepth_boxplot.pdf",height=4.25,width=5.5,family="Times")
boxplot(fdi$trade_ln~fdi$Year, col="lightgray", main = "PTA Depth", #yaxt='n',
        xlab="Year", ylab = "Latent Depth of PTA", cex=0.5, pch = 16)
axis(2,at=c(0, 50000, 100000, 150000),labels=c("0","50K","100K","150K"))
dev.off()

#check summed world totals

fdi2 <- read.csv("../../create_datasets/clean_stock.csv", stringsAsFactors=FALSE) #FDI
df1 <- fdi[,c(1:3,5)]
df1$Value <- ifelse(is.na(df1$Value),0, df1$Value)
df1 <- summaryBy(Value ~ Destination +Year,data=df1, FUN = sum, keep.names = T)
df1$Source <- "Sum"
df1 <- df1[,c(1,4,2,3)]
df2 <- subset(fdi2, fdi2$Origin=="World")[,c(2,4,6,7)]
df2$Value <- ifelse(is.na(df2$Value),0, df2$Value)
colnames(df2)[2] <- "Source"

df1 <- subset(df1, df1$Destination %in% df2$Destination)
df2 <- subset(df2, df2$Destination %in% df1$Destination)

df <- rbind(df1, df2)


name <- ggplot(df ,aes(x=(Value), fill=Source)) + 
  geom_density(alpha=0.25)+ 
  scale_fill_manual(values=c("red","blue"),name = "Group")+
  ggtitle(paste0("Distribution of World vs.Partner Sums"))+
  #geom_vline(xintercept=max(d$donation.sum)/1000, colour="red") +
  #geom_vline(xintercept=max(r$donation.sum)/1000, colour="blue") +
  #geom_text(aes(x=max(d$donation.sum)/1000, label="Max Contribution Total, BE", y=0.05), 
  #          colour="black", angle=90, vjust = 1.2, family="URWTimes")+
  #geom_text(aes(x=max(r$donation.sum)/1000, label="Max Contribution Total, Random", y=0.05), 
  #          colour="black", angle=90, vjust = 1.2, family="URWTimes")+
  ylab("Density")+ xlab("FDI Value, USD")+
ggsave(paste("descriptive_plots/check_sums.pdf",sep=""), name, device="pdf", width=6, height=4)


#check summed world totals to imputed totals
load("../Impute_Missing_Values/amelia_fit.Rdata") 


df_a <- amelia_fit$imputations[[1]]
for(i in 2:10){
  df_a$Value <- df_a$Value+amelia_fit$imputations[[i]]$Value
}
df_a$Value <- df_a$Value/10
fdi2 <- read.csv("../../create_datasets/clean_stock.csv", stringsAsFactors=FALSE) #FDI




df1 <- df_a[,c(1:3,5)]
df1$Value <- ifelse(df1$Value<0, 0,df1$Value)
df1 <- summaryBy(Value ~ Destination +Year,data=df1, FUN = sum, keep.names = T)
df1$Source <- "Imputed_Sums"
df1 <- df1[,c(1,4,2,3)]
df2 <- subset(fdi2, fdi2$Origin=="World")[,c(2,4,6,7)]
df2 <- na.omit(df2)
colnames(df2)[2] <- "Source"
df1$CY <- paste0(df1$Destination,df1$Year)
df2$CY <- paste0(df2$Destination,df2$Year)

df1 <- subset(df1, df1$CY %in% df2$CY)
df2 <- subset(df2, df2$CY %in% df1$CY)

df <- rbind(df1, df2)


name <- ggplot(df ,aes(x=(Value), fill=Source)) + 
  geom_density(alpha=0.25)+ 
  scale_fill_manual(values=c("red","blue"),name = "Group")+
  ggtitle(paste0("Distribution of World vs. MI Partner Sums"))+
  #geom_vline(xintercept=max(d$donation.sum)/1000, colour="red") +
  #geom_vline(xintercept=max(r$donation.sum)/1000, colour="blue") +
  #geom_text(aes(x=max(d$donation.sum)/1000, label="Max Contribution Total, BE", y=0.05), 
  #          colour="black", angle=90, vjust = 1.2, family="URWTimes")+
  #geom_text(aes(x=max(r$donation.sum)/1000, label="Max Contribution Total, Random", y=0.05), 
  #          colour="black", angle=90, vjust = 1.2, family="URWTimes")+
  ylab("Density")+ xlab("FDI Value, USD")
  ggsave(paste("descriptive_plots/check_MIsums.pdf",sep=""), name, device="pdf", width=6, height=4)


df$Value <- ceiling(log(df$Value)*2)
  
  
name <- ggplot(df ,aes(x=(Value), fill=Source)) + 
    geom_density(alpha =.25,adjust = 3)+ 
    scale_fill_manual(values=c("red","blue"),name = "Group")+
    ggtitle(paste0("Distribution of Reported Total vs. MI Partner Sums"))+
    #geom_vline(xintercept=max(d$donation.sum)/1000, colour="red") +
    #geom_vline(xintercept=max(r$donation.sum)/1000, colour="blue") +
    #geom_text(aes(x=max(d$donation.sum)/1000, label="Max Contribution Total, BE", y=0.05), 
    #          colour="black", angle=90, vjust = 1.2, family="URWTimes")+
    #geom_text(aes(x=max(r$donation.sum)/1000, label="Max Contribution Total, Random", y=0.05), 
    #          colour="black", angle=90, vjust = 1.2, family="URWTimes")+
    ylab("Density")+ xlab("FDI Value,USD (transformed)")
ggsave(paste("descriptive_plots/check_MIsums_logged.pdf",sep=""), name, device="pdf", width=6, height=4)