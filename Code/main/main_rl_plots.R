#rm(list=ls())
#set.seed(19)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#libraries
library(broom)
library(ergm.count)

#load ERGM results for with
load("main_network_results_RR.RData")
#create dataframe for with
years <- 2002:2012
df_w <- tidy(ergms[[1]], conf.int=TRUE)
df_w$year <- 2002
for(i in 2:11){
  df <- tidy(ergms[[i]], conf.int=TRUE)
  df$year <- years[i]
  df_w <- rbind(df_w, df)
}
#remove everything but dataframe without 
#rm(list=setdiff(ls(), "df_w"))
#load ERGM results for with
load("main_independent_results_RR.RData")
#create dataframe for without
years <- 2002:2012
df_wo <- tidy(ergms[[1]], conf.int=TRUE)
df_wo$year <- 2002
for(i in 2:11){
  df <- tidy(ergms[[i]], conf.int=TRUE)
  df$year <- years[i]
  df_wo <- rbind(df_wo, df)
}

df_w$model <- "with"
df_wo$model <- "without"

df_50 <- rbind(df_w, df_wo)
#remove everything but dataframe without 
#rm(list=setdiff(ls(), "df_50"))



#name dataframe variables
vars <- c("Sum", "Sum_5", "Nonzero", "Mutuality_OECD", "Mutuality_notOECD",
          "Transitivity","Dest_Polity", "Origin_Polity", "Dest_TO","Origin_TO",
          "Dest_GDPpc", "Origin_GDPpc","Nodematch_OECD",
          "LDV", "Mass", "Distance", "Non_Defense_Treaty", "Defense_Treaty", 
          "Trade_Volume", "Bilateral_Investment_Treaty", "PTA_Depth")

vars_wo <- c("Sum", "Sum_5", "Nonzero", 
             "Dest_Polity", "Origin_Polity", "Dest_TO","Origin_TO",
             "Dest_GDPpc", "Origin_GDPpc","Nodematch_OECD",
             "LDV", "Mass", "Distance", "Non_Defense_Treaty", "Defense_Treaty", 
             "Trade_Volume", "Bilateral_Investment_Treaty", "PTA_Depth")

df_50$term <- c(rep(vars,11), rep(vars_wo,11))

df_names <- unique(df_50$term)


#Years
m_yr <- (2002:2012)


# plot the variables
#extrafont::loadfonts(device="win")
library(ggplot2)
#library(gridExtra)



list_vars <- c(1:3, 7:21)


for(i in list_vars){
  df <- subset(df_50, df_50$term==df_names[i])
  df$year <- ifelse(df$model=="with", df$year+.1, df$year)
  df$year <- ifelse(df$model=="without", df$year-.1, df$year)
  name <- ggplot(df, aes(x=year, y = estimate, ymin= conf.low, ymax= conf.high,
                         colour=model,legend=TRUE, shape=model), legend=TRUE)+
    geom_pointrange(size= .5, fatten = 1.25)+ geom_hline(yintercept = 0)+
    scale_x_continuous(breaks= seq(2002,2012,3))+
    scale_color_manual(breaks = c("W/", "W/"),
                       values=c("#D55E00", "#000000"))+  
    xlab("Year") + ylab("Coefficient")+ 
    theme(axis.text  = element_text(family="Times", size=9),
          axis.title  = element_text(family="Times", size=8),
          legend.text  = element_text(family="Times"),
          legend.title  = element_text(family="Times"),  
          plot.title = element_text(family="Times"),
          legend.position="none")
  #create plot object
  ggsave(paste("figures/main_rl_plots/",vars[i],".pdf", sep=""), name, device="pdf", width=2.85, height=2)
}


for(i in 4:6){
  df <- subset(df_50, df_50$term==df_names[i])
  name <- ggplot(df, aes(x=year, y = estimate, ymin= conf.low, ymax= conf.high,
                         colour=model,legend=TRUE, shape=model), legend=TRUE)+
    geom_pointrange(size= .5, fatten = 1.25)+ geom_hline(yintercept = 0)+
    scale_x_continuous(breaks= seq(2002,2012,3))+
    scale_color_manual(breaks = c("W/", "W/"),
                       values=c("#D55E00", "#000000"))+  
    xlab("Year") + ylab("Coefficient")+ 
    theme(axis.text  = element_text(family="Times", size=9),
          axis.title  = element_text(family="Times", size=8),
          legend.text  = element_text(family="Times"),
          legend.title  = element_text(family="Times"),  
          plot.title = element_text(family="Times"),
          legend.position="none")
  #create plot object
  ggsave(paste("figures/main_rl_plots/",vars[i],".pdf", sep=""), name, device="pdf", width=2.85, height=2)
}

