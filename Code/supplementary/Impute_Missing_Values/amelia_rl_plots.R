

#libraries
library(broom)
library(ergm.count)

for(q in c(25,50,100)){

list_w <- list() 
for(n in 1:10){
#load ERGM results for with
load(paste0("results_w_q", q,"_",n,".RData"))
#create dataframe for with
years <- 2002:2012
df_w <- tidy(ergms[[1]], conf.int=TRUE)
df_w$year <- 2002
for(i in 2:11){
  df <- tidy(ergms[[i]], conf.int=TRUE)
  df$year <- years[i]
  df_w <- rbind(df_w, df)
}
list_w[n] <- df_w
}

df_w <- list_w[1]
for(n in 2:10){
  df_w2 <- list_w[n]
  df_w[,c(2:7)] <- df_w[,c(2:7)] + df_w2[,c(2:7)]
}  
df_w[,c(2:7)] <- df_w[,c(2:7)]/10
#remove everything but dataframe without 
rm(list=setdiff(ls(), "df_w"))

list_wo <- list() 
for(n in 1:10){
#load ERGM results for with
load(paste0("results_wo_q", q,"_",n,".RData"))
list_wo <- list() 
#create dataframe for without
years <- 2002:2012
df_wo <- tidy(ergms[[1]], conf.int=TRUE)
df_wo$year <- 2002
for(i in 2:11){
  df <- tidy(ergms[[i]], conf.int=TRUE)
  df$year <- years[i]
  df_wo <- rbind(df_wo, df)
}
list_wo[n] <- df_w
}

df_wo <- list_wo[1]
for(n in 2:10){
  df_wo2 <- list_wo[n]
  df_wo[,c(2:7)] <- df_wo[,c(2:7)] + df_wo2[,c(2:7)]
}  
df_wo[,c(2:7)] <- df_wo[,c(2:7)]/10



df_w$model <- "with"
df_wo$model <- "without"

df_25 <- rbind(df_w, df_wo)
#remove everything but dataframe without 
rm(list=setdiff(ls(), "df_25"))


 
#name dataframe variables
vars <- c("Sum", "Sum_5", "Nonzero", "Mutuality", "Transitivity",
          "Dest_Polity", "Origin_Polity", "Dest_TO","Origin_TO",
          "Dest_GDPpc", "Origin_GDPpc",
          "LDV", "Mass", "Distance", "Non_Defense_Treaty", "Defense_Treaty", 
          "Trade_Volume", "Bilateral_Investment_Treaty")

vars_wo <- c("Sum", "Sum_5", "Nonzero", 
             "Dest_Polity", "Origin_Polity", "Dest_TO","Origin_TO",
             "Dest_GDPpc", "Origin_GDPpc",
             "LDV", "Mass", "Distance", "Non_Defense_Treaty", "Defense_Treaty", 
             "Trade_Volume", "Bilateral_Investment_Treaty")

df_names <- unique(df_25$term)


#Years
m_yr <- (2002:2012)


# plot the variables
#extrafont::loadfonts(device="win")
library(ggplot2)
#library(gridExtra)



list_vars <- c(1:3, 6:18)


for(i in list_vars){
  df <- subset(df_25, df_25$term==df_names[i])
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
  ggsave(paste("q",q,"_rl_plots/",vars[i],".pdf", sep=""), name, device="pdf", width=2.85, height=2)
}


for(i in 4:5){
  df <- subset(df_25, df_25$term==df_names[i])
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
  ggsave(paste("q",q,"_rl_plots/",vars[i],".pdf", sep=""), name, device="pdf", width=2.85, height=2)
}


}
