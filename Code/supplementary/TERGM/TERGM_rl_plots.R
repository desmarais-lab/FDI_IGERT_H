

#libraries
library(broom)
library(ergm.count)
library(doBy)

# load data
load("pooledERGM.rda")
df_w  <- pooledERGMres
#load("pooledERGM_wo.rda")
#df_wo <- pooledERGMres

ergm_info <- function(pooledergm_object){
  df <- as.data.frame(pooledergm_object$estimate)
  colnames(df)[1] <- "coef"
  df$term <- names(pooledergm_object$estimate)
  df$se <- pooledergm_object$SE
  df$conf.low <- df$coef - (1.96 * df$se)
  df$conf.high <- df$coef + (1.96 * df$se)
  return(df)
}
  
  

#create dataframe for with
df_w <- ergm_info(df_w)
#df_wo <- ergm_info(df_wo)
df_w$model <- "with"
#df_wo$model <- "without"

#df_50 <- rbind(df_w, df_wo)
#remove everything but dataframe without 
rm(list=setdiff(ls(), "df_w"))



#name dataframe variables
vars <- c("Sum_5", "Nonzero", "Mutuality_OECD_Pair", "Mutuality_Not_OECD_Pair",  "Transitivity",
          "Dest_Polity", "Origin_Polity", "Dest_TO","Origin_TO",
          "Dest_GDPpc", "Origin_GDPpc", "OECD_membership_nodematch",
          "LDV", "Mass", "Distance", "Non_Defense_Treaty", "Defense_Treaty", 
          "Trade_Volume", "Bilateral_Investment_Treaty", "PTA_Depth")

#vars_wo <- c("Sum_5", "Nonzero", 
#             "Dest_Polity", "Origin_Polity", "Dest_TO","Origin_TO",
#             "Dest_GDPpc", "Origin_GDPpc",
#             "LDV", "Mass", "Distance", "Non_Defense_Treaty", "Defense_Treaty", 
#             "Trade_Volume", "Bilateral_Investment_Treaty")

df_w$term <- c(vars, rep("sum", 11))

df_names <- unique(df_w$term)


# plot the variables
#extrafont::loadfonts(device="win")
library(ggplot2)
#library(gridExtra)

list_vars <- c(1:2, 6:20)

for(i in list_vars){
  df <- subset(df_w, df_w$term==df_names[i])
  name <- ggplot(df, aes(x=coef, y = model, xmin= conf.low, xmax= conf.high,
                         colour=model,legend=TRUE, shape=model), legend=TRUE)+
    geom_point()+
    geom_errorbarh(height = .25)+ 
    scale_color_manual(breaks = c("W/", "W/"),
                       values=c("#D55E00", "#000000"))+  
    xlab("coef_value") + ylab("model")+ 
    theme(axis.text  = element_text(family="Times", size=9),
          axis.title  = element_text(family="Times", size=8),
          legend.text  = element_text(family="Times"),
          legend.title  = element_text(family="Times"),  
          plot.title = element_text(family="Times"),
          legend.position="none")
  #create plot object
  ggsave(paste("rl_TERGM_plots/",vars[i],".pdf", sep=""), name, device="pdf", width=2.85, height=2)
}


for(i in 3:5){
  df <- subset(df_w, df_w$term==df_names[i])
  name <- ggplot(df, aes(x=coef, y = model, xmin= conf.low, xmax= conf.high,
                         colour=model,legend=TRUE, shape=model), legend=TRUE)+
    geom_point()+
    geom_errorbarh(height = .25)+ 
    scale_color_manual(breaks = c("W/", "W/"),
                       values=c("#D55E00", "#000000"))+  
    xlab("coef_value") + ylab("model")+ 
    theme(axis.text  = element_text(family="Times", size=9),
          axis.title  = element_text(family="Times", size=8),
          legend.text  = element_text(family="Times"),
          legend.title  = element_text(family="Times"),  
          plot.title = element_text(family="Times"),
          legend.position="none")
  #create plot object
  ggsave(paste("rl_TERGM_plots/",vars[i],".pdf", sep=""), name, device="pdf", width=2.85, height=2)
}

df <- subset(df_w, df_w$term=="sum")
df <- summaryBy(coef+conf.low+conf.high ~ model, data=df)
name <- ggplot(df, aes(x=coef.mean, y = model, xmin= conf.low.mean, xmax= conf.high.mean,
                       colour=model,legend=TRUE, shape=model), legend=TRUE)+
  geom_point()+
  geom_errorbarh(height = .25)+ 
  scale_color_manual(breaks = c("W/", "W/"),
                     values=c("#D55E00", "#000000"))+  
  xlab("coef_value") + ylab("model")+ 
  theme(axis.text  = element_text(family="Times", size=9),
        axis.title  = element_text(family="Times", size=8),
        legend.text  = element_text(family="Times"),
        legend.title  = element_text(family="Times"),  
        plot.title = element_text(family="Times"),
        legend.position="none")
#create plot object
ggsave(paste("rl_TERGM_plots/","Sum",".pdf", sep=""), name, device="pdf", width=2.85, height=2)



