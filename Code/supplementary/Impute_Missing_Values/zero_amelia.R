# clear workspace, set seed, set wd
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#load packages
library(Amelia)
library(ggplot2)


#load in data
fdi <- read.csv("../../main/subset_stock2.csv", stringsAsFactors=FALSE)[,-1]        #FDI

#   check missingness
n <- nrow(fdi)
n_NAs <- nrow(subset(fdi,is.na(fdi$Value)))
n_zeros <- nrow(subset(fdi,fdi$Value==0))

#NAs as percentage
n_NAs/n
#zeros as percantage of non-NAs
n_zeros/(n-n_NAs)
#zeros + imputed zeroes as percantage of all data
(n_zeros+n_NAs)/(n)


#drop missing values by variables
df1 <- fdi[,c(1:19,23:25)]


############# Amelia ################


#set id variables for ameilia
ids <-  c("Origin","Destination")

#run imputations
amelia_fit <- amelia(df1, m =10, idvars =ids,
              ts = "Year", cs = c("dyadid"), polytime = 1)

############# Amelia vs Zero ################
df_z <- df1
df_z$Value <- ifelse(is.na(df_z$Value),0,df_z$Value)

df_a <- amelia_fit$imputations[[1]]
for(i in 2:10){
  df_a$Value <- df_a$Value+amelia_fit$imputations[[i]]$Value
}
df_a$Value <- df_a$Value/10

df_z$Group <- "zero-imputed"
df_a$Group <- "Amelia-imputed"

df <- rbind(df_z[,c(5,23)],df_a[,c(5,23)])
df$Value <- ifelse(df$Value<0,0,df$Value)
df$Value <- ceiling(log(df$Value+1)*2)

name <- ggplot(df ,aes(x=(Value), fill=Group)) + 
  geom_density(alpha=0.25)+ 
  scale_fill_manual(values=c("red","blue"),name = "Group")+
  ggtitle(paste0("Zero-imputed Values vs. Amelia-imputations"))+
  ylab("Density")+ xlab("FDI Value, logged, USD")
ggsave("compare_imputed.pdf", name, device="pdf", width=6, height=4)




save(amelia_fit, file = "amelia_fit.Rdata")
