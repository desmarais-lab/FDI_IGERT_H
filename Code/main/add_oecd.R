# clear workspace
rm(list=ls())
set.seed(19)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#read in data
fdi <- read.csv("subset_stock.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]


# add OECD binary for node variable and edgecov
oecd <- read.csv("../create_datasets/covariate_data/oecd_join_year.csv", stringsAsFactors=FALSE)        #FDI


df <- df1 <- subset(fdi, fdi$Year==2001)
df2 <- subset(oecd, oecd$year<=2001)
df$Dest.oecd <- ifelse(df$Destination %in% df2$country, 1,0)
df$Origin.oecd <- ifelse(df$Origin %in% df2$country, 1,0)
df$both_oecd <- ifelse(df$Dest.oecd==1 & df$Origin.oecd==1, 1,0)
for(y in 2002:2012){
  df1 <- subset(fdi, fdi$Year==y)
  df2 <- subset(oecd, oecd$year<=y)
  df1$Dest.oecd <- ifelse(df1$Destination %in% df2$country, 1,0)
  df1$Origin.oecd <- ifelse(df1$Origin %in% df2$country, 1,0)
  df1$both_oecd <- ifelse(df1$Dest.oecd==1 & df1$Origin.oecd==1, 1,0)
  df <- rbind(df,df1)
}


write.csv(df, file ="subset_stock2.csv")