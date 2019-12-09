
#load packages
library(magic)
library(network)
library(igraph)
library(doBy)
library(plyr)



#load in data
fdi <- read.csv("../../main/subset_stock.csv", stringsAsFactors=FALSE)        #FDI
fdi <- fdi[,-1]



# remove variables and create dummy for missing data
fdi <- fdi[,1:5]
fdi$Value <- ifelse(!is.na(fdi$Value), 1, fdi$Value)
fdi$Value <- ifelse(is.na(fdi$Value), 0, fdi$Value)


### create list of countries and number of missing values ###
c_list <- data.frame(unique(fdi$Origin))
n <- 1
for(t in 2001:2012){
  n <- n+1
  c_list <- cbind(c_list,NA)
  colnames(c_list)[n] <- paste0("obs","." ,t)
  ### create adjacency matrix to rank countries by edge list completeness ####
  fdi_yr <- subset(fdi, fdi$Year ==t)
  fdi_graph <- graph.data.frame(fdi_yr)
  full <- get.adjacency(fdi_graph,attr='Value', sparse=FALSE)
  #colnames(full) <- paste(colnames(full), "." ,"2001", sep = "")
  #row.names(full) <- paste(row.names(full), "." ,"2001", sep = "")
  
  for(i in 1:length(c_list[,1])){
    a <- sum(full[,i])
    b <- sum(full[i,]) 
    c_list[i,n] <- (a + b)/(length(c_list[,1])*2-2)
  }

}

# find average missingness 
c_list$avg <- rowSums(c_list[,2:13])/12

hist(c_list$avg, breaks = 25, xlab = "Completeness")


for(i in seq(0, .7, .05)){
  a <- length(subset(c_list$avg, c_list$avg>=i))
  name <- paste("p=", 1-i, ", # of countries =", a)
  print(name)
}



# find the p that gives full(er) adjacency matrix
# p = .14, q = .5, n =70/ p =.28, q = 0.25, n = 28


sub <- c_list[,1][c_list$avg>=.28]

### create adjacency matrix to rank countries by edge list completeness ####
fdi_yr <- subset(fdi, fdi$Year ==2001)
fdi_sub <- fdi_yr[fdi_yr$Origin %in% sub,]
fdi_sub <- fdi_sub[fdi_sub$Destination %in% sub,]
fdi_graph <- graph.data.frame(fdi_sub)
full <- get.adjacency(fdi_graph,attr='Value', sparse=FALSE)

n <- dim(full)[1]^2-dim(full)[1]
1-sum(full)/n

#write csv
write.csv(rownames(full), file = "q_25.csv")

sub <- c_list[,1][c_list$avg>=.14]

### create adjacency matrix to rank countries by edge list completeness ####
fdi_yr <- subset(fdi, fdi$Year ==2001)
fdi_sub <- fdi_yr[fdi_yr$Origin %in% sub,]
fdi_sub <- fdi_sub[fdi_sub$Destination %in% sub,]
fdi_graph <- graph.data.frame(fdi_sub)
full <- get.adjacency(fdi_graph,attr='Value', sparse=FALSE)

n <- dim(full)[1]^2-dim(full)[1]
1-sum(full)/n

#write csv
write.csv(rownames(full), file = "q_50.csv")


sub <- c_list[,1][c_list$avg>=1]

### create adjacency matrix to rank countries by edge list completeness ####
fdi_yr <- subset(fdi, fdi$Year ==2001)
fdi_sub <- fdi_yr[fdi_yr$Origin %in% sub,]
fdi_sub <- fdi_sub[fdi_sub$Destination %in% sub,]
fdi_graph <- graph.data.frame(fdi_sub)
full <- get.adjacency(fdi_graph,attr='Value', sparse=FALSE)

n <- dim(full)[1]^2-dim(full)[1]
1-sum(full)/n

#write csv
write.csv(rownames(full), file = "q_100.csv")



