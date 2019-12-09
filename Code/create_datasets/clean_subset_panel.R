#=============================================================#
# Work Done: subset panel based on missing data and write files
#            to the analysis folders
#=============================================================#
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# libraries
library(DataCombine)


###### create subset panels and write to SI and main folder ##################
fdi_types <- c("stock", "flows")

for(df_type in fdi_types){


#load in data
fdi <- read.csv(paste0("panel_", df_type, ".csv"), stringsAsFactors=FALSE)
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


fdi<-fdi[,c(1,2,3,8,9,18,28,32:35,39,46:51)]

#drop based on missing covariates
fdi_drop <- na.omit(fdi[,c(1:4,6:18)])
a <- unique(fdi_drop$Origin)
fdi <- fdi[fdi$Origin %in% a,]
fdi <- fdi[fdi$Destination %in% a,]
rm(fdi_drop,a)


#drop missing values by variables
df_na <- subset(fdi, is.na(fdi$Dest.polity) | is.na(fdi$Dest.TO))
df_na <- (unique(df_na$Destination))
'%!in%' <- function(x,y)!('%in%'(x,y))
fdi <- subset(fdi, fdi$Origin %!in% df_na)
fdi <- subset(fdi, fdi$Destination %!in% df_na)

########## Variable Transformations ##############

#log
fdi$dist <- log(fdi$dist)
fdi$mass <- log(fdi$mass)
fdi$trade_ln <- log(fdi$dyadic_trade+1)
fdi$Origin.GDPpc <- log(fdi$Origin.GDPpc)
fdi$Dest.GDPpc <- log(fdi$Dest.GDPpc)


# log and round DV, create lag
fdi$Value <- ifelse(fdi$Value<0, 0, fdi$Value)
fdi$Value_ln <- log(fdi$Value+1)
fdi$Value_ln <- ifelse(is.na(fdi$Value_ln), 0, fdi$Value_ln)
fdi$Value_ln <- round(fdi$Value_ln, digits = 0)
fdi <- slide(fdi, Var = "Value_ln", GroupVar = "dyadid", slideBy = -1)
fdi <- slide(fdi, Var = "Value", GroupVar = "dyadid", slideBy = -1)

#write out files
if(df_type == "stock"){write.csv(fdi, file = "../main/subset_stock.csv")}
else if(df_type == "flows"){write.csv(fdi, file = "../supplementary/subset_flows.csv")}

}