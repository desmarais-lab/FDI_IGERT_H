#load packages
library(gtools)

fdi_types <- c("stock", "flows")

for(df_type in fdi_types){

#load in data
fdi <- read.csv(paste0("clean_", df_type, ".csv"), stringsAsFactors=FALSE)   

#create full panel

id1 <- unique(fdi$Origin.Code) # 186
years <- seq(2001, 2012)
panel <- data.frame(permutations(n=184,r=2,v=c(id1),repeats.allowed=F)) # 33672
panel$dyadid <- seq(1:33672)
panel <- do.call("rbind", replicate(12, panel, simplify = FALSE)) # 408480
yearid <- data.frame()
for(i in years){
  year <- data.frame(rep(i, 33672))
  yearid <- rbind(yearid,year)
}
panel$Year <- yearid[,1]
names(panel) <- c("Dest.Code", "Origin.Code", "dyadid", "Year")
rm(i, id1, years, yearid, year)

#merge fdi data to panel
dest_name <- data.frame(unique(fdi$Dest.Code), unique(fdi$Destination))
panel <- merge(panel, dest_name, by.x =c("Dest.Code"), by.y = c("unique.fdi.Dest.Code."))
origin_name <- data.frame(unique(fdi$Dest.Code), unique(fdi$Destination))
panel <- merge(panel, origin_name, by.x =c("Origin.Code"), by.y = c("unique.fdi.Dest.Code."))
names(panel) <- c("Origin.Code","Dest.Code",  "dyadid", "Year", "Destination", "Origin")
fdi_value <- fdi[3:7]
fdi_value <- fdi_value[,-2]
panel <- merge(panel, fdi_value, by =c("Dest.Code", "Origin.Code", "Year"), all.x =TRUE)
rm(dest_name, origin_name, fdi_value, fdi)

########### control variable datasets #############




# add distance data
distance <- read.csv("covariate_data/dist.csv", stringsAsFactors=FALSE)        #Distance, etc.
panel <- merge(panel, distance, by.x =c("Dest.Code", "Origin.Code"), 
               by.y = c("iso_d", "iso_o"), all.x=TRUE)
rm(distance)

# GDP, population, and exchange rate data from Penn World
pwt <- read.csv("covariate_data/pwt.csv", stringsAsFactors=FALSE)        
pwt <- pwt[,c(1,4,7, 18,26)]  #drop other varables
pwt <- subset(pwt, pwt$year>2000 & pwt$year<2013)  #subset by year
colnames(pwt)[1] <- "countrycode"
panel <- merge(panel, pwt, by.x = c("Dest.Code", "Year"), by.y = c("countrycode", "year"), all.x =TRUE)
colnames(panel)[20:22] <- c("Dest.pop", "Dest.GDP", "Dest.xr")
panel <- merge(panel, pwt, by.x = c("Origin.Code", "Year"), by.y = c("countrycode", "year"), all.x =TRUE)
colnames(panel)[23:25] <- c("Origin.pop", "Origin.GDP", "Origin.xr")
rm(pwt)

# Alliances
alliance <- read.csv("covariate_data/alliance.csv", stringsAsFactors=FALSE)    #Security Alliances
alliance <- subset(alliance, alliance$year>=2000) # only 126 countries
library(countrycode)
panel$Origin.cown <- countrycode(panel$Origin.Code, "wb", "cown") # create COW numeric codes
panel$Dest.cown <- countrycode(panel$Dest.Code, "wb", "cown")
alliance <- alliance[,-2]
alliance <- alliance[,-3]
# only 16,146 unique obsevations. need to collapse
library(doBy)
alliance <- summaryBy(defense+nonaggression+neutrality+entente ~ ccode1+ccode2+year, 
                      data=alliance, FUN=max)
#merge with panel
panel <- merge(panel, alliance, by.x = c("Origin.cown", "Dest.cown", "Year"),
               by.y = c("ccode1", "ccode2", "year"), all.x =TRUE)
panel <- merge(panel, alliance, by.x = c("Origin.cown", "Dest.cown", "Year"), 
               by.y = c("ccode2", "ccode1", "year"), all.x =TRUE)
panel$defense.max.x <- ifelse(is.na(panel$defense.max.x ), 
                              panel$defense.max.y ,panel$defense.max.x )
panel$neutrality.max.x <- ifelse(is.na(panel$neutrality.max.x), 
                                 panel$neutrality.max.y,panel$neutrality.max.x)
panel$nonaggression.max.x <- ifelse(is.na(panel$nonaggression.max.x), 
                                    panel$nonaggression.max.y,panel$nonaggression.max.x)
panel$entente.max.x <- ifelse(is.na(panel$entente.max.x), 
                              panel$entente.max.y,panel$entente.max.x)
panel <- panel[,1:31]
panel <- merge(panel, alliance, by.x = c("Dest.cown","Origin.cown", "Year"),
               by.y = c("ccode1", "ccode2", "year"), all.x =TRUE)
colnames(panel)[32] <- "defense"
colnames(panel)[33] <- "non_aggression"
colnames(panel)[34] <- "neutrality"
colnames(panel)[35] <- "entente"
panel <- merge(panel, alliance, by.x = c("Dest.cown","Origin.cown",  "Year"), 
               by.y = c("ccode2", "ccode1", "year"), all.x =TRUE)
panel$defense.max.x <- ifelse(is.na(panel$defense.max.x ), 
                              panel$defense.max ,panel$defense.max.x )
panel$neutrality.max.x <- ifelse(is.na(panel$neutrality.max.x), 
                                 panel$neutrality.max,panel$neutrality.max.x)
panel$nonaggression.max.x <- ifelse(is.na(panel$nonaggression.max.x), 
                                    panel$nonaggression.max,panel$nonaggression.max.x)
panel$entente.max.x <- ifelse(is.na(panel$entente.max.x), 
                              panel$entente.max,panel$entente.max.x)
panel$defense.max.x <- ifelse(is.na(panel$defense.max.x ), 
                              panel$defense ,panel$defense.max.x )
panel$neutrality.max.x <- ifelse(is.na(panel$neutrality.max.x), 
                                 panel$neutrality,panel$neutrality.max.x)
panel$nonaggression.max.x <- ifelse(is.na(panel$nonaggression.max.x), 
                                    panel$non_aggression,panel$nonaggression.max.x)
panel$entente.max.x <- ifelse(is.na(panel$entente.max.x), 
                              panel$entente,panel$entente.max.x)
panel <- panel[,1:31]
rm(alliance)


# PolityIV
polity <- read.csv("covariate_data/PolityIV.csv", stringsAsFactors=FALSE)      #Polity Score
polity <- subset(polity, polity$year>2000 & polity$year<2013)
polity$ccode <- ifelse(polity$ccode==818, 816, polity$ccode)
polity <- polity[,-2:-3]
panel <- merge(panel, polity, by.x = c("Dest.cown", "Year"), by.y = c("ccode", "year"), all.x =TRUE)
colnames(panel)[32] <- "Dest.polity"
panel <- merge(panel, polity, by.x = c("Origin.cown", "Year"), by.y = c("ccode", "year"), all.x =TRUE)
colnames(panel)[33] <- "Origin.polity"
rm(polity)

# WB: Population, trade openness, and growth rate
WB <- read.csv("covariate_data/WB_var.csv", stringsAsFactors=FALSE)            #Trade opennes, Pop., growth
WB <- WB[,c(-2,-4,-6)]
panel <- merge(panel, WB, by.x = c("Dest.Code", "Year"), by.y = c("ccode", "Year"), all.x =TRUE)
colnames(panel)[34] <- "Dest.TO"
panel <- merge(panel, WB, by.x = c("Origin.Code", "Year"), by.y = c("ccode", "Year"), all.x =TRUE)
colnames(panel)[35] <- "Origin.TO"
rm(WB)

# Political Violence
violence <- read.csv("covariate_data/pv_total.csv", stringsAsFactors=FALSE)    #Political Violence
violence <- subset(violence, violence$year>2000 & violence$year<2013)
violence <- violence[,-3]
violence <- violence[,-1]
violence$ccode <- ifelse(violence$ccode==818, 816, violence$ccode)
panel <- merge(panel, violence, by.x = c("Dest.cown", "Year"), by.y = c("ccode", "year"), all.x =TRUE)
colnames(panel)[36] <- "Dest.pv"
panel <- merge(panel, violence, by.x = c("Origin.cown", "Year"), by.y = c("ccode", "year"), all.x =TRUE)
colnames(panel)[37] <- "Origin.pv"
rm(violence)

# PTA network
PTA_s <- read.csv("covariate_data/PTA_sign.csv", stringsAsFactors=FALSE)       #PTA signed
PTA_w <- read.csv("covariate_data/PTA_withdrawal.csv", stringsAsFactors=FALSE) #PTA withdrew
PTA_d <- read.csv("covariate_data/PTA_depth.csv", stringsAsFactors=FALSE)      #PTA withdrew
# remove unnessecary variables from depth
PTA_d <- PTA_d[,1:6]
PTA_d <- PTA_d[,-2:-4]
# clean and change signing data
PTA_s <- PTA_s[,2:8]
PTA_s$ISO1 <- countrycode(PTA_s$ISO1, "iso3n", "cown")
PTA_s$ISO2 <- countrycode(PTA_s$ISO2, "iso3n", "cown")
PTA_s <- PTA_s[,-3:-4]
colnames(PTA_s)[5] <- "pta_s.year"
colnames(PTA_s)[4] <- "pta_name"
# clean and change withdraw
PTA_w <- PTA_w[,2:8]
PTA_w$ISO1 <- countrycode(PTA_w$ISO1, "iso3n", "cown")
PTA_w$ISO2 <- countrycode(PTA_w$ISO2, "iso3n", "cown")
PTA_w <- PTA_w[,-3:-4]
PTA_w <- PTA_w[,-4]
colnames(PTA_w)[4] <- "pta_w.year"
#merge ptas together
pta <- merge(PTA_s, PTA_w, by = c("ISO1","ISO2","NumberSimple"),  all.x =TRUE)
pta <- merge(pta, PTA_d, by = c("NumberSimple"),  all.x =TRUE)
rm(PTA_s, PTA_w, PTA_d)
#clean pta dataset
pta <- subset(pta, pta$pta_w.year>2000 | is.na(pta$pta_w.year))
pta <- subset(pta, pta$pta_s.year<2013)
pta <- subset(pta, is.na(pta$pta_w.year))# all those with withdraw, happened same year, didn't include
pta <- pta[,-1]
pta <- pta[,-5]
pta <- pta[,-3]
pta$pta_s.year <- ifelse(pta$pta_s.year<2001, 2001, pta$pta_s.year)
pta <- summaryBy(depth_index + depth_rasch_new ~ ISO1+ISO2+pta_s.year, 
                 data=pta, FUN=max)
#merge pta data to panel
library(dplyr)
library(zoo)
panel <- merge(panel, pta, by.x = c("Origin.cown", "Dest.cown", "Year"), 
               by.y = c("ISO1","ISO2", "pta_s.year"), all.x =TRUE)
panel <- merge(panel, pta, by.x = c("Origin.cown", "Dest.cown", "Year"), 
               by.y = c("ISO2", "ISO1", "pta_s.year"), all.x =TRUE)
panel$depth_index.max.x <- ifelse(is.na(panel$depth_index.max.x), 
                                  panel$depth_index.max.y,panel$depth_index.max.x)
panel$depth_rasch_new.max.x <- ifelse(is.na(panel$depth_rasch_new.max.x), 
                                      panel$depth_rasch_new.max.y,panel$depth_rasch_new.max.x)
panel <- panel[order(panel$dyadid, panel$Year),]
na.locf2 <- function(x) na.locf(x, na.rm = FALSE)
panel <- transform(panel, depth_index.max.x = ave(depth_index.max.x, dyadid, FUN = na.locf2))
panel <- transform(panel, depth_rasch_new.max.x = ave(depth_rasch_new.max.x, dyadid, FUN = na.locf2))
panel <- panel[,1:39]
colnames(panel)[38] <- "depth_index"
colnames(panel)[39] <- "depth_latent"
panel$depth_index <- ifelse(is.na(panel$depth_index),0,panel$depth_index)
panel$depth_latent <- ifelse(!is.na(panel$depth_latent),
                             panel$depth_latent - min(na.omit(panel$depth_latent)),
                             panel$depth_latent)
panel$depth_latent <- ifelse(is.na(panel$depth_latent),0,panel$depth_latent)
rm(pta)

# Supply Chains: imports, COU is desination
trade_end <- read.csv("covariate_data/oecd_enduse.csv", stringsAsFactors=FALSE)       
trade_int <- read.csv("covariate_data/oecd_intmed.csv", stringsAsFactors=FALSE)
trade <- rbind(trade_end, trade_int)
rm(trade_end, trade_int)
# clean off variables
trade <- trade[,-2:-4]
trade <- trade[,-3]
trade <- trade[,-4:-9]
trade <- trade[,-5:-10]
trade <- trade[,1:5]
#split into four datasets
mix <- subset(trade, trade$CAT=="XMIXED")
mix <- mix[,-3]
colnames(mix)[4] <- "trade_mix"

cap <- subset(trade, trade$CAT=="CAP")
cap <- cap[,-3]
colnames(cap)[4] <- "trade_cap"

int <- subset(trade, trade$CAT=="INT")
int <- int[,-3]
colnames(int)[4] <- "trade_int"

hco <- subset(trade, trade$CAT=="CONS")
hco <- hco[,-3]
colnames(hco)[4] <- "trade_hco"
rm(trade)
colnames(cap)[1] <- "COU"
colnames(hco)[1] <- "COU"
colnames(int)[1] <- "COU"
colnames(mix)[1] <- "COU"
#merge to panel
panel <- merge(panel, hco, by.x = c("Dest.Code", "Origin.Code", "Year"), 
               by.y = c("COU", "PAR", "Time"), all.x =TRUE)
panel[,40] <- ifelse(is.na(panel[,40]), 0, panel[,40])
panel <- merge(panel, int, by.x = c("Dest.Code", "Origin.Code", "Year"), 
               by.y = c("COU", "PAR", "Time"), all.x =TRUE)
panel[,41] <- ifelse(is.na(panel[,41]), 0, panel[,41])
panel <- merge(panel, cap, by.x = c("Dest.Code", "Origin.Code", "Year"), 
               by.y = c("COU", "PAR", "Time"), all.x =TRUE)
panel[,42] <- ifelse(is.na(panel[,42]), 0, panel[,42])
panel <- merge(panel, mix, by.x = c("Dest.Code", "Origin.Code", "Year"), 
               by.y = c("COU", "PAR", "Time"), all.x =TRUE)
panel[,43] <- ifelse(is.na(panel[,43]), 0, panel[,43])
rm(mix, cap, int, hco)


# Transparency
transparency <- read.csv("covariate_data/cpi.csv", stringsAsFactors=FALSE)     #Transparency Score
#wide to long
years <- colnames(transparency)[3:20]
transparency <- reshape(transparency, varying = years, v.names = "trans_score", 
                        timevar = "year", times = years,
                        direction="long")
transparency <- transparency[,2:4]
transparency$year <- gsub("X", "",transparency$year)
transparency$year <- as.numeric(transparency$year)
transparency <- subset(transparency, transparency$year>2000 & transparency$year<2013)
#merge
panel <- merge(panel, transparency, by.x = c("Destination", "Year"), 
               by.y = c("Jurisdiction","year"), all.x =TRUE)
colnames(panel)[44] <- "t.score_Dest"
panel <- merge(panel, transparency, by.x = c("Origin", "Year"), 
               by.y = c("Jurisdiction","year"), all.x =TRUE)
colnames(panel)[45] <- "t.score_Origin"
rm(transparency, years)

# BIT ########################################################################
bit <- read.csv("covariate_data/bit.csv", stringsAsFactors=FALSE)
#clean off unneeded columns and split year out
bit <- bit[,c(2,3,6,7)]
bit <- subset(bit, bit$inforce_pre != "")
bit$force_yr <- sapply(strsplit(bit$inforce_pre, "/"),"[", 3)
bit <- bit[,c(1,2,5)]
bit$bit_dummy <- 1
bit <- unique(bit)
# fix names for merge ###
#subset and create sets with beglium and luxembourg seperate
c1_bl <- subset(bit, bit$country1=="Belgium/Luxembourg")
c1_bl$country1.1 <- "Belgium"
c1_bl$country1.2 <- "Luxembourg"
c1_bl.1 <- c1_bl[,c(2,3,4,5)]
c1_bl.2 <- c1_bl[,c(2,3,4,6)]
colnames(c1_bl.1)[4] <- "country1"
colnames(c1_bl.2)[4] <- "country1"
c1_bl <- rbind(c1_bl.1, c1_bl.2)
rm(c1_bl.1,c1_bl.2)
c1_bleu <- subset(bit, bit$country1=="BLEU (Belgium_Luxembourg Economic Union)")
c1_bleu$country1.1 <- "Belgium"
c1_bleu$country1.2 <- "Luxembourg"
c1_bleu.1 <- c1_bleu[,c(2,3,4,5)]
c1_bleu.2 <- c1_bleu[,c(2,3,4,6)]
colnames(c1_bleu.1)[4] <- "country1"
colnames(c1_bleu.2)[4] <- "country1"
c1_bleu <- rbind(c1_bleu.1, c1_bleu.2)
rm(c1_bleu.1,c1_bleu.2)
c2_bl <- subset(bit, bit$country2=="Belgium/Luxembourg")
c2_bl$country1.1 <- "Belgium"
c2_bl$country1.2 <- "Luxembourg"
c2_bl.1 <- c2_bl[,c(2,3,4,5)]
c2_bl.2 <- c2_bl[,c(2,3,4,6)]
colnames(c2_bl.1)[4] <- "country1"
colnames(c2_bl.2)[4] <- "country1"
c2_bl <- rbind(c2_bl.1, c2_bl.2)
rm(c2_bl.1,c2_bl.2)
c2_bleu <- subset(bit, bit$country2=="BLEU (Belgium_Luxembourg Economic Union)")
c2_bleu$country1.1 <- "Belgium"
c2_bleu$country1.2 <- "Luxembourg"
c2_bleu.1 <- c2_bleu[,c(2,3,4,5)]
c2_bleu.2 <- c2_bleu[,c(2,3,4,6)]
colnames(c2_bleu.1)[4] <- "country1"
colnames(c2_bleu.2)[4] <- "country1"
c2_bleu <- rbind(c2_bleu.1, c2_bleu.2)
rm(c2_bleu.1,c2_bleu.2)
#remove bl obs from master and add in new sets
bit <- subset(bit, bit$country2 != "BLEU (Belgium_Luxembourg Economic Union)")
bit <- subset(bit, bit$country1 != "BLEU (Belgium_Luxembourg Economic Union)")
bit <- subset(bit, bit$country2 != "Belgium/Luxembourg")
bit <- subset(bit, bit$country1 != "Belgium/Luxembourg")
bit <- rbind(bit, c1_bl, c1_bleu, c2_bl, c2_bleu)
rm(c1_bl, c1_bleu, c2_bl, c2_bleu)
# now fix wrong country names
bit$country2 <- ifelse(bit$country2=="Bolivia, Plurinational State of","Bolivia" ,bit$country2)
bit$country2 <- ifelse(bit$country2=="C<f4>te d'Ivoire","Ivory Coast" ,bit$country2)
bit$country2 <- ifelse(bit$country2=="Congo","Congo (Brazzaville)" ,bit$country2)
bit$country2 <- ifelse(bit$country2=="Congo, Republic of","Congo, Democratic Republic of the" ,bit$country2)
bit$country2 <- ifelse(bit$country2=="Hong Kong, China SAR","Hong Kong, Special Administrative Region of China" ,bit$country2)
bit$country2 <- ifelse(bit$country2=="Korea, Dem. People's Rep. of","Korea, Democratic People's Republic of" ,bit$country2)
bit$country2 <- ifelse(bit$country2=="Lao People's Democratic Republic","Lao PDR" ,bit$country2)
bit$country2 <- ifelse(bit$country2=="Macedonia, The former Yugoslav Republic of","Macedonia, Republic of" ,bit$country2)
bit$country2 <- ifelse(bit$country2=="Moldova, Republic of","Moldova" ,bit$country2)
bit$country2 <- ifelse(bit$country2=="Occupied Palestinian territory","Palestinian Territory, Occupied" ,bit$country2)
bit$country2 <- ifelse(bit$country2=="Serbia and Montenegro","Montenegro" ,bit$country2)
bit$country2 <- ifelse(bit$country2=="Syrian Arab Republic","Syrian Arab Republic (Syria)" ,bit$country2)
bit$country2 <- ifelse(bit$country2=="Taiwan Province of China","Taiwan, Republic of China" ,bit$country2)
bit$country2 <- ifelse(bit$country2=="Timor_Leste","Timor-Leste" ,bit$country2)
bit$country2 <- ifelse(bit$country2=="United Republic of Tanzania","Tanzania, United Republic of" ,bit$country2)
bit$country2 <- ifelse(bit$country2=="Venezuela, Bolivarian Republic of","Venezuela (Bolivarian Republic of)" ,bit$country2)
bit$country1 <- ifelse(bit$country1=="Bolivia, Plurinational State of","Bolivia" ,bit$country1)
bit$country1 <- ifelse(bit$country1=="C<f4>te d'Ivoire","Ivory Coast" ,bit$country1)
bit$country1 <- ifelse(bit$country1=="Congo","Congo (Brazzaville)" ,bit$country1)
bit$country1 <- ifelse(bit$country1=="Congo, Democratic Republic of the","Congo, Democratic Republic of the" ,bit$country1)
bit$country1 <- ifelse(bit$country1=="Guinea_Bissau","Guinea-Bissau" ,bit$country1)
bit$country1 <- ifelse(bit$country1=="Hong Kong, China SAR","Hong Kong, Special Administrative Region of China" ,bit$country1)
bit$country1 <- ifelse(bit$country1=="Korea, Dem. People's Rep. of","Korea, Democratic People's Republic of" ,bit$country1)
bit$country1 <- ifelse(bit$country1=="Lao People's Democratic Republic","Lao PDR" ,bit$country1)
bit$country1 <- ifelse(bit$country1=="Macao, China SAR","Macao, Special Administrative Region of China" ,bit$country1)
bit$country1 <- ifelse(bit$country1=="Macedonia, The former Yugoslav Republic of","Macedonia, Republic of" ,bit$country1)
bit$country1 <- ifelse(bit$country1=="Moldova, Republic of","Moldova" ,bit$country1)
bit$country1 <- ifelse(bit$country1=="Syrian Arab Republic","Syrian Arab Republic (Syria)" ,bit$country1)
bit$country1 <- ifelse(bit$country1=="Taiwan Province of China","Taiwan, Republic of China" ,bit$country1)
bit$country1 <- ifelse(bit$country1=="United Republic of Tanzania","Tanzania, United Republic of" ,bit$country1)
# merge together
panel <- merge(panel, bit, by.x = c("Destination", "Origin", "Year"), 
               by.y = c("country1", "country2", "force_yr"), all.x =TRUE)
panel <- merge(panel, bit, by.x = c("Destination", "Origin", "Year"), 
               by.y = c("country2","country1",  "force_yr"), all.x =TRUE)
panel$bit_dummy.x <- ifelse(is.na(panel$bit_dummy.x),panel$bit_dummy.y,panel$bit_dummy.x)
panel <- panel[,-47]
panel <- merge(panel, bit, by.x = c("Origin","Destination",  "Year"), 
               by.y = c("country1", "country2", "force_yr"), all.x =TRUE)
panel$bit_dummy.x <- ifelse(is.na(panel$bit_dummy.x),panel$bit_dummy,panel$bit_dummy.x)
panel <- panel[,-47]
panel <- merge(panel, bit, by.x = c("Origin","Destination",  "Year"), 
               by.y = c("country2","country1",  "force_yr"), all.x =TRUE)
panel$bit_dummy <- ifelse(is.na(panel$bit_dummy),panel$bit_dummy.x,panel$bit_dummy)
panel <- panel[,-46]
#carryforward values
panel <- panel[order(panel$dyadid, panel$Year),]
panel <- transform(panel, bit_dummy = ave(bit_dummy, dyadid, FUN = na.locf2))
#replace remaining empty values with zero
panel$bit_dummy <- ifelse(is.na(panel$bit_dummy),0,panel$bit_dummy)
rm(bit)





#write csv
write.csv(panel, file = paste0("panel_", df_type, ".csv"))
}
