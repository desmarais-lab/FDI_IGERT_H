 #=============================================================#
# Work Done: Clean raw UNCTAD FDI data
#=============================================================#

# load Libraries
library(reshape2)


#strip off top and bottom rows and first column
clean1 <- function(matrix){
  n = nrow(matrix)-2
  matrix <- matrix[4:n,-1]
} 

clean1.1 <- function(matrix){
  if((matrix[1,1]=="")){
    matrix = matrix[,-1]
  } else{
    matrix = matrix
  }
}


#remove last column
clean2 <- function(matrix){
  col = ncol(matrix)
  matrix=matrix[,-col]
}

# combine clean 1 and 2
clean3 <- function(matrix){
  #strip with clean1
  matrix1 = clean1(matrix)
  matrix2 = clean1.1(matrix1)
  #check if last column is empty and if so remove
  col = ncol(matrix2)
  if(is.na(matrix2[1,col])){
    matrix = clean2(matrix2)
  } else {
    matrix = matrix2
  }
}


# remove column before values
clean4 <- function(matrix){
  n = ncol(matrix)-12
  matrix=matrix[,-n]
}


#combine 3 and 4 to remove NAs inbetween values and country names
clean5 <- function(matrix){
  #apply up to clean 3
  matrix2 = clean3(matrix)
  #
  col = ncol(matrix2)-12
  if(is.na(matrix2[1,col])){
    matrix3 = clean4(matrix2)
  } else{
    matrix3 = matrix2
  }
  matrix = matrix3
}

#check for second column of NAs
clean5.2 <- function(matrix){
  col = ncol(matrix)-12
  if(is.na(matrix[1,col])){
    matrix3 = clean4(matrix)
  } else{
    matrix3 = matrix
  }
  matrix = matrix3
}



# check if row 1 is empty, is so  fill it in.
clean6 <- function(matrix){
  col = ncol(matrix)-12
  r = nrow(matrix)
  for(i in 1:r){
    if(matrix[i,col]==""){
      
      matrix[i,col]=matrix[i,1]
    }
  }
  matrix[,-1]
}


#combine through 5 and loop 6
clean7 <- function(matrix){
  #apply up to clean 3
  matrix2 = clean5(matrix)
  matrix3 = clean5.2(matrix2)
  #
  col = ncol(matrix3)-12
  if(matrix3[1,col]==""){
    matrix = clean6(matrix3)
  } else{
    matrix = matrix3
  }
}

#combine through 7 and loop 6
clean8 <- function(matrix){
  #apply up to clean 3
  matrix2 = clean7(matrix)
  #
  col = ncol(matrix2)-12
  if(nrow(matrix2)>=2 && matrix2[2,col]==""){
    matrix = clean6(matrix2)
  } else{
    matrix = matrix2
  }
}

#combine through 8 and loop 6
clean9 <- function(matrix){
  #apply up to clean 3
  matrix2 = clean8(matrix)
  #
  col = ncol(matrix2)-12
  if(nrow(matrix2)>=3 && matrix2[3,col]==""){
    matrix = clean6(matrix2)
  } else{
    matrix = matrix2
  }
}


#combine through 9 + last empty row
clean10 <- function(matrix){
  #apply up to clean 3
  matrix2 = clean9(matrix)
  #
  col = ncol(matrix2)-12
  if(nrow(matrix2)>=4 && matrix2[4,col]==""){
    matrix = clean6(matrix2)
  } else{
    matrix = matrix2
  }
}

#strip NA rows at the end
clean11 <- function(matrix){
  col = ncol(matrix)
  if(is.na(matrix[1,col])){matrix = matrix[,-col]} 
  else{matrix=matrix}
}



#Create loop that reads in data, cleans it down to 13 columns, and then appends it
fdi_types <- c("stock")

for(df_type in fdi_types){




# read in list to loop over
list_code <- read.csv("country_codes.csv", stringsAsFactors=FALSE)


appended_m <- data.frame("Origin", "2001", "2002", "2003", "2004", 
                         "2005", "2006", "2007", "2008", "2009", "2010", 
                         "2011", "2012", "destination")

names(appended_m) <- c("Origin", "X2001", "X2002", "X2003", "X2004", "X2005", 
                       "X2006", "X2007", "X2008", "X2009", "X2010", "X2011", "X2012"
                       , "destination")

#names(debug_list) <- c()

#debug_list <- matrix(nrow=205,ncol=2)
n=0
for(code in list_code[,3]){
  #create file name
  n = n+1
  folder = paste0("raw_",df_type,"/")
  file_1 <- paste(folder, code, ".csv", sep = "")
  #read in file and clean it
  a <- read.csv(file_1, stringsAsFactors=FALSE)
  c <- clean10(a)
  c <- clean11(c)
  c <- clean11(c)
  c <- clean11(c)
  c <- clean11(c) 
  for(i in 2:13){
    c[,i] <-as.character(c[,i])
  }
  #rename variables
  names(c) <- c("Origin", "X2001", "X2002", "X2003", "X2004", "X2005", 
                "X2006", "X2007", "X2008", "X2009", "X2010", "X2011", "X2012")
  #List destination
  c$destination = code
  #Append together
  appended_m = rbind(appended_m, c, stringsAsFactors=FALSE)
}


appended_fdi = appended_m


#get rid of empty rows and duplicate years
appended_fdi <- subset(appended_fdi, X2001 != "")
appended_fdi <- subset(appended_fdi, X2001 != "2001")

# get rid of aggregates
appended_fdi <- subset(appended_fdi, 
                       #Origin != "World" &                                   
                         Origin != "Europe"&                             
                         Origin != "North America"&                 
                         Origin != "Other developed countries"&               
                         Origin != "Developing economies" &              
                         Origin != "South Asia"            &                            
                         Origin != "West Asia"              &            
                         Origin != "Other developed Europe"  &       
                         Origin != "Asia"              &
                         Origin != "Caribbean"          &            
                         Origin != "European Union"      &     
                         Origin != "Developed economies"  &
                         Origin != "Africa"&
                         Origin != "East Asia" &
                         Origin != "South-East Asia" &
                         Origin != "South America"&
                         Origin != "Unspecified" &
                         Origin != ".." &
                         Origin != "Transition economies"&
                         Origin != "International organizations"&
                         Origin != "Other Africa" &
                         Origin != "Latin America and the Caribbean"&
                         Origin != "North Africa" &
                         Origin != "South-East Europe" &
                         Origin != "Oceania" &
                         Origin != "Central America"
)


#rehape data
fdi <- reshape(appended_fdi, direction="long", varying=list(names(appended_fdi)[2:13]), v.names="Value", 
               idvar=c("Origin","destination"), timevar="Year", times=2001:2012)
fdi <- fdi[order(fdi$destination, fdi$Year),] 

#convert strings to values
fdi$Value  <- gsub(" ", "", fdi$Value)
fdi$Value  <- gsub("--", "-", fdi$Value)
fdi$Value <- ifelse(fdi$Value=="..", 999999, fdi$Value)
fdi$Value <- as.numeric(fdi$Value)
fdi$Value <- ifelse(is.na(fdi$Value), 0 , fdi$Value)
fdi$Value <- ifelse(fdi$Value==999999, NA, fdi$Value)

# Country Codes
ccodes <- read.csv("country_codes.csv", stringsAsFactors=FALSE, header=FALSE)
ccodes <- ccodes[,-4]
ccodes <- ccodes[,-2]


# merge to get Destination names
fdi <- merge(fdi, ccodes, by.x = "Origin", by.y = "V1", all.x =TRUE)
fdi <- merge(fdi, ccodes, by.x = "destination", by.y = "V3", all.x =TRUE)


#reorder and name variables
fdi <- fdi[,c(6, 1,2,5,3,4)]
names(fdi) <- c("Destination","Dest.Code","Origin", "Origin.Code","Year","Value") 
fdi <- fdi[order(fdi$Destination, fdi$Year),] 

fdi$Origin.Code <- ifelse(fdi$Origin=="United States" , "USA"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Hong Kong, China" , "HKG"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Syrian Arab Republic" , "SYR"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Venezuela, Bolivarian Rep. of" , "VEN"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Taiwan Province of China" , "TWN"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Monaco" , "MCO"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Andorra" , "AND"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Netherlands Antilles " , "ANT"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Gibraltar" , "GIB"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Liechtenstein" , "LIE"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Bolivia, Plurinational State of" , "BOL"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Jersey" , "JEY"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="US Virgin Islands" , "VIR"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="United Rep. of Tanzania" , "TZA"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="The FYR of Macedonia" , "MKD"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Isle of Man" , "IMN"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Guam" , "GUM"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Congo" , "COG"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Congo, Democratic Rep. of" , "COD"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="C\xf4te d' Ivoire" , "CIV"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Serbia and Montenegro" , "SRB"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Guernsey" , "GGY"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Macao, China" , "MAC"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Moldova, Republic of" , "MDA"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Uzbekistan" , "UZB"  ,fdi$Origin.Code)
fdi$Origin.Code <- ifelse(fdi$Origin=="Lao People's Dem. Rep." , "LAO"  ,fdi$Origin.Code)

#write csv
write.csv(fdi, file = paste0("clean_",df_type,".csv"))

}
