#=============================================================#
# John Schoeneman
# Work Done For: R GIS
# Date: August 2016
# Work Done: Clean FDI data
# Machine: MacPro OSX Yosemite
#=============================================================#

# clear workspace
rm(list=ls())


setwd("/Users/johnpschoeneman/Dropbox/FA16 - RA - ZHU")


#/Users/johnpschoeneman/Documents/school/Penn State/RA:TA/FA16 - RA - ZHU/FDI Raw Data



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

# read in list to loop over
list_code <- read.csv("country_codes.csv", stringsAsFactors=FALSE)


appended_m <- data.frame("Origin", "2001", "2002", "2003", "2004", 
                         "2005", "2006", "2007", "2008", "2009", "2010", 
                         "2011", "2012", "destination")
names(appended_m) <- c("Origin", "2001", "2002", "2003", "2004", 
                       "2005", "2006", "2007", "2008", "2009", 
                       "2010", "2011", "2012", "destination")

#names(debug_list) <- c()

debug_list <- matrix(nrow=205,ncol=2)
n=0
for(code in list_code[,3]){
  #create file name
  n = n+1
  folder = "FDI Raw Data/"
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
  names(c) <- c("Origin", "2001", "2002", "2003", "2004", "2005", 
                "2006", "2007", "2008", "2009", "2010", "2011", "2012")
  #List destination
  c$destination = code
  #Append together
  appended_m = rbind(appended_m, c, stringsAsFactors=FALSE)
}




