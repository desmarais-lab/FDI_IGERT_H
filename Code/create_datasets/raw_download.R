#=============================================================#
# Work Done: Call UNCTAD website for FDI data
#=============================================================#

#Load Packages
library(gdata)

# choose stock or flows in function call
# sheet_num = 1 for flows, 3 for stocks

#create function to create API
get.FDI <- function(url="http://unctad.org/Sections/dite_fdistat/docs/webdiaeia2014d3_"
                         ,code = "", file_ext= ".xls", sheet_num=3)
{
  #create address
  string <- paste0(url,code,file_ext)
  if(sheet_num == 3){
  #read in file
  fdi_file <- read.xls(string,header=TRUE, sheet=sheet_num)
  #write file to folder
  folder = "raw_stock/"
  file_1 <- paste0(folder, code, ".csv")
  write.csv(fdi_file, file = file_1)
  }
  else if(sheet_num == 1){
    #read in file
    fdi_file <- read.xls(string,header=TRUE, sheet=sheet_num)
    #write file to folder
    folder = "raw_flows/"
    file_1 <- paste0(folder, code, ".csv")
    write.csv(fdi_file, file = file_1)
  }
}


# Read in country list
codes <- read.csv("country_codes.csv",header=TRUE, sep=",", stringsAsFactors = F)

#loop downloading stocks
for(ab in codes[,3]){
  # sheet_num = 1 for flows, 3 for stocks
  get.FDI(code=ab, sheet_num=3)
  
  #Sys.sleep(10)
  
}

#loop downloading flows
for(ab in codes[,3]){
  # sheet_num = 1 for flows, 3 for stocks
  get.FDI(code=ab, sheet_num=1)
  
  #Sys.sleep(10)
  
}