# Code to plot BIC of model with and without network effects

library(ergm)

# Get file names for ERGM results
modelFiles <- dir("./Code/fdi_models")

# Matrix to store results
BICMat <- matrix(0,length(modelFiles)/2,2)

# Row labels for fit matrix
rowLabels <- sort(unique(substr(modelFiles,7,8)))
rownames(BICMat) <- rowLabels
colnames(BICMat) <- c("Independent","Network")

# Loop 
for(i in 1:length(modelFiles)){
	
	# Create environment in which to load results
	fiti <- new.env()
	
	# load the fit in the ith model file
	load(paste("./Code/fdi_models/",modelFiles[i],sep=""),envir=fiti)
	
	# Extract the year from the file name
	yri <- substr(modelFiles,7,8)[i]
	
	# Identify whether there are network effects based on file name
	col <- 2-1*(nchar(modelFiles[i])==15)
	
	# Store results
	BICMat[match(yri,rowLabels),col] <- BIC(fiti[[ls(fiti)]])
}



