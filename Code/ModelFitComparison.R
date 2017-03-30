# Code to plot BIC of model with and without network effects

library(ergm)

# Get file names for ERGM results
modelFiles <- dir("./Code/models_tweight")
modelFiles <- modelFiles[which(grepl("rda",modelFiles))]

# Matrix to store results
BICMat <- matrix(0,length(modelFiles)/2,2)

# Row labels for fit matrix

cols <- 1+grepl("model2",modelFiles)


rowLabels <- sort(unique(substr(modelFiles,6+cols,7+cols)))
rownames(BICMat) <- rowLabels
colnames(BICMat) <- c("Independent","Network")

# Loop 
for(i in 1:length(modelFiles)){
	
	# Create environment in which to load results
	fiti <- new.env()
	
	# load the fit in the ith model file
	load(paste("./Code/models_tweight/",modelFiles[i],sep=""),envir=fiti)
    
    # Identify whether there are network effects based on file name
    col <- 1+grepl("model2",modelFiles[i])
	
	# Extract the year from the file name
	yri <- substr(modelFiles,6+col,7+col)[i]
	
	# Store results
	BICMat[match(yri,rowLabels),col] <- BIC(fiti[[ls(fiti)]])
}

pdf("./Draft/draft_figures/BICdiff.pdf",height=4,width=6.5)
par(las=1)
barplot(BICMat[,1]-BICMat[,2],ylab="BIC Difference (Ind-Net)",xlab="Year")
dev.off()



