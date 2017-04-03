# Identify path for working directory
wdpath <- "/users/brucedesmarais/Dropbox/professional/Teaching/ICPSR/ICPSRNetworkAnalysis/Lectures/wk1_1ERGMinDepth/"

# Set the working directory
setwd(wdpath)

# Read in network
el <- read.csv("senEL.csv",stringsAsFactors=F)

# Read in vertex data
dwnom <- read.csv("dwnom1.csv",stringsAsFactors=F)

# Create the network
net <- network.initialize(nrow(dwnom))
network.vertex.names(net) <- dwnom$labs

# Add in edges
net[as.matrix(el)] <- 1

# Define vertex attribute
set.vertex.attribute(net,"dwnom",dwnom$dwnom)

# Read in ERGM library
library(ergm)

# ergm with simple ideological homophily
est1 <- ergm(net~edges+absdiff("dwnom"))
summary(est1)

# add popularity effect
est2 <- ergm(net~edges+absdiff("dwnom")+istar(2))
# Check for degeneracy
mcmc.diagnostics(est2)
summary(est2)

# add classic transitivity effect
est3 <- ergm(net~edges+absdiff("dwnom")+istar(2)+triangles)
# Check for degeneracy
mcmc.diagnostics(est3)
summary(est3)

# try conservative GWESP
est4 <- ergm(net~edges+absdiff("dwnom")+istar(2)+gwesp(.1,fixed=T))
# Check for degeneracy
mcmc.diagnostics(est4)
summary(est4)
