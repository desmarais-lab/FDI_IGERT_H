ergmPooledMeta <- function(list_of_networks,ergm_call,ergm_call_no_offset,list_of_edgecovs=NULL,seed=1234,ncores=2){
    require(ergm)
    require(ergm.count)
    require(doParallel)
    require(foreach)
    
    # ergm_call should be an expression
    
    # time fixed effects are required
    
    # formula should include all pooled terms in offset()
    
    # network should be called 'net' in the formula
    
    # edgecovs should be a T x Number_of_edgecovs length named list of matrices
    # , where names(list_of_edgecovs[[t]])[i] is the matrix name used in the
    # model formula
    
    ## Doing individual ergm estimation for pooling
    registerDoParallel(cores=ncores)
    ergms <- foreach(i=1:length(list_of_networks)) %dopar% {
        net <- list_of_networks[[i]]
        if(length(list_of_networks)==length(list_of_edgecovs)){
         edgecovars <- list_of_edgecovs[[i]]
        }
        set.seed(seed)
        eval(ergm_call_no_offset)
    }
    stopImplicitCluster()
    
    print("finished stage 1")
    
    coefMat <- NULL
    weightMat <- NULL
    meta.coef <- 0
    varDenom <- 0
    for(i in 1:length(list_of_networks)){
        if(length(list_of_networks)==length(list_of_edgecovs)){
            edgecovars <- list_of_edgecovs[[i]]
        }
        coefMat <- rbind(coefMat,coef(ergms[[i]]))
        weightMat <- rbind(weightMat,1/(summary(ergms[[i]])$asyse)^2)
        varDenom <- varDenom + weightMat[i,]
        meta.coef <- meta.coef + coefMat[i,]*weightMat[i,]
    }
    meta.coef <- meta.coef/varDenom
    
    SE <- sqrt(1/varDenom)
    
    pooled_par <- meta.coef[-1]
    
    registerDoParallel(cores=ncores)
    ergms.pooled <- foreach(i=1:length(list_of_networks)) %dopar% {
        net <- list_of_networks[[i]]
        if(length(list_of_networks)==length(list_of_edgecovs)){
            edgecovars <- list_of_edgecovs[[i]]
        }
        set.seed(seed)
        eval(ergm_call)
    }
    stopImplicitCluster()
    
    print("finished stage 2")
    
    coefs <- meta.coef[-1]
    SE  <- SE[-1]
    loglik <- 0
    for(i in 1:length(ergms.pooled)){
        if(length(list_of_networks)==length(list_of_edgecovs)){
            edgecovars <- list_of_edgecovs[[i]]
        }
        loglik <- loglik + as.numeric(logLik(ergms.pooled[[i]]))
        coefs <- c(coefs,coef(ergms.pooled[[i]])[1])
        SE <- c(SE,summary(ergms.pooled[[i]])$asyse[1])
    }
    
    AIC <- 2*length(coefs)-2*loglik
    
    list(estimate=coefs,SE=SE,AIC=AIC,pooled.ergms=ergms.pooled)
    
}


### Illustrative example
library(ergm)

### Read in data from ERGM package
data(florentine)

# Create a fake node attribute for illustration
wealthT1 <- c(10,36,27,146,55,44,20,8,42,103,48,49,10,48,32,3)

# Create another fake node attribute for illustration
statusT1 <- sample(1:4,length(wealthT1),rep=T)

# create a dyadic matrix for edgecov
diffWealthT1 <- as.matrix(dist(cbind(wealthT1),diag=T,upper=T))

# create a dyadic matrix for edgecov
diffStatusT1 <- as.matrix(dist(cbind(statusT1),diag=T,upper=T))

# add some random noise for time 2
wealthT2 <- c(10,36,27,146,55,44,20,8,42,103,48,49,10,48,32,3) + rnorm(length(wealthT1))

# add some random noise for time 2
statusT2 <- sample(1:4,length(wealthT1),rep=T) + rnorm(length(wealthT1))

# re-construct the two dyadic covariates
diffWealthT2 <- as.matrix(dist(cbind(wealthT2),diag=T,upper=T))
diffStatusT2 <- as.matrix(dist(cbind(statusT2),diag=T,upper=T))

# Create an empty list in which to store the networks
netlist <- list()

# use flomarriage as network in both years
netlist[[1]] <- flomarriage
netlist[[2]] <- flomarriage

# change year two network by one edge
netlist[[2]][4,3] <- 1
netlist[[2]][3,4] <- 1

# add all the vertex attributes to the networks
netlist[[1]] %v% "wealth" <- wealthT1
netlist[[2]] %v% "wealth" <- wealthT2
netlist[[1]] %v% "status" <- statusT1
netlist[[2]] %v% "status" <- statusT2

# create a list of the dyadic covariate matrices
# note that there is a list of matrices for each time point
# in the list for each time point, the covariates have the same names---the names by which they are called in the ergm formula
covlist <- list(list(diffWealth=diffWealthT1,diffStatus=diffStatusT1),list(diffWealth=diffWealthT2,diffStatus=diffStatusT2))

# the main ergm call is a complete call to ERGM wrapped in the expression() function. The differences for use in
# the ergmPooledMeta function are two-fold. First, all terms beside the edges term are wrapped in the 'offset()'
# function. Second, offset.coef is defined as pooled_par. These two differences are necessary for estimating the
# partially pooled models.
# note that edge covariates should be specified as edgecov(edgecovars[["nameOfMatrix"]])
ergm.call <- expression(ergm(net ~ edges + offset(esp(0))+offset(nodecov("wealth"))+offset(nodecov("status"))+offset(edgecov(edgecovars[["diffWealth"]]))+offset(edgecov(edgecovars[["diffStatus"]])),offset.coef=pooled_par))

# the no offset call strips the ERGM call of the offset() function and the offset.coef argument
# terms must match and be in the same order
ergm.call.no.offset <- expression(ergm(net ~ edges + esp(0)+nodecov("wealth")+nodecov("status")+edgecov(edgecovars[["diffWealth"]])+edgecov(edgecovars[["diffStatus"]])))

# the partially pooled ergm conducts a two-step meta analysis
# in the first step, coefficients are estimated for each network individually
# in the second step, all effects are fixed at the fixed effects meta analysis estimate, and only the
# intercepts are estimated for each network
# The results include the pooled coefficients (note network-specific intercepts are at the end), SEs, AIC,
# and the ergm opjects from the second stage of pooled estimation
# MCMC diagnostics can be run on the second-stage ERGM results.
pooledERGMres <- ergmPooledMeta(list_of_networks=netlist,ergm_call=ergm.call,ergm_call_no_offset=ergm.call.no.offset,list_of_edgecovs=covlist)

