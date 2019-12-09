# clear workspace, set seed, and set wd
rm(list=ls())

set.seed(19)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


### Read in data
load("../../main/main_cov.Rdata")
load("../../main/main_net.Rdata")

#set DV expansion
e <- 2
i = 1
list_of_networks=netlist
list_of_edgecovs=covlist
ergm_call=ergm.call
ergm_call_no_offset=ergm.call.no.offset

ergmPooledMeta <- function(list_of_networks,ergm_call,ergm_call_no_offset,list_of_edgecovs=NULL,seed=1234,ncores=11){
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
    edge_vals <- net %e% "Value"
    net %e% "tValue" <- ceiling(log(edge_vals)*e)
    if(length(list_of_networks)==length(list_of_edgecovs)){
      edgecovars <- list_of_edgecovs[[i]]
      oecd_both <- edgecovars[["OECD_both"]]
      not_oecd_both <- 1-oecd_both
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
    edge_vals <- net %e% "Value"
    net %e% "tValue" <- ceiling(log(edge_vals)*e)
    if(length(list_of_networks)==length(list_of_edgecovs)){
      edgecovars <- list_of_edgecovs[[i]]
      oecd_both <- edgecovars[["OECD_both"]]
      not_oecd_both <- 1-oecd_both
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


# the main ergm call is a complete call to ERGM wrapped in the expression() function. The differences for use in
# the ergmPooledMeta function are two-fold. First, all terms beside the edges term are wrapped in the 'offset()'
# function. Second, offset.coef is defined as pooled_par. These two differences are necessary for estimating the
# partially pooled models.
# note that edge covariates should be specified as edgecov(edgecovars[["nameOfMatrix"]])


ergm.call <- expression(ergm(net ~ sum + offset(sum(pow=1/2))+ offset(nonzero) +
                               offset(edgecovmutual(oecd_both))+ offset(edgecovmutual(not_oecd_both))+
                               offset(transitiveweights("min", "max", "min"))+
                               offset(nodeicov("polity"))+offset(nodeocov("polity"))+ 
                               offset(nodeicov("trade_opennes"))+offset(nodeocov("trade_opennes"))+
                               offset(nodeicov("gdp.pc"))+offset(nodeocov("gdp.pc"))+
                               offset(nodematch("OECD_mem"))+
                               offset(edgecov(edgecovars[[1]]))+offset(edgecov(edgecovars[[2]]))+
                               offset(edgecov(edgecovars[[3]]))+offset(edgecov(edgecovars[[4]]))+
                               offset(edgecov(edgecovars[[5]]))+offset(edgecov(edgecovars[[6]]))+
                               offset(edgecov(edgecovars[[7]]))+offset(edgecov(edgecovars[[9]])),
                             offset.coef=pooled_par,
                             response="tValue",
                             reference=~Poisson,
                             control=control.ergm(MCMLE.trustregion=100,
                                                  MCMLE.maxit=50, 
                                                  MCMC.samplesize=10000,
                                                  MCMC.burnin=500,
                                                  MCMC.interval=1000)
))



# the no offset call strips the ERGM call of the offset() function and the offset.coef argument
# terms must match and be in the same order
ergm.call.no.offset <- expression(ergm(net ~ sum+ sum(1/2) +nonzero+
                                         edgecovmutual(oecd_both)+ edgecovmutual(not_oecd_both)+
                                         transitiveweights("min", "max", "min")+
                                         nodeicov("polity")+nodeocov("polity")+
                                         nodeicov("trade_opennes")+nodeocov("trade_opennes")+
                                         nodeicov("gdp.pc")+nodeocov("gdp.pc")+
                                         nodematch("OECD_mem")+
                                         edgecov(edgecovars[[1]])+edgecov(edgecovars[[2]])+
                                         edgecov(edgecovars[[3]])+edgecov(edgecovars[[4]])+
                                         edgecov(edgecovars[[5]])+edgecov(edgecovars[[6]])+
                                         edgecov(edgecovars[[7]])+edgecov(edgecovars[[9]]),
                                       response="tValue",
                                       reference=~Poisson,
                                       control=control.ergm(MCMLE.trustregion=100,
                                                            MCMLE.maxit=50, 
                                                            MCMC.samplesize=10000,
                                                            MCMC.burnin=500,
                                                            MCMC.interval=100)
))

# the partially pooled ergm conducts a two-step meta analysis
# in the first step, coefficients are estimated for each network individually
# in the second step, all effects are fixed at the fixed effects meta analysis estimate, and only the
# intercepts are estimated for each network
# The results include the pooled coefficients (note network-specific intercepts are at the end), SEs, AIC,
# and the ergm opjects from the second stage of pooled estimation
# MCMC diagnostics can be run on the second-stage ERGM results.
pooledERGMres <- ergmPooledMeta(list_of_networks=netlist,ergm_call=ergm.call,ergm_call_no_offset=ergm.call.no.offset,list_of_edgecovs=covlist)

## save this model
save(pooledERGMres, file = "pooledERGM.Rdata")