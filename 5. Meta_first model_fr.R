library(RSiena)
library(tidyverse)
setwd("mypath")
classrooms <- c(12100,14100,21200,23100,31100,31200,43200,55200,57100,63100,67100)

makeData <- function(i){ # by default, dolby = T
  
  load(paste0('netDynamics_fr_gp',i,'.Rdata')) # load data
  mydata <- netDynamics_fr
  themodel <- getEffects(netDynamics_fr) # default
  
  # structural effects
  themodel <- includeEffects(themodel, transTrip)
  themodel <- includeEffects(themodel, inPopSqrt, outActSqrt)
  
  # gender
  themodel <- includeEffects(themodel, name = "friendship", altX, egoX, sameX, interaction1= "gender")
  # ethnicity
  themodel <- includeEffects(themodel, name = "friendship", sameX, interaction1= "roma")
  
  # Algorithms:
  algo0 <- sienaAlgorithmCreate(projname = paste('model1_', i, sep=""), n3=5000, seed=1234)
  # "continue the estimation, using prevAns,with nsub=1; a high value for n3, e.g., 3000 or 5000; and a high value for n2start."
  # The default n2s is 2.52×(p+7), where p is the number of estimated parameters, divided
  # by nbrNodes, if multiple processes are used.
  #  For a ‘high value’, start with a value that is about twice as large(*2)
  p <- length(themodel$effectName)
  n2s <- ((2.52)*(p+7))*2
  n2s <- round(n2s)
  
  algo1 <- sienaAlgorithmCreate(projname = paste('model1_', i, sep=""),
                                nsub=1, n2start = n2s, n3=5000, seed=1234)
  
  # output is a list including the data, the effects, and two algorithms
  list(Data=mydata, Eff=themodel, algo0=algo0, algo1=algo1)
}

# utility function: either reach convergence or stop if reaching convergence is impossible
siena07ToConvergence <- function(alg0, alg1, dat, eff, ans0=NULL, ...){
  numr <- 0
  ans <- siena07(alg0, data=dat, effects=eff, prevAns=ans0, ...) # the first run
  repeat {
    save(ans, file=paste("ans",numr,".RData",sep="")) # to be safe
    numr <- numr+1 # count number of repeated runs
    tm <- ans$tconv.max # convergence indicator
    cat(numr, tm,"\n") # report how far we are
    if (tm < 0.25) {break} # success
    if (tm > 10) {break} # divergence without much hope
                         # of returning to good parameter values
    if (numr > 10) {break} # now it has lasted too long
    if (tm > 0.5){
      ans <- siena07(alg0, data=dat, effects=eff, prevAns=ans, ...)
    } else {
      ans <- siena07(alg1, data=dat, effects=eff, prevAns=ans, ...)
    }
  }
  if (tm > 0.25)
  {
    cat(    "Warning: convergence inadequate.\n")
  }
  ans
}

analyseData <- function(i){
  cat(' class ',i,'* ') # show progress, which classroom
  sink('temp.txt') # unclutter screen
  mod <- makeData(i) # list with Sienadata, effects, 2 algorithms
  
  sink() # unclutter screen
  ModEstimates <- siena07ToConvergence(mod$algo0, mod$algo1,
                                       dat = mod$Data, eff = mod$Eff)
  
  # add a group number to the object:
  ModEstimates$groupNumber <- i
  ModEstimates # results
}


resultsList0 <- list()

for (i in classrooms){
  i0 <- length(resultsList0) + 1
  resultsList0[[i0]] <- analyseData(i)
  flush.console()
}
sink()  # perhaps still necessary

# this took a lot of work: save results:
setwd("mypath")
save(resultsList0, file='firstResults0.RData')

# get our saved result
setwd("mypath")
load("firstResults0.RData")
# Inspect quality of convergence
hist(sapply(resultsList0, function(x){x$tconv.max}), xlab='tconv.max', main='')
max(sapply(resultsList0, function(x){x$tconv.max})) # ~0.20

# Look at a table of parameter estimates, and a table of standard errors,
# for all the groups.


# Extract parameter estimates and standard errors
parameters <-  sapply(resultsList0, function(x){x$theta})
dim(parameters)
# Check that you understand what you have here.
standers <- sapply(resultsList0, function(x){x$se})
(eff.names <-
    resultsList0[[1]]$effects[resultsList0[[1]]$effects$include,'effectName'])
# The effects are the same for all groups,
# so we can take the effect names from the first group.
rownames(parameters) <- eff.names
rownames(standers) <- eff.names
colnames(parameters) <- sapply(resultsList0, function(x){x$groupNumber})
colnames(standers) <- sapply(resultsList0, function(x){x$groupNumber})
# print rounded to 2 decimals so that on a wide console everything can fit.
round(parameters,2)
round(standers,2)
# Problem to be fixed if exist: estimates and standard errors both being large and often in divergence of the algorithm. 
# Fix this parameter to some large value. 
# (Note: large here means, e.g., more than 5 or less than -5;depending on the effect, of course.)
# (RSiena_Manual P77)

# But, here the estimate and standard error looks fine. Let's continue.

# Make some funnel plots:
funnelPlot(resultsList0, 1)
funnelPlot(resultsList0, 2)
funnelPlot(resultsList0, 3)
funnelPlot(resultsList0, 4)
funnelPlot(resultsList0, 5)
funnelPlot(resultsList0, 6)
funnelPlot(resultsList0, 7)
funnelPlot(resultsList0, 8)
funnelPlot(resultsList0, 9)
funnelPlot(resultsList0, 10)

# save
model1_par1 <- round(parameters,2)
model1_se1 <- round(standers,2)
save(model1_par1, file="model1_par1.Rdata")
save(model1_se1, file="model1_se1.Rdata")

# check small variance? #
(resultsList0[[1]]$sd)^2 <0.1
(resultsList0[[2]]$sd)^2 <0.1
(resultsList0[[3]]$sd)^2 <0.1
(resultsList0[[4]]$sd)^2 <0.1
(resultsList0[[5]]$sd)^2 <0.1
(resultsList0[[6]]$sd)^2 <0.1
(resultsList0[[7]]$sd)^2 <0.1
(resultsList0[[8]]$sd)^2 <0.1
(resultsList0[[9]]$sd)^2 <0.1
(resultsList0[[10]]$sd)^2 <0.1
# all false


## Goodness of Fit
# GeodesicDistribution calculates the distribution of non-directed
# geodesic distances; see ?sna::geodist
# The default for \code{levls} reflects the usual phenomenon
# that geodesic distances larger than 5
# do not differ appreciably with respect to interpretation.
# Note that the levels of the result are named;
# these names are used in the \code{plot} method.
GeodesicDistribution <- function (i, data, sims, period, groupName,
                                  varName, levls=c(1:5,Inf), cumulative=TRUE, ...) {
  x <- networkExtraction(i, data, sims, period, groupName, varName)
  require(sna)
  a <- sna::geodist(symmetrize(x))$gdist
  if (cumulative)
  {
    gdi <- sapply(levls, function(i){ sum(a<=i) })
  }
  else
  {
    gdi <- sapply(levls, function(i){ sum(a==i) })
  }
  names(gdi) <- as.character(levls)
  gdi
}





# make a list of graphs and of the p-values of the gof estimation
GOF <- list()
GOFP <- list()
classrooms <- c(12100,14100,21200,23100,31100,31200,43200,55200,57100,63100,67100)
# specify the options for the simulation algorithm:
simcontrols <- sienaAlgorithmCreate(n3=250,nsub=0,seed=12345)
# nsub=0 : phase 2 (parameter estimation) will be skipped
# n3=250 : in phase 3, sample 250 independent network evolution processes
for(i in 1:length(resultsList0)){
  
  print(i) # check progress
  
  a <- classrooms[i]
  load(paste0('mypath/netDynamics_fr_gp',a,'.Rdata')) # load data
  mydata <- netDynamics_fr
  themodel <- getEffects(netDynamics_fr)
  themodel <- includeEffects(themodel, transTrip)
  themodel <- includeEffects(themodel, inPopSqrt, outActSqrt)
  themodel <- includeEffects(themodel, name = "friendship", altX, egoX, sameX, interaction1= "gender")
  themodel <- includeEffects(themodel, name = "friendship", sameX, interaction1= "roma")
  
  
  # add simulated networks to model results:
  thesims <- siena07(simcontrols, data = mydata, effects = themodel,
                returnDeps=TRUE, # simulated networks at ends of observation period made available
                prevAns=resultsList0[[i]]) # use estimates from "theresults" for simulations
  
  gof1.indegrees <- sienaGOF(thesims,IndegreeDistribution,
                             varName = "friendship",cumulative = FALSE,levls=0:10)
  gof1.outdegrees <- sienaGOF(thesims,OutdegreeDistribution,
                              varName = "friendship",cumulative = FALSE,levls=0:10)
  gof1.triads <- sienaGOF(thesims,TriadCensus,varName = "friendship",verbose=TRUE)
  gof1.geodesic <- sienaGOF(thesims,GeodesicDistribution,
                            varName = "friendship",cumulative = FALSE)
  # save p-values
  P <- c(gof1.indegrees$Joint$p,
         gof1.outdegrees$Joint$p,
         gof1.triads$Joint$p,
         gof1.geodesic$Joint$p)
  
  GOFP[[i]] <- P
  
  goflist <- list(gof1.indegrees, gof1.outdegrees, gof1.triads, gof1.geodesic)
  GOF[[i]] <- goflist
}

# save
save(GOFP,file='GOFP1.Rdata')
save(GOF,file='GOF1.Rdata')
# goodness of fit? control p is not <0.05
GOFP # gof1.triads of the last classroom is 0.  


################################

## result table
results.table1 <- list()
for(i in 1:length(resultsList0)){
  
  parameter <- resultsList0[[i]]$effects$effectName
  type <- resultsList0[[i]]$effects$type
  estimate <- resultsList0[[i]]$theta
  st.error <- sqrt(diag(resultsList0[[i]]$covtheta))
  normal.variate <- estimate/st.error
  p.value.2sided <- 2*pnorm(abs(normal.variate),lower.tail = FALSE)
  
  results.table1[[i]] <- data.frame(parameter,
                                    estimate = round(estimate,3),
                                    st.error = round(st.error,3),
                                    normal.variate = round(normal.variate,2),
                                    p.value = round(p.value.2sided,4))
}

# save the results
save(results.table1, file="results.table1.Rdata")

# Meta-analysis using siena08
meta_fr_1 <- siena08(resultsList0,  projname = "meta_fr")
meta_fr_1
save(meta_fr_1, file='meta_fr_1.Rdata')
# Look at the order of the list elements of which meta_fr_1 is composed:
names(meta_fr_1)
meta.table(meta_fr_1, filename="meta08_1.2.tex", option=2)
meta.table(meta_fr_1, filename="meta08_1.tex", option=1)
?meta.table

summary(meta_fr_1)


# aree the SE's RANDOM?
plot(meta_fr_1, which = 1:length(meta_fr_1$theta),
     useBound=TRUE)

neff <- sum(resultsList0[[1]]$effects$include)
parameters.08 <- t(sapply(1:neff, function(i){c(meta_fr_1[[i]]$mu.ml,
                                                meta_fr_1[[i]]$mu.ml.se, meta_fr_1[[i]]$mu.confint,
                                                meta_fr_1[[i]]$sigma.ml, meta_fr_1[[i]]$sigma.confint,
                                                meta_fr_1[[i]]$n1)}))

colnames(parameters.08) <- c('mu-hat', 'mu-se',
                             'mu-min', 'mu-plus', 'alpha_mu',
                             'sigma-hat', 'sigma-min', 'sigma-plus', 'alpha_sigma', 'N')
# These are: parameter estimate for population mean; s.e.;
# confidence interval for population mean (left, right, significance level);
# estimate for population standard deviation;
# confidence interval for standard deviation (left, right, significance level);
# number of groups on which this is based.

round(parameters.08, 3)
save(parameters.08,file = 'Meta.Par1.Rdata')



# Construct the names for the effects:
efnames <- names(meta_fr_1)[1:neff]

# Extract heterogeneity tests from siena08 output;
# these are called Q in Snijders & Baerveldt (2003):
hetero.08 <- t(sapply(1:neff, function(i){c(meta_fr_1[[i]]$Qstat,
                                            meta_fr_1[[i]]$n1-1, meta_fr_1[[i]]$pttilde)}))
colnames(hetero.08) <- c('Q', 'df', 'pQ')
# These are: Q statistic; degrees of freedom; p-value
rownames(hetero.08) <- efnames
round(hetero.08, 3)

# Extract overall tests;
Overalls.08 <- t(sapply(1:neff, function(i){c(meta_fr_1[[i]]$Tsq,
                                              meta_fr_1[[i]]$n1-1, meta_fr_1[[i]]$pTsq)}))
colnames(Overalls.08) <- c('T^2', 'df', 'pT^2')

# These are: T^2 statistic; degrees of freedom; p-value
rownames(Overalls.08) <- efnames
round(Overalls.08, 3)

save(Overalls.08, file='overalls08.1.Rdata')


# We also present the Fisher combinations:
Fishers <- t(sapply(1:neff,
                    function(i){c(meta_fr_1[[i]]$cjplus, meta_fr_1[[i]]$cjminus, meta_fr_1[[i]]$cjplusp, meta_fr_1[[i]]$cjminusp, 2*meta_fr_1[[i]]$n1 )}))
colnames(Fishers) <- c('Fplus', 'Fminus', 'pplus', 'pminus', 'df')
rownames(Fishers) <- efnames
round(Fishers,3)

save(Fishers, file = 'Fishers1.Rdata')





