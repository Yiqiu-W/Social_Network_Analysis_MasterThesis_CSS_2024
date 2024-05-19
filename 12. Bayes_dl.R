library(multiSiena)
library(parallel)
library(tidyverse)
library(kableExtra)
setwd("mypath")

# select groups for analysis
class <- c(12100,14100,21200,23100,31100,31200,43200,55200,57100,63100,67100)

# create a list containing network objects
c <- paste0('mypath/netDynamics_dl_gp',class,'.Rdata')
objectlist <- list()
t <- 0
for(i in c){
  t <- t+1
  load(i)
  objectlist[[t]] <- netDynamics_dl
}

# create multigroup object
(multigroup <- sienaGroupCreate(objectlist))
multigroup[[1]]
# how many students in each classroom
group_n <- sapply(multigroup, function(x){length(x$nodeSets[[1]])})
group_n

# Get the initial description
print01Report(multigroup, modelname = 'dl_gp_11classroom')
# save
save(multigroup, file='multigroup_11class_dl.RData')

################################################################################
### Defining the model
################################################################################

# Construct the effects object; first without random effects.
GroupEffects <- getEffects(multigroup)
# Three ways to report what this is:
GroupEffects
print(GroupEffects, includeRandoms=TRUE)
print(GroupEffects, includeRandoms=TRUE, dropRates=TRUE)
# Note the randomEffects column.
# randomEffects for outdegree(density) is TRUE

# Construct a basic algorithm object.
GroupsAlgo <- sienaAlgorithmCreate(projname = 'dl_gp_11classroom',
                                   mult=5, seed=12345)
# mult=5 is the default.

# detect for use of multiple cores during run
n.clus <- detectCores() - 1 # 7
# then n.clus is the largest reasonable number to use, provided
# you are the only person using this machine during the operation of RSienaTest.
# Specify:
GroupEffects <- getEffects(multigroup)
# effectsDocumentation(GroupEffects)
GroupEffects <- includeEffects(GroupEffects, transTrip)
GroupEffects <- includeEffects(GroupEffects, inPopSqrt)
GroupEffects <- includeEffects(GroupEffects, outActSqrt)
# gender
GroupEffects <- includeEffects(GroupEffects, name = "dislike", altX, interaction1= "gender")
GroupEffects <- includeEffects(GroupEffects, name = "dislike", egoX, interaction1= "gender")
GroupEffects <- includeEffects(GroupEffects, name = "dislike", sameX, interaction1= "gender")
# ethnicity
GroupEffects <- includeEffects(GroupEffects, name = "dislike", sameX, interaction1= "roma")

# gossip perception main effect
GroupEffects <- includeEffects(GroupEffects,name='dislike',
                               X, interaction1='gp')
# gp reciprocity
GroupEffects <- includeEffects(GroupEffects,name='dislike',
                               XRecip, interaction1='gp') 
# shared enermy
GroupEffects <- includeEffects(GroupEffects,name='dislike',
                               OutWWX ,interaction1='gp')
# friend/enermy of the potential gossiper
GroupEffects <- includeEffects(GroupEffects,name='dislike',
                               WXX,interaction1='gp')

print(GroupEffects, includeRandoms=TRUE, dropRates=TRUE)
# Length of priorSigEta for sienaBayes, if used, should be 12.
# Dimensions of priorMu and priorSigma for sienaBayes should be 1 + 3 = 4 .

################################################################################
### First estimate the multi-group model
################################################################################
ans <- siena07(GroupsAlgo, data = multigroup,
               effects = GroupEffects,
               useCluster=TRUE, nbrNodes=n.clus)
ans

tt.ans <- sienaTimeTest(ans)
# TimeTest constructed a null hypothesis with 13 estimated parameters
# and 416 dummy variables to be tested.
# However, there are 4 linear dependencies between these.
# This may be because some of the parameters are already
# interactions with time dummies or other time variables.

# Automatic discovery of dependencies yielded the exclusion of effect  WW=>X shared outgoing gp 
# The effect with number 8()  in ans was excluded automatically because of collinearity.

summary(tt.ans)
# We look at the effect-wise joint significance tests
# the p-values and the chi-square values
# give information about the variability.

GroupEffects <- setEffect(GroupEffects, density, random=TRUE)
GroupEffects <- setEffect(GroupEffects, recip, random=TRUE)
GroupEffects <- setEffect(GroupEffects, transTrip, random=TRUE)
GroupEffects <- setEffect(GroupEffects, inPopSqrt, random=TRUE)
GroupEffects <- setEffect(GroupEffects, outActSqrt, random=TRUE)
GroupEffects <- setEffect(GroupEffects, name = "dislike", sameX, interaction1= "gender", random=TRUE)
GroupEffects <- setEffect(GroupEffects, name = "dislike", sameX, interaction1= "roma", random=TRUE)
GroupEffects <- setEffect(GroupEffects,name='dislike',
                          WXX,interaction1='gp',random=TRUE)
# show effects list
print(GroupEffects, includeRandoms=TRUE, dropRates=F)
# Length of priorSigEta for sienaBayes, if used, should be 5 .
# Dimensions of priorMu and priorSigma for sienaBayes should be 8 + 3 = 11 .

# define prior Mu and variance
# The list and order of only the randomly varying effects can be shown by requesting
GroupEffects[(GroupEffects$randomEffects | (GroupEffects$basicRate & (GroupEffects$group==1) )) & GroupEffects$include, ]

Mu <- rep(0,11)
# In most cases the outdegree parameter is expected to be negative and the reciprocity parameter positive.
# The researcher should consider earlier studies of similar network dynamics; 
# reasonable values for the prior mean for the outdegree parameter might be –2 or –1, 
# and for the reciprocity parameter +1.5 or +2.
# For homophily parameters on important attributes expressed by the simX effect 
# (which is standardized), as long as these are regarded as control effects, 
# one might specify the prior mean conservatively as 0.3 or 0.5. (check manual p131)
Mu[4] <- -2  # outdegree
Mu[5] <- 1 # reciprocity
Mu[10] <- 0.3 # same gender
Mu[11] <- 0.3 # same roma

Mu

Sig <- matrix(0,11,11)
diag(Sig) <- 0.01
Sig

groupalgo_new <- sienaAlgorithmCreate(projname = 'dl_gp_bay', seed = 12345, nsub = 2, firstg=0.05)
#?sienaAlgorithmCreate()
(ans_new <- siena07(groupalgo_new, data=multigroup, effects=GroupEffects, useCluster=TRUE, nbrNodes=n.clus)) 
# Overall maximum convergence ratio:   0.770
# (ans_new <- siena07(groupalgo_new, data=multigroup, effects=GroupEffects, useCluster=TRUE, nbrNodes=n.clus,prevAns =ans_new)) 
save(ans_new, file= 'ans_new_dl.Bayes.Rdata')

testrun <- sienaBayes(data= multigroup, effects = GroupEffects, algo = GroupsAlgo,
                      priorMu = Mu, priorSigma = Sig,
                      priorKappa = 0.01,
                      prevAns = ans_new,
                      nwarm=5, nmain=10, nrunMHBatches=5, nImproveMH=20,
                      nbrNodes=n.clus, silentstart=FALSE) # start with initgainGroupwise = 0.02 default

save(testrun, file = 'Bayes_test_dl.Rdata')
# load("Bayes_test.Rdata")
summary(testrun)
# high rate parameters for some groups???


# complete run

# manual p127
# For normal use, nwarm = 500, nmain = 1000, nrunMHBatches = 20, 
# nImproveMH = 100 may be reasonable. 
# Computing time is roughly proportional to nmain × nrunMHBatches.
Bayes_re <- sienaBayes(data= multigroup, effects = GroupEffects, algo = GroupsAlgo,
                       initgainGlobal=0.1, initgainGroupwise = 0.001,
                       priorMu = Mu, priorSigma = Sig,
                       priorKappa = 0.01,
                       prevAns = ans_new,
                       nwarm=500, nmain=1000, nrunMHBatches=40, nImproveMH=100,
                       nbrNodes=n.clus, silentstart=FALSE) 

save(Bayes_re, file = "Bayes_re.Rdata")

summary(Bayes_re)
source("https://www.stats.ox.ac.uk/~snijders/siena/BayesPlots.r")
NonRateTracePlots(Bayes_re)
RateTracePlots(Bayes_re)
# prolong
Bayes_re.2 <- sienaBayes(data= multigroup, effects = GroupEffects, algo = GroupsAlgo,
                         initgainGlobal=0.1, initgainGroupwise = 0.001,
                         priorMu = Mu, priorSigma = Sig,
                         priorKappa = 0.01,
                         prevBayes = Bayes_re,
                         nwarm=500, nmain=2000, nrunMHBatches=40,nImproveMH=100,
                         nbrNodes=7, silentstart=FALSE)
save(Bayes_re.2, file="Bayes_re.2.RData")

#load("Bayes_re.RData")
#load("Bayes_re.2.RData")


# combine it with the earlier chain:
Bayesgroup <- glueBayes(Bayes_re, Bayes_re.2)
# trace plots
RateTracePlots(Bayesgroup)
NonRateTracePlots(Bayesgroup)
NonRateTracePlots(Bayesgroup, setOfEffects = 4:8, title = "model_base")
NonRateTracePlots(Bayesgroup, setOfEffects = 13:16, title = "model_gr")
NonRateTracePlots(Bayesgroup, setOfEffects = 9:12, title = "model_gp")

# the result
summary(Bayesgroup, nfirst = 1500)
(pmean <- extract.posteriorMeans(Bayesgroup))
short_br <- shortBayesResults(Bayesgroup, nfirst = 1500)

# find orders of the effect names
getNames(Bayesgroup)

# distribution plots
AllDensityPlots(Bayesgroup, nfirst = 1500)
# MD plot
?plotPostMeansMDS()
plotPostMeansMDS(Bayesgroup, pmonly=1,nfirst = 1500,excludeRates=F, method = 3)
# global parameters
GlobalRateParameterPlots(Bayesgroup)
GlobalNonRateParameterPlots(Bayesgroup, setOfEffects = 4:8, title = "m_base")
GlobalNonRateParameterPlots(Bayesgroup, setOfEffects = 13:16, title = "m_gr")
GlobalNonRateParameterPlots(Bayesgroup, setOfEffects = 9:12, title = "m_gp")

# result table
my_test <-simpleBayesTest(Bayesgroup, nfirst=1500, ndigits = 3)
postmean <- short_br[34:46,15]
postsd <- short_br[34:46,16]

my_test <- my_test %>% 
  mutate(post.mean = round(postmean,3), post.s.d.m = round(postsd,3)) %>% 
  rename("parameter" = " ",cred.to = "  cred.to", p = "  p  ") %>% 
  select(parameter, varying, post.mean, post.s.d.m, cred.from, cred.to, p) %>% 
  rename("p (> 0)" = "p")

my_test$post.s.d.m <- paste0("(",my_test$post.s.d.m,")")

my_test %>% kbl(caption="Table 10: Posterior Results of Bayesian Multilevel Random Coefficients (Disliking Network)", booktabs = T) %>%  
  kable_styling(font_size = 12, htmltable_class = "lightable-classic",html_font = "Times New Roman", full_width = F) %>% 
  footnote("Data Source:RECENS (2017), N = 184") %>% 
  save_kable("network_descr_dl.html", bs_theme = "flatly")
