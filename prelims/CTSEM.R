
library(ctsem)
library(ctnet)
library(ctsemOMX)
library(tidyverse)
library(qgraph)


dt$time = dt$year - 2013
dt = dt[, c('id', 'time', 'skills_funding', 'public_health', 'GHQ', 'income')]

# Scaling
dt[, c('skills_funding', 'public_health', 'GHQ', 'income')] =
  scale(dt[, c('skills_funding', 'public_health', 'GHQ', 'income')])
summary(dt)

# Model
nlatent = 4
nmanifest = 4
tpoints = length(unique(dt$time))
latentNames = paste0("eta", 1:nlatent)
manifestNames = c('skills_funding', 'public_health',
                  'income', 'GHQ')
#ntdpred = 1
#TDPREDMEANS = matrix(0,ntdpred*(tpoints),1)
#TDPREDMEANS[floor(tpoints/2)] = 1

A <- matrix(c( "pskill_drift", 0, 'inc_pskill', 0,
               0, 'phealth_drift', 0, 'health_phealth',
               'pskill_inc', 0, 'inc_drift', 'health_inc',
               0, 'phealth_health', 'inc_health', 'health_drift'),
             nrow = nlatent, ncol = nlatent)

fullm = ctModel(Tpoints = tpoints, 
                n.latent = nlatent,
                manifestNames = manifestNames,
                n.manifest = nmanifest,
                LAMBDA = diag(nlatent),
                DRIFT = A,
                DIFFUSION = "auto",
                CINT = matrix(c(0), nrow = nlatent, ncol = 1),
                MANIFESTMEANS = matrix(0, nrow = nlatent, ncol = 1),
                MANIFESTVAR = matrix(0, nrow = nlatent, ncol = nlatent),
                #type = "stanct" 
                )

#start <- Sys.time()
#set.seed(1234)
#simfit <- ctStanFit(datalong = sub, ctstanmodel = fullm, optimize = FALSE)
#print(runtime <- Sys.time() - start)


####
start <- Sys.time()
set.seed(1234) # for reproducible standard errors
r <- ctFit(dat = dt,
            dataform = "long", 
            ctmodelobj = fullm,
            stationary = "all" )
print(runtime <- Sys.time() - start)

# extract results
print(summary(r)$ctparameters)

# plot autoregressive (AR) effects depending on interval lengths 0 to 30 days
ctPlot(r, plotType = "AR",
       xlim=c(0, 7),
       ylim=c(0, 1),
       ylab="Autoregressive effect",
       xlab="Time interval")
# plot unstandardized cross-lagged effects depending on interval lengths 0 to 30 days
ctPlot(r, plotType = "CR",
       xlim=c(0, 7),
       ylim=c(-0.05, 0.05),
       ylab="Cross-lagged effect",
       xlab="Time interval")
# plot standardized cross-lagged effects depending on interval lengths 0 to 30 days
ctPlot(r, plotType = "standardiseCR",
       xlim=c(0, 7),
       ylim=c(-0.05, 0.05),
       ylab="Cross-lagged effect",
       xlab="Time interval ")
# calculate unstandardized discrete-time parameter estimates for a time interval of 
expm(summary(r)$DRIFT*1)
# calculate standardized discrete-time parameter estimates for a time interval of
summary(r, verbose = TRUE, timeInterval = 1)["discreteDRIFTstd"]

ctsem_results <- summary(r)

driftdf <- ctsem_results$DRIFT
driftdf

qgraph(t(driftdf), fade = F,edge.labels = TRUE, theme = "colorblind", layout = "circle",
       mar=(rep(6,4)))


ctnet::TE(driftdf, dt = 5, IV = 3, DV = 4)
ctnet::IE(driftdf, dt = 2, IV = 1, DV = 4, M = 3)
ctnet::DE(driftdf, dt = 2, IV = 1, DV = 4, M = 3)


fullm$pars
fullm$pars[3,"indvarying"]





