

library(ctsem)
library(ctsemOMX)

nlatent = 3
nmanifest = 2
tpoints = 30
ntdpred = 1
TDPREDMEANS = matrix(0,ntdpred*(tpoints),1)
TDPREDMEANS[floor(tpoints/2)] = 1
genm = ctModel(Tpoints = tpoints,
             n.latent = nlatent, n.manifest = nmanifest, n.TDpred = ntdpred,
             LAMBDA = matrix(c(1, 0, 0, 1, 0, 0), nrow = nmanifest, ncol = nlatent),
             DRIFT = matrix(c(-.2, .1, 0,
                            0, -.3,1,
                            0,0,.0001), byrow = TRUE, nrow = nlatent, ncol = nlatent),
             DIFFUSION = matrix(c(.1, 0, 0,
                                .3,.2,0,
                                0,0,.0001), byrow = TRUE, nrow = nlatent, ncol = nlatent),
             MANIFESTVAR = matrix(c(.1,0,0,.1), nrow = nmanifest, ncol = nmanifest),
             TDPREDEFFECT = matrix(c(0, 0, 1), nrow = nlatent, ncol = ntdpred),
             CINT = matrix(c(0), nrow = nlatent, ncol = 1),
             TDPREDMEANS = TDPREDMEANS,
             MANIFESTMEANS = matrix(c(0), nrow = nmanifest, ncol = 1))

dat = ctGenerate(ctmodelobj = genm, n.subjects = 50, burnin = 50)

#ctIndplot(datawide = dat,n.subjects = 10,n.manifest = 2,Tpoints = tpoints)
nlatent = 4 #because for our fit we include extra predictor process
fullm = ctModel(Tpoints = tpoints, #type = "stanct",
              n.latent = nlatent, n.manifest = nmanifest, n.TDpred = ntdpred,
              LAMBDA = matrix(c(1, 0, 0, 0, 0, 1, 0, 0), nrow = nmanifest, ncol = nlatent),
              DRIFT = matrix(c("drift11", 1, "drift13", 0,
                             0, "drift22", 0, 0,
                             0, 0, "drift33", 1,
                             0, 0, 0, "drift44"), byrow = TRUE, nrow = nlatent, ncol = nlatent),
              DIFFUSION = matrix(c("diffusion11", 0, 0, 0,
                                 0, .0001, 0, 0,
                                 "diffusion31", 0, "diffusion33", 0,
                                 0, 0, 0, .0001), byrow = TRUE, nrow = nlatent, ncol = nlatent),
              T0VAR = matrix(c("t0var11", 0, 0, 0,
                             0, .0001, 0, 0,
                             "t0var31", 0, "t0var33", 0,
                             0, 0, 0, .0001), byrow = TRUE, nrow = nlatent, ncol = nlatent),
              TDPREDEFFECT = matrix(c("tdpredeffect11", "tdpredeffect21",
                                    "tdpredeffect31", "tdpredeffect41"), nrow = nlatent, ncol = ntdpred),
              CINT = matrix(c(0), nrow = nlatent, ncol = 1),
              T0MEANS = matrix(c("t0mean1", 0, "t0mean3",0),nrow = nlatent,ncol = 1),
              MANIFESTVAR = matrix(c("merror11",0,0,"merror22"), nrow = nmanifest, ncol = nmanifest),
              MANIFESTMEANS = matrix(c("mmeans_fit","mmeans_ex"), nrow = nmanifest, ncol = 1))
mediationm = fullm
mediationm$TDPREDEFFECT[1:2,1] = 0
mediationfit = ctFit(dat, mediationm)
fullfit = ctFit(dat, fullm,carefulFit = FALSE,
              omxStartValues  =  omxGetParameters(mediationfit$mxobj))
mxCompare(base  =  fullfit$mxobj, comparison  =  mediationfit$mxobj)




data("ctExample1", package = "ctsem")
traitmodel <- ctModel(n.manifest = 2, n.latent = 2, Tpoints = 6,
                         LAMBDA = diag(2), manifestNames = c("LeisureTime", "Happiness"),
                         latentNames = c("LeisureTime", "Happiness"), TRAITVAR = "auto")
traitfit <- ctFit(datawide = ctExample1, ctmodelobj = traitmodel)



data("datastructure", package = "ctsem")
datastructure
semModel <- ctModel(n.latent = 2, n.manifest = 3, TRAITVAR = "auto",
                     n.TIpred = 2, n.TDpred = 1, Tpoints = 3,
                     LAMBDA = matrix(c(1, "lambda21", 0, 0, 1, 0), nrow = 3))
semFit <- ctFit(datastructure, semModel, nofit = TRUE)
semFit$mxobj$A$labels
semFit$mxobj$S$labels
semFit$mxobj$M$labels
semFit$mxobj$F$values




















