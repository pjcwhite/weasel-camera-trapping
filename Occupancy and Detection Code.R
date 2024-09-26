#######################################################################
# OCCUPANCY AND DETECTION MODELLING - Kate Ebel & Patrick J. C. White #
#######################################################################

# INITIAL DATA MANAGEMENT

  # INSTALL spOccupancy PACKAGE
  library(spOccupancy)

  # READ IN FILE 'WEASEL OCCUPANCY MODEL RAW DATA.csv'
  rm(list=ls())
  weasel.raw<-read.csv(file=fil.choose(),stringsAsFactors = T)

  # EMPTY ROW REMOVAL IF REQUIRED
  weasels<-weasel.raw[1:126,]

  # BELOW RESTRICTS DATA TO CAMERAS THAT ARE >= 20m APART (SEE PAPER). ALL 
  # SUBSEQUENT CODE CAN BE RUN EITHER WITH THE FULL (n = 126 setups) OR 
  # RESTRICTED(n = 73 setups) DASTSET
  weasels<-weasels[weasels$Distance.to.nearest.cam..m.>=20,]
  
# FORMATTING DATA FOR USE IN PGOcc() FUNCTION
  
  # DETECTIONS
  weasels.encounters<-weasels[,-c(1:25)] #delete non encounter columns - change if covariates added or removed
  y<-as.matrix(weasels.encounters)
  dimnames(y)<-list(1:nrow(weasels.encounters),1:ncol(weasels.encounters))
  
  #OCCURENCE COVARIATES
  fall<-weasels$Season=="fall"
  winter<-weasels$Season=="winter"
  spring<-weasels$Season=="spring"
  summer<-weasels$Season=="summer"
  occ.covs<-cbind(fall,winter,spring,summer,weasels$Site.Random.Effect)
  colnames(occ.covs)<-c("fall","winter","spring","summer","site")
  
  #DETECTION COVARIATES
  det.covs<-list(weasels$Bait,weasels$Lure,weasels$anybaitlure,weasels$Season,weasels$baitluretype,weasels$Site.Random.Effect,weasels$Kill.Squeak)
  names(det.covs)<-c("bait","lure","anybaitlure","season","baitluretype","site","KS")
  
  #COORDINATES (UNUSED IN THIS ANALYSES)
  coords<-cbind(weasels$X,weasels$Y)
  dimnames(coords)<-list(1:nrow(weasels.encounters),c("X","Y"))
  
  #CREATE LIST OBJECT FOR PGOcc() FUNCTION
  weasel.for.spoc<-list(y,occ.covs,det.covs,coords)
  names(weasel.for.spoc)<-c("y","occ.covs","det.covs","coords")
  
# SINGLE SPECIES OCCUPANCY MODELS
  
  # SPECIFIC INITIAL VALUES
  weasel.inits<-list(alpha = c(0),beta = c(0),z=apply(weasel.for.spoc$y,1,max,na.rm = TRUE))
  
  # SPECIFY PRIORS
  weasel.priors<-list(alpha.normal=list(mean=0,var=2.72),beta.normal=list(mean=0,var=2.72))
  
  # MCMC ALGORITHM SETTINGS
  n.samples<-10000 # increased from default 5000 due to non convergence
  n.burn<-3000;n.thin<-2;n.chains<-3
  
  # CANDIDATE MODEL SET (NOTE MODELS MAY TALE SEVERAL MINUTES TO RUN)
  model1<- PGOcc(occ.formula = ~ fall + winter + spring + summer + (1 | site), det.formula = ~ 1 + (1 | site), 
                 data = weasel.for.spoc, inits = weasel.inits, n.samples = n.samples, priors = weasel.priors, n.omp.threads = 1, verbose = TRUE, n.report = 1000,  n.burn = n.burn, n.thin = n.thin, n.chains = n.chains)
  model2<- PGOcc(occ.formula = ~ fall + winter + spring + summer + (1 | site), det.formula = ~ anybaitlure + (1 | site), 
                 data = weasel.for.spoc, inits = weasel.inits, n.samples = n.samples, priors = weasel.priors, n.omp.threads = 1, verbose = TRUE, n.report = 1000,  n.burn = n.burn, n.thin = n.thin, n.chains = n.chains)
  model3<- PGOcc(occ.formula = ~ fall + winter + spring + summer + (1 | site), det.formula = ~ bait + (1 | site), 
                 data = weasel.for.spoc, inits = weasel.inits, n.samples = n.samples, priors = weasel.priors, n.omp.threads = 1, verbose = TRUE, n.report = 1000,  n.burn = n.burn, n.thin = n.thin, n.chains = n.chains)
  model4<- PGOcc(occ.formula = ~ fall + winter + spring + summer + (1 | site), det.formula = ~ lure + (1 | site), 
                 data = weasel.for.spoc, inits = weasel.inits, n.samples = n.samples, priors = weasel.priors, n.omp.threads = 1, verbose = TRUE, n.report = 1000,  n.burn = n.burn, n.thin = n.thin, n.chains = n.chains)
  model5<- PGOcc(occ.formula = ~ fall + winter + spring + summer + (1 | site), det.formula = ~ KS + (1 | site), 
                 data = weasel.for.spoc, inits = weasel.inits, n.samples = n.samples, priors = weasel.priors, n.omp.threads = 1, verbose = TRUE, n.report = 1000,  n.burn = n.burn, n.thin = n.thin, n.chains = n.chains)
  model6<- PGOcc(occ.formula = ~ fall + winter + spring + summer + (1 | site), det.formula = ~ baitluretype + (1 | site), 
                 data = weasel.for.spoc, inits = weasel.inits, n.samples = n.samples, priors = weasel.priors, n.omp.threads = 1, verbose = TRUE, n.report = 1000,  n.burn = n.burn, n.thin = n.thin, n.chains = n.chains)
  model7<- PGOcc(occ.formula = ~ fall + winter + spring + summer + (1 | site), det.formula = ~ season + (1 | site), 
                 data = weasel.for.spoc, inits = weasel.inits, n.samples = n.samples, priors = weasel.priors, n.omp.threads = 1, verbose = TRUE, n.report = 1000,  n.burn = n.burn, n.thin = n.thin, n.chains = n.chains)
  model8<- PGOcc(occ.formula = ~ fall + winter + spring + summer + (1 | site), det.formula = ~ season + anybaitlure + (1 | site), 
                 data = weasel.for.spoc, inits = weasel.inits, n.samples = n.samples, priors = weasel.priors, n.omp.threads = 1, verbose = TRUE, n.report = 1000,  n.burn = n.burn, n.thin = n.thin, n.chains = n.chains)
  model9<- PGOcc(occ.formula = ~ fall + winter + spring + summer + (1 | site), det.formula = ~ season + bait + (1 | site), 
                 data = weasel.for.spoc, inits = weasel.inits, n.samples = n.samples, priors = weasel.priors, n.omp.threads = 1, verbose = TRUE, n.report = 1000,  n.burn = n.burn, n.thin = n.thin, n.chains = n.chains)
  model10<- PGOcc(occ.formula = ~ fall + winter + spring + summer + (1 | site), det.formula = ~ season + lure + (1 | site), 
                  data = weasel.for.spoc, inits = weasel.inits, n.samples = n.samples, priors = weasel.priors, n.omp.threads = 1, verbose = TRUE, n.report = 1000,  n.burn = n.burn, n.thin = n.thin, n.chains = n.chains)
  model11<- PGOcc(occ.formula = ~ fall + winter + spring + summer + (1 | site), det.formula = ~ season + KS + (1 | site), 
                  data = weasel.for.spoc, inits = weasel.inits, n.samples = n.samples, priors = weasel.priors, n.omp.threads = 1, verbose = TRUE, n.report = 1000,  n.burn = n.burn, n.thin = n.thin, n.chains = n.chains)
  model12<- PGOcc(occ.formula = ~ fall + winter + spring + summer + (1 | site), det.formula = ~ season + baitluretype + (1 | site), 
                  data = weasel.for.spoc, inits = weasel.inits, n.samples = n.samples, priors = weasel.priors, n.omp.threads = 1, verbose = TRUE, n.report = 1000,  n.burn = n.burn, n.thin = n.thin, n.chains = n.chains)
  
  # MODEL SELECTION STATISTICS
  WAICetc<-as.data.frame(rbind(waicOcc(model1),waicOcc(model2),waicOcc(model3),
    waicOcc(model4),waicOcc(model5),waicOcc(model6),waicOcc(model7),waicOcc(model8),
    waicOcc(model9),waicOcc(model10),waicOcc(model11),waicOcc(model12)))
  
  # GENERATION OF A WAIC MODEL SELECTION TABLE
  model.names<-c("model1","model2","model3","model4","model5","model6","model7","model8","model9",
                 "model10","model11","model12")
  occ.terms<-as.vector(unlist(c(model1$call[[2]],model2$call[[2]],model3$call[[2]],model4$call[[2]],model5$call[[2]],model6$call[[2]],model7$call[[2]],model8$call[[2]],model9$call[[2]],
                                model10$call[[2]],model11$call[[2]],model12$call[[2]])),'character')
  det.terms<-as.vector(unlist(c(model1$call[[3]],model2$call[[3]],model3$call[[3]],model4$call[[3]],model5$call[[3]],model6$call[[3]],model7$call[[3]],model8$call[[3]],model9$call[[3]],
                                model10$call[[3]],model11$call[[3]],model12$call[[3]])),'character')
  deltaWAIC<-WAICetc$WAIC-min(WAICetc$WAIC)
  rel.likelihood<-exp(-0.5*deltaWAIC)
  akaikes.w<-round(rel.likelihood/sum(rel.likelihood),3)
  mod.selection<-data.frame(model.names,occ.terms,det.terms,WAICetc,deltaWAIC,rel.likelihood,akaikes.w)
  mod.selection<- mod.selection[order(mod.selection$WAIC, decreasing = FALSE),]
  mod.selection
  
# MODEL ASSESSMENT (EXAMPLES GIVEN FOR ONE MODEL ONLY)
  
  # TO CHECK C-HAT VALUES
  summary(model4)
  
  # TRACE PLOTS
  plot(model4, 'alpha', density = FALSE)
  plot(model4, 'beta', density = FALSE)
  
  # BAYESIAN P-VALUE
  n.samples<-5000 # samples reduced for memory allocation, so not same model
  model4<- PGOcc(occ.formula = ~ fall + winter + spring + summer + (1 | site), det.formula = ~ season + lure + (1 | site), 
                 data = weasel.for.spoc, inits = weasel.inits, n.samples = n.samples, priors = weasel.priors, n.omp.threads = 1, verbose = TRUE, n.report = 1000,  n.burn = n.burn, n.thin = n.thin, n.chains = n.chains)
  ppc.out <- ppcOcc(model4, fit.stat = 'freeman-tukey', group = 1)
  summary(ppc.out)
  
# EXTRACING OCCUPANCY ESTIMATES PER SEASON (EXAMPLES GIVEN FOR ONE MODEL ONLY)
  
  # NAIVE OCCUPANCY
  table(weasels$Weasel.Detection,weasels$Season)
  
  # MODELLED OCCUPANCY
  model<-model4
  X.0<-cbind(1,1,0,0,0) # FALL
  prediction<-predict(model,X.0,ignore.RE = T)
  summary(prediction$psi.0.samples)
  X.0<-cbind(1,0,0,0,1) # WINTER
  prediction<-predict(model,X.0,ignore.RE = T)
  summary(prediction$psi.0.samples)
  X.0<-cbind(1,0,1,0,0) # SPRING
  prediction<-predict(model,X.0,ignore.RE = T)
  summary(prediction$psi.0.samples)
  X.0<-cbind(1,0,0,1,0) # SUMMER
  prediction<-predict(model,X.0,ignore.RE = T)
  summary(prediction$psi.0.samples)
  
# EXTRACTING DETECTION PARAMETERS
  
  summary(model4) # values are on the logit scale
