
# see brief tutorial on running Maxent at:
# https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md


# OPEN for model settings:
# https://groups.google.com/g/maxent/c/yRBlvZ1_9rQ



###################################### FIRST STEP: CHOOSE CONTINENT FOR MODELING

continent <- "NAM"
coords <- c(-168, -47, 8, 85) #for projection to North-America


#####################################    
    
# create directories
dir.create(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResults/"))
dir.create(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResultsRasters/"))
#dir.create(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/1FUTUREmodelResultsRasters/"))
#dir.create(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/2FUTUREmodelResultsRasters/"))
dir.create(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/TSS_results"))
dir.create(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResultsBinary"))
dir.create(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelEval"))
dir.create(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelEval/prob_presence/"))
dir.create(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResultsBinary_cropped/"))
dir.create(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResultsRasters_cropped/"))



# for Maxent modeling
library(dismo)
library(raster)
library(rgeos)
library(rJava)
library(dplyr)

# for TSS calculations
library(ROCR)
library(vcd)
library(boot)

############################################### INSTALL SOFTWARE

# allow access maxent.jar to dismo package
java.parameters = c('-Xss2560k', '-Xmx2g')

#setwd("wd/distribution_models/maxent/")
#utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.3.3k/maxent.jar", 
#          destfile = paste0(system.file("java", package = "dismo"), 
#          "/maxent.jar"), mode = "wb")  ## wb for binary file, otherwise maxent.jar can not execute


###############################################
############################################### ENVIRONMENTAL VARIABLES
library(maptools)

setwd("wd/distribution_models/climate/p1")
clim_list <- list.files(pattern = ".grd$", full.names = T)
clim_list
clim <- raster::stack(clim_list)

setwd("wd/distribution_models/climate/p2")
clim_listPROJ <- list.files(pattern = ".grd$", full.names = T)
clim_listPROJ
climPROJ <- raster::stack(clim_listPROJ)
    extent <- extent(coords)
    climPROJ <- crop(climPROJ, extent)  

#setwd("wd/distribution_models/futureClimModels/layers_selected/canesmASCII/")
#fut_clim_listPROJ1 <- list.files(pattern = ".grd$", full.names = T) 
#fut_climPROJ1 <- raster::stack(fut_clim_listPROJ1)
#    extent <- extent(coords)
#    fut_climPROJ1 <- crop(fut_climPROJ1, extent)  
#    names(fut_climPROJ1) <- c("layer.1", "layer.2","layer.3")


#setwd("wd/distribution_models/futureClimModels/layers_selected/cnrmASCII/")
#fut_clim_listPROJ2 <- list.files(pattern = ".grd$", full.names = T) 
#fut_climPROJ2 <- raster::stack(fut_clim_listPROJ2)
#    extent <- extent(coords)
#    fut_climPROJ2 <- crop(fut_climPROJ2, extent)  
#    names(fut_climPROJ2) <- c("layer.1", "layer.2","layer.3")
    
# set occurence data
setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/"))
odedata <- list.files(path = paste0(getwd(), "/data_byspecies_p1/"), pattern = ".csv$", full.names = F)


for (i in odedata) {
  setwd(paste0("wd/distribution_models/0MAXENTMODEL_",continent))
  dat <- read.csv(paste0("data_byspecies_p1/", i))
  
  occ_clean <- dat %>% dplyr::filter(timeperiod == "P1") %>% dplyr::select(species, long, lat) %>% unique()
  dups <- duplicated(occ_clean[c("long", "lat")])
  occ_unique <- occ_clean[!dups, ]
  cat(nrow(occ_clean) - nrow(occ_unique), "records are removed")

  coordinates(occ_unique) <- ~long + lat
  occ_final <- occ_unique

  projection <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" ## set projection
  crs(occ_final) <- projection
  
  # this creates a 4-decimal-degree buffer around the
  # occurrence data
  occ_buff <- buffer(occ_final, width = 500000) # the buffer is in meters

  # crop study area to a manageable extent (rectangle shaped)
  studyArea <- crop(clim,extent(occ_buff))  

  # the 'study area' created by extracting the buffer area from the raster stack
  studyArea <- mask(studyArea,occ_buff)
  # output will still be a raster stack, just of the study area

  #increase the number of cells to be able to extract 10 000 background points
  studyArea <- disaggregate(studyArea, fact=4)
  res(studyArea)

  # save the new study area rasters as ascii
  writeRaster(studyArea,
            # a series of names for output files
            filename=paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/", names(studyArea), 
                            i, ".asc"), 
            format="ascii", ## the output format
            bylayer=TRUE, ## this will save a series of layers
            overwrite=T)

  # select background points from this buffered area; when the number provided 
  # to set.seed() function, the same random sample will be selected in the next line			
  # use this code before the sampleRandom function every time, if you want to get
  # the same "random samples"
  set.seed(1)
  
    bg <- sampleRandom(x=studyArea,
                   size=10000,
                   na.rm=T, #removes the 'Not Applicable' points  
                   sp=T) # return spatial points 

  # extracting env conditions for training occ from the raster
  # stack; a data frame is returned (i.e multiple columns)
  p <- raster::extract(clim, occ_final)
  # env conditions for testing occ
  #p_test <- extract(clim, occ_test)
  # extracting env conditions for background
  a <- raster::extract(clim, bg)

  # repeat the number 1 as many numbers as the number of rows
  # in p, and repeat 0 as the rows of background points
  pa <- c(rep(1, nrow(p)), rep(0, nrow(a)))

  # (rep(1,nrow(p)) creating the number of rows as the p data
  # set to have the number '1' as the indicator for presence;
  # rep(0,nrow(a)) creating the number of rows as the a data
  # set to have the number '0' as the indicator for absence;
  # the c combines these ones and zeros into a new vector that
  # can be added to the Maxent table data frame with the
  # environmental attributes of the presence and absence
  # locations
  pder <- as.data.frame(rbind(p, a))
  

  ######################################################################################## RUNNING THE MODELS
  ########################################################################################
  ########################################################################################

  
  # create one directory per species in the folder modelResults

  dir.create(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResults/", i))

  # train Maxent with spatial data
  # mod <- maxent(x=clim,p=occ_train)

  # train Maxent with tabular data
  mod <- maxent(x=pder, ## env conditions
              p=pa,   ## 1:presence or 0:absence
              path=paste0("wd/distribution_models/0MAXENTMODEL_", 
                          continent, "/modelResults/", i), ## folder for maxent output; 
              # if we do not specify a folder R will put the results in a temp file, 
              # and it gets messy to read those. . .
              args=c("responsecurves=TRUE",
                     "jackknife=TRUE",
                     "doclamp=TRUE",
                     "outputgrids=FALSE",
                     "outputformat=logistic",
                     "replicates=5",
                     "replicatetype=crossvalidate",
                     "writebackgroundpredictions=TRUE")
              )
  
# view the maxent model in a html brower
# mod

#View detailed results
# print(mod@results)


# project to current climate

climPROJext <- mask(climPROJ, occ_buff) # set extent for model predictions
ew <- extent(climPROJext)
proj <- predict(mod, climPROJext, ext = ew)
proj <- calc(proj, fun = mean)

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResultsRasters"))
writeRaster(proj, paste0("ave_", i), overwrite = TRUE)  

##########################################################################################################
#### to create binary maps you first have to see what threshold maximises TSS
############################# CALCULATE TSS

##### Set Output Maxent Directory

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent))

#Output Maxent Directory

list.files(path = paste0("modelResults/", i), pattern="_samplePredictions.csv")->listaoutput
sub("_samplePredictions.csv","",listaoutput)->listaoutput

TSS <- as.data.frame(matrix(ncol = 3, nrow = 0))
colnames(TSS)<-c("Replica","TSS","Bin.Prob")

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResults/", i))

lapply(listaoutput, function(name){
  name->sp
  presence <- read.csv(paste(sp,"_samplePredictions.csv",sep=""))
  background <- read.csv(paste(sp,"_backgroundPredictions.csv",sep=""))
  pp <- presence$Logistic.prediction                # get the predictions column
  testpp <- pp[presence$Test.or.train=="test"]       # select test points only
  trainpp <- pp[presence$Test.or.train=="train"]   # select training points only
  bb <- background$logistic
  combined <- c(testpp, bb)                             # combine into a single vector
  read.csv("maxentResults.csv")->maxres
  ### Set the threshold rule
  threshold<-maxres[maxres[,1]==name,"Equal.test.sensitivity.and.specificity.logistic.threshold"]
  bin.prob<-maxres[maxres[,1]==name,"Equal.test.sensitivity.and.specificity.binomial.probability"]
  #Number of values higher and lower than threshold in test
  sum(testpp > threshold)->maiortest
  sum(testpp < threshold)->menortest
  #Number of values higher and lower than threshold in background
  sum(bb > threshold)->maiorbb
  sum(bb < threshold)->menorbb
  #Calculate sensitivity and specificity
  sensit<-(maiortest)/(maiortest+menortest)
  specit<-(menorbb)/(maiorbb+menorbb)
  tss<-sensit+specit-1
  tsssp<-as.data.frame(sp)
  tsssp[2]<-tss
  tsssp[3]<-bin.prob
  colnames(tsssp)<-c("Replica","TSS","Bin.Prob")
  rbind(TSS,tsssp)->>TSS
  
})

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/TSS_results/"))
write.csv(TSS,quote=FALSE,file=paste0(i, "__TSSequalTest.csv"))

###########################################

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent))

#Output Maxent Directory

list.files(path = paste0("modelResults/", i), pattern="_samplePredictions.csv")->listaoutput
sub("_samplePredictions.csv","",listaoutput)->listaoutput

TSS <- as.data.frame(matrix(ncol = 3, nrow = 0))
colnames(TSS)<-c("Replica","TSS","Bin.Prob")

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResults/", i))

lapply(listaoutput, function(name){
  name->sp
  presence <- read.csv(paste(sp,"_samplePredictions.csv",sep=""))
  background <- read.csv(paste(sp,"_backgroundPredictions.csv",sep=""))
  pp <- presence$Logistic.prediction                # get the predictions column
  testpp <- pp[presence$Test.or.train=="test"]       # select test points only
  trainpp <- pp[presence$Test.or.train=="train"]   # select training points only
  bb <- background$logistic
  combined <- c(testpp, bb)                             # combine into a single vector
  read.csv("maxentResults.csv")->maxres
  ### Set the threshold rule
  threshold<-maxres[maxres[,1]==name,"Equal.training.sensitivity.and.specificity.logistic.threshold"]
  bin.prob<-maxres[maxres[,1]==name,"Equal.training.sensitivity.and.specificity.binomial.probability"]
  #Number of values higher and lower than threshold in test
  sum(testpp > threshold)->maiortest
  sum(testpp < threshold)->menortest
  #Number of values higher and lower than threshold in background
  sum(bb > threshold)->maiorbb
  sum(bb < threshold)->menorbb
  #Calculate sensitivity and specificity
  sensit<-(maiortest)/(maiortest+menortest)
  specit<-(menorbb)/(maiorbb+menorbb)
  tss<-sensit+specit-1
  tsssp<-as.data.frame(sp)
  tsssp[2]<-tss
  tsssp[3]<-bin.prob
  colnames(tsssp)<-c("Replica","TSS","Bin.Prob")
  rbind(TSS,tsssp)->>TSS
  
})

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/TSS_results/"))
write.csv(TSS,quote=FALSE,file=paste0(i, "__TSSequalTrain.csv"))




###########################################

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent))

#Output Maxent Directory

list.files(path = paste0("modelResults/", i), pattern="_samplePredictions.csv")->listaoutput
sub("_samplePredictions.csv","",listaoutput)->listaoutput

TSS <- as.data.frame(matrix(ncol = 3, nrow = 0))
colnames(TSS)<-c("Replica","TSS","Bin.Prob")

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResults/", i))

lapply(listaoutput, function(name){
  name->sp
  presence <- read.csv(paste(sp,"_samplePredictions.csv",sep=""))
  background <- read.csv(paste(sp,"_backgroundPredictions.csv",sep=""))
  pp <- presence$Logistic.prediction                # get the predictions column
  testpp <- pp[presence$Test.or.train=="test"]       # select test points only
  trainpp <- pp[presence$Test.or.train=="train"]   # select training points only
  bb <- background$logistic
  combined <- c(testpp, bb)                             # combine into a single vector
  read.csv("maxentResults.csv")->maxres
  ### Set the threshold rule
  threshold<-maxres[maxres[,1]==name,"Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold"]
  bin.prob<-maxres[maxres[,1]==name,"Equate.entropy.of.thresholded.and.original.distributions.binomial.probability"]
  #Number of values higher and lower than threshold in test
  sum(testpp > threshold)->maiortest
  sum(testpp < threshold)->menortest
  #Number of values higher and lower than threshold in background
  sum(bb > threshold)->maiorbb
  sum(bb < threshold)->menorbb
  #Calculate sensitivity and specificity
  sensit<-(maiortest)/(maiortest+menortest)
  specit<-(menorbb)/(maiorbb+menorbb)
  tss<-sensit+specit-1
  tsssp<-as.data.frame(sp)
  tsssp[2]<-tss
  tsssp[3]<-bin.prob
  colnames(tsssp)<-c("Replica","TSS","Bin.Prob")
  rbind(TSS,tsssp)->>TSS
  
})

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/TSS_results/"))
write.csv(TSS,quote=FALSE,file=paste0(i, "__TSSequateEntropy.csv"))



###########################################

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent))

#Output Maxent Directory

list.files(path = paste0("modelResults/", i), pattern="_samplePredictions.csv")->listaoutput
sub("_samplePredictions.csv","",listaoutput)->listaoutput

TSS <- as.data.frame(matrix(ncol = 3, nrow = 0))
colnames(TSS)<-c("Replica","TSS","Bin.Prob")

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResults/", i))

lapply(listaoutput, function(name){
  name->sp
  presence <- read.csv(paste(sp,"_samplePredictions.csv",sep=""))
  background <- read.csv(paste(sp,"_backgroundPredictions.csv",sep=""))
  pp <- presence$Logistic.prediction                # get the predictions column
  testpp <- pp[presence$Test.or.train=="test"]       # select test points only
  trainpp <- pp[presence$Test.or.train=="train"]   # select training points only
  bb <- background$logistic
  combined <- c(testpp, bb)                             # combine into a single vector
  read.csv("maxentResults.csv")->maxres
  ### Set the threshold rule
  threshold<-maxres[maxres[,1]==name,"Maximum.test.sensitivity.plus.specificity.logistic.threshold"]
  bin.prob<-maxres[maxres[,1]==name,"Maximum.test.sensitivity.plus.specificity.binomial.probability"]
  #Number of values higher and lower than threshold in test
  sum(testpp > threshold)->maiortest
  sum(testpp < threshold)->menortest
  #Number of values higher and lower than threshold in background
  sum(bb > threshold)->maiorbb
  sum(bb < threshold)->menorbb
  #Calculate sensitivity and specificity
  sensit<-(maiortest)/(maiortest+menortest)
  specit<-(menorbb)/(maiorbb+menorbb)
  tss<-sensit+specit-1
  tsssp<-as.data.frame(sp)
  tsssp[2]<-tss
  tsssp[3]<-bin.prob
  colnames(tsssp)<-c("Replica","TSS","Bin.Prob")
  rbind(TSS,tsssp)->>TSS
  
})

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/TSS_results/"))
write.csv(TSS,quote=FALSE,file=paste0(i, "__TSSmaxTest.csv"))



###########################################

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent))

#Output Maxent Directory

list.files(path = paste0("modelResults/", i), pattern="_samplePredictions.csv")->listaoutput
sub("_samplePredictions.csv","",listaoutput)->listaoutput

TSS <- as.data.frame(matrix(ncol = 3, nrow = 0))
colnames(TSS)<-c("Replica","TSS","Bin.Prob")

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResults/", i))

lapply(listaoutput, function(name){
  name->sp
  presence <- read.csv(paste(sp,"_samplePredictions.csv",sep=""))
  background <- read.csv(paste(sp,"_backgroundPredictions.csv",sep=""))
  pp <- presence$Logistic.prediction                # get the predictions column
  testpp <- pp[presence$Test.or.train=="test"]       # select test points only
  trainpp <- pp[presence$Test.or.train=="train"]   # select training points only
  bb <- background$logistic
  combined <- c(testpp, bb)                             # combine into a single vector
  read.csv("maxentResults.csv")->maxres
  ### Set the threshold rule
  threshold<-maxres[maxres[,1]==name,"Maximum.training.sensitivity.plus.specificity.logistic.threshold"]
  bin.prob<-maxres[maxres[,1]==name,"Maximum.training.sensitivity.plus.specificity.binomial.probability"]
  #Number of values higher and lower than threshold in test
  sum(testpp > threshold)->maiortest
  sum(testpp < threshold)->menortest
  #Number of values higher and lower than threshold in background
  sum(bb > threshold)->maiorbb
  sum(bb < threshold)->menorbb
  #Calculate sensitivity and specificity
  sensit<-(maiortest)/(maiortest+menortest)
  specit<-(menorbb)/(maiorbb+menorbb)
  tss<-sensit+specit-1
  tsssp<-as.data.frame(sp)
  tsssp[2]<-tss
  tsssp[3]<-bin.prob
  colnames(tsssp)<-c("Replica","TSS","Bin.Prob")
  rbind(TSS,tsssp)->>TSS
  
})

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/TSS_results/"))
write.csv(TSS,quote=FALSE,file=paste0(i, "__TSSmaxTrain.csv"))


###########################################

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent))

#Output Maxent Directory

list.files(path = paste0("modelResults/", i), pattern="_samplePredictions.csv")->listaoutput
sub("_samplePredictions.csv","",listaoutput)->listaoutput

TSS <- as.data.frame(matrix(ncol = 3, nrow = 0))
colnames(TSS)<-c("Replica","TSS","Bin.Prob")

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResults/", i))

lapply(listaoutput, function(name){
  name->sp
  presence <- read.csv(paste(sp,"_samplePredictions.csv",sep=""))
  background <- read.csv(paste(sp,"_backgroundPredictions.csv",sep=""))
  pp <- presence$Logistic.prediction                # get the predictions column
  testpp <- pp[presence$Test.or.train=="test"]       # select test points only
  trainpp <- pp[presence$Test.or.train=="train"]   # select training points only
  bb <- background$logistic
  combined <- c(testpp, bb)                             # combine into a single vector
  read.csv("maxentResults.csv")->maxres
  ### Set the threshold rule
  threshold<-maxres[maxres[,1]==name,"Minimum.training.presence.logistic.threshold"]
  bin.prob<-maxres[maxres[,1]==name,"Minimum.training.presence.binomial.probability"]
  #Number of values higher and lower than threshold in test
  sum(testpp > threshold)->maiortest
  sum(testpp < threshold)->menortest
  #Number of values higher and lower than threshold in background
  sum(bb > threshold)->maiorbb
  sum(bb < threshold)->menorbb
  #Calculate sensitivity and specificity
  sensit<-(maiortest)/(maiortest+menortest)
  specit<-(menorbb)/(maiorbb+menorbb)
  tss<-sensit+specit-1
  tsssp<-as.data.frame(sp)
  tsssp[2]<-tss
  tsssp[3]<-bin.prob
  colnames(tsssp)<-c("Replica","TSS","Bin.Prob")
  rbind(TSS,tsssp)->>TSS
  
})

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/TSS_results/"))
write.csv(TSS,quote=FALSE,file=paste0(i, "__TSSminTrain.csv"))


###########################################

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent))

#Output Maxent Directory

list.files(path = paste0("modelResults/", i), pattern="_samplePredictions.csv")->listaoutput
sub("_samplePredictions.csv","",listaoutput)->listaoutput

TSS <- as.data.frame(matrix(ncol = 3, nrow = 0))
colnames(TSS)<-c("Replica","TSS","Bin.Prob")

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResults/", i))

lapply(listaoutput, function(name){
  name->sp
  presence <- read.csv(paste(sp,"_samplePredictions.csv",sep=""))
  background <- read.csv(paste(sp,"_backgroundPredictions.csv",sep=""))
  pp <- presence$Logistic.prediction                # get the predictions column
  testpp <- pp[presence$Test.or.train=="test"]       # select test points only
  trainpp <- pp[presence$Test.or.train=="train"]   # select training points only
  bb <- background$logistic
  combined <- c(testpp, bb)                             # combine into a single vector
  read.csv("maxentResults.csv")->maxres
  ### Set the threshold rule
  threshold<-maxres[maxres[,1]==name,"X10.percentile.training.presence.logistic.threshold"]
  bin.prob<-maxres[maxres[,1]==name,"X10.percentile.training.presence.binomial.probability"]
  #Number of values higher and lower than threshold in test
  sum(testpp > threshold)->maiortest
  sum(testpp < threshold)->menortest
  #Number of values higher and lower than threshold in background
  sum(bb > threshold)->maiorbb
  sum(bb < threshold)->menorbb
  #Calculate sensitivity and specificity
  sensit<-(maiortest)/(maiortest+menortest)
  specit<-(menorbb)/(maiorbb+menorbb)
  tss<-sensit+specit-1
  tsssp<-as.data.frame(sp)
  tsssp[2]<-tss
  tsssp[3]<-bin.prob
  colnames(tsssp)<-c("Replica","TSS","Bin.Prob")
  rbind(TSS,tsssp)->>TSS
  
})

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/TSS_results/"))
write.csv(TSS,quote=FALSE,file=paste0(i, "__TSSx10perc.csv"))


}



#############################################
#############################################
#############################################
#############################################
#############################################
#############################################
#############################################

#library(bnspatial)

# set occurence data
#setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/"))
#species <- list.files(path = paste0(getwd(), "/data_byspecies_p1/"), pattern = ".csv$", full.names = F)
#species <- lapply(X = species, FUN = function(t) gsub(pattern = ".csv", replacement = "", x = t, fixed = TRUE))

#for (i in species) {
#  setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent))
#  studyAr <- raster(paste0("layer.1", i, ".csv.asc"))

#  # load raster results to crop
#  setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResultsBinary"))
#  binaryMap <- raster(paste0(i, ".grdbinary.grd"))
  
#  setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResultsRasters"))
#  continuousMap <- raster(paste0("ave_", i, ".grd"))

  #crop to outline of studyAr (creted with a buffer)
#  binaryMap <- crop(binaryMap, extent(studyAr))  
#  binaryMap <- extractByMask(binaryMap, studyAr, spatial = TRUE)
  
#  setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, 
#               "/modelResultsBinary_cropped"))
#  writeRaster(binaryMap, paste0(i, ".asc"), overwrite = TRUE)  
  
#  continuousMap <- crop(continuousMap, extent(studyAr))  
#  continuousMap <- extractByMask(continuousMap, studyAr, spatial = TRUE)
  
#  setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, 
#               "/modelResultsRasters_cropped"))
#  writeRaster(continuousMap, paste0(i, ".asc"), overwrite = TRUE)  
  
#}




#############################################
#############################################
#############################################
#############################################
#############################################
#############################################
#############################################

library(dplyr)

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/TSS_results"))
tss <- list.files(pattern = ".csv$", full.names = F)

numberSpecies <- length(tss)
data <- data.frame(tss = rep(NA))     # Creating data containing NA

for (i in tss) {
  dat <- read.csv(i, header = T)
  TSScolumn = dat[,3]
  meanTSS <- mean(TSScolumn)
  data <- rbind(data, meanTSS)
  }

data <- na.omit(data)
rownames(data) <- NULL
data$id <- rownames(data)
threshold <- as.data.frame(tss)
colnames(threshold) <- c("thresh")
threshold$id <- rownames(threshold)
data <- dplyr::full_join(data, threshold) %>% dplyr::select(tss, thresh)

newThresh <- as.data.frame(gsub(".csv.*", "", data$thresh))
data['newThresh'] = newThresh

maxThresh <- data %>% group_by(newThresh, thresh) %>% dplyr::summarize(maxThresh = max(tss)) %>% ungroup()
maxThresh1 <- data %>% group_by(newThresh) %>% dplyr::summarize(maxThresh = max(tss)) %>% ungroup()
maxThresh <- dplyr::left_join(maxThresh1, maxThresh)

maxThresh

#### decide which threshold type you select

    # apply threshold of >0.4 to TSS value (cite Landis JR, Koch GG. 
          #The measurement of observer agreement for categorical data. Biometrics 1977;33:159-74.) 

#maxThresh_filter <- maxThresh %>% dplyr::filter(maxThresh >= 0.4)
maxThresh_filter <- maxThresh

maxThresh_filter$thresh <- gsub(".csv", "", maxThresh_filter$thresh, fixed=TRUE)
maxThresh_filter <- as.data.frame(maxThresh_filter)
colnames(maxThresh_filter) <- c("species", "maxThresh", "threshType")
maxThresh_filter$threshType <- gsub(".*__", "", maxThresh_filter$thresh)

maxThresh_filter$threshType <- with(maxThresh_filter, ifelse(
  threshType == "TSSequalTest", "Equal.test.sensitivity.and.specificity.logistic.threshold", ifelse(
    threshType == "TSSequalTrain", "Equal.training.sensitivity.and.specificity.logistic.threshold", ifelse(
      threshType == "TSSequateEntropy", "Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold", ifelse(
        threshType == "TSSmaxTest", "Maximum.test.sensitivity.plus.specificity.logistic.threshold", ifelse(
          threshType == "TSSmaxTrain", "Maximum.training.sensitivity.plus.specificity.logistic.threshold", ifelse(
            threshType == "TSSminTrain", "Minimum.training.presence.logistic.threshold", ifelse(
              threshType == "TSSx10perc", "X10.percentile.training.presence.logistic.threshold", threshType))))))))
# now we know what threshold type each species needs

thresh <- maxThresh_filter
thresh$species <- paste0(thresh$species, ".grd")

########################### Select threshold value for each species

library(data.table)

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResults"))
all_sp_results <- list.files(pattern = ".csv$", full.names = F)

data <- data.frame(matrix(NA, nrow = 0, ncol = 4))
colnames(data) <- c("species", "maxThresh", "threshType", "value")

for (i in all_sp_results) {
setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResults/", i))
maxent <- read.csv("maxentResults.csv")

maxent <- maxent %>% dplyr::select(Species,
                                   Equal.test.sensitivity.and.specificity.logistic.threshold,
                                   Equal.training.sensitivity.and.specificity.logistic.threshold,
                                   Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold,
                                   Maximum.test.sensitivity.plus.specificity.logistic.threshold,
                                   Maximum.training.sensitivity.plus.specificity.logistic.threshold,
                                   Minimum.training.presence.logistic.threshold,
                                   X10.percentile.training.presence.logistic.threshold) 

maxent <- maxent[nrow(maxent),]

long <- melt(setDT(maxent), id.vars = c("Species"), variable.name = "threshType")
long$species <- i
long$species <- gsub(".csv.*", ".grd", long$species)
long <- long %>% dplyr::select(species, threshType, value)
long <- dplyr::left_join(thresh, long) %>% na.omit()

data <- rbind(data, long)

}

data <- data %>% dplyr::select(species, maxThresh, value)
colnames(data) <- c("species", "TSS", "value")
data

# for species in data to match the grid names

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/TSS_results/"))
write.csv(data,quote=FALSE,file="0_all_TSS.csv")


################################################################################### RECLASS TO BINARY MAPS



rasfiles <- data$species
setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent))

for (i in rasfiles) {
  ras <- raster(paste0("modelResultsRasters/", "ave_", i))
  species_name <- paste0(gsub("ave_", "", i, fixed=TRUE))
  reclassVal <- data %>% dplyr::filter(species == species_name)
  reclassVal <- reclassVal$value
  reclassmatrix <- matrix(c(-Inf, reclassVal, 0, reclassVal, Inf, 1), nrow = 2, ncol = 3, byrow = TRUE)
  reclassify(ras, reclassmatrix, filename = paste0("modelResultsBinary/", i, "binary"), overwrite = TRUE)
}

# these binary and continuous maps should represent current distributions

################################################################################# CREATE FISHNETS

#### MAKE FISHNETS
# (COPY AND PASTE FISHNET FROM TRAITS FOLDER, PREVIOUSLY MADE)



####################################################
####################################################
####################################################
#################################################### Associate mean probability of persistence as predicted by model

library(rgdal)
library(sp)
library(maps)
library(maptools)
library(ggplot2)

setwd(paste0("wd/distribution_models/fishnet"))
grid <- readOGR("fishnet100.shp")

setwd(paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResultsRasters"))
predfiles <- list.files(pattern="grd", all.files=FALSE, full.names = FALSE)
path = paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelResultsRasters/")
output = paste0("wd/distribution_models/0MAXENTMODEL_", continent, "/modelEval/prob_presence/")

for (i in predfiles) {
  setwd(path)
  dat <- raster(i)
  pred <- projectRaster(dat, crs = "+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  
  #extract raster values from polygon
  pred <- raster::extract(pred, grid, fun=mean, na.rm=TRUE)
  name <- sapply(grid@polygons, slot, "ID")
  pred_df <- data.frame(ID = name, Value = pred)
  colnames(pred_df) <- c("gridsquare", "pred")
  pred_df <- as.data.frame(pred_df)
  pred_df <- na.omit(pred_df)
  
  setwd(output)
  write.table(pred_df, file = paste0(i, ".csv"), 
              quote = TRUE, sep = ",", row.names = FALSE)
  
}


