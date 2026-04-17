
### analysis on distribution models

########################################################### PUTTING THE TABLE TOGETHER TO
########################################################### ASSESS WHICH SPECIES ARE WELL MODELED

### choose continent on which to focus


library(ggplot2)
library(broom)
library(purrr)
library(readr)
library(tidyverse)
library(dplyr)

continent <- c("NAM")
samplingP1 <- 75          # minimum number of quadrats in which species are sampled in P1
samplingP2 <- 75          # minimum number of quadrats in which species are sampled in P2

allDat <- as.data.frame(matrix(ncol = 7, nrow = 0))
colnames(allDat) <- c("species", "continent", "gridsquare", "presence", "binaryPresenceMAX", "binaryPresenceRF", "binaryPresenceGAM")

probabilityPres <- as.data.frame(matrix(ncol = 6, nrow = 0))
colnames(probabilityPres) <- c("species", "gridsquare", "presence", "predMAX", "predRF", "predGAM")


for (continent in continent) {

#################################################################################################

# create a table with the columns: species, quadrat ID, presence in current time period, 
# probability of persistence prediction, binary prediction

setwd(paste0("wd/distribution_models/"))

presfiles <- list.files(path = paste0(getwd(), "/p2_presence", continent, "/"), pattern=".csv", 
                        all.files=FALSE, full.names = FALSE)
predfilesMAX <- list.files(path = paste0(getwd(), "/maxent/", continent, "/prob_presence/"), 
                           pattern=".tif", all.files=FALSE, full.names = FALSE)
predfilesRF <- list.files(path = paste0(getwd(), "/rf/", continent, "/prob_presence/"), 
                          pattern=".tif", all.files=FALSE, full.names = FALSE)
predfilesGAM <- list.files(path = paste0(getwd(), "/gam/", continent, "/prob_presence/"), 
                           pattern=".tif", all.files=FALSE, full.names = FALSE)

# create table with actual presences
pathP2 = paste0("wd/distribution_models/p2_presence", continent, "/")
resultsTable <- as.data.frame(matrix(ncol = 8, nrow = 0))
colnames(resultsTable)<-c("species","continent","lat", "long", "year", "date", "gridsquare", "presence")
for (i in presfiles) {
  setwd(pathP2)
  dat <- read.csv(i)
  resultsTable <- rbind(resultsTable, dat)
}

#############################################################################################################
########################################################################################### SAMPLING CRITERIA

#
#
#
#
# identify species sufficiently sampled in P1 (number of quadrats in which each species is found)
setwd(paste0("wd/distribution_models/"))
filesP1 <- list.files(path = paste0(getwd(), "/p1_presence", continent, "/"), pattern=".csv", 
                      all.files=FALSE, full.names = FALSE)
p1 <- as.data.frame(matrix(ncol = 2, nrow = 0))
colnames(p1) <- c("species", "countp1")
for (i in filesP1) {
  setwd(paste0("wd/distribution_models/p1_presence", continent, "/"))
  d <- read.csv(i)
  d <- d %>% dplyr::select(species, gridsquare) %>% unique()
  countp1 <- d %>% group_by(species) %>% dplyr::summarize(countp1 = n()) %>% ungroup()
  p1 <- rbind(p1, countp1)
}
p1 <- p1 %>% dplyr::filter(countp1 >= samplingP1) %>% dplyr::select(species, countp1)
assign(paste0(continent, "p1"), p1)


# identify species sufficiently sampled in P2 (number of quadrats in which each species is found)
setwd(paste0("wd/distribution_models/"))
filesP2<- list.files(path = paste0(getwd(), "/p2_presence", continent, "/"), pattern=".csv", 
                      all.files=FALSE, full.names = FALSE)
p2<- as.data.frame(matrix(ncol = 2, nrow = 0))
colnames(p2) <- c("species", "countp2")
for (i in filesP2) {
  setwd(paste0("wd/distribution_models/p2_presence", continent, "/"))
  d <- read.csv(i)
  d <- d %>% dplyr::select(species, gridsquare) %>% unique()
  countp2 <- d %>% group_by(species) %>% dplyr::summarize(countp2 = n()) %>% ungroup()
  p2<- rbind(p2, countp2)
}
p2<- p2 %>% dplyr::filter(countp2 >= samplingP2) %>% dplyr::select(species, countp2)
assign(paste0(continent, "p2"), p2)


###############################################################################################################

#table with MAXENT predictions
pathPred = paste0("wd/distribution_models/maxent/", continent, "/prob_presence/")
probabilityPresence1 <- as.data.frame(matrix(ncol = 2, nrow = 0))
colnames(probabilityPresence1)<-c("gridsquare","pred")
for (i in predfilesMAX) {
  setwd(pathPred)
  dat <- read.csv(i)
  dat$species <- paste0(i)
  dat$species <- gsub(".csv.tif.csv", "", dat$species)
  probabilityPresence1 <- rbind(probabilityPresence1, dat)
}
colnames(probabilityPresence1) <- c("gridsquare", "predMAX", "species")
head(probabilityPresence1)

#table with RANDOM FOREST predictions
pathPred = paste0("wd/distribution_models/rf/", continent, "/prob_presence/")
probabilityPresence2 <- as.data.frame(matrix(ncol = 2, nrow = 0))
colnames(probabilityPresence2)<-c("gridsquare","pred")
for (i in predfilesRF) {
  setwd(pathPred)
  dat <- read.csv(i)
  dat$species <- paste0(i)
  dat$species <- gsub(".csv.tif.csv", "", dat$species)
  probabilityPresence2 <- rbind(probabilityPresence2, dat)
}
colnames(probabilityPresence2) <- c("gridsquare", "predRF", "species")
head(probabilityPresence2)

#table with GAM predictions
pathPred = paste0("wd/distribution_models/gam/", continent, "/prob_presence/")
probabilityPresence3 <- as.data.frame(matrix(ncol = 2, nrow = 0))
colnames(probabilityPresence3)<-c("gridsquare","pred")
for (i in predfilesRF) {
  setwd(pathPred)
  dat <- read.csv(i)
  dat$species <- paste0(i)
  dat$species <- gsub(".csv.tif.csv", "", dat$species)
  probabilityPresence3 <- rbind(probabilityPresence3, dat)
}

colnames(probabilityPresence3) <- c("gridsquare", "predGAM", "species")
head(probabilityPresence3)
unique(probabilityPresence3$species)


#################################################################
# put together probabilities of presence per quadrat (from 3 model predictions) with current observations

resultsTable <- resultsTable %>% dplyr::select(species, gridsquare, presence) %>% unique()
fullTable <- dplyr::full_join(resultsTable, probabilityPresence1)
fullTable <- dplyr::full_join(fullTable, probabilityPresence2)
fullTable <- dplyr::full_join(fullTable, probabilityPresence3)

fullTable$presence[is.na(fullTable$presence)] = 0 # species absences in gridsquares evaluated by models set to a 0
head(fullTable)

# remove NAs because this shows quadrats that were not evaluated by the model
fullTable <- fullTable %>% na.omit()

ContinuousValues <- fullTable

#################################################################### CHOOSE THRESHOLD HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
setwd(paste0("wd/distribution_models/maxent/", continent, "/threshold/"))
dataMAX <- read.csv("allThresholds.csv")
dataMAX <- dataMAX %>% dplyr::select(species, spec_sens) # maximises TSS
colnames(dataMAX) <- c("species", "threshMAX")

setwd(paste0("wd/distribution_models/rf/", continent, "/threshold/"))
dataRF <- read.csv("allThresholds.csv")
dataRF <- dataRF %>% dplyr::select(species, spec_sens) # maximises TSS
colnames(dataRF) <- c("species", "threshRF")

setwd(paste0("wd/distribution_models/gam/", continent, "/threshold/"))
dataGAM <- read.csv("allThresholds.csv")
dataGAM <- dataGAM %>% dplyr::select(species, spec_sens) # maximises TSS
colnames(dataGAM) <- c("species", "threshGAM")

fullTable <- dplyr::left_join(fullTable, dataMAX)
fullTable <- dplyr::left_join(fullTable, dataRF)
fullTable <- dplyr::left_join(fullTable, dataGAM)
head(fullTable)

fullTable$binaryPresenceMAX <- with(fullTable, ifelse(
  predMAX >= threshMAX, 1, 0))
fullTable$binaryPresenceRF <- with(fullTable, ifelse(
  predRF >= threshRF, 1, 0))
fullTable$binaryPresenceGAM <- with(fullTable, ifelse(
  predGAM >= threshGAM, 1, 0))

fullTable <- fullTable %>% dplyr::select(species, gridsquare, presence, 
                           binaryPresenceMAX, binaryPresenceRF, binaryPresenceGAM) %>% unique()

fullT <- fullTable
allDat <- rbind(allDat, fullT)

probabilityPres <- rbind(probabilityPres, ContinuousValues)

}

fullTable <- allDat

rownames(NAMp1) <- NULL
speciesp1 <- NAMp1
speciesp1 <- speciesp1 %>% dplyr::select(species)

rownames(NAMp2) <- NULL
speciesp2 <- NAMp2
speciesp2 <- speciesp2 %>% dplyr::select(species)

# apply all criteria
fullTable <- dplyr::inner_join(fullTable, speciesp1)
fullTable <- dplyr::inner_join(fullTable, speciesp2)

head(fullTable)
unique(fullTable$gridsquare)
unique(fullTable$species)

############################################# find number of quadrats sampled in each time period
persist <- dplyr::full_join(NAMp1, NAMp2)
persist$persistChange <- persist$countp2 - persist$countp1
################################ all species are sampled more often, adding to the argument of 
                                        # comission rates



#####################################################################################################################
# Calculate rate of omission and rate of comission per 100X100 quadrat
# group by species to find % of quadrats where presence = 1 but binary presence = 0
# also find % of quadrats (per species) where presence = 0 but binary presence = 1

# get list of all species and continents
setwd("wd/distribution_models/")

tbl <- as.data.frame(matrix(ncol = 2, nrow = 0))
colnames(tbl)<-c("species","continent")


setwd(paste0("wd/distribution_models/data_byspecies_p1NAM/"))
for (i in continent) {
  continent = "NAM"
  presfiles <- list.files(path = getwd(), pattern=".csv", 
                          all.files=FALSE, full.names = FALSE)
  a <- as.data.frame(presfiles)
  a$presfiles <- str_sub(a$presfiles, end = -5)
  a$cont <- paste0(continent)
  tblNAM <- rbind(tbl, a)
}




###############################################################################################################
error_plots <- FALSE
if (error_plots)
{

# count errors of omissions AND commissions
FN <- fullTable %>% dplyr::filter(presence == 1 & binaryPresenceMAX == 0)
FN <- FN %>% group_by(species) %>% dplyr::summarize(FN = n()) %>% ungroup()
nrow(FN)

FP <- fullTable %>% dplyr::filter(presence == 0 & binaryPresenceMAX == 1)
FP <- FP %>% group_by(species) %>% dplyr::summarize(FP = n()) %>% ungroup()
nrow(FP)

TP <- fullTable %>% dplyr::filter(presence == 1 & binaryPresenceMAX == 1)
TP <- TP %>% group_by(species) %>% dplyr::summarize(TP = n()) %>% ungroup()
nrow(TP)

TN <- fullTable %>% dplyr::filter(presence == 0 & binaryPresenceMAX == 0)
TN <- TN %>% group_by(species) %>% dplyr::summarize(TN = n()) %>% ungroup()
nrow(TN)
#OMISSION = FN/(FN+TP)  (total Reference)
#COMISSION = FP/(FP+TN) (total Predicted)

stats <- dplyr::left_join(FN, FP)
stats <- dplyr::left_join(stats, TP)
stats <- dplyr::left_join(stats, TN)

stats$omRate <- stats$FN/(stats$FN+stats$TP)
stats$comRate <- stats$FP/(stats$FP+stats$TN)

stats <- stats %>% dplyr::select(species, omRate, comRate)
stats

stats <- dplyr::left_join(stats, cont)

stats$omRate[is.na(stats$omRate)] = 0
stats$comRate[is.na(stats$comRate)] = 0

# plot comission and omission rates

ggplot(stats, aes(x=omRate, y=comRate)) +
  geom_point()

setwd("wd/distribution_models/all_figures/")
#ggsave(paste0("om_com_", continent, ".pdf"))

##########################################################################################

# count errors of omissions AND commissions
FN <- fullTable %>% dplyr::filter(presence == 1 & binaryPresenceRF == 0)
FN <- FN %>% group_by(species) %>% dplyr::summarize(FN = n()) %>% ungroup()
nrow(FN)

FP <- fullTable %>% dplyr::filter(presence == 0 & binaryPresenceRF == 1)
FP <- FP %>% group_by(species) %>% dplyr::summarize(FP = n()) %>% ungroup()
nrow(FP)

TP <- fullTable %>% dplyr::filter(presence == 1 & binaryPresenceRF == 1)
TP <- TP %>% group_by(species) %>% dplyr::summarize(TP = n()) %>% ungroup()
nrow(TP)

TN <- fullTable %>% dplyr::filter(presence == 0 & binaryPresenceRF == 0)
TN <- TN %>% group_by(species) %>% dplyr::summarize(TN = n()) %>% ungroup()
nrow(TN)

#OMISSION = FN/(FN+TP)  (total Reference)
#COMISSION = FP/(FP+TN) (total Predicted)

stats <- dplyr::left_join(FN, FP)
stats <- dplyr::left_join(stats, TP)
stats <- dplyr::left_join(stats, TN)

stats$omRate <- stats$FN/(stats$FN+stats$TP)
stats$comRate <- stats$FP/(stats$FP+stats$TN)

stats <- stats %>% dplyr::select(species, omRate, comRate)
stats

stats <- dplyr::left_join(stats, cont)

stats$omRate[is.na(stats$omRate)] = 0
stats$comRate[is.na(stats$comRate)] = 0

# plot comission and omission rates

ggplot(stats, aes(x=omRate, y=comRate)) +
  geom_point()

##############################################################################################

# count errors of omissions AND commissions
FN <- fullTable %>% dplyr::filter(presence == 1 & binaryPresenceGAM == 0)
FN <- FN %>% group_by(species) %>% dplyr::summarize(FN = n()) %>% ungroup()
nrow(FN)

FP <- fullTable %>% dplyr::filter(presence == 0 & binaryPresenceGAM == 1)
FP <- FP %>% group_by(species) %>% dplyr::summarize(FP = n()) %>% ungroup()
nrow(FP)

TP <- fullTable %>% dplyr::filter(presence == 1 & binaryPresenceGAM == 1)
TP <- TP %>% group_by(species) %>% dplyr::summarize(TP = n()) %>% ungroup()
nrow(TP)

TN <- fullTable %>% dplyr::filter(presence == 0 & binaryPresenceGAM == 0)
TN <- TN %>% group_by(species) %>% dplyr::summarize(TN = n()) %>% ungroup()
nrow(TN)

#OMISSION = FN/(FN+TP)  (total Reference)
#COMISSION = FP/(FP+TN) (total Predicted)

stats <- dplyr::left_join(FN, FP)
stats <- dplyr::left_join(stats, TP)
stats <- dplyr::left_join(stats, TN)

stats$omRate <- stats$FN/(stats$FN+stats$TP)
stats$comRate <- stats$FP/(stats$FP+stats$TN)

stats <- stats %>% dplyr::select(species, omRate, comRate)
stats

stats <- dplyr::left_join(stats, cont)

stats$omRate[is.na(stats$omRate)] = 0
stats$comRate[is.na(stats$comRate)] = 0

# plot comission and omission rates

ggplot(stats, aes(x=omRate, y=comRate)) +
  geom_point()

}


# consider the need to normalize data (to differentiate between high likelihood of presence vs. low),
        # even though all model types calculate probability of presence with different algorithms
# zi = (xi - min(x)) / (max(x) - min(x))

boyce <- FALSE
if (boyce)
{
# calculate the Boyce index, to find out which species are temporally transferable

#############################################################  MAXENT
library(raster)
library(ecospat)

#create table to store all Boyce index values
df <- as.data.frame(matrix(ncol = 2, nrow = 0)) 
colnames(df) <- c("species", "boyce")

#load all rasters
setwd("wd/distribution_models/maxent/NAM/rasterResults/")
rasters <- list.files(pattern = ".tif$", full.names = F)

setwd("wd/distribution_models/")
for (i in rasters){
  
fit <- raster(paste0("maxent/NAM/rasterResults/", i))
values(fit)[is.na(values(fit))] <- 0 #reclassify values to be finite (for Boyce index)
values(fit) <- (values(fit)-min(values(fit)))/(max(values(fit))-min(values(fit)))

  obs_files <- substr(i, 1, nchar(i)-4)
obs <- read.csv(paste0("p2_presenceNAM/", obs_files, ".csv"))
obs <- obs %>% dplyr::select(long, lat)

boyce <- ecospat.boyce(fit, obs, nclass = 0, window.w = "default", res = 100)$cor
species <- c(substr(i, 1, nchar(i)-4))

ind <- data.frame(species, boyce)
df <- rbind(df, ind)
}

maxentBoyce <- df
colnames(maxentBoyce) <- c("species", "MaxInd")


#############################################################  RANDOM FOREST

#create table to store all Boyce index values
df <- as.data.frame(matrix(ncol = 2, nrow = 0)) 
colnames(df) <- c("species", "boyce")

#load all rasters
setwd("wd/distribution_models/rf/NAM/rasterResults/")
rasters <- list.files(pattern = ".tif$", full.names = F)

setwd("wd/distribution_models/")
for (i in rasters){
  
  fit <- raster(paste0("rf/NAM/rasterResults/", i))
  values(fit)[is.na(values(fit))] <- 0 #reclassify values to be finite (for Boyce index)
  values(fit) <- (values(fit)-min(values(fit)))/(max(values(fit))-min(values(fit)))
  
  obs_files <- substr(i, 1, nchar(i)-4)
  obs <- read.csv(paste0("p2_presenceNAM/", obs_files, ".csv"))
  obs <- obs %>% dplyr::select(long, lat)
  
  boyce <- ecospat.boyce(fit, obs, nclass = 0, window.w = "default", res = 100)$cor
  species <- c(substr(i, 1, nchar(i)-4))
  
  ind <- data.frame(species, boyce)
  df <- rbind(df, ind)
}

randomForest <- df
colnames(randomForest) <- c("species", "RFInd")

#############################################################  GAMs

#create table to store all Boyce index values
df <- as.data.frame(matrix(ncol = 2, nrow = 0)) 
colnames(df) <- c("species", "boyce")

#load all rasters
setwd("wd/distribution_models/gam/NAM/rasterResults/")
rasters <- list.files(pattern = ".tif$", full.names = F)

setwd("wd/distribution_models/")
for (i in rasters){
  fit <- raster(paste0("gam/NAM/rasterResults/", i))
  values(fit)[is.na(values(fit))] <- 0 #reclassify values to be finite (for Boyce index)
  values(fit) <- (values(fit)-min(values(fit)))/(max(values(fit))-min(values(fit)))
  
  obs_files <- substr(i, 1, nchar(i)-4)
  obs <- read.csv(paste0("p2_presenceNAM/", obs_files, ".csv"))
  obs <- obs %>% dplyr::select(long, lat)
  
  boyce <- ecospat.boyce(fit, obs, nclass = 0, window.w = "default", res = 100)$cor
  species <- c(substr(i, 1, nchar(i)-4))
  
  ind <- data.frame(species, boyce)
  df <- rbind(df, ind)
}

GAMboyce <- df
colnames(GAMboyce) <- c("species", "gamInd")

# bind all boyce index values from 3 different model types
BoyceInd <- dplyr::left_join(maxentBoyce, randomForest)
BoyceInd <- dplyr::left_join(BoyceInd, GAMboyce)
BoyceInd$species <- c(substr(BoyceInd$species, 1, nchar(BoyceInd$species)-4)) # clean sp names

plot(BoyceInd$MaxInd~BoyceInd$RFInd)
plot(BoyceInd$MaxInd~BoyceInd$gamInd)
plot(BoyceInd$RFInd~BoyceInd$gamInd)

mean(BoyceInd$MaxInd)
mean(BoyceInd$RFInd)
mean(BoyceInd$gamInd)

setwd("wd/distribution_models/")
write.table(BoyceInd, "boyce.csv", quote = TRUE, sep = ",", row.names = FALSE)
}



#load boyce index per species per model type
setwd("wd/distribution_models/")
BoyceInd <- read.csv("boyce.csv")
BoyceInd5 <- BoyceInd

BoyceInd$model_cat <- with(BoyceInd, ifelse(
  MaxInd > 0.4 & RFInd > 0.4 & gamInd > 0.4, 1, 0))
hist(BoyceInd$model_cat)
# gam and Maxent seem to provide better models

summar <- BoyceInd %>% group_by(model_cat) %>% dplyr::summarize(fr = n()) %>% ungroup()
summar
worstSpecies <- BoyceInd %>% dplyr::filter(model_cat == 0) %>% dplyr::select(species)
bestSpecies <- BoyceInd %>% dplyr::filter(model_cat == 1) %>% dplyr::select(species)



####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################

######################### TESTS OF PREDICTIVE ABILITY OF PROBABILITY OF PRESENCE TO ACTUAL PRESENCE

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(rcompanion)

####################### # testing if results are the same when restricting to only final quadrats

#colnames(finalQuadrats) <- c("gridsquare")
#finalQuadrats$gridsquare <- as.integer(finalQuadrats$gridsquare)
#probabilityPres <- dplyr::inner_join(probabilityPres, finalQuadrats)

########################

var <- c("predMAX", "predRF", "predGAM")
matrix <- as.data.frame(matrix(ncol = 6, nrow = 0))
colnames(matrix) <- c("coef", "pvalue", "variable", "species", "Rsquare", "varType")

for (modeltype in var){
  
  #species11 <- as.data.frame(unique(tab_comT$species))
  #colnames(species11) <- c("species")
  #probPres <- dplyr::inner_join(probabilityPres, species11)
  
  probPres <- probabilityPres
  
  warn <- capture.output({for (i in unique(probPres$species)){
    probPresS <- probPres %>% dplyr::filter(species == i)
    
    test <- glm(presence~get(modeltype), family = "binomial", data = probPresS)
    
    # extract coefficieints
    coef <- coef(summary(test))[,1]
    
    #extract pvalue
    pvalue <- coef(summary(test))[,4]
    
    #extract R square
    nagel <- nagelkerke(test)$Pseudo.R.squared.for.model.vs.null
    nagel <- nagel[3,]
    
    df <- data.frame(coef, pvalue)
    df$variable <- rownames(df)
    rownames(df) <- NULL
    
    df$species <- paste0(i)
    
    df$Rsquare <- nagel
    
    df$varType <- modeltype
    
    matrix <- rbind(matrix, df)
  }
  })
}

### all predictive accuracy values
predModel <- matrix %>% dplyr::filter(variable == "get(modeltype)")

# save for the code that creates a table
setwd("wd/distribution_models/")
write.table(predModel, "glmBinomialResultsTable1.csv", quote = TRUE, sep = ",", row.names = FALSE)  

  maxPredModel <- predModel %>% dplyr::filter(varType == "predMAX")
      maxPredModel %>% dplyr::filter(pvalue > 0.05) # all species p<0.05
      maxPredModel %>% dplyr::filter(coef < 0) # all species corf is positive
      range(maxPredModel$Rsquare) # r square varies between 1% and 65% with mean of 28%
        mean(maxPredModel$Rsquare)
        mean(BoyceInd5$MaxInd)# mean Boyce of 0.87
        range(BoyceInd5$MaxInd) # range Boyce 0.15 to 0.99
            # number of species with satisfactory boyce index:
          nrow(BoyceInd5) - nrow(BoyceInd5 %>% dplyr::filter(MaxInd >= 0)) # 0 species 
  
  gamPredModel <- predModel %>% dplyr::filter(varType == "predGAM")
      gamPredModel %>% dplyr::filter(pvalue > 0.05) # 2 species have p > 0.05
      gamPredModel %>% dplyr::filter(coef < 0) # no species have negative coef
      range(gamPredModel$Rsquare) # r square varies between 0.5% and 64%, mean of 23%
        mean(gamPredModel$Rsquare)
        mean(BoyceInd5$gamInd) # mean Boyce of 0.44
        range(BoyceInd5$gamInd) # range Boyce -0.751 to 0.96
          nrow(BoyceInd5) - nrow(BoyceInd5 %>% dplyr::filter(gamInd >= 0)) # 21 species
  
  rfPredModel <- predModel %>% dplyr::filter(varType == "predRF")
      rfPredModel %>% dplyr::filter(pvalue > 0.05) # all species have p <0.05
      rfPredModel %>% dplyr::filter(coef < 0) # no species have negative coef
      range(rfPredModel$Rsquare) # r square varies between 1% and 66%, mean of 23%
        mean(gamPredModel$Rsquare)
        mean(BoyceInd5$RFInd) # mean Boyce of 0.56
        range(BoyceInd5$RFInd) # range Boyce -0.70 to 0.99
          nrow(BoyceInd5) - nrow(BoyceInd5 %>% dplyr::filter(RFInd >= 0)) # 13 species 

# name well modeled species and badly modeled species:
maxbestsp <- maxPredModel %>% dplyr::select(species, coef, pvalue, Rsquare)
    colnames(maxbestsp) <- c("species", "coefMax", "pvalueMax", "RsquareMax")
gambestsp <- gamPredModel %>% dplyr::select(species, coef, pvalue, Rsquare)
    colnames(gambestsp) <- c("species", "coefGam", "pvalueGam", "RsquareGam")
rfbestsp <- rfPredModel %>% dplyr::select(species, coef, pvalue, Rsquare)
    colnames(rfbestsp) <- c("species", "coefRf", "pvalueRf", "RsquareRf")
merge <- dplyr::left_join(maxbestsp, gambestsp)
merge <- dplyr::left_join(merge, rfbestsp)
merge <- dplyr::left_join(merge, BoyceInd5)
          
mergebest <- merge %>% dplyr::filter(MaxInd >= 0.8 & gamInd >= 0.8 & RFInd >=0.8 &
                      RsquareMax >= 0.4 & RsquareGam >= 0.4 & RsquareRf >= 0.4) %>%
                      dplyr::select(species, RsquareMax, RsquareGam, RsquareRf, MaxInd, RFInd, gamInd)
mergebest

mergeworst <- merge %>% dplyr::filter(MaxInd <= 0.5 & gamInd <= 0.5 & RFInd <= 0.5 &
                                           RsquareMax <= 0.15 & RsquareGam <= 0.15 & RsquareRf <= 0.15) %>%
  dplyr::select(species, RsquareMax, RsquareGam, RsquareRf, MaxInd, RFInd, gamInd)

mergeworst

########################### ASSESS TEMPORAL TRANSFERABILITY - how many species are transferable

critMin <- merge %>% dplyr::filter(MaxInd >= 0.4 & gamInd >= 0.4 & RFInd >= 0.4 &
                        RsquareMax >= 0.05 & RsquareGam >= 0.05 & RsquareRf >= 0.05) %>%
                        dplyr::select(species, RsquareMax, RsquareGam, RsquareRf, MaxInd, RFInd, gamInd)
####
critMax <- merge %>% dplyr::filter(MaxInd >= 0.8 & gamInd >= 0.8 & RFInd >= 0.8 &
                        RsquareMax >= 0.3 & RsquareGam >= 0.3 & RsquareRf >= 0.3) %>%
                        dplyr::select(species, RsquareMax, RsquareGam, RsquareRf, MaxInd, RFInd, gamInd)
###
nrow(critMin)/nrow(merge) # 51% of species are transferable at minimum criteria
nrow(critMax)/nrow(merge) # 3% of species are transferable at maximum criteria




#########################


predVal <- matrix %>% dplyr::filter(variable == "get(modeltype)" 
                                    & pvalue <= 0.05 & coef > 0)

rsquare_pvalue <- matrix %>% dplyr::select(species, Rsquare, varType, coef) %>% unique()
row.names(rsquare_pvalue) <- NULL

#save table for use in the phylogenetic signal test
setwd("wd/distribution_models/")
write.table(rsquare_pvalue, "TransModel.csv", quote = TRUE, sep = ",", row.names = FALSE)

rSquarePlot <- ggplot(rsquare_pvalue, aes(x=Rsquare, group=varType, fill=varType)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()
rSquarePlot

setwd("wd/distribution_models/all_figures/")
#ggsave(paste0("TemporalTransferabilityGLM.pdf"))


#### dataframe to pair down to species that are best modeled***************************************

bestRsquare <- matrix %>% dplyr::filter(Rsquare <= 0.05) %>% 
                          dplyr::select(species) %>% unique()
bestRsquare$design <- "worst"

######################################### TESTS BETWEEN HOW WELL MODELS FIT ~ SPECIES DECLINES

setwd("wd/distribution_models/")
a <- read.csv("Ch1range_loss.csv")
A <- read.csv("boyce.csv")
ss <- matrix %>% dplyr::filter(variable == "get(modeltype)")
a <- dplyr::left_join(ss, a)
a <- dplyr::left_join(a, A) %>% na.omit() 

#hist(a$decline_proportion)
#qqnorm(a$decline_proportion)
b <- a %>% dplyr::filter(varType =="predMAX")
plot(b$decline_proportion ~ b$Rsquare)
test <- glm(Rsquare~decline_proportion, data = b)
  summary(test)
  nagelkerke(test)
plot(b$decline_proportion ~ b$MaxInd)
test <- glm(MaxInd~decline_proportion, data = b)
  summary(test)
  nagelkerke(test)
  
            fig <- b %>% dplyr::filter(decline_proportion >= 0.5 & MaxInd <= 0.7)
            fig
            # use aeshna juncea
            fig <- b %>% dplyr::filter(decline_proportion <= 0.1 & MaxInd >= 0.9)
            fig
            # use calopteryx maculata
  
  
b <- a %>% dplyr::filter(varType =="predRF")
plot(b$decline_proportion ~ b$Rsquare)
test <- glm(Rsquare~decline_proportion, data = b)
  summary(test)
  nagelkerke(test)
plot(b$decline_proportion ~ b$RFInd)
test <- glm(RFInd~decline_proportion, data = b)
  summary(test)
  nagelkerke(test)
  
b <- a %>% dplyr::filter(varType =="predGAM")
plot(b$decline_proportion ~ b$Rsquare)
test <- glm(Rsquare~decline_proportion, data = b)
  summary(test)
  nagelkerke(test)
plot(b$decline_proportion ~ b$gamInd)
test <- glm(gamInd~decline_proportion, data = b)
  summary(test)
  nagelkerke(test)
#########################################

#identify species with some degree of reasonable predictive ability
mx <- predVal %>% dplyr::filter(varType == "predMAX" & Rsquare >= 0)
mx <- as.data.frame(mx$species)
colnames(mx) <- c("species")
rf <- predVal %>% dplyr::filter(varType == "predRF" & Rsquare >= 0)
rf <- as.data.frame(rf$species)
colnames(rf) <- c("species")
gm <- predVal %>% dplyr::filter(varType == "predGAM" & Rsquare >= 0)
gm <- as.data.frame(gm$species)
colnames(gm) <- c("species")

mx <- dplyr::inner_join(mx, rf)
mx <- dplyr::inner_join(mx, gm)
mx

#merge with boyce info
setwd("wd/distribution_models/")
BoyceInd <- read.csv("boyce.csv")
BoyceMX <- BoyceInd %>% dplyr::select(species, MaxInd)
mx <- predVal %>% dplyr::filter(varType == "predMAX")
BoyceMX <- dplyr::inner_join(BoyceMX, mx)
plot(BoyceMX$MaxInd, BoyceMX$Rsquare)
cor.test(BoyceMX$MaxInd, BoyceMX$Rsquare, method = "pearson")[4]

BoyceRF <- BoyceInd %>% dplyr::select(species, RFInd)
rf <- predVal %>% dplyr::filter(varType == "predRF")
BoyceRF <- dplyr::inner_join(BoyceRF, rf)
plot(BoyceRF$RFInd, BoyceRF$Rsquare)
cor.test(BoyceRF$RFInd, BoyceRF$Rsquare, method = "pearson")[4]

BoyceGM <- BoyceInd %>% dplyr::select(species, gamInd)
gm <- predVal %>% dplyr::filter(varType == "predGAM")
BoyceGM <- dplyr::inner_join(BoyceGM, gm)
plot(BoyceGM$gamInd, BoyceGM$Rsquare)
cor.test(BoyceGM$gamInd, BoyceGM$Rsquare, method = "pearson")[4]

######### Are species that are well modeled also more likely to respond well?
                  # PREPARE RESPONSE DATA HERE BUT INCORPORATE IN MODELS BELOW AS WELL

setwd("wd/distribution_models/RESPONSESCHAPTER2")
res <- read.csv("damseldragon.csv") 
res <- res %>% dplyr::select(species, final_phen, exp_km)
response_per_species <- res
res <- dplyr::left_join(res, BoyceInd) %>% na.omit()

# likely no relationship to boyce index, but see if there is a relationship with comission errors below
#########


# is there a relationship between predictive ability and response ?
response_per_species <- dplyr::left_join(response_per_species, BoyceMX)
modRes <- glm(MaxInd ~ final_phen + exp_km, data = response_per_species)
summary(modRes)
# NO RESPNSE

########################################################################
########################################################################


####################################################################################################################
####################################################################################################################
####################################################################################################################

# create map with errors of comission for each model, AND only for overlapping errors

map <- FALSE
if (map)
{

library(raster)
library(sp)
library(rgdal)
library(dplyr)

datmap <- fullTable
datmap <- dplyr::left_join(datmap, cont)
datmap <- datmap %>% dplyr::filter(continent == "NAM")

datmap$com_overlap <- with(datmap, ifelse(
  presence == 0 & binaryPresenceMAX == 1 & binaryPresenceGAM == 1 & binaryPresenceRF == 1, 1, NA))

datmap <- datmap %>% dplyr::select(species, gridsquare, com_overlap) %>% na.omit()

datmap <- datmap %>% group_by(gridsquare) %>% dplyr::summarise(freq = n()) %>% ungroup()
datmap <- as.data.frame(datmap)
datmap$gridsquare <- as.character(datmap$gridsquare)

setwd(paste0("wd/distribution_models/fishnet_for_map"))
grid <- readOGR("fishnet100.shp")
#save the attribute table
write.table(grid@data,"att_table.csv",quote=T,row.names=F,sep=",") 
att_table <- read.csv("att_table.csv")
att_table$gridsquare <- rownames(att_table)

att_table <- dplyr::left_join(att_table, datmap)

# merge to the attribute table of the fishnet
grid@data$error <- att_table$freq
writeOGR(grid, ".", "errorGrid", driver="ESRI Shapefile", overwrite_layer=TRUE) 

# feature to raster tool in arcmap

}

####################################################################################################################
####################################################################################################################
####################################################################################################################

# bestSpecies are those with Boyce > 0.4
# combine with those with rSquare > 5%

bestSpeciesT <- dplyr::left_join(bestSpecies, bestRsquare) %>% filter_all(any_vars(is.na(.))) 
bestSpeciesT <- bestSpeciesT %>% dplyr::select(species)
bestSpeciesT

worstSpeciesT <- dplyr::left_join(worstSpecies, bestRsquare) %>% na.omit()
worstSpeciesT <- worstSpeciesT %>% dplyr::select(species)
worstSpeciesT


best_worst_pairDown <- bestSpeciesT # bestSpeciesT vs worstSpeciesT ***************************************

quant <- 0.25

runAll <- TRUE
if (runAll)
{

finalPrep <- TRUE
if (finalPrep)
{

library(rcompanion) # for function nagelkerke
library(MASS) # for negative binomial GLM
  
tab <- fullTable

tab <- dplyr::left_join(tab, cont)
tab <- tab %>% dplyr::filter(continent == "NAM")

tab <- tab %>% dplyr::select(species, gridsquare, presence, binaryPresenceMAX, binaryPresenceRF, 
                                   binaryPresenceGAM) %>% unique()
tab <- dplyr::left_join(tab, cont)
head(tab)

############################## add mean trait per quadrat
tab55 <- tab %>% dplyr::filter(presence == 1)
res1 <- dplyr::left_join(tab55, res)
res1 <- res1 %>% dplyr::select(species, gridsquare, exp_km, final_phen) %>% na.omit()
res_expansion <- res1 %>% dplyr::group_by(gridsquare) %>% 
                         dplyr::summarise(mean_exp = mean(exp_km)) %>% ungroup()
res_phenology <- res1 %>% dplyr::group_by(gridsquare) %>% 
  dplyr::summarise(mean_phen = mean(final_phen)) %>% ungroup()
res1 <- dplyr::full_join(res_expansion, res_phenology)
res1 <- res1 %>% mutate_at(c(2,3), funs(c(scale(., center = TRUE, scale = TRUE))))
############################

# ID gridsquares where all three models show ERRORS OF COMISSION

tab$errorTypeC <- with(tab, ifelse(
  presence == 0 & binaryPresenceMAX == 1 & binaryPresenceGAM == 1 & binaryPresenceRF == 1, 2, ifelse(
  #models agree and accurately identify species presence
  presence == 1 & binaryPresenceMAX == 1 & binaryPresenceGAM == 1 & binaryPresenceRF == 1, 1, NA)))


tab <- dplyr::inner_join(tab, best_worst_pairDown) # choose best or worst *****************************
unique(tab$species)

tab <- tab %>% dplyr::select(species, gridsquare, errorTypeC) %>% unique() %>% na.omit()


############################################# create a table with sampling change per quadrat

####### add sampling change to main dataframe
# step 1:
speciesCorrect <- tab %>% dplyr::select(species) %>% unique()
rownames(speciesCorrect) <- NULL

sampling <- TRUE
if (sampling)
{
  ####### for P2
  
  # identify species sufficiently sampled in p2 (number of quadrats in which each species is found)
  setwd(paste0("wd/distribution_models/"))
  filesp2 <- list.files(path = paste0(getwd(), "/p2_presence", continent, "/"), pattern=".csv", 
                        all.files=FALSE, full.names = FALSE)
  p2 <- as.data.frame(matrix(ncol = 2, nrow = 0))
  colnames(p2) <- c("gridsquare", "species")
  for (i in filesp2) {
    setwd(paste0("wd/distribution_models/p2_presence", continent, "/"))
    d <- read.csv(i)
    d <- d %>% dplyr::select(gridsquare, species) %>% unique()
    p2 <- rbind(p2, d)
  }
  p2 <- dplyr::inner_join(p2, speciesCorrect)
  count <- p2 %>% group_by(gridsquare) %>% dplyr::summarize(countp2 = n()) %>% ungroup()

}



#insert criteria: sampling in p2 is sufficient 
  # remove the nth worst sampled quadrats
  #only keep the top n % best sampled quadrats places

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
percentilesample = count$countp2
sample_criteria <- as.data.frame(quantile(percentilesample, c(quant)))[,1] # richness threshold
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

countX <- count %>% dplyr::filter(countp2 >= sample_criteria)
countX <- countX %>% mutate_at(c(2),funs(c(scale(., center = TRUE, scale = TRUE))))

tab <- dplyr::inner_join(tab, countX)


########################################################## CLIM VARIABILITY
    # number of years that exceed historical mean by n percent
# number of years that exceed historical mean by n percent
setwd("wd/distribution_models/climate/tmx/")
clim_all_years <- read.csv("climateVariability.csv")
rownames(clim_all_years) <- clim_all_years$gridsquare_100
clim_all_years <- clim_all_years[-c(1)]

clim_all_yearsP1 <- clim_all_years[,1:85] # select values for P1 (1901 - 1985)
clim_all_yearsP1A <- clim_all_yearsP1
clim_all_yearsP2 <- clim_all_years[,100:120] # select values for P2 (2000 - 2020)
clim_all_yearsP2A <- clim_all_yearsP2

clim_all_yearsP1$climateVariabilityMax<-apply(clim_all_yearsP1,1,FUN=max)
clim_all_yearsP1A$climateVariabilityMin<-apply(clim_all_yearsP1A,1,FUN=min)
clim_all_yearsP1$climateVariabilityMin <- clim_all_yearsP1A$climateVariabilityMin
clim_all_yearsP1$gridsquare_100 <- rownames(clim_all_yearsP1)
rownames(clim_all_yearsP1) <- NULL
clim_all_yearsP1 <- dplyr::select(clim_all_yearsP1, gridsquare_100, 
                                  climateVariabilityMax, climateVariabilityMin) %>% unique()
clim_all_yearsP1$climateVariability <- clim_all_yearsP1$climateVariabilityMax - clim_all_yearsP1$climateVariabilityMin 

clim_all_yearsP2$climateVariabilityMax<-apply(clim_all_yearsP2,1,FUN=max)
clim_all_yearsP2A$climateVariabilityMin<-apply(clim_all_yearsP2A,1,FUN=min)
clim_all_yearsP2$climateVariabilityMin <- clim_all_yearsP2A$climateVariabilityMin
clim_all_yearsP2$gridsquare_100 <- rownames(clim_all_yearsP2)
rownames(clim_all_yearsP2) <- NULL
clim_all_yearsP2 <- dplyr::select(clim_all_yearsP2, gridsquare_100,
                                  climateVariabilityMax, climateVariabilityMin) %>% unique()
clim_all_yearsP2$climateVariability <- clim_all_yearsP2$climateVariabilityMax - clim_all_yearsP2$climateVariabilityMin 

colnames(clim_all_yearsP2) <- c("gridsquare_100", "climateVariabilityMax2", "climateVariabilityMin2", 
                                "climateVariability2")

clim_var <- dplyr::left_join(clim_all_yearsP1, clim_all_yearsP2)
clim_var$delta_var <- clim_var$climateVariability2/clim_var$climateVariability

clim_recent_years <- clim_var %>% dplyr::select(gridsquare_100, climateVariability, climateVariability2, delta_var)

colnames(clim_recent_years) <- c("gridsquare", "climateVariability1", 
                                 "climateVariability2", "climateVariability")

head(clim_recent_years)
#################################################################

setwd("wd/distribution_models/covariatesPrep/finaldataframes")
crop <- read.csv("cropland.csv")
pest <- read.csv("pesticides.csv")
  colnames(pest) <- c("gridsquare", "pest")
pretmx <- read.csv("pre_tmx.csv")
pop <- read.csv("pop.csv")
past <- read.csv("past.csv")
protect <- read.csv("ProtectedAreas.csv")
landcover <- read.csv("Landcover.csv")
  colnames(landcover) <- c("gridsquare", "natArea", "total", "NaturalAreasPercent")

covariates <- dplyr::left_join(crop, pretmx)
covariates <- dplyr::left_join(covariates, pop)
covariates <- dplyr::left_join(covariates, past)
covariates <- dplyr::left_join(covariates, pest)
covariates <- dplyr::left_join(covariates, protect)
covariates <- dplyr::left_join(covariates, landcover)
covariates <- covariates %>% na.omit()
rownames(covariates) <- NULL
covariates$gridsquare <- as.character(covariates$gridsquare)
covariates$delta_tmx <- covariates$tmx2 - covariates$tmx1
covariates$delta_pre <- covariates$pre2 - covariates$pre1
covariates$delta_crop <- covariates$crop2 - covariates$crop1
covariates$delta_pop <- covariates$pop2 - covariates$pop1
covariates$delta_past <- covariates$past2 - covariates$past1
###############################################################

# merge covariates and error table
tab$gridsquare <- as.character(tab$gridsquare)
tab <- dplyr::left_join(tab, clim_recent_years)
tab <- dplyr::left_join(tab, covariates)

tab <- tab %>% na.omit()

head(tab)

tab_comN <- tab

wiltest <- FALSE
if (wiltest)
{
# comparing places of error (ALL MODELS AGREE) to everywhere else
library(MuMIn)
library(lme4)

table <- as.data.frame(matrix(ncol = 5, nrow = 0))
colnames(table) <- c("species", "coef", "pvalue", "nagel", "WilcoxPvalue")
for (i in unique(tab_com$species)){
  tab_com1 <- tab_com %>% dplyr::filter(species == paste0(i))
  
  #ggplot(tab_com1, aes(x=as.factor(errorTypeC), y=climateVariability)) + geom_boxplot()
  #setwd("wd/distribution_models/all_figures/boxplotErrors")
  #ggsave(paste0(i, ".pdf"))
  
  WilTest <- wilcox.test(tab_com1$climateVariability ~ tab_com1$errorTypeC)
    WilcoxPvalue <- as.data.frame(WilTest[3])
    colnames(WilcoxPvalue) <- c("WilcoxPvalue")
  mod <- glm(errorTypeC ~ climateVariability, family = "binomial", data = tab_com1)
  r_square <- as.data.frame(nagelkerke(mod)[2])
  nagel <- c(r_square[3,])
  coef <- c(coef(summary(mod))[,'Estimate'][2])
  pvalue <- c(coef(summary(mod))[,'Pr(>|z|)'][2])
  species <- c(i)
  tab <- data.frame(species, coef, pvalue, nagel, WilcoxPvalue)
  row.names(tab) <- NULL
  table <- rbind(table, tab)
  }

unique(table$species)
negRel <- table %>% dplyr::filter(pvalue <= 0.05 & coef <= 0 & WilcoxPvalue <= 0.05)
posRel <- table %>% dplyr::filter(pvalue <= 0.05 & coef >= 0 & WilcoxPvalue <= 0.05)
significant <- table %>% dplyr::filter(pvalue <= 0.05 & WilcoxPvalue <= 0.05)
nonSig <- nrow(table) - nrow(significant)

nrow(negRel)
nrow(posRel)
nrow(significant)
nonSig

plot(table$nagel ~ table$coef)

sp <- significant %>% dplyr::select(species)
tab_com <- dplyr::inner_join(tab_com, sp)
}


#########################################################################################################
#########################################################################################################

# comparing probability that species will be badly modeled

###################################################################### COMISSION ERRORS
tabProbC <- tab %>% group_by(errorTypeC, gridsquare) %>% dplyr::summarize(freq = n()) %>% ungroup()
tabProbC$freq <- as.character(tabProbC$freq)

t0 <- tabProbC %>% dplyr::filter(errorTypeC == 1) %>% dplyr::select(gridsquare, freq)
colnames(t0) <- c("gridsquare", "freq0")
t1 <- tabProbC %>% dplyr::filter(errorTypeC == 2) %>% dplyr::select(gridsquare, freq)
colnames(t1) <- c("gridsquare", "freq1")
t <- dplyr::full_join(t0, t1)

##!!!! NEW
t <- t %>% na.omit()
###

#t$freq0[is.na(t$freq0)] = 0
#t$freq1[is.na(t$freq1)] = 0
t$freq0 <- as.numeric(t$freq0)
t$freq1 <- as.numeric(t$freq1)
t$sum <- t$freq0 + t$freq1
t$prob1 <- t$freq1/t$sum          # probability of getting a 1 (error) per quadrat
t <- t %>% dplyr::select(gridsquare, prob1, freq0, freq1)

tabC <- dplyr::left_join(tab, t) %>% 
        dplyr::select(gridsquare, countp2, climateVariability1, climateVariability2, climateVariability,
                      prob1, delta_crop, delta_pop, delta_past,
                      crop2, past2, pop2, freq0, freq1, pest, PA_index, NaturalAreasPercent) %>% 
                      unique()

# scale all variables
tabC <- tabC %>% mutate_at(c(2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 15, 16), 
                             funs(c(scale(., center = TRUE, scale = TRUE))))


#########################################################################################
#########################################################################################

library(rcompanion)
library(mgcv)

# prob1 is the probability of encountering an error vs. success (by all models)
# freq1 is the count data of the number of species with comission error 
# freq 0 is count data of no error by all models

# remove 0s 
tabCprob <- tabC %>% dplyr::select(gridsquare, countp2, climateVariability1, climateVariability2, 
                                   climateVariability, delta_crop, delta_pop, 
                               delta_past, crop2, past2, pop2, prob1, pest, 
                               PA_index, NaturalAreasPercent) %>% 
                               na.omit()
tabC1 <- tabC %>% dplyr::select(gridsquare, countp2, climateVariability1, climateVariability2, 
                                climateVariability, delta_crop, delta_pop, 
                                delta_past, crop2, past2, pop2, freq1, pest, 
                                PA_index, NaturalAreasPercent) %>% 
                                na.omit()
tabC0 <- tabC %>% dplyr::select(gridsquare, countp2, climateVariability1, climateVariability2, 
                                climateVariability, delta_crop, delta_pop, 
                                delta_past, crop2, past2, pop2, freq0, pest,
                                PA_index, NaturalAreasPercent) %>% 
                                na.omit()

print(sample_criteria)
}


tab_comT <- tab_comN %>% unique()
nrow(as.data.frame(unique(tab_comT$species)))
sample_criteria

binomial_tests <- TRUE
if (binomial_tests)
{
  
  tab_comT$errorTypeC <- as.numeric(tab_comT$errorTypeC)
  tab_comT$errorTypeC <- with(tab_comT, ifelse(
    errorTypeC == 1, 0, ifelse(
      errorTypeC == 2, 1, "NA")))
  
  ##########
  res1$gridsquare <- as.character(res1$gridsquare) # make sure to turn off if not using
  tab_comT <- dplyr::left_join(tab_comT, res1) %>% na.omit() # make sure to turn off if not using
  #######
  
###############################################################################################
matrix <- as.data.frame(matrix(ncol = 5, nrow = 0))
colnames(matrix) <- c("coef", "pvalue", "variable", "species", "Rsquare")

warn <- capture.output({for (i in unique(tab_comT$species)){
  tab_comS <- tab_comT %>% dplyr::filter(species == i)
  
  tab_comS$errorTypeC <- as.numeric(tab_comS$errorTypeC)
  
  errors <- tryCatch({glm(errorTypeC~climateVariability + delta_past + delta_pop + pest + PA_index, 
              family = "binomial", data = tab_comS)}, 
              warning = function(w) {print(paste0(i))})
  
    test <- glm(errorTypeC~climateVariability + delta_past + delta_pop + pest + PA_index, 
                          family = "binomial", data = tab_comS)
  
  # extract coefficieints
  coef <- coef(summary(test))[,1]
  
  #extract pvalue
  pvalue <- coef(summary(test))[,4]
  
  #extract R square
  nagel <- nagelkerke(test)$Pseudo.R.squared.for.model.vs.null
  nagel <- nagel[3,]
  
  df <- data.frame(coef, pvalue)
  df$variable <- rownames(df)
  rownames(df) <- NULL
  
  df$species <- paste0(i)
  
  df$Rsquare <- nagel
  
  matrix <- rbind(matrix, df)
  }
  })


# remove models that do not converge
warn <- as.data.frame(warn)
colnames(warn) <- c("species")
warn$species <- paste0(gsub("[1] ", "", warn$species, fixed=TRUE))
    # remove quotes
      library(stringr)
      library(plyr)

      del <- colwise(function(x) str_replace_all(x, '\"', ""))
      warn <- del(warn)
      warn$warning <- "yes"
      warn$species <- trimws(warn$species)

      matrix <- dplyr::left_join(matrix, warn)
      matrix$warning <- with(matrix, ifelse(
        is.na(warning), "no", "yes"))
      matrix <- matrix %>% dplyr::filter(warning == "no" & Rsquare != 1.0)

tot <- nrow(as.data.frame(unique(matrix$species)))

climpvalue <- matrix %>% dplyr::filter(variable == "climateVariability" & pvalue <= 0.05)
#plot(climpvalue$coef)
mean(climpvalue$coef)
nrow(climpvalue) #sig
tot - nrow(climpvalue) #not sig

pastpvalue <- matrix %>% dplyr::filter(variable == "delta_past" & pvalue <= 0.05)
#plot(pastpvalue$coef)
mean(pastpvalue$coef)
nrow(pastpvalue) #sig
tot - nrow(pastpvalue) #not sig

pestpvalue <- matrix %>% dplyr::filter(variable == "pest" & pvalue <= 0.05)
#plot(pestpvalue$coef)
mean(pestpvalue$coef)
nrow(pestpvalue) #sig
tot - nrow(pestpvalue) #not sig

poppvalue <- matrix %>% dplyr::filter(variable == "delta_pop" & pvalue <= 0.05)
#plot(poppvalue$coef)
mean(poppvalue$coef)
nrow(poppvalue) #sig
tot - nrow(poppvalue) #not sig

PApvalue <- matrix %>% dplyr::filter(variable == "PA_index" & pvalue <= 0.05)
#plot(poppvalue$coef)
mean(PApvalue$coef)
nrow(PApvalue) #sig
tot - nrow(PApvalue) #not sig

allPavlue <- rbind(climpvalue, pastpvalue, pestpvalue, poppvalue, PApvalue)

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)

}



#..count.. <- areas under the density curves to be scaled relative to the 
            # number of species statistically sig in each group
p2 <- ggplot(allPavlue, aes(x=coef, y=..count.., group=variable, fill=variable)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()
p2
setwd("wd/distribution_models/all_figures/")
#ggsave(paste0("CoefBinomial117Models.pdf"))
unique(allPavlue$species)
################

# same thing but only for species with R square greater than X
rsquare_min <- matrix %>% dplyr::select(species, Rsquare) %>% unique() %>%
  dplyr::filter(Rsquare >= 0.10)

colnames(rsquare_min) <- c("species", "RsquareMin")

allPvalue_Rcriteria <- dplyr::left_join(allPavlue, rsquare_min) %>% na.omit()

p2 <- ggplot(allPvalue_Rcriteria, aes(x=coef, y=..count.., group=variable, fill=variable)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()
p2
unique(allPvalue_Rcriteria$species)

##############
rsquare_pvalue <- matrix %>% dplyr::select(species, Rsquare) %>% unique()
plot(rsquare_pvalue$Rsquare)
mean(rsquare_pvalue$Rsquare)
row.names(rsquare_pvalue) <- NULL
rsquare_pvalue


library(ggplot2)
# Histogram with density plot
ggplot(rsquare_pvalue, aes(x=Rsquare)) + 
  geom_histogram(aes(y=..density..), colour="white", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")  +
  geom_vline(aes(xintercept=mean(Rsquare)),
             color="blue", linetype="dashed", size=1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))

mean(rsquare_pvalue$Rsquare)

setwd("wd/distribution_models/all_figures/")
ggsave(paste0("Rsquares.pdf"))


###########################################
###########################################
###########################################

head(tabC1)
hist(tabCprob$prob1)
hist(tabC1$freq1)
qqnorm(tabCprob$prob1)
shap <- shapiro.test(tabC1$freq1)
shap
    # data is not normal

################### testing binary data with species as random effect

#library(lme4)
#library(sjstats)
#tab_com$errorTypeC <- as.factor(tab_com$errorTypeC)
##########
#res1$gridsquare <- as.character(res1$gridsquare) # make sure to turn off if not using
#tab_com <- dplyr::left_join(tab_com, res1) %>% na.omit() # make sure to turn off if not using
#######
#modC <- glmer(errorTypeC ~ climateVariability + delta_past + delta_pop + pest + 
#                mean_exp + mean_phen + PA_index +
#                (1|species), family = "binomial", data = tab_com)
#summary(modC)
#performance::r2(modC)
# number of species per quadrat that maximises conditional R square is:

#library(ggplot2)
library(pscl)
library(boot)

################################## test Gaussian GLM of effect into probability of error
##################### probability of error compared to not errors
library(betareg)
hist(tabCprob$prob1)
mean(tabCprob$prob1)
var(tabCprob$prob1)
###########
#tabCprob <- dplyr::left_join(tabCprob, res1) %>% na.omit() # make sure to turn off if not using
##########

summary(model <- betareg(prob1 ~ countp2, 
                         data = tabCprob))
summary(model <- betareg(prob1 ~ climateVariability*pest + past2 + pop2, data = tabCprob))

summary(model)
plot(fitted(model), residuals(model))


###################################################
}
mean(tabC1$freq1) # 5.795
var(tabC1$freq1) # 27.471

finalQuadrats <- as.data.frame(tabC1$gridsquare)
finalQuadrats

##########
#res1$gridsquare <- as.character(res1$gridsquare) # make sure to turn off if not using
#tabC1 <- dplyr::left_join(tabC1, res1) %>% na.omit() # make sure to turn off if not using
#######

modC <- glm.nb(freq1 ~ countp2, data = tabC1) # effect of sampling (number of species sampled in P2)
summary(modC) # count not sig
nagelkerke(modC)[2] # 0.3%
plot(resid(modC)) # check for independance of errors

# check all correlations among anthropogenic pressures
cor(tabC1 %>% dplyr::select(countp2, climateVariability, pest, delta_past, delta_pop, delta_crop))

tabC1$variabTest <- tabC1$climateVariability2 - tabC1$climateVariability1

# base model
modC <- glm.nb(freq1 ~ variabTest + pest + delta_past + delta_pop + countp2, data = tabC1)
summary(modC) # 4232
nagelkerke(modC)[2] # R = 19%

modC <- glm.nb(freq1 ~ 1, data = tabC1)
summary(modC) # AIC 4373
nagelkerke(modC)[2] # R = 0

modC <- glm.nb(freq1 ~ pest*variabTest + delta_past, data = tabC1)
summary(modC) # AIC 3350 vs. 4221
nagelkerke(modC)[2] # R = 21%
#removing countp2, delta_pop, 

dropterm(modC)



library(broom.mixed)
library(interactions)
library(jtools)
plot_coefs(modC)
setwd("wd/distribution_models/all_figures/")
ggsave(paste0("bestModelEstimates.pdf"))

#The Poisson distribution can be considered to be a special case of the negative binomial distribution.
    # use this because the variance here is much larger than the mean, so a poisson model
          # causes over-dispersion

range(tabC1$pest)

library(ggtext)

interact_plot(modC, pred = variabTest, modx = pest, 
              partial.residuals = FALSE,
              plot.points = FALSE,
              modx.values = c(-1, 0, 1, 2, 3, 4),
              x.label = "Temperature variability",
              y.label = "Frequency of error",
              legend.main = "Pesticide application") +
              xlim(0, 2) +
              ylim(5, 30) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x = ggtext::element_markdown(family = "Times")) +
  theme(axis.title.y = ggtext::element_markdown(family = "Times"))+
  theme(axis.text.x = ggtext::element_markdown(family = "Times"))+
  theme(axis.text.y = ggtext::element_markdown(family = "Times"))+
  theme(legend.title = ggtext::element_markdown(family = "Times"))+
  theme(legend.text = ggtext::element_markdown(family = "Times"))

#setwd("wd/distribution_models/all_figures/")
#ggsave(paste0("interactionPlot.pdf"))
             
library(broom.mixed)
plot_coefs(modC)

#####################
# Look at places that are the most natural
percentileLC = tabC1$NaturalAreasPercent
quantVal = 0.30 # about 50% of most developed quadrats included here lead to this interaction
LC_criteria <- as.data.frame(quantile(percentileLC, c(quantVal)))[,1] # richness threshold

best_protected <- tabC1 %>% dplyr::filter(NaturalAreasPercent >= LC_criteria)
plot(best_protected$NaturalAreasPercent)
min(best_protected$NaturalAreasPercent)

modC <- glm.nb(freq1 ~ climateVariability*pest + past2 + pop2, data = best_protected)
summary(modC)
nagelkerke(modC)
plot(resid(modC))
unique(best_protected$gridsquare)
plot_coefs(modC)



#####################


# plotting predicted effects of the different predictor variables

effect_plot(modC, pred = climateVariability, plot.points = TRUE)
effect_plot(modC, pred = past2, plot.points = TRUE)
effect_plot(modC, pred = pop2, plot.points = TRUE)
effect_plot(modC, pred = pest, plot.points = TRUE)


#########################################################################################################
# check for correlation between predictors:

# pest/past/clim and crop
# clim/past and count

cor.test(tabC1$NaturalAreasPercent, tabC1$climateVariability, method = "pearson")[4]
cor.test(tabC1$NaturalAreasPercent, tabC1$crop2, method = "pearson")[4] #****
cor.test(tabC1$NaturalAreasPercent, tabC1$past2, method = "pearson")[4] #***
cor.test(tabC1$NaturalAreasPercent, tabC1$pop2, method = "pearson")[4]
cor.test(tabC1$NaturalAreasPercent, tabC1$pest, method = "pearson")[4] #***

cor.test(tabC1$countp2, tabC1$climateVariability, method = "pearson")[4]
cor.test(tabC1$countp2, tabC1$crop2, method = "pearson")[4]
cor.test(tabC1$countp2, tabC1$past2, method = "pearson")[4]
cor.test(tabC1$countp2, tabC1$pop2, method = "pearson")[4]
cor.test(tabC1$countp2, tabC1$pest, method = "pearson")[4]
cor.test(tabC1$climateVariability, tabC1$pest, method = "pearson")[4]
cor.test(tabC1$pest, tabC1$pop2, method = "pearson")[4]
cor.test(tabC1$pest, tabC1$past2, method = "pearson")[4]

cor.test(tabC1$pest, tabC1$crop2, method = "pearson")[4] #***

cor.test(tabC1$climateVariability, tabC1$delta_crop, method = "pearson")[4]
cor.test(tabC1$climateVariability, tabC1$delta_pop, method = "pearson")[4]
cor.test(tabC1$climateVariability, tabC1$delta_past, method = "pearson")[4]
cor.test(tabC1$crop2, tabC1$pop2, method = "pearson")[4]
cor.test(tabC1$crop2, tabC1$past2, method = "pearson")[4]
cor.test(tabC1$past2, tabC1$pop2, method = "pearson")[4]

# visualize results

library(ggstance)
library(jtools)
library(broom)

# plot regression coefficients
plot_summs(m1, scale = TRUE, exp = TRUE)

################################################# 
#################################################
#################################################
################################################# BINARY MAP - SIGNAL TO ERROR RATIO

library(raster)

# read all binary raster files of model predictions
# stack and sum, save in folder sum_all_predictions

# north america MAXENT
dir.create(paste0("wd/distribution_models/maxent/NAM/binary2"))

setwd(paste0("wd/distribution_models/maxent/NAM/threshold"))
thresholds <- read.csv("allThresholds.csv")
thresholds <- thresholds %>% dplyr::select(spec_sens, species)
  
setwd(paste0("wd/distribution_models/maxent/NAM/rasterResults"))
rasList <- list.files(pattern = ".tif", full.names = F)

for (i in rasList) {
setwd(paste0("wd/distribution_models/maxent/NAM/rasterResults"))  
ras <- raster(i)
species_name <- paste0(gsub(".csv.tif", "", i, fixed=TRUE))
reclassVal <- thresholds %>% dplyr::filter(species == species_name)
reclassVal <- reclassVal$spec_sens
reclassmatrix <- matrix(c(-Inf, reclassVal, 0, reclassVal, Inf, 1), nrow = 2, ncol = 3, byrow = TRUE)
setwd(paste0("wd/distribution_models/maxent/NAM/binary2"))
reclassify(ras, reclassmatrix, filename = paste0(i, "binary"), overwrite = TRUE)
}

##########################################################################################################

# north america RANDOM FOREST
dir.create(paste0("wd/distribution_models/rf/NAM/binary2"))

setwd(paste0("wd/distribution_models/rf/NAM/threshold"))
thresholds <- read.csv("allThresholds.csv")
thresholds <- thresholds %>% dplyr::select(spec_sens, species)

setwd(paste0("wd/distribution_models/rf/NAM/rasterResults"))
rasList <- list.files(pattern = ".tif", full.names = F)

for (i in rasList) {
  setwd(paste0("wd/distribution_models/rf/NAM/rasterResults"))  
  ras <- raster(i)
  species_name <- paste0(gsub(".csv.tif", "", i, fixed=TRUE))
  reclassVal <- thresholds %>% dplyr::filter(species == species_name)
  reclassVal <- reclassVal$spec_sens
  reclassmatrix <- matrix(c(-Inf, reclassVal, 0, reclassVal, Inf, 1), nrow = 2, ncol = 3, byrow = TRUE)
  setwd(paste0("wd/distribution_models/rf/NAM/binary2"))
  reclassify(ras, reclassmatrix, filename = paste0(i, "binary"), overwrite = TRUE)
}

#########################################################################################################
# north america GAMs

dir.create(paste0("wd/distribution_models/gam/NAM/binary2"))

setwd(paste0("wd/distribution_models/gam/NAM/threshold"))
thresholds <- read.csv("allThresholds.csv")
thresholds <- thresholds %>% dplyr::select(spec_sens, species)

setwd(paste0("wd/distribution_models/gam/NAM/rasterResults"))
rasList <- list.files(pattern = ".tif", full.names = F)

for (i in rasList) {
  setwd(paste0("wd/distribution_models/gam/NAM/rasterResults"))  
  ras <- raster(i)
  species_name <- paste0(gsub(".csv.tif", "", i, fixed=TRUE))
  reclassVal <- thresholds %>% dplyr::filter(species == species_name)
  reclassVal <- reclassVal$spec_sens
  reclassmatrix <- matrix(c(-Inf, reclassVal, 0, reclassVal, Inf, 1), nrow = 2, ncol = 3, byrow = TRUE)
  setwd(paste0("wd/distribution_models/gam/NAM/binary2"))
  reclassify(ras, reclassmatrix, filename = paste0(i, "binary"), overwrite = TRUE)
}

#########################################################################################################

# sum binary maps for all species (per model type)
dir.create(paste0("wd/distribution_models/0mapCorrelation"))

# MAXENT
setwd(paste0("wd/distribution_models/maxent/NAM/binary2"))
binaryfiles <- list.files(pattern = "binary.grd", full.names = F)
s.p1 <- stack(binaryfiles)
a <- sum(s.p1, na.rm=TRUE)
setwd(paste0("wd/distribution_models/0mapCorrelation"))
writeRaster(a, "MaxentAll", overwrite = TRUE)

# RANDOM FOREST
setwd(paste0("wd/distribution_models/rf/NAM/binary2"))
binaryfiles <- list.files(pattern = "binary.grd", full.names = F)
s.p1 <- stack(binaryfiles)
a <- sum(s.p1, na.rm=TRUE)
setwd(paste0("wd/distribution_models/0mapCorrelation"))
writeRaster(a, "RandomForestAll", overwrite = TRUE)

# GAM
setwd(paste0("wd/distribution_models/gam/NAM/binary2"))
binaryfiles <- list.files(pattern = "binary.grd", full.names = F)
s.p1 <- stack(binaryfiles)
a <- sum(s.p1, na.rm=TRUE)
setwd(paste0("wd/distribution_models/0mapCorrelation"))
writeRaster(a, "GamAll", overwrite = TRUE)


##########################################################
##########################################################
##########################################################
##########################################################
########################################################## sum binary maps for only well modeled species
# well modeled species according to the boyce index:
head(bestSpecies)
BestSP <- bestSpecies
BestSP$matchName <- paste0(BestSP$species, ".csv.tifbinary.grd")

# MAXENT
setwd(paste0("wd/distribution_models/maxent/NAM/binary2"))
binaryfiles <- list.files(pattern = "binary.grd", full.names = F)
# then keep only the basename (file names) matching dataframe column
binaryfilesGood <- binaryfiles[ basename(binaryfiles) %in% BestSP$matchName ]
s.p1 <- stack(binaryfilesGood)
a <- sum(s.p1, na.rm=TRUE)
setwd(paste0("wd/distribution_models/0mapCorrelation"))
writeRaster(a, "MaxentGood", overwrite = TRUE)

# RANDOM FOREST
setwd(paste0("wd/distribution_models/rf/NAM/binary2"))
binaryfiles <- list.files(pattern = "binary.grd", full.names = F)
# then keep only the basename (file names) matching dataframe column
binaryfilesGood <- binaryfiles[ basename(binaryfiles) %in% BestSP$matchName ]
s.p1 <- stack(binaryfilesGood)
a <- sum(s.p1, na.rm=TRUE)
setwd(paste0("wd/distribution_models/0mapCorrelation"))
writeRaster(a, "RandomForestGood", overwrite = TRUE)

# GAM
setwd(paste0("wd/distribution_models/gam/NAM/binary2"))
binaryfiles <- list.files(pattern = "binary.grd", full.names = F)
# then keep only the basename (file names) matching dataframe column
binaryfilesGood <- binaryfiles[ basename(binaryfiles) %in% BestSP$matchName ]
s.p1 <- stack(binaryfilesGood)
a <- sum(s.p1, na.rm=TRUE)
setwd(paste0("wd/distribution_models/0mapCorrelation"))
writeRaster(a, "GamGood", overwrite = TRUE)



########################################################## sum binary maps for only badly modeled species
# well modeled species according to the boyce index:
head(worstSpecies)
worstSP <- worstSpecies
worstSP$matchName <- paste0(worstSP$species, ".csv.tifbinary.grd")

# MAXENT
setwd(paste0("wd/distribution_models/maxent/NAM/binary2"))
binaryfiles <- list.files(pattern = "binary.grd", full.names = F)
# then keep only the basename (file names) matching dataframe column
binaryfilesBad <- binaryfiles[ basename(binaryfiles) %in% worstSP$matchName ]
s.p1 <- stack(binaryfilesBad)
a <- sum(s.p1, na.rm=TRUE)
setwd(paste0("wd/distribution_models/0mapCorrelation"))
writeRaster(a, "MaxentBad", overwrite = TRUE)

# RANDOM FOREST
setwd(paste0("wd/distribution_models/rf/NAM/binary2"))
binaryfiles <- list.files(pattern = "binary.grd", full.names = F)
# then keep only the basename (file names) matching dataframe column
binaryfilesBad <- binaryfiles[ basename(binaryfiles) %in% worstSP$matchName ]
s.p1 <- stack(binaryfilesBad)
a <- sum(s.p1, na.rm=TRUE)
setwd(paste0("wd/distribution_models/0mapCorrelation"))
writeRaster(a, "RandomForestBad", overwrite = TRUE)

# GAM
setwd(paste0("wd/distribution_models/gam/NAM/binary2"))
binaryfiles <- list.files(pattern = "binary.grd", full.names = F)
# then keep only the basename (file names) matching dataframe column
binaryfilesBad <- binaryfiles[ basename(binaryfiles) %in% worstSP$matchName ]
s.p1 <- stack(binaryfilesBad)
a <- sum(s.p1, na.rm=TRUE)
setwd(paste0("wd/distribution_models/0mapCorrelation"))
writeRaster(a, "GamBad", overwrite = TRUE)


#################################################################### ANALYSE THE MAPS

library(raster)

# run a spatial correlation function
# first, read all rasters and save attribte tables
setwd(paste0("wd/distribution_models/0mapCorrelation"))
allMax <- raster("allSumMaxent.grd")
allRF <- raster("allSumRF.grd")
allGAM <- raster("allSumGAM.grd")
GoodMax <- raster("GoodSumMaxent.grd")
GoodRF <- raster("GoodSumRF.grd")
GoodGAM <- raster("GoodSumGAM.grd")
BadMax <- raster("BadSumMaxent.grd")
BadRF <- raster("BadSumRF.grd")
BadGAM <- raster("BadSumGAM.grd")

max <- stack(allMax, GoodMax)
max <- values(max)
max <- as.data.frame(max)
plot(max$layer.1~max$layer.2)
cor.test(max$layer.1, max$layer.2, method = "pearson")

rf <- stack(allRF, GoodRF)
rf <- values(rf)
rf <- as.data.frame(rf)
plot(rf$layer.1~rf$layer.2)
cor.test(rf$layer.1, rf$layer.2, method = "pearson")

gam <- stack(allGAM, GoodGAM)
gam <- values(gam)
gam <- as.data.frame(max)
plot(gam$layer.1~gam$layer.2)
cor.test(gam$layer.1, gam$layer.2, method = "pearson")

######### testing correlations with badly modeled species

max <- stack(allMax, BadMax)
max <- values(max)
max <- as.data.frame(max)
plot(max$layer.1~max$layer.2)
cor.test(max$layer.1, max$layer.2, method = "pearson")

rf <- stack(allRF, BadRF)
rf <- values(rf)
rf <- as.data.frame(rf)
plot(rf$layer.1~rf$layer.2)
cor.test(rf$layer.1, rf$layer.2, method = "pearson")

gam <- stack(allGAM, BadGAM)
gam <- values(gam)
gam <- as.data.frame(max)
plot(gam$layer.1~gam$layer.2)
cor.test(gam$layer.1, gam$layer.2, method = "pearson")


######### testing correlations with badly modeled species

max <- stack(GoodMax, BadMax)
max <- values(max)
max <- as.data.frame(max)
plot(max$layer.1~max$layer.2)
cor.test(max$layer.1, max$layer.2, method = "pearson")

rf <- stack(GoodRF, BadRF)
rf <- values(rf)
rf <- as.data.frame(rf)
plot(rf$layer.1~rf$layer.2)
cor.test(rf$layer.1, rf$layer.2, method = "pearson")

gam <- stack(GoodGAM, BadGAM)
gam <- values(gam)
gam <- as.data.frame(max)
plot(gam$layer.1~gam$layer.2)
cor.test(gam$layer.1, gam$layer.2, method = "pearson")


#######################################################################################
#######################################################################################
#######################################################################################

################## FIGURE TP COMPARE RICHNESS IN HOTSPOTS WHEN ASSESSING ONLY WELL MODELED SPECIES
########################### VS. ALL SPECIES

# percent of species included in the identification of hotspots
percent <- 1:100
for (percent in percent){
  setwd(paste0("wd/distribution_models/0mapCorrelation"))
  rasList <- list.files(pattern = "Good.grd", full.names = F)
  for (i in rasList) {
    setwd(paste0("wd/distribution_models/0mapCorrelation"))  
    ras <- raster(i)
    reclassVal <- max(values(ras))*(percent/100)
    reclassmatrix <- matrix(c(-Inf, reclassVal, 0, reclassVal, Inf, 1), nrow = 2, ncol = 3, byrow = TRUE)
    setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots"))  
    reclassify(ras, reclassmatrix, filename = paste0(i, percent, "hotspot"), overwrite = TRUE)
  }
  
  setwd(paste0("wd/distribution_models/0mapCorrelation"))
  rasList <- list.files(pattern = "Bad.grd", full.names = F)
  for (i in rasList) {
    setwd(paste0("wd/distribution_models/0mapCorrelation"))  
    ras <- raster(i)
    reclassVal <- max(values(ras))*(percent/100)
    reclassmatrix <- matrix(c(-Inf, reclassVal, 0, reclassVal, Inf, 2), nrow = 2, ncol = 3, byrow = TRUE)
    setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots"))  
    reclassify(ras, reclassmatrix, filename = paste0(i, percent, "hotspot"), overwrite = TRUE)
  }
  
  setwd(paste0("wd/distribution_models/0mapCorrelation"))
  rasList <- list.files(pattern = "All.grd", full.names = F)
  
  for (i in rasList) {
    setwd(paste0("wd/distribution_models/0mapCorrelation"))  
    ras <- raster(i)
    reclassVal <- max(values(ras))*(percent/100)
    reclassmatrix <- matrix(c(-Inf, reclassVal, 0, reclassVal, Inf, 4), nrow = 2, ncol = 3, byrow = TRUE)
    setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots"))  
    reclassify(ras, reclassmatrix, filename = paste0(i, percent, "hotspot"), overwrite = TRUE)
  }
  
}


########################################################

# save in respective folders to facilitate next steps
setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots"))
rasList <- list.files(pattern = "Maxent.*.grd$", full.names = F)

for (i in rasList) {
  setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots"))  
  ras <- raster(i)
  setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots/Maxent"))  
  writeRaster(ras, paste0(i), overwrite = TRUE)
}

# save in respective folders to facilitate next steps
setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots"))
rasList <- list.files(pattern = "Gam.*.grd$", full.names = F)

for (i in rasList) {
  setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots"))  
  ras <- raster(i)
  setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots/Gam"))  
  writeRaster(ras, paste0(i), overwrite = TRUE)
}

# save in respective folders to facilitate next steps
setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots"))
rasList <- list.files(pattern = "RandomForest.*.grd$", full.names = F)

for (i in rasList) {
  setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots"))  
  ras <- raster(i)
  setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots/Randomforest"))  
  writeRaster(ras, paste0(i), overwrite = TRUE)
}

library(dplyr)
library(readr)
library(raster)
library(ggplot2)


# start here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Sum rasters (good species, bad, all)
setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots/Gam"))
hotspots <- list.files(pattern = "hotspot.grd", full.names = F)

gamDF <- as.data.frame(matrix(ncol = 4, nrow = 0))
colnames(gamDF) <- c("quadrat", "cat", "val", "percent")
gamDF$quadrat <- as.character(gamDF$quadrat)

for (i in hotspots) {
setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots/Gam")) 
r <- raster(i)
r <- values(r)
r <- as.data.frame(r)
colnames(r) <- c("val")

r$cat <- gsub("hotspot.grd", "", i)
  r$cat <- gsub(".grd", "", r$cat)
  r$cat <- gsub("Gam", "", r$cat)
r <- r %>% mutate(percent = parse_number(cat))
r$cat <- substr(r$cat, 1, 3)
r$quadrat <- rownames(r)

gamDF <- rbind(gamDF, r)

}
head(gamDF)

gam_table <- gamDF %>% dplyr::filter(cat != "Bad")

good <- gam_table %>% dplyr::filter(cat == "Goo")
colnames(good) <- c("valGood", "catGood", "percent", "quadrat")

all <- gam_table %>% dplyr::filter(cat == "All")
colnames(all) <- c("valAll", "catAll", "percent", "quadrat")

       # percent and quadrat are the units of overlap we are interested in

gam_table <- dplyr::left_join(all, good)

gam_table$Legend <- with(gam_table, ifelse(
  valAll == 4 & valGood == 1, "Overlap", ifelse(
    valAll == 0 & valGood == 1, "Best modeled species", ifelse(
      valAll == 4 & valGood == 0, "All species", NA))))

table <- gam_table %>% dplyr::select(Legend, quadrat, percent) %>% na.omit()
  
table <- table %>% group_by(Legend, percent) %>% 
  dplyr::summarize(freq = n()) %>% ungroup()

table$Legend <- factor(table$Legend, levels = c('All species', 'Overlap', 'Best modeled species'))

ggplot(data=table, aes(x=percent, y=freq, fill=Legend)) +
  geom_bar(stat="identity", width=0.5) +
  scale_fill_brewer(palette="Reds") + theme_minimal()

setwd("wd/distribution_models/all_figures/")
ggsave(paste0("gam_hotspots.pdf"))

############################################################################################

# Sum rasters (good species, bad, all)
setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots/Maxent"))
hotspots <- list.files(pattern = "hotspot.grd", full.names = F)
maxDF <- as.data.frame(matrix(ncol = 4, nrow = 0))
colnames(maxDF) <- c("quadrat", "cat", "val", "percent")
maxDF$quadrat <- as.character(maxDF$quadrat)

for (i in hotspots) {
  setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots/Maxent")) 
  r <- raster(i)
  r <- values(r)
  r <- as.data.frame(r)
  colnames(r) <- c("val")
  
  r$cat <- gsub("hotspot.grd", "", i)
  r$cat <- gsub(".grd", "", r$cat)
  r$cat <- gsub("Maxent", "", r$cat)
  r <- r %>% mutate(percent = parse_number(cat))
  r$cat <- substr(r$cat, 1, 3)
  r$quadrat <- rownames(r)
  
  maxDF <- rbind(maxDF, r)
  
}
head(maxDF)

max_table <- maxDF %>% dplyr::filter(cat != "Bad")

good <- max_table %>% dplyr::filter(cat == "Goo")
colnames(good) <- c("valGood", "catGood", "percent", "quadrat")

all <- max_table %>% dplyr::filter(cat == "All")
colnames(all) <- c("valAll", "catAll", "percent", "quadrat")

# percent and quadrat are the units of overlap we are interested in

max_table <- dplyr::left_join(all, good)

max_table$Legend <- with(max_table, ifelse(
  valAll == 4 & valGood == 1, "Overlap", ifelse(
    valAll == 0 & valGood == 1, "Best modeled species", ifelse(
      valAll == 4 & valGood == 0, "All species", NA))))

table <- max_table %>% dplyr::select(Legend, quadrat, percent) %>% na.omit()

table <- table %>% group_by(Legend, percent) %>% 
  dplyr::summarize(freq = n()) %>% ungroup()

table$Legend <- factor(table$Legend, levels = c('All species', 'Overlap', 'Best modeled species'))

ggplot(data=table, aes(x=percent, y=freq, fill=Legend)) +
  geom_bar(stat="identity", width=0.5) +
  scale_fill_brewer(palette="Reds") + theme_minimal()

setwd("wd/distribution_models/all_figures/")
ggsave(paste0("max_hotspots.pdf"))

############################################################################################


# Sum rasters (good species, bad, all)
setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots/Randomforest"))
hotspots <- list.files(pattern = "hotspot.grd", full.names = F)
rfDF <- as.data.frame(matrix(ncol = 4, nrow = 0))
colnames(rfDF) <- c("quadrat", "cat", "val", "percent")
rfDF$quadrat <- as.character(rfDF$quadrat)

for (i in hotspots) {
  setwd(paste0("wd/distribution_models/0mapCorrelation/hotspots/Randomforest")) 
  r <- raster(i)
  r <- values(r)
  r <- as.data.frame(r)
  colnames(r) <- c("val")
  
  r$cat <- gsub("hotspot.grd", "", i)
  r$cat <- gsub(".grd", "", r$cat)
  r$cat <- gsub("RandomForest", "", r$cat)
  r <- r %>% mutate(percent = parse_number(cat))
  r$cat <- substr(r$cat, 1, 3)
  r$quadrat <- rownames(r)
  
  rfDF <- rbind(rfDF, r)
  
}
head(rfDF)

rf_table <- rfDF %>% dplyr::filter(cat != "Bad")

good <- rf_table %>% dplyr::filter(cat == "Goo")
colnames(good) <- c("valGood", "catGood", "percent", "quadrat")

all <- rf_table %>% dplyr::filter(cat == "All")
colnames(all) <- c("valAll", "catAll", "percent", "quadrat")

# percent and quadrat are the units of overlap we are interested in

rf_table <- dplyr::left_join(all, good)

rf_table$Legend <- with(rf_table, ifelse(
  valAll == 4 & valGood == 1, "Overlap", ifelse(
    valAll == 0 & valGood == 1, "Best modeled species", ifelse(
      valAll == 4 & valGood == 0, "All species", NA))))

table <- rf_table %>% dplyr::select(Legend, quadrat, percent) %>% na.omit()

table <- table %>% group_by(Legend, percent) %>% 
  dplyr::summarize(freq = n()) %>% ungroup()

table$Legend <- factor(table$Legend, levels = c('All species', 'Overlap', 'Best modeled species'))

ggplot(data=table, aes(x=percent, y=freq, fill=Legend)) +
  geom_bar(stat="identity", width=0.5) +
  scale_fill_brewer(palette="Reds") + theme_minimal()

setwd("wd/distribution_models/all_figures/")
ggsave(paste0("rf_hotspots.pdf"))

############################################################################################







################################################# 
#################################################
#################################################
#################################################

# save full table for model evaluation by species in the modelEval folder
dir.create(paste0("wd/distribution_models/modelsEval/"))

#maxent results
setwd(paste0("wd/distribution_models/modelsEval/"))
#make a copy of the results table
tab2 <- tab
# Split by variable
spt2 <- split(tab2, tab2$species)
# Save
invisible(lapply(names(spt2), function(x){
  write_csv(spt2[[x]], path = paste(x, ".csv", sep = ""))
}))

all_files <- list.files(pattern = ".csv", full.names = F)

normalTest <- as.data.frame(matrix(ncol = 2, nrow = 0))
colnames(normalTest) <- c("species", "pvalue")

for (i in all_files) {
  d <- read.csv(i)
  one <- d %>% dplyr::filter(errorTypeC == 1)
  shap <- shapiro.test(one$climateVariability)
  sh <- as.data.frame(shap[2])
  colnames(sh) <- c("pvalue")
  sh$species <- unique(d$species)
  normalTest <- rbind(normalTest, sh)
  
  zero <- d %>% dplyr::filter(errorTypeC == 0)
  shap <- shapiro.test(zero$climateVariability)
  sh <- as.data.frame(shap[2])
  colnames(sh) <- c("pvalue")
  sh$species <- unique(d$species)
  normalTest <- rbind(normalTest, sh)

}

normalTest # samples are not normally distributed, so we will use Wilcoxon Test instead of T.tests

library(stats)

resTable <- as.data.frame(matrix(ncol = 2, nrow = 0))
colnames(resTable) <- c("species", "pvalue")

for (i in all_files) {
  df <- read.csv(i)
  
  #comission errors
  dfCom <- df %>% dplyr::select(species, gridsquare, errorTypeC, climateVariability) %>% unique()
  #dfComX <- dfCom %>% dplyr::filter(errorTypeC == 1) # errors agree AND disagree
  #dfComY <- dfCom %>% dplyr::filter(errorTypeC == 0) # successful identification of presence/absence
  WilTestC <- wilcox.test(dfCom$climateVariability ~ dfCom$errorTypeC)

  table1 <- as.data.frame(c(unique(df$species), WilTestC[3]))

  colnames(table1) <- c("species", "pvalue")

  resTable <- rbind(resTable, table1)

}

head(resTable)

noDiff <- resTable %>% dplyr::filter(pvalue >= 0.05)
nrow(noDiff)

tTestFig <- resTable %>% dplyr::filter(pvalue <= 0.05)
nrow(tTestFig)




###################################################################################################################

# Wilcoxon test with ALL OBSERVATIONS FOR OVERALL PATTERN

# think about: to make your two groups, think about using quadrats where lots of species show errors vs. everywhere else????

#comission errors
dfTab <- tab %>% dplyr::select(species, gridsquare, errorTypeC, climateVariability) %>% unique()
WilTest <- wilcox.test(dfTab$climateVariability ~ dfTab$errorTypeC, alternative = "less")
WilTest

ggplot(dfTab, aes(x=as.factor(errorTypeC), y=climateVariability)) + 
  geom_boxplot()







#########################################################################################################
#########################################################################################################

# Determine if high probabilities of presence are associated with actual presences, and vice versa
# statistical test (probably a GLM or LM), depending on whether data meets assumptions

# ask if probablities of presence actually predict true presences
# you may have to do this individually for EVERY psecies, and put in a table all the results
# to see which species is well predicted by the model
# perhaps save the table BY SPECIES to fun a for loop
mod <- glm(presence ~ pred, data = fullTable)
summary(mod)








########################################################### MODEL EVALUATION
###########################################################
###########################################################


# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
ped1 <- predict(mod, studyArea)  # studyArea is the clipped rasters 
plot(ped1)  # plot the continuous prediction

# example 2, project to the world
ped2 <- predict(mod,clim)
plot(ped2)

# example 3, project with training occurrences [dataframes]
ped3 <- predict(mod, p)

dismo::predict(mod, p)

mod_eval_train <- dismo::evaluate(p = p, a = a, model = mod)
mod_eval_test <- dismo::evaluate(p = p_test, a = a, model = mod)


mod_eval_test <- dismo::evaluate(p = p_test, a = a, model = mod)
print(mod_eval_test)  # training AUC may be higher than testing AUC

# calculate thresholds of models
thd1 <- threshold(mod_eval_train, "no_omission")  # 0% omission rate 
thd2 <- threshold(mod_eval_train, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
plot(proj >= thd1)



