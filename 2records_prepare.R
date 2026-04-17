
### LOAD ALL DATA

library(dplyr)
setwd("wd/distribution_models/")

# Download https://doi.org/10.15468/dl.uesyds

# GBIF
gbif_readr <- readr::read_delim("GBIF/GBIF.csv", delim = "\t", escape_double = FALSE, col_names = TRUE, na = c("", "NA"))
gbif <- dplyr::select(gbif_readr, scientificName, decimalLongitude, decimalLatitude, eventDate)

gbif_year <- gbif %>% rowwise() %>% mutate(year = substr(eventDate, 1, 4))
gbif_year <- gbif_year %>% rowwise() %>% mutate(date = substr(eventDate, 1, 10))
gbif_year <- dplyr::select(gbif_year, scientificName, decimalLongitude, decimalLatitude, date, year)
colnames(gbif_year)<- c("species","long","lat", "date", "year")
gbif_year$inst <- "GBIF"
gbif <- gbif_year

####
####
####
####
####
####
####
####
####
####
####
####
#### CANADENSYS DATA

# info on raw data found in Canadensys folder (inside Traits folder)

setwd("wd/distribution_models/")
Canadensys <- read.csv("canadensys/records-2021-11-12.csv")
Can <- dplyr::select(Canadensys, scientificName, decimalLongitude, decimalLatitude, eventDate, year, institutionCode)
colnames(Can)<- c("species","long","lat", "date", "year", "inst")

# to display institutions and their respective # of observations (NORTH AMERICA)
Freq_inst <- Can %>% group_by(inst) %>% dplyr::summarize(Freq = n()) %>% ungroup()

####
####
####
####
####
####
####
####
####
####
####
####
####


#ODONATACENTRAL DATA
setwd("wd/distribution_models/")
ODCEN <- read.csv("OdonataCentral/odonata_central_20211007180848.csv")
ODCEN_all <- dplyr::select(ODCEN, Species.Scientific.Name, Longitude, Latitude, Date.Observed)
ODCEN_all$Longitude <- as.numeric(as.character(ODCEN_all$Longitude))
ODCEN_all$Latitude <- as.numeric(as.character(ODCEN_all$Latitude))

#add year only column
ODCEN_year <- ODCEN_all %>% rowwise() %>% mutate(year = substr(Date.Observed, 1, 4))
#rename columns
colnames(ODCEN_year)<- c("species","long","lat", "date", "year")

ODCEN_year <- ODCEN_year %>% rowwise() %>% mutate(date.x = substr(date, 1, 4))  #year
ODCEN_year <- ODCEN_year %>% rowwise() %>% mutate(date.y = substr(date, 9, 10)) #day
ODCEN_year <- ODCEN_year %>% rowwise() %>% mutate(date.z = substr(date, 6, 7))  #month
#concatenate cells for full date in the right format (year-month-day)
ODCEN_year <- within(ODCEN_year,  fulldate <- paste(date.x, date.z, date.y, sep="-"))
ODCEN_year <- dplyr::select(ODCEN_year, species, long, lat, fulldate, year)
colnames(ODCEN_year)<- c("species","long","lat", "date", "year")
ODCEN_year$inst <- "Odonata Central"
odcen <- ODCEN_year

####
####
####
####
####
####
####
####
####
####
####
####
####

#BIND ALL DATASETS
odonates <- rbind(gbif, Can, odcen)

odonates$continent <- with(odonates, ifelse(
  long >= -168 & long <= -44 & lat >= 8 & lat <= 85, "NAM", NA))


####### set date format

#NAM SPECIES
newdate <- as.data.frame(gsub("-", "_", odonates$date))
odonates['newdate'] = newdate
odonates <- dplyr::select(odonates, species, long, lat, newdate, year, continent, inst)

newdate <- as.data.frame(gsub("__", "NA", odonates$newdate))
odonates['newdate'] = newdate
odonates <- dplyr::select(odonates, species, long, lat, newdate, year, continent, inst)
colnames(odonates) <- c("species", "long", "lat", "date", "year", "continent", "inst")

odonates$date <- as.character(odonates$date)
odonates <- odonates %>% mutate(date = substr(date, 1, 10))

#SAVE DATASET
setwd("wd/distribution_models/dataprep/")
write.table(odonates, file = "rawdata.csv", quote = TRUE, sep = ",", row.names = FALSE)

#####################################
#####################################
# find number of observations in the raw dataset
setwd("wd/distribution_models/dataprep/")
o <- read.csv("rawdata.csv")
o <- o %>% dplyr::filter(continent == "NAM")
nrow(o)

######################
######################
######################
######################
######################

setwd("wd/distribution_models/dataprep")

#install packages
library(dplyr)
library(geosphere)
library(dismo)
library(raster)
library(rgdal)


Rawdata <- read.csv("rawdata.csv", header = TRUE, sep = ",", na.strings = c(""))

#convert year column to numeric
Rawdata$yearX <- as.numeric(as.character(Rawdata$year))
Rawdata <- dplyr::select(Rawdata, species, long, lat, date, yearX, continent)
colnames(Rawdata) <- c("species", "long", "lat", "date", "year", "continent")

#select years included in the study
Rawdata <- dplyr::filter(Rawdata, year >= 1900 & year <= 2021)

#convert longitude column to numeric
Rawdata$longX <- as.numeric(Rawdata$long)
Rawdata <- dplyr::select(Rawdata, species, longX, lat, date, year, continent)
colnames(Rawdata) <- c("species", "long", "lat", "date", "year", "continent")

#convert latitude column to numeric
Rawdata$latX <- as.numeric(Rawdata$lat)
Rawdata <- dplyr::select(Rawdata, species, long, latX, date, year, continent)
colnames(Rawdata) <- c("species", "long", "lat", "date", "year", "continent")

#CLEANING THE SPECIES COLUMN > Only keeping genus and species (first 2 words)
library(stringr)
species_clean <- word(Rawdata$species, 1, 2)
Rawdata['species_clean'] = species_clean
#remove old species column
data_species_clean <- dplyr::select(Rawdata, species_clean, long, lat, date, year, continent)
#change names
colnames(data_species_clean)<- c("species","long","lat", "date", "year", "continent")

#delete rows where species name contains "," 
data_species_clean1 <- data_species_clean[ !grepl(",", data_species_clean$species) , ]

#sort the dataframe by species name and continent
df8 <- data_species_clean1[order(data_species_clean1$species, data_species_clean1$continent),]

#change column order
df9 <- dplyr::select(df8, species, continent, lat, long, date, year)
df9 <- dplyr::filter(df9, species != "")
df9 <- dplyr::filter(df9, species != "NA")
df9 <- dplyr::filter(df9, species != " ")

#remove records outside study area
df9 <- df9 %>% dplyr::filter(continent != "NA")

write.table(df9, "odonates1.csv", quote = TRUE, sep = ",", row.names = FALSE)


############################
############################

setwd("wd/distribution_models/dataprep/")
data <- read.csv("odonates1.csv")

#Cordulia shurtleffii and shurtlefi so change for the former (as confirmed by ITIS)
data[] <- lapply(data, function(x) replace(x, grepl("Cordulia shurtleffi", x), "Cordulia shurtleffii"))
#species with multiple names must match phylogenetic data
#see the code scribbles where I show how to compare names of phylogeny and data
data[] <- lapply(data, function(x) replace(x, grepl("Tetragoneuria canis", x), "Epitheca canis"))
data[] <- lapply(data, function(x) replace(x, grepl("Tetragoneuria cynosura", x), "Epitheca cynosura"))
data[] <- lapply(data, function(x) replace(x, grepl("Ladona depressa", x), "Libellula depressa"))
data[] <- lapply(data, function(x) replace(x, grepl("Ladona fulva", x), "Libellula fulva"))
data[] <- lapply(data, function(x) replace(x, grepl("Enallagma annexum/vernale", x), "Enallagma_annexum"))
data <- data %>% dplyr::filter(species != "Enallagma boreale/cyanthigerum")
data <- data %>% dplyr::filter(species != "Aeshna interrupta?")
data <- data %>% dplyr::filter(species != "Enallagma boreale?")
data <- data %>% dplyr::filter(species != "Lestes forcipatus?")
data <- data[ !grepl("sp.", data$species) , ]

data$word_number <- sapply(strsplit(data$species, " "), length)
data <- data %>% dplyr::filter(word_number == 2)

## 1) ADD UNDERSCORES IN SPECIES NAMES OF THE DATA
## 2) SAVING EACH SPECIES INDIVIDUALLY IN A SEPERATE FOLDER

# matching names in data to names in phenology tree by adding underscores in the big data table

newspecies <- as.data.frame(gsub(" ", "_", data$species, fixed=TRUE))
data['newspecies'] = newspecies
#remove old species column
data <- data %>% dplyr::select(newspecies, continent, lat, long, date, year)
colnames(data) <- c("species", "continent", "lat", "long", "date", "year")

#Remove species sampled in remote islands (ex. Hawaii) since they have no where else to go:
data <- dplyr::filter(data, !(lat < 24 & long < -150 & continent == "NAM"))
data <- dplyr::filter(data, !(lat < 20 & long > -67 & continent == "NAM"))

# keep species with a minumum of 150 observations per time period
data <- unique(data)
data$timeperiod <- with(data, ifelse(
  year <= 1985, "P1", ifelse(
  year >= 2000, "P2", "NA")))
data <- data %>% dplyr::filter(timeperiod != "NA")

# identify species to keep
data_summarize <- data %>% group_by(species, continent, timeperiod) %>% dplyr::summarize(freq = n()) %>% ungroup()
data_sum1 <- data_summarize %>% dplyr::filter(freq >= 150 & timeperiod == "P1") %>% dplyr::select(species, continent)
data_sum2 <- data_summarize %>% dplyr::filter(freq >= 150 & timeperiod == "P2") %>% dplyr::select(species, continent)
data_summarize <- dplyr::inner_join(data_sum1, data_sum2)

data <- dplyr::inner_join(data, data_summarize)

write.table(data, "odonates2.csv", quote = TRUE, sep = ",", row.names = FALSE)
write.table(data, "odo_final_data.csv", quote = TRUE, sep = ",", row.names = FALSE)

############################
# supporting information: find how many observations per time period in the cleaned dataset
setwd("wd/distribution_models/dataprep/")
o <- read.csv("odo_final_data.csv")
o <- o %>% dplyr::filter(continent == "NAM")
nrow(o)
hist <- o %>% dplyr::filter(timeperiod == "P1")
curr <- o %>%  dplyr::filter(timeperiod == "P2")
nrow(hist)
nrow(curr)

#########################################
#########################################    #also save species in seperate csvs in the folder odeclim_byspecies

library(tidyverse)

setwd("wd/distribution_models/dataprep")
odeclim <- read.csv("odonates2.csv")

allNAM <- odeclim %>% dplyr::filter(continent == "NAM") # for methods
nrow(allNAM)


## separate by time period and save separately
odeclimP1_nam <- odeclim %>% dplyr::filter(timeperiod == "P1" & continent == "NAM")

odeclimP2_nam <- odeclim %>% dplyr::filter(timeperiod == "P2" & continent == "NAM")

# Make a copy
odeclim2_p1nam <- odeclimP1_nam

odeclim2_p2nam <- odeclimP2_nam


setwd("wd/distribution_models/data_byspecies_p1NAM/")
# Split by variable
spt2_p1 <- split(odeclim2_p1nam, odeclim2_p1nam$species)

# Save
invisible(lapply(names(spt2_p1), function(x){
  write_csv(spt2_p1[[x]], path = paste(x, ".csv", sep = ""))
}))

setwd("wd/distribution_models/data_byspecies_p2NAM/")
# Split by variable
spt2_p2 <- split(odeclim2_p2nam, odeclim2_p2nam$species)

# Save
invisible(lapply(names(spt2_p2), function(x){
  write_csv(spt2_p2[[x]], path = paste(x, ".csv", sep = ""))
}))





##################### SPATIALLY RAREFY OCCURENCE POINTS ON THE BASIS OF CLIMATE HETEROGENEITY

# first, make sure link 1prepareClim.R is done, where climate rasters are averaged and organized
# STEPS IN ORDER:

        # Clip to: NAM: -168, 85, -47, 8 & maintin clipping extent
        
        # Define to WGS84 (folder) - SDM toolbox

        # PROJECT RASTER (WGS84) to Equal Area projection (folder) - in SDM toolbox
              # choose World: equal area cylindrical

        # Raster to other format
              # project rasters to TIFF
                      
        # Raster calculator: tmp = Con(IsNull("tmp"), 0, "tmp")
                      # if there is any No Data, it doesn't work.
                      
        # raster to other format - to TIFF

        # raster to ASCII - in SDM toolbox

        # then, you are ready to use the tool 2a Calculate climate heterogeneity (it should be very quick!)
              
                      
# STEP 2
                      
#Tool 2b
#Neighborhood settings 3X3 rectangles were used to calculate heterogeneity (non significant effect to the model compared to 19 x 19)
#Percent of Eigenvalues is found in a text file created by the previous step.

# STEP 3                      

  # Create SHAPEFILE of SPECIES OBSERVATIONS --- OF P1
        # first, create a species list to use this tool in a batch
        # Use csv to shapefile tool in SDM toolbox - Basic tools, on individual species
                      
                      setwd("wd/distribution_models/data_byspecies_p1NAM")
                      sp <- list.files(pattern = ".csv$", full.names = F)
                      sp <- as.data.frame(sp)
                      sp$sp <- gsub(".csv", "", sp$sp)
                      
                      setwd("wd/distribution_models/climate/climate_heterogeneity/NAM")
                      write.table(sp, "1all_speciesP1.csv", quote = TRUE, sep = ",", row.names = FALSE)
                      
                      # open excel documents "1all_species.csv" and "2excel_txt_to_shp" (the one we will use for the next tool)
                      
                      # tool csv to shapefile - SDM toolbox - in a batch, copy-paste form the excel doc "excel_txt_to_shp"
                              # browse to data_byspeciesp1NAM, and select all species (to open the correct number of rows)
                              # copy and paste into excel_txt_to_shp page. and copy/paste species list from 1all_speciesP1
                                        # into excel_txt_tp_shp to complete the table, and copy/paste it back into the batch tool
                                        # of csv to shapefile
                              # save shapefiles in folder species_SHP (inside heterogeneity folder) *****
                      
  # Tool Spatially Rarefy Occurrence Data for SDMs (reduce spatial autocorrelation)
        #Removes clusters of observations as to reduce risk of introducing bias in model.
        
              # settings: Multidistance: min = 5 km and max = 25 km using classification type, number of heterogeneity classes = 2

# STEP 5
                      
# Shapefile to CSV tool (SDMtoolbox) - batch
          # save this new dataset to be used in distribution models
          # this is the FINAL dataset for P1 observations

               # move on to the 3_running_model.R step       
                      
                      
                      
                      
#####################################################################################################################
#####################################################################################################################                      
#####################################################################################################################                      

# since this step is only for observations of the current time period (P2), no need to re-run everytime you run your maxent models
                      
#################################################### associate CURRENT (P2) observations to fishnet
                      
                      library(rgdal)
                      library(sp)
                      library(maps)
                      library(maptools)
                      library(ggplot2)
                      
                      # prepare data for the grid of 100x100 quadrats
                      
                      setwd("wd/distribution_models/fishnet/")
                      #to prepare a second grid for extracting grid square name:
                      grid <- readOGR("fishnet100.shp")
                      #save the attribute table
                      write.table(grid@data,"att_table.csv",quote=T,row.names=T,sep=",") 
                      #read att table
                      att_table <- read.csv("att_table.csv")
                      att_table <- cbind(rownames(att_table), data.frame(att_table, row.names=NULL))
                      colnames(att_table) <- c("Id", "todelete")
                      att_table <- dplyr::select(att_table, Id)
                      att_table$Id <- paste("sq",att_table$Id, sep = "")
                      grid@data$Id <- att_table$Id
                      writeOGR(grid, ".", "fishnet_temp", driver="ESRI Shapefile", overwrite_layer=TRUE) 
                      
                      latlong2grid <- function(pointsDF) {
                        setwd("wd/distribution_models/fishnet/")
                        grid <- readOGR("fishnet_temp.shp")
                        grid <- SpatialPolygons2map(grid, namefield = "Id")
                        grid <- map(grid, fill=TRUE, col="transparent", plot=FALSE)
                        IDs <- sapply(strsplit(grid$names, ":"), function(x) x[1])
                        gridsquare_sp <- map2SpatialPolygons(grid, IDs=IDs, proj4string=CRS("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 
                                                                                            +datum=WGS84 +units=m +no_defs"))
                        
                        pointsSP <- SpatialPoints(pointsDF, proj4string=CRS("+init=epsg:4326"))
                        pointsSP <- spTransform(pointsSP, CRS("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 
                                                              +datum=WGS84 +units=m +no_defs"))
                        indices <- over(pointsSP, gridsquare_sp)
                        
                        gridsquareNames <- sapply(gridsquare_sp@polygons, function(x) x@ID)
                        gridsquareNames[indices]
                      }
                      
                      ####### SELECT CONTINENT 
                      
                      continent <- "NAM" 
                      
                      dir.create(paste0("wd/distribution_models/p2_presence", continent, "/"))
                      
                      #######
                      
                      # iterate through all the species to apply the function above
                      setwd(paste0("wd/distribution_models/data_byspecies_p2", continent))
                      
                      spfiles <- list.files(pattern="csv", all.files=FALSE, full.names = FALSE)
                      path = paste0("wd/distribution_models/data_byspecies_p2", continent, "/")
                      output = paste0("wd/distribution_models/p2_presence", continent, "/")
                      
                      for (i in spfiles) {
                        setwd(path)
                        odes_table <- read.csv(i)
                        odes_full <- dplyr::select(odes_table, species, continent, lat, long, year, date)
                        
                        #round up lat and long
                        odes_full$lat <- round(as.numeric(odes_full$lat), digits=2)
                        odes_full$long <- round(as.numeric(odes_full$long), digits=2)
                        
                        #find unique species location years
                        odes_full <- unique(odes_full)
                        odes_coord <- dplyr::select(odes_full, long, lat)
                        
                        #associate grid with all observation data
                        odes_coord$gridsquare <- latlong2grid(odes_coord)
                        odes_full$gridsquare <- odes_coord$gridsquare
                        odes_full <- na.omit(odes_full) # only keep grid squares in the fishnet 
                        odes_full$gridsquare <- gsub("sq", "", odes_full$gridsquare)
                        
                        odes_full$presence <- 1
                        
                        # save file in modelEval
                        setwd(output)
                        write.table(odes_full, file = paste0(i, ".csv"), 
                                    quote = TRUE, sep = ",", row.names = FALSE)
                      }
                      
                      
                      
                      
#####################################################################################################################
#Do the same thing for P1 so that when you analyse the results you can control for species that were better sampled 
#           across the study period 
#################################################### associate CURRENT (P2) observations to fishnet

library(rgdal)
library(sp)
library(maps)
library(maptools)
library(ggplot2)

# prepare data for the grid of 100x100 quadrats

setwd("wd/distribution_models/fishnet/")
#to prepare a second grid for extracting grid square name:
grid <- readOGR("fishnet100.shp")
#save the attribute table
write.table(grid@data,"att_table.csv",quote=T,row.names=T,sep=",") 
#read att table
att_table <- read.csv("att_table.csv")
att_table <- cbind(rownames(att_table), data.frame(att_table, row.names=NULL))
colnames(att_table) <- c("Id", "todelete")
att_table <- dplyr::select(att_table, Id)
att_table$Id <- paste("sq",att_table$Id, sep = "")
grid@data$Id <- att_table$Id
writeOGR(grid, ".", "fishnet_temp", driver="ESRI Shapefile", overwrite_layer=TRUE) 

latlong2grid <- function(pointsDF) {
  setwd("wd/distribution_models/fishnet/")
  grid <- readOGR("fishnet_temp.shp")
  grid <- SpatialPolygons2map(grid, namefield = "Id")
  grid <- map(grid, fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(grid$names, ":"), function(x) x[1])
  gridsquare_sp <- map2SpatialPolygons(grid, IDs=IDs, proj4string=CRS("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 
    +datum=WGS84 +units=m +no_defs"))
  
  pointsSP <- SpatialPoints(pointsDF, proj4string=CRS("+init=epsg:4326"))
  pointsSP <- spTransform(pointsSP, CRS("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 
                  +datum=WGS84 +units=m +no_defs"))
  indices <- over(pointsSP, gridsquare_sp)
  
  gridsquareNames <- sapply(gridsquare_sp@polygons, function(x) x@ID)
  gridsquareNames[indices]
}

####### SELECT CONTINENT 

continent <- "NAM" 
dir.create(paste0("wd/distribution_models/p1_presence", continent, "/"))

#######

# iterate through all the species to apply the function above
setwd(paste0("wd/distribution_models/data_byspecies_p1", continent))

spfiles <- list.files(pattern="csv", all.files=FALSE, full.names = FALSE)
path = paste0("wd/distribution_models/data_byspecies_p1", continent, "/")
output = paste0("wd/distribution_models/p1_presence", continent, "/")

for (i in spfiles) {
  setwd(path)
  odes_table <- read.csv(i)
  odes_full <- dplyr::select(odes_table, species, continent, lat, long, year, date)
  
  #round up lat and long
  odes_full$lat <- round(as.numeric(odes_full$lat), digits=2)
  odes_full$long <- round(as.numeric(odes_full$long), digits=2)
  
  #find unique species location years
  odes_full <- unique(odes_full)
  odes_coord <- dplyr::select(odes_full, long, lat)
  
  #associate grid with all observation data
  odes_coord$gridsquare <- latlong2grid(odes_coord)
  odes_full$gridsquare <- odes_coord$gridsquare
  odes_full <- na.omit(odes_full) # only keep grid squares in the fishnet 
  odes_full$gridsquare <- gsub("sq", "", odes_full$gridsquare)
  
  odes_full$presence <- 1
  
  # save file in modelEval
  setwd(output)
  write.table(odes_full, file = paste0(i, ".csv"), 
              quote = TRUE, sep = ",", row.names = FALSE)
}

                      


####################
####################
####################
####################
####################

#LOADING PHYLOGENY DATA AND CHANGING LABELS (NOT NEEDED TO RE-RUN)

setwd("wd/NEWPHYLO")
library(data.tree)
library(ape)
library(phytools)
library(dplyr)
library(phylotools)
library(stringr)

load("C:wd/NEWPHYLO/tree.rda")
OdeTree <- tree

#Fix species names
#oldNames <- as.data.frame(OdeTree$tip.label)
#colnames(oldNames) <- c("species")
#tips <- as.data.frame(gsub("_", " ", oldNames$species, fixed=TRUE))
#colnames(tips) <- c("species")
#tips <- as.data.frame(gsub("'", "", tips$species, fixed=TRUE))
#colnames(tips) <- c("species")
#tips <- as.data.frame(gsub("-", " ", tips$species, fixed=TRUE))
#colnames(tips) <- c("species")

#species_clean <- word(tips$species, 1, 2)
#df_sp_clean <- as.data.frame(species_clean)
#number all species names that are not unique
#species_clean <- make.unique(as.character(df_sp_clean$species_clean), sep = "")

#tips['species_clean'] = species_clean
#remove old species column
#data <- dplyr::select(tips, species_clean)
#colnames(data) <- c("newnames")
#newnames <- data
#head(newnames)

#write.table(newnames, "newnames.csv", quote = TRUE, sep = ",", row.names = FALSE)

#newnames <- read.csv("newnames.csv", header = TRUE)
#colnames(newnames) <- ("new")

#newnames <- newnames %>% mutate(old = oldNames$species)
#head(newnames)

#change order of columns
#dat <- dplyr::select(newnames, old, new)

#replacing the tip label names
#OdeTree <- phylotools::sub.taxa.label(OdeTree, dat)
#head(OdeTree)

#save the tree in the working directory of the analyses
setwd("wd/distribution_models/")
write.tree(OdeTree, "OdeTree.tre", digits = 10, tree.names = FALSE)




