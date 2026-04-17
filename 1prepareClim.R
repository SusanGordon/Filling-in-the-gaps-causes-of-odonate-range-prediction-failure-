

################################################################################ load and prepare climatic variables

# download data - cru_ts_4.05

# https://data.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.05
# variables: https://crudata.uea.ac.uk/cru/data/hrg/#vars
  # A gridded time-series dataset
  # This version, released 17 March 2021, covers the period 1901-2020
  # Dataset DOI will appear here when granted.
  # Coverage: All land areas (excluding Antarctica) at 0.5? resolution
  # Variables: pre, tmp, tmx, tmn, dtr, vap, cld, wet, frs, pet
  # NEW Reference: Harris et al. (2020) doi:10.1038/s41597-020-0453-3 (click to access)

#STEP 1) use the program 7-zip and extract all climate files
#type path to the downloaded CRU data files
#select all .dat.nc
#right click on one of them -> 7-zip -> extract files -> type destination path -> extract
#copy paste all new files in one folder to batch steps in arcmap

#STEP 2) on arcmap use the tool MakeCDF Raster Layer (multidimension) - do this step by hand
#use time as the band dimension (NOT dimension values) - result is 3 bands
#use the toolbox (downloaded online, saved on desktop) "NetCDF_time_slice_to_raster" - result is in .tif - batch 
    # (make sure to create empty year folders first)

#STEP 3)
#afterwards you can read the rasters on R (120 bands for 120 months in 10 years) and calculate yearly average
# see code in climate folder and use "replace all" depending on the variable

#STEP 4) Copy the years you want in the 
#folder for that time period (p1 or p2) before calculating means to make it easy and switch up the time periods.

library(raster)
library(dplyr)

#######################################################################################################################################
# average according to time periods: 1901 - 1980, compared to 2000 - 2021

###### specifically for tmx, you need to use the hottest temperature of July
#Stack all files from each period
setwd("wd/distribution_models/climate/tmx/p1")
p1files <- list.files(path=getwd(), pattern="*.grd$", all.files=FALSE, full.names = FALSE)
s.p1 <- stack(p1files)
a <- mean(s.p1, na.rm=TRUE)
setwd("wd/distribution_models/climate/p1")
writeRaster(a, "tmx", overwrite = TRUE)

setwd("wd/distribution_models/climate/tmx/p2")
p2files <- list.files(path=getwd(), pattern="*.grd$", all.files=FALSE, full.names = FALSE)
s.p2 <- stack(p2files)
b <- mean(s.p2, na.rm=TRUE)
setwd("wd/distribution_models/climate/p2")
writeRaster(b, "tmx", overwrite = TRUE)




######### same thing as above, but for precipitation

#Stack all files from each period
setwd("wd/distribution_models/climate/pre/p1")
p1files <- list.files(path=getwd(), pattern="*.grd$", all.files=FALSE, full.names = FALSE)
s.p1 <- stack(p1files)
a <- mean(s.p1, na.rm=TRUE)
setwd("wd/distribution_models/climate/p1")
writeRaster(a, "pre", overwrite = TRUE)

setwd("wd/distribution_models/climate/pre/p2")
p2files <- list.files(path=getwd(), pattern="*.grd$", all.files=FALSE, full.names = FALSE)
s.p2 <- stack(p2files)
b <- mean(s.p2, na.rm=TRUE)
setwd("wd/distribution_models/climate/p2")
writeRaster(b, "pre", overwrite = TRUE)



######### same thing as above, but for diurnal temperature range

#Stack all files from each period
setwd("wd/distribution_models/climate/dtr/p1")
p1files <- list.files(path=getwd(), pattern="*.grd$", all.files=FALSE, full.names = FALSE)
s.p1 <- stack(p1files)
a <- mean(s.p1, na.rm=TRUE)
setwd("wd/distribution_models/climate/p1")
writeRaster(a, "dtr", overwrite = TRUE)

setwd("wd/distribution_models/climate/dtr/p2")
p2files <- list.files(path=getwd(), pattern="*.grd$", all.files=FALSE, full.names = FALSE)
s.p2 <- stack(p2files)
b <- mean(s.p2, na.rm=TRUE)
setwd("wd/distribution_models/climate/p2")
writeRaster(b, "dtr", overwrite = TRUE)


######### same thing as above, but for average surface temperature

#Stack all files from each period
setwd("wd/distribution_models/climate/tmp/p1")
p1files <- list.files(path=getwd(), pattern="*.grd$", all.files=FALSE, full.names = FALSE)
s.p1 <- stack(p1files)
a <- mean(s.p1, na.rm=TRUE)
setwd("wd/distribution_models/climate/p1")
writeRaster(a, "tmp", overwrite = TRUE)

setwd("wd/distribution_models/climate/tmp/p2")
p2files <- list.files(path=getwd(), pattern="*.grd$", all.files=FALSE, full.names = FALSE)
s.p2 <- stack(p2files)
b <- mean(s.p2, na.rm=TRUE)
setwd("wd/distribution_models/climate/p2")
writeRaster(b, "tmp", overwrite = TRUE)

###################################################################################################################
###################################################################################################################
###################################################################################################################


# prepare future climate models

# download models from: https://www.worldclim.org/data/cmip6/cmip6_clim2.5m.html

#only run if using a new future climate model:
# 1) in arcmap: make raster layer to each band
# 2) right click layer - data- export data and save to futureClimModels/layers_selected
# 3) raster to ASCII in SDM toolbox
# 4) run this code:

setwd("wd/distribution_models/futureClimModels/layers_selected/canesmASCII/")
fil <- list.files(pattern = ".asc$", full.names = T)
for (i in fil) {
  r <- raster(i)
  name <- paste0(gsub(".asc", "", i))
  writeRaster(r, paste0(name, ".grd"))
}

setwd("wd/distribution_models/futureClimModels/layers_selected/cnrmASCII/")
fil <- list.files(pattern = ".asc$", full.names = T)
for (i in fil) {
  r <- raster(i)
  name <- paste0(gsub(".asc", "", i))
  writeRaster(r, paste0(name, ".grd"))
}





