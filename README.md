# Filling in the gaps: Climate variability and pesticide application cause predictions of odonate ranges to fail 
Access the code for this project at https://github.com/SusanGordon/Filling-in-the-gaps-causes-of-odonate-range-prediction-failure-
This code accompanies a project on drivers of range prediction failure for odonates (dragonflies and damselflies), with ranges predicted using
species distribution models (SDMs).

Data are already published and publicly available from the following repositories: 
- Harris, I., T. J. Osborn, P. Jones, and D. Lister. 2020. Version 4 of the CRUTS monthly high-
resolution gridded multivariate climate dataset. Scientific Data 7:109. 
https://catalogue.ceda.ac.uk/uuid/c26a65020a5e4b80b20018f148556681/
- Goldewijk, K. K., A. Beusen, G. van Drecht, and M. de Vos. 2011. The HYDE 3.1 spatially 
explicit database of human-induced global land-use change over the past 12,000 years. Global 
Ecology and Biogeography 20:73–86. https://public.yoda.uu.nl/geo/UU01/8K9D7F.html
- Thelin, G. P., and W. W. Stone. 2013. Estimation of annual agricultural pesticide use for counties 
of the conterminous United States, 1992-2009. Page Scientific Investigations Report. U.S. Geological Survey. 
https://www.sciencebase.gov/catalog/item/6081ae7cd34e8564d6866222

## File Structure
There are four files contained in this repository, which were originally used for this project. Please note that these files contain multiple
setwd() commands; this script should be used as a guide.

- 1prepareClim.R: this code was used to prepare climatic data for analysis; the file includes both code and instructions for processing
- 2records_prepare.R: this code was used to prepare odonate observation data for analysis; the file includes both code and instructions for processing
- 3running_model.R: this code was used to run the species distribution models
- 4model_analysis.R: this code was used to analyse the outputs of species distribution models

## Code/Software
This code is run in R statistical software.
