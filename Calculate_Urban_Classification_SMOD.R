# Script to Categorise Admin Units of any L2 Country Shapfile into a
# GHSL-SMOD Urban classification as defined @ 
# https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2019.pdf
# Designed to reproduce results from Degree of Urbanisation Territorial Units Classifier (GHS-DU-TUC) Tool
# Author: G.E.Rogers@soton.ac.uk

#Install and Load Required Packages as needed
packages <- c("raster","exactextractr","sf","rgdal","dplyr","hablar","fasterize","stars","rmapshaper")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{install.packages(setdiff(packages, rownames(installed.packages())))}
invisible(lapply(packages, require, character.only = TRUE))

#Set Working Directory to location of Current R File
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1.	Load global GHS SMOD land classification raster and clip to L0 country of interest
# 2.	Extract each urban class into 8 separate rasters and convert to binary (0,1)
# 3.	Load Population raster and clip to L0 country of interest
# 4.	Multiply population raster by the binary urban classification rasters
# 5.	Load the Country L2 Shapefile, convert to 50m Resolution Raster and convert back to shapefile to 
#     Run zonal statistics to calculate sum of population for each urban classification in each admin unit
# 6.  Determine final admin unit class by a nested hierarchy then majority.  
#     First group the SMOD population class together:  Group 1 - (10,11,12,12) Group 2 - (21,22,23) Group 3 - (30)
#     Use Group 1 if Group 1 > 50% Total Population
#     Use Group 2 if Group 1 < 50% & Group 3 < 50% Total Population
#     Use Group 3 if if Group 3 > 50% Total Population
#     Then within the highest group, the individual highest value provides the final SMOD admin unit class
#     Also include the "Degree of Urbanisation Class"  
#     https://ec.europa.eu/eurostat/ramon/miscellaneous/index.cfm?TargetUrl=DSP_DEGURBA
# 8.  Output Results to CSV

# Expected Files
#
# SMOD GLOBAL: "POPULATION_RASTERS/GHS_SMOD_GLOBAL_WGS84.tif"
# Obtained @ https://ghsl.jrc.ec.europa.eu/download.php?ds=smod
#
# POPULATION (WORLDPOP): "POPULATION_RASTERS/ppp_2020_global_refactored.tif"
# Obtained @ https://www.worldpop.org/geodata/summary?id=24777
# Reprojected from WGS84 to ESRI:54009 - World_Mollweide
# and set to 1km x 1km resolution to match SMOD Raster (using Gdal Warp)
#
# COUNTRY L2 (WORLDPOP): "GLH_SHAPEFILES/{country_iso}/{country_iso}_L2_UTM_GLH.shp"
# COUNTRY L0 (WORLDPOP): "GLH_SHAPEFILES/{country_iso}/{country_iso}_L0_buffer.shp"

#Define the Coordinate References System used by both the population and SMOD Raster
#Mollweide is setup as default to match the output of SMOD's own DEBUGRBA Tool
#But can use WGS84 to avoid distorting the original WorldPop Raster
crs_selected <- "mollweide"

if(crs_selected == "mollweide")
{ crs_active <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs " #Define Mollweide CRS
} else if (crs_selected == "wgs84")
{ crs_active <- "epsg:4326" #Define WGS84 CRS
} else { stop("No CRS Specified")}

#Define Country of Interest (alpha3 ISO) 
#countries_of_interest <- c("MOZ","KEN","NAM","RWA","ZAF","BWA","EGY","GHA","SWZ","TZA","ZAF")
countries_of_interest <- c("KEN")


#Loop Over All Countries
for(country_of_interest in countries_of_interest)
{
  
# Load global GHS SMOD land classification raster and clip to L0 country of interest ####

#Load L0 Country Buffer Shapefile (for clipping Global Rasters)
country_L0_filename   <- paste("GLH_SHAPEFILES/",country_of_interest,"/",country_of_interest,"_L0_buffer.shp",sep="")
country_L0_shapefile  <- st_read(country_L0_filename,quiet = TRUE)

#Reproject to ESRI 54009 (GHSL Recommended CRS)
country_L0_shapefile <- st_transform(country_L0_shapefile,crs = crs_active)

#Load Global SMOD Raster and clip to Country L0 100km Buffer (for efficiency)
smod_raster_global    <- raster("POPULATION_RASTERS/GHS_SMOD_GLOBAL.tif")
smod_raster_local     <- crop(smod_raster_global, country_L0_shapefile)

# Extract each urban class into 8 separate rasters and convert to binary (0,1) ####

#The 8 SMOD Raster Classes : 10 11 12 13 21 22 23 30 

# Extract 10 Component of SMOD Raster
mask_raster_10     <- smod_raster_local
mask_raster_10[mask_raster_10 != 10] <- NA
raster_10          <- mask(smod_raster_local,mask_raster_10)
# Reclass 10 Component to 1
raster_10[raster_10 == 10] <- 1

# Extract 11 Component of SMOD Raster
mask_raster_11     <- smod_raster_local
mask_raster_11[mask_raster_11 != 11] <- NA
raster_11          <- mask(smod_raster_local,mask_raster_11)
# Reclass 11 Component to 1
raster_11[raster_11 == 11] <- 1

# Extract 12 Component of SMOD Raster
mask_raster_12     <- smod_raster_local
mask_raster_12[mask_raster_12 != 12] <- NA
raster_12          <- mask(smod_raster_local,mask_raster_12)
# Reclass 12 Component to 1
raster_12[raster_12 == 12] <- 1

# Extract 13 Component of SMOD Raster
mask_raster_13     <- smod_raster_local
mask_raster_13[mask_raster_13 != 13] <- NA
raster_13          <- mask(smod_raster_local,mask_raster_13)
# Reclass 13 Component to 1
raster_13[raster_13 == 13] <- 1

# Extract 21 Component of SMOD Raster
mask_raster_21     <- smod_raster_local
mask_raster_21[mask_raster_21 != 21] <- NA
raster_21          <- mask(smod_raster_local,mask_raster_21)
# Reclass 21 Component to 1
raster_21[raster_21 == 21] <- 1

# Extract 22 Component of SMOD Raster
mask_raster_22     <- smod_raster_local
mask_raster_22[mask_raster_22 != 22] <- NA
raster_22          <- mask(smod_raster_local,mask_raster_22)
# Reclass 22 Component to 1
raster_22[raster_22 == 22] <- 1

# Extract 23 Component of SMOD Raster
mask_raster_23     <- smod_raster_local
mask_raster_23[mask_raster_23 != 23] <- NA
raster_23          <- mask(smod_raster_local,mask_raster_23)
# Reclass 23 Component to 1
raster_23[raster_23 == 23] <- 1

# Extract 30 Component of SMOD Raster
mask_raster_30     <- smod_raster_local
mask_raster_30[mask_raster_30 != 30] <- NA
raster_30          <- mask(smod_raster_local,mask_raster_30)
# Reclass 30 Component to 1
raster_30[raster_30 == 30] <- 1

#Cleanup Finsished Rasters
remove(mask_raster_10,mask_raster_11,mask_raster_12,mask_raster_13,mask_raster_21,mask_raster_22,mask_raster_23,mask_raster_30)
remove(smod_raster_global,smod_raster_local)

# Load Population raster and clip to L0 country of interest ####

#Load World Pop Population Raster (Reprojected to ESRI 54009 & Cell Resolution warped to 1x1km)
population_raster_global  <- raster("POPULATION_RASTERS/ppp_2020_global_refactored.tif") 

#Clip Population Raster to Country L0 100km Buffer (for efficiency)
population_raster_local   <- crop(population_raster_global, country_L0_shapefile)

# Multiply population raster by the binary urban classification rasters ####

# Remap the Extents to be inline with populations rasters (to avoid warnings)
extent(raster_10) <- extent(population_raster_local)
extent(raster_11) <- extent(population_raster_local)
extent(raster_12) <- extent(population_raster_local)
extent(raster_13) <- extent(population_raster_local)
extent(raster_21) <- extent(population_raster_local)
extent(raster_22) <- extent(population_raster_local)
extent(raster_23) <- extent(population_raster_local)
extent(raster_30) <- extent(population_raster_local)

#Multiply Population by SMOD Classification Rasters
raster_10_pop  <- raster_10*population_raster_local
raster_11_pop  <- raster_11*population_raster_local
raster_12_pop  <- raster_12*population_raster_local
raster_13_pop  <- raster_13*population_raster_local
raster_21_pop  <- raster_21*population_raster_local
raster_22_pop  <- raster_22*population_raster_local
raster_23_pop  <- raster_23*population_raster_local
raster_30_pop  <- raster_30*population_raster_local

#Cleanup Finished Rasters
remove(population_raster_global,population_raster_local)

# Run zonal statistics with country L2 shapefile to calculate sum of population for each ####

#Load Country L2 Admin Unit Shapefile
country_L2_filename       <- paste("GLH_SHAPEFILES/",country_of_interest,"/",country_of_interest,"_L2_UTM_GLH.shp",sep="")
if (!file.exists(country_L2_filename))
  country_L2_filename     <- paste("GLH_SHAPEFILES/",country_of_interest,"/",country_of_interest,"_L2_Albers_GLH.shp",sep="")
country_L2_shapefile      <- st_read(country_L2_filename,quiet=TRUE)

#Begin process to rasterise the shapefile and convert back to shapefile to ensure all geometries overlap with population rasters

#Reproject to Molinar
country_L2_shapefile      <- st_transform(country_L2_shapefile,crs=crs_active)

#Create Raster Mask (50m Resolution) 
rasterise_mask            <- raster(extent(country_L2_shapefile),res=50,crs=crs_active)

#Convert L2 Shapefile to Raster
L2_raster                 <- fasterize(country_L2_shapefile, rasterise_mask,field="ADM2_id")

#Convert L2 Raster Back into Shapefile
L2_raster_shapefile       <- st_as_sf(st_as_stars(L2_raster), as_points = FALSE, merge = TRUE)

#Remove any invalid geometries if detected 
if("FALSE" %in% st_is_valid(L2_raster_shapefile))
{ L2_raster_shapefile     <- st_make_valid(L2_raster_shapefile); cat("Fixing Invalid Geometries") } 

#Dissolve Any Seperate Shapefile Features that have the Same Admin Name
L2_raster_shapefile_dissolved <- ms_dissolve(L2_raster_shapefile,field = "layer")

#Rename Admin ID Column
names(L2_raster_shapefile_dissolved)[names(L2_raster_shapefile_dissolved) == "layer"] <- "UID"

#Convert UID to Integer Format
L2_raster_shapefile_dissolved$UID <- as.integer(L2_raster_shapefile_dissolved$UID)

country_shapefile_stats <- L2_raster_shapefile_dissolved

#Setup duplicate for Zonal Stats
country_shapefile_stats   <- country_L2_shapefile

#Run Zonal Stats to obtain the Sum of Population for Each Urban Classification
country_shapefile_stats$sum_10     <- exact_extract(raster_10_pop, country_shapefile_stats,'sum')
country_shapefile_stats$sum_11     <- exact_extract(raster_11_pop, country_shapefile_stats,'sum')
country_shapefile_stats$sum_12     <- exact_extract(raster_12_pop, country_shapefile_stats,'sum')
country_shapefile_stats$sum_13     <- exact_extract(raster_13_pop, country_shapefile_stats,'sum')
country_shapefile_stats$sum_21     <- exact_extract(raster_21_pop, country_shapefile_stats,'sum')
country_shapefile_stats$sum_22     <- exact_extract(raster_22_pop, country_shapefile_stats,'sum')
country_shapefile_stats$sum_23     <- exact_extract(raster_23_pop, country_shapefile_stats,'sum')
country_shapefile_stats$sum_30     <- exact_extract(raster_30_pop, country_shapefile_stats,'sum')

#cleanup Finished Rasters
remove(raster_10_pop,raster_11_pop,raster_12_pop,raster_13_pop,raster_21_pop,raster_22_pop,raster_23_pop,raster_30_pop)

# Convert Final Country Shapefile To Dataframe & Prepare for CSV Export 
country_df          <- as.data.frame(country_shapefile_stats)
country_df$geometry <- NULL
country_df$ADM2     <- NULL
#Rename Admin ID Column
names(country_df)[names(country_df) == "ADM2_id"] <- "UID"

#Create Urban Classification Columns 'Class'
country_df$class    <- "0"
#Create Degree of Urbanisation 'Class' 
country_df$DOU_class    <- "0"


# Determine final admin unit classification ####

#Create Combined Total Population Column 
country_df$total_pop    <- country_df$sum_10 + country_df$sum_11 + country_df$sum_12 + country_df$sum_13 +
                           country_df$sum_21 + country_df$sum_22 + country_df$sum_23 + country_df$sum_30

#Create Group Classification 
country_df$Group_Rural    <- country_df$sum_10 + country_df$sum_11 + country_df$sum_12 + country_df$sum_13
country_df$Group_Suburban <- country_df$sum_21 + country_df$sum_22 + country_df$sum_23 #not actually used
country_df$Group_City     <- country_df$sum_30

#### Final classification per admin unit is determined ####

#Determine Which Column has highest value and assign classification accordingly
for(i in 1:length(country_df[[1]]))
{
  #Follow classification rules as specified in the script description
  if (country_df$Group_Rural[i] > country_df$total_pop[i]*0.5 ) 
  { pop_columns_subset <- country_df[,c("sum_10","sum_11","sum_12","sum_13")] ; country_df$DOU_class[i]="rural" }
  if (country_df$Group_Rural[i] < country_df$total_pop[i]*0.5 && country_df$Group_City[i] < country_df$total_pop[i]*0.5 ) 
  { pop_columns_subset <- country_df[,c("sum_21","sum_22","sum_23")] ; country_df$DOU_class[i]="suburban" } 
  if (country_df$Group_City[i]  > country_df$total_pop[i]*0.5 ) 
  { pop_columns_subset <- country_df[,c("sum_30"),drop=FALSE]  ; country_df$DOU_class[i]="city"}
  
  #Determine which column of the population subsets contains the highest value
  max_population_column_id <- which(pop_columns_subset == max(s(pop_columns_subset[i,]),na.rm=TRUE),arr.ind=TRUE)[[2]]
  
  #Assign final class label based on a string extraction of the column header
  country_df$class[i] <- substr(colnames(pop_columns_subset)[max_population_column_id], 
                         nchar(colnames(pop_columns_subset)[max_population_column_id])-2+1,
                         nchar(colnames(pop_columns_subset)[max_population_column_id]))
}

#Extract Only UID & SMOD Classification Column for Simple CSV Output
country_df_simple = country_df[, grepl(paste("UID|\\bclass\\b",sep="|") , names(country_df) )]

#### Output Results to CSV ####

output_directory = "CLASSIFICATION_DATA_SMOD/"

if (!dir.exists(output_directory)){ dir.create(output_directory) }

#Setup Output Filename for simple CSV output
classification_output_simple_filename <- paste(output_directory,country_of_interest,"_Urban_Classifications.csv",sep="")

#Setup Output Filename for full CSV output
classification_output_full_filename <- paste(output_directory,country_of_interest,"_Urban_Classifications_FULL.csv",sep="")

#Output Simple Classification to CSV
if (file.exists(classification_output_simple_filename))
  file.remove(classification_output_simple_filename)
  write.csv(country_df_simple,classification_output_simple_filename,row.names=FALSE)
  
#Output Detailed Classification to CSV
if (file.exists(classification_output_full_filename))
  file.remove(classification_output_full_filename)
  write.csv(country_df,classification_output_full_filename,row.names=FALSE)

}