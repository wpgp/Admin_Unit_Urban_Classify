# Admin_Unit_Urban_Classify
R-Script to Assign an GHSL-SMOD Urban Classification to L2 Admin Units
as defined @ https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2019.pdf

Designed to reproduce results from Degree of Urbanisation Territorial Units Classifier (GHS-DU-TUC) Tool
Author: G.E.Rogers@soton.ac.uk

1.	Load global GHS SMOD land classification raster and clip to L0 country of interest
2.	Extract each urban class into 8 separate rasters and convert to binary (0,1)
3.	Load Population raster and clip to L0 country of interest
4.	Multiply population raster by the binary urban classification rasters
5.	Load the Country L2 Shapefile, convert to 50m Resolution Raster and convert back to shapefile to 
    Run zonal statistics to calculate sum of population for each urban classification in each admin unit
6.  Determine final admin unit class by a nested hierarchy then majority.  
    First group the SMOD population class together:  Group 1 - (10,11,12,12) Group 2 - (21,22,23) Group 3 - (30)
    Use Group 1 if Group 1 > 50% Total Population
    Use Group 2 if Group 1 < 50% & Group 3 < 50% Total Population
    Use Group 3 if if Group 3 > 50% Total Population
    Then within the highest group, the individual highest value provides the final SMOD admin unit class
    Also include the "Degree of Urbanisation Class"  
    https://ec.europa.eu/eurostat/ramon/miscellaneous/index.cfm?TargetUrl=DSP_DEGURBA
8.  Output Results to CSV

Expected Files

SMOD GLOBAL: "POPULATION_RASTERS/GHS_SMOD_GLOBAL.tif"
Obtained @ https://ghsl.jrc.ec.europa.eu/download.php?ds=smod

POPULATION (WORLDPOP): "POPULATION_RASTERS/ppp_2020_global_refactored.tif"
Obtained @ https://www.worldpop.org/geodata/summary?id=24777
Reprojected from WGS84 to ESRI:54009 - World_Mollweide
and set to 1km x 1km resolution to match SMOD Raster (using Gdal Warp)

COUNTRY L2 (WORLDPOP): "GLH_SHAPEFILES/{country_iso}/{country_iso}_L2_UTM_GLH.shp"
COUNTRY L0 (WORLDPOP): "GLH_SHAPEFILES/{country_iso}/{country_iso}_L0_buffer.shp"
