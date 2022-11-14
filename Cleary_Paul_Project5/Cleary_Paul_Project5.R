library("raster")
library("sf")
library("tidyr")
library("tidyverse")
library("plyr")
library("dplyr")
library("tmap")
library("tmaptools")
library("RColorBrewer")
library("lwgeom")
library("Hmisc")
library("exactextractr")
library("GISTools")
library("rgeos")
#--------------------Data Preperation-------------------------
# Set the working directory
setwd('~/Fall 19/Data Analytics/Cleary_Paul_Project5')
wd <- '~/Fall 19/Data Analytics/Cleary_Paul_Project5'
# Import the DEM
dem <- raster("IN_DEM_UTM/indiana_utm84/w001001.adf")
# Import the Indiana shapefile
IN <- st_read("Census_Counties/Census_County_TIGER00_IN.shp")
# Import the National Land Cover Data
nlcd_path <- "/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img"
nlcd <- raster(paste0(wd,nlcd_path))
# Import the Indiana Flood Plain Data
flood <- st_read("Floodplains_FIRM/Floodplains_FIRM_IDNR_IN.shp")
#Flood data has invalid geometries
flood <- st_make_valid(flood)
# Project everything to match the nlcd, previous attempts to use UTM 16N as the base have 
# proven unsucessful
IN <- st_transform(IN, crs(dem))
flood <- st_transform(flood, crs(dem))
# Cropping and masking the data to Indiana boundaries
IN_dem <- crop(dem,IN, snap='in')
IN_lcd <- crop(nlcd, extent(st_transform(IN,crs(nlcd))))
IN_lcd <- projectRaster(IN_lcd,crs=crs(dem), res = )
IN_lcd <- raster::mask(IN_lcd,IN)
#mapview(IN, add=T) + mapview(IN_lcd)
#mapview(IN_lcd)
#mapview(IN_dem)
#--------------------Terrain Analysis------------------
# Analysis of Elevation and Slope
slope <- terrain(dem, opt ='slope', unit='degrees')
aspect <- terrain(dem, opt='slope', unit='degrees')
hillshade_IN <- hillShade(slope, aspect)
# Calculate the avergae elevation and slope by county
IN$avg_el <- exact_extract(dem, IN, fun = 'mean')
IN$AvgSlope <- exact_extract(slope, IN, fun = 'mean')
# Calculate the Standard Deviation of elevation and slope by county
IN$elevStd <- exact_extract(dem, IN, function(value, cov_frac) sd(value*cov_frac))
IN$SlopeStd <- exact_extract(slope, IN, function(value, cov_frac) sd(value*cov_frac))

# Analysis of the flood zones
# Average elevation and Slope of the flood zones 
flood$avg_el <- exact_extract(dem, flood, fun = 'mean')
flood$AvgSlope <- exact_extract(slope, flood, fun = 'mean')
# Elevation and slope Standard Deviation of the flood zones elevation
flood$elevStd <- exact_extract(dem, flood, function(value, cov_frac) sd(value*cov_frac))
flood$SlopeStd <- exact_extract(slope, flood, function(value, cov_frac) sd(value*cov_frac))

# Analysis of the Land Cover Class
lcd_path <- "/NLCD_2016_Indiana_UTM16_masked.img"
IN_lcd2 <- raster(paste0(wd,lcd_path))

# Convert land cover classes to factor
IN_lcd2 <- ratify(IN_lcd2)

extent(IN_dem) <- extent(IN_lcd2)
extent(slope) <- extent(IN_lcd2)
# Resample to match values
IN_dem_resam <- raster::resample(IN_dem, IN_lcd2, method = "bilinear")
IN_lcd$avg_el <- zonal(IN_dem_resam, IN_lcd2, fun = 'mean', na.rm=T)
IN_slope_resam <- raster::resample(slope, IN_lcd2, method = "bilinear")
IN_lcd$AvgSlope <- zonal(IN_slope_resam, IN_lcd2, fun = 'mean')
IN_lcd$Slope_SD <- zonal(IN_slope_resam, IN_lcd2, fun = 'sd')
IN_lcd$Elev_SD <- zonal(IN_dem_resam, IN_lcd2, fun = 'sd')

res(IN_lcd)
# Select just Tippecanoe County for flood processing
IN_Tippe <- subset(IN, NAME_U == "TIPPECANOE")
flood_Tippe <- st_crop(flood, IN_Tippe)
tmap_mode("view")
plot(IN$avg_el,IN$AvgSlope)
# Map the average elevation across Indiana
tm_shape(IN) + tm_fill(col = "avg_el") + tm_polygons()
# Map the average slope across Indiana
tm_shape(IN) + tm_fill(col = "AvgSlope") + tm_polygons()

# Map the landcover across Indiana
tm_shape(IN) + tm_polygons() + tm_shape(IN_lcd) + tm_raster(alpha = 0.7)
# Map the average slope across Indiana
tm_shape(flood) + tm_fill(col = "AvgSlope") + tm_polygons()

#tm_shape(IN) + tm_polygons() + tm_shape(slope) + tm_raster(alpha = 0.7)

#------------------Evaluate Zone Data--------------------
#plot the land cover data over Indiana
#tm_shape(IN) + tm_polygons() + tm_shape(IN_lcd) + tm_raster(alpha = 0.7)
#plot the flood data over Indiana
tm_shape(IN) + tm_polygons() + tm_shape(flood) + tm_fill("FLD_ZONE")
tm_shape(flood) + tm_fill(col = "FLD_ZONE")
