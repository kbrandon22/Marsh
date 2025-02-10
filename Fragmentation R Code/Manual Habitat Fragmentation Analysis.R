library(landscapemetrics)
library(terra)
library(sf)
library(tidyr)
library(raster)
library(dplyr)

#Set working directory
setwd("C:\\Users\\Kristin\\Documents\\SMHM Project\\Data Analysis\\ArcGIS Pro")

#-------------------------------------------------------------------------------
#ALMONTE

#1. Import and manage files
#A. Read and view habitat raster
almonte_hab <- terra::rast("Almonte_Habitat.tif")
plot(almonte_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
almonte_shp <- read_sf("Almonte.shp")
plot(almonte_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(almonte_hab, almonte_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(almonte_hab)                                                                      #Check extent of the habitat raster
ext(almonte_shp)                                                                      #Check extent of the shapefile
almonte_shp <- st_transform(almonte_shp, crs(almonte_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
almonte_shp                                                                           #Check to make sure the transformation worked
almonte_habitat <- crop(almonte_hab, almonte_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
almonte_habitat <- mask(almonte_hab, almonte_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(almonte_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
almonte_perim <- lsm_p_perim(almonte_habitat)                                         #Calculate perimeter of all habitat patches
almonte_edge <- sum(almonte_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
almonte_patch <- lsm_c_np(almonte_habitat)                                            #Calculate number of habitat patches for each class
almonte_frag <- sum(almonte_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
almonte_cover <- lsm_c_pland(almonte_habitat)                                                       
almonte_cover <- as.data.frame(almonte_cover)
almonte_cover <- almonte_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Intertidal Channel",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Tidal Flat",
      class == 5 ~ "High Marsh",
      class == 6 ~ "Levee",
      class == 7 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
)

#D. Total class area (hectares)
almonte_area <- lsm_c_tca(almonte_habitat)
almonte_area <- as.data.frame(almonte_area)
almonte_area <- almonte_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Intertidal Channel",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Tidal Flat",
      class == 5 ~ "High Marsh",
      class == 6 ~ "Levee",
      class == 7 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
almonte_res <- as.data.frame(merge(almonte_cover, almonte_area, by = "Habitat Type"))
almonte_res <- almonte_res %>%
  mutate(
    Edge = almonte_edge,
    Number_Patches = almonte_frag,
    Marsh = "Almonte"
  )
almonte_res


#-------------------------------------------------------------------------------
#ALTO

#1. Import and manage files
#A. Read and view habitat raster
alto_hab <- terra::rast("Alto_Habitat.tif")
plot(alto_hab, legend = "topleft", plg = list(size=0.8, cex=0.8))

#B. Remove raster background 
#1) Read and view wetland shapefile
alto_shp <- read_sf("Alto.shp")
plot(alto_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(alto_hab, alto_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(alto_hab)                                                                      #Check extent of the habitat raster
ext(alto_shp)                                                                      #Check extent of the shapefile
alto_shp <- st_transform(alto_shp, crs(alto_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
alto_shp                                                                           #Check to make sure the transformation worked
alto_habitat <- crop(alto_hab, alto_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
alto_habitat <- mask(alto_hab, alto_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(alto_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
alto_perim <- lsm_p_perim(alto_habitat)                                         #Calculate perimeter of all habitat patches
alto_edge <- sum(alto_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
alto_patch <- lsm_c_np(alto_habitat)                                            #Calculate number of habitat patches for each class
alto_frag <- sum(alto_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
alto_cover <- lsm_c_pland(alto_habitat)                                                       
alto_cover <- as.data.frame(alto_cover)
alto_cover <- alto_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Levee",
      class == 3 ~ "Low Marsh",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Tidal Flat",
      class == 6 ~ "Undetermined Other Marsh",
      class == 7 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
alto_area <- lsm_c_tca(alto_habitat)
alto_area <- as.data.frame(alto_area)
alto_area <- alto_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Levee",
      class == 3 ~ "Low Marsh",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Tidal Flat",
      class == 6 ~ "Undetermined Other Marsh",
      class == 7 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
alto_res <- as.data.frame(merge(alto_cover, alto_area, by = "Habitat Type"))
alto_res <- alto_res %>%
  mutate(
    Edge = alto_edge,
    Number_Patches = alto_frag,
    Marsh = "Alto"
  )
alto_res


#-------------------------------------------------------------------------------
#ALVISO TRIANGLE

#1. Import and manage files
#A. Read and view habitat raster
alviso_triangle_hab <- terra::rast("AlvisoTriangle_Habitat.tif")
plot(alviso_triangle_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
alviso_triangle_shp <- read_sf("AlvisoTriangle.shp")
plot(alviso_triangle_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(alviso_triangle_hab, alviso_triangle_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(alviso_triangle_hab)                                                                      #Check extent of the habitat raster
ext(alviso_triangle_shp)                                                                      #Check extent of the shapefile
alviso_triangle_shp <- st_transform(alviso_triangle_shp, crs(alviso_triangle_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
alviso_triangle_shp                                                                           #Check to make sure the transformation worked
alviso_triangle_habitat <- crop(alviso_triangle_hab, alviso_triangle_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
alviso_triangle_habitat <- mask(alviso_triangle_hab, alviso_triangle_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(alviso_triangle_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
alviso_triangle_perim <- lsm_p_perim(alviso_triangle_habitat)                                         #Calculate perimeter of all habitat patches
alviso_triangle_edge <- sum(alviso_triangle_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
alviso_triangle_patch <- lsm_c_np(alviso_triangle_habitat)                                            #Calculate number of habitat patches for each class
alviso_triangle_frag <- sum(alviso_triangle_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
alviso_triangle_cover <- lsm_c_pland(alviso_triangle_habitat)                                                       
alviso_triangle_cover <- as.data.frame(alviso_triangle_cover)
alviso_triangle_cover <- alviso_triangle_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Intertidal Channel",
      class == 3 ~ "Tidal Flat",
      class == 4 ~ "High Marsh",
      class == 5 ~ "Tidal Pond/Panne",
      class == 6 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
alviso_triangle_area <- lsm_c_tca(alviso_triangle_habitat)
alviso_triangle_area <- as.data.frame(alviso_triangle_area)
alviso_triangle_area <- alviso_triangle_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Intertidal Channel",
      class == 3 ~ "Tidal Flat",
      class == 4 ~ "High Marsh",
      class == 5 ~ "Tidal Pond/Panne",
      class == 6 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
alviso_triangle_res <- as.data.frame(merge(alviso_triangle_cover, alviso_triangle_area, by = "Habitat Type"))
alviso_triangle_res <- alviso_triangle_res %>%
  mutate(
    Edge = alviso_triangle_edge,
    Number_Patches = alviso_triangle_frag,
    Marsh = "Alviso Triangle"
  )
alviso_triangle_res

#-------------------------------------------------------------------------------
#AMERICAN CANYON

#1. Import and manage files
#A. Read and view habitat raster
amer_canyon_hab <- terra::rast("AmericanCanyon_Habitat.tif")
plot(amer_canyon_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
amer_canyon_shp <- read_sf("AmericanCanyon.shp")
plot(amer_canyon_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(amer_canyon_hab, amer_canyon_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(amer_canyon_hab)                                                                      #Check extent of the habitat raster
ext(amer_canyon_shp)                                                                      #Check extent of the shapefile
amer_canyon_shp <- st_transform(amer_canyon_shp, crs(amer_canyon_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
amer_canyon_shp                                                                           #Check to make sure the transformation worked
amer_canyon_habitat <- crop(amer_canyon_hab, amer_canyon_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
amer_canyon_habitat <- mask(amer_canyon_hab, amer_canyon_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(amer_canyon_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
amer_canyon_perim <- lsm_p_perim(amer_canyon_habitat)                                         #Calculate perimeter of all habitat patches
amer_canyon_edge <- sum(amer_canyon_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
amer_canyon_patch <- lsm_c_np(amer_canyon_habitat)                                            #Calculate number of habitat patches for each class
amer_canyon_frag <- sum(amer_canyon_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
amer_canyon_cover <- lsm_c_pland(amer_canyon_habitat)                                                       
amer_canyon_cover <- as.data.frame(amer_canyon_cover)
amer_canyon_cover <- amer_canyon_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Tidal Pond/Panne",
      class == 2 ~ "Levee",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
amer_canyon_area <- lsm_c_tca(amer_canyon_habitat)
amer_canyon_area <- as.data.frame(amer_canyon_area)
amer_canyon_area <- amer_canyon_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Tidal Pond/Panne",
      class == 2 ~ "Levee",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
amer_canyon_res <- as.data.frame(merge(amer_canyon_cover, amer_canyon_area, by = "Habitat Type"))
amer_canyon_res <- amer_canyon_res %>%
  mutate(
    Edge = amer_canyon_edge,
    Number_Patches = amer_canyon_frag,
    Marsh = "American Canyon"
  )
amer_canyon_res

#-------------------------------------------------------------------------------
#ARROWHEAD

#1. Import and manage files
#A. Read and view habitat raster
arrowhead_hab <- terra::rast("Arrowhead_Habitat.tif")
plot(arrowhead_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
arrowhead_shp <- read_sf("Arrowhead.shp")
plot(arrowhead_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(arrowhead_hab, arrowhead_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(arrowhead_hab)                                                                      #Check extent of the habitat raster
ext(arrowhead_shp)                                                                      #Check extent of the shapefile
arrowhead_shp <- st_transform(arrowhead_shp, crs(arrowhead_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
arrowhead_shp                                                                           #Check to make sure the transformation worked
arrowhead_habitat <- crop(arrowhead_hab, arrowhead_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
arrowhead_habitat <- mask(arrowhead_hab, arrowhead_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(arrowhead_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
arrowhead_perim <- lsm_p_perim(arrowhead_habitat)                                         #Calculate perimeter of all habitat patches
arrowhead_edge <- sum(arrowhead_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
arrowhead_patch <- lsm_c_np(arrowhead_habitat)                                            #Calculate number of habitat patches for each class
arrowhead_frag <- sum(arrowhead_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
arrowhead_cover <- lsm_c_pland(arrowhead_habitat)                                                       
arrowhead_cover <- as.data.frame(arrowhead_cover)
arrowhead_cover <- arrowhead_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "Intertidal Channel",
      class == 4 ~ "Tidal Flat",
      class == 5 ~ "Tidal Pond/Panne",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
arrowhead_area <- lsm_c_tca(arrowhead_habitat)
arrowhead_area <- as.data.frame(arrowhead_area)
arrowhead_area <- arrowhead_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "Intertidal Channel",
      class == 4 ~ "Tidal Flat",
      class == 5 ~ "Tidal Pond/Panne",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
arrowhead_res <- as.data.frame(merge(arrowhead_cover, arrowhead_area, by = "Habitat Type"))
arrowhead_res <- arrowhead_res %>%
  mutate(
    Edge = arrowhead_edge,
    Number_Patches = arrowhead_frag,
    Marsh = "Arrowhead"
  )
arrowhead_res

#-------------------------------------------------------------------------------
#BAYFRONT

#1. Import and manage files
#A. Read and view habitat raster
bayfront_hab <- terra::rast("Bayfront_Habitat.tif")
plot(bayfront_hab, legend = "topright", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
bayfront_shp <- read_sf("Bayfront.shp")
plot(bayfront_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(bayfront_hab, bayfront_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(bayfront_hab)                                                                      #Check extent of the habitat raster
ext(bayfront_shp)                                                                      #Check extent of the shapefile
bayfront_shp <- st_transform(bayfront_shp, crs(bayfront_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
bayfront_shp                                                                           #Check to make sure the transformation worked
bayfront_habitat <- crop(bayfront_hab, bayfront_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
bayfront_habitat <- mask(bayfront_hab, bayfront_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(bayfront_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
bayfront_perim <- lsm_p_perim(bayfront_habitat)                                         #Calculate perimeter of all habitat patches
bayfront_edge <- sum(bayfront_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
bayfront_patch <- lsm_c_np(bayfront_habitat)                                            #Calculate number of habitat patches for each class
bayfront_frag <- sum(bayfront_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
bayfront_cover <- lsm_c_pland(bayfront_habitat)                                                       
bayfront_cover <- as.data.frame(bayfront_cover)
bayfront_cover <- bayfront_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Developed",
      class == 5 ~ "Levee",
      class == 6 ~ "Intertidal Channel",
      class == 7 ~ "Other Open Water",
      class == 8 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 9 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
bayfront_area <- lsm_c_tca(bayfront_habitat)
bayfront_area <- as.data.frame(bayfront_area)
bayfront_area <- bayfront_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Developed",
      class == 5 ~ "Levee",
      class == 6 ~ "Intertidal Channel",
      class == 7 ~ "Other Open Water",
      class == 8 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 9 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
bayfront_res <- as.data.frame(merge(bayfront_cover, bayfront_area, by = "Habitat Type"))
bayfront_res <- bayfront_res %>%
  mutate(
    Edge = bayfront_edge,
    Number_Patches = bayfront_frag,
    Marsh = "Bayfront"
  )
bayfront_res

#-------------------------------------------------------------------------------
#BAY POINT

#1. Import and manage files
#A. Read and view habitat raster
bay_point_hab <- terra::rast("BayPoint_Habitat.tif")
plot(bay_point_hab, legend = "bottomleft", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
bay_point_shp <- read_sf("BayPoint.shp")
plot(bay_point_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(bay_point_hab, bay_point_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(bay_point_hab)                                                                      #Check extent of the habitat raster
ext(bay_point_shp)                                                                      #Check extent of the shapefile
bay_point_shp <- st_transform(bay_point_shp, crs(bay_point_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
bay_point_shp                                                                           #Check to make sure the transformation worked
bay_point_habitat <- crop(bay_point_hab, bay_point_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
bay_point_habitat <- mask(bay_point_hab, bay_point_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(bay_point_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
bay_point_perim <- lsm_p_perim(bay_point_habitat)                                         #Calculate perimeter of all habitat patches
bay_point_edge <- sum(bay_point_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
bay_point_patch <- lsm_c_np(bay_point_habitat)                                            #Calculate number of habitat patches for each class
bay_point_frag <- sum(bay_point_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
bay_point_cover <- lsm_c_pland(bay_point_habitat)                                                       
bay_point_cover <- as.data.frame(bay_point_cover)
bay_point_cover <- bay_point_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Levee",
      class == 6 ~ "Tidal Pond/Panne",
      class == 7 ~ "Developed",
      class == 8 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
bay_point_area <- lsm_c_tca(bay_point_habitat)
bay_point_area <- as.data.frame(bay_point_area)
bay_point_area <- bay_point_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Levee",
      class == 6 ~ "Tidal Pond/Panne",
      class == 7 ~ "Developed",
      class == 8 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
bay_point_res <- as.data.frame(merge(bay_point_cover, bay_point_area, by = "Habitat Type"))
bay_point_res <- bay_point_res %>%
  mutate(
    Edge = bay_point_edge,
    Number_Patches = bay_point_frag,
    Marsh = "Bay Point"
  )
bay_point_res

#-------------------------------------------------------------------------------
#BELMONT SLOUGH

#1. Import and manage files
#A. Read and view habitat raster
belmont_hab <- terra::rast("BelmontSlough_Habitat.tif")
plot(belmont_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
belmont_shp <- read_sf("BelmontSlough.shp")
plot(belmont_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(belmont_hab, belmont_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(belmont_hab)                                                                      #Check extent of the habitat raster
ext(belmont_shp)                                                                      #Check extent of the shapefile
belmont_shp <- st_transform(belmont_shp, crs(belmont_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
belmont_shp                                                                           #Check to make sure the transformation worked
belmont_habitat <- crop(belmont_hab, belmont_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
belmont_habitat <- mask(belmont_hab, belmont_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(belmont_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
belmont_perim <- lsm_p_perim(belmont_habitat)                                         #Calculate perimeter of all habitat patches
belmont_edge <- sum(belmont_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
belmont_patch <- lsm_c_np(belmont_habitat)                                            #Calculate number of habitat patches for each class
belmont_frag <- sum(belmont_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
belmont_cover <- lsm_c_pland(belmont_habitat)                                                       
belmont_cover <- as.data.frame(belmont_cover)
belmont_cover <- belmont_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Tidal Pond/Panne",
      class == 6 ~ "Shallow Subtidal",
      class == 7 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
belmont_area <- lsm_c_tca(belmont_habitat)
belmont_area <- as.data.frame(belmont_area)
belmont_area <- belmont_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Tidal Pond/Panne",
      class == 6 ~ "Shallow Subtidal",
      class == 7 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
belmont_res <- as.data.frame(merge(belmont_cover, belmont_area, by = "Habitat Type"))
belmont_res <- belmont_res %>%
  mutate(
    Edge = belmont_edge,
    Number_Patches = belmont_frag,
    Marsh = "Belmont Slough"
  )
belmont_res

#-------------------------------------------------------------------------------
#BENICIA INDUSTRIAL

#1. Import and manage files
#A. Read and view habitat raster
benicia_ind_hab <- terra::rast("BeniciaIndustrial_Habitat.tif")
plot(benicia_ind_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
benicia_ind_shp <- read_sf("BeniciaIndustrial.shp")
plot(benicia_ind_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(benicia_ind_hab, benicia_ind_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(benicia_ind_hab)                                                                      #Check extent of the habitat raster
ext(benicia_ind_shp)                                                                      #Check extent of the shapefile
benicia_ind_shp <- st_transform(benicia_ind_shp, crs(benicia_ind_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
benicia_ind_shp                                                                           #Check to make sure the transformation worked
benicia_ind_habitat <- crop(benicia_ind_hab, benicia_ind_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
benicia_ind_habitat <- mask(benicia_ind_hab, benicia_ind_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(benicia_ind_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
benicia_ind_perim <- lsm_p_perim(benicia_ind_habitat)                                         #Calculate perimeter of all habitat patches
benicia_ind_edge <- sum(benicia_ind_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
benicia_ind_patch <- lsm_c_np(benicia_ind_habitat)                                            #Calculate number of habitat patches for each class
benicia_ind_frag <- sum(benicia_ind_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
benicia_ind_cover <- lsm_c_pland(benicia_ind_habitat)                                                       
benicia_ind_cover <- as.data.frame(benicia_ind_cover)
benicia_ind_cover <- benicia_ind_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Developed",
      class == 2 ~ "Other Open Water",
      class == 3 ~ "Levee",
      class == 4 ~ "Muted Tidal Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
benicia_ind_area <- lsm_c_tca(benicia_ind_habitat)
benicia_ind_area <- as.data.frame(benicia_ind_area)
benicia_ind_area <- benicia_ind_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Developed",
      class == 2 ~ "Other Open Water",
      class == 3 ~ "Levee",
      class == 4 ~ "Muted Tidal Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
benicia_ind_res <- as.data.frame(merge(benicia_ind_cover, benicia_ind_area, by = "Habitat Type"))
benicia_ind_res <- benicia_ind_res %>%
  mutate(
    Edge = benicia_ind_edge,
    Number_Patches = benicia_ind_frag,
    Marsh = "Benicia Industrial"
  )
benicia_ind_res

#-------------------------------------------------------------------------------
#BOTHIN MARSH

#1. Import and manage files
#A. Read and view habitat raster
bothin_hab <- terra::rast("BothinMarsh_Habitat.tif")
plot(bothin_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
bothin_shp <- read_sf("BothinMarsh.shp")
plot(bothin_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(bothin_hab, bothin_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(bothin_hab)                                                                      #Check extent of the habitat raster
ext(bothin_shp)                                                                      #Check extent of the shapefile
bothin_shp <- st_transform(bothin_shp, crs(bothin_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
bothin_shp                                                                           #Check to make sure the transformation worked
bothin_habitat <- crop(bothin_hab, bothin_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
bothin_habitat <- mask(bothin_hab, bothin_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(bothin_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
bothin_perim <- lsm_p_perim(bothin_habitat)                                         #Calculate perimeter of all habitat patches
bothin_edge <- sum(bothin_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
bothin_patch <- lsm_c_np(bothin_habitat)                                            #Calculate number of habitat patches for each class
bothin_frag <- sum(bothin_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
bothin_cover <- lsm_c_pland(bothin_habitat)                                                       
bothin_cover <- as.data.frame(bothin_cover)
bothin_cover <- bothin_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Tidal Pond/Panne",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Flat",
      class == 4 ~ "Levee",
      class == 5 ~ "Low Marsh",
      class == 6 ~ "Intertidal Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
bothin_area <- lsm_c_tca(bothin_habitat)
bothin_area <- as.data.frame(bothin_area)
bothin_area <- bothin_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Tidal Pond/Panne",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Flat",
      class == 4 ~ "Levee",
      class == 5 ~ "Low Marsh",
      class == 6 ~ "Intertidal Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
bothin_res <- as.data.frame(merge(bothin_cover, bothin_area, by = "Habitat Type"))
bothin_res <- bothin_res %>%
  mutate(
    Edge = bothin_edge,
    Number_Patches = bothin_frag,
    Marsh = "Bothin Marsh"
  )
bothin_res

#-------------------------------------------------------------------------------
# BURDELL UNIT

#1. Import and manage files
#A. Read and view habitat raster
burdell_hab <- terra::rast("BurdellUnit_Habitat.tif")
plot(burdell_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
burdell_shp <- read_sf("BurdellUnit.shp")
plot(burdell_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(burdell_hab, burdell_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(burdell_hab)                                                                      #Check extent of the habitat raster
ext(burdell_shp)                                                                      #Check extent of the shapefile
burdell_shp <- st_transform(burdell_shp, crs(burdell_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
burdell_shp                                                                           #Check to make sure the transformation worked
burdell_habitat <- crop(burdell_hab, burdell_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
burdell_habitat <- mask(burdell_hab, burdell_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(burdell_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
burdell_perim <- lsm_p_perim(burdell_habitat)                                         #Calculate perimeter of all habitat patches
burdell_edge <- sum(burdell_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
burdell_patch <- lsm_c_np(burdell_habitat)                                            #Calculate number of habitat patches for each class
burdell_frag <- sum(burdell_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
burdell_cover <- lsm_c_pland(burdell_habitat)                                                       
burdell_cover <- as.data.frame(burdell_cover)
burdell_cover <- burdell_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Other Open Water",
      class == 2 ~ "Levee",
      class == 3 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
burdell_area <- lsm_c_tca(burdell_habitat)
burdell_area <- as.data.frame(burdell_area)
burdell_area <- burdell_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Other Open Water",
      class == 2 ~ "Levee",
      class == 3 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
burdell_res <- as.data.frame(merge(burdell_cover, burdell_area, by = "Habitat Type"))
burdell_res <- burdell_res %>%
  mutate(
    Edge = burdell_edge,
    Number_Patches = burdell_frag,
    Marsh = "Burdell Unit"
  )
burdell_res

#-------------------------------------------------------------------------------
#BYXBEE HARBOR

#1. Import and manage files
#A. Read and view habitat raster
byxbee_hab <- terra::rast("ByxbeeHarbor_Habitat.tif")
plot(byxbee_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
byxbee_shp <- read_sf("ByxbeeHarbor.shp")
plot(byxbee_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(byxbee_hab, byxbee_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(byxbee_hab)                                                                      #Check extent of the habitat raster
ext(byxbee_shp)                                                                      #Check extent of the shapefile
byxbee_shp <- st_transform(byxbee_shp, crs(byxbee_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
byxbee_shp                                                                           #Check to make sure the transformation worked
byxbee_habitat <- crop(byxbee_hab, byxbee_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
byxbee_habitat <- mask(byxbee_hab, byxbee_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(byxbee_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
byxbee_perim <- lsm_p_perim(byxbee_habitat)                                         #Calculate perimeter of all habitat patches
byxbee_edge <- sum(byxbee_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
byxbee_patch <- lsm_c_np(byxbee_habitat)                                            #Calculate number of habitat patches for each class
byxbee_frag <- sum(byxbee_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
byxbee_cover <- lsm_c_pland(byxbee_habitat)                                                       
byxbee_cover <- as.data.frame(byxbee_cover)
byxbee_cover <- byxbee_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Intertidal Channel",
      class == 3 ~ "Levee",
      class == 4 ~ "High Marsh",
      class == 5 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
byxbee_area <- lsm_c_tca(byxbee_habitat)
byxbee_area <- as.data.frame(byxbee_area)
byxbee_area <- byxbee_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Intertidal Channel",
      class == 3 ~ "Levee",
      class == 4 ~ "High Marsh",
      class == 5 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
byxbee_res <- as.data.frame(merge(byxbee_cover, byxbee_area, by = "Habitat Type"))
byxbee_res <- byxbee_res %>%
  mutate(
    Edge = byxbee_edge,
    Number_Patches = byxbee_frag,
    Marsh = "Byxbee Harbor"
  )
byxbee_res

#-------------------------------------------------------------------------------
#CHINA CAMP

#1. Import and manage files
#A. Read and view habitat raster
china_camp_hab <- terra::rast("ChinaCamp_Habitat.tif")
plot(china_camp_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
china_camp_shp <- read_sf("ChinaCamp.shp")
plot(china_camp_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(china_camp_hab, china_camp_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(china_camp_hab)                                                                      #Check extent of the habitat raster
ext(china_camp_shp)                                                                      #Check extent of the shapefile
china_camp_shp <- st_transform(china_camp_shp, crs(china_camp_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
china_camp_shp                                                                           #Check to make sure the transformation worked
china_camp_habitat <- crop(china_camp_hab, china_camp_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
china_camp_habitat <- mask(china_camp_hab, china_camp_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(china_camp_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
china_camp_perim <- lsm_p_perim(china_camp_habitat)                                         #Calculate perimeter of all habitat patches
china_camp_edge <- sum(china_camp_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
china_camp_patch <- lsm_c_np(china_camp_habitat)                                            #Calculate number of habitat patches for each class
china_camp_frag <- sum(china_camp_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
china_camp_cover <- lsm_c_pland(china_camp_habitat)                                                       
china_camp_cover <- as.data.frame(china_camp_cover)
china_camp_cover <- china_camp_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
china_camp_area <- lsm_c_tca(china_camp_habitat)
china_camp_area <- as.data.frame(china_camp_area)
china_camp_area <- china_camp_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
china_camp_res <- as.data.frame(merge(china_camp_cover, china_camp_area, by = "Habitat Type"))
china_camp_res <- china_camp_res %>%
  mutate(
    Edge = china_camp_edge,
    Number_Patches = china_camp_frag,
    Marsh = "China Camp"
  )
china_camp_res

#-------------------------------------------------------------------------------
#CORTE MADERA

#1. Import and manage files
#A. Read and view habitat raster
corte_madera_hab <- terra::rast("CorteMaderaTriangle_Habitat.tif")
plot(corte_madera_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
corte_madera_shp <- read_sf("CorteMaderaTriangle.shp")
plot(corte_madera_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(corte_madera_hab, corte_madera_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(corte_madera_hab)                                                                      #Check extent of the habitat raster
ext(corte_madera_shp)                                                                      #Check extent of the shapefile
corte_madera_shp <- st_transform(corte_madera_shp, crs(corte_madera_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
corte_madera_shp                                                                           #Check to make sure the transformation worked
corte_madera_habitat <- crop(corte_madera_hab, corte_madera_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
corte_madera_habitat <- mask(corte_madera_hab, corte_madera_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(corte_madera_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
corte_madera_perim <- lsm_p_perim(corte_madera_habitat)                                         #Calculate perimeter of all habitat patches
corte_madera_edge <- sum(corte_madera_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
corte_madera_patch <- lsm_c_np(corte_madera_habitat)                                            #Calculate number of habitat patches for each class
corte_madera_frag <- sum(corte_madera_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
corte_madera_cover <- lsm_c_pland(corte_madera_habitat)                                                       
corte_madera_cover <- as.data.frame(corte_madera_cover)
corte_madera_cover <- corte_madera_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Intertidal Channel", 
      class == 5 ~ "Levee",
      class == 6 ~ "Developed", 
      class == 7 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
corte_madera_area <- lsm_c_tca(corte_madera_habitat)
corte_madera_area <- as.data.frame(corte_madera_area)
corte_madera_area <- corte_madera_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Intertidal Channel", 
      class == 5 ~ "Levee",
      class == 6 ~ "Developed", 
      class == 7 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
corte_madera_res <- as.data.frame(merge(corte_madera_cover, corte_madera_area, by = "Habitat Type"))
corte_madera_res <- corte_madera_res %>%
  mutate(
    Edge = corte_madera_edge,
    Number_Patches = corte_madera_frag,
    Marsh = "Corte Madera"
  )
corte_madera_res

#-------------------------------------------------------------------------------
#CREEKSIDE

#1. Import and manage files
#A. Read and view habitat raster
creekside_hab <- terra::rast("Creekside_Habitat.tif")
plot(creekside_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
creekside_shp <- read_sf("Creekside.shp")
plot(creekside_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(creekside_hab, creekside_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(creekside_hab)                                                                      #Check extent of the habitat raster
ext(creekside_shp)                                                                      #Check extent of the shapefile
creekside_shp <- st_transform(creekside_shp, crs(creekside_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
creekside_shp                                                                           #Check to make sure the transformation worked
creekside_habitat <- crop(creekside_hab, creekside_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
creekside_habitat <- mask(creekside_hab, creekside_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(creekside_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
creekside_perim <- lsm_p_perim(creekside_habitat)                                         #Calculate perimeter of all habitat patches
creekside_edge <- sum(creekside_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
creekside_patch <- lsm_c_np(creekside_habitat)                                            #Calculate number of habitat patches for each class
creekside_frag <- sum(creekside_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
creekside_cover <- lsm_c_pland(creekside_habitat)                                                       
creekside_cover <- as.data.frame(creekside_cover)
creekside_cover <- creekside_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Open Other Water",
      class == 2 ~ "Muted Tidal Marsh",
      class == 3 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
creekside_area <- lsm_c_tca(creekside_habitat)
creekside_area <- as.data.frame(creekside_area)
creekside_area <- creekside_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Open Other Water",
      class == 2 ~ "Muted Tidal Marsh",
      class == 3 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
creekside_res <- as.data.frame(merge(creekside_cover, creekside_area, by = "Habitat Type"))
creekside_res <- creekside_res %>%
  mutate(
    Edge = creekside_edge,
    Number_Patches = creekside_frag,
    Marsh = "Creekside"
  )
creekside_res

#-------------------------------------------------------------------------------
#CULLINAN RANCH

#1. Import and manage files
#A. Read and view habitat raster
cullinan_hab <- terra::rast("CullinanRanch_Habitat.tif")
plot(cullinan_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
cullinan_shp <- read_sf("CullinanRanch.shp")
plot(cullinan_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(cullinan_hab, cullinan_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(cullinan_hab)                                                                      #Check extent of the habitat raster
ext(cullinan_shp)                                                                      #Check extent of the shapefile
cullinan_shp <- st_transform(cullinan_shp, crs(cullinan_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
cullinan_shp                                                                           #Check to make sure the transformation worked
cullinan_habitat <- crop(cullinan_hab, cullinan_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
cullinan_habitat <- mask(cullinan_hab, cullinan_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(cullinan_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
cullinan_perim <- lsm_p_perim(cullinan_habitat)                                         #Calculate perimeter of all habitat patches
cullinan_edge <- sum(cullinan_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
cullinan_patch <- lsm_c_np(cullinan_habitat)                                            #Calculate number of habitat patches for each class
cullinan_frag <- sum(cullinan_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
cullinan_cover <- lsm_c_pland(cullinan_habitat)                                                       
cullinan_cover <- as.data.frame(cullinan_cover)
cullinan_cover <- cullinan_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "Tidal Flat",
      class == 4 ~ "Levee",
      class == 5 ~ "Other Open Water",
      class == 6 ~ "Managed Marsh",
      class == 7 ~ "Undetermined Other Marsh",
      class == 8 ~ "Shallow Subtidal",
      class == 9 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
cullinan_area <- lsm_c_tca(cullinan_habitat)
cullinan_area <- as.data.frame(cullinan_area)
cullinan_area <- cullinan_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "Tidal Flat",
      class == 4 ~ "Levee",
      class == 5 ~ "Other Open Water",
      class == 6 ~ "Managed Marsh",
      class == 7 ~ "Undetermined Other Marsh",
      class == 8 ~ "Shallow Subtidal",
      class == 9 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
cullinan_res <- as.data.frame(merge(cullinan_cover, cullinan_area, by = "Habitat Type"))
cullinan_res <- cullinan_res %>%
  mutate(
    Edge = cullinan_edge,
    Number_Patches = cullinan_frag,
    Marsh = "Cullinan Ranch"
  )
cullinan_res

#-------------------------------------------------------------------------------
#DIXON LANDING

#1. Import and manage files
#A. Read and view habitat raster
dixon_hab <- terra::rast("DixonLanding_Habitat.tif")
plot(dixon_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
dixon_shp <- read_sf("DixonLanding.shp")
plot(dixon_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(dixon_hab, dixon_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(dixon_hab)                                                                      #Check extent of the habitat raster
ext(dixon_shp)                                                                      #Check extent of the shapefile
dixon_shp <- st_transform(dixon_shp, crs(dixon_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
dixon_shp                                                                           #Check to make sure the transformation worked
dixon_habitat <- crop(dixon_hab, dixon_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
dixon_habitat <- mask(dixon_hab, dixon_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(dixon_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
dixon_perim <- lsm_p_perim(dixon_habitat)                                         #Calculate perimeter of all habitat patches
dixon_edge <- sum(dixon_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
dixon_patch <- lsm_c_np(dixon_habitat)                                            #Calculate number of habitat patches for each class
dixon_frag <- sum(dixon_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
dixon_cover <- lsm_c_pland(dixon_habitat)                                                       
dixon_cover <- as.data.frame(dixon_cover)
dixon_cover <- dixon_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 3 ~ "Levee",
      class == 4 ~ "Managed Marsh",
      class == 5 ~ "High Marsh",
      class == 6 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
dixon_area <- lsm_c_tca(dixon_habitat)
dixon_area <- as.data.frame(dixon_area)
dixon_area <- dixon_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 3 ~ "Levee",
      class == 4 ~ "Managed Marsh",
      class == 5 ~ "High Marsh",
      class == 6 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
dixon_res <- as.data.frame(merge(dixon_cover, dixon_area, by = "Habitat Type"))
dixon_res <- dixon_res %>%
  mutate(
    Edge = dixon_edge,
    Number_Patches = dixon_frag,
    Marsh = "Dixon Landing"
  )
dixon_res

#-------------------------------------------------------------------------------
#DOTSON NORTH

#1. Import and manage files
#A. Read and view habitat raster
dotson_hab <- terra::rast("DotsonNorth_Habitat.tif")
plot(dotson_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
dotson_shp <- read_sf("DotsonNorth.shp")
plot(dotson_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(dotson_hab, dotson_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(dotson_hab)                                                                      #Check extent of the habitat raster
ext(dotson_shp)                                                                      #Check extent of the shapefile
dotson_shp <- st_transform(dotson_shp, crs(dotson_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
dotson_shp                                                                           #Check to make sure the transformation worked
dotson_habitat <- crop(dotson_hab, dotson_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
dotson_habitat <- mask(dotson_hab, dotson_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(dotson_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
dotson_perim <- lsm_p_perim(dotson_habitat)                                         #Calculate perimeter of all habitat patches
dotson_edge <- sum(dotson_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
dotson_patch <- lsm_c_np(dotson_habitat)                                            #Calculate number of habitat patches for each class
dotson_frag <- sum(dotson_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
dotson_cover <- lsm_c_pland(dotson_habitat)                                                       
dotson_cover <- as.data.frame(dotson_cover)
dotson_cover <- dotson_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Levee",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Flat",
      class == 4 ~ "Low Marsh",
      class == 5 ~ "Intertidal Channel",
      class == 6 ~ "Tidal Pond/Panne",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
dotson_area <- lsm_c_tca(dotson_habitat)
dotson_area <- as.data.frame(dotson_area)
dotson_area <- dotson_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Levee",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Flat",
      class == 4 ~ "Low Marsh",
      class == 5 ~ "Intertidal Channel",
      class == 6 ~ "Tidal Pond/Panne",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
dotson_res <- as.data.frame(merge(dotson_cover, dotson_area, by = "Habitat Type"))
dotson_res <- dotson_res %>%
  mutate(
    Edge = dotson_edge,
    Number_Patches = dotson_frag,
    Marsh = "Dotson North"
  )
dotson_res

#-------------------------------------------------------------------------------
#ENTERPRISE

#1. Import and manage files
#A. Read and view habitat raster
enterprise_hab <- terra::rast("Enterprise_Habitat.tif")
plot(enterprise_hab, legend = "bottomright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
enterprise_shp <- read_sf("Enterprise.shp")
plot(enterprise_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(enterprise_hab, enterprise_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(enterprise_hab)                                                                      #Check extent of the habitat raster
ext(enterprise_shp)                                                                      #Check extent of the shapefile
enterprise_shp <- st_transform(enterprise_shp, crs(enterprise_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
enterprise_shp                                                                           #Check to make sure the transformation worked
enterprise_habitat <- crop(enterprise_hab, enterprise_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
enterprise_habitat <- mask(enterprise_hab, enterprise_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(enterprise_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
enterprise_perim <- lsm_p_perim(enterprise_habitat)                                         #Calculate perimeter of all habitat patches
enterprise_edge <- sum(enterprise_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
enterprise_patch <- lsm_c_np(enterprise_habitat)                                            #Calculate number of habitat patches for each class
enterprise_frag <- sum(enterprise_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
enterprise_cover <- lsm_c_pland(enterprise_habitat)                                                       
enterprise_cover <- as.data.frame(enterprise_cover)
enterprise_cover <- enterprise_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 2 ~ "Developed",
      class == 3 ~ "Levee",
      class == 4 ~ "Managed Marsh",
      class == 5 ~ "Undetermined Other Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
enterprise_area <- lsm_c_tca(enterprise_habitat)
enterprise_area <- as.data.frame(enterprise_area)
enterprise_area <- enterprise_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 2 ~ "Developed",
      class == 3 ~ "Levee",
      class == 4 ~ "Managed Marsh",
      class == 5 ~ "Undetermined Other Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
enterprise_res <- as.data.frame(merge(enterprise_cover, enterprise_area, by = "Habitat Type"))
enterprise_res <- enterprise_res %>%
  mutate(
    Edge = enterprise_edge,
    Number_Patches = enterprise_frag,
    Marsh = "Enterprise"
  )
enterprise_res

#-------------------------------------------------------------------------------
#FLOOD CONTROL BASIN

#1. Import and manage files
#A. Read and view habitat raster
flood_control_hab <- terra::rast("FloodControlBasin_Habitat.tif")
plot(flood_control_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
flood_control_shp <- read_sf("FloodControlBasin.shp")
plot(flood_control_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(flood_control_hab, flood_control_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(flood_control_hab)                                                                      #Check extent of the habitat raster
ext(flood_control_shp)                                                                      #Check extent of the shapefile
flood_control_shp <- st_transform(flood_control_shp, crs(flood_control_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
flood_control_shp                                                                           #Check to make sure the transformation worked
flood_control_habitat <- crop(flood_control_hab, flood_control_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
flood_control_habitat <- mask(flood_control_hab, flood_control_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(flood_control_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
flood_control_perim <- lsm_p_perim(flood_control_habitat)                                         #Calculate perimeter of all habitat patches
flood_control_edge <- sum(flood_control_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
flood_control_patch <- lsm_c_np(flood_control_habitat)                                            #Calculate number of habitat patches for each class
flood_control_frag <- sum(flood_control_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
flood_control_cover <- lsm_c_pland(flood_control_habitat)                                                       
flood_control_cover <- as.data.frame(flood_control_cover)
flood_control_cover <- flood_control_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Other Open Water",
      class == 2 ~ "Levee",
      class == 3 ~ "Managed Marsh",
      class == 4 ~ "Developed",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
flood_control_area <- lsm_c_tca(flood_control_habitat)
flood_control_area <- as.data.frame(flood_control_area)
flood_control_area <- flood_control_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Other Open Water",
      class == 2 ~ "Levee",
      class == 3 ~ "Managed Marsh",
      class == 4 ~ "Developed",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
flood_control_res <- as.data.frame(merge(flood_control_cover, flood_control_area, by = "Habitat Type"))
flood_control_res <- flood_control_res %>%
  mutate(
    Edge = flood_control_edge,
    Number_Patches = flood_control_frag,
    Marsh = "Flood Control Basin"
  )
flood_control_res

#-------------------------------------------------------------------------------
#FREEMAN ISLAND

#1. Import and manage files
#A. Read and view habitat raster
freeman_hab <- terra::rast("FreemanIsland_Habitat.tif")
plot(freeman_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
freeman_shp <- read_sf("FreemanIsland.shp")
plot(freeman_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(freeman_hab, freeman_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(freeman_hab)                                                                      #Check extent of the habitat raster
ext(freeman_shp)                                                                      #Check extent of the shapefile
freeman_shp <- st_transform(freeman_shp, crs(freeman_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
freeman_shp                                                                           #Check to make sure the transformation worked
freeman_habitat <- crop(freeman_hab, freeman_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
freeman_habitat <- mask(freeman_hab, freeman_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(freeman_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
freeman_perim <- lsm_p_perim(freeman_habitat)                                         #Calculate perimeter of all habitat patches
freeman_edge <- sum(freeman_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
freeman_patch <- lsm_c_np(freeman_habitat)                                            #Calculate number of habitat patches for each class
freeman_frag <- sum(freeman_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
freeman_cover <- lsm_c_pland(freeman_habitat)                                                       
freeman_cover <- as.data.frame(freeman_cover)
freeman_cover <- freeman_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
freeman_area <- lsm_c_tca(freeman_habitat)
freeman_area <- as.data.frame(freeman_area)
freeman_area <- freeman_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
freeman_res <- as.data.frame(merge(freeman_cover, freeman_area, by = "Habitat Type"))
freeman_res <- freeman_res %>%
  mutate(
    Edge = freeman_edge,
    Number_Patches = freeman_frag,
    Marsh = "Freeman Island"
  )
freeman_res

#-------------------------------------------------------------------------------
#GIANT MARSH

#1. Import and manage files
#A. Read and view habitat raster
giant_hab <- terra::rast("GiantMarsh_Habitat.tif")
plot(giant_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
giant_shp <- read_sf("GiantMarsh.shp")
plot(giant_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(giant_hab, giant_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(giant_hab)                                                                      #Check extent of the habitat raster
ext(giant_shp)                                                                      #Check extent of the shapefile
giant_shp <- st_transform(giant_shp, crs(giant_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
giant_shp                                                                           #Check to make sure the transformation worked
giant_habitat <- crop(giant_hab, giant_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
giant_habitat <- mask(giant_hab, giant_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(giant_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
giant_perim <- lsm_p_perim(giant_habitat)                                         #Calculate perimeter of all habitat patches
giant_edge <- sum(giant_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
giant_patch <- lsm_c_np(giant_habitat)                                            #Calculate number of habitat patches for each class
giant_frag <- sum(giant_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
giant_cover <- lsm_c_pland(giant_habitat)                                                       
giant_cover <- as.data.frame(giant_cover)
giant_cover <- giant_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Beach",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Flat",
      class == 4 ~ "Low Marsh",
      class == 5 ~ "Tidal Pond/Panne",
      class == 6 ~ "Intertidal Channel",
      class == 7 ~ "Muted Tidal Marsh",
      class == 8 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
giant_area <- lsm_c_tca(giant_habitat)
giant_area <- as.data.frame(giant_area)
giant_area <- giant_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Beach",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Flat",
      class == 4 ~ "Low Marsh",
      class == 5 ~ "Tidal Pond/Panne",
      class == 6 ~ "Intertidal Channel",
      class == 7 ~ "Muted Tidal Marsh",
      class == 8 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
giant_res <- as.data.frame(merge(giant_cover, giant_area, by = "Habitat Type"))
giant_res <- giant_res %>%
  mutate(
    Edge = giant_edge,
    Number_Patches = giant_frag,
    Marsh = "Giant Marsh"
  )
giant_res

#-------------------------------------------------------------------------------
#GOODYEAR

#1. Import and manage files
#A. Read and view habitat raster
goodyear_hab <- terra::rast("Goodyear_Habitat.tif")
plot(goodyear_hab, legend = "topleft", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
goodyear_shp <- read_sf("Goodyear.shp")
plot(goodyear_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(goodyear_hab, goodyear_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(goodyear_hab)                                                                      #Check extent of the habitat raster
ext(goodyear_shp)                                                                      #Check extent of the shapefile
goodyear_shp <- st_transform(goodyear_shp, crs(goodyear_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
goodyear_shp                                                                           #Check to make sure the transformation worked
goodyear_habitat <- crop(goodyear_hab, goodyear_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
goodyear_habitat <- mask(goodyear_hab, goodyear_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(goodyear_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
goodyear_perim <- lsm_p_perim(goodyear_habitat)                                         #Calculate perimeter of all habitat patches
goodyear_edge <- sum(goodyear_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
goodyear_patch <- lsm_c_np(goodyear_habitat)                                            #Calculate number of habitat patches for each class
goodyear_frag <- sum(goodyear_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
goodyear_cover <- lsm_c_pland(goodyear_habitat)                                                       
goodyear_cover <- as.data.frame(goodyear_cover)
goodyear_cover <- goodyear_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "Low Marsh",
      class == 4 ~ "Levee",
      class == 5 ~ "Managed Marsh",
      class == 6 ~ "Intertidal Channel",
      class == 7 ~ "Tidal Pond/Panne",
      class == 8 ~ "Undetermined Non-Aquatic Diked Bayland",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
goodyear_area <- lsm_c_tca(goodyear_habitat)
goodyear_area <- as.data.frame(goodyear_area)
goodyear_area <- goodyear_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "Low Marsh",
      class == 4 ~ "Levee",
      class == 5 ~ "Managed Marsh",
      class == 6 ~ "Intertidal Channel",
      class == 7 ~ "Tidal Pond/Panne",
      class == 8 ~ "Undetermined Non-Aquatic Diked Bayland",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
goodyear_res <- as.data.frame(merge(goodyear_cover, goodyear_area, by = "Habitat Type"))
goodyear_res <- goodyear_res %>%
  mutate(
    Edge = goodyear_edge,
    Number_Patches = goodyear_frag,
    Marsh = "Goodyear"
  )
goodyear_res

#-------------------------------------------------------------------------------
#GUADALCANAL

#1. Import and manage files
#A. Read and view habitat raster
guadalcanal_hab <- terra::rast("Guadalcanal_Habitat.tif")
plot(guadalcanal_hab, legend = "topright", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
guadalcanal_shp <- read_sf("Guadalcanal.shp")
plot(guadalcanal_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(guadalcanal_hab, guadalcanal_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(guadalcanal_hab)                                                                      #Check extent of the habitat raster
ext(guadalcanal_shp)                                                                      #Check extent of the shapefile
guadalcanal_shp <- st_transform(guadalcanal_shp, crs(guadalcanal_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
guadalcanal_shp                                                                           #Check to make sure the transformation worked
guadalcanal_habitat <- crop(guadalcanal_hab, guadalcanal_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
guadalcanal_habitat <- mask(guadalcanal_hab, guadalcanal_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(guadalcanal_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
guadalcanal_perim <- lsm_p_perim(guadalcanal_habitat)                                         #Calculate perimeter of all habitat patches
guadalcanal_edge <- sum(guadalcanal_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
guadalcanal_patch <- lsm_c_np(guadalcanal_habitat)                                            #Calculate number of habitat patches for each class
guadalcanal_frag <- sum(guadalcanal_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
guadalcanal_cover <- lsm_c_pland(guadalcanal_habitat)                                                       
guadalcanal_cover <- as.data.frame(guadalcanal_cover)
guadalcanal_cover <- guadalcanal_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Tidal Pond/Panne",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Levee",
      class == 5 ~ "Intertidal Channel",
      class == 6 ~ "Tidal Flat",
      class == 7 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
guadalcanal_area <- lsm_c_tca(guadalcanal_habitat)
guadalcanal_area <- as.data.frame(guadalcanal_area)
guadalcanal_area <- guadalcanal_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Tidal Pond/Panne",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Levee",
      class == 5 ~ "Intertidal Channel",
      class == 6 ~ "Tidal Flat",
      class == 7 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
guadalcanal_res <- as.data.frame(merge(guadalcanal_cover, guadalcanal_area, by = "Habitat Type"))
guadalcanal_res <- guadalcanal_res %>%
  mutate(
    Edge = guadalcanal_edge,
    Number_Patches = guadalcanal_frag,
    Marsh = "Guadalcanal"
  )
guadalcanal_res

#-------------------------------------------------------------------------------
#HARD MARSH

#1. Import and manage files
#A. Read and view habitat raster
hard_hab <- terra::rast("HardMarsh_Habitat.tif")
plot(hard_hab, legend = "topleft", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
hard_shp <- read_sf("HardMarsh.shp")
plot(hard_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(hard_hab, hard_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(hard_hab)                                                                      #Check extent of the habitat raster
ext(hard_shp)                                                                      #Check extent of the shapefile
hard_shp <- st_transform(hard_shp, crs(hard_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
hard_shp                                                                           #Check to make sure the transformation worked
hard_habitat <- crop(hard_hab, hard_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
hard_habitat <- mask(hard_hab, hard_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(hard_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
hard_perim <- lsm_p_perim(hard_habitat)                                         #Calculate perimeter of all habitat patches
hard_edge <- sum(hard_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
hard_patch <- lsm_c_np(hard_habitat)                                            #Calculate number of habitat patches for each class
hard_frag <- sum(hard_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
hard_cover <- lsm_c_pland(hard_habitat)                                                       
hard_cover <- as.data.frame(hard_cover)
hard_cover <- hard_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Levee",
      class == 2 ~ "Other Open Water",
      class == 3 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
hard_area <- lsm_c_tca(hard_habitat)
hard_area <- as.data.frame(hard_area)
hard_area <- hard_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Levee",
      class == 2 ~ "Other Open Water",
      class == 3 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
hard_res <- as.data.frame(merge(hard_cover, hard_area, by = "Habitat Type"))
hard_res <- hard_res %>%
  mutate(
    Edge = hard_edge,
    Number_Patches = hard_frag,
    Marsh = "HARD Marsh"
  )
hard_res

#-------------------------------------------------------------------------------
#HAYSTACK

#1. Import and manage files
#A. Read and view habitat raster
haystack_hab <- terra::rast("Haystack_Habitat.tif")
plot(haystack_hab, legend = "topright", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
haystack_shp <- read_sf("Haystack.shp")
plot(haystack_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(haystack_hab, haystack_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(haystack_hab)                                                                      #Check extent of the habitat raster
ext(haystack_shp)                                                                      #Check extent of the shapefile
haystack_shp <- st_transform(haystack_shp, crs(haystack_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
haystack_shp                                                                           #Check to make sure the transformation worked
haystack_habitat <- crop(haystack_hab, haystack_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
haystack_habitat <- mask(haystack_hab, haystack_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(haystack_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
haystack_perim <- lsm_p_perim(haystack_habitat)                                         #Calculate perimeter of all habitat patches
haystack_edge <- sum(haystack_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
haystack_patch <- lsm_c_np(haystack_habitat)                                            #Calculate number of habitat patches for each class
haystack_frag <- sum(haystack_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
haystack_cover <- lsm_c_pland(haystack_habitat)                                                       
haystack_cover <- as.data.frame(haystack_cover)
haystack_cover <- haystack_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Intertidal Channel",
      class == 4 ~ "Tidal Pond/Panne",
      class == 5 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
haystack_area <- lsm_c_tca(haystack_habitat)
haystack_area <- as.data.frame(haystack_area)
haystack_area <- haystack_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Intertidal Channel",
      class == 4 ~ "Tidal Pond/Panne",
      class == 5 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
haystack_res <- as.data.frame(merge(haystack_cover, haystack_area, by = "Habitat Type"))
haystack_res <- haystack_res %>%
  mutate(
    Edge = haystack_edge,
    Number_Patches = haystack_frag,
    Marsh = "Haystack"
  )
haystack_res

#-------------------------------------------------------------------------------
#HAYWARD PASTURE

#1. Import and manage files
#A. Read and view habitat raster
hayward_hab <- terra::rast("HaywardPasture_Habitat.tif")
plot(hayward_hab, legend = "bottomleft", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
hayward_shp <- read_sf("HaywardPasture.shp")
plot(hayward_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(hayward_hab, hayward_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(hayward_hab)                                                                      #Check extent of the habitat raster
ext(hayward_shp)                                                                      #Check extent of the shapefile
hayward_shp <- st_transform(hayward_shp, crs(hayward_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
hayward_shp                                                                           #Check to make sure the transformation worked
hayward_habitat <- crop(hayward_hab, hayward_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
hayward_habitat <- mask(hayward_hab, hayward_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(hayward_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
hayward_perim <- lsm_p_perim(hayward_habitat)                                         #Calculate perimeter of all habitat patches
hayward_edge <- sum(hayward_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
hayward_patch <- lsm_c_np(hayward_habitat)                                            #Calculate number of habitat patches for each class
hayward_frag <- sum(hayward_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
hayward_cover <- lsm_c_pland(hayward_habitat)                                                       
hayward_cover <- as.data.frame(hayward_cover)
hayward_cover <- hayward_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Levee",
      class == 5 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
hayward_area <- lsm_c_tca(hayward_habitat)
hayward_area <- as.data.frame(hayward_area)
hayward_area <- hayward_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Levee",
      class == 5 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
hayward_res <- as.data.frame(merge(hayward_cover, hayward_area, by = "Habitat Type"))
hayward_res <- hayward_res %>%
  mutate(
    Edge = hayward_edge,
    Number_Patches = hayward_frag,
    Marsh = "Hayward"
  )
hayward_res

#-------------------------------------------------------------------------------
#HEAD ALBRAE

#1. Import and manage files
#A. Read and view habitat raster
headalbrae_hab <- terra::rast("HeadAlbrae_Habitat.tif")
plot(headalbrae_hab, legend = "topright", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
headalbrae_shp <- read_sf("HeadAlbrae.shp")
plot(headalbrae_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(headalbrae_hab, headalbrae_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(headalbrae_hab)                                                                      #Check extent of the habitat raster
ext(headalbrae_shp)                                                                      #Check extent of the shapefile
headalbrae_shp <- st_transform(headalbrae_shp, crs(headalbrae_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
headalbrae_shp                                                                           #Check to make sure the transformation worked
headalbrae_habitat <- crop(headalbrae_hab, headalbrae_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
headalbrae_habitat <- mask(headalbrae_hab, headalbrae_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(headalbrae_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
headalbrae_perim <- lsm_p_perim(headalbrae_habitat)                                         #Calculate perimeter of all habitat patches
headalbrae_edge <- sum(headalbrae_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
headalbrae_patch <- lsm_c_np(headalbrae_habitat)                                            #Calculate number of habitat patches for each class
headalbrae_frag <- sum(headalbrae_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
headalbrae_cover <- lsm_c_pland(headalbrae_habitat)                                                       
headalbrae_cover <- as.data.frame(headalbrae_cover)
headalbrae_cover <- headalbrae_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Other Open Water",
      class == 2 ~ "Undetermined Other Marsh",
      class == 3 ~ "Levee",
      class == 4 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
headalbrae_area <- lsm_c_tca(headalbrae_habitat)
headalbrae_area <- as.data.frame(headalbrae_area)
headalbrae_area <- headalbrae_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Other Open Water",
      class == 2 ~ "Undetermined Other Marsh",
      class == 3 ~ "Levee",
      class == 4 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
headalbrae_res <- as.data.frame(merge(headalbrae_cover, headalbrae_area, by = "Habitat Type"))
headalbrae_res <- headalbrae_res %>%
  mutate(
    Edge = headalbrae_edge,
    Number_Patches = headalbrae_frag,
    Marsh = "Head Albrae"
  )
headalbrae_res

#-------------------------------------------------------------------------------
#HEERDT

#1. Import and manage files
#A. Read and view habitat raster
heerdt_hab <- terra::rast("Heerdt_Habitat.tif")
plot(heerdt_hab, legend = "topright", plg = list(size=1, cex=0.8))

#B. Remove raster background 
#1) Read and view wetland shapefile
heerdt_shp <- read_sf("Heerdt.shp")
plot(heerdt_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(heerdt_hab, heerdt_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(heerdt_hab)                                                                      #Check extent of the habitat raster
ext(heerdt_shp)                                                                      #Check extent of the shapefile
heerdt_shp <- st_transform(heerdt_shp, crs(heerdt_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
heerdt_shp                                                                           #Check to make sure the transformation worked
heerdt_habitat <- crop(heerdt_hab, heerdt_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
heerdt_habitat <- mask(heerdt_hab, heerdt_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(heerdt_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
heerdt_perim <- lsm_p_perim(heerdt_habitat)                                         #Calculate perimeter of all habitat patches
heerdt_edge <- sum(heerdt_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
heerdt_patch <- lsm_c_np(heerdt_habitat)                                            #Calculate number of habitat patches for each class
heerdt_frag <- sum(heerdt_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
heerdt_cover <- lsm_c_pland(heerdt_habitat)                                                       
heerdt_cover <- as.data.frame(heerdt_cover)
heerdt_cover <- heerdt_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Developed/Urban",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Tidal Pond/Panne",
      class == 6 ~ "Tidal Flat",
      class == 7 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
heerdt_area <- lsm_c_tca(heerdt_habitat)
heerdt_area <- as.data.frame(heerdt_area)
heerdt_area <- heerdt_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Developed/Urban",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Tidal Pond/Panne",
      class == 6 ~ "Tidal Flat",
      class == 7 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
heerdt_res <- as.data.frame(merge(heerdt_cover, heerdt_area, by = "Habitat Type"))
heerdt_res <- heerdt_res %>%
  mutate(
    Edge = heerdt_edge,
    Number_Patches = heerdt_frag,
    Marsh = "Heerdt"
  )
heerdt_res

#-------------------------------------------------------------------------------
#HERCULES

#1. Import and manage files
#A. Read and view habitat raster
hercules_hab <- terra::rast("Hercules_Habitat.tif")
plot(hercules_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
hercules_shp <- read_sf("Hercules.shp")
plot(hercules_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(hercules_hab, hercules_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(hercules_hab)                                                                      #Check extent of the habitat raster
ext(hercules_shp)                                                                      #Check extent of the shapefile
hercules_shp <- st_transform(hercules_shp, crs(hercules_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
hercules_shp                                                                           #Check to make sure the transformation worked
hercules_habitat <- crop(hercules_hab, hercules_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
hercules_habitat <- mask(hercules_hab, hercules_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(hercules_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
hercules_perim <- lsm_p_perim(hercules_habitat)                                         #Calculate perimeter of all habitat patches
hercules_edge <- sum(hercules_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
hercules_patch <- lsm_c_np(hercules_habitat)                                            #Calculate number of habitat patches for each class
hercules_frag <- sum(hercules_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
hercules_cover <- lsm_c_pland(hercules_habitat)                                                       
hercules_cover <- as.data.frame(hercules_cover)
hercules_cover <- hercules_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Dune",
      class == 2 ~ "Tidal Pond/Panne",
      class == 3 ~ "Beach",
      class == 4 ~ "High Marsh",
      class == 5 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
hercules_area <- lsm_c_tca(hercules_habitat)
hercules_area <- as.data.frame(hercules_area)
hercules_area <- hercules_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Dune",
      class == 2 ~ "Tidal Pond/Panne",
      class == 3 ~ "Beach",
      class == 4 ~ "High Marsh",
      class == 5 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
hercules_res <- as.data.frame(merge(hercules_cover, hercules_area, by = "Habitat Type"))
hercules_res <- hercules_res %>%
  mutate(
    Edge = hercules_edge,
    Number_Patches = hercules_frag,
    Marsh = "Hercules"
  )
hercules_res

#-------------------------------------------------------------------------------
#HILL SLOUGH 8

#1. Import and manage files
#A. Read and view habitat raster
hillslough_hab <- terra::rast("HillSlough8_Habitat.tif")
plot(hillslough_hab, legend = "topright", plg = list(size=1, cex=0.8))

#B. Remove raster background 
#1) Read and view wetland shapefile
hillslough_shp <- read_sf("HillSlough8.shp")
plot(hillslough_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(hillslough_hab, hillslough_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(hillslough_hab)                                                                      #Check extent of the habitat raster
ext(hillslough_shp)                                                                      #Check extent of the shapefile
hillslough_shp <- st_transform(hillslough_shp, crs(hillslough_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
hillslough_shp                                                                           #Check to make sure the transformation worked
hillslough_habitat <- crop(hillslough_hab, hillslough_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
hillslough_habitat <- mask(hillslough_hab, hillslough_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(hillslough_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
hillslough_perim <- lsm_p_perim(hillslough_habitat)                                         #Calculate perimeter of all habitat patches
hillslough_edge <- sum(hillslough_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
hillslough_patch <- lsm_c_np(hillslough_habitat)                                            #Calculate number of habitat patches for each class
hillslough_frag <- sum(hillslough_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
hillslough_cover <- lsm_c_pland(hillslough_habitat)                                                       
hillslough_cover <- as.data.frame(hillslough_cover)
hillslough_cover <- hillslough_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "Muted Tidal Marsh",
      class == 4 ~ "Tidal Pond/Panne",
      class == 5 ~ "Intertidal Channel",
      class == 6 ~ "Levee",
      class == 7 ~ "Tidal Flat",
      class == 8 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
hillslough_area <- lsm_c_tca(hillslough_habitat)
hillslough_area <- as.data.frame(hillslough_area)
hillslough_area <- hillslough_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "Muted Tidal Marsh",
      class == 4 ~ "Tidal Pond/Panne",
      class == 5 ~ "Intertidal Channel",
      class == 6 ~ "Levee",
      class == 7 ~ "Tidal Flat",
      class == 8 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
hillslough_res <- as.data.frame(merge(hillslough_cover, hillslough_area, by = "Habitat Type"))
hillslough_res <- hillslough_res %>%
  mutate(
    Edge = hillslough_edge,
    Number_Patches = hillslough_frag,
    Marsh = "Hill Slough 8"
  )
hillslough_res

#-------------------------------------------------------------------------------
#IDEAL MARSH

#1. Import and manage files
#A. Read and view habitat raster
ideal_hab <- terra::rast("IdealMarsh_Habitat.tif")
plot(ideal_hab, legend = "topright", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
ideal_shp <- read_sf("IdealMarsh.shp")
plot(ideal_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(ideal_hab, ideal_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(ideal_hab)                                                                      #Check extent of the habitat raster
ext(ideal_shp)                                                                      #Check extent of the shapefile
ideal_shp <- st_transform(ideal_shp, crs(ideal_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
ideal_shp                                                                           #Check to make sure the transformation worked
ideal_habitat <- crop(ideal_hab, ideal_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
ideal_habitat <- mask(ideal_hab, ideal_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(ideal_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
ideal_perim <- lsm_p_perim(ideal_habitat)                                         #Calculate perimeter of all habitat patches
ideal_edge <- sum(ideal_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
ideal_patch <- lsm_c_np(ideal_habitat)                                            #Calculate number of habitat patches for each class
ideal_frag <- sum(ideal_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
ideal_cover <- lsm_c_pland(ideal_habitat)                                                       
ideal_cover <- as.data.frame(ideal_cover)
ideal_cover <- ideal_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Beach",
      class == 6 ~ "Tidal Flat",
      class == 7 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
ideal_area <- lsm_c_tca(ideal_habitat)
ideal_area <- as.data.frame(ideal_area)
ideal_area <- ideal_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Beach",
      class == 6 ~ "Tidal Flat",
      class == 7 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
ideal_res <- as.data.frame(merge(ideal_cover, ideal_area, by = "Habitat Type"))
ideal_res <- ideal_res %>%
  mutate(
    Edge = ideal_edge,
    Number_Patches = ideal_frag,
    Marsh = "Ideal Marsh"
  )
ideal_res

#-------------------------------------------------------------------------------
#INNER BAIR

#1. Import and manage files
#A. Read and view habitat raster
innerbair_hab <- terra::rast("InnerBair_Habitat.tif")
plot(innerbair_hab, legend = "topright", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
innerbair_shp <- read_sf("InnerBair.shp")
plot(innerbair_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(innerbair_hab, innerbair_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(innerbair_hab)                                                                      #Check extent of the habitat raster
ext(innerbair_shp)                                                                      #Check extent of the shapefile
innerbair_shp <- st_transform(innerbair_shp, crs(innerbair_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
innerbair_shp                                                                           #Check to make sure the transformation worked
innerbair_habitat <- crop(innerbair_hab, innerbair_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
innerbair_habitat <- mask(innerbair_hab, innerbair_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(innerbair_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
innerbair_perim <- lsm_p_perim(innerbair_habitat)                                         #Calculate perimeter of all habitat patches
innerbair_edge <- sum(innerbair_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
innerbair_patch <- lsm_c_np(innerbair_habitat)                                            #Calculate number of habitat patches for each class
innerbair_frag <- sum(innerbair_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
innerbair_cover <- lsm_c_pland(innerbair_habitat)                                                       
innerbair_cover <- as.data.frame(innerbair_cover)
innerbair_cover <- innerbair_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "Levee",
      class == 4 ~ "Tidal Pond/Panne",
      class == 5 ~ "Intertidal Channel",
      class == 6 ~ "Low Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
innerbair_area <- lsm_c_tca(innerbair_habitat)
innerbair_area <- as.data.frame(innerbair_area)
innerbair_area <- innerbair_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "Levee",
      class == 4 ~ "Tidal Pond/Panne",
      class == 5 ~ "Intertidal Channel",
      class == 6 ~ "Low Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
innerbair_res <- as.data.frame(merge(innerbair_cover, innerbair_area, by = "Habitat Type"))
innerbair_res <- innerbair_res %>%
  mutate(
    Edge = innerbair_edge,
    Number_Patches = innerbair_frag,
    Marsh = "Inner Bair"
  )
innerbair_res

#-------------------------------------------------------------------------------
#JOICE ISLAND

#1. Import and manage files
#A. Read and view habitat raster
joice_hab <- terra::rast("JoiceIsland_Habitat.tif")
plot(joice_hab, legend = "topright", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
joice_shp <- read_sf("JoiceIsland.shp")
plot(joice_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(joice_hab, joice_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(joice_hab)                                                                      #Check extent of the habitat raster
ext(joice_shp)                                                                      #Check extent of the shapefile
joice_shp <- st_transform(joice_shp, crs(joice_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
joice_shp                                                                           #Check to make sure the transformation worked
joice_habitat <- crop(joice_hab, joice_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
joice_habitat <- mask(joice_hab, joice_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(joice_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
joice_perim <- lsm_p_perim(joice_habitat)                                         #Calculate perimeter of all habitat patches
joice_edge <- sum(joice_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
joice_patch <- lsm_c_np(joice_habitat)                                            #Calculate number of habitat patches for each class
joice_frag <- sum(joice_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
joice_cover <- lsm_c_pland(joice_habitat)                                                       
joice_cover <- as.data.frame(joice_cover)
joice_cover <- joice_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Intertidal Channel",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "Managed Marsh",
      class == 4 ~ "Levee",
      class == 5 ~ "High Marsh",
      class == 6 ~ "Tidal Flat",
      class == 7 ~ "Tidal Pond/Panne",
      class == 8 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
joice_area <- lsm_c_tca(joice_habitat)
joice_area <- as.data.frame(joice_area)
joice_area <- joice_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Intertidal Channel",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "Managed Marsh",
      class == 4 ~ "Levee",
      class == 5 ~ "High Marsh",
      class == 6 ~ "Tidal Flat",
      class == 7 ~ "Tidal Pond/Panne",
      class == 8 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
joice_res <- as.data.frame(merge(joice_cover, joice_area, by = "Habitat Type"))
joice_res <- joice_res %>%
  mutate(
    Edge = joice_edge,
    Number_Patches = joice_frag,
    Marsh = "Joice Island"
  )
joice_res

#-------------------------------------------------------------------------------
#MALLARD

#1. Import and manage files
#A. Read and view habitat raster
mallard_hab <- terra::rast("Mallard_Habitat.tif")
plot(mallard_hab, legend = "bottomleft", plg = list(size=1, cex=0.8))

#B. Remove raster background 
#1) Read and view wetland shapefile
mallard_shp <- read_sf("Mallard.shp")
plot(mallard_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(mallard_hab, mallard_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(mallard_hab)                                                                      #Check extent of the habitat raster
ext(mallard_shp)                                                                      #Check extent of the shapefile
mallard_shp <- st_transform(mallard_shp, crs(mallard_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
mallard_shp                                                                           #Check to make sure the transformation worked
mallard_habitat <- crop(mallard_hab, mallard_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
mallard_habitat <- mask(mallard_hab, mallard_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(mallard_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
mallard_perim <- lsm_p_perim(mallard_habitat)                                         #Calculate perimeter of all habitat patches
mallard_edge <- sum(mallard_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
mallard_patch <- lsm_c_np(mallard_habitat)                                            #Calculate number of habitat patches for each class
mallard_frag <- sum(mallard_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
mallard_cover <- lsm_c_pland(mallard_habitat)                                                       
mallard_cover <- as.data.frame(mallard_cover)
mallard_cover <- mallard_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Intertidal Channel",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Tidal Flat",
      class == 5 ~ "High Marsh",
      class == 6 ~ "Other Open Water",
      class == 7 ~ "Undetermined Other Marsh",
      class == 8 ~ "Levee",
      class == 9 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
mallard_area <- lsm_c_tca(mallard_habitat)
mallard_area <- as.data.frame(mallard_area)
mallard_area <- mallard_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Intertidal Channel",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Tidal Flat",
      class == 5 ~ "High Marsh",
      class == 6 ~ "Other Open Water",
      class == 7 ~ "Undetermined Other Marsh",
      class == 8 ~ "Levee",
      class == 9 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
mallard_res <- as.data.frame(merge(mallard_cover, mallard_area, by = "Habitat Type"))
mallard_res <- mallard_res %>%
  mutate(
    Edge = mallard_edge,
    Number_Patches = mallard_frag,
    Marsh = "Mallard"
  )
mallard_res

#-------------------------------------------------------------------------------
#MARINER'S POINT

#1. Import and manage files
#A. Read and view habitat raster
mariner_hab <- terra::rast("MarinersPoint_Habitat.tif")
plot(mariner_hab, legend = "topleft", plg = list(size=1, cex=0.8))

#B. Remove raster background 
#1) Read and view wetland shapefile
mariner_shp <- read_sf("MarinersPoint.shp")
plot(mariner_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(mariner_hab, mariner_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(mariner_hab)                                                                      #Check extent of the habitat raster
ext(mariner_shp)                                                                      #Check extent of the shapefile
mariner_shp <- st_transform(mariner_shp, crs(mariner_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
mariner_shp                                                                           #Check to make sure the transformation worked
mariner_habitat <- crop(mariner_hab, mariner_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
mariner_habitat <- mask(mariner_hab, mariner_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(mariner_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
mariner_perim <- lsm_p_perim(mariner_habitat)                                         #Calculate perimeter of all habitat patches
mariner_edge <- sum(mariner_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
mariner_patch <- lsm_c_np(mariner_habitat)                                            #Calculate number of habitat patches for each class
mariner_frag <- sum(mariner_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
mariner_cover <- lsm_c_pland(mariner_habitat)                                                       
mariner_cover <- as.data.frame(mariner_cover)
mariner_cover <- mariner_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Dune",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Levee",
      class == 5 ~ "Tidal Flat",
      class == 6 ~ "Beach",
      class == 7 ~ "Tidal Pond/Panne",
      class == 8 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
mariner_area <- lsm_c_tca(mariner_habitat)
mariner_area <- as.data.frame(mariner_area)
mariner_area <- mariner_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Dune",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Levee",
      class == 5 ~ "Tidal Flat",
      class == 6 ~ "Beach",
      class == 7 ~ "Tidal Pond/Panne",
      class == 8 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
mariner_res <- as.data.frame(merge(mariner_cover, mariner_area, by = "Habitat Type"))
mariner_res <- mariner_res %>%
  mutate(
    Edge = mariner_edge,
    Number_Patches = mariner_frag,
    Marsh = "Mariner's Point"
  )
mariner_res

#-------------------------------------------------------------------------------
#MARTA

#1. Import and manage files
#A. Read and view habitat raster
marta_hab <- terra::rast("Marta_Habitat.tif")
plot(marta_hab, legend = "topleft", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
marta_shp <- read_sf("Marta.shp")
plot(marta_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(marta_hab, marta_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(marta_hab)                                                                      #Check extent of the habitat raster
ext(marta_shp)                                                                      #Check extent of the shapefile
marta_shp <- st_transform(marta_shp, crs(marta_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
marta_shp                                                                           #Check to make sure the transformation worked
marta_habitat <- crop(marta_hab, marta_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
marta_habitat <- mask(marta_hab, marta_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(marta_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
marta_perim <- lsm_p_perim(marta_habitat)                                         #Calculate perimeter of all habitat patches
marta_edge <- sum(marta_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
marta_patch <- lsm_c_np(marta_habitat)                                            #Calculate number of habitat patches for each class
marta_frag <- sum(marta_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
marta_cover <- lsm_c_pland(marta_habitat)                                                       
marta_cover <- as.data.frame(marta_cover)
marta_cover <- marta_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Intertidal Channel",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Tidal Pond/Panne",
      class == 5 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
marta_area <- lsm_c_tca(marta_habitat)
marta_area <- as.data.frame(marta_area)
marta_area <- marta_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Intertidal Channel",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Tidal Pond/Panne",
      class == 5 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
marta_res <- as.data.frame(merge(marta_cover, marta_area, by = "Habitat Type"))
marta_res <- marta_res %>%
  mutate(
    Edge = marta_edge,
    Number_Patches = marta_frag,
    Marsh = "Marta"
  )
marta_res

#-------------------------------------------------------------------------------
#MARTINEZ RS

#1. Import and manage files
#A. Read and view habitat raster
martinez_hab <- terra::rast("MartinezRS_Habitat.tif")
plot(martinez_hab, legend = "topleft", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
martinez_shp <- read_sf("MartinezRS.shp")
plot(martinez_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(martinez_hab, martinez_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(martinez_hab)                                                                      #Check extent of the habitat raster
ext(martinez_shp)                                                                      #Check extent of the shapefile
martinez_shp <- st_transform(martinez_shp, crs(martinez_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
martinez_shp                                                                           #Check to make sure the transformation worked
martinez_habitat <- crop(martinez_hab, martinez_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
martinez_habitat <- mask(martinez_hab, martinez_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(martinez_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
martinez_perim <- lsm_p_perim(martinez_habitat)                                         #Calculate perimeter of all habitat patches
martinez_edge <- sum(martinez_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
martinez_patch <- lsm_c_np(martinez_habitat)                                            #Calculate number of habitat patches for each class
martinez_frag <- sum(martinez_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
martinez_cover <- lsm_c_pland(martinez_habitat)                                                       
martinez_cover <- as.data.frame(martinez_cover)
martinez_cover <- martinez_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Intertidal Channel",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Tidal Pond/Panne",
      class == 5 ~ "Levee",
      class == 6 ~ "Developed/Urban",
      class == 7 ~ "Muted Tidal Marsh",
      class == 8 ~ "Beach",
      class == 9 ~ "Tidal Flat",
      class == 10 ~ "Undetermind Non-Aquatic Diked Bayland",
      class == 11 ~ "Undetermined Other Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
martinez_area <- lsm_c_tca(martinez_habitat)
martinez_area <- as.data.frame(martinez_area)
martinez_area <- martinez_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Intertidal Channel",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Tidal Pond/Panne",
      class == 5 ~ "Levee",
      class == 6 ~ "Developed/Urban",
      class == 7 ~ "Muted Tidal Marsh",
      class == 8 ~ "Beach",
      class == 9 ~ "Tidal Flat",
      class == 10 ~ "Undetermind Non-Aquatic Diked Bayland",
      class == 11 ~ "Undetermined Other Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
martinez_res <- as.data.frame(merge(martinez_cover, martinez_area, by = "Habitat Type"))
martinez_res <- martinez_res %>%
  mutate(
    Edge = martinez_edge,
    Number_Patches = martinez_frag,
    Marsh = "Martinez RS"
  )
martinez_res

#-------------------------------------------------------------------------------
#MCLAUGHLIN

#1. Import and manage files
#A. Read and view habitat raster
mclaughlin_hab <- terra::rast("Mclaughlin_Habitat.tif")
plot(mclaughlin_hab, legend = "topright", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
mclaughlin_shp <- read_sf("Mclaughlin.shp")
plot(mclaughlin_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(mclaughlin_hab, mclaughlin_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(mclaughlin_hab)                                                                      #Check extent of the habitat raster
ext(mclaughlin_shp)                                                                      #Check extent of the shapefile
mclaughlin_shp <- st_transform(mclaughlin_shp, crs(mclaughlin_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
mclaughlin_shp                                                                           #Check to make sure the transformation worked
mclaughlin_habitat <- crop(mclaughlin_hab, mclaughlin_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
mclaughlin_habitat <- mask(mclaughlin_hab, mclaughlin_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(mclaughlin_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
mclaughlin_perim <- lsm_p_perim(mclaughlin_habitat)                                         #Calculate perimeter of all habitat patches
mclaughlin_edge <- sum(mclaughlin_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
mclaughlin_patch <- lsm_c_np(mclaughlin_habitat)                                            #Calculate number of habitat patches for each class
mclaughlin_frag <- sum(mclaughlin_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
mclaughlin_cover <- lsm_c_pland(mclaughlin_habitat)                                                       
mclaughlin_cover <- as.data.frame(mclaughlin_cover)
mclaughlin_cover <- mclaughlin_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Dune",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Beach",
      class == 5 ~ "Low Marsh",
      class == 6 ~ "Intertidal Channel",
      class == 7 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
mclaughlin_area <- lsm_c_tca(mclaughlin_habitat)
mclaughlin_area <- as.data.frame(mclaughlin_area)
mclaughlin_area <- mclaughlin_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Dune",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Beach",
      class == 5 ~ "Low Marsh",
      class == 6 ~ "Intertidal Channel",
      class == 7 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
mclaughlin_res <- as.data.frame(merge(mclaughlin_cover, mclaughlin_area, by = "Habitat Type"))
mclaughlin_res <- mclaughlin_res %>%
  mutate(
    Edge = mclaughlin_edge,
    Number_Patches = mclaughlin_frag,
    Marsh = "McLaughlin"
  )
mclaughlin_res

#-------------------------------------------------------------------------------
#MCNABNEY

#1. Import and manage files
#A. Read and view habitat raster
mcnabney_hab <- terra::rast("Mcnabney_Habitat.tif")
plot(mcnabney_hab, legend = "bottomleft", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
mcnabney_shp <- read_sf("Mcnabney.shp")
plot(mcnabney_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(mcnabney_hab, mcnabney_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(mcnabney_hab)                                                                      #Check extent of the habitat raster
ext(mcnabney_shp)                                                                      #Check extent of the shapefile
mcnabney_shp <- st_transform(mcnabney_shp, crs(mcnabney_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
mcnabney_shp                                                                           #Check to make sure the transformation worked
mcnabney_habitat <- crop(mcnabney_hab, mcnabney_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
mcnabney_habitat <- mask(mcnabney_hab, mcnabney_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(mcnabney_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
mcnabney_perim <- lsm_p_perim(mcnabney_habitat)                                         #Calculate perimeter of all habitat patches
mcnabney_edge <- sum(mcnabney_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
mcnabney_patch <- lsm_c_np(mcnabney_habitat)                                            #Calculate number of habitat patches for each class
mcnabney_frag <- sum(mcnabney_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
mcnabney_cover <- lsm_c_pland(mcnabney_habitat)                                                       
mcnabney_cover <- as.data.frame(mcnabney_cover)
mcnabney_cover <- mcnabney_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Developed/Urban",
      class == 2 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 3 ~ "Managed Marsh",
      class == 4 ~ "Other Open Water",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
mcnabney_area <- lsm_c_tca(mcnabney_habitat)
mcnabney_area <- as.data.frame(mcnabney_area)
mcnabney_area <- mcnabney_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Developed/Urban",
      class == 2 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 3 ~ "Managed Marsh",
      class == 4 ~ "Other Open Water",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
mcnabney_res <- as.data.frame(merge(mcnabney_cover, mcnabney_area, by = "Habitat Type"))
mcnabney_res <- mcnabney_res %>%
  mutate(
    Edge = mcnabney_edge,
    Number_Patches = mcnabney_frag,
    Marsh = "McNabney"
  )
mcnabney_res

#-------------------------------------------------------------------------------
#MLK

#1. Import and manage files
#A. Read and view habitat raster
mlk_hab <- terra::rast("MLK_Habitat.tif")
plot(mlk_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
mlk_shp <- read_sf("MLK.shp")
plot(mlk_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(mlk_hab, mlk_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(mlk_hab)                                                                      #Check extent of the habitat raster
ext(mlk_shp)                                                                      #Check extent of the shapefile
mlk_shp <- st_transform(mlk_shp, crs(mlk_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
mlk_shp                                                                           #Check to make sure the transformation worked
mlk_habitat <- crop(mlk_hab, mlk_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
mlk_habitat <- mask(mlk_hab, mlk_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(mlk_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
mlk_perim <- lsm_p_perim(mlk_habitat)                                         #Calculate perimeter of all habitat patches
mlk_edge <- sum(mlk_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
mlk_patch <- lsm_c_np(mlk_habitat)                                            #Calculate number of habitat patches for each class
mlk_frag <- sum(mlk_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
mlk_cover <- lsm_c_pland(mlk_habitat)                                                       
mlk_cover <- as.data.frame(mlk_cover)
mlk_cover <- mlk_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Levee",
      class == 2 ~ "Tidal Pond/Panne",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
mlk_area <- lsm_c_tca(mlk_habitat)
mlk_area <- as.data.frame(mlk_area)
mlk_area <- mlk_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Levee",
      class == 2 ~ "Tidal Pond/Panne",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
mlk_res <- as.data.frame(merge(mlk_cover, mlk_area, by = "Habitat Type"))
mlk_res <- mlk_res %>%
  mutate(
    Edge = mlk_edge,
    Number_Patches = mlk_frag,
    Marsh = "MLK"
  )
mlk_res

#-------------------------------------------------------------------------------
#MOUNTAIN VIEW SLOUGH

#1. Import and manage files
#A. Read and view habitat raster
mtview_hab <- terra::rast("MountainViewSlough_Habitat.tif")
plot(mtview_hab, legend = "bottomright", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
mtview_shp <- read_sf("MountainViewSlough.shp")
plot(mtview_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(mtview_hab, mtview_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(mtview_hab)                                                                      #Check extent of the habitat raster
ext(mtview_shp)                                                                      #Check extent of the shapefile
mtview_shp <- st_transform(mtview_shp, crs(mtview_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
mtview_shp                                                                           #Check to make sure the transformation worked
mtview_habitat <- crop(mtview_hab, mtview_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
mtview_habitat <- mask(mtview_hab, mtview_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(mtview_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
mtview_perim <- lsm_p_perim(mtview_habitat)                                         #Calculate perimeter of all habitat patches
mtview_edge <- sum(mtview_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
mtview_patch <- lsm_c_np(mtview_habitat)                                            #Calculate number of habitat patches for each class
mtview_frag <- sum(mtview_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
mtview_cover <- lsm_c_pland(mtview_habitat)                                                       
mtview_cover <- as.data.frame(mtview_cover)
mtview_cover <- mtview_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "Intertidal Channel",
      class == 4 ~ "Shallow Subtidal",
      class == 5 ~ "Levee",
      class == 6 ~ "High Marsh",
      class == 7 ~ "Tidal Pond/Panne",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
mtview_area <- lsm_c_tca(mtview_habitat)
mtview_area <- as.data.frame(mtview_area)
mtview_area <- mtview_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "Intertidal Channel",
      class == 4 ~ "Shallow Subtidal",
      class == 5 ~ "Levee",
      class == 6 ~ "High Marsh",
      class == 7 ~ "Tidal Pond/Panne",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
mtview_res <- as.data.frame(merge(mtview_cover, mtview_area, by = "Habitat Type"))
mtview_res <- mtview_res %>%
  mutate(
    Edge = mtview_edge,
    Number_Patches = mtview_frag,
    Marsh = "Mountain View Slough"
  )
mtview_res

#-------------------------------------------------------------------------------
#MUNDY

#1. Import and manage files
#A. Read and view habitat raster
mundy_hab <- terra::rast("Mundy_Habitat.tif")
plot(mundy_hab, legend = "bottomleft", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
mundy_shp <- read_sf("Mundy.shp")
plot(mundy_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(mundy_hab, mundy_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(mundy_hab)                                                                      #Check extent of the habitat raster
ext(mundy_shp)                                                                      #Check extent of the shapefile
mundy_shp <- st_transform(mundy_shp, crs(mundy_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
mundy_shp                                                                           #Check to make sure the transformation worked
mundy_habitat <- crop(mundy_hab, mundy_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
mundy_habitat <- mask(mundy_hab, mundy_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(mundy_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
mundy_perim <- lsm_p_perim(mundy_habitat)                                         #Calculate perimeter of all habitat patches
mundy_edge <- sum(mundy_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
mundy_patch <- lsm_c_np(mundy_habitat)                                            #Calculate number of habitat patches for each class
mundy_frag <- sum(mundy_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
mundy_cover <- lsm_c_pland(mundy_habitat)                                                       
mundy_cover <- as.data.frame(mundy_cover)
mundy_cover <- mundy_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Intertidal Channel",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "High Marsh",
      class == 5 ~ "Tidal Flat",
      class == 6 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
mundy_area <- lsm_c_tca(mundy_habitat)
mundy_area <- as.data.frame(mundy_area)
mundy_area <- mundy_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Intertidal Channel",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "High Marsh",
      class == 5 ~ "Tidal Flat",
      class == 6 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
mundy_res <- as.data.frame(merge(mundy_cover, mundy_area, by = "Habitat Type"))
mundy_res <- mundy_res %>%
  mutate(
    Edge = mundy_edge,
    Number_Patches = mundy_frag,
    Marsh = "Mundy"
  )
mundy_res

#-------------------------------------------------------------------------------
#MUNSTER PROPERTY

#1. Import and manage files
#A. Read and view habitat raster
munster_hab <- terra::rast("MunsterProperty_Habitat.tif")
plot(munster_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
munster_shp <- read_sf("MunsterProperty.shp")
plot(munster_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(munster_hab, munster_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(munster_hab)                                                                      #Check extent of the habitat raster
ext(munster_shp)                                                                      #Check extent of the shapefile
munster_shp <- st_transform(munster_shp, crs(munster_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
munster_shp                                                                           #Check to make sure the transformation worked
munster_habitat <- crop(munster_hab, munster_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
munster_habitat <- mask(munster_hab, munster_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(munster_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
munster_perim <- lsm_p_perim(munster_habitat)                                         #Calculate perimeter of all habitat patches
munster_edge <- sum(munster_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
munster_patch <- lsm_c_np(munster_habitat)                                            #Calculate number of habitat patches for each class
munster_frag <- sum(munster_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
munster_cover <- lsm_c_pland(munster_habitat)                                                       
munster_cover <- as.data.frame(munster_cover)
munster_cover <- munster_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Developed/Urban",
      class == 2 ~ "Managed Marsh",
      class == 3 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
munster_area <- lsm_c_tca(munster_habitat)
munster_area <- as.data.frame(munster_area)
munster_area <- munster_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Developed/Urban",
      class == 2 ~ "Managed Marsh",
      class == 3 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
munster_res <- as.data.frame(merge(munster_cover, munster_area, by = "Habitat Type"))
munster_res <- munster_res %>%
  mutate(
    Edge = munster_edge,
    Number_Patches = munster_frag,
    Marsh = "Munster Property"
  )
munster_res

#-------------------------------------------------------------------------------
#MUZZI NORTH

#1. Import and manage files
#A. Read and view habitat raster
muzzin_hab <- terra::rast("MuzziNorth_Habitat.tif")
plot(muzzin_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
muzzin_shp <- read_sf("MuzziNorth.shp")
plot(muzzin_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(muzzin_hab, muzzin_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(muzzin_hab)                                                                      #Check extent of the habitat raster
ext(muzzin_shp)                                                                      #Check extent of the shapefile
muzzin_shp <- st_transform(muzzin_shp, crs(muzzin_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
muzzin_shp                                                                           #Check to make sure the transformation worked
muzzin_habitat <- crop(muzzin_hab, muzzin_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
muzzin_habitat <- mask(muzzin_hab, muzzin_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(muzzin_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
muzzin_perim <- lsm_p_perim(muzzin_habitat)                                         #Calculate perimeter of all habitat patches
muzzin_edge <- sum(muzzin_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
muzzin_patch <- lsm_c_np(muzzin_habitat)                                            #Calculate number of habitat patches for each class
muzzin_frag <- sum(muzzin_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
muzzin_cover <- lsm_c_pland(muzzin_habitat)                                                       
muzzin_cover <- as.data.frame(muzzin_cover)
muzzin_cover <- muzzin_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Levee",
      class == 5 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
muzzin_area <- lsm_c_tca(muzzin_habitat)
muzzin_area <- as.data.frame(muzzin_area)
muzzin_area <- muzzin_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Tidal Flat",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Levee",
      class == 5 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
muzzin_res <- as.data.frame(merge(muzzin_cover, muzzin_area, by = "Habitat Type"))
muzzin_res <- muzzin_res %>%
  mutate(
    Edge = muzzin_edge,
    Number_Patches = muzzin_frag,
    Marsh = "Muzzi North"
  )
muzzin_res

#-------------------------------------------------------------------------------
#MUZZI SOUTH

#1. Import and manage files
#A. Read and view habitat raster
muzzis_hab <- terra::rast("MuzziSouth_Habitat.tif")
plot(muzzis_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
muzzis_shp <- read_sf("MuzziSouth.shp")
plot(muzzis_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(muzzis_hab, muzzis_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(muzzis_hab)                                                                      #Check extent of the habitat raster
ext(muzzis_shp)                                                                      #Check extent of the shapefile
muzzis_shp <- st_transform(muzzis_shp, crs(muzzis_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
muzzis_shp                                                                           #Check to make sure the transformation worked
muzzis_habitat <- crop(muzzis_hab, muzzis_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
muzzis_habitat <- mask(muzzis_hab, muzzis_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(muzzis_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
muzzis_perim <- lsm_p_perim(muzzis_habitat)                                         #Calculate perimeter of all habitat patches
muzzis_edge <- sum(muzzis_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
muzzis_patch <- lsm_c_np(muzzis_habitat)                                            #Calculate number of habitat patches for each class
muzzis_frag <- sum(muzzis_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
muzzis_cover <- lsm_c_pland(muzzis_habitat)                                                       
muzzis_cover <- as.data.frame(muzzis_cover)
muzzis_cover <- muzzis_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Levee",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Flat",
      class == 4 ~ "Tidal Pond/Panne",
      class == 5 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
muzzis_area <- lsm_c_tca(muzzis_habitat)
muzzis_area <- as.data.frame(muzzis_area)
muzzis_area <- muzzis_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Levee",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Flat",
      class == 4 ~ "Tidal Pond/Panne",
      class == 5 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
muzzis_res <- as.data.frame(merge(muzzis_cover, muzzis_area, by = "Habitat Type"))
muzzis_res <- muzzis_res %>%
  mutate(
    Edge = muzzis_edge,
    Number_Patches = muzzis_frag,
    Marsh = "Muzzi South"
  )
muzzis_res

#-------------------------------------------------------------------------------
#NEWARK MOUSE PASTURE

#1. Import and manage files
#A. Read and view habitat raster
newark_hab <- terra::rast("NewarkMousePasture_Habitat.tif")
plot(newark_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
newark_shp <- read_sf("NewarkMousePasture.shp")
plot(newark_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(newark_hab, newark_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(newark_hab)                                                                      #Check extent of the habitat raster
ext(newark_shp)                                                                      #Check extent of the shapefile
newark_shp <- st_transform(newark_shp, crs(newark_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
newark_shp                                                                           #Check to make sure the transformation worked
newark_habitat <- crop(newark_hab, newark_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
newark_habitat <- mask(newark_hab, newark_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(newark_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
newark_perim <- lsm_p_perim(newark_habitat)                                         #Calculate perimeter of all habitat patches
newark_edge <- sum(newark_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
newark_patch <- lsm_c_np(newark_habitat)                                            #Calculate number of habitat patches for each class
newark_frag <- sum(newark_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
newark_cover <- lsm_c_pland(newark_habitat)                                                       
newark_cover <- as.data.frame(newark_cover)
newark_cover <- newark_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Open Other Water",
      class == 2 ~ "Managed Marsh",
      class == 3 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
newark_area <- lsm_c_tca(newark_habitat)
newark_area <- as.data.frame(newark_area)
newark_area <- newark_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Open Other Water",
      class == 2 ~ "Managed Marsh",
      class == 3 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
newark_res <- as.data.frame(merge(newark_cover, newark_area, by = "Habitat Type"))
newark_res <- newark_res %>%
  mutate(
    Edge = newark_edge,
    Number_Patches = newark_frag,
    Marsh = "Newark Mouse Pasture"
  )
newark_res

#-------------------------------------------------------------------------------
#NEW CHICAGO

#1. Import and manage files
#A. Read and view habitat raster
newchicago_hab <- terra::rast("NewChicago_Habitat.tif")
plot(newchicago_hab, legend = "topleft", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
newchicago_shp <- read_sf("NewChicago.shp")
plot(newchicago_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(newchicago_hab, newchicago_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(newchicago_hab)                                                                      #Check extent of the habitat raster
ext(newchicago_shp)                                                                      #Check extent of the shapefile
newchicago_shp <- st_transform(newchicago_shp, crs(newchicago_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
newchicago_shp                                                                           #Check to make sure the transformation worked
newchicago_habitat <- crop(newchicago_hab, newchicago_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
newchicago_habitat <- mask(newchicago_hab, newchicago_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(newchicago_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
newchicago_perim <- lsm_p_perim(newchicago_habitat)                                         #Calculate perimeter of all habitat patches
newchicago_edge <- sum(newchicago_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
newchicago_patch <- lsm_c_np(newchicago_habitat)                                            #Calculate number of habitat patches for each class
newchicago_frag <- sum(newchicago_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
newchicago_cover <- lsm_c_pland(newchicago_habitat)                                                       
newchicago_cover <- as.data.frame(newchicago_cover)
newchicago_cover <- newchicago_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Developed/Urban",
      class == 2 ~ "Other Open Water",
      class == 3 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
newchicago_area <- lsm_c_tca(newchicago_habitat)
newchicago_area <- as.data.frame(newchicago_area)
newchicago_area <- newchicago_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Developed/Urban",
      class == 2 ~ "Other Open Water",
      class == 3 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
newchicago_res <- as.data.frame(merge(newchicago_cover, newchicago_area, by = "Habitat Type"))
newchicago_res <- newchicago_res %>%
  mutate(
    Edge = newchicago_edge,
    Number_Patches = newchicago_frag,
    Marsh = "New Chicago"
  )
newchicago_res

#-------------------------------------------------------------------------------
#NOVATO

#1. Import and manage files
#A. Read and view habitat raster
novato_hab <- terra::rast("Novato_Habitat.tif")
plot(novato_hab, legend = "topright", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
novato_shp <- read_sf("Novato.shp")
plot(novato_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(novato_hab, novato_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(novato_hab)                                                                      #Check extent of the habitat raster
ext(novato_shp)                                                                      #Check extent of the shapefile
novato_shp <- st_transform(novato_shp, crs(novato_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
novato_shp                                                                           #Check to make sure the transformation worked
novato_habitat <- crop(novato_hab, novato_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
novato_habitat <- mask(novato_hab, novato_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(novato_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
novato_perim <- lsm_p_perim(novato_habitat)                                         #Calculate perimeter of all habitat patches
novato_edge <- sum(novato_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
novato_patch <- lsm_c_np(novato_habitat)                                            #Calculate number of habitat patches for each class
novato_frag <- sum(novato_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
novato_cover <- lsm_c_pland(novato_habitat)                                                       
novato_cover <- as.data.frame(novato_cover)
novato_cover <- novato_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Developed/Urban",
      class == 2 ~ "Other Open Water",
      class == 3 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 4 ~ "High Marsh",
      class == 5 ~ "Managed Marsh",
      class == 6 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
novato_area <- lsm_c_tca(novato_habitat)
novato_area <- as.data.frame(novato_area)
novato_area <- novato_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Developed/Urban",
      class == 2 ~ "Other Open Water",
      class == 3 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 4 ~ "High Marsh",
      class == 5 ~ "Managed Marsh",
      class == 6 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
novato_res <- as.data.frame(merge(novato_cover, novato_area, by = "Habitat Type"))
novato_res <- novato_res %>%
  mutate(
    Edge = novato_edge,
    Number_Patches = novato_frag,
    Marsh = "Novato"
  )
novato_res

#-------------------------------------------------------------------------------
#OAKLAND AIRPORT

#1. Import and manage files
#A. Read and view habitat raster
oakland_hab <- terra::rast("OaklandAirport_Habitat.tif")
plot(oakland_hab, legend = "bottomleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
oakland_shp <- read_sf("OaklandAirport.shp")
plot(oakland_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(oakland_hab, oakland_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(oakland_hab)                                                                      #Check extent of the habitat raster
ext(oakland_shp)                                                                      #Check extent of the shapefile
oakland_shp <- st_transform(oakland_shp, crs(oakland_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
oakland_shp                                                                           #Check to make sure the transformation worked
oakland_habitat <- crop(oakland_hab, oakland_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
oakland_habitat <- mask(oakland_hab, oakland_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(oakland_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
oakland_perim <- lsm_p_perim(oakland_habitat)                                         #Calculate perimeter of all habitat patches
oakland_edge <- sum(oakland_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
oakland_patch <- lsm_c_np(oakland_habitat)                                            #Calculate number of habitat patches for each class
oakland_frag <- sum(oakland_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
oakland_cover <- lsm_c_pland(oakland_habitat)                                                       
oakland_cover <- as.data.frame(oakland_cover)
oakland_cover <- oakland_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Undetermined Other Marsh",
      class == 2 ~ "Undetermined Non-Aquatic Diked Bayland",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
oakland_area <- lsm_c_tca(oakland_habitat)
oakland_area <- as.data.frame(oakland_area)
oakland_area <- oakland_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Undetermined Other Marsh",
      class == 2 ~ "Undetermined Non-Aquatic Diked Bayland",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
oakland_res <- as.data.frame(merge(oakland_cover, oakland_area, by = "Habitat Type"))
oakland_res <- oakland_res %>%
  mutate(
    Edge = oakland_edge,
    Number_Patches = oakland_frag,
    Marsh = "Oakland Airport"
  )
oakland_res

#-------------------------------------------------------------------------------
#PIPER PARK

#1. Import and manage files
#A. Read and view habitat raster
piperpark_hab <- terra::rast("PiperPark_Habitat.tif")
plot(piperpark_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
piperpark_shp <- read_sf("PiperPark.shp")
plot(piperpark_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(piperpark_hab, piperpark_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(piperpark_hab)                                                                      #Check extent of the habitat raster
ext(piperpark_shp)                                                                      #Check extent of the shapefile
piperpark_shp <- st_transform(piperpark_shp, crs(piperpark_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
piperpark_shp                                                                           #Check to make sure the transformation worked
piperpark_habitat <- crop(piperpark_hab, piperpark_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
piperpark_habitat <- mask(piperpark_hab, piperpark_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(piperpark_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
piperpark_perim <- lsm_p_perim(piperpark_habitat)                                         #Calculate perimeter of all habitat patches
piperpark_edge <- sum(piperpark_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
piperpark_patch <- lsm_c_np(piperpark_habitat)                                            #Calculate number of habitat patches for each class
piperpark_frag <- sum(piperpark_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
piperpark_cover <- lsm_c_pland(piperpark_habitat)                                                       
piperpark_cover <- as.data.frame(piperpark_cover)
piperpark_cover <- piperpark_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Intertidal Channel",
      class == 4 ~ "Levee",
      class == 5 ~ "Tidal Flat",
      class == 6 ~ "Urban",
      class == 7 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
piperpark_area <- lsm_c_tca(piperpark_habitat)
piperpark_area <- as.data.frame(piperpark_area)
piperpark_area <- piperpark_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Intertidal Channel",
      class == 4 ~ "Levee",
      class == 5 ~ "Tidal Flat",
      class == 6 ~ "Urban",
      class == 7 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
piperpark_res <- as.data.frame(merge(piperpark_cover, piperpark_area, by = "Habitat Type"))
piperpark_res <- piperpark_res %>%
  mutate(
    Edge = piperpark_edge,
    Number_Patches = piperpark_frag,
    Marsh = "Piper Park"
  )
piperpark_res

#-------------------------------------------------------------------------------
#PLUMMER SLOUGH

#1. Import and manage files
#A. Read and view habitat raster
plummer_hab <- terra::rast("PlummerSlough_Habitat.tif")
plot(plummer_hab, legend = "bottomright", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
plummer_shp <- read_sf("PlummerSlough.shp")
plot(plummer_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(plummer_hab, plummer_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(plummer_hab)                                                                      #Check extent of the habitat raster
ext(plummer_shp)                                                                      #Check extent of the shapefile
plummer_shp <- st_transform(plummer_shp, crs(plummer_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
plummer_shp                                                                           #Check to make sure the transformation worked
plummer_habitat <- crop(plummer_hab, plummer_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
plummer_habitat <- mask(plummer_hab, plummer_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(plummer_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
plummer_perim <- lsm_p_perim(plummer_habitat)                                         #Calculate perimeter of all habitat patches
plummer_edge <- sum(plummer_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
plummer_patch <- lsm_c_np(plummer_habitat)                                            #Calculate number of habitat patches for each class
plummer_frag <- sum(plummer_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
plummer_cover <- lsm_c_pland(plummer_habitat)                                                       
plummer_cover <- as.data.frame(plummer_cover)
plummer_cover <- plummer_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Tidal Pond/Panne",
      class == 3 ~ "Levee",
      class == 4 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 5 ~ "Low Marsh",
      class == 6 ~ "Tidal Flat",
      class == 7 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
plummer_area <- lsm_c_tca(plummer_habitat)
plummer_area <- as.data.frame(plummer_area)
plummer_area <- plummer_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Tidal Pond/Panne",
      class == 3 ~ "Levee",
      class == 4 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 5 ~ "Low Marsh",
      class == 6 ~ "Tidal Flat",
      class == 7 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
plummer_res <- as.data.frame(merge(plummer_cover, plummer_area, by = "Habitat Type"))
plummer_res <- plummer_res %>%
  mutate(
    Edge = plummer_edge,
    Number_Patches = plummer_frag,
    Marsh = "Plummer Slough"
  )
plummer_res

#-------------------------------------------------------------------------------
#POINT PINOLE

#1. Import and manage files
#A. Read and view habitat raster
ptpinole_hab <- terra::rast("PointPinole_Habitat.tif")
plot(ptpinole_hab, legend = "bottomleft", plg = list(size=1, cex=0.6))

#B. Remove raster background 
#1) Read and view wetland shapefile
ptpinole_shp <- read_sf("PointPinole.shp")
plot(ptpinole_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(ptpinole_hab, ptpinole_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(ptpinole_hab)                                                                      #Check extent of the habitat raster
ext(ptpinole_shp)                                                                      #Check extent of the shapefile
ptpinole_shp <- st_transform(ptpinole_shp, crs(ptpinole_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
ptpinole_shp                                                                           #Check to make sure the transformation worked
ptpinole_habitat <- crop(ptpinole_hab, ptpinole_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
ptpinole_habitat <- mask(ptpinole_hab, ptpinole_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(ptpinole_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
ptpinole_perim <- lsm_p_perim(ptpinole_habitat)                                         #Calculate perimeter of all habitat patches
ptpinole_edge <- sum(ptpinole_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
ptpinole_patch <- lsm_c_np(ptpinole_habitat)                                            #Calculate number of habitat patches for each class
ptpinole_frag <- sum(ptpinole_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
ptpinole_cover <- lsm_c_pland(ptpinole_habitat)                                                       
ptpinole_cover <- as.data.frame(ptpinole_cover)
ptpinole_cover <- ptpinole_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Tidal Pond/Panne",
      class == 3 ~ "Beach",
      class == 4 ~ "Dune",
      class == 5 ~ "Levee",
      class == 6 ~ "Intertidal Channel",
      class == 7 ~ "Undetermined Other Marsh",
      class == 8 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
ptpinole_area <- lsm_c_tca(ptpinole_habitat)
ptpinole_area <- as.data.frame(ptpinole_area)
ptpinole_area <- ptpinole_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Tidal Pond/Panne",
      class == 3 ~ "Beach",
      class == 4 ~ "Dune",
      class == 5 ~ "Levee",
      class == 6 ~ "Intertidal Channel",
      class == 7 ~ "Undetermined Other Marsh",
      class == 8 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
ptpinole_res <- as.data.frame(merge(ptpinole_cover, ptpinole_area, by = "Habitat Type"))
ptpinole_res <- ptpinole_res %>%
  mutate(
    Edge = ptpinole_edge,
    Number_Patches = ptpinole_frag,
    Marsh = "Point Pinole"
  )
ptpinole_res

#-------------------------------------------------------------------------------
#PROFESSIONAL CIRCLE

#1. Import and manage files
#A. Read and view habitat raster
profcircle_hab <- terra::rast("ProfessionalCircle_Habitat.tif")
plot(profcircle_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
profcircle_shp <- read_sf("ProfessionalCircle.shp")
plot(profcircle_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(profcircle_hab, profcircle_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(profcircle_hab)                                                                      #Check extent of the habitat raster
ext(profcircle_shp)                                                                      #Check extent of the shapefile
profcircle_shp <- st_transform(profcircle_shp, crs(profcircle_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
profcircle_shp                                                                           #Check to make sure the transformation worked
profcircle_habitat <- crop(profcircle_hab, profcircle_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
profcircle_habitat <- mask(profcircle_hab, profcircle_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(profcircle_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
profcircle_perim <- lsm_p_perim(profcircle_habitat)                                         #Calculate perimeter of all habitat patches
profcircle_edge <- sum(profcircle_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
profcircle_patch <- lsm_c_np(profcircle_habitat)                                            #Calculate number of habitat patches for each class
profcircle_frag <- sum(profcircle_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
profcircle_cover <- lsm_c_pland(profcircle_habitat)                                                       
profcircle_cover <- as.data.frame(profcircle_cover)
profcircle_cover <- profcircle_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Other Open Water",
      class == 2 ~ "Managed Marsh",
      class == 3 ~ "Levee",
      class == 4 ~ "Undetermined Other Marsh",
      class == 5 ~ "Developed/Urban",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
profcircle_area <- lsm_c_tca(profcircle_habitat)
profcircle_area <- as.data.frame(profcircle_area)
profcircle_area <- profcircle_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Other Open Water",
      class == 2 ~ "Managed Marsh",
      class == 3 ~ "Levee",
      class == 4 ~ "Undetermined Other Marsh",
      class == 5 ~ "Developed/Urban",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
profcircle_res <- as.data.frame(merge(profcircle_cover, profcircle_area, by = "Habitat Type"))
profcircle_res <- profcircle_res %>%
  mutate(
    Edge = profcircle_edge,
    Number_Patches = profcircle_frag,
    Marsh = "Professional Circle"
  )
profcircle_res

#-------------------------------------------------------------------------------
#RADIO POINT

#1. Import and manage files
#A. Read and view habitat raster
radiopt_hab <- terra::rast("RadioPoint_Habitat.tif")
plot(radiopt_hab, legend = "bottomleft", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
radiopt_shp <- read_sf("RadioPoint.shp")
plot(radiopt_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(radiopt_hab, radiopt_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(radiopt_hab)                                                                      #Check extent of the habitat raster
ext(radiopt_shp)                                                                      #Check extent of the shapefile
radiopt_shp <- st_transform(radiopt_shp, crs(radiopt_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
radiopt_shp                                                                           #Check to make sure the transformation worked
radiopt_habitat <- crop(radiopt_hab, radiopt_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
radiopt_habitat <- mask(radiopt_hab, radiopt_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(radiopt_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
radiopt_perim <- lsm_p_perim(radiopt_habitat)                                         #Calculate perimeter of all habitat patches
radiopt_edge <- sum(radiopt_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
radiopt_patch <- lsm_c_np(radiopt_habitat)                                            #Calculate number of habitat patches for each class
radiopt_frag <- sum(radiopt_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
radiopt_cover <- lsm_c_pland(radiopt_habitat)                                                       
radiopt_cover <- as.data.frame(radiopt_cover)
radiopt_cover <- radiopt_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Tidal Pond/Panne",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Beach",
      class == 6 ~ "Tidal Flat",
      class == 7 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
radiopt_area <- lsm_c_tca(radiopt_habitat)
radiopt_area <- as.data.frame(radiopt_area)
radiopt_area <- radiopt_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Tidal Pond/Panne",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Beach",
      class == 6 ~ "Tidal Flat",
      class == 7 ~ "Levee",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
radiopt_res <- as.data.frame(merge(radiopt_cover, radiopt_area, by = "Habitat Type"))
radiopt_res <- radiopt_res %>%
  mutate(
    Edge = radiopt_edge,
    Number_Patches = radiopt_frag,
    Marsh = "Radio Point"
  )
radiopt_res

#-------------------------------------------------------------------------------
#RAVENSWOOD SLOUGH

#1. Import and manage files
#A. Read and view habitat raster
ravenswood_hab <- terra::rast("RavenswoodSlough_Habitat.tif")
plot(ravenswood_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
ravenswood_shp <- read_sf("RavenswoodSlough.shp")
plot(ravenswood_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(ravenswood_hab, ravenswood_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(ravenswood_hab)                                                                      #Check extent of the habitat raster
ext(ravenswood_shp)                                                                      #Check extent of the shapefile
ravenswood_shp <- st_transform(ravenswood_shp, crs(ravenswood_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
ravenswood_shp                                                                           #Check to make sure the transformation worked
ravenswood_habitat <- crop(ravenswood_hab, ravenswood_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
ravenswood_habitat <- mask(ravenswood_hab, ravenswood_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(ravenswood_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
ravenswood_perim <- lsm_p_perim(ravenswood_habitat)                                         #Calculate perimeter of all habitat patches
ravenswood_edge <- sum(ravenswood_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
ravenswood_patch <- lsm_c_np(ravenswood_habitat)                                            #Calculate number of habitat patches for each class
ravenswood_frag <- sum(ravenswood_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
ravenswood_cover <- lsm_c_pland(ravenswood_habitat)                                                       
ravenswood_cover <- as.data.frame(ravenswood_cover)
ravenswood_cover <- ravenswood_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Intertidal Channel",
      class == 4 ~ "Tidal Flat",
      class == 5 ~ "Levee",
      class == 6 ~ "Tidal Pond/Panne",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
ravenswood_area <- lsm_c_tca(ravenswood_habitat)
ravenswood_area <- as.data.frame(ravenswood_area)
ravenswood_area <- ravenswood_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Intertidal Channel",
      class == 4 ~ "Tidal Flat",
      class == 5 ~ "Levee",
      class == 6 ~ "Tidal Pond/Panne",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
ravenswood_res <- as.data.frame(merge(ravenswood_cover, ravenswood_area, by = "Habitat Type"))
ravenswood_res <- ravenswood_res %>%
  mutate(
    Edge = ravenswood_edge,
    Number_Patches = ravenswood_frag,
    Marsh = "Ravenswood Slough"
  )
ravenswood_res

#-------------------------------------------------------------------------------
#RENZEL WETLANDS

#1. Import and manage files
#A. Read and view habitat raster
renzel_hab <- terra::rast("RenzelWetlands_Habitat.tif")
plot(renzel_hab, legend = "topleft", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
renzel_shp <- read_sf("RenzelWetlands.shp")
plot(renzel_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(renzel_hab, renzel_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(renzel_hab)                                                                      #Check extent of the habitat raster
ext(renzel_shp)                                                                      #Check extent of the shapefile
renzel_shp <- st_transform(renzel_shp, crs(renzel_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
renzel_shp                                                                           #Check to make sure the transformation worked
renzel_habitat <- crop(renzel_hab, renzel_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
renzel_habitat <- mask(renzel_hab, renzel_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(renzel_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
renzel_perim <- lsm_p_perim(renzel_habitat)                                         #Calculate perimeter of all habitat patches
renzel_edge <- sum(renzel_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
renzel_patch <- lsm_c_np(renzel_habitat)                                            #Calculate number of habitat patches for each class
renzel_frag <- sum(renzel_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
renzel_cover <- lsm_c_pland(renzel_habitat)                                                       
renzel_cover <- as.data.frame(renzel_cover)
renzel_cover <- renzel_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Undetermined Other Marsh",
      class == 2 ~ "Managed Marsh",
      class == 3 ~ "Developed/Urban",
      class == 4 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 5 ~ "Other Open Water",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
renzel_area <- lsm_c_tca(renzel_habitat)
renzel_area <- as.data.frame(renzel_area)
renzel_area <- renzel_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Undetermined Other Marsh",
      class == 2 ~ "Managed Marsh",
      class == 3 ~ "Developed/Urban",
      class == 4 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 5 ~ "Other Open Water",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
renzel_res <- as.data.frame(merge(renzel_cover, renzel_area, by = "Habitat Type"))
renzel_res <- renzel_res %>%
  mutate(
    Edge = renzel_edge,
    Number_Patches = renzel_frag,
    Marsh = "Renzel Wetlands"
  )
renzel_res

#-------------------------------------------------------------------------------
#RINGSTROM

#1. Import and manage files
#A. Read and view habitat raster
ringstrom_hab <- terra::rast("Ringstrom_Habitat.tif")
plot(ringstrom_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
ringstrom_shp <- read_sf("Ringstrom.shp")
plot(ringstrom_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(ringstrom_hab, ringstrom_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(ringstrom_hab)                                                                      #Check extent of the habitat raster
ext(ringstrom_shp)                                                                      #Check extent of the shapefile
ringstrom_shp <- st_transform(ringstrom_shp, crs(ringstrom_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
ringstrom_shp                                                                           #Check to make sure the transformation worked
ringstrom_habitat <- crop(ringstrom_hab, ringstrom_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
ringstrom_habitat <- mask(ringstrom_hab, ringstrom_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(ringstrom_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
ringstrom_perim <- lsm_p_perim(ringstrom_habitat)                                         #Calculate perimeter of all habitat patches
ringstrom_edge <- sum(ringstrom_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
ringstrom_patch <- lsm_c_np(ringstrom_habitat)                                            #Calculate number of habitat patches for each class
ringstrom_frag <- sum(ringstrom_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
ringstrom_cover <- lsm_c_pland(ringstrom_habitat)                                                       
ringstrom_cover <- as.data.frame(ringstrom_cover)
ringstrom_cover <- ringstrom_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Levee",
      class == 5 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
ringstrom_area <- lsm_c_tca(ringstrom_habitat)
ringstrom_area <- as.data.frame(ringstrom_area)
ringstrom_area <- ringstrom_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Tidal Pond/Panne",
      class == 4 ~ "Levee",
      class == 5 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
ringstrom_res <- as.data.frame(merge(ringstrom_cover, ringstrom_area, by = "Habitat Type"))
ringstrom_res <- ringstrom_res %>%
  mutate(
    Edge = ringstrom_edge,
    Number_Patches = ringstrom_frag,
    Marsh = "Ringstrom"
  )
ringstrom_res

#-------------------------------------------------------------------------------
#RUSH CREEK

#1. Import and manage files
#A. Read and view habitat raster
rushcreek_hab <- terra::rast("RushCreek_Habitat.tif")
plot(rushcreek_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
rushcreek_shp <- read_sf("RushCreek.shp")
plot(rushcreek_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(rushcreek_hab, rushcreek_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(rushcreek_hab)                                                                      #Check extent of the habitat raster
ext(rushcreek_shp)                                                                      #Check extent of the shapefile
rushcreek_shp <- st_transform(rushcreek_shp, crs(rushcreek_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
rushcreek_shp                                                                           #Check to make sure the transformation worked
rushcreek_habitat <- crop(rushcreek_hab, rushcreek_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
rushcreek_habitat <- mask(rushcreek_hab, rushcreek_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(rushcreek_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
rushcreek_perim <- lsm_p_perim(rushcreek_habitat)                                         #Calculate perimeter of all habitat patches
rushcreek_edge <- sum(rushcreek_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
rushcreek_patch <- lsm_c_np(rushcreek_habitat)                                            #Calculate number of habitat patches for each class
rushcreek_frag <- sum(rushcreek_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
rushcreek_cover <- lsm_c_pland(rushcreek_habitat)                                                       
rushcreek_cover <- as.data.frame(rushcreek_cover)
rushcreek_cover <- rushcreek_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Other Open Water",
      class == 2 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
rushcreek_area <- lsm_c_tca(rushcreek_habitat)
rushcreek_area <- as.data.frame(rushcreek_area)
rushcreek_area <- rushcreek_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Other Open Water",
      class == 2 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
rushcreek_res <- as.data.frame(merge(rushcreek_cover, rushcreek_area, by = "Habitat Type"))
rushcreek_res <- rushcreek_res %>%
  mutate(
    Edge = rushcreek_edge,
    Number_Patches = rushcreek_frag,
    Marsh = "Rush Creek"
  )
rushcreek_res

#-------------------------------------------------------------------------------
#SAN BRUNO CANAL

#1. Import and manage files
#A. Read and view habitat raster
sanbruno_hab <- terra::rast("SanBrunoCanal_Habitat.tif")
plot(sanbruno_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
sanbruno_shp <- read_sf("SanBrunoCanal.shp")
plot(sanbruno_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(sanbruno_hab, sanbruno_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(sanbruno_hab)                                                                      #Check extent of the habitat raster
ext(sanbruno_shp)                                                                      #Check extent of the shapefile
sanbruno_shp <- st_transform(sanbruno_shp, crs(sanbruno_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
sanbruno_shp                                                                           #Check to make sure the transformation worked
sanbruno_habitat <- crop(sanbruno_hab, sanbruno_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
sanbruno_habitat <- mask(sanbruno_hab, sanbruno_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(sanbruno_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
sanbruno_perim <- lsm_p_perim(sanbruno_habitat)                                         #Calculate perimeter of all habitat patches
sanbruno_edge <- sum(sanbruno_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
sanbruno_patch <- lsm_c_np(sanbruno_habitat)                                            #Calculate number of habitat patches for each class
sanbruno_frag <- sum(sanbruno_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
sanbruno_cover <- lsm_c_pland(sanbruno_habitat)                                                       
sanbruno_cover <- as.data.frame(sanbruno_cover)
sanbruno_cover <- sanbruno_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Tidal Flat",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Low Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
sanbruno_area <- lsm_c_tca(sanbruno_habitat)
sanbruno_area <- as.data.frame(sanbruno_area)
sanbruno_area <- sanbruno_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Tidal Flat",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Low Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
sanbruno_res <- as.data.frame(merge(sanbruno_cover, sanbruno_area, by = "Habitat Type"))
sanbruno_res <- sanbruno_res %>%
  mutate(
    Edge = sanbruno_edge,
    Number_Patches = sanbruno_frag,
    Marsh = "San Bruno Canal"
  )
sanbruno_res

#-------------------------------------------------------------------------------
#SAN MATEO BRIDGE

#1. Import and manage files
#A. Read and view habitat raster
sanmateo_hab <- terra::rast("SanMateoBridge_Habitat.tif")
plot(sanmateo_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
sanmateo_shp <- read_sf("SanMateoBridge.shp")
plot(sanmateo_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(sanmateo_hab, sanmateo_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(sanmateo_hab)                                                                      #Check extent of the habitat raster
ext(sanmateo_shp)                                                                      #Check extent of the shapefile
sanmateo_shp <- st_transform(sanmateo_shp, crs(sanmateo_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
sanmateo_shp                                                                           #Check to make sure the transformation worked
sanmateo_habitat <- crop(sanmateo_hab, sanmateo_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
sanmateo_habitat <- mask(sanmateo_hab, sanmateo_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(sanmateo_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
sanmateo_perim <- lsm_p_perim(sanmateo_habitat)                                         #Calculate perimeter of all habitat patches
sanmateo_edge <- sum(sanmateo_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
sanmateo_patch <- lsm_c_np(sanmateo_habitat)                                            #Calculate number of habitat patches for each class
sanmateo_frag <- sum(sanmateo_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
sanmateo_cover <- lsm_c_pland(sanmateo_habitat)                                                       
sanmateo_cover <- as.data.frame(sanmateo_cover)
sanmateo_cover <- sanmateo_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Tidal Pond/Panne",
      class == 3 ~ "Beach",
      class == 4 ~ "Low Marsh",
      class == 5 ~ "Intertidal Channel",
      class == 6 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
sanmateo_area <- lsm_c_tca(sanmateo_habitat)
sanmateo_area <- as.data.frame(sanmateo_area)
sanmateo_area <- sanmateo_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Tidal Pond/Panne",
      class == 3 ~ "Beach",
      class == 4 ~ "Low Marsh",
      class == 5 ~ "Intertidal Channel",
      class == 6 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
sanmateo_res <- as.data.frame(merge(sanmateo_cover, sanmateo_area, by = "Habitat Type"))
sanmateo_res <- sanmateo_res %>%
  mutate(
    Edge = sanmateo_edge,
    Number_Patches = sanmateo_frag,
    Marsh = "San Mateo Bridge"
  )
sanmateo_res

#-------------------------------------------------------------------------------
#SAN PABLO CREEK

#1. Import and manage files
#A. Read and view habitat raster
sanpablo_hab <- terra::rast("SanPabloCreek_Habitat.tif")
plot(sanpablo_hab, legend = "bottomright", plg = list(size=1, cex=1))

#B. Remove raster background 
#1) Read and view wetland shapefile
sanpablo_shp <- read_sf("SanPabloCreek.shp")
plot(sanpablo_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(sanpablo_hab, sanpablo_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(sanpablo_hab)                                                                      #Check extent of the habitat raster
ext(sanpablo_shp)                                                                      #Check extent of the shapefile
sanpablo_shp <- st_transform(sanpablo_shp, crs(sanpablo_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
sanpablo_shp                                                                           #Check to make sure the transformation worked
sanpablo_habitat <- crop(sanpablo_hab, sanpablo_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
sanpablo_habitat <- mask(sanpablo_hab, sanpablo_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(sanpablo_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
sanpablo_perim <- lsm_p_perim(sanpablo_habitat)                                         #Calculate perimeter of all habitat patches
sanpablo_edge <- sum(sanpablo_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
sanpablo_patch <- lsm_c_np(sanpablo_habitat)                                            #Calculate number of habitat patches for each class
sanpablo_frag <- sum(sanpablo_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
sanpablo_cover <- lsm_c_pland(sanpablo_habitat)                                                       
sanpablo_cover <- as.data.frame(sanpablo_cover)
sanpablo_cover <- sanpablo_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Levee",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Low Marsh",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Tidal Pond/Panne",
      class == 6 ~ "Muted Tidal Marsh",
      class == 7 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
sanpablo_area <- lsm_c_tca(sanpablo_habitat)
sanpablo_area <- as.data.frame(sanpablo_area)
sanpablo_area <- sanpablo_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Levee",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Low Marsh",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Tidal Pond/Panne",
      class == 6 ~ "Muted Tidal Marsh",
      class == 7 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
sanpablo_res <- as.data.frame(merge(sanpablo_cover, sanpablo_area, by = "Habitat Type"))
sanpablo_res <- sanpablo_res %>%
  mutate(
    Edge = sanpablo_edge,
    Number_Patches = sanpablo_frag,
    Marsh = "San Pablo Creek"
  )
sanpablo_res

#-------------------------------------------------------------------------------
#SNAG ISLAND

#1. Import and manage files
#A. Read and view habitat raster
snag_hab <- terra::rast("SnagIsland_Habitat.tif")
plot(snag_hab, legend = "topright", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
snag_shp <- read_sf("SnagIsland.shp")
plot(snag_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(snag_hab, snag_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(snag_hab)                                                                      #Check extent of the habitat raster
ext(snag_shp)                                                                      #Check extent of the shapefile
snag_shp <- st_transform(snag_shp, crs(snag_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
snag_shp                                                                           #Check to make sure the transformation worked
snag_habitat <- crop(snag_hab, snag_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
snag_habitat <- mask(snag_hab, snag_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(snag_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
snag_perim <- lsm_p_perim(snag_habitat)                                         #Calculate perimeter of all habitat patches
snag_edge <- sum(snag_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
snag_patch <- lsm_c_np(snag_habitat)                                            #Calculate number of habitat patches for each class
snag_frag <- sum(snag_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
snag_cover <- lsm_c_pland(snag_habitat)                                                       
snag_cover <- as.data.frame(snag_cover)
snag_cover <- snag_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "Tidal Flat",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Tidal Pond/Panne",
      class == 6 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
snag_area <- lsm_c_tca(snag_habitat)
snag_area <- as.data.frame(snag_area)
snag_area <- snag_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "High Marsh",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "Tidal Flat",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Tidal Pond/Panne",
      class == 6 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
snag_res <- as.data.frame(merge(snag_cover, snag_area, by = "Habitat Type"))
snag_res <- snag_res %>%
  mutate(
    Edge = snag_edge,
    Number_Patches = snag_frag,
    Marsh = "Snag Island"
  )
snag_res

#-------------------------------------------------------------------------------
#STEINBERGER SLOUGH

#1. Import and manage files
#A. Read and view habitat raster
steinberger_hab <- terra::rast("SteinbergerSlough_Habitat.tif")
plot(steinberger_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
steinberger_shp <- read_sf("SteinbergerSlough.shp")
plot(steinberger_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(steinberger_hab, steinberger_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(steinberger_hab)                                                                      #Check extent of the habitat raster
ext(steinberger_shp)                                                                      #Check extent of the shapefile
steinberger_shp <- st_transform(steinberger_shp, crs(steinberger_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
steinberger_shp                                                                           #Check to make sure the transformation worked
steinberger_habitat <- crop(steinberger_hab, steinberger_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
steinberger_habitat <- mask(steinberger_hab, steinberger_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(steinberger_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
steinberger_perim <- lsm_p_perim(steinberger_habitat)                                         #Calculate perimeter of all habitat patches
steinberger_edge <- sum(steinberger_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
steinberger_patch <- lsm_c_np(steinberger_habitat)                                            #Calculate number of habitat patches for each class
steinberger_frag <- sum(steinberger_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
steinberger_cover <- lsm_c_pland(steinberger_habitat)                                                       
steinberger_cover <- as.data.frame(steinberger_cover)
steinberger_cover <- steinberger_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Tidal Pond/Panne",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Levee",
      class == 6 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
steinberger_area <- lsm_c_tca(steinberger_habitat)
steinberger_area <- as.data.frame(steinberger_area)
steinberger_area <- steinberger_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Tidal Pond/Panne",
      class == 2 ~ "Low Marsh",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Intertidal Channel",
      class == 5 ~ "Levee",
      class == 6 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
steinberger_res <- as.data.frame(merge(steinberger_cover, steinberger_area, by = "Habitat Type"))
steinberger_res <- steinberger_res %>%
  mutate(
    Edge = steinberger_edge,
    Number_Patches = steinberger_frag,
    Marsh = "Steinberger Slough"
  )
steinberger_res

#-------------------------------------------------------------------------------
#STRIP MARSH EAST

#1. Import and manage files
#A. Read and view habitat raster
stripmarsh_hab <- terra::rast("StripMarshEast_Habitat.tif")
plot(stripmarsh_hab, legend = "bottomleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
stripmarsh_shp <- read_sf("StripMarshEast.shp")
plot(stripmarsh_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(stripmarsh_hab, stripmarsh_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(stripmarsh_hab)                                                                      #Check extent of the habitat raster
ext(stripmarsh_shp)                                                                      #Check extent of the shapefile
stripmarsh_shp <- st_transform(stripmarsh_shp, crs(stripmarsh_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
stripmarsh_shp                                                                           #Check to make sure the transformation worked
stripmarsh_habitat <- crop(stripmarsh_hab, stripmarsh_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
stripmarsh_habitat <- mask(stripmarsh_hab, stripmarsh_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(stripmarsh_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
stripmarsh_perim <- lsm_p_perim(stripmarsh_habitat)                                         #Calculate perimeter of all habitat patches
stripmarsh_edge <- sum(stripmarsh_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
stripmarsh_patch <- lsm_c_np(stripmarsh_habitat)                                            #Calculate number of habitat patches for each class
stripmarsh_frag <- sum(stripmarsh_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
stripmarsh_cover <- lsm_c_pland(stripmarsh_habitat)                                                       
stripmarsh_cover <- as.data.frame(stripmarsh_cover)
stripmarsh_cover <- stripmarsh_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Tidal Flat",
      class == 2 ~ "Levee",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 5 ~ "Tidal Pond/Panne",
      class == 6 ~ "Low Marsh",
      class == 7 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
stripmarsh_area <- lsm_c_tca(stripmarsh_habitat)
stripmarsh_area <- as.data.frame(stripmarsh_area)
stripmarsh_area <- stripmarsh_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Tidal Flat",
      class == 2 ~ "Levee",
      class == 3 ~ "High Marsh",
      class == 4 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 5 ~ "Tidal Pond/Panne",
      class == 6 ~ "Low Marsh",
      class == 7 ~ "Intertidal Channel",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
stripmarsh_res <- as.data.frame(merge(stripmarsh_cover, stripmarsh_area, by = "Habitat Type"))
stripmarsh_res <- stripmarsh_res %>%
  mutate(
    Edge = stripmarsh_edge,
    Number_Patches = stripmarsh_frag,
    Marsh = "Strip Marsh East"
  )
stripmarsh_res

#-------------------------------------------------------------------------------
#TISCORNIA MARSH

#1. Import and manage files
#A. Read and view habitat raster
tiscornia_hab <- terra::rast("TiscorniaMarsh_Habitat.tif")
plot(tiscornia_hab, legend = "bottomleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
tiscornia_shp <- read_sf("TiscorniaMarsh.shp")
plot(tiscornia_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(tiscornia_hab, tiscornia_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(tiscornia_hab)                                                                      #Check extent of the habitat raster
ext(tiscornia_shp)                                                                      #Check extent of the shapefile
tiscornia_shp <- st_transform(tiscornia_shp, crs(tiscornia_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
tiscornia_shp                                                                           #Check to make sure the transformation worked
tiscornia_habitat <- crop(tiscornia_hab, tiscornia_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
tiscornia_habitat <- mask(tiscornia_hab, tiscornia_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(tiscornia_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
tiscornia_perim <- lsm_p_perim(tiscornia_habitat)                                         #Calculate perimeter of all habitat patches
tiscornia_edge <- sum(tiscornia_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
tiscornia_patch <- lsm_c_np(tiscornia_habitat)                                            #Calculate number of habitat patches for each class
tiscornia_frag <- sum(tiscornia_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
tiscornia_cover <- lsm_c_pland(tiscornia_habitat)                                                       
tiscornia_cover <- as.data.frame(tiscornia_cover)
tiscornia_cover <- tiscornia_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Intertidal Channel",
      class == 3 ~ "Levee",
      class == 4 ~ "High Marsh",
      class == 5 ~ "Tidal Flat",
      class == 6 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
tiscornia_area <- lsm_c_tca(tiscornia_habitat)
tiscornia_area <- as.data.frame(tiscornia_area)
tiscornia_area <- tiscornia_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Intertidal Channel",
      class == 3 ~ "Levee",
      class == 4 ~ "High Marsh",
      class == 5 ~ "Tidal Flat",
      class == 6 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
tiscornia_res <- as.data.frame(merge(tiscornia_cover, tiscornia_area, by = "Habitat Type"))
tiscornia_res <- tiscornia_res %>%
  mutate(
    Edge = tiscornia_edge,
    Number_Patches = tiscornia_frag,
    Marsh = "Tiscornia"
  )
tiscornia_res

#-------------------------------------------------------------------------------
#WEST FAMILY

#1. Import and manage files
#A. Read and view habitat raster
westfamily_hab <- terra::rast("WestFamily_Habitat.tif")
plot(westfamily_hab, legend = "bottomleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
westfamily_shp <- read_sf("WestFamily.shp")
plot(westfamily_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(westfamily_hab, westfamily_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(westfamily_hab)                                                                      #Check extent of the habitat raster
ext(westfamily_shp)                                                                      #Check extent of the shapefile
westfamily_shp <- st_transform(westfamily_shp, crs(westfamily_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
westfamily_shp                                                                           #Check to make sure the transformation worked
westfamily_habitat <- crop(westfamily_hab, westfamily_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
westfamily_habitat <- mask(westfamily_hab, westfamily_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(westfamily_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
westfamily_perim <- lsm_p_perim(westfamily_habitat)                                         #Calculate perimeter of all habitat patches
westfamily_edge <- sum(westfamily_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
westfamily_patch <- lsm_c_np(westfamily_habitat)                                            #Calculate number of habitat patches for each class
westfamily_frag <- sum(westfamily_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
westfamily_cover <- lsm_c_pland(westfamily_habitat)                                                       
westfamily_cover <- as.data.frame(westfamily_cover)
westfamily_cover <- westfamily_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Developed/Urban",
      class == 2 ~ "Levee",
      class == 3 ~ "Other Open Water",
      class == 4 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
westfamily_area <- lsm_c_tca(westfamily_habitat)
westfamily_area <- as.data.frame(westfamily_area)
westfamily_area <- westfamily_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Developed/Urban",
      class == 2 ~ "Levee",
      class == 3 ~ "Other Open Water",
      class == 4 ~ "Managed Marsh",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
westfamily_res <- as.data.frame(merge(westfamily_cover, westfamily_area, by = "Habitat Type"))
westfamily_res <- westfamily_res %>%
  mutate(
    Edge = westfamily_edge,
    Number_Patches = westfamily_frag,
    Marsh = "West Family"
  )
westfamily_res

#-------------------------------------------------------------------------------
#WEST SONOMA CREEK

#1. Import and manage files
#A. Read and view habitat raster
westsonoma_hab <- terra::rast("WestSonomaCreek_Habitat.tif")
plot(westsonoma_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
westsonoma_shp <- read_sf("WestSonomaCreek.shp")
plot(westsonoma_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(westsonoma_hab, westsonoma_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(westsonoma_hab)                                                                      #Check extent of the habitat raster
ext(westsonoma_shp)                                                                      #Check extent of the shapefile
westsonoma_shp <- st_transform(westsonoma_shp, crs(westsonoma_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
westsonoma_shp                                                                           #Check to make sure the transformation worked
westsonoma_habitat <- crop(westsonoma_hab, westsonoma_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
westsonoma_habitat <- mask(westsonoma_hab, westsonoma_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(westsonoma_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
westsonoma_perim <- lsm_p_perim(westsonoma_habitat)                                         #Calculate perimeter of all habitat patches
westsonoma_edge <- sum(westsonoma_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
westsonoma_patch <- lsm_c_np(westsonoma_habitat)                                            #Calculate number of habitat patches for each class
westsonoma_frag <- sum(westsonoma_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
westsonoma_cover <- lsm_c_pland(westsonoma_habitat)                                                       
westsonoma_cover <- as.data.frame(westsonoma_cover)
westsonoma_cover <- westsonoma_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Tidal Pond/Panne",
      class == 3 ~ "Intertidal Channel",
      class == 4 ~ "High Marsh",
      class == 5 ~ "Levee",
      class == 6 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 7 ~ "Tidal Flat",
      class == 8 ~ "Managed Marsh",
      class == 9 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
westsonoma_area <- lsm_c_tca(westsonoma_habitat)
westsonoma_area <- as.data.frame(westsonoma_area)
westsonoma_area <- westsonoma_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "Tidal Pond/Panne",
      class == 3 ~ "Intertidal Channel",
      class == 4 ~ "High Marsh",
      class == 5 ~ "Levee",
      class == 6 ~ "Undetermined Non-Aquatic Diked Bayland",
      class == 7 ~ "Tidal Flat",
      class == 8 ~ "Managed Marsh",
      class == 9 ~ "Shallow Subtidal",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
westsonoma_res <- as.data.frame(merge(westsonoma_cover, westsonoma_area, by = "Habitat Type"))
westsonoma_res <- westsonoma_res %>%
  mutate(
    Edge = westsonoma_edge,
    Number_Patches = westsonoma_frag,
    Marsh = "West Sonoma Creek"
  )
westsonoma_res

#-------------------------------------------------------------------------------
#WHITE SLOUGH

#1. Import and manage files
#A. Read and view habitat raster
white_hab <- terra::rast("WhiteSlough_Habitat.tif")
plot(white_hab, legend = "topleft", plg = list(size=1, cex=1.2))

#B. Remove raster background 
#1) Read and view wetland shapefile
white_shp <- read_sf("WhiteSlough.shp")
plot(white_shp$geometry)   

#C. Crop the raster to the extent of the shapefile
hab <- crop(white_hab, white_shp)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(white_hab)                                                                      #Check extent of the habitat raster
ext(white_shp)                                                                      #Check extent of the shapefile
white_shp <- st_transform(white_shp, crs(white_hab))                            #If extents don't match, transform the shapefile extent to match that of the raster
white_shp                                                                           #Check to make sure the transformation worked
white_habitat <- crop(white_hab, white_shp)                                     #Crop the raster to the extent of the shapefile

#D. Mask the cropped raster using the shapefile
white_habitat <- mask(white_hab, white_shp)

#----------
#2. Check raster requirements                                                         #Distance must be in meters, classes as integer, classified landscape
check_landscape(white_habitat)

#----------
#3. Calculate landscape metrics
#A. Edge (m)
white_perim <- lsm_p_perim(white_habitat)                                         #Calculate perimeter of all habitat patches
white_edge <- sum(white_perim$value)                                              #Sum patch perimeters to get total edge

#B. Number of habitat patches
white_patch <- lsm_c_np(white_habitat)                                            #Calculate number of habitat patches for each class
white_frag <- sum(white_patch$value)                                              #Sum to get total number of habitat patches within the raster

#C. Percent coverage for each habitat class
white_cover <- lsm_c_pland(white_habitat)                                                       
white_cover <- as.data.frame(white_cover)
white_cover <- white_cover %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Intertidal Channel",
      class == 4 ~ "Levee",
      class == 5 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Percent" = value
  )

#D. Total class area (hectares)
white_area <- lsm_c_tca(white_habitat)
white_area <- as.data.frame(white_area)
white_area <- white_area %>%
  select(class, value) %>%
  mutate(
    class = case_when(
      class == 1 ~ "Low Marsh",
      class == 2 ~ "High Marsh",
      class == 3 ~ "Intertidal Channel",
      class == 4 ~ "Levee",
      class == 5 ~ "Tidal Flat",
      TRUE ~ as.character(class)
    )
  ) %>%
  rename(
    "Habitat Type" = class,
    "Hectares" = value
  )

#----------
#4. Merge all metrics into one data frame
white_res <- as.data.frame(merge(white_cover, white_area, by = "Habitat Type"))
white_res <- white_res %>%
  mutate(
    Edge = white_edge,
    Number_Patches = white_frag,
    Marsh = "White Slough"
  )
white_res


#-------------------------------------------------------------------------------
#DATA MANAGEMENT

#A. Combine all results into one dataframe
results <- as.data.frame(rbind(almonte_res, alto_res, alviso_triangle_res, amer_canyon_res, arrowhead_res, bayfront_res, bay_point_res,
                         belmont_res, benicia_ind_res, bothin_res, burdell_res, byxbee_res, china_camp_res, corte_madera_res,
                         creekside_res, cullinan_res, dixon_res, dotson_res, enterprise_res, flood_control_res, freeman_res,
                         giant_res, goodyear_res, guadalcanal_res, hard_res, haystack_res, hayward_res, headalbrae_res, heerdt_res,
                         hercules_res, hillslough_res, ideal_res, innerbair_res, joice_res, mallard_res, mariner_res, marta_res,
                         martinez_res, mclaughlin_res, mcnabney_res, mlk_res, mtview_res, mundy_res, munster_res, muzzin_res, muzzis_res,
                         newark_res, newchicago_res, novato_res, oakland_res, piperpark_res, plummer_res, ptpinole_res, profcircle_res,
                         radiopt_res, ravenswood_res, renzel_res, ringstrom_res, rushcreek_res, sanbruno_res, sanmateo_res, sanpablo_res,
                         snag_res, steinberger_res, stripmarsh_res, tiscornia_res, westfamily_res, westsonoma_res, white_res)
                %>% mutate(across(where(is.numeric), ~ round(., digits = 3))))

#B. Export to CSV
write.csv(results, row.names = TRUE, file = "Habitat Fragmentation Analysis.csv")
