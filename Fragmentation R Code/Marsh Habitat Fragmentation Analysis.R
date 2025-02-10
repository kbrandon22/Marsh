library(landscapemetrics)
library(terra)
library(sf)
library(tidyr)
library(raster)
library(future)
library(future.apply)

#Set working directory
setwd("C:\\Users\\Kristin\\Documents\\SMHM Project\\Data Analysis\\ArcGIS Pro")

#-------------------------------------------------------------------------------
#IMPORT AND MANAGE FILES

#1. Read and view habitat raster
habitat <- terra::rast("PiperPark_Habitat.tif")
habitat
plot(habitat, legend = "topleft", plg = list(size=1, cex=1.2))

#2. Remove raster background 
#A. Read and view wetland shapefile
PiperPark <- read_sf("PiperPark.shp")
plot(PiperPark$geometry)   

#B. Crop the raster to the extent of the shapefile
hab <- crop(habitat, PiperPark)     

#NOTE: Crop may not function if the extent of the shapefile does not match that of the raster exactly
#To ensure the extents match, run the following five lines - skip if crop works
ext(habitat)                                                                        #Check extent of the habitat raster
ext(PiperPark)                                                                      #Check extent of the shapefile
PiperPark <- st_transform(PiperPark, crs(habitat))                                  #If extents don't match, transform the shapefile extent to match that of the raster
PiperPark                                                                           #Check to make sure the transformation worked
hab <- crop(habitat, PiperPark)                                                     #Crop the raster to the extent of the shapefile

#C. Mask the cropped raster using the shapefile
habitat <- mask(hab, PiperPark)
plot(habitat)

#3. Check raster requirements                                                      #Distance must be in meters, classes as integer, classified landscape
check_landscape(habitat)

#-------------------------------------------------------------------------------
#CALCULATE LANDSCAPE METRICS

#1. Edge (m)
perim <- lsm_p_perim(habitat)                                                       #Calculate perimeter of all habitat patches
edge <- sum(perim$value)                                                            #Sum patch perimeters to get total edge
edge

#2. Number of habitat patches
patch <- lsm_c_np(habitat)                                                          #Calculate number of habitat patches for each class
frag <- sum(patch$value)                                                            #Sum to get total number of habitat patches within the raster
frag

#3. Percent coverage for each habitat class
cover <- lsm_c_pland(habitat)                                                       
cover

#4. Total class area (hectares)
area <- lsm_c_tca(habitat)
area

#-------------------------------------------------------------------------------
#LOOP STEPS TO ITERATE ACROSS ALL RASTERS

#1. Define directory
direct <- "C:\\Users\\Kristin\\Documents\\SMHM Project\\Data Analysis\\ArcGIS Pro"

#----------
#2. Manage files
#A. List all raster files in the directory and read into a list
rasters <- list.files(path = direct, pattern = "\\.tif$", full.names = TRUE)
raster_list <- lapply(rasters, rast)
names(raster_list) <- basename(rasters)
raster_list

#B. List all wetland shapefiles and read into a list
shapes <- list.files(path = direct, pattern = "\\.shp$", full.names = TRUE)
shapes_list <- lapply(shapes, read_sf)
names(shapes_list) <- basename(shapes)
shapes_list

#----------
#3. Define the functions 
#A. Crop raster to shapefile, with optional extent transformation
crop_loop <- function(raster, shapefile){                                              
  if(!identical(ext(raster), st_bbox(shapefile))){
    shapefile <- st_transform(shapefile, crs(raster))
  }
    cropped_raster <- terra::crop(raster, shapefile)
    return(cropped_raster)
}

#B. Mask the cropped raster using the shapefile
mask_loop <- function(raster, shapefile){                                            
  masked_raster <- terra::mask(raster, shapefile)
  return(masked_raster)
}

#C. Check raster requirements
check_loop <- function(raster){                                                          
  check_landscape(raster)
}

#D. Calculate the perimeter of all habitat patches
calc_perim <- function(raster){                                                         
  perim_metrics <- lsm_p_perim(raster)
  return(perim_metrics)
}

#E. Sum perimeter of all habitat patches
sum_edge <- function(perim_metrics){                                                         
  total_edge <- sum(perim_metrics$value)
  return(total_edge)
}

#F. Calculate number of habitat patches
calc_patch <- function(raster){                                                         
  patch_metrics <- lsm_c_np(raster)
  return(patch_metrics)
}

#G. Sum number of habitat patches
sum_frag <- function(patch_metrics){                                                         
  total_frag <- sum(patch_metrics$value)
  return(total_frag)
}

#H. Calculate percent coverage of each habitat class
calc_cover <- function(raster){                                                          
  cover_metrics <- lsm_c_pland(raster)
  return(cover_metrics)
}

#I. Calculate total area of each habitat class
calc_area <- function(raster){                                                           
  area_metrics <- lsm_c_tca(raster)
  return(area_metrics)
}

#----------
#5. Process batches
#A. Create the function that applies the defined functions above to process batches
process_batch <- function(rasters_batch, shapes_batch){
  results_batch <- list()
  
  for(i in seq_along(rasters_batch)){
    raster <- rasters_batch[[i]]
    shapefile <- shapes_batch[[i]]
    
    cropped_raster <- crop_loop(raster, shapefile)
    masked_raster <- mask_loop(cropped_raster, shapefile)
    check_loop(masked_raster)
    
    perim_metrics <- calc_perim(masked_raster)
    total_edge <- sum_edge(perim_metrics)
    
    patch_metrics <- calc_patch(masked_raster)
    total_frag <- sum_frag(patch_metrics)
    
    cover_metrics <- calc_cover(masked_raster)
    area_metrics <- calc_area(masked_raster)
  
    results_batch[[basename(rasters_batch[i])]] <- list(
      edge = total_edge,
      frag = total_frag,
      cover = cover_metrics,
      area = area_metrics
    )
  }
  return(results_batch)
}

#B. Set up parallel processing
plan(multisession, workers = availableCores())

#C. Define batch size
batch <- 20

#D. Split rasters and shapefiles into batches
split_batches <- function(files, batch){
  split(files, ceiling(seq_along(files)/batch))
}

rasters_batch <- split_batches(raster_list, batch)
shapes_batch <- split_batches(shapes_list, batch)

#C. Create list to store results
results <- list()

#D. Loop through each batch in parallel
for(batch_idx in seq_along(rasters_batch)){
  raster_batch <- rasters_batch[[batch_idx]]
  shape_batch <- shapes_batch[[batch_idx]]
  
  future_result <- future({
    process_batch(raster_batch, shape_batch)
  })
  
  batch_results <- value(future_result)
  results <- c(results, batch_results)
}

#E. Combine all results
results_df <- do.call(rbind, lapply(results, function(res) {
  data.frame(
    edge = res$edge,
    frag = res$frag,
    cover = I(list(res$cover)),
    area = I(list(res$area))
  )
}))

#F. View results
print(results)

#G. Export results as a CSV
write.csv(results_df, "C:\\Users\\Kristin\\Documents\\SMHM Project\\Data Analysis\\ArcGIS Pro\\Patch fragmentation metrics.csv", row.names = TRUE)






#2) Convert habitat classes to categorical values
unique_values <- unique(unlist(lapply(raster_list, values)))
print(unique_values)

lookup_table <- data.frame(
  value = unique_values,
  category = c("Beach", "Dune", "Intertidal Channel", "Levee", "Urban", 
               "Non-Aquatic Diked Bayland", "Managed Marsh", "Muted Tidal Marsh",
               "Undetermined Other Marsh", "Other Open Water", "Shallow Subtidal",
               "Tidal Flat", "High Marsh", "Low Marsh", "Tidal Pond/Panne")
)
convert_to_categorical <- function(raster, lookup_table){
  values(habitat) <- factor(values(habitat), levels=lookup_table$value, labels = lookup_table$category)
  levels(raster) <- data.frame(
    value = lookup_table$value,
    category = lookup_table$category
  )
  return(raster)
}

categorical_rasters <- lapply(raster_list, convert_to_categorical, lookup_table = lookup_table)
plot(categorical_rasters[[1]])

