# Description ---------------------------------------------------------------------------------------------------------
# This document extracts and saves a number of SST data products downloaded from NASA's Eardata downloader service. 
# It also creates a "typical year" at the surface and at 1m, ready to be used later.

# Setup ---------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(here)
library(purrr)
library(ncdf4)
library(tictoc)

here("src", "dirs.R") %>% source()
here("src", "functions.R") %>% source()

# Dataset extract function --------------------------------------------------------------------------------------------
# An extraction function for this dataset specifically
extract_raster <- function(filename, varname, day_offset = 0, destpath, destfile_prefix, overwrite = T, time = F) {
  nc <- nc_open(filename)
  reftime <- nc$dim$time$units %>% 
    str_extract(., "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}") %>% 
    ymd_hms() %>% 
    as.Date()
  formatted_date <- (reftime + duration(nc$dim$time$vals, units = "seconds") - duration(day_offset, units = "days")) %>%
    as.Date() %>% 
    as.character()
  nc_close(nc)

  # Check if file exists
  output_filename <- file.path(destpath, paste0(destfile_prefix, formatted_date, ".tif"))
  if (!file.exists(output_filename) | overwrite) {
    sst <- terra::rast(filename, lyrs = varname)
    terra::ext(sst) <- c(-180, 180, -90, 90)
    
    # Change offset metadata to convert to celcius
    sc <- scoff(sst)
    sc[, 'offset'] <- sc[, 'offset'] - 273.15
    scoff(sst) <- sc
    units(sst) <- "celsius"

    names(sst) <- formatted_date

    # Save file
    terra::writeRaster(
      sst, 
      output_filename,
      datatype = "FLT4S",
      gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=1"), # compression for space
      overwrite = TRUE
    )
  }
}

# Extract and prep global SST -----------------------------------------------------------------------------------------
files <- file.path(bigdata_path, "MUR25-JPL-L4-GLOB-v04.2_4.2", "raw_ncs") %>% 
  list.files(full.names = TRUE, recursive = T) %>% 
  sort() %>% 
  str_subset(paste0(paste0("/", 2014:2025), collapse = "|"))
# length(files)

# Make sure directory exists
destpath <- file.path(bigdata_path, "MUR25-JPL-L4-GLOB-v04.2_4.2", "extracted")
dir.create(destpath, showWarnings = F)

# Extract and save
walk(
  .x = files,
  .f = extract_raster,
  varname = "analysed_sst",
  day_offset = 1,
  destpath = destpath,
  destfile_prefix = "analysed-SST-MUR25-v4.2_",
  overwrite = F,
  .progress = T
)

# Create mean year 2015-2024 ------------------------------------------------------------------------------------------
files <- file.path(bigdata_path, "MUR25-JPL-L4-GLOB-v04.2_4.2", "extracted") %>% 
  list.files(full.names = TRUE, recursive = T) %>% 
  sort() %>% 
  str_subset(paste0(2015:2024, collapse = "|"))

files_by_doy <- by_doy(files)

# Make sure directory exists
destpath <- file.path(bigdata_path, "MUR25-JPL-L4-GLOB-v04.2_4.2", "meanyears")
dir.create(destpath, showWarnings = F)

# Loop through each DOY and create average SST raster
for (i in seq_len(nrow(files_by_doy))) {
  current_doy <- files_by_doy$doy_adjusted[i]
  current_date <- files_by_doy$month_day[i]
  current_files <- files_by_doy$filenamess[[i]]
  
  cat("\nProcessing DOY", current_doy, "(", current_date, ") - ", length(current_files), "files\n")
  
  raster_stack <- rast(current_files)
  mean_raster <- mean(raster_stack, na.rm = T)

  # # Gapfill raster - WARNING: will give some land cells SST!
  # mean_gf_raster <- focal(mean_raster, w = 7, fun = "mean", na.policy = "only")

  output_filename <- file.path(destpath, paste0("analysed-SST-MUR25-v4.2_meanyear_2015_2024_", current_date, ".tif"))
  writeRaster(
    mean_raster, 
    output_filename,
    datatype = "FLT4S",
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=3"), # compression for space
    overwrite = TRUE
  )
}
