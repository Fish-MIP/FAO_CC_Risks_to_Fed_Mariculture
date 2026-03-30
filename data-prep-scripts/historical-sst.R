# Description ----------------------------------------------------------------------------------------------
# This document extracts and saves a number of SST data products downloaded from NASA's Eardata downloader service. 
# It also creates a "typical year" at the surface and at 1m, ready to be used later.

library(tidyverse)
library(terra)
library(here)
library(purrr)
library(ncdf4)
library(tictoc)

here("src", "dirs.R") %>% source()
here("src", "functions.R") %>% source()

# Functions ------------------------------------------------------------------------------------------------
# A generic extraction function
extract_raster <- function(filename, varname, day_offset = 1, convert = T, destpath, destfile_prefix, overwrite = T) {
  tic()
  dir.create(destpath, showWarnings = F, recursive = T)

  nc <- nc_open(filename)
  reftime <- nc$dim$time$units %>% 
    str_extract(., "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}") %>% 
    ymd_hms() %>% 
    as.Date()
  formatted_date <- (reftime + duration(nc$dim$time$vals, units = "seconds") - duration(day_offset, units = "days")) %>% # times are at 9:00am, so label with the previous day
    as.Date() %>% 
    as.character()
  nc_close(nc)

  # Check if file exists
  output_filename <- file.path(destpath, paste0(destfile_prefix, formatted_date, ".tif"))
  if (!file.exists(output_filename) | overwrite) {
    sst <- terra::rast(filename, lyrs = varname)
    terra::ext(sst) <- c(-180, 180, -90, 90)
    if (convert) {sst <- sst - 273.15}
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
  cat(paste("\n", basename(output_filename), "saved,", toc(quiet = T)$callback_msg, "\n"))
}

# This takes a list of files and groups them by DOY (month-day)
by_doy <- function(files) {
  tibble(filenames = files) %>%
    mutate(
      date = str_extract(filenames, "\\d{4}-\\d{2}-\\d{2}") %>% as.Date(),
      doy = yday(date) %>% as.integer(),
      month_day = format(date, "%m-%d"),
      leap_year = leap_year(date)
    ) %>%
    mutate(
      # For leap years, Feb 29 gets converted to Feb 28
      month_day = if_else(month_day == "02-29", "02-28", month_day),
      doy_adjusted = case_when(
        leap_year & doy >= 60 ~ doy-1,
        T ~ doy
      ) %>% as.integer()
    ) %>%
    group_by(doy_adjusted, month_day) %>%
    summarise(
      filenamess = list(filenames),
      .groups = "drop"
    ) %>%
    arrange(doy_adjusted)
}

# Extract and prep -----------------------------------------------------------------------------------------
## General global SST --------------------------------------------------------------------------------------
files <- file.path(bigdata_path, "MUR-JPL-L4-GLOB-v4.1_4.1-20260202_235425") %>% 
  list.files(full.names = TRUE, recursive = T) %>% 
  sort()

# Extract and save
walk(
  .x = files,
  .f = extract_raster,
  varname = "analysed_sst",
  day_offset = 1,
  convert = F,
  destpath = file.path(prepdata_path, "SST"),
  destfile_prefix = "analysed-SST_",
  overwrite = F
)

## Global SST data at 1m depth -----------------------------------------------------------------------------
files <- file.path(bigdata_path, "K10_SST-NAVO-L4-GLOB-v01_1.0-20260203_035713") %>% 
  list.files(full.names = TRUE, recursive = T) %>% 
  sort()

walk(
  .x = files,
  .f = extract_raster,
  varname = "analysed_sst",
  day_offset = 1,
  convert = T,
  destpath = file.path(prepdata_path, "SST_1m"),
  destfile_prefix = "analysed-SST-1m_",
  overwrite = T
)

# Create typical year --------------------------------------------------------------------------------------
## General global SST --------------------------------------------------------------------------------------
destpath <- file.path(prepdata_path, "SST_meanyear")
dir.create(destpath, showWarnings = F, recursive = T)

files <- file.path(bigdata_path, "prepped_data", "SST") %>% 
  list.files(full.names = TRUE, recursive = T) %>% sort()

files_by_doy <- by_doy(files)

# Loop through each DOY and create average SST raster
for (i in seq_len(nrow(files_by_doy))) {
  current_doy <- files_by_doy$doy_adjusted[i]
  current_date <- files_by_doy$month_day[i]
  current_files <- files_by_doy$filenamess[[i]]
  
  cat("\nProcessing DOY", current_doy, "(", current_date, ") - ", length(current_files), "files\n")
  
  raster_stack <- rast(current_files)
  mean_raster <- mean(raster_stack)
  mean_raster <- mean_raster - 273.15 # Didn't convert during extraction because it took too long

  # Gapfill raster - will give some land cells SST!
  mean_gf_raster <- focal(mean_raster, w = 7, fun = "mean", na.policy = "only")

  output_filename <- file.path(destpath, paste0("analysed-SST_", current_date, ".tif"))
  writeRaster(
    mean_gf_raster, 
    output_filename,
    datatype = "FLT4S",
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=3"), # compression for space
    overwrite = TRUE
  )
}

## Global SST at 1m depth ----------------------------------------------------------------------------------
destpath <- file.path(prepdata_path, "SST_1m_meanyear")
dir.create(destpath, showWarnings = F, recursive = T)

files <- file.path(bigdata_path, "prepped_data", "SST_1m") %>% 
  list.files(full.names = TRUE, recursive = T) %>% 
  sort()

files_by_doy <- by_doy(files)

# Loop through each DOY and create average SST raster
for (i in seq_len(nrow(files_by_doy))) {
  current_doy <- files_by_doy$doy_adjusted[i]
  current_date <- files_by_doy$month_day[i]
  current_files <- files_by_doy$filenamess[[i]]
  
  cat("Processing DOY", current_doy, "(", current_date, ") - ", length(current_files), "files\n")
  
  raster_stack <- rast(current_files)
  mean_raster <- mean(raster_stack)
  # Don't need to convert, did it during extraction
  
  # Gapfill raster - will give some land cells SST!
  mean_gf_raster <- focal(mean_raster, w = 7, fun = "mean", na.policy = "only")

  output_filename <- file.path(destpath, paste0("analysed-SST_", current_date, ".tif"))
  writeRaster(
    mean_gf_raster, 
    output_filename,
    datatype = "FLT4S",
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=3"), # compression for space
    overwrite = TRUE
  )
}
