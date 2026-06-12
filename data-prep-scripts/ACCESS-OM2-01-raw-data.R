# For the Qian Li runs paul mentioned the paths on gadi are:
# •	Zonal wind + thermal + humidity + longwave ssp585 changes: /g/data/cj50/access-om2/raw-output/access-om2-01/01deg_jra55v13_ryf9091_qian_wthp
# •	The above + glacial melt changes: /g/data/cj50/access-om2/raw-output/access-om2-01/01deg_jra55v13_ryf9091_qian_wthmp
# •	And the parallel control is the same years of: /g/data/cj50/access-om2/raw-output/access-om2-01/01deg_jra55v13_ryf9091
# The paths should be the same on the nci threads (? Sorry never looked on there)

# The model years 2100 --> 2159 correspond to 1990 --> 2049 in the perturbation scenario. The wthp run branches off the repeat year forcing control at 2100 and the wthmp branches off the wthp at 2110. 

library(tidyverse)
library(terra)
library(ncdf4)
library(sf)
library(here)

here("src", "dirs.R") %>% source()
here("src", "functions.R") %>% source()

ncs_list <- file.path(bigdata_path, "ACCESS-OM2-01", "qian_raw_outputs", "monthly_ncs") %>% 
  list.files(full.names = T, recursive = T) %>% 
  sort()
grid_file <- file.path(bigdata_path, "ACCESS-OM2-01", "qian_raw_outputs", "ocean_grid_1035.nc")

tif_nm_prefix <- "surface-temp_"
overwrite <- F

# Get the grid
grid <- nc_open(grid_file)              # surface_temp is on [xt_ocean,yt_ocean,time]
lon  <- ncvar_get(grid, "geolon_t")
lat  <- ncvar_get(grid, "geolat_t")
nc_close(grid)
rm(grid)
grid_template <- rast(resolution = 0.1, xmin = -180, xmax = 180, ymin = -90, ymax = 90, crs = "EPSG:4326")


# nc_file <- ncs_list[1]
for (nc_file in ncs_list) {
  nc <- nc_open(nc_file)

  # Get times
  time_raw  <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value  
  # "days since 1900-01-01 00:00:00"
  origin <- as_datetime(sub("days since ", "", time_units))
  dates_raw  <- as_date(origin + duration(time_raw, "days"))

  # According to Sam, the model years 2100-2159 correspond to 1990-2049 in the perturbation scenario.
  dates <- dates_raw - duration(110, "years")

  message(sprintf("\nFile %s spans from %s to %s\n", basename(nc_file), dates[1], dates[length(dates)]))
  nc_close(nc)

  tif_nms <- file.path(bigdata_path, "ACCESS-OM2-01", "rasts", paste0(tif_nm_prefix, dates, ".tif"))
  lyrs_todo <- if (overwrite) {
    seq_along(tif_nms)
  } else {
    which(!file.exists(tif_nms))
  }

  if (length(lyrs_todo) > 0) {
    message(sprintf("\nStarting file %s...\n", basename(nc_file)))

    walk(
      lyrs_todo,
      function(lyr) {
        df <- data.frame(
          lon = as.vector(lon),
          lat = as.vector(lat),
          surface_temp = as.vector(ncvar_get(nc, "surface_temp", start = c(1,1,lyr), count = c(-1,-1,1)))
        )
        df$lon <- ifelse(df$lon < -180, df$lon + 360, df$lon)
        df$surface_temp <- df$surface_temp - 272.15
        pts <- vect(df, geom = c("lon", "lat"), crs = "EPSG:4326")
        r <- rasterize(pts, grid_template, field = "surface_temp", fun = "mean")
        names(r) <- dates[lyr]
        # plot(r)

        # Gapfill the raster - note that this will give some (coastal) land cells SST values!
        gfr <- focal(
          r, 
          w = matrix(c(0,1,0,1,-4,1,0,1,0), nrow=3), 
          fun = "mean", 
          na.policy = "only", 
          na.rm = T
        )
        # plot(gfr)

        writeRaster(gfr, tif_nms[lyr], overwrite = T)
      },
      .progress = T
    )

    nc_close(nc)

  } else {
    message(sprintf("\nSkipping file %s, raster files already exist.\n", basename(nc_file)))
    nc_close(nc)
  }
}


