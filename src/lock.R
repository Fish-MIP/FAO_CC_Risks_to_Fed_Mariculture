# This script "locks" important/common packages and makes sure they aren't cleaned by renv, even if they aren't being actively used
# install.packages("ropensci/rnaturalearth")
# install.packages("ropensci/rnaturalearthdata")
# install.packages("ropensci/rnaturalearthhires")

library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

library(yaml)
library(units)
library(qs)

library(targets)
Sys.setenv(TAR_PROJECT = "project_1")
tar_renv(path = "targets_pipelines/_packages_1.R")
