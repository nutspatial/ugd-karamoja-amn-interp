## ---- Load required libraries ------------------------------------------------
library(dplyr)
library(lubridate)
library(mwana)
library(sf)
library(spdep)
library(gstat)
library(stars)
library(automap)
library(ggplot2)
library(mapview)
library(janitor)
library(cyphr)

## ---- Set global options and global enviroment variables ---------------------
options(timeout = 300) ## increase timeout for downloads to 300 seconds

## ---- Retrieve secret key for decryption -------------------------------------
secret_key <- data_key(".", path_user = Sys.getenv("path_secret_key"))

## ---- Load project-specific functions ----------------------------------------
for (i in list.files(path = "R", full.names = TRUE)) source(i)

## ---- Read in nutrition data and Uganda shapefiles ---------------------------
source("scripts/read-in-data.R")

## ---- Wrangle aspatial data attributes ---------------------------------------
source("scripts/wrangle-aspatial-attributes.R")

## ---- Run data plausibility checks -------------------------------------------
source("scripts/data-quality-check.R")

## ---- Wrangle spatial data attributes ----------------------------------------
source("scripts/wrangle-spatial-attributes.R")

## ---- Run spatial interpolation ----------------------------------------------
source("scripts/krige-interpolate-wfhz.R")

## ---- Run automated spatial interpolation with `{automap}` -------------------
source("scripts/krige-interpolate-wfhz-automap.R")

## ---- Run automated spatial interpolation of GAM by MUAC with `{automap}` ----
source("scripts/krige-interpolate-muac-automap.R")
