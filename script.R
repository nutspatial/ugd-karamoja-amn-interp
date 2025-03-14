## ---- Load required libraries ------------------------------------------------
library(readxl)
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

## ---- Load utility functions -------------------------------------------------
source("R/utils.R")

## ---- Read in input data and set Coordinate Reference System -----------------
source("scripts/read-in-input-data.R")

## ---- Read in Karamoja shapefiles and set CRS --------------------------------
source("scripts/read-in-shapefiles.R")

## ---- Wrangle non-spatial data -----------------------------------------------
source("scripts/wrangle-aspatial-attributes.R")

## ---- Run data plausibility checks -------------------------------------------
source("scripts/data-quality-check.R")

## ---- Wrangle spatial data ---------------------------------------------------
source("scripts/wrangle-spatial-attributes.R")

## ---- Run spatial interpolation ----------------------------------------------
source("scripts/krige-interpolate-wfhz.R")

## ---- Run automated spatial interpolation with `{automap}` -------------------
source("scripts/krige-interpolate-wfhz-automap.R")
