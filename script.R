## ---- Load required libraries ------------------------------------------------
library(readxl)
library(dplyr)
library(lubridate)
library(mwana)
library(sf)
library(gstat)
library(stars)
library(rgeoda)
library(ggplot2)

## ---- Read in input data and set Coordinate Reference System -----------------
source("scripts/read-in-input-data.R")

## ---- Wrangle non-spatial data -----------------------------------------------
source("scripts/wrangle-data.R")

## ---- Run data plausibility checks -------------------------------------------
source("scripts/data-quality-check.R")

## ---- Read in Sudan shapefile, project and filter out Al Fasher --------------
source("scripts/shapefiles.R")

## ---- Wrangle spatial data ---------------------------------------------------
source("scripts/wrangle-spatial-attributes.R")

## ---- Run spatial interpolation ----------------------------------------------
source("scripts/interpolate.R")
