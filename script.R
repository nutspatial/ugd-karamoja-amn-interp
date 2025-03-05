## ---- Load required libraries ------------------------------------------------
library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)
library(mwana)
library(sf)
library(gstat)
library(stars)
library(spdep)
library(rgeoda)
library(ggplot2)

## ---- Read in input data and set Coordinate Reference System -----------------
source("scripts/read-in-input-data.R")

## ---- Read in Sudan shapefile, project and filter out Al Fasher --------------
source("scripts/shapefiles.R")

## ---- Wrangle non-spatial data -----------------------------------------------
source("scripts/wrangle-data.R")

## ---- Wrangle spatial data -----------------------------------------------
source("scripts/wrangle-spatial-attributes.R")

## ---- Run spatial interpolation ----------------------------------------------
source("scripts/interpolate.R")