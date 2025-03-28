################################################################################
#                                  READ DATA                                   #
################################################################################


## ---- Read data --------------------------------------------------------------

### --------------------------------------- Decrypt and read nutrition data ----
nut_data <- decrypt(
  expr = read.csv("data-raw/input-data-fsla-2021.csv"),
  key = secret_key
)

### ------------------------------------------------------- Read shapefiles ----

#### Download zipfile of Uganda shapefiles from Humanitarian Data Exchange ----
download_uga_shp(overwrite = FALSE)

#### Read zipped Uganda shapefile ----
read_uga_shp(path_to_zip = "data-raw/uganda-shp.zip", overwrite = TRUE)

#### Uganda's admin 2 ----
uga2 <- st_read(
  dsn = "data-raw/uga-shp",
  layer = "uga_admbnda_adm2_ubos_20200824", 
  quiet = TRUE
)

#### Uganda's admin 4 ----
uga4 <- st_read(
  dsn = "data-raw/uga-shp",
  layer = "uga_admbnda_adm4_ubos_20200824", 
  quiet = TRUE
)

################################ End of workflow ###############################
