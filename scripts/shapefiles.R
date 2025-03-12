################################################################################
#                                READ SHAPEFILES                               #
################################################################################

## ---- List of district that falls under Karamoja region ----------------------
districts <- c(
  "Abim", "Amudat", "Kotido", "Karenga", "Kaabong", "Napak",
  "Nakapiripirit", "Nabilatuk", "Moroto")

## ---- Read in the shapefiles and set the CRS ---------------------------------
### ------------------------------------------------------------- Districts ----
karamoja_admn2 <- st_read(
  dsn = "data-raw/uga_admbnda_adm2_ubos_20200824.shp"
) |> 
  filter(ADM2_EN %in% districts) |> 
  st_transform(crs = "EPSG:32636")

### -------------------------------------------------------------- Counties ----
karamoja_admn4 <- st_read(
  dsn = "data-raw/uga_admbnda_adm4_ubos_20200824.shp"
) |> 
  filter(ADM2_EN %in% districts) |> 
  st_transform(crs = "EPSG:32636")

################################ End of workflow ###############################
