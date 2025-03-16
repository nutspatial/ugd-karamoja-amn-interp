#'
#' Download zipfile of Uganda shapefiles from Humanitarian Data Exchange
#' 
#' @param url Download URL for Uganda shapefiles from Humanitarian Data
#'   Exchange. This is set to https://data.humdata.org/dataset/6d6d1495-196b-49d0-86b9-dc9022cde8e7/resource/43dbdff6-aadd-4941-957e-f7eab0717f53/download/uga_admbnda_ubos_20200824_shp.zip
#'   by default.
#' @param destfile Download path for Uganda shapefiles zipfile. This is set to
#'   "data-raw/uganda_shp.zip" by default.
#' @param overwrite Logical. Should `destfile` be overwritten if present?
#'   Default is FALSE.
#'   
#' @returns A downloaded zipfile as specified in `destfile`.
#' 
#' 
#'

download_uga_shp <- function(url = "https://data.humdata.org/dataset/6d6d1495-196b-49d0-86b9-dc9022cde8e7/resource/43dbdff6-aadd-4941-957e-f7eab0717f53/download/uga_admbnda_ubos_20200824_shp.zip", 
  destfile = "data-raw/uganda-shp.zip", 
  overwrite = FALSE) {
## Check whether destfile exists and download accordingly ----
if (file.exists(destfile)) {
if (overwrite) {
warning(
paste0("`", destfile, "` exists and `overwrite = TRUE`. )"),
paste0("Downloading `", destfile, "`.")
)
download.file(url = url, destfile = destfile, mode = "wb")
} else {
warning(
paste0("`", destfile, "` exists and `overwrite = FALSE`. )"),
paste0("`", destfile, "` will not be downloaded.")
)
}
} else {
download.file(url = url, destfile = destfile, mode = "wb")
}

## Return download path ----
destfile
}
