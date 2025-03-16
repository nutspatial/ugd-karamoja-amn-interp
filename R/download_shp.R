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
#' @examples
#' download_uga_shp()
#' 
#'

download_uga_shp <- function(url = "https://data.humdata.org/dataset/6d6d1495-196b-49d0-86b9-dc9022cde8e7/resource/43dbdff6-aadd-4941-957e-f7eab0717f53/download/uga_admbnda_ubos_20200824_shp.zip", 
                             destfile = "data-raw/uganda_shp.zip", 
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


#'
#' Read zipped Uganda shapefile
#' 
#' @param path_to_zip Path to downloaded zipfile. This is usually produced via
#'   a call to `download_uga_shp`.
#' @param exdir Directory to extract contents of zipfile into. Default to
#'   "data-raw/uga_shp".
#' @param overwrite Logical. Should existing files be overwritten? Default to
#'   FALSE.
#' @param layer Layer/s to read. If NULL (default), then all 4 layers of the
#'   Uganda shapefile is read.
#'
#' @returns A list of `sf` objects for each layer specified.
#' 
#' @examples
#' read_uga_shp(path_to_zip = "data-raw/uga_shp.zip")
#' 
#'

read_uga_shp <- function(path_to_zip, 
                         exdir = "data-raw/uga_shp",
                         overwrite = FALSE,
                         layer = NULL) {
  unzip(zipfile = path_to_zip, exdir = exdir, overwrite = overwrite)
  
  if (is.null(layer)) layer <- 1:4 else layer <- layer
  
  lapply(
    X = layer,
    FUN = function(x) {
      assign(
        x = paste0("uga", x),
        value = sf::st_read(
          dsn = exdir, 
          layer = paste0("uga_admbnda_adm", x, "_ubos_20200824") 
        )
      )
    }
  )
}
