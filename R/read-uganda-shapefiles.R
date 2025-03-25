
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
#'

read_uga_shp <- function(path_to_zip, 
                         exdir = "data-raw/uga-shp",
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
