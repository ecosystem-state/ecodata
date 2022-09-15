#' get_dart grabs processed DART data off of Github, and joins in the OBJECTID
#' @return null, data file written to inst/ folder
#' @export
#' 
#' @import dplyr
#' @importFrom stringr str_split
#' @importFrom utils data
#' @importFrom stats time
#' 
#' @examples
#' \dontrun{
#' get_dart()
#' }
get_dart <- function() {

  data("ocean_grid", envir=environment())
  ocean_grid <- dplyr::select(ocean_grid, OBJECTID, OBJECT_X, OBJECT_Y) %>%
    dplyr::rename(latitude = OBJECT_Y, longitude = OBJECT_X)
  
    d <- readRDS(url("https://github.com/ericward-noaa/cbr_dart/raw/main/data/bonneville_counts.rds"))

    dist <- sqrt((ocean_grid$latitude - d$latitude[1])^2 + (ocean_grid$longitude - d$longitude[1])^2)
    indx <- which.min(dist)[1]
    d$OBJECTID <- ocean_grid$OBJECTID[indx]
    
    # save output
    saveRDS(d, "inst/bonneville_counts.rds")
}