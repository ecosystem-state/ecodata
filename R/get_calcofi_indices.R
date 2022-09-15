#' get_calcofi_indices grabs processed indices off of Github, and joins in the OBJECTID
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
#' get_calcofi_indices()
#' }
get_calcofi_indices <- function() {

  # pull in station data
  stations <- read.csv("https://github.com/ecosystem-state/calcofi-auto/raw/main/data/CalCOFIStationOrder.csv")
  stations <- dplyr::filter(stations, StaType == "ROS") %>%
    dplyr::select(Dlongitude, Station.Dlatitude) %>%
    dplyr::rename(latitude = "Station.Dlatitude", longitude = "Dlongitude")
  
  # pull in ocean grid
  data("ocean_grid", envir=environment())
  ocean_grid <- dplyr::select(ocean_grid, OBJECTID, OBJECT_X, OBJECT_Y) %>%
    dplyr::rename(latitude = OBJECT_Y, longitude = OBJECT_X)
  
  ids <- NA
  for(i in 1:nrow(stations)) {
    dist <- sqrt((ocean_grid$latitude - stations$latitude[i])^2 + (ocean_grid$longitude - stations$longitude[i])^2)
    ids <- c(ids, ocean_grid$OBJECTID[which(dist < 1)])
  }
  ids <- unique(ids[-1])
  
  # pull in index
  d <- readRDS(url("https://github.com/ecosystem-state/calcofi-auto/raw/main/indices/predicted_indices_sdmtmb.rds"))
  
  # generate grid of species - year - id
  df <- expand.grid(OBJECTID = ids, species = unique(d$species), year = unique(d$year))
  df <- dplyr::left_join(df, d)
  
  df <- dplyr::select(df, -mean_cpue, -n_pos_cpue)
  saveRDS(df, "inst/calcofi_indices.rds")
}