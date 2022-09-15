#' get_rreas_indices grabs processed indices off of Github, and joins in the OBJECTID
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
#' get_rreas_indices()
#' }
get_rreas_indices <- function() {

  # pull in station data
  stations <- readRDS(url("https://github.com/ecosystem-state/rreas-auto/raw/main/data/raw_data.rds"))
  stations <- unique(paste(round(stations$lat_dd,2), round(stations$lon_dd,2)))
  stations <- as.data.frame(cbind(as.numeric(unlist(lapply(strsplit(stations, " "), getElement, 1))),
                    as.numeric(unlist(lapply(strsplit(stations, " "), getElement, 2)))))
  names(stations) = c("latitude","longitude")

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
  d <- readRDS(url("https://github.com/ecosystem-state/rreas-auto/raw/main/indices/predicted_indices.rds"))
  d <- dplyr::select(d, -mean_catch, -n_pos_catch)
  
  # generate grid of species - year - id
  df <- expand.grid(OBJECTID = ids, species = unique(d$species), year = unique(d$year))
  df <- dplyr::left_join(df, d)
  
  saveRDS(df, "inst/rreas_indices.rds")
}