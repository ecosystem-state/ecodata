#' #' get_buoy is a wrapper for rnoaa::buoy, to access the National Buoy Data Center
#' #' @param years A single year, or vector of years -- if not included, all data is pulled
#' #' @return null, data file written to inst/ folder
#' #' @export
#' #' 
#' #' @import dplyr
#' #' @importFrom rnoaa buoy
#' #' @importFrom lubridate parse_date_time year month
#' #'
#' #' @examples
#' #' \dontrun{
#' #' get_buoy()
#' #' }
#' get_buoy <- function(years = NULL) {
#' 
#'   # list of all buoys
#'   all_buoys <- c("46050","46029","46084","46082")#"46206"
#'   
#'   data("ocean_grid", envir=environment())
#'   ocean_grid <- dplyr::select(ocean_grid, OBJECTID, OBJECT_X, OBJECT_Y) %>%
#'     dplyr::rename(latitude = OBJECT_Y, longitude = OBJECT_X)
#'   
#'   for(i in 1:length(all_buoys)) {
#'     ocean = buoy(buoyid = all_buoys[i], dataset = 'stdmet', year = 9999)
#'     sub <- dplyr::select(ocean$data, time, lat, lon, sea_surface_temperature)
#'     sub$time <- unlist(lapply(strsplit(sub$time, "T"), getElement, 1))
#'     sub$time <- lubridate::parse_date_time(sub$time, orders = "ymd")
#'     sub$year <- lubridate::year(sub$time)
#'     sub$month <- lubridate::month(sub$time)
#'     
#'     avgs <- dplyr::group_by(sub, month, year) %>%
#'       dplyr::summarise(sea_surface_temperature = mean(sea_surface_temperature, na.rm=T),
#'                        latitude = lat,
#'                        longitude = lon)
#'     if(!is.null(years)) {
#'       avgs <- dplyr::filter(avgs, year %in% years)
#'     }
#'     avgs$buoy <- all_buoys[i]
#'     
#'     # find the ID that is closest
#'     dist <- sqrt((ocean_grid$latitude - avgs$latitude[1])^2 + (ocean_grid$longitude - avgs$longitude[1])^2)
#'     indx <- which.min(dist)[1]
#'     avgs$OBJECTID <- ocean_grid$OBJECTID[indx]
#'     
#'     if(i == 1) {
#'       all_dat <- avgs
#'     } else {
#'       all_dat <- rbind(all_dat, avgs)
#'     }
#'   }
#'   
#'   saveRDS(all_dat, "inst/buoy_sst.rds")
#' }
