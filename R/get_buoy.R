#' get_buoy is a wrapper for rnoaa::buoy, to access the National Buoy Data Center
#' @return null, data file written to inst/ folder
#' @export
#' 
#' @import dplyr
#' @importFrom rnoaa buoy
#' @importFrom lubridate parse_date_time year month
#'
#' @examples
#' \dontrun{
#' get_buoy()
#' }
get_buoy <- function(years = NULL) {

  # list of all buoys
  all_buoys <- c("46050","46029","46206","46084","46082")
  
  for(i in 1:length(buoys)) {
    ocean = buoy(buoyid = all_buoys[i], dataset = 'stdmet', year = 9999)
    sub <- dplyr::select(ocean$data, time, lat, lon, sea_surface_temperature)
    sub$time <- unlist(lapply(strsplit(sub$time, "T"), getElement, 1))
    sub$time <- lubridate::parse_date_time(sub$time, orders = "ymd")
    sub$year <- lubridate::year(sub$time)
    sub$month <- lubridate::month(sub$time)
    
    avgs <- dplyr::group_by(sub, month, year) %>%
      dplyr::summarise(sea_surface_temperature = mean(sea_surface_temperature, na.rm=T))
    if(!is.null(years)) {
      avgs <- dplyr::filter(avgs, year %in% years)
    }
    avgs$buoy <- all_buoys[i]
    
    if(i == 1) {
      all_dat <- avgs
    } else {
      all_dat <- rbind(all_dat, avgs)
    }
  }
  
  saveRDS(all_dat, "inst/buoy_sst.rds")
}
