#' bc_lighthouse is downloads and processes data from the BC lighthouse archive
#' @param years A single year, or vector of years -- if not included, all data is pulled
#' @return null, data file written to inst/ folder
#' @export
#' 
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom utils download.file unzip
#'
#' @examples
#' \dontrun{
#' get_lighthouse()
#' }
get_lighthouse <- function(years = NULL) {

  temp <- tempfile()
  download.file("https://pacgis01.dfo-mpo.gc.ca/FGPPublic/BCLightstations/DATA_-_Active_Sites.zip",temp)
  unzip(temp)
  
  sites <- dir("DATA_-_Active_Sites")
  for(i in 1:length(sites)) {
    files <- dir(paste0("DATA_-_Active_Sites/",sites[i]))
    files <- files[-grep("french",files)]
    files <- files[grep("Sea_Surface_Temperatures",files)]
    d <- read.csv(paste0("DATA_-_Active_Sites/",sites[i],"/",files),
                  stringsAsFactors = FALSE, skip = 1)
    d <- tidyr::pivot_longer(d, cols = 2:13)
    d <- dplyr::rename(d, year = YEAR, month = name, sea_surface_temperature = value)
    d$lighthouse <- sites[i]
    if(i == 1) {
      all_data <- d
    } else {
      all_data <- rbind(all_data, d)
    }
  }
  # this dataset uses 999.99 to fill NAs
  all_data$sea_surface_temperature[which(all_data$sea_surface_temperature > 100)] = NA
  
  if(!is.null(years)) {
    all_data <- dplyr::filter(all_data, year %in% years)
  }
  
  # convert month to numeric
  all_data$fmonth <- factor(all_data$month, levels = all_data$month[1:12])
  all_data$month <- as.numeric(all_data$fmonth)
  all_data <- dplyr::select(all_data, -fmonth)
  
  latlon <- data.frame(lighthouse = c("Amphitrite_Point","Bonilla_Point","Chrome_Island",
                                      "Departure_Bay_PBS","Egg_Island","Entrance_Island",
                                      "Kains_Island","Langara_Island","McInnes_Island",
                                      "Nootka_Point","Pine_Island","Race_Rocks"),
                       lon = c(-125.541,-130.637,-124.685,-123.955,-127.835,-123.808,-128.032,-133.059,-128.722,-126.615,-127.728,-123.532),
                       lat = c(48.922,53.493,49.472,49.21,51.25,49.209,50.441,54.255,52.261,49.593,50.976,48.298))
  all_data <- dplyr::left_join(all_data, latlon)
  unlink(temp)
 
  unlink("DATA_-_Active_Sites", recursive=TRUE)
  saveRDS(all_data, "inst/bc_lighthouse_sst.rds")
}
