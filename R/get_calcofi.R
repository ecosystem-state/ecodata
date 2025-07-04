#' get_calcofi is the primary function for retrieving CalCOFI data via ERDDAP
#' @param min_year The minimum year used, defaults to 1985
#' @param min_years The minimum number of years with positive observations that a species needs to have to be included, defaults to 30
#' @param min_n The minimum number of occurrences a species needs to have across all years, defaults to 300
#'
#' @return null, data file written to inst/ folder
#' @export
#'
#' @importFrom rerddap info tabledap
#' @importFrom utils read.csv
#' @import dplyr
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @import lubridate
#'
#' @examples
#' \dontrun{
#' get_calcofi(min_year = 1990, min_years = 10, min_n = 100)
#' }
get_calcofi <- function(min_year = 1985, min_years = 30, min_n = 300) {

  # grab data for all species
  calcofi_erddap[1] <- "erdCalCOFIlrvcnt"
  for (i in 1:length(calcofi_erddap)) {
    out <- try(info(as.character(calcofi_erddap[i])), silent=TRUE)
    station_dat <- NULL
    try(
      station_dat <- tabledap(out, fields = c(
        "station", "line", "latitude",
        "longitude", "time", "scientific_name", "larvae_10m2", "common_name"
      )),
      silent = TRUE
    )

    if (!is.null(station_dat)) {
      # dataset is very large; > 70 million rows
      station_dat <- as.data.frame(station_dat)
      station_dat$date <- lubridate::as_date(station_dat$time)
      station_dat$year <- lubridate::year(station_dat$date)
      station_dat$month <- lubridate::month(station_dat$date)
      station_dat$yday <- lubridate::yday(station_dat$date)

      # filter out recent years with consistent sampling
      station_dat <- dplyr::filter(station_dat, year >= min_year)
      # format response
      station_dat$larvae_10m2 <- as.numeric(station_dat$larvae_10m2)
      station_dat$larvae_10m2[which(is.na(station_dat$larvae_10m2))] <- 0

      # do lat / lon processing and add UTM
      station_dat$file <- calcofi_erddap[i]

      if (i == 1) {
        dat <- station_dat
      } else {
        dat <- rbind(dat, station_dat)
      }
    }
  }

  stations <- read.csv("inst/CalCOFIStationOrder.csv")
  stations <- dplyr::rename(stations, station = Station, line = Line)
  dat <- dplyr::left_join(dat, stations[, c("station", "line", "StaType")])
  dat <- dplyr::filter(dat, StaType == "ROS") |>
    dplyr::select(-StaType)

  # remove species with 0 records - cuts about 150
  dat <- dplyr::group_by(dat, scientific_name) |>
    dplyr::mutate(tot = sum(larvae_10m2)) |>
    dplyr::filter(tot > 0) |>
    dplyr::select(-tot)

  # filter out species with 30 or more years of data
  thresh_spp <- dplyr::group_by(dat, scientific_name, year) |>
    dplyr::summarise(s = sum(larvae_10m2, na.rm = T)) |>
    dplyr::group_by(scientific_name) |>
    dplyr::summarize(n = length(which(s != 0))) |>
    dplyr::filter(n >= min_years)
  dat <- dplyr::filter(dat, scientific_name %in% thresh_spp$scientific_name)

  # further drop spp with <
  dat <- dplyr::group_by(dat, scientific_name) |>
    dplyr::mutate(npos = length(which(larvae_10m2 > 0))) |>
    dplyr::filter(npos > min_n) |>
    dplyr::select(-npos)

  dat <- dplyr::filter(dat, scientific_name %in% c(
    "Disintegrated fish larvae",
    "Argyropelecus sladeni",
    "Lestidiops ringens",
    "Citharichthys",
    "Cyclothone",
    "Idiacanthus antrostomus",
    "Ceratoscopelus townsendi"
  ) == FALSE)

  # convert to UTM - kms
  # make the UTM cols spatial (X/Easting/lon, Y/Northing/lat)
  dat$latitude <- dat$lat_dd <- as.numeric(dat$latitude)
  dat$longitude <- dat$lon_dd <- as.numeric(dat$longitude)

  dat_coords <-
    st_as_sf(dat[, c("longitude", "latitude")],
      coords = c("longitude", "latitude"),
      crs = 4326
    )
  dat_coords <- st_transform(x = dat_coords, crs = 32610)
  dat$longitude <- as.numeric(st_coordinates(dat_coords)[, 1])
  dat$latitude <- as.numeric(st_coordinates(dat_coords)[, 2])
  dat <- as.data.frame(dat)
  dat$longitude <- dat$longitude / 1000 # to kms
  dat$latitude <- dat$latitude / 1000 # to kms

  # save a file with minimal -- but all key info for the index standardization
  saveRDS(dat, "inst/calcofi_index_data.rds")
}
