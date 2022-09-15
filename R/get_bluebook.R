#' get_bluebook grabs processed bluebook data off of Github, and joins in the OBJECTID
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
#' get_bluebook()
#' }
get_bluebook <- function() {
  file_names = c("pfmc_ca_rec.csv",
  "pfmc_ca_rec_catch.csv",
  "pfmc_ca_troll_catch.csv",
  "pfmc_ca_troll_econ.csv",
  "pfmc_ca_troll_effort.csv",
  "pfmc_or_rec.csv",
  "pfmc_or_rec_catch.csv",
  "pfmc_or_troll_catch.csv",
  "pfmc_or_troll_effort.csv",
  "pfmc_wa_rec.csv",
  "pfmc_wa_rec_catch.csv",
  "pfmc_wa_troll_catch.csv",
  "pfmc_wa_troll_effort.csv",
  "pfmc_wa_troll_indian_catch.csv",
  "pfmc_wa_troll_indian_effort.csv")

  data("ocean_grid", envir=environment())
  ocean_grid <- dplyr::select(ocean_grid, OBJECTID, OBJECT_X, OBJECT_Y) %>%
    dplyr::rename(latitude = OBJECT_Y, longitude = OBJECT_X)
  
  for(i in 1:length(file_names)) {
    # read in data from github
    d <- read.csv(paste("https://github.com/ericward-noaa/nmfs-bluebook/raw/main/data/",file_names[i],sep=""))
    
    # identify ports
    ports <- dplyr::group_by(d, port) %>%
        dplyr::summarise(latitude = latitude[1], longitude = longitude[1],
                         OBJECTID = NA)
    
    # find closest center
    for(p in 1:nrow(ports)) {
      dist <- sqrt((ocean_grid$latitude - ports$latitude[p])^2 + (ocean_grid$longitude - ports$longitude[p])^2)
      indx <- which.min(dist)[1]
      ports$OBJECTID[p] <- ocean_grid$OBJECTID[indx]
    }
    
    # join objectid
    d <- dplyr::left_join(d, ports)
    
    # save output
    file_n <- str_split(file_names[i], pattern=".csv")[[1]][1]
    saveRDS(d, paste0("inst/",file_n,".rds"))
  }
}