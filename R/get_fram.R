#' get_fram grabs processed DART data off of Github, and joins in the OBJECTID
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
#' get_fram()
#' }
get_fram <- function() {

  data("ocean_grid", envir=environment())
  ocean_grid <- dplyr::select(ocean_grid, OBJECTID, OBJECT_X, OBJECT_Y) %>%
    dplyr::rename(latitude = OBJECT_Y, longitude = OBJECT_X)
  
  data("fram_pfmc_2019", envir=environment())
  fram_pfmc_2019 <- dplyr::filter(fram_pfmc_2019, 
                                            !is.na(run_year))
  
  # collapse across age andn shelton groups
  fram_pfmc_2019 <-
    dplyr::filter(fram_pfmc_2019, age > 2) %>%
  dplyr::group_by(stock_long_name, fram_time_step, run_year, region) %>%
     dplyr::summarize(abundance = sum(abundance),
                      stock_id = stock_id[1],
                      stock_name = stock_name[1],
                      shelton_stock = shelton_stock[1])
  
  
  grid <- expand.grid(stock_name = unique(fram_pfmc_2019$stock_name),
                       fram_time_step = unique(fram_pfmc_2019$fram_time_step),
                       #region = unique(fram_pfmc_2019$region),
                       run_year = unique(fram_pfmc_2019$run_year),
                       OBJECTID = ocean_grid$OBJECTID)
  grid = dplyr::left_join(grid, ocean_grid)
  grid$region <- NA
  grid$region[which(grid$latitude < 42)] = "CALI"
  grid$region[which(grid$latitude >= 42 & grid$latitude < 46.26)] = "OR"
  grid$region[which(grid$latitude >= 45.7676 & grid$latitude < 48.5)] = "NOF"
  grid$region[which(grid$latitude >= 48.5 & grid$latitude < 49.151479)] = "SWWCVI"
  grid$region[which(grid$latitude >= 49.151479)] = "NORTH"
  
  grid <- dplyr::select(grid, -latitude, -longitude)
  grid <- dplyr::left_join(grid, fram_pfmc_2019)
  # convert ocean grid to regions
  
    # save output
    saveRDS(grid, "inst/fram_pfmc_grid.rds")
}