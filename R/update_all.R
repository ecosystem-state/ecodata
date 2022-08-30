#' update_all is a wrapper for updating all public datasets
#' @return null
#' @export
#' 
#' @examples
#' \dontrun{
#' update_all()
#' }
update_all <- function() {
  get_rreas(min_year = 1985, lat_max = 38.1884, lat_min = 36.544)
  get_calcofi(min_year = 1985, min_years = 30, min_n = 300)
  get_lighthouse()
  get_buoy()
}
