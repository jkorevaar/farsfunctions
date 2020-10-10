
#' Read FARS Data.
#'
#' @note reads FARS data into the environment.
#' @note won't work if file name doesn't exist
#' @param filename Character string giving the filename of the data.
#' @importFrom magrittr "%>%"
#' @importFrom readr "read_csv"
#' @importFrom dplyr "tbl_df"
#' @return A tibble containing the FARS data.
#' @examples
#' filename <- 'accident_2013.csv.bz2'
#' fars_read(filename)
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Make FARS Filename.
#'
#' @note makes a properly formatted FARS filename given a year as input
#' @note won't work if year is not in integer or a string format
#' @param year the year of interest - string or integer
#' @return returns a string that is the proper filename for the FARS data for
#' that year
#' @examples
#' make_filename(year = '2013')
#' make_filename(year = 2013)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  filename <- sprintf("accident_%d.csv.bz2", year)
  full_filename <- system.file('extdata', filename, package = 'farsdata')
  full_filename
}

#' Read FARS files for one or more years.
#'
#' @note produces a list of tibbles of FARS data, given an input vector of years.
#' @note won't work if referring to a year that has no data associated
#' @param years a string or integer showing which years of data to load
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "mutate"
#' @importFrom dplyr "select_"
#' @return Returns a list of tibbles. Each tibble contains the year and month
#' rom the observations
#' @examples
#' fars_read_years(years = c(2013, 2014, 2015))
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select_(~ MONTH, ~ year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Produce a Summary of FARS Files.
#'
#' @note produces a summary tibble of FARS years and months given a vector of years.
#' @note won't work if referring to a year that has no data associated
#' @param years Va string or integer showing which years of data to summarize
#' @importFrom dplyr "group_by_"
#' @importFrom dplyr "bind_rows"
#' @importFrom dplyr "summarise_"
#' @importFrom tidyr "spread_"
#' @return a tibble with an id column as monht, and the remaining columns for
#' selected yars. Values in year columns are number of observation that month /
#' year
#'
#' @examples
#' fars_summarize_years(years = c(2013, 2014, 2015))
#' fars_summarize_years(years = 2015)
#
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_(~ year, ~ MONTH) %>%
    dplyr::summarise_(n = ~ n()) %>%
    tidyr::spread_('year', 'n')
}


#' Map State Motor Vehicle Fatalities.
#'
#' @note state motor vehicle fatalities given a year and state id number.
#' @note won't work if referring to a year that has no data associated or a
#' state id without a relevant state associated
#' @param state.num the number refering to the state of interest
#' @param year the year of interest - string or integer
#' @importFrom maps "map"
#' @importFrom dplyr "filter_"
#' @importFrom graphics "points"
#' @return doesn't return anything but prints a map
#' @examples
#' library(mapdata)
#' fars_map_state(12, 2014)
#' fars_map_state(36, 2014)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~ STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
