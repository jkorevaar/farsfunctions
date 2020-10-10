#' Reads  an accident file from the US FARS.
#'
#' \code{far_raed} is a function which reads an accident data from the US FARS (Fatality Analysis Recording System),
#'  by yearly from 2013-2015.
#'
#' @param filename A chracter of filename of accident which has the form of "accident_2013.csv.bz2". The file sholud not be a vector.
#'
#' @details To import the accident data  saved as  \code{CSV} file, first uses function.
#'          And then converts to tabular data frame using a \code{tbl_df} function. If the file name does't exist, it throws an error with the file name does not exist.
#'
#' @return It returns a data fram of 50 variables where the number of rows varies by year. If the file does't exist it returns does not exist.
#' @importFrom  readr read_csv
#' @importFrom  tibble as_tibble
#' @examples
#' \dontrun{
#' year_input <- 2013
#' data <- year_input %>%
#'   make_filename %>%
#'   fars_read
#' head(data)
#' }
#' @note To generate file name use: \code{\link{make_filename}}
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  tibble::as_tibble(data)
}

#' Makes a file name for a given year.
#'
#' \code{make_filename} function takes a year argument and creates a file name of the given year.
#'
#' @param year Can be a numeric or chracter vector of years. if it is chracter, chnages to numeric using \code{as.integer} function.
#' @return It returns a character of file name, like "accident_2013.csv.bz2".
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' }
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  system.file("extdata",
              sprintf("accident_%d.csv.bz2", year),
              package = "farsfunctions",mustWork = TRUE)
}

#' Reads a FARS file for a given vector of years.
#'
#' @details \code{fars_read_years} function imports the FRAS file for a given
#'          vector years, and it uses \code{\link{make_filename}} to create the
#'          correct file name associated with \code{years}.And then imports the file using
#'          \code{fars_read} function, and then adds(mutates) a varaible \code{year}. Then selects and displays
#'          two variables \code{MONTH}, \code{year}.
#'
#'
#'
#' @param years A numeric vector of years corresponding to FARS file by year.
#'              It also accepts a charcter vector and converts to numeric.
#' @return It returns a list of data frame with two columns and where each list
#'         is for the input years. The two columns are \code{MONTH} and \code{year}.
#'         If the file for the given year is not found, it returns a list of
#'         \code{NULL} with a warning message of invalid year:  \code{years}.
#'
#' @importFrom  dplyr mutate select
#' @importFrom  magrittr "%>%"
#'
#'
#' @examples
#' fars_read_years(2015)
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(.data$MONTH, .data$year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarizes the FARS data by year and month.
#'
#' The \code{fars_summarize_years} function summarizes the number of accidents for
#'  each year by month. The first column represents \code{MONTH} and the
#'  other column are the input years. If the input year is not present in the data
#'  it returns an error.
#'
#'
#' @param years Integer of years or acharacter coreciable to numeric.
#'
#' @return Returns a summary of accidents in a data frame where the first column
#'         reprsents months. Each column are the input years and is filled by
#'         the number of accidents happend in each montht. If the input year is
#'         not in the data set it returns an error.
#' @importFrom  dplyr bind_rows summarize group_by n
#' @importFrom  tidyr spread
#' @importFrom magrittr "%>%"
#'
#' @examples
#' fars_summarize_years(c(2013,2014,2015))
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(.data$year, .data$MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
#' Map of the accidents for a given stat in a year.
#'
#' \code{fars_map_state} function first reads the given \code{state.num} variable,
#   for the given \code{year}, and then plots the accidents in the map of given states.
#'  The map of the accident data for states where a \code{LONGITUDE} of greater
#'  than 900 and \code{LATITUDE} of greater than 90 but not for less than.
#'
#'
#' @param year An iteger of years fro 2013 to 2015 or corceabile to itegers.
#' @param state.num Numeric vector representing the value of STATE variable
#'        in the data.
#'
#' @return  Returns a plot of map of points of accidents for the given state
#'          number in the year. If the stat number is not in the STAT variable, it
#'         returns \code{NULL}.Similarly, if the given stat number has no accident,
#          it displays no accidents to plot. The map of the accident shows for
#'         \code{LONGITUDE} >900 and \code{LATITUDE}>90.
#' @importFrom maps map
#' @importFrom  graphics points
#' @importFrom  dplyr filter
#'
#' @examples
#' fars_map_state(20, 2013)
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, .data$STATE == state.num)
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
