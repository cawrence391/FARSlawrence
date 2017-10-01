#' Read in fars data
#'
#' This function reads and returns a dataframe of the specified file
#'
#'@param filename character string of full filename with extension
#'@return dataframe of filename data
#'
#'@import readr
#'@import dplyr
#'
#'@examples
#'\dontrun{
#'fars_read("accident_2013.csv.bz2")
#'}
#'
#'@note ERROR if following conditions not met: Input must be character string with full extension and file location must be correct
#'
#'@export
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Generate file name string from year
#'
#' This function generates a standardised filename string with csv.bz2 extension based on the year defined by the user
#'
#' @param year integer of year
#' @return string representing standard filename of format "accident_<year>.csv.bz2"
#'
#' @examples
#' \dontrun{ make_filename(2013)
#' }
#'
#' @note ERROR note: Function does not accept vectors containing multiple numerics
#'
#
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read in multiple fars files retuning list of dataframes
#'
#' This function returns individual data frames from raw fars files
#'
#' @param years numeric vector defining years to read in from raw file
#' @return dataframes based on each yearly raw fars file
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' }
#'
#' @import dplyr
#' @import magrittr
#'
#' @export
#'
#' @note ERROR if following conditions not met: Files must be in working directory and filenames must be of format "accident_<year>.csv.bz2"
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summary table of number of incidents per month per year
#'
#' This function generates a summary table of the number incidents per calendar month in each of the yearly files defined
#'
#' @inheritParams fars_read_years
#' @return table summarising the number incidents per calendar month (represented as rows) in each year (represented in columns)
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2015)
#' }
#'
#' @import tidyr
#' @import magrittr
#' @import dplyr
#'
#' @export
#'
#' @note ERROR if following conditions not met: Files must be in working directory and filenames must be of format "accident_<year>.csv.bz2"
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot incidents from a single year on state map
#'
#' This function genreates a plot showing the location of each incident in the defined year and state
#'
#' @param state.num Serial number of state - integer
#' @param year year to plot - integer
#' @return plot with state map outlined and incident location shown as points on the map
#'
#' @import dplyr
#' @import maps
#' @import graphics
#' @import magrittr
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' fars_map_state(1, 2013)
#' }
#'
#' @note ERROR if following conditions not met: Files must be in working directory and filenames must be of format "accident_<year>.csv.bz2"
#'
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
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
