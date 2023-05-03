#' Calculates dates with defined frequency with from and to inclusive
#'
#'The functions take start and end dates, periodic frequency. The result is a vector of dates. The reason of creating this functions is to get dates that are sequence of dates ending at each ending date specified frequency. We could use seq.Date but it has two limitations. First it does provide periodic sequence but those sequences are on dependent on start or end date. Second, it excludes either the ending dates of specified frequency or the starting dates (when used in reverse mode). So even if we use endpoints from lubridate we will miss out one whole period.
#' For example if to=as.Date("2020-06-30") with by="quarter" then the second quarter of 2020 will skip the sequence of date created by seq.Date. Since we need all dates and periods including ending period, our function gets all dates of calculations
#' @param from the starting date of the calculation dates
#' @param to the ending date of the calculation dates
#' @param freq the required frequency of calculations
#' @return the function (calculationDates) returns a vector of the sequence of dates with
#' the defined frequency. The results sequence of dates with specific frequency
#' may be used for time series analysis
#' @import lubridate
#' @import stringr
#' @export
#' @examples
#' calculationDates("2019-12-31", "2020-06-30", "daily")
#' calculationDates("2019-12-31", "2020-06-30", "weekly")
#' calculationDates("2019-12-31", "2020-06-30", "monthly")
#' calculationDates("2019-12-31", "2020-06-30", "quarterly")
#' calculationDates("2019-12-31", "2020-06-30", "yearly")
## Add in the author of the `dummiesCRSP()` and 'dummiesRefinitiv()' function
#' @author "Mulazim-Ali KHOKHAR <mulazim.ali.khokhar@vub.be>"
## List the `seq.Date` function (from the `base` package)
## List the `endpoints` function (from the `lubridate` package)
#' @seealso \link[base]{seq.Date}
#' @seealso \link[lubridate]{endpoints}



calculationDates <- function(from, to, freq){

# We get the unit of frequency as seq.Date accepts unit like "month/m" not monthly etc
  unit = unit_of_frequency(freq)

#start of the period
  startDate <- ceiling_date(as.Date(from), unit)-days(1)

#end of the period
  endDate <- ceiling_date(as.Date(to), unit)-days(1)

# First we get sequence of daily dates
  calc_dates <- seq.Date(from=startDate, to=endDate, by='days')

# then we convert the daily sequence of dates to specific frequency
  calc_dates <- calc_dates[endpoints(calc_dates, on=unit)]

# Its returns dates including start and end of required frequency periodic dates
  return(calc_dates)
}


unit_of_frequency <- function(freq){
  #take first word of the given frequency
  temp.freq <- tolower(substring(as.character(freq), 1, 1))

  # Assign values to frequency and units form further use
  if(temp.freq=="d"){
    unit="day"
  }else if(temp.freq=="w"){
    unit <- "week"
  }else if(temp.freq=="m"){
    unit <- "month"
  }else if(temp.freq=="q"){
    unit <- "quarter"
  }else if(temp.freq=="y"|temp.freq=="a"){
    unit <- "year"
  }
  return(unit)
}
