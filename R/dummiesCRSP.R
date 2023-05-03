#' Create dummies for selected Index/Portfolio
#'
#' Create times series of dummies for selected Index/Portfolio constituents from CRSP dataset
#' @param data A data file should be in data.frame/data.table format. The file should contain three variables i.e. unique security identifier from CRSP database as "ID", its dates of inclusion and exclusion ("date_in" and "date_out" respectively):
#' \itemize{
#'  \item var_ID: A variable that contains a unique identification code for each security from CRSP/Compustat database. It should be named as ID and must have numeric data format.
#'  \item var_Date_in: A variable that records the date of inclusion of a security in the portfolio. It must be in date format.
#'  \item var_Date_out A variable that records the date of exclusion of a security from the portfolio. It must be in date format.
#' }
#' @param var_ID This is the security ID as per the data-set
#' @param var_Date_in This is the date of inclusion in the portfolio
#' @param var_Date_out This is the date of exclusion in the portfolio
#' @param from This is a date (constant). The resulting time series of dummies will start from this date (or end of period depending on frequency selected). If it is left empty the function will take the starting date of the date_in in the data (min(data[,"date_in"])).
#' @param to This is a date (constant). The resulting time series of dummies will end on this date (or end of period depending on frequency selected). If it is left empty the function will take the maximum date of the date_out in the data (max(data[,"date_in"])).
#' @param freq This is a constant string variable. It specifies the month as frequency as default. But you may choose any of c("daily", "weekly", 'monthly',"quarterly" or "yearly") as frequencies. The resulting dummies xts file will be of the same frequency as defined here.
#' @import xts
#' @import dplyr
#' @import stringr
#' @export
#' @examples
#' dummiesCRSP(
#' data=data.frame(ID = c(0001, 0002, 0003, 0004),
#'                 date_in = as.Date(c("2019-12-31","2019-12-31","2019-12-31","2020-03-31")),
#'                 date_out = as.Date(c(NA,NA, "2020-03-31",NA))),
#' var_ID = "ID",
#' var_Date_in = "date_in",
#' var_Date_out = "date_out",
#' from="2019-12-31",
#' to="2020-06-30",
#' freq="monthly")
## Add in the author of the `dummies_port()` function
#' @author "Mulazim-Ali KHOKHAR <mulazim.ali.khokhar@vub.be>"

dummiesCRSP <- function(data, var_ID, var_Date_in, var_Date_out, from=NULL, to=NULL, freq="monthly"){

  # ensure data.frame structure
  data <- as.data.frame(data)

  # Create sequence of calculation-dates with daily frequency
  date_start <- min(data[[var_Date_in]], na.rm=TRUE )
  date_end <- max(max(data[[var_Date_out]], na.rm=TRUE ),
                  max(data[[var_Date_in]], na.rm=TRUE ),
                  as.Date(to),
              na.rm=TRUE )

  dailyDates <- calculationDates(date_start, date_end, "daily")

  #Get the unique GVKEY codes of securities that have been part of the Index/Portfolio at any point in time
  constituents <- unique(data[[var_ID]])

  # Create dummies xts that will be filled by the next procedure
  dummies <- matrix(0, nrow=length(dailyDates), ncol = length(constituents))
  colnames(dummies)<- unique(constituents)
  dummies <- xts(dummies, dailyDates)

  gk <- NULL
  for(gk in constituents){
    # gk=constituents[1]
    tempData <- data[which ( data[, var_ID] == paste0 ( gk ) ), ]
    count_IN_OUTs <- nrow ( tempData )

    # We assign sequence of dates at which the firm enters-in or exits-from the index.
    # The sequence of dates is constructed as per the frequency provided for resulting time series.
    if(count_IN_OUTs==1){

      # If there is only one entry of a security in the portfolio
      count_NA <- sum(is.na(tempData[, var_Date_out]))

      if(count_NA==1){# If exit date is not available
        s <- tempData[[var_Date_in]]
        e <- date_end
      } else if(count_NA==0){# If exit date is  available
        s <- tempData[[var_Date_in]]
        e <- tempData[[var_Date_out]]-1
      }

      # If data entry is such that security enters and exits on same day
      # May be because of error or last day of data-entry
      if (e < s) {e <- s}
      tempDates <-  calculationDates( s, e, "daily")
      dummies[tempDates[tempDates%in%time(dummies)], paste0(gk)] <- 1

    } else if ( count_IN_OUTs > 1 ) {

        # If there are more than one entries of a security in the portfolio
        for(i in 1:count_IN_OUTs){
          #i=2
          count_NA <- sum(is.na(tempData[i, var_Date_out]))
          if(!is.na(tempData[i, var_Date_out]) & tempData[i, var_Date_out]==tempData[i, var_Date_in]){
            paste0("Error: Both inclusion and exclusion in same time period for security with GVKEY = ", gk)
            next
            } else if(count_NA==1){# If exit date is not available
              s <- tempData[i,][[var_Date_in]]
              e <- date_end

            } else if(count_NA==0){# If exit date is  available
              s <- tempData[i,][[var_Date_in]]
              e <- tempData[i,][[var_Date_out]]-1
            }

          # If data entry is such that security enters and exits on same day
          # May be because of error or last day of data-entry
          if (e < s) {e <- s}
          tempDates <-  calculationDates ( s, e, "daily" )
          dummies[tempDates[tempDates%in%time(dummies)], paste0(gk)] <- 1
        }
      }
  }

  # Verify desired dates for start and end of the results
  if( !is.null(from) ) { date_start <- from } else { date_start <- min( data[[var_Date_in]] ) }
  if( !is.null(to) ) { date_end <- to } else { date_end <- max(data[[var_Date_out]], na.rm=TRUE ) }

  if ( length(grep("^d", freq, ignore.case = TRUE)) == 0 ) {
    datadates <- calculationDates(date_start, date_end, freq)
    dummies <- dummies[datadates,]
  }

  return(dummies)
}





####################################### END of CODE HERE #######################################
