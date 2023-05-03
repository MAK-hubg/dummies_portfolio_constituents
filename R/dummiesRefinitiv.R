#' Create dummies for selected Index/Portfolio
#'
#' Create times series of dummies for selected Index/Portfolio from Refinitiv data set
#' @param data A data frame. The file must contains following variables with specified data-type:
#' \itemize{
#'  \item ID: The variable records Reuters Identification Code for each security (as character)
#'  \item InOut: The variable records '+' or '-' indicating inclusion and exclusion of security from a Index/portfolio. The data-type for in_out must be a factor.
#'  \item var_Date: The variable records dates. When read with 'in_out' variables clarifies in and out date of any security in the Index/Portfolio. Please make sure that the date variable has 'Date' format.
#' }
#' @param var_ID This is the security ID as per the data-set
#' @param var_InOut This is the variable with c("+", "-") for c(inclusion, exclusion) in the portfolio respectively
#' @param var_Date This is the variable of dates and read with var_InOu to define date of inclusion or exclusion.
#' @param from This is a constant date. The resulting time series of dummies will start from this date (or end of period depending on frequency selected). If it is left empty the function will take the starting date of the date in the data (min(data[,"date"])).
#' @param to This is a constant date. The resulting time series of dummies will end on this date (or end of period depending on frequency selected). If it is left empty the function will take the maximum date of the date in the data (max(data[,"date"])).
#' @param freq This is a constant string variable. It specifies the month as frequency as default. But you may choose any of c("daily", "weekly", 'monthly',"quarterly" or "yearly") as frequencies. The resulting dummies xts file will be of the same frequency as defined here.
#' @import xts
#' @import dplyr
#' @import stringr
#' @import lubridate
#' @export
#' @examples
#' dummiesRefinitiv(
#' data=data.frame(ID = c("A.x", "B.v", "C.x", "B.v","D.x"),
#'                 date = as.Date(c("2019-12-31","2019-12-31","2019-12-31","2020-03-31","2020-03-31")),
#'                 in_out = c("+", "+", "+", "-", "+")),
#' var_ID="ID",
#' var_InOut="in_out",
#' var_Date = "date" ,
#' from="2019-12-31",
#' to="2020-06-30",
#' freq="monthly")
## Add in the author of the `dummies_port()` function
#' @author "Mulazim-Ali KHOKHAR <mulazim.ali.khokhar@vub.be>"

dummiesRefinitiv <- function(data, var_ID, var_InOut, var_Date, from=NULL, to=NULL, freq="monthly"){

  # ensure data.frame structure
  data <- as.data.frame(data)

  # create sequence of calculation dates with required frequency
  date_start <- min ( data[[var_Date]] )
  date_end   <- max ( data[[var_Date]], na.rm=TRUE )
  dailyDates <- calculationDates(date_start, date_end, "daily")

  #Get the unique RIC codes of securities that have been part of the Index/Portfolio at any point in time
  constituents <- unique(data[[var_ID]])

  # Create dummies xts that will be filled by the next procedure
  dummies <- matrix (0, nrow = length (dailyDates), ncol = length (constituents) )
  colnames (dummies) <- unique ( constituents )
  dummies <- xts ( dummies, dailyDates )

  # Search each constituents for their inclusion and exclusion and fill dummies accordingly
  #rowCounts_all <- NULL
  for(id in constituents){
    # The number of row tells us how many time a security comes in and out of the portfolio.
    rowCounts <- NULL
    tempData <- data[data[, c(var_ID)]==id,]
    tempData <- tempData[order(tempData[[var_Date]] ),]

    #records the index(ices) of number of inclusions in selected data of particular security
    iINs <- grep("\\+", tempData[[var_InOut]])
    #records the index(ices) of number of exclusions in selected data of particular security
    iOUTs <- grep("\\-", tempData[[var_InOut]])

    count_INs <-  length(iINs)
    count_OUTs <-  length(iOUTs)
    rowCounts <- count_INs + count_OUTs

    # Check if "+" inclusion of any security is before its exclusion "-" & alternate after sorting by date
    # index of "+" must be odd numbered(1,3, ...) and vice-versa for index of "-" i.e. (2,4,...)

    if ( sum(iINs%%2)==0 | sum(iOUTs%%2)!=0 ) {
      print("Error: There is an issue with inclusion and
             exclusion signs for security with RIC = ",
             id)
    }

    #rowCounts_all <- c(rowCounts_all,rowCounts)
    if(rowCounts<2|rowCounts==2){

      #Start Date
      s <- ceiling_date(as.Date(tempData[iINs,][[var_Date]]), "day")-days(1)

      #End date may be two cases
      if (count_INs==1 & count_OUTs==0) {
        # 1. If security entered but never exited the Index/Portfolio
        e <- date_end
      } else if (count_INs==1 & count_OUTs==1){
        #2. If security entered and exited during our calculation times
        e <- tempData[iOUTs,][[var_Date]] - 1
      }

      if (e < s) {e <- s} #Cases when in and out is same day
      tempDates <-  calculationDates ( s, e, "daily" )
      dummies[tempDates[tempDates%in%time(dummies)], paste0(id)] <- 1

    } else if (rowCounts>2 & rowCounts%%2==0) {
      # If the enter & exit of a firm in portfolios is recorded multiple times,
        # but the firm finally exits (i.e. exit date is not NA)
        for (j in 1:ceiling(rowCounts/2)) {

          s <- tempData[j+j-1,][[var_Date]]
          e <- tempData[j+j,][[var_Date]] - 1

          if (e < s) {e <- s} #Cases when in and out is same day
          tempDates <-  calculationDates ( s, e, "daily" )
          dummies[tempDates[tempDates%in%time(dummies)], paste0(id)] <- 1
        }
      } else if ( rowCounts > 2 & rowCounts%%2 != 0 ) {
        # If the enter & exit of a firm in portfolios is recorded multiple times, and
        # the security still exists in the portfolio at the end of our calculation dates.
          for(j in 1:ceiling(rowCounts/2)){
            s <- tempData[j+j-1,][[var_Date]]
            ifelse ( is.na ( tempData[j+j,][[var_Date]] ),
                     { e <- date_end },
                     { e <- tempData[j+j,][[var_Date]] - 1 }
                   )

            if (e < s) {e <- s} #Cases when in and out is same day
            tempDates <-  calculationDates( s, e, "daily")
            dummies[tempDates[tempDates%in%time(dummies)], paste0(id)] <- 1
          }
      }
  }

  if ( !is.null(from) ) { date_start <- from } else { date_start <- min ( data[[var_Date]] ) }
  if ( !is.null(to) ) { date_end <- to } else { date_end <- max ( data[[var_Date]] ) }
  if ( length ( grep ( "^d", freq, ignore.case = TRUE ) ) == 0 ) {
      datadates <- calculationDates(date_start, date_end, freq)
      dummies <- dummies[datadates,]
    }
    return(dummies)
}


####################################### END of CODE HERE #######################################
