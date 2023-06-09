% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dummiesCRSP.R
\name{dummiesCRSP}
\alias{dummiesCRSP}
\title{Create dummies for selected Index/Portfolio}
\usage{
dummiesCRSP(
  data,
  var_ID,
  var_Date_in,
  var_Date_out,
  from = NULL,
  to = NULL,
  freq = "monthly"
)
}
\arguments{
\item{data}{A data file should be in data.frame/data.table format. The file should contain three variables i.e. unique security identifier from CRSP database as "ID", its dates of inclusion and exclusion ("date_in" and "date_out" respectively):
\itemize{
\item var_ID: A variable that contains a unique identification code for each security from CRSP/Compustat database. It should be named as ID and must have numeric data format.
\item var_Date_in: A variable that records the date of inclusion of a security in the portfolio. It must be in date format.
\item var_Date_out A variable that records the date of exclusion of a security from the portfolio. It must be in date format.
}}

\item{var_ID}{This is the security ID as per the data-set}

\item{var_Date_in}{This is the date of inclusion in the portfolio}

\item{var_Date_out}{This is the date of exclusion in the portfolio}

\item{from}{This is a date (constant). The resulting time series of dummies will start from this date (or end of period depending on frequency selected). If it is left empty the function will take the starting date of the date_in in the data (min(data\link{,"date_in"})).}

\item{to}{This is a date (constant). The resulting time series of dummies will end on this date (or end of period depending on frequency selected). If it is left empty the function will take the maximum date of the date_out in the data (max(data\link{,"date_in"})).}

\item{freq}{This is a constant string variable. It specifies the month as frequency as default. But you may choose any of c("daily", "weekly", 'monthly',"quarterly" or "yearly") as frequencies. The resulting dummies xts file will be of the same frequency as defined here.}
}
\description{
Create times series of dummies for selected Index/Portfolio constituents from CRSP dataset
}
\examples{
dummiesCRSP(
data=data.frame(ID = c(0001, 0002, 0003, 0004),
                date_in = as.Date(c("2019-12-31","2019-12-31","2019-12-31","2020-03-31")),
                date_out = as.Date(c(NA,NA, "2020-03-31",NA))),
var_ID = "ID",
var_Date_in = "date_in",
var_Date_out = "date_out",
from="2019-12-31",
to="2020-06-30",
freq="monthly")
}
\author{
"Mulazim-Ali KHOKHAR \href{mailto:mulazim.ali.khokhar@vub.be}{mulazim.ali.khokhar@vub.be}"
}
