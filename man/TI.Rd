% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TI.R
\name{TI}
\alias{TI}
\title{Calculate Turbidity Index (TI)}
\usage{
TI(dataframe, ssc, datetime, round_time = "hour")
}
\arguments{
\item{dataframe}{A data set object}

\item{ssc}{Turbidity variable}

\item{datetime}{a vector of date-time objects}

\item{round_time}{unit a character string specifying a time unit or a multiple of a unit
to be rounded to. Valid base units are `second`, `minute`, `hour`, `day`,
`week`, `month`, `bimonth`, `quarter`, `season`, `halfyear` and
`year`. See \code{lubridate::round_date}}
}
\value{
A numeric variable
}
\description{
This function calculates Turbidity Index (TI)
following the description from \emph{Chalov & Tsyplenkov} (2020)
}
\examples{
library(dplyr)
data(djanturb)
output_table <- hydro_events(
  dataframe = djanturb,
  q = discharge,
  datetime = time,
  window = 21
)

output_table \%>\%
  filter(he == 2) \%>\%
  TI(
    ssc = discharge,
    datetime = time
  )
}
\references{
Chalov SR, Tsyplenkov AS. 2020. Influence of macroturbulence on the dynamics of river water turbidity. Vestnik Moskovskogo universiteta. Seriya 5, Geografiya 0 (3): 34–46 (In Russ.)
}
\author{
Anatoly Tsyplenkov and Sergey Chalov
}
