#' @title Calculate Simple Hysteresis Index (SHI)
#'
#' @description This function calculates Simple Hysteresis Index (SHI)
#' following the description from \emph{Tsyplenkov et al.} (2020)
#'
#' @param dataframe A data set object
#' @param q Water discharge variable (or water stage)
#' @param ssc Suspended sediment concentration variable
#'
#' @return A numeric variable
#'
#' @author Matthias Vanmaercke and Anatoly Tsyplenkov
#'
#' @references Tsyplenkov A, Vanmaercke M, Golosov V, Chalov S. 2020. Suspended sediment budget and intra-event sediment dynamics of a small glaciated mountainous catchment in the Northern Caucasus. \emph{Journal of Soils and Sediments} 20 (8): 3266–3281 DOI: 10.1007/s11368-020-02633-z
#'
#' @examples
#' library(dplyr)
#' data(djan)
#' output_table <- hydro_events(dataframe = djan,
#'  q = discharge,
#'  datetime = time,
#'  window = 21)
#'
#' output_table %>%
#'  filter(he == 2) %>%
#'  SHI(q = discharge, ssc = SS)
#'
#' @export
#' @importFrom dplyr "%>%" enquo select
#' @importFrom tidyr drop_na
#' @importFrom stats lm coef
#'

SHI <- function(dataframe, q, ssc) {

  q <- dplyr::enquo(q)
  ssc <- dplyr::enquo(ssc)

  df <- dataframe %>%
    tidyr::drop_na(!!q, !!ssc) %>%
    dplyr::select(q = !!q, ssc = !!ssc)

  M <- lm(log(ssc) ~ I(log(q)),
          data = df)

  a <- exp(coef(M)[1])
  b <- coef(M)[2]
  res <- df$ssc - a*df$q^b
  r <- mean(res[1:which.max(df$q)])
  f <- mean(res[c((which.max(df$q)+1):length(df$q))])
  simple_hi <- r-f

  simple_hi <- simple_hi / max(df$ssc, na.rm = T)

  return(simple_hi)
}
