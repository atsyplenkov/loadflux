#' @title Cross-sectional data for countries
#'
#' @docType data
#'
#' @usage data(djan17)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{ccode}{ISO3 country code (as character) for the countries in the
#'      sample (Angola, Benin, France, Rwanda, and UK)}
#'  \item{year}{A value between 1990 and 1999}
#'  \item{month}{An abbreviation (MMM) for month (character)}
#'  \item{gpd}{A fake value for GDP (randomly generated)}
#'  \item{population}{A fake value for population (randomly generated)}
#' }
#' @references https://link.springer.com/article/10.1007%2Fs11368-020-02633-z
#' @keywords datasets
#' @examples
#'
#' data(djan17)
#' head(djan17)
"djan17"
