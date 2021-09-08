#' A sample log returns for a few assets.
#'
#' Data extracted from Yahoo Finance representing the log returns for
#' Google, Apple, and Amazon stocks between 2014-01-13 and 2018-01-01 where in total
#' there are 1000 data points.
#'
#'
#'
#' @docType data
#'
#' @usage data(sample_returns)
#'
#' @format data.frame with 4 columns and 1000 rows, the first column `date` represents the daily dates while the other 3 columns (`GOOG`, `AAPL`, `AMZN`) represent
#' the daily log return of the 3 stocks.
#'
#' @keywords datasets
#'
#' @source \href{https://finance.yahoo.com}{Yahoo Finance}
#'
#'
#' @examples
#' data(sample_returns)
#' head(sample_returns)
"sample_returns"

