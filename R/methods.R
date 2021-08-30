
#' function: GARCH Models settings.
#'
#' Method for creating a GarchSettings object.
#' @param train.size The training indow size.
#' @param refit.every Determines every how many periods the garch models are re-estimated.
#' @param specs A named list (stock ticker => \code{\link[rugarch:uGARCHspec-class]{rugarch::uGARCHspec}} class). If some of the tickers are left empty then a default spec will be assigned to that stock TODO link garch_vine_roll.
#' @import rugarch
#' @export
garch_settings <- function(train.size=500, refit.every=50, specs=list()) {
  UseMethod("garch_settings")
}



#' function: vine models settings.
#'
#' Method for creating a VineSettings object.
#' @param train.size The training window size.
#' @param refit.every Determines every how many periods the garch models are re-estimated.
#' @param family.set vector of bivariate copula families. Mimics the `family_set` argument of method \code{\link[rvinecopulib:vinecop]{rvinecopulib::vinecop}}.
#' @import rvinecopulib
#' @export
vine_settings <- function(train.size=250, refit.every=25, family.set='all') {
  UseMethod("vine_settings")
}


#' function: GARCH Vine Rolling Value-at-Risk Forecast and Backtesting.
#'
#' Method for rolling forecast for the Value-at-Risk of a portfolio of assets.
#' @param data A matrix or data.frame where each column corresponds to the sorted log return series of a single stock. The row names are idially dates.
#' @param garch.settings A \code{\linkS4class{GarchSettings}} object specifying the settings for the ARMA-GARCH models.
#' @param vine.settings A \code{\linkS4class{VineSettings}} object specifying the settings for the vine copula model.
#' @param alpha The Value-at-Risk level to calculate.
#' @param weights A numerical vector specifying the weights of the portfolio components. If ommitted an equally weighted portfolio is assumed.
#' @import rvinecopulib
#' @import rugarch
#' @import magrittr
#' @import data.table
#' @import dplyr
#' @export
garch_vine_roll <- function(data = NULL, garch.settings = NULL, vine.settings = NULL, alpha = c(0.01, 0.05), weights=NULL) {
  UseMethod("garch_vine_roll")
}


###########################################################

.garch_settings <- function(train.size=500, refit.every=50, specs=list()) {
  new("GarchSettings", train.size = train.size, refit.every = refit.every, specs=specs)
}

.vine_settings <- function(train.size=250, refit.every=25, family.set='all') {
  new("VineSettings", train.size = train.size, refit.every = refit.every, family.set = family.set)
}

.garch_vine_roll <-  function(data = NULL,
                              garch.settings = NULL,
                              vine.settings = NULL,
                              alpha = c(0.01, 0.05),
                              weights = NULL) {

  n <- garch.settings@train.size
  m <- garch.settings@refit.every
  p <- vine.settings@train.size
  q <- vine.settings@refit.every
  pairCopulaFamilies <- vine.settings@family.set

  dt <- data %>% as.data.table(keep.rownames = 'date') %>% melt(id.vars = c('date'), variable.name = 'ticker', value.name = 'ret.closing.prices')


  garch.vine.roll <- new("GarchVineRoll")
  garch.vine.roll@garch.settings <- garch.settings
  garch.vine.roll@vine.settings <- vine.settings

  garch.vine.roll@garch.rolls <- list()

  tickers <- dt$ticker %>% as.character %>%  unique

  weights <- .initialize_weights(tickers = tickers, weights = weights)
  garch.vine.roll@weights <- weights
  garch.settings@specs <- .initialize_specs(tickers, garch.settings@specs)

  realized_portfolio_returns <- data.table(date = as.POSIXct(rownames(data)), realized = rowSums(as.data.table(t(t(as.matrix(data)) * weights))))

  colnames(dt) <- c("ref.date", "ticker", "ret.closing.prices")
  cat("Fitting garch models ")
  garch_rolls <- lapply(tickers, function(.ticker) {
    cat(.ticker, "")
    r <- dt[ticker == .ticker]$ret.closing.prices
    names(r) <- dt[ticker == .ticker]$ref.date

    spec <- garch.settings@specs[[.ticker]]

    ugarchroll(spec, data = r,
               forecast.length = length(r) - n,
               refit.every = m, refit.window = "moving",
               keep.coef = T, solver = "hybrid")
  })
  names(garch_rolls) <- tickers

  garch.vine.roll@garch.rolls <- garch_rolls

  cat('\n')
  garch_forecasts <- lapply(garch_rolls, function(roll) {
    ans <- roll@forecast$density %>% as.data.table(keep.rownames = 'date')
    ans <- ans[, .(date, Mu, Sigma, Realized, Shape, Skew)]
  })
  number_of_windows <- garch_rolls[[tickers[1]]]@model$n.refits
  residuals_per_window <- lapply(1:number_of_windows, function(i) {
    # print(i)
    lapply(tickers, function(.ticker) {
      # print(.ticker)
      .coefficients <- garch_rolls[[.ticker]]@model$coef[[i]]$coef[, 1]
      spec <- ugarchspec(distribution.model = 'sstd', fixed.pars = .coefficients)
      r <- dt[ticker == .ticker & order(ref.date)]$ret.closing.prices
      names(r) <- dt[ticker == .ticker & order(ref.date)]$ref.date
      start <- (i - 1) * m + 1
      # train on n, forecast m
      end <- start + (n - 1) + m
      r <- r[start:end]
      mod <- ugarchfilter(spec, data = r, out.sample = m)
      fitted_residuals <- residuals(mod, standardize = T) %>% as.data.table
      colnames(fitted_residuals) <- c('date', 'z')
      fitted_residuals[, Mu := NA]
      fitted_residuals[, Sigma := NA]
      shape <- .coefficients['shape']
      skew <- .coefficients['skew']
      fitted_residuals[, Shape := shape]
      fitted_residuals[, Skew := skew]
      forecast_start <- start
      forecast_end <- start + m - 1
      forecasted_residuals <- garch_forecasts[[.ticker]] %>%
        .[forecast_start:forecast_end] %>%
        .[, .(date = as.POSIXct(date),
              z = (Realized - Mu) / Sigma, Mu, Sigma, Shape, Skew)]
      ans <- rbind(fitted_residuals, forecasted_residuals)
      colnames(ans) <- colnames(ans) %>% tolower()
      ans[, ticker := .ticker]
      ans[, window_index := i]
    }) %>% rbindlist
  }) %>% rbindlist
  cat("Forecasting Value-at-Risk: ")


  iter <- 1
  garch.vine.roll@vines <- list()
  prev_window_index <- -1
  number_of_observations <- dt[ticker == tickers[[1]]] %>% nrow
  value_at_risk_forecast <- data.table()
  for (end in seq(n, number_of_observations - 1, by = q)) {
    start <- end - p + 1
    cur_window_index <- floor((end - n) / m) + 1
    if(cur_window_index != prev_window_index) {
      cat('\n')
      cat("Window", cur_window_index, ":")
      prev_window_index <- cur_window_index
    }
    cat('.')
    residuals_start <- start - (cur_window_index - 1) * m
    residuals_end <- end - (cur_window_index - 1) * m
    uData <- lapply(tickers, function(.ticker) {
      ticker_residuals <- residuals_per_window[window_index == cur_window_index &
                                              ticker == .ticker] %>%
        .[order(date)] %>%
        .[residuals_start:residuals_end] %>%
        .[, .(u = getUData(z, "sstd", shape, skew))]
      colnames(ticker_residuals) <- c(.ticker)
      ticker_residuals
    }) %>% bind_cols()

    vine <- vinecop(data = uData, family_set = pairCopulaFamilies, presel = F)

    garch.vine.roll@vines[[iter]] <- vine
    iter <- iter + 1

    simulated_u_data <- rvinecop(n = 100000, vinecop = vine) %>% as.data.table
    simulated_x_data <- lapply(tickers, function(.ticker) {
      u <- simulated_u_data[[.ticker]]
      shape <- residuals_per_window[window_index == cur_window_index &
                                    ticker == .ticker]$shape %>% unique
      skew <- residuals_per_window[window_index == cur_window_index &
                                   ticker == .ticker]$skew %>% unique
      ans <- qdist(distribution = 'sstd', shape = shape, skew = skew, p = u) %>%
        as.data.table
      colnames(ans) <- c(.ticker)
      ans
    }) %>% bind_cols()

    forecast_start <- residuals_end + 1
    forecast_end <- forecast_start + q - 1
    # cat("", "forecastStart:", forecastStart, ", forecastEnd:", forecastEnd, "\n")
    VaR_forecasts <- lapply(forecast_start:forecast_end, function(i) {
      simulated_returns <- lapply(tickers, function(.ticker) {
        mu <- residuals_per_window[window_index == cur_window_index &
                                   ticker == .ticker] %>%
          .[order(date)] %>% .[i, mu]
        sigma <- residuals_per_window[window_index == cur_window_index &
                                      ticker == .ticker] %>%
          .[order(date)] %>% .[i, sigma]
        z <- simulated_x_data[[.ticker]]
        mu + sigma * z
      }) %>% as.data.table()
      colnames(simulated_returns) <- tickers

      weighted_simulated_returns <- as.data.table(t(t(as.matrix(simulated_returns)) * weights))
      portfolio_returns <- rowSums(weighted_simulated_returns)
      lapply(alpha, function(.alpha) {
        col_dt <- data.table(tmp = quantile(portfolio_returns, .alpha))
        colnames(col_dt) <- paste0("alpha_", .alpha)
        col_dt
      }) %>% bind_cols()

    }) %>% rbindlist

    forecast_dates <- residuals_per_window[window_index == cur_window_index &
                                          ticker == tickers[1] & order(date)] %>%
      .[forecast_start:forecast_end, date]
    VaR_forecasts[, date := as.POSIXct(forecast_dates)]

    VaR_forecasts <- merge(VaR_forecasts, realized_portfolio_returns, by = 'date')

    value_at_risk_forecast <- rbind(value_at_risk_forecast, VaR_forecasts)
  }
  cat('\n')

  garch.vine.roll@VaR.forecast <- value_at_risk_forecast
  garch.vine.roll
}


######## Helpers ########
# TODO: remove and get distribution from garch models
getUData <- function(z, dist, shape, skew) {
  pdist(distribution = dist, shape = shape, skew = skew, q = z)
}

.initialize_weights <- function(tickers, weights) {
  if(is.null(weights)) {
    weights <- rep(1/length(tickers), length(tickers))
  }
  return(weights)
}

.initialize_specs <- function(tickers, specs) {
  default_spec <- ugarchspec (
    variance.model = list(garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 1)),
    distribution.model = "sstd"
  )
  for (.ticker in tickers) {
    if (!(.ticker %in% names(specs))) {
      specs[[.ticker]] = default_spec
    }
  }
  return(specs)
}


###########################################################
setMethod(f = "garch_settings", definition = .garch_settings)
setMethod(f = "vine_settings", definition = .vine_settings)
setMethod(f = "garch_vine_roll", definition = .garch_vine_roll)


