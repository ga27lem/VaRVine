
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
#' @param weights A numerical vector specifying the weights of the portfolio components. If ommitted an equally weighted portfolio is assumed.
#' @import rvinecopulib
#' @import rugarch
#' @import magrittr
#' @import data.table
#' @import dplyr
#' @export
garch_vine_roll <- function(data = NULL, garch.settings = NULL, vine.settings = NULL, weights=NULL) {
  UseMethod("garch_vine_roll")
}


###########################################################

.garch_settings <- function(train.size=500, refit.every=50, specs=list()) {
  new("GarchSettings", train.size = train.size, refit.every = refit.every, specs=specs)
}

.vine_settings <- function(train.size=250, refit.every=25, family.set='all') {
  new("VineSettings", train.size = train.size, refit.every = refit.every, family.set = family.set)
}

.garch_vine_roll <-  function(data = NULL, garch.settings = NULL, vine.settings = NULL, weights = NULL) {

  n <- garch.settings@train.size
  m <- garch.settings@refit.every
  p <- vine.settings@train.size
  q <- vine.settings@refit.every
  pairCopulaFamilies <- vine.settings@family.set

  dt <- data %>% as.data.table(keep.rownames = 'date') %>% melt(id.vars = c('date'), variable.name = 'ticker', value.name = 'ret.closing.prices')


  garch_vine_roll <- new("GarchVineRoll")
  garch_vine_roll@garch.settings <- garch.settings
  garch_vine_roll@vine.settings <- vine.settings

  garch_vine_roll@garch.rolls <- list()

  tickers <- dt$ticker %>% as.character %>%  unique

  weights <- .initialize_weights(tickers = tickers, weights = weights)
  garch_vine_roll@weights <- weights
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

  garch_vine_roll@garch.rolls <- garch_rolls

  cat('\n')
  garchForecasts <- lapply(garch_rolls, function(roll) {
    ans <- roll@forecast$density %>% as.data.table(keep.rownames = 'date')
    ans <- ans[, .(date, Mu, Sigma, Realized, Shape, Skew)]
  })
  numberOfWindows <- garch_rolls[[tickers[1]]]@model$n.refits
  residualsPerWindow <- lapply(1:numberOfWindows, function(i) {
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
      fittedResiduals <- residuals(mod, standardize = T) %>% as.data.table
      colnames(fittedResiduals) <- c('date', 'z')
      fittedResiduals[, Mu := NA]
      fittedResiduals[, Sigma := NA]
      shape <- .coefficients['shape']
      skew <- .coefficients['skew']
      fittedResiduals[, Shape := shape]
      fittedResiduals[, Skew := skew]
      forecastStart <- start
      forecastEnd <- start + m - 1
      forecastedResiduals <- garchForecasts[[.ticker]] %>%
        .[forecastStart:forecastEnd] %>%
        .[, .(date = as.POSIXct(date),
              z = (Realized - Mu) / Sigma, Mu, Sigma, Shape, Skew)]
      ans <- rbind(fittedResiduals, forecastedResiduals)
      colnames(ans) <- colnames(ans) %>% tolower()
      ans[, ticker := .ticker]
      ans[, window_index := i]
    }) %>% rbindlist
  }) %>% rbindlist
  cat("Forecasting Value-at-Risk: ")


  iter <- 1
  garch_vine_roll@vines <- list()
  prevWindowIndex <- -1
  numberOfObservations <- dt[ticker == tickers[[1]]] %>% nrow
  valueAtRiskForecast <- data.table()
  for (end in seq(n, numberOfObservations - 1, by = q)) {
    # valueAtRiskForecast <- lapply(seq(n, numberOfObservations - 1, by = q), function(end) {
    start <- end - p + 1
    curWindowIndex <- floor((end - n) / m) + 1
    if(curWindowIndex != prevWindowIndex) {
      cat('\n')
      cat("Window", curWindowIndex, ":")
      prevWindowIndex <- curWindowIndex
    }
    cat('.')
    residualsStart <- start - (curWindowIndex - 1) * m
    residualsEnd <- end - (curWindowIndex - 1) * m
    # cat("", "start: ", start, ", end:", end, "residualsStart:", residualsStart, "residualsEnd:", residualsEnd)
    uData <- lapply(tickers, function(.ticker) {
      tickerResiduals <- residualsPerWindow[window_index == curWindowIndex &
                                              ticker == .ticker] %>%
        .[order(date)] %>%
        .[residualsStart:residualsEnd] %>%
        .[, .(u = getUData(z, "sstd", shape, skew))]
      colnames(tickerResiduals) <- c(.ticker)
      tickerResiduals
    }) %>% bind_cols()

    vine <- vinecop(data = uData, family_set = pairCopulaFamilies, presel = F)

    garch_vine_roll@vines[[iter]] <- vine
    iter <- iter + 1

    simulated.uData <- rvinecop(n = 100000, vinecop = vine) %>% as.data.table
    simulated.xData <- lapply(tickers, function(.ticker) {
      u <- simulated.uData[[.ticker]]
      shape <- residualsPerWindow[window_index == curWindowIndex &
                                    ticker == .ticker]$shape %>% unique
      skew <- residualsPerWindow[window_index == curWindowIndex &
                                   ticker == .ticker]$skew %>% unique
      ans <- qdist(distribution = 'sstd', shape = shape, skew = skew, p = u) %>%
        as.data.table
      colnames(ans) <- c(.ticker)
      ans
    }) %>% bind_cols()
    forecastStart <- residualsEnd + 1
    forecastEnd <- forecastStart + q - 1
    # cat("", "forecastStart:", forecastStart, ", forecastEnd:", forecastEnd, "\n")
    varForecasts <- lapply(forecastStart:forecastEnd, function(i) {
      simulatedReturns <- lapply(tickers, function(.ticker) {
        mu <- residualsPerWindow[window_index == curWindowIndex &
                                   ticker == .ticker] %>%
          .[order(date)] %>% .[i, mu]
        sigma <- residualsPerWindow[window_index == curWindowIndex &
                                      ticker == .ticker] %>%
          .[order(date)] %>% .[i, sigma]
        z <- simulated.xData[[.ticker]]
        mu + sigma * z
      }) %>% as.data.table()
      colnames(simulatedReturns) <- tickers

      weighted_simulated_returns <- as.data.table(t(t(as.matrix(simulatedReturns)) * weights))


      portfolioReturns <- rowSums(weighted_simulated_returns)
      data.table(`5%` = quantile(portfolioReturns, 0.05),
                 `7.5%` = quantile(portfolioReturns, 0.075),
                 `10%` = quantile(portfolioReturns, 0.1))
    }) %>% rbindlist
    forecastDates <- residualsPerWindow[window_index == curWindowIndex &
                                          ticker == tickers[1] & order(date)] %>%
      .[forecastStart:forecastEnd, date]
    varForecasts[, date := as.POSIXct(forecastDates)]

    varForecasts <- merge(varForecasts, realized_portfolio_returns, by = 'date')

    valueAtRiskForecast <- rbind(valueAtRiskForecast, varForecasts)
  }
  cat('\n')

  garch_vine_roll@VaR.forecast <- valueAtRiskForecast
  garch_vine_roll
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


