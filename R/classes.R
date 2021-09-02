
#' class: GARCH Settings Class.
#'
#' Class for the GARCH models settings.
#'
#' @slot train.size The training window size.
#' @slot refit.every Determines every how many periods the garch models are re-estimated.
#' @slot specs A named list (stock ticker => \code{\link[rugarch:uGARCHspec-class]{rugarch::uGARCHspec}} class).
#'
#' @import rugarch
#'
#' @name GarchSettings
setClass("GarchSettings",
         slots = c(
           train.size="numeric",
           refit.every="numeric",
           specs="list"))





#' class: Vine Settings Class.
#'
#' Class for the vine model settings.
#'
#' @slot train.size The training window size.
#' @slot refit.every Determines every how many periods the garch models are re-estimated.
#' @slot family.set vector of bivariate copula families. Mimics the `family_set` argument of method \code{\link[rvinecopulib:vinecop]{rvinecopulib::vinecop}}.
#'
#' @import rvinecopulib
#'
#' @name VineSettings
setClass("VineSettings",
         slots = c(
           train.size="numeric",
           refit.every="numeric",
           family.set="character"))



#' class:  GARCH Vine Rolling Forecast Class
#'
#' Class for GARCH vine rolling forecast.
#'
#' @slot train.size The training window size.
#' @slot refit.every Determines every how many periods the garch models are re-estimated.
#' @slot family.set vector of bivariate copula families. Mimics the `family_set` argument of method \code{\link[rvinecopulib:vinecop]{rvinecopulib::vinecop}}.
#'
#' @import rvinecopulib
#' @import data.table
#'
#' @name VineSettings
setClass("GarchVineRoll",
         slots = c(
           garch.settings="GarchSettings",
           vine.settings="VineSettings",
           weights="numeric",
           garch.rolls="list",
           vines="list",
           VaR.forecast="data.table",
           time.taken="difftime"
           ))

