#' @title rapidsplithalf package
#' @name rapidsplithalf
#' @aliases rapidsplithalf-package
#' @import Rcpp
#' @useDynLib rapidsplithalf
#' @importFrom stats cor setNames qnorm pt pnorm quantile weighted.mean
#' @importFrom grDevices rgb
#' @importFrom graphics text
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom fastmatch fmatch %fin%
#' @importFrom kit funique countOccur uniqLen
#' @author Sercan Kahveci
#' @description
#' Accurately estimates the reliability of cognitive tasks using a fast and flexible 
#' permutation-based split-half reliability algorithm 
#' that supports stratified splitting while maintaining equal split sizes. 
#' See Kahveci, Bathke, and Blechert (2025) \doi{10.3758/s13423-024-02597-y} for details.
#' To learn more about rapidsplithalf, view the introductory vignette:
#' \code{vignette("rapidsplithalf",package="rapidsplithalf")}
#' 
NULL