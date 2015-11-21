#' mean sample size
#'
#' Calculate sample size needed to estimate a mean within a given margin of error.
#'
#' @param moe margin of error.
#' @param sd standard deviation.
#' @param conf confidence level (default = 0.95)
#'
#' @return Estimated sample size needed to estimate a mean within a given margin
#'   of error.
#'
#' @examples
#' meanSampleSize(moe = 1, sd = 15)
meanSampleSize <- function(moe, sd, conf=0.95){
  if (!all(sapply(list(moe,sd,conf),is.numeric)))
    stop("All arguments must be numbers.")
  n <- (qnorm(p = (1 - conf)/2, lower.tail = FALSE) * sd / moe)^2
  ceiling(n)
}


#' proportion sample size
#'
#' Calculate sample size needed to estimate a proportion within a given margin
#' of error.
#'
#' @param moe margin of error
#' @param p estimated proportion (default = 0.5)
#' @param conf confidence level (default = 0.95)
#'
#' @return Estimated sample size needed to estimate a proportion within a given
#'   margin of error.
#' @export
#'
#' @examples
#' propSampleSize(moe=0.03)
propSampleSize <- function(moe, p=0.5, conf=0.95){
  if (!all(sapply(list(moe,p,conf),is.numeric)))
    stop("All arguments must be numbers.")
  n <- (qnorm(p = (1 - conf)/2, lower.tail = FALSE) / moe)^2 * p * (1 - p)
  ceiling(n)
}

#' proportion sample size - finite (known) population
#'
#' Calculate sample size needed to estimate a proportion within a given margin
#' of error for a finite (known) population size.
#'
#'
#' @param N population size
#' @param moe margin of error
#' @param p estimated proportion (default = 0.5)
#' @param conf confidence level (default = 0.95)
#'
#' @return Estimated sample size needed to estimate a proportion within a given
#'   margin of error for a finite (known) population size.
#' @export
#'
#' @examples finitePropSampleSize(N=3000, moe=0.03)
finitePropSampleSize <- function(N, moe, p=0.5, conf=0.95){
  if (!all(sapply(list(N,moe,p,conf),is.numeric)))
    stop("All arguments must be numbers.")
  m <- prop.sample.size(moe,p,conf)
  n <- m / (1 + (m - 1)/N)
  ceiling(n)
}


