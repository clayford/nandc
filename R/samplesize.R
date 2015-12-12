#
#' Mean sample size
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
#' @references Hogg, R. V. and Tanis, E. A. (2006), \emph{Probability and Statistical Inference 7e}, Pearson, New Jersey.
#'
#' @examples
#' meanSampleSize(moe = 1, sd = 15)
#'
#' @export

meanSampleSize <- function(moe, sd, conf=0.95){
  if (!all(sapply(list(moe,sd,conf),is.numeric)))
    stop("All arguments must be numbers.")
  if(moe < 0) stop("moe must be a positive number")
  n <- (qnorm(p = (1 - conf)/2, lower.tail = FALSE) * sd / moe)^2
  ceiling(n)
}


#' Proportion sample size
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
#'
#' @references Hogg, R. V. and Tanis, E. A. (2006), \emph{Probability and Statistical Inference 7e}, Pearson, New Jersey.
#'
#' @examples
#' propSampleSize(moe=0.03)
#'
#' @export

propSampleSize <- function(moe, p=0.5, conf=0.95){
  if (!all(sapply(list(moe,p,conf),is.numeric)))
    stop("All arguments must be numbers.")
  if(moe < 0) stop("moe must be a positive number")
  if(moe > 1 || p > 1) warning("moe or p was entered as greater than 1")
  if(p < 0) warning("p was entered as less than 0")
  n <- (qnorm(p = (1 - conf)/2, lower.tail = FALSE) / moe)^2 * p * (1 - p)
  ceiling(n)
}

#' Proportion sample size - finite (known) population
#'
#' Calculate sample size needed to estimate a proportion within a given margin
#' of error for a finite (known) population size.
#'
#'
#' @param N population size
#' @param moe margin of error
#' @param p estimated proportion (default = 0.5); leave as is for most conservative estimate.
#' @param conf confidence level (default = 0.95)
#'
#' @return Estimated sample size needed to estimate a proportion within a given
#'   margin of error for a finite (known) population size.
#'
#' @references Hogg, R. V. and Tanis, E. A. (2006), \emph{Probability and Statistical Inference 7e}, Pearson, New Jersey.
#'
#'
#' @examples
#' finitePropSampleSize(N=3000, moe=0.03)
#'
#' @export

finitePropSampleSize <- function(N, moe, p=0.5, conf=0.95){
  if (!all(sapply(list(N,moe,p,conf),is.numeric)))
    stop("All arguments must be numbers.")
  if(moe < 0) stop("moe must be a positive number")
  if(p > 1) warning("p was entered as greater than 1")
  if(p < 0) warning("p was entered as less than 0")
  m <- propSampleSize(moe,p,conf)
  n <- m / (1 + (m - 1)/N)
  ceiling(n)
}


#' Difference in means sample size
#'
#' @param moe margin of error
#' @param psd pooled estimate of the common standard deviation (assuming that
#'   the variances in the populations are similar)
#' @param conf confidence level (default = 0.95)
#'
#' @return Estimated sample size needed to estimate a difference in population
#'   means within a given margin of error. The sample size is for each group.
#'
#' @references Hogg, R. V. and Tanis, E. A. (2006), \emph{Probability and Statistical Inference 7e}, Pearson, New Jersey.
#
#' @examples
#' dmeanSampleSize(moe = 1.26, psd = 19)
#'
#' @export
dmeanSampleSize <- function(moe, psd, conf=0.95){
  if (!all(sapply(list(moe,psd,conf),is.numeric)))
    stop("All arguments must be numbers.")
  if(moe < 0) stop("moe must be a positive number")
  n <- (qnorm(p = (1 - conf)/2, lower.tail = FALSE) * psd * sqrt(2) / moe)^2
  cat(ceiling(n), "per group \n")
}



#' Difference in proportions sample size
#'
#' @param moe margin of error
#' @param p1 estimated proportion in group 1 (default = 0.5)
#' @param p2 estimated proportion in group 2 (default = 0.5)
#' @param conf confidence level (default = 0.95)
#'
#' @return Estimated sample size needed to estimate a difference in proportions
#'   within a given margin of error. The sample size is for each group.
#'
#' @references Hogg, R. V. and Tanis, E. A. (2006), \emph{Probability and Statistical Inference 7e}, Pearson, New Jersey.
#'
#' @examples
#' dpropSampleSize(moe = 0.02, p1 = 0.11, p2 = 0.09)
#'
#' @export
dpropSampleSize <- function(moe, p1=0.5, p2=0.5, conf=0.95){
  if (!all(sapply(list(moe,p1,p2,conf),is.numeric)))
    stop("All arguments must be numbers.")
  if(moe < 0) stop("moe must be a positive number")
  if(moe > 1 || p1 > 1 || p2 > 1) warning("One or more of moe, p1 or p2 was entered as greater than 1")
  if(p1 < 0 || p2 < 0) warning("p1 or p2 was entered as less than 0")
  n <- (qnorm(p = (1 - conf)/2, lower.tail = FALSE) / moe)^2 * (p1 * (1 - p1) + p2 * (1 - p2))
  cat(ceiling(n), "per group \n")
}


