#
#' Mean sample size
#'
#' Calculate sample size needed to estimate a mean within a given margin of error.
#'
#' @param moe margin of error expressed in your measure's units
#' @param sd standard deviation expressed in your measure's units
#' @param conf confidence level associated with margin of error (default = 0.95)
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
#' @param moe margin of error expressed as a proportion (0 < moe < 1)
#' @param p estimated proportion (default = 0.5); leave as is for most conservative estimate.
#' @param conf confidence level associated with margin of error (default = 0.95)
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
#' @param moe margin of error expressed as a proportion (0 < moe < 1)
#' @param p estimated proportion (default = 0.5); leave as is for most conservative estimate.
#' @param conf confidence level associated with margin of error (default = 0.95)
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
#' Calculate sample size (per group) needed to estimate a difference in means within a
#' given margin of error. This function assumes the means come from
#' Normal distributions with common variances. Those are big and likely untrue
#' assumptions, therefore the estimated sample size should be treated as a rough estimate.
#'
#' @param moe margin of error expressed in your measure's units
#' @param sd the common standard deviation of the two populations expressed in your measure's units
#' @param conf confidence level associated with margin of error (default = 0.95)
#'
#' @return Estimated sample size for each group.
#'
#' @references Hogg, R. V. and Tanis, E. A. (2006), \emph{Probability and Statistical Inference 7e}, Pearson, New Jersey.
#
#' @examples
#' ## Approximate sample size needed to estimate the difference in means between two
#' ## populations within 1.5 units, each with standard deviation 20 units.
#' dmeanSampleSize(moe = 1.5, sd = 20)
#'
#' @export
dmeanSampleSize <- function(moe, sd, conf=0.95){
  if (!all(sapply(list(moe,sd,conf),is.numeric)))
    stop("All arguments must be numbers.")
  if(moe < 0) stop("moe must be a positive number")
  n <- (qnorm(p = (1 - conf)/2, lower.tail = FALSE) * sd * sqrt(2) / moe)^2
  cat(ceiling(n), "per group \n")
}



#' Difference in proportions sample size
#'
#' Calculate sample size (per group) needed to estimate a difference in proportions within a
#' given margin of error. This function is derived from the classic approximate
#' confidence interval formula for the difference in proportions (Hogg & Tanis, p. 383).
#' The estimated sample size should be treated as a rough estimate.
#'
#'
#' @param moe margin of error expressed as proportion (0 < moe < 1)
#' @param p1 estimated proportion in group 1 (default = 0.5)
#' @param p2 estimated proportion in group 2 (default = 0.5)
#' @param conf confidence level associated with margin of error (default = 0.95)
#'
#' @return Estimated sample size for each group.
#'
#' @references Hogg, R. V. and Tanis, E. A. (2006), \emph{Probability and
#'   Statistical Inference 7e}, Pearson, New Jersey.
#'
#' @examples
#' ## Approximate sample size needed to estimate the difference in proportions between two
#' ## binomial populations within 0.02, one with p = 0.11 and the other with p = 0.09.
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


