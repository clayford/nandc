
#' mean sample size and cutoff
#'
#' given type I error, type II error, standard deviation, null mean, and
#' alternative mean, return cutoff (critical region boundary) and sample size
#'
#' @param alpha Type I error
#' @param beta Type II error
#' @param null the null mean value
#' @param alt the alternative mean value
#' @param sd standard deviation of null and alternative distributions
#' @param direction less than or greater than
#'
#' @return cutoff (critical region boundary) and sample size given type I error,
#' type II error, standard deviation, null mean, and alternative mean,
#' @export
#'
#' @examples
#' # example 9.1-2
#' meanSampCutoff(a = 0.025,b = 0.05,null = 60,alt = 65,sd = 10)
#' # problem 9.1-5
#' meanSampCutoff(a = 0.05,b = 0.05,null = 1.5,alt = 1.7,sd = sqrt(0.09))
#' # problem 9.1-7
#' meanSampCutoff(a = 0.05,b = 0.10,null = 715,alt = 650,sd = 140, direction = "less.than")

meanSampCutoff <- function(alpha,beta,null,alt,sd,
                         direction=c("greater.than","less.than")){
  if (!all(sapply(list(alpha,beta,null,alt,sd),is.numeric)))
    stop("alpha, beta, null, alt and sd must be numbers.")
  direction <- match.arg(direction)
  tdirection <- switch(direction, greater.than = 1, less.than = -1)
  a <- qnorm(p = 1 - alpha)*tdirection
  b <- qnorm(p = beta)*tdirection
  n <- ((a + -b)*sd/(alt - null))^2
  c <- b*(sd/sqrt(n)) + alt
  list(cutoff = c, `sample size`=ceiling(n))
}



#' proportion sample size and cutoff
#'
#' given type I error, type II error, null proportion, and
#' alternative proportion, return cutoff (critical region boundary) and sample size
#'
#' @param alpha Type I error
#' @param beta Type II error
#' @param null null proportion
#' @param alt alternative proportion
#' @param direction less than or greater than
#'
#' @return cutoff (critical region boundary) and sample size given type I error,
#' type II error, null proportion, and alternative proportion
#' @export
#'
#' @examples
#' # example 9.1-3
#' propSampCutoff(alpha = 0.05,beta = 0.1,null = 0.5,alt = 0.25,
#' direction = "less.than")
#' # problem 9.1-11
#' propSampCutoff(alpha = 0.05,beta = 0.1,null = 1/26,alt = 1/10)

propSampCutoff <- function(alpha,beta,null,alt,
                         direction=c("greater.than","less.than")){
  if (!all(sapply(list(alpha,beta,null,alt),is.numeric)))
    stop("alpha, beta, null, and alt must be numbers.")
  direction <- match.arg(direction)
  tdirection <- switch(direction, greater.than = 1, less.than = -1)
  a <- qnorm(p = 1 - alpha)*tdirection
  b <- qnorm(p = beta)*tdirection
  n <- ((-a*sqrt(null*(1-null)) + b*sqrt(alt*(1-alt)))/(null - alt))^2
  c <- b*sqrt(n*alt*(1-alt)) + n*alt
  list(cutoff = c, `sample size`=ceiling(n))
}
