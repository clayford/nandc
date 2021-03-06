% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_and_c.R
\name{meanSampCutoff}
\alias{meanSampCutoff}
\title{Mean sample size and cutoff}
\usage{
meanSampCutoff(alpha, beta, null, alt, sd, direction = c("greater", "less"))
}
\arguments{
\item{alpha}{Type I error}

\item{beta}{Type II error}

\item{null}{the null mean value}

\item{alt}{the alternative mean value}

\item{sd}{standard deviation of null and alternative distributions}

\item{direction}{"greater" (default) or "less"}
}
\value{
Cutoff (critical region boundary) and sample size given type I error,
type II error, standard deviation, null mean, and alternative mean.
}
\description{
Given type I error, type II error, standard deviation, null mean, and
alternative mean, return cutoff (critical region boundary) and sample size.
}
\examples{
# example 9.1-2
meanSampCutoff(a = 0.025,b = 0.05,null = 60,alt = 65,sd = 10)
# problem 9.1-5
meanSampCutoff(a = 0.05,b = 0.05,null = 1.5,alt = 1.7,sd = sqrt(0.09))
# problem 9.1-7
meanSampCutoff(a = 0.05,b = 0.10,null = 715,alt = 650,sd = 140, direction = "less")

}
\references{
Hogg, R. V. and Tanis, E. A. (2006), \emph{Probability and Statistical Inference 7e},
Pearson, New Jersey. Chapter 9.1.
}

