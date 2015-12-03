% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/samplesize.R
\name{dmeanSampleSize}
\alias{dmeanSampleSize}
\title{difference in means sample size}
\usage{
dmeanSampleSize(moe, psd, conf = 0.95)
}
\arguments{
\item{moe}{margin of error}

\item{psd}{pooled estimate of the common standard deviation (assuming that
the variances in the populations are similar)}

\item{conf}{confidence level (default = 0.95)}
}
\value{
Estimated sample size needed to estimate a difference in population
  means within a given margin of error. The sample size is for each group.
}
\description{
difference in means sample size
}
\examples{
dmeanSampleSize(moe = 1.26, psd = 19)

}
\references{
Hogg, R. V. and Tanis, E. A. (2006), \emph{Probability and Statistical Inference 7e}, Pearson, New Jersey.
}
