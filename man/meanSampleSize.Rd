% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/samplesize.R
\name{meanSampleSize}
\alias{meanSampleSize}
\title{mean sample size}
\usage{
meanSampleSize(moe, sd, conf = 0.95)
}
\arguments{
\item{moe}{margin of error.}

\item{sd}{standard deviation.}

\item{conf}{confidence level (default = 0.95)}
}
\value{
Estimated sample size needed to estimate a mean within a given margin
  of error.
}
\description{
Calculate sample size needed to estimate a mean within a given margin of error.
}
\examples{
meanSampleSize(moe = 1, sd = 15)
}
