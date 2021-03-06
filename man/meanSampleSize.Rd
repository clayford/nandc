% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/samplesize.R
\name{meanSampleSize}
\alias{meanSampleSize}
\title{Mean sample size}
\usage{
meanSampleSize(moe, sd, conf = 0.95)
}
\arguments{
\item{moe}{margin of error expressed in your measure's units}

\item{sd}{standard deviation expressed in your measure's units}

\item{conf}{confidence level associated with margin of error (default = 0.95)}
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
\references{
Hogg, R. V. and Tanis, E. A. (2006), \emph{Probability and Statistical Inference 7e}, Pearson, New Jersey.
}

