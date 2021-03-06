% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/design_function.R
\name{design}
\alias{design}
\title{Design Function for sample size recalculation}
\usage{
design(n1, alpha_glob, n2, alpha_0, beta, b)
}
\arguments{
\item{n1}{An integer. Interim sample size per group at stage one.}

\item{alpha_glob}{Numeric. Global significance level (range [0, 1]).}

\item{n2}{An integer. Initial incremental sample size per group for stage two.}

\item{alpha_0}{Numeric. Futility bound for stage one (range [0, 1]).}

\item{beta}{Numeric. Desired type II error (range [0, 1]).}

\item{b}{Numeric. Factor for maximal sample size (nmax = b * n1), b > 1.}
}
\value{
The function returns an object of class \code{"TwoStageDesign"}, which is a list with
the following components:
\itemize{n1} The interim size per group at stage one (as given to the function).
\itemize{alpha_1} The local efficacy bound for a two stage design according to Pocock.
\itemize{n2} The initial incremental sample size per group for stage two (as given to the function).
\itemize{alpha_0} The futility bound for stage one (as given to the function).
\itemize{b} The factor for maximal sample size (as given to the function).
\itemize{alpha_glob} The global significance level (as given to the function).
\itemize{beta} The desired type II error (as given to the function).
}
\description{
Specifies the design of an adaptive two-stage clinical trial.
Provides a frame to conduct sample size recalculation.
}
\details{
This function is necessary for the recalculation functions of this package
(\code{ocp_design, rocp_design, pz_design, opt_design}). It calculates
the local significance level according to Pocock (1977) and provides the frame that
each recalculation function of \code{bootSSR} needs.
}
\examples{
design <- design(50, .05, 50, .5, .2, 4)
}
