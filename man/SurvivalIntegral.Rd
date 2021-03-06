% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CLCalc.R
\name{SurvivalIntegral}
\alias{SurvivalIntegral}
\title{SurvivalIntegral}
\usage{
SurvivalIntegral(homedir = getwd(), fileName = "SurvivalPercentage.csv")
}
\arguments{
\item{homedir}{Path where a csv containing the percent surviving for an experiment lives. default=getwd()}

\item{fileName}{Name of the file with the experimental data. default="SurvivalPercentage.csv"}
}
\description{
Calculate the area under the curve as determined by the survival percentage for an experiment.
}
