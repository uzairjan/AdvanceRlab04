% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lab04.R
\docType{class}
\name{linreg-class}
\alias{linreg-class}
\alias{linreg}
\title{Create a linreg object}
\arguments{
\item{formula}{formula for linear model}

\item{data}{the dataset provided}
}
\value{
the output from the \code{\link{linreg}}]
}
\description{
Create a linreg object
}
\section{Fields}{

\describe{
\item{\code{formula:}}{Linear regression formula .}

\item{\code{data:}}{Recieved data from user.}

\item{\code{X:}}{matrix for containing all the data.}

\item{\code{Y:}}{Response variable.}

\item{\code{beta_estimates}}{matrix:Estimates the variability of the beta coefficients .}

\item{\code{esti_y:}}{Estimation of the Y values.}

\item{\code{residual}}{residuals computed by subtracting esti_y from y values.}

\item{\code{t_params}}{number of parameters.}

\item{\code{deg_freedom:}}{Degrees of freedom .}

\item{\code{residualt}}{for containing calculated Variances value .}

\item{\code{resid_var}}{numeric:Estimates of the variance of the error variable .}

\item{\code{residualstd}}{standard deviation of residuals.}

\item{\code{betavariance}}{variance of beta estimates.}

\item{\code{b_diag}}{diagonal of betavariance matrix.}

\item{\code{tvalues}}{matrix:T-values for significance of coefficients.}

\item{\code{pvalues:}}{computed according to tvalues.}

\item{\code{standardizedresiduals}}{.}

\item{\code{sqrtstresiduals}}{.}

\item{\code{export_formula}}{.}

\item{\code{export_data}}{.}
}}

\section{Methods}{

\describe{
\item{\code{print()}}{Prints Model information}
}}

