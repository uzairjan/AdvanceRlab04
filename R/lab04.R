#' Multiple linear regression package in R
#' @param  formula linear model formula
#' @param data dataset for linear regression
#' @field formaula linear model formula
#' @field X the matrix independent variable
#' @field Y the matrix of dependent variable
#' @field X_trans transpose indepent matrix variable
#' @field xtx, opposit of multiplying X_t and m_X matrix
#' @field betaestimated,   parameters estimation
#' @field yfit   estimated values of y
#' @field  residual, residuals as a result of subtracting yfit from actual y values
#' @field nparameters, how much parameters
#' @field residualvariance, variance calculated of residuals
#' @field stnresid, sd of residuals
#' @field betavarience, variance as a result of beta estimates
#' @field bb, digonaly betavariance matrix
#' @field tvalues, t values of every parameter calculated
#' @field pvalues, calculation of pvalues as reported by pt function and tvalues
#' @field standradized residuals, standradizedresiduals
#' @field sqtresiduals, the underoot of standradizedresiduals
#'
#' @return nothing
#' @export
#'
#'
#' @examples
#' linreg()
linreg <- setRefClass("linreg",
                      fields = list(formula="formula",
                                    data="data.frame",
                                    x="matrix",
                                    y="matrix",
                                    x="matrix",
                                    xtx="matrix",
                                    betaestimated="matrix",
                                    yfit="matrix",
                                    residual="matrix",
                                    nparameters="integer",
                                    dof="integer",
                                    residualvariance="numeric",
                                    stnresid="numeric",
                                    betavariance=
                                    bb="numeric",
                                    tvalues="matrix",
                                    pvalues="matrix",
                                    standradizedresiduals="matrix",
                                    sqtresiduals="matrix"
                                    ),


                      methods= list(
                        print=function(){},
                        plot =function(){},
                        resid=function(){},
                        pred=function(){},
                        coef=function(){},
                        summary=function(){}
                        )
                      )

