#' Multiple linear regression package in R
#' @return
#' @export
#'
#'
#' @examples
#' linreg()
linreg <- setRefClass("linreg",
                      fields = list(
                        sqrtstresiduals = "matrix",
                        export_formula = "formula",
                        export_data = "character"),
                      methods= list(
                        print=function(){},
                        plot =function(){},
                        resid=function(){},
                        pred=function(){},
                        coef=function(){},
                        summary=function(){}
                        )
                      )

