#' Create a linreg object
#'
#' @field formula: Linear regression formula .
#' @field data: Recieved data from user.
#' @field X: matrix for containing all the data.
#' @field Y: Response variable.
#' @field beta_estimates matrix:Estimates the variability of the beta coefficients .
#' @field esti_y: Estimation of the Y values.
#' @field residual residuals computed by subtracting esti_y from y values.
#' @field t_params  number of parameters.
#' @field deg_freedom: Degrees of freedom .
#' @field residualt for containing calculated Variances value .
#' @field resid_var numeric:Estimates of the variance of the error variable .
#' @field residualstd standard deviation of residuals.
#' @field betavariance variance of beta estimates.
#' @field b_diag diagonal of betavariance matrix.
#' @field tvalues matrix:T-values for significance of coefficients.
#' @field pvalues: computed according to tvalues.
#' @field standardizedresiduals .
#' @field sqrtstresiduals .
#' @field export_formula .
#' @field export_data .
#' @param formula formula for linear model
#' @param data the dataset provided
#' @import methods
#' @return the output from the \code{\link{linreg}}]
#' @importFrom ggplot2 theme_linedraw theme element_blank element_text stat_summary ggtitle xlab scale_x_continuous ggplot aes geom_point geom_smooth labs %+replace% theme_bw rel element_rect element_text element_line margin unit
#' @exportClass linreg
#' @export linreg
linreg <- setRefClass("linreg",
                      fields = list(formula="formula",
                                    data="data.frame",
                                    X = "matrix",
                                    Y="matrix",
                                    beta_estimates = "matrix",
                                    esti_y="matrix",
                                    residual="matrix",
                                    t_params = "integer",
                                    deg_freedom="integer",
                                    residualt = "matrix",
                                    resid_var="numeric",
                                    residualstd = "numeric",
                                    betavariance = "matrix",
                                    b_diag = "numeric",
                                    tvalues="matrix",
                                    pvalues= "matrix",
                                    standardizedresiduals ="matrix",
                                    sqrtstresiduals = "matrix",
                                    export_formula = "formula",
                                    export_data = "character"
                      ),
                      methods = list(
                        initialize = function(formula, data){
                          formula<<-formula
                          data<<-data
                          #slope and intercept
                          X <<- model.matrix(formula, data)
                          Y <<- as.matrix(data[all.vars(formula)[1]])
                          # Transpose matrix X, multiply and  solve
                          beta_estimates <<- solve((t(X)%*%X))%*%t(X)%*%Y
                          # Estimate y
                          esti_y <<- X %*% beta_estimates
                          # Estimate residuals
                          residual <<- Y - esti_y
                          # degrees of freedom
                          t_params <<- length(beta_estimates)
                          deg_freedom <<- length(Y) - t_params
                          # calculate Variances
                          residualt <<- t(residual)
                          resid_var <<- as.numeric((residualt %*% residual) / deg_freedom)
                          residualstd <<- sqrt(resid_var)
                          betavariance <<- resid_var * solve((t(X)) %*%X)
                          b_diag <<- diag(betavariance)
                          # t-values
                          tvalues <<- beta_estimates/sqrt(b_diag)
                          # p-values
                          pvalues <<- 2 * pt(abs(tvalues), deg_freedom, lower.tail = FALSE)
                          # Standardized residuals for summary
                          standardizedresiduals <<- residual / sd(residual)
                          sqrtstresiduals <<- sqrt(abs(standardizedresiduals))

                          # saving names
                          export_formula <<- formula
                          export_data <<- deparse(substitute(data))
                        },
                        print = function(){
                          "Prints Model information"
                          cat(paste("linreg(formula = ", format(export_formula), ", data = ", export_data , ")\n\n ", sep = ""))
                          setNames(round(beta_estimates[1:nrow(beta_estimates)],3),rownames(beta_estimates))
                        },
                        resid = function(){
                          return(as.vector(residual))
                        },
                        pred = function(){
                          return(esti_y)
                        },
                        coef = function(){
                          vec <- as.vector(beta_estimates)
                          names(vec) <- colnames(X)
                          return(vec)
                        },
                        plot = function(size = 12, base_family = "") {
                          theme_liu <- function(){
                            bisect_line <- size/2
                            theme(line = element_line(colour = "black", size = 0.5, linetype = 1,
                                                      lineend = "butt"), rect = element_rect(fill = "#323232",
                                                                                             colour = "black", size = 0.5, linetype = 1), text = element_text(family = base_family,
                                                                                                                                                              face = "plain", colour = "#54d8e0", size = size, lineheight = 0.9,
                                                                                                                                                              hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),
                                  axis.line   = element_line(),
                                  axis.line.x = element_blank(),
                                  axis.line.y = element_blank(),
                                  axis.text   = element_text(size = rel(0.8), colour = "#54d8e0"),
                                  axis.text.x = element_text(margin = margin(t = 0.8 * bisect_line/2), vjust = 1), axis.text.y = element_text(margin = margin(r = 0.8 * bisect_line/2), hjust = 1),
                                  axis.ticks  = element_line(colour = "#ffffff"),
                                  axis.ticks.length = unit(bisect_line/2, "pt"),
                                  axis.title.x = element_text(margin = margin(t = 0.8 * bisect_line, b = 0.8 * bisect_line/2)),
                                  axis.title.y = element_text(angle = 90, margin = margin(r = 0.8 * bisect_line, l = 0.8 * bisect_line/2)),
                                  legend.background = element_rect(colour = "#ffffff"), legend.key = element_rect(fill = "#ffffff", colour = "white"), legend.key.size = unit(1.2, "lines"),
                                  legend.key.height = NULL,
                                  legend.key.width = NULL,
                                  legend.text = element_text(size = rel(0.8)),
                                  legend.text.align = NULL,
                                  legend.title = element_text(hjust = 0),
                                  legend.title.align = NULL,
                                  legend.position = "right",
                                  legend.direction = NULL,
                                  legend.justification = "center",
                                  legend.box = NULL,
                                  panel.background = element_rect(fill = "#3e4d4f", colour = NA),
                                  panel.border = element_blank(),
                                  panel.grid.major = element_line(colour = "#dddddd"),
                                  panel.grid.minor = element_line(colour = "#cccccc", size = 0.25),
                                  panel.margin.y = NULL, panel.ontop = FALSE,
                                  strip.background = element_rect(fill = "#ffffff", colour = NA),
                                  strip.text = element_text(colour = "#ffffff", size = rel(0.8)),
                                  strip.text.x = element_text(margin = margin(t = bisect_line,b = bisect_line)),
                                  strip.text.y = element_text(angle = -90, margin = margin(l = bisect_line, r = bisect_line)),
                                  strip.switch.pad.grid = unit(0.1, "cm"), strip.switch.pad.wrap = unit(0.1, "cm"),
                                  plot.background = element_rect(colour = "white"),
                                  plot.title = element_text(size = rel(1.2),
                                                            margin = margin(b = bisect_line * 1.2)),
                                  plot.margin = margin(bisect_line, bisect_line, bisect_line, bisect_line),complete = TRUE)
                          }
                          initial_plot <- ggplot(data.frame(esti_y, residual), aes(y=residual, x=esti_y)) + geom_point(shape=21, size=3, colour="black", fill="white")
                          initial_plot <- initial_plot + theme_liu()

                          initial_plot <- initial_plot + stat_summary(fun=median, colour="red", geom="line", aes(group = 1))
                          initial_plot <- initial_plot + ggtitle("Residuals vs fitted") + xlab(paste("Fitted values \n lm(Petal.Length ~ Species)"))
                          secondary_plot <- ggplot(data.frame(esti_y, sqrtstresiduals), aes(y=sqrtstresiduals, x=esti_y)) + geom_point(alpha = 0.6, shape=21, size=3, colour="black", fill="white")
                          secondary_plot <- secondary_plot + theme_liu()
                          secondary_plot <- secondary_plot + stat_summary(fun=median, colour="red", geom="line", aes(group = 1))
                          secondary_plot <- secondary_plot + ggtitle("Scale-Location") + xlab(paste("Fitted values \n lm(Petal.Length ~ Species)"))
                          secondary_plot <- secondary_plot + scale_x_continuous(breaks = seq(0.0, 1.5, by= 0.5))
                          plotlist <- list(initial_plot, secondary_plot)
                          return(plotlist)
                        },
                        summary = function(){
                          l <- list()
                          m = matrix(NA,t_params,4)
                          m[,1] = beta_estimates
                          m[,2] = sqrt(diag(betavariance))
                          m[,3] = tvalues
                          m[,4] = (1-pt(tvalues, deg_freedom))*2
                          colnames(m) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
                          rownames(m) <- colnames(X)
                          l$matrix <- m
                          l$variance <- resid_var
                          cat(format("",width = 12))
                          cat(format(colnames(m), width=20, justify="right"),'\n')
                          for(i in 1:nrow(m)){
                            cat(format(rownames(m)[i], width = 20, justify = "left"))
                            if(m[i,4]<0.001)
                              cat(format(round(m[i,], digits = 5), width=20, justify="right", scientific = F), format("***", width=20, justify="left"), '\n')
                            else if(m[i,4]<0.01)
                              cat(format(round(m[i,], digits = 5), width=20, justify="right", scientific = F),format("**", width=20, justify="left"),'\n')
                            else if(m[i,4]<0.05)
                              cat(format(round(m[i,], digits = 5), width=20, justify="right", scientific = F),format("*", width=20, justify="left"),'\n')
                            else if(m[i,4]<0.1)
                              cat(format(round(m[i,], digits = 5), width=20, justify="right", scientific = F),format(".", width=20, justify="left"),'\n')
                            else
                              cat(format(round(m[i,], digits = 5), width=20, justify="right", scientific = F),format(" ", width=20, justify="left"),'\n')

                          }
                          cat("Residual standard error:", round(sqrt(resid_var), 3), "on", deg_freedom, "degrees of freedom")
                        }
                      ))

