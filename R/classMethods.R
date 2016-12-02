# TODO: Roxygenize these
fitted.krr <- function(krrm) krrm$pred

resid.krr <- function(krrm) krrm$residuals

MSE <- function(x) UseMethod("MSE", x)
MSE.krr <- function(krrm) krrm$MSE

plot.krr <- function(krrm) {
  x <- as.matrix(krrm$x)
  p <- ncol(x)
  if(p != 1) {
    stop("ERROR: Can only plot univariate predictors!")
  }
  
  y <- krrm$pred - krrm$residuals
  df <- data.frame(x, y, yhat = krrm$pred)
  df <- df[order(df$x),]
  plot(df$x, df$y, pch = 16, xlab = "x", ylab = "y")
  lines(df$x, df$yhat, col = "red", lwd = 2)
}