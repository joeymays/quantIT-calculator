
getRegression <- function(x, y){
  
  fit <- lm(y ~ x)
  
  return(list(yint = fit$coefficients[1], slope = fit$coefficients[2], rsq = summary(fit)$r.squared))
}