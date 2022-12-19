library(tidyverse)
library(lme4)
#' GLMM package with CTSIB.csv
#'
#' @param data the data set for my example
#'
#' @return A list with summary statistics from the fitted model
#' @export GLMM()
#'
#' @examples
#' model <- GLMM("ctsib.csv")
#' GLMM function can create a GLMM model, whose summary statistics are returned.

GLMM <- function(data="ctsib.csv")
{
  ctsib = read.csv(data)
  ctsib <- ctsib %>%
    mutate(stable = 1 * (CTSIB == 1))
  ctsib_fit <- run_model(ctsib, "ctsib")
  n=100
  Bootstrap_result=matrix(nrow=n,ncol=5)
  for (i in 1:n){
    S = sample(1:480,size=480,replace=TRUE)
    X = ctsib[S,]
    X_fit <- run_model(X,"ctsib")
    Bootstrap_result[i,1:4] = X_fit$beta
    Bootstrap_result[i,5] = X_fit$sigmasq
  }
  Bootstrap_result <- as_tibble(Bootstrap_result)
  SD <- sapply(Bootstrap_result,sd)
  TS <- ctsib_fit$test_stat
  PR <- 2*pnorm(-abs(TS))
  fixed.effect <- tibble(
    coef. = ctsib_fit$beta,
    std.err = SD[1:4],
    ci.lower = coef. - 1.96 * std.err,
    ci.upper = coef. + 1.96 * std.err,
    Z.value = TS,
    P.value = PR
  )
  random.variance <- tibble(
    sigmasq = ctsib_fit$sigmasq,
    std.err = SD[5],
    ci.lower = sigmasq - 1.96 * std.err,
    ci.upper = sigmasq + 1.96 * std.err,
  )
  Return <- list(
    fixed.effect = fixed.effect,
    random.variance = random.variance,
    random.effect = ctsib_fit$re
  )
  return(Return)
}
