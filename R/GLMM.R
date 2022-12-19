#' Generalized linear mixed effects model function package with data set CTSIB.csv
#'
#' @param data the data set for analysis
#'
#' @return A list with summary statistics from the fitted model including fixed effect, random variance, and random effect.
#' @import tidyverse
#' @import lme4
#' @export GLMM
#'
#' @usage GLMM(data)
#' @author Yichao Liu, Weixuan Zhang
#' @references
#' Leppik, I.E., et al. (1985) A double-blind crossover evaluation of progabide in partial seizures.Neurology 35(285).
#' McKeon CS, Stier AC, McIlroy SE, Bolker BM. Multiple defender effects: synergistic coral defense by mutualist crustaceans. Oecologia. 2012 Aug;169(4):1095-103.
#' Ozgul et al. (2009) Upper respiratory tract disease, force of infection, and effects on survival of gopher tortoises. Ecological Applications 19(3):786-798.
#' Steele, R. (1998). Effect of Surface and Vision on Balance. Ph.D. Thesis, Department of Physiotherapy, University of Queensland.
#'
#' @examples
#' model <- GLMM("ctsib.csv")
#' the parameter should be a csv data set file
#' GLMM function can create a GLMM model, that will return summary of statistics after analysis.

# add the GLMM function
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
  #transfer the result from the function to the tibble set
  Bootstrap_result <- as_tibble(Bootstrap_result)
  # create standard deviation variable
  SD <- sapply(Bootstrap_result,sd)
  #create test statistics variable
  TS <- ctsib_fit$test_stat
  # create the p_value variable
  PR <- 2*pnorm(-abs(TS))
  # sum the new variables to new tibble set
  fixed.effect <- tibble(
    coef. = ctsib_fit$beta,
    std.err = SD[1:4],
    # use coef and std.err to sum CI, the target result
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
