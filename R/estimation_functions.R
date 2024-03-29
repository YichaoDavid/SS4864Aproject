#' Final Project Model Fitting
#'
#' @param data the data set for your example
#' @param example the name of your example: one of "culcita", "ctsib", "epilepsy", or "tortoise"
#'
#' @return A list with summary statistics from the fitted model.
#' @export
#'
#' @examples
#' ##### Epilepsy #####
#'
#' ## Load data
#' epilepsy <- read_csv("epilepsy.csv")
#'
#' ## Sum separate observations for each patient in the after period
#' epilepsy <- epilepsy %>%
#'   group_by(id,treat,expind,age) %>%
#'   summarize(seizures = sum(seizures),
#'             .groups = "drop")
#'
#' ## Fit model to epilepsy data. Fixed effects in the model are:
#' ##  (Intercept) -- the intercept
#' ##  age -- age as a continuous predictor
#' ##  expind -- a categorical variable with two levels (0 for before and 1 for after)
#' ##  expind:treat -- a categorical variable with two levels (1 for observations from individuals on the drug in the after period and 0 otherwise)
#'
#' epilepsy_fit <- run_model(epilepsy, "epilepsy")
#'
#' ##### CTSIB #####
#'
#' ## Load data
#' ctsib <- read_csv("ctsib.csv")
#'
#' ## Define response (stable or not)
#' ctsib <- ctsib %>%
#'   mutate(stable = 1 * (CTSIB == 1))
#'
#' ## Fit model to ctsib data. Fixed effects in the model include:
#' ##  (Intercept) -- the intercept
#' ##  Surface -- a categorical variable with two levels (foam and norm)
#' ##  Vision -- a categorical variable with three levels (closed, dome, open)
#'
#' ctsib_fit <- run_model(ctsib, "ctsib")
#'
#' ##### Tortoise #####
#' ## Load data
#' tortoise <- read_csv("gopher_tortoise.csv")
#'
#' ## Fit model to tortoise data. Fixed effects in the model are:
#' ##   (Intercept) -- the intercept
#' ##   prev -- the seroprevalence
#' ##   year -- year effect (as a categorical variable)
#'
#' tortoise_fit <- run_model(tortoise, "tortoise")
#'
#' ##### Culcita #####
#' ## Load data
#' culcita <- read_csv("culcita.csv")
#'
#' ## Fit model to culcita data. Fixed effects in the model are:
#' ##   (Intercept) -- the intercept
#' ##   ttt -- the treatment as a categorical variable with four levels: none, crabs, shrimp, or both
#'
#' culcita_fit <- run_model(culcita, "culcita")
run_model <- function(data, example = "tortoise"){

  ## Fit model
  if(example == "tortoise")
    lmer_fit <- glmer(shells~prev+offset(log(Area))+factor(year)+(1|Site),
                      family=poisson,
                      data=data)

  else if(example == "culcita")
    lmer_fit <- glmer(predation~ttt+(1|block),
                      data=data,
                      family=binomial)

  else if(example == "ctsib")
    lmer_fit <- glmer(stable ~ Surface + Vision + (1|Subject),
                      family = binomial,
                      data = data)

  else if(example == "epilepsy")
    lmer_fit <- glmer(seizures ~ age + expind + expind:treat + (1|id),
                      family = poisson,
                      data = data)
  else
    stop("You must set example to one of: tortoise, culcita, ctsib, or epilspsy.")

  ## Compute model summary
  lmer_summ <- summary(lmer_fit)

  ## Extract coefficients
  coeff <- coefficients(lmer_summ)[,"Estimate"]

  ## Extract t-statistics for each coefficient
  test_stat <- coefficients(lmer_summ)[,"z value"]

  ## Extract random effects
  re <- ranef(lmer_fit)[[1]][[1]]

  ## Extract random effects variance
  sigmasq <- lmer_summ$varcor[[1]][[1]]

  ## Adjust sigmasq
  if(sigmasq == 0){
    sigmasq <- .100

    set.seed(7777)
    re <- rnorm(length(re), sd = sqrt(sigmasq))
  }

  ## Return values
  list(beta = coeff,
       sigmasq = sigmasq,
       re = re, #480/40=12
       test_stat = test_stat)#z-value
}
