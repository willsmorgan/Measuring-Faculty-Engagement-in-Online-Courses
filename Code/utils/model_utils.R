### Model Utils

# This script is a repository for any supplementary code used in to run models


#------------------------------------------------------------------------------#

runCountMods <- function(formulas, data, family) {
  ### Run Poisson, Quasipoisson, or Negative Binomial Regression on a list 
  ### of formulas 
  
  ### Null model is automatically run and does not have to be specified
  
  # Extract dv from first formula to run null model
  null_form <- update.formula(formulas[[1]], ~ 1)
  
  if (family != "nb") {  
    
    # Run null model
    null_mod <- list("null" = glm(null_form, family = family, data = data,
                                  control = glm.control(maxit = 200)))
    
    # Estimate models for given formulas
    results <- map(formulas, function(x) glm(formula = x, family = family, data = data,
                                             control = glm.control(maxit = 200)))
    
  } else if (family == "nb") {
    
    # Run null model
    null_mod <- list("null" = MASS::glm.nb(null_form, data = data,
                                           control = glm.control(maxit = 200)))
    
    # Estimate models for given formulas
    results <- map(formulas, function(x) MASS::glm.nb(formula = x, family = family, data = data,
                                                      control = glm.control(maxit = 200)))
    
  }
  
  return(c(null_mod, results))
  
}

checkFit <- function(models) {
  ### Run LR test of models against null
  ### Run Chisq. on sig. of coefficients

  # Test of difference in deviances (useful for QP models that don't report LL)
  fitTest <- function(proposed) {
    
    null_dev <- proposed$null.deviance
    null_df <-  proposed$df.null
    
    proposed_dev <- proposed$deviance
    proposed_df <- proposed$df.residual
    
    p_val <- 1 - pchisq(null_dev - proposed_dev, null_df - proposed_df)
    
    return(p_val)
  }
  
  null <- models[["Null"]]
  
  fits <- models
  fits[["Null"]] <- NULL

  # Likelihood ratio test
  gof_test <- map(fits, function(x) lrtest(null, x))
  
  # if family is quasipoisson, run instead a test for difference in deviances
  if (null$family[[1]] == "quasipoisson") {
    gof_test <- map(fits, fitTest)
  }
  
  # chisq coef tests
  coef_test <- map(fits, function(x) anova(x, test = 'Chisq'))
  
  # test results
  out <- list(
    "gof" = gof_test,
    "coef" = coef_test
  )
  
  return(out)
  
}

avMarginalFX <- function(model) {
  
  err <- summary(model)$coefficients[, 2]
  coef <- summary(model)$coefficients[, 1]
  coef_hi <- coef + err
  coef_low <- coef - err
  
  # exp <- exp(coef) - 1
  # exp_hi <- exp(coef_hi) - 1
  # exp_low <- exp(coef_low) - 1
  
  fitted <- predict(model, model$model)
  
  effects <- data.frame(
    avg_effect_hi = sapply(coef_hi, function(x) mean(fitted*x)),
    avg_effect_low = sapply(coef_low, function(x) mean(fitted*x)),
    avg_effect = sapply(coef, function(x) mean(fitted*x)),
    se = err
  )
  
  return(effects)
}

addName <- function(list, name) {
  ### Add name of list (model type) as column in DF
  list <- map(list, function(x){
    x <- as.data.table(x)
    x[, model := name]
  })
  
  return(list)
}