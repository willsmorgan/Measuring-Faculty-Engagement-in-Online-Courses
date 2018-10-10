#### Modeling
#### Author: William Morgan

# 1. Var summaries - checking for overdisperson, KS test
# 2. QP, NB regression

# NOTES:
#   IVs of interest: faculty_response, faculty_owner
#   DVs: num_responses, thread_width, avg_stu_response_len

#------------------------------------------------------------------------------#
## 0. Setup

libs <- c("data.table", "dplyr", "magrittr", "ggplot2", "forcats", "purrr", "AER")
lapply(libs, library, character.only = TRUE)
source("Code/utils/model_utils.R")

data <- fread("Data/thread_cleaned.csv") %>%
  mutate(faculty_owner = if_else(thread_owner_role == "P", "Y", "N"),
         faculty_responded = if_else(faculty_responses > 0, "Y", "N")) %>%
  as.data.table()

# get rid of obscene outliers (anything beyond 99.5% quantile - see Preliminary Analysis for details)
subjects <- c("ENG", "PSY", "COM", "BIS", "OGL")

data %<>%
  filter(thread_participants <= 15,
         thread_length <= 32,
         student_responses <= 30,
         studs_in_thread <= 15,
         !is.na(parent_views),
         !is.na(thread_width)) %>%
  as.data.table()


data %<>%
  filter(strm %in% c(2167, 2171, 2177, 2181)) %>%
  as.data.table()

#------------------------------------------------------------------------------#
## 1. Number of Student Responses

# Grab data
nsr <- data[subject == "ENG", .(student_responses, faculty_responded, faculty_owner,
                                     parent_views, parent_post_length)][
                                       , parent_post_length := log(1 + parent_post_length)]


# Define models
formulas <- list(
  as.formula(student_responses ~ faculty_responded),
  as.formula(student_responses ~ faculty_owner),
  as.formula(student_responses ~ .),
  as.formula(student_responses ~ . + faculty_owner*faculty_responded +
               faculty_owner*parent_views + faculty_owner*parent_post_length),
  as.formula(student_responses ~ (.)^2)
)

# Name models (for later processing)
formula_names <- c("Null", "Faculty Responses Only", "Faculty Owner Only",
                   "All Predictors", "Predictors and Interactions", "Saturated")

# Subset for testing
nsr %<>% sample_frac(0.1)

## POISSON
pois <- runCountMods(formulas, nsr, "poisson")
#checkFit(pois)
pois_fx <- map(pois, avMarginalFX)

# Dispersion (ratio of deviance to residual.df, deviance approaches chi2 with mean = df)
map(pois, function(x) x$deviance / x$df.residual)
map(pois, dispersiontest, alternative = 'two.sided')

names(pois) <- formula_names
pois_fx <- addName(pois_fx, "poisson")

## QP
qp <- runCountMods(formulas, nsr, 'quasipoisson')
#checkFit(qp)
qp_fx <- map(qp, avMarginalFX)

names(qp) <- formula_names
names(qp_fx) <- formula_names
qp_fx <- addName(qp_fx, "quasipoisson")

## NB
nb <- runCountMods(formulas, nsr, 'nb')
checkFit(nb)
nb_fx <- map(nb, avMarginalFX)

names(nb) <- formula_names
names(nb_fx) <- formula_names
nb_fx <- addName(nb_fx, "negbin")

## Model comparisons
aic <- bind_rows(
  data.frame(aic = map_dbl(nb, AIC), model = "nb", formula = names(nb), stringsAsFactors = FALSE),
  data.frame(aic = map_dbl(pois, AIC), model = "pois", formula = names(pois), stringsAsFactors = FALSE)
)

ggplot(aic, aes(factor(formula, levels = rev(formula_names)), aic/min(aic), fill = model)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  coord_flip() +
  theme(axis.title.y = element_blank())

nsr_effects <- pmap(list(pois_fx, qp_fx, nb_fx), bind_rows)

#------------------------------------------------------------------------------#
## 2. Thread Width

# Grab data
tw <- data[subject %in% subjects, .(thread_width, studs_in_thread, parent_post_length,
                                    parent_views, faculty_owner, faculty_responded)][
                 , parent_post_length := log(1 + parent_post_length)
                 ]

# Define models
formulas <- list(
  as.formula(thread_width ~ faculty_responded),
  as.formula(thread_width ~ faculty_owner),
  as.formula(thread_width ~ .),
  as.formula(thread_width ~ . + faculty_owner*studs_in_thread + faculty_responded*studs_in_thread),
  as.formula(thread_width ~ (.)^2)
)

# Name models (for later processing)
formula_names <- c("Null", "Faculty Responses Only", "Faculty Owner Only",
                   "All Predictors", "Predictors and Interactions", "Saturated")

# Subset for testing
tw %<>% sample_frac(0.1)

## POISSON
pois <- runCountMods(formulas, tw, "poisson")
pois_fits <- checkFit(pois)
pois_fx <- map(pois, avMarginalFX)

# Dispersion (ratio of deviance to residual.df, deviance approaches chi2 with mean = df)
map(pois, function(x) x$deviance / x$df.residual)
map(pois, dispersiontest, alternative = 'two.sided')

names(pois) <- formula_names

## QP
qp <- runCountMods(formulas, tw, 'quasipoisson')
qp_fits <- checkFit(qp)
qp_fx <- map(qp, avMarginalFX)

names(qp) <- formula_names
names(qp_fx) <- formula_names

## NB
nb <- runCountMods(formulas, tw, 'nb')
nb_fits <- checkFit(nb)
nb_fx <- map(nb, avMarginalFX)

names(nb) <- formula_names
names(nb_fx) <- formula_names

## Model comparisons
aic <- bind_rows(
  data.frame(aic = map_dbl(nb, AIC), model = "nb", formula = names(nb), stringsAsFactors = FALSE),
  data.frame(aic = map_dbl(pois, AIC), model = "pois", formula = names(pois), stringsAsFactors = FALSE)
)
