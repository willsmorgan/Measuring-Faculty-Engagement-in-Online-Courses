#### Modeling
#### Author: William Morgan

# 1. Var summaries - checking for overdisperson, KS test
# 2. QP, NB regression

# NOTES:
#   IVs of interest: faculty_response, faculty_owner
#   DVs: num_responses, thread_width, avg_stu_response_len

#------------------------------------------------------------------------------#
## 0. Setup
libs <- c("data.table", "dplyr", "magrittr", "ggplot2", "forcats", "purrr")
lapply(libs, library, character.only = TRUE)
source("Code/utils/model_utils.R")

data <- fread("Data/thread_cleaned.csv") %>%
  mutate(faculty_owner = if_else(thread_owner_role == "P", "Y", "N"),
         faculty_responded = if_else(faculty_responses > 0, 1, 0)) %>%
  as.data.table()


# get rid of obscene outliers (anything beyond 99.5% quantile - see Preliminary Analysis for details)
data %<>%
  filter(thread_participants <= 15,
         thread_length <= 32,
         student_responses <= 30,
         studs_in_thread <= 15,
         !is.na(parent_views),
         !is.na(thread_width)) %>%
  as.data.table()

#------------------------------------------------------------------------------#
## 1. Number of Student Responses

nsr <- data[, .(student_responses, faculty_responses, faculty_owner,
                parent_views, parent_post_length)][
                  , parent_post_length := log(1 + parent_post_length)
                ]

# Models with one predictor (faculty_responses, faculty_owner)
f1a <- as.formula(student_responses ~ faculty_responses)
f1b <- as.formula(student_responses ~ faculty_owner)

# Model with all predictors
f2 <- as.formula(student_responses ~ .)

# Model with interactions
f3 <- as.formula(student_responses ~ . + faculty_responses*faculty_owner +
                   faculty_owner*parent_views + faculty_owner*parent_post_length)

# Run Models and export
nsr_results <- list(
  runCountModels(f1a, nsr, model_name = "Fac Responses Only"),
  runCountModels(f1b, nsr, model_name = "Fac Owned Only"),
  runCountModels(f2, nsr, model_name = "All Predictors"),
  runCountModels(f3, nsr, model_name = "Predictors and Interactions")
) %>%
  bind_rows() %>%
  select(model_name, everything())

fwrite(nsr_results, "Data/Results/nsr.csv")
#------------------------------------------------------------------------------#
## 2. Thread Width

tw <- data[, .(thread_width, studs_in_thread, parent_post_length,
               parent_views, faculty_owner, faculty_responded)][
                 , parent_post_length := log(1 + parent_post_length)
               ]

# One predictor models
f1a <- as.formula(thread_width ~ faculty_responded)
f1b <- as.formula(thread_width ~ faculty_owner)

# All predictor model
f2 <- as.formula(thread_width ~ .)

# All predictor + interactions
f3 <- as.formula(thread_width ~ . + faculty_owner*studs_in_thread + faculty_responded*studs_in_thread)

# Run Models and export
tw_results <- list(
  runModels(f1a, tw, model_name = "Fac Responses Only"),
  runModels(f1b, tw, model_name = "Fac Owned Only"),
  runModels(f2, tw, model_name = "All Predictors"),
  runModels(f3, tw, model_name = "Predictors and Interactions")
) %>%
  bind_rows() %>%
  select(model_name, everything())

fwrite(tw_results, "Data/Results/tw.csv")

#------------------------------------------------------------------------------#
## 3. Avg. Student Response Length

srl <- data[!is.na(avg_stu_response_len), .(avg_stu_response_len, faculty_owner, faculty_responses,
                parent_post_length)][
                  , `:=`(avg_stu_response_len = log(1 + avg_stu_response_len),
                         parent_post_length = log(1 + parent_post_length))
                ]

# Single Predictor models
f1a <- as.formula(avg_stu_response_len ~ faculty_responses)
f1b <- as.formula(avg_stu_response_len ~ faculty_owner)

# All predictor model
f2 <- as.formula(avg_stu_response_len ~ .)

# All predictor and interaction model + 
f3 <- as.formula(avg_stu_response_len ~ . + faculty_owner*parent_post_length)


srl_results <- list(
  "Fac Responses Only" = lm(f1a, data = srl),
  "Fac Owned Only" = lm(f1b, data = srl),
  "All Predictors" = lm(f2, data = srl),
  "Predictors + Interaction" = lm(f3, data = srl)
)

lapply(srl_results, summary)
#------------------------------------------------------------------------------#
## Additional goodies
cormat <- round(cor(data[!is.na(avg_stu_response_len), .(student_responses, thread_width, avg_stu_response_len,
                                                         faculty_responded, faculty_owner, parent_views, parent_post_length,
                                                         studs_in_thread)]), 2)
cormat[lower.tri(cormat)] <- NA
cormat <- melt(cormat, na.rm = TRUE)

ggplot(cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))