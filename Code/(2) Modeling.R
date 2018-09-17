#### Modeling
#### Author: William Morgan




#------------------------------------------------------------------------------#
## 0. Setup
libs <- c("data.table", "dplyr", "magrittr", "broom")
lapply(libs, library, character.only = TRUE)

data <- fread("Data/thread_cleaned.csv")

#------------------------------------------------------------------------------#
## 1. Check for overdispersion

# Number of student posts
hist(data[, num_stu_posts])

mean(data[, num_stu_posts])
var(data[, num_stu_posts])

# full model, no interactions
formula <- as.formula(num_stu_posts ~ thread_owner_role + thread_participants + 
                        thread_length + num_fac_posts + thread_width + parent_views +
                        parent_post_length + studs_in_thread)



test_ols <- glm(formula, family = 'gaussian', data = data)
test_poi <- glm(formula, family = poisson(link = 'log'), data = data)
test_nlb <- MASS::glm.nb(formula, data)


summary(test_ols)
summary(test_poi)


