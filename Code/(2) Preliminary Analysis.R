## Preliminary Analysis - Checking distributional assumptions
## Author: William Morgan

# General outline for each variable to check:
#   plot ecdf of var against cdf of pois(lambda = x_bar)
#   ks.test() of var against pois(lambda = x_bar)
#   LR test?

#------------------------------------------------------------------------------#
## 0. Setup
libs <- c("data.table", "dplyr", "magrittr", "ggplot2", "forcats", "purrr", "AER", "vcd")
lapply(libs, library, character.only = TRUE)
set.seed(18)


data <- fread("Data/thread_cleaned.csv") %>%
  mutate(faculty_owner = if_else(thread_owner_role == "P", 1, 0),
         faculty_responded = if_else(faculty_responses > 0, 1, 0)) %>%
  as.data.table()

cols <- c("thread_participants", "thread_length", "student_responses", "faculty_responses",
          "thread_width", "num_fac_posts", "num_stu_posts", "avg_response_len", "avg_stu_response_len",
          "avg_fac_response_len", "posts_per_student", "studs_in_thread", "parent_post_length", "num_responses")

#sapply(data[, ..cols], function(x) quantile(x, seq(0.99, 1, 0.001), na.rm = TRUE))


# get rid of obscene outliers (anything beyond 99.5% quantile)
data %<>%
  filter(thread_participants <= 15,
         thread_length <= 32,
         student_responses <= 30,
         studs_in_thread <= 15,
         !is.na(parent_views),
         !is.na(thread_width)) %>%
  as.data.table()

#sapply(data[, ..cols], function(x) quantile(x, seq(0.99, 1, 0.001), na.rm = TRUE))

#------------------------------------------------------------------------------#
## 1. Number of student Responses

mean(data[, student_responses])
var(data[, student_responses])

# Plot CDFs
data %>%
  mutate(truth = rpois(nrow(.), mean(student_responses))) %>%
  rename(observed = student_responses) %>%
  ggplot() +
  stat_ecdf(aes(truth), color = 'red') +
  stat_ecdf(aes(observed), color = 'blue') +
  xlim(c(0, 15)) +
  labs(x = "Number of Student Responses",
       y = "Cumulative Density",
       title = "Observed CDF vs. True CDF")+
  theme(plot.title = element_text(hjust = 0.5))

# Chi-sq GOF
nsr_counts <- data %>%
  group_by(student_responses) %>%
  summarise(observed = n()) %>%
  ungroup() %>%
  mutate(prob = dpois(student_responses, 1.53),
         expected = as.integer(observed * prob))

chisq.test(c(nsr_counts$observed, 0), p = c(nsr_counts$prob, 1e-16))

# VCD package
nsr_fit <- vcd::goodfit(c(nsr_counts$observed, 0), type = 'poisson')
summary(nsr_fit)

# Plot observed v. expected counts
ggplot(nsr_counts) +
  geom_line(aes(student_responses, observed), color = 'blue') + 
  geom_line(aes(student_responses, expected), color = 'red')

#------------------------------------------------------------------------------#
## 2. Thread Width
mean(data[, thread_width])

# Plot CDFs
data %>%
  mutate(truth = rpois(nrow(.), mean(thread_width))) %>%
  rename(observed = thread_width) %>%
  ggplot() +
  stat_ecdf(aes(truth), color = 'red') +
  stat_ecdf(aes(observed), color = 'blue') +
  xlim(c(0, 15)) +
  labs(x = "Thread Width",
       y = "Cumulative Density",
       title = "Observed CDF vs. True CDF") +
  theme(plot.title = element_text(hjust = 0.5))

# Chi-sq GOF
tw_counts <- data %>%
  group_by(thread_width) %>%
  summarise(observed = n()) %>%
  ungroup() %>%
  mutate(prob = dpois(thread_width, 1.53),
         expected = as.integer(observed * prob))

chisq.test(c(tw_counts$observed, 0), p = c(tw_counts$prob, 1e-16))

# VCD package
tw_fit <- vcd::goodfit(c(tw_counts$observed, 0), type = 'poisson')
summary(tw_fit)


