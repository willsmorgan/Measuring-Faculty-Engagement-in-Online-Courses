### Sample Selection

# Not meant to be run here, meant to be sourced by modeling script

#------------------------------------------------------------------------------#

### 0. Setup
libs <- c('readxl', 'tidyverse', 'data.table', 'gridExtra')
lapply(libs, library, character.only = TRUE)

### 1. Import and format
forum <- read_excel('Data/20180405 Fac Engagement.xlsx', sheet = 1)
names(forum) <- str_to_lower(names(forum))

forum <- forum %>%
  select(-c(cour_st_dt, cour_end_dt, cem_unique)) %>%
  rename(session_code = term_session) %>%
  mutate(subject = str_sub(course, 1, 3), 
         catalog_nbr = str_sub(course, 4, 6),
         course = paste(subject, catalog_nbr, sep = '|'),
         enrl_total = pass_ct + wdwn_ct + fail_ct)


### 2. Dropping observations

## The drops are partitioned so that they can be used conditionally; different
## analyses require different data and we want to minimize the amount of 
## unnecessary drops

# Graduate Courses
forum <- forum %>%
  filter(course != '-' & as.numeric(str_sub(course, 4, 6)) < 500)

# Low-enrollment Courses
forum <- forum %>% filter(enrl_total >= 5)

# Missing survey data
forum <-  forum %>% filter(!is.na(num_evals_expected))

# Missing faculty data
forum <- filter(forum, num_fac_ta > 0)

# Total faculty post length outliers
forum <- forum %>%
  filter(total_len_fac_ta_posts <= quantile(total_len_fac_ta_posts, .95))

# Student post outliers
forum <- forum %>%
  filter(num_stu_posts <= quantile(num_stu_posts, .95))


### 3. Global variable changes

# Grab question names
questions <-  forum %>%
  select(starts_with('q_')) %>%
  names()

# Force questions to numeric 
forum <- forum %>%
  mutate_at(questions, as.numeric)

# Add new variables
forum_cleaned <- forum %>%
  mutate(prop_stu_posted = num_stu_with_posts / enrl_total,
         avg_gpa = sum_gpa / enrl_total,
         wdrw_rate = wdwn_ct / enrl_total,
         instr_score = (q_responded + q_present + q_feedback) / 3,
         design_score = (q_success + q_prepared + q_presentations + q_navigate) / 4,
         posts_per_student = num_stu_posts / enrl_total,
         posts_per_fac = num_fac_ta_posts / num_fac_ta,
         pass_rate = pass_ct / enrl_total,
         avg_fac_post_len = total_len_fac_ta_posts / num_fac_ta_posts,
         upper_division = if_else(
           as.numeric(str_sub(course, 5, 7)) >= 300, 1, 0),
         fac_posts_boc = num_fac_posts_wk0 + num_fac_posts_wk1 + num_fac_posts_wk2,
         fac_posts_eoc = num_fac_posts_wk6 + num_fac_posts_wk7 + num_fac_posts_wk8,
         pass_rate = pass_ct / enrl_total,
         response_rate = num_evals_taken / num_evals_expected) %>%
  rename(course_id = myasu_course) %>%
  select(course_id, session_code, pass_rate, avg_gpa, wdrw_rate,
         enrl_total, response_rate, has_hallway,
         fac_posts_boc, fac_posts_eoc, total_len_fac_ta_posts,
         instr_score, design_score, posts_per_fac,
         posts_per_student, avg_fac_post_len, upper_division) %>%
  filter(session_code %in% c("A", "B"))

















