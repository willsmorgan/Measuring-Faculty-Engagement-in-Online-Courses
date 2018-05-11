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
  select(-c(cour_st_dt, cour_end_dt, cem_unique, total_len_fac_ta_posts)) %>%
  rename(session_code = term_session,
         course_id = myasu_course) %>%
  mutate(subject = str_sub(course, 1, 3), 
         catalog_nbr = str_sub(course, 4, 6),
         course = paste(subject, catalog_nbr, sep = '|'),
         enrl_total = pass_ct + wdwn_ct + fail_ct,
         num_fac_ta_posts = num_fac_posts_wk0 + num_fac_posts_wk1 + num_fac_posts_wk2 +
           num_fac_posts_wk3 + num_fac_posts_wk4 + num_fac_posts_wk5 + 
           num_fac_posts_wk6 + num_fac_posts_wk7 + num_fac_posts_wk8 + 
           num_fac_posts_wk9 + num_fac_posts_wk10 + num_fac_posts_wk11 +
           num_fac_posts_wk12 + num_fac_posts_wk13 + num_fac_posts_wk14 +
           num_fac_posts_wk15)


# Note: choose to recreate total num of faculty posts during course here 
# so that we can get rid of observations that do not have any posts

### 2. Dropping observations

## The drops are partitioned so that they can be used conditionally; different
## analyses require different data and we want to minimize the amount of 
## unnecessary drops

# Graduate Courses
forum <- forum %>%
  filter(str_length(course) == 7 & as.numeric(str_sub(course, 5, 7)) < 500)

# Low-enrollment Courses
forum <- forum %>% filter(enrl_total >= 5)

# Missing survey data
#forum <-  forum %>% filter(!is.na(num_evals_expected))

# Missing faculty data
forum <- filter(forum, num_fac_ta > 0)

# Student post outliers
forum <- forum %>%
  filter(num_stu_posts <= quantile(num_stu_posts, .95))

# Courses without any faculty posts
forum <- forum %>%
  filter(num_fac_ta_posts > 0)

### 3. Global variable changes

# Grab question names
questions <-  forum %>%
  select(starts_with('q_')) %>%
  names()

# Force questions to numeric 
forum <- forum %>%
  mutate_at(questions, as.numeric)

# Student and Faculty post consistency
### This variable is defined as 1/var(num_posts_per_student in week x). The variance
### measures how consistent students and faculty are in posting every week, and 
### we then take the reciprocal of the value so it is easier to interpret.

### Larger values mean more consistent number of posts per student or faculty
### from week to week
stu_consistency <- select(forum,
            num_stu_posts_wk0,
            num_stu_posts_wk1,
            num_stu_posts_wk2,
            num_stu_posts_wk3,
            num_stu_posts_wk4,
            num_stu_posts_wk5,
            num_stu_posts_wk6,
            num_stu_posts_wk7,
            num_stu_posts_wk8,
            course_id,
            enrl_total) %>%
  mutate_at(vars(starts_with("num_stu")),
            .funs = function(x) x / .$enrl_total) %>%
  gather(posts, count, starts_with("num_stu")) %>%
  group_by(course_id) %>%
  summarise(stu_post_consistency = 1 / sqrt(var(count)))

fac_consistency <- select(forum,
            num_fac_posts_wk0,
            num_fac_posts_wk1,
            num_fac_posts_wk2,
            num_fac_posts_wk3,
            num_fac_posts_wk4,
            num_fac_posts_wk5,
            num_fac_posts_wk6,
            num_fac_posts_wk7,
            num_fac_posts_wk8,
            course_id,
            num_fac_ta) %>%
  gather(posts, count, starts_with("num_fac")) %>%
  mutate_at(vars(starts_with("num_fac")),
            .funs = function(x) x / .$num_fac_ta) %>%
  group_by(course_id) %>%
  summarise(fac_post_consistency = 1 / sqrt(var(count))) %>%
  inner_join(stu_consistency, by = 'course_id')

## Proportion of posts in the first 2 weeks of class
fac_posts <- forum %>%
  select(course_id, starts_with("num_fac_posts_wk")) %>%
  mutate(tot_fac_posts = num_fac_posts_wk0 + num_fac_posts_wk1 + num_fac_posts_wk2 +
           num_fac_posts_wk3 + num_fac_posts_wk4 + num_fac_posts_wk5 + 
           num_fac_posts_wk6 + num_fac_posts_wk7 + num_fac_posts_wk8 + 
           num_fac_posts_wk9 + num_fac_posts_wk10 + num_fac_posts_wk11 +
           num_fac_posts_wk12 + num_fac_posts_wk13 + num_fac_posts_wk14 +
           num_fac_posts_wk15,
         prop_posts_boc = (num_fac_posts_wk0 + num_fac_posts_wk1 + num_fac_posts_wk2) / tot_fac_posts,
         prop_posts_eoc = (num_fac_posts_wk6 + num_fac_posts_wk7 + num_fac_posts_wk8) / tot_fac_posts) %>%
  select(course_id, prop_posts_boc, prop_posts_eoc, tot_fac_posts)

## Join new variables and create final ones
forum <- forum %>%
  inner_join(fac_consistency, by = 'course_id') %>%
  inner_join(fac_posts, by = 'course_id') %>%
  filter(session_code %in% c("A", "B")) %>%
  mutate(prop_stu_posted = num_stu_with_posts / enrl_total,
         session_a = if_else(session_code == "A", 1, 0),
         avg_gpa = sum_gpa / enrl_total,
         wdrw_rate = wdwn_ct / enrl_total,
         instr_score = (q_responded + q_present + q_feedback) / 3,
         design_score = (q_success + q_prepared + q_presentations + q_navigate) / 4,
         posts_per_student = num_stu_posts / enrl_total,
         posts_per_fac = tot_fac_posts / num_fac_ta,
         pass_rate = pass_ct / enrl_total,
         upper_division = if_else(
           as.numeric(str_sub(course, 5, 7)) >= 300, 1, 0),
         pass_rate = pass_ct / enrl_total,
         response_rate = num_evals_taken / num_evals_expected) %>%
  select(course_id, has_hallway, enrl_total, fac_post_consistency,
         stu_post_consistency, prop_posts_boc, prop_posts_eoc,
         session_a, avg_gpa, wdrw_rate, instr_score, design_score, posts_per_fac,
         posts_per_student, pass_rate, upper_division) %>%
  mutate_at(vars(session_a, has_hallway, upper_division), as.factor)


rm(fac_consistency, fac_posts, stu_consistency)

# write_csv(forum, "R:\\Projects\\Faculty Engagement\\Data\\data_w_evals.csv")
# write_csv(forum, "R:\\Projects\\Faculty Engagement\\Data\\data_wo_evals.csv")

