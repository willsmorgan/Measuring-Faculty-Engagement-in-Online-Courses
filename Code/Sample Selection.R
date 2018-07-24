### Sample Selection

# Not meant to be run here, meant to be sourced by modeling script

#------------------------------------------------------------------------------#
### 0. Setup
libs <- c('readxl', 'tidyverse', 'data.table', 'magrittr')
lapply(libs, library, character.only = TRUE)

### 1. Import and format
forum <- read_excel('Data/20180405 Fac Engagement.xlsx', sheet = 1) %>%
  rename_all(str_to_lower)

forum %<>%
  rename(session_code = term_session,
         course_id = myasu_course) %>%
  mutate(subject = str_sub(course, 1, 3), 
         catalog_nbr = str_sub(course, 4, 6),
         course = paste(subject, catalog_nbr, sep = '|'),
         enrl_total = pass_ct + wdwn_ct + fail_ct,
         num_fac_posts = num_fac_posts_wk0 + num_fac_posts_wk1 + num_fac_posts_wk2 +
           num_fac_posts_wk3 + num_fac_posts_wk4 + num_fac_posts_wk5 + 
           num_fac_posts_wk6 + num_fac_posts_wk7 + num_fac_posts_wk8 + 
           num_fac_posts_wk9 + num_fac_posts_wk10 + num_fac_posts_wk11 +
           num_fac_posts_wk12 + num_fac_posts_wk13 + num_fac_posts_wk14 +
           num_fac_posts_wk15,
         avg_fac_len = total_len_fac_ta_posts / num_fac_posts) %>%
  select(-starts_with("fac_posted_")) %>%
  select(order(colnames(.)))

#------------------------------------------------------------------------------#
### 2. Dropping observations

# Only A/B Session Codes
forum %<>%
  filter(session_code != "C")

# Graduate Courses
forum %<>%
  filter(str_length(course) == 7 & as.numeric(str_sub(course, 5, 7)) < 500)

# Low-enrollment Courses
forum %<>% filter(enrl_total >= 5)

# Missing survey data
if (survey_drop) {
  forum <-  forum %>% filter_at(vars(starts_with("q_")), any_vars(!is.na(.)))
}

# Missing faculty data
forum %<>% filter(num_fac_ta > 0)

# Student post outliers
forum %<>%
  filter(num_stu_posts <= quantile(num_stu_posts, .95))

# Courses without any faculty posts
forum %<>%
  filter(num_fac_ta_posts > 0)

# Avg. Fac Post Length outliers
forum %<>%
  filter(!is.na(avg_fac_len)) %>%
  filter(avg_fac_len <= quantile(avg_fac_len, 0.95))


#------------------------------------------------------------------------------#
### 3. Variable additions

# Grab question names
questions <-  forum %>%
  select(starts_with('q_')) %>%
  names()

# Force questions to numeric 
forum %<>%
  mutate_at(questions, as.numeric)

# Student post consistency
## Defined as variance of number of posts per student/faculty per week
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
  summarise(stu_consistency = sqrt(var(count)))

## Faculty post consistency
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
  summarise(fac_consistency = var(count)) %>%
  inner_join(stu_consistency, by = 'course_id')

## Proportion of posts in the first 2 weeks of class
fac_posts <- forum %>%
  select(course_id, num_fac_posts, starts_with("num_fac_posts_wk")) %>%
  mutate(prop_posts_boc = (num_fac_posts_wk0 + num_fac_posts_wk1 + num_fac_posts_wk2) / num_fac_posts) %>%
  select(course_id, prop_posts_boc)

## Average grade received in the course
forum %<>% mutate(avg_grade = sum_gpa / enrl_total)

## Instructor evaluation score (averaged over the three criteria)
forum %<>% mutate(instr_score = (q_responded + q_present + q_feedback) / 3)

## Posts per student/faculty
forum %<>% mutate(pps = num_stu_posts / enrl_total,
                  ppf = num_fac_posts / num_fac_ta)

## Upper Division indicator
forum %<>% mutate(upper_division = if_else(as.numeric(catalog_nbr) <= 300, "1", "0"),
                  upper_division = as_factor(upper_division),
                  upper_division = relevel(upper_division, "0"))

## Has Hallway indicator
forum %<>% mutate(hallway = as.character(has_hallway),
                  hallway = as_factor(hallway),
                  hallway = relevel(hallway, "0"))

## Withdrawal rate
forum %<>% mutate(wdrw_rate = wdwn_ct / enrl_total)

## Join new variables and alphabetize
forum %<>%
  inner_join(fac_consistency, by = 'course_id') %>%
  inner_join(fac_posts, by = 'course_id') %>%
  select(order(colnames(.)))

#------------------------------------------------------------------------------#
### 4. Final Cleanup

## Select final variables and do last sweep for outliers
if (survey_drop) {
  forum %<>%
    select(avg_fac_len, avg_grade, enrl_total, fac_consistency,
           hallway, instr_score, ppf, pps, prop_posts_boc, stu_consistency,
           upper_division, wdrw_rate) %>%
    filter_all(all_vars(!is.na(.))) %>%
    filter_if(is.numeric, all_vars(. <= quantile(., 0.99))) %>%
    select(order(colnames(.)))
} else {
  forum %<>%
    select(avg_fac_len, avg_grade, enrl_total, fac_consistency,
           hallway, ppf, pps, prop_posts_boc, stu_consistency,
           upper_division, wdrw_rate) %>%
    filter_all(all_vars(!is.na(.))) %>%
    filter_if(is.numeric, all_vars(. <= quantile(., 0.99))) %>%
    select(order(colnames(.)))
}

rm(fac_consistency, fac_posts, stu_consistency)