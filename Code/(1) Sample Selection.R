### Cleaning Raw Forum Data
### Author: Will Morgan

## Purpose:
# Take post-level forum data and organize into student-level and course-level data
# so we can conduct correlational analyses

#------------------------------------------------------------------------------#
## 0. Setup and Import
libs <- c("data.table", "tidyverse", "lubridate", "magrittr")
lapply(libs, library, character.only = TRUE)

data <- fread("Data/raw_forum.csv") %>%
  rename_all(str_to_lower) %>%
  rename(thread_id = post_thread,
         parent_id = post_parent)

source("Code/utils/postFeatures.R")
source("Code/utils/threadFeatures.R")
#------------------------------------------------------------------------------#
## 1. Initial drops - getting rid of unwanted courses

# DEV, Non-undergrad courses
data %<>%
  mutate(course = str_extract(bb_course_id, "[A-Z]{3}[0-9]{3}"),
         subject = str_sub(course, 1, 3),
         catalog_nbr = str_sub(course, 4, 6)) %>%
  filter(!is.na(course),
         as.numeric(catalog_nbr) < 500)

# Terms before Spring 2016
data %<>%
  filter(strm != "-",
         as.numeric(strm) >= 2161)

# Unwanted users
data %<>%
  filter(!user_role %in% c("B", "G"))

# Only Session A/B
data %<>%
  filter(session_code %in% c("A", "B"))

# Absurdly long posts
data %<>%
  filter(post_length <= quantile(post_length, 0.999))

# Threads without any parent posts
data %<>%
  mutate(parent_check = if_else(is.na(parent_id), 1, 0)) %>%
  group_by(forum_id, thread_id) %>%
  mutate(parent_check = sum(parent_check)) %>%
  ungroup() %>%
  filter(parent_check > 0) %>%
  select(-parent_check)

#------------------------------------------------------------------------------#
## 2. Variable changes and new features

# Combine TA and Professor user role
data %<>%
  mutate(user_role = if_else(user_role == "T", "P", user_role))

# Ensure timestamp is POSIX
data %<>%
  mutate(post_timestamp = as_datetime(post_timestamp))

#------------------------------------------------------------------------------#
## 3. Create Forum Features

# parent_post (binary)
# child_post (binary)
# thread_owner (categorical)
# parent_role (categorical)
# thread_responses (numeric)
# num_responses (numeric; number of responses the post got)
# num_stu_responses (numeric; number of student responses the post got)
# num_inst_responses (numeric; number of inst. responses the post got)

data <- postFeatures(data)

# export post-level dataset (keep for archive)
fwrite(data, "Data/posts_cleaned.csv")
#------------------------------------------------------------------------------#
## 4. Aggregate to thread level

data <- threadFeatures(data)

# export thread-level dataset
fwrite(data, "Data/thread_cleaned.csv")