## postFeatures 
## Author: Will Morgan

## Purpose:
# Add relevant features to post-level forum data, including:
#   parent_post (binary)
#   child_post (binary)
#   thread_owner (categorical)
#   parent_role (categorical)
#   thread_responses (numeric)
#   num_responses (numeric; number of responses the post got)
#   num_stu_responses (numeric; number of student responses the post got)
#   num_inst_responses (numeric; number of inst. responses the post got)

#------------------------------------------------------------------------------#
library(data.table)
library(magrittr)


postFeatures <- function(forum_df) {
  
  if (!is.data.table(forum_df)) {
    forum_df <- as.data.table(forum_df)
  }
  
  ## Parent post or child post
  forum_df[, `:=`(parent_post = if_else(is.na(parent_id), 1, 0),
                  child_post = if_else(!is.na(parent_id), 1, 0))]
  
  ## Role of thread creator
  forum_df %<>%
    .[order(thread_id, post_timestamp)] %>%
    .[, thread_owner_role := user_role[1], by = .(thread_id, forum_id)]
  
  ## Role of user whose post is being responded to
  id_role_map <- forum_df[, .(post_id, user_role, forum_id)]
  parents <- forum_df[!is.na(parent_id), .(parent_id, forum_id)]
  
  parent_roles <- id_role_map[parents, on = c("post_id" = "parent_id", "forum_id" = "forum_id")] %>%
    .[!is.na(post_id), .(parent_id = post_id, parent_role = user_role, forum_id)] %>%
    .[.[, .I[1], by = .(parent_id, forum_id)]$V1]
  
  forum_df <- parent_roles[forum_df, on = c("parent_id", "forum_id")]
  
  rm(id_role_map, parents, parent_roles)
  
  ## Length of a thread 
  forum_df[, thread_length := as.integer(.N), by = .(thread_id, forum_id)]
  
  ## Number of responses
  forum_df[, num_responses := thread_length - 1]
  
  ## Number of posts and responses by user role (student v. instructor)
  forum_df[user_role == "S", num_stu_posts := .N, by = .(thread_id, forum_id)] %>%        # num of student posts in thread
    .[user_role == "P", num_fac_posts := .N, by = .(thread_id, forum_id)] %>%         # num of faculty posts in thread
    .[is.na(num_fac_posts), num_fac_posts := 0] %>%
    .[is.na(num_stu_posts), num_stu_posts := 0] %>%
    .[, `:=`(num_fac_posts = max(num_fac_posts),
             num_stu_posts = max(num_stu_posts)), by = .(thread_id, forum_id)] 
  
  
  post_responses <- forum_df[, .(post_responses = .N), by = .(thread_id, parent_id, user_role, forum_id)] %>%
    .[thread_id != parent_id, .(parent_id, post_responses, user_role, forum_id)]
  
  student_responses <- post_responses[user_role == "S", .(post_id = parent_id,
                                                          student_responses = post_responses,
                                                          forum_id)]
  
  fac_responses <- post_responses[user_role == "P", .(post_id = parent_id,
                                                      faculty_responses = post_responses,
                                                      forum_id)]
  
  # Left join stu/fac responses back in
  forum_df <- student_responses[forum_df, on = c('post_id', 'forum_id')] %>%
    fac_responses[., on = c('post_id', 'forum_id')]
  
  # Do correction for parent post counts
  forum_df <- forum_df[parent_post == 1 & user_role == "P", `:=`(faculty_responses = num_fac_posts - 1L,
                                                                 student_responses = num_stu_posts)] %>%
    .[parent_post == 1 & user_role == "S", `:=`(student_responses = num_stu_posts - 1L,
                                                faculty_responses = num_fac_posts)] %>%
    .[is.na(student_responses), student_responses := 0] %>%
    .[is.na(faculty_responses), faculty_responses := 0]
  
  
  forum_df <- setcolorder(forum_df, c("forum_id", "thread_id", "parent_post",  "thread_length", "num_fac_posts", "num_stu_posts",
                                      "thread_owner_role", "parent_id", "post_id", "user_role"))
  
  return(forum_df)
}