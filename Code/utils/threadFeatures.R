## threadFeatures 
## Author: Will Morgan

## Purpose:
# Aggregate post-level forum data to thread level, creating new features along the way:

# X  # Role of thread owner
# X  # Length of thread (number of posts)
# X  # Number of posts made by students
# X  # Number of posts made by faculty
# X  # Proportion of posts made by students
# X  # Proportion of posts made by faculty
# X  # Width of thread (conversations)
# X  # Number of students participating in thread
# X  # Avg. length of responses
# X  # Avg. length of student responses
# X  # Avg. length of faculty responses
# X  # Number of times parent post has been viewed
# X  # Posts per student (of students active in the thread)
# X  # Number of posts that are students responding to students
# X  # Number of posts that are students responding to faculty
# X  # Number of posts that are faculty responding to students
# X  # Number of posts that are faculty responding to faculty (don't anticipate much, should only be used for next three)
# X  # Proportion of posts that are students responding to students
# X  # Proportion of posts that are students responding to faculty
# X  # Proportion of posts that are faculty responding to students
# X  # Proportion of students who get a reply from faculty
# X  # Lifecycle of thread (last post timestamp - first post timestamp)
# 
# HOLD OFF  # responses per post in thread?? mean(num_responses) if parent == 0 by forum, thread
#------------------------------------------------------------------------------#
library(foreach)
source("Code/utils/threadParents.R")

threadFeatures <- function(post_data) {

  # Proportion of posts made by students/faculty
  post_data <- post_data[order(forum_id, thread_id, post_timestamp)]
  post_data[, `:=`(pr_posts_stu = num_stu_posts[1] / thread_length,
              pr_posts_fac = num_fac_posts[1] / thread_length),
       by = .(forum_id, thread_id)]
  
  ## Get Parent post IDs
  threads <- unique(post_data$thread_id)

  post_parents <- map_dfr(threads, function(x) findThreadParents(x, post_data))
  
  # Calculate width by taking max(rowSum(is.na(parent_*))) by thread
  post_parents[, sum := rowSums(!is.na(.SD)), .SDcols = grep("parent_", names(post_parents)), by = thread_id]
  
  widths <- post_parents[order(thread_id, -sum), .(thread_id, sum)] %>%
    .[.[, .I[1], by = .(thread_id)]$V1]
  
  setnames(widths, "sum", "thread_width")
  
  post_data <- widths[post_data, on = 'thread_id']
  
  # Number of users participating in thread
  post_data[, thread_participants := length(unique(emplid)), by = .(forum_id, thread_id)]
  
  # Number of students participating in thread
  post_data[user_role == "S", studs_in_thread := length(unique(emplid)), by = .(forum_id, thread_id)] %>%
    .[is.na(studs_in_thread), studs_in_thread := 0L] %>%
    .[, studs_in_thread := max(studs_in_thread), by = .(forum_id, thread_id)]

  # Length of parent post (NA for threads with no parents)
  post_data[parent_post == 1, parent_post_length := post_length] %>%
    .[parent_post == 0, parent_post_length := 0L] %>%
    .[, parent_post_length := max(parent_post_length), by = .(forum_id, thread_id)]
  
  # Average length of responses (will be NA for threads with no responses)
  post_data[parent_post == 0, avg_response_len := mean(post_length), by = .(forum_id, thread_id)] %>%
    .[parent_post == 1 & thread_length > 1, avg_response_len := 0] %>%
    .[, avg_response_len := max(avg_response_len), by = .(forum_id, thread_id)] %>%
    .[thread_length == 1, avg_response_len := NA_integer_]
  
  # Average length of faculty responses
  post_data[parent_post == 0 & user_role == "P", avg_fac_response_len := mean(post_length), by = .(forum_id, thread_id)] %>%
    .[is.na(avg_fac_response_len) & thread_length > 1, avg_fac_response_len := 0] %>%
    .[, avg_fac_response_len := max(avg_fac_response_len), by = .(forum_id, thread_id)] %>%
    .[faculty_responses == 0, avg_fac_response_len := NA_integer_]
  
  # Average length of student responses
  post_data[parent_post == 0 & user_role == "S", avg_stu_response_len := mean(post_length), by = .(forum_id, thread_id)] %>%
    .[is.na(avg_stu_response_len) & thread_length > 1, avg_stu_response_len := 0] %>%
    .[, avg_stu_response_len := max(avg_stu_response_len), by = .(forum_id, thread_id)]
  
  # Number of times parent post has been viewed
  post_data[, parent_views := if_else(parent_post == 1, num_views, 0L)] %>%
    .[, parent_views := max(parent_views), by = .(forum_id, thread_id)]
  
  # Posts per active student
  post_data[, posts_per_student := num_stu_posts / thread_participants, by = .(forum_id, thread_id)]
  
  # Lifecycle of a thread (must check validity later)
  post_data[thread_length > 1, thread_lifetime := difftime(max(post_timestamp), min(post_timestamp), units = c("days")), by = .(forum_id, thread_id)]
  
  ## Response type counts (stu_stu, stu_fac, etc.)
  post_data[thread_length > 1, `:=`(stu_stu_resp = if_else(child_post == 1 & user_role == "S" & parent_role == "S", 1, 0),
              stu_fac_resp = if_else(child_post == 1 & user_role == "S" & parent_role == "P", 1, 0),
              fac_stu_resp = if_else(child_post == 1 & user_role == "P" & parent_role == "S", 1, 0),
              fac_fac_resp = if_else(child_post == 1 & user_role == "P" & parent_role == "P", 1, 0))] %>%
    .[thread_length > 1, `:=`(stu_stu_resp = sum(stu_stu_resp, na.rm = TRUE),
             stu_fac_resp = sum(stu_fac_resp, na.rm = TRUE),
             fac_stu_resp = sum(fac_stu_resp, na.rm = TRUE),
             fac_fac_resp = sum(fac_fac_resp, na.rm = TRUE)), by = .(thread_id, forum_id)]
  
  ## Response type proportions
  post_data[thread_length > 1, `:=`(stu_stu_pr = stu_stu_resp / (thread_length - 1),
              stu_fac_pr = stu_fac_resp / (thread_length - 1),
              fac_stu_pr = fac_stu_resp / (thread_length - 1),
              fac_fac_pr = fac_fac_resp / (thread_length - 1))]
  
  
  ## Proportion of students in thread who get a reply from faculty
  prop_responses <- post_data[parent_post == 0, .(forum_id, thread_id, post_id, parent_id, user_role, parent_role, emplid, studs_in_thread)]
  
  temp <- post_data[parent_post == 0, .(forum_id, thread_id, post_id, parent_id, user_role, parent_role, emplid)] %>%
    .[, fac_responded := if_else(user_role == "P" & parent_role == "S", 1, 0)] %>%
    .[fac_responded == 1, .(forum_id, thread_id, parent_id, fac_responded)] %>%
    .[.[, .I[1], by = .(forum_id, thread_id, parent_id)]$V1] %>%
    .[, `:=`(post_id = parent_id,
             parent_id = NULL)]
  
  prop_responses <- temp[prop_responses, on = c("post_id", "forum_id", "thread_id")] %>%
    .[is.na(fac_responded), fac_responded := 0L] %>%
    .[, fac_responded := max(fac_responded), by = .(forum_id, thread_id, emplid)] %>%              # did they respond to a given student?
    .[.[, .I[1], by = .(forum_id, thread_id, emplid)]$V1] %>%                                    # select one obs from each thread and empl
    .[user_role == "S", fac_responded := sum(fac_responded), by = .(forum_id, thread_id)] %>%      # how many students did they respond to 
    .[.[, .I[1], by = .(forum_id, thread_id)]$V1] %>%                                            # select one obs from each thread
    .[, .(forum_id, thread_id, fac_responded, studs_in_thread)] %>%
    .[, `:=`(prop_stu_responded_to = fac_responded / studs_in_thread,
             studs_in_thread = NULL,
             fac_responded = NULL)] %>%
    .[!is.na(forum_id)]
  
  post_data <- prop_responses[post_data, on = c("forum_id", "thread_id")] %>%
    .[is.na(prop_stu_responded_to) & num_stu_posts == 0, prop_stu_responded_to := NA_real_] %>%
    .[is.na(prop_stu_responded_to) & num_stu_posts > 0, prop_stu_responded_to := 0] %>%
    .[thread_length == 1, prop_stu_responded_to := NA_real_]
  
  rm(prop_responses, temp)
  
  
  # subset and reduce to thread-level
  cols <- c("forum_id", "thread_id", "thread_participants", "prop_stu_responded_to",
            "thread_length", "student_responses", "faculty_responses",
            "stu_stu_pr", "stu_stu_resp", "stu_fac_pr", "stu_fac_resp",
            "fac_stu_pr", "fac_stu_resp", "thread_width", "pr_posts_stu",
            "pr_posts_fac", "num_fac_posts", "num_stu_posts", "thread_owner_role",
            "bb_course_id", "forum_name", "parent_views", "strm", "location", 
            "campus", "session_code", "course", "subject", "catalog_nbr", "avg_response_len",
            "avg_stu_response_len", "avg_fac_response_len", "posts_per_student", "thread_lifetime",
            "parent_post_length", "studs_in_thread", "num_responses")
  
  
  thread_df <- post_data[, ..cols] %>%
    .[.[, .I[1], by = .(thread_id)]$V1]
  
  setcolorder(thread_df, c("bb_course_id", "course", "subject", "catalog_nbr", "strm",
                      "forum_id", "thread_id"))
  
  return(thread_df)
}