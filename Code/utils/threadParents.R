## For each post in a thread, find all parents of that post
## Parents will be the immediately preceding parent post

findThreadParents <- function(thread, df) {
  
  ## Determine all parents of a given post in a thread (following hierarchy back
  ## up the tree of comments)

  findPostParent <- function(id, ref_table) {
    ## Find the parent id of a single post
    parent <- ref_table[post_id == id, parent_id]
    return(parent)
  }
    
  # Create lookup table
  ref_table <- df[thread_id == thread, .(post_id, parent_id, thread_id)]
  
  # Create parent table to build out
  parent_table <- data.table(thread_id = ref_table$thread_id,
                             post_id = ref_table$post_id,
                             parent_1 = ref_table$parent_id)
  
  # Identify thread parent for early stopping
  thread_parent_id <- parent_table[is.na(parent_1), post_id]
  
  ## Continue until there are no parents left to find
  i <- 1
  while (parent_table[, sum(is.na(.SD)), .SDcols = ncol(parent_table)] != nrow(parent_table)) {
    
    # Pull ids to look up
    ids <- na.omit(unique(parent_table[, get(paste0("parent_", i))]))
    
    # If the only id left to lookup is the thread parent, break - sometimes no parents so check that first
    if (length(ids) == 1 & length(thread_parent_id) > 0) {
      if (thread_parent_id %in% ids) {
        break
      }
    }
    
    # Generate empty column to dump new parents into
    parent_table[, paste0("parent_", i + 1) := vector(mode = 'integer')]
    
    for (j in ids) {
      parent <- findPostParent(j, ref_table)
     
       if (length(parent) > 0) {
        parent_table[get(paste0("parent_", i)) == j, paste0("parent_", i + 1) := parent]
      }
    }
    
    # Increase iterator
    i <- i + 1
  }
  
  return(parent_table)
}

