indexify <- function(logical_vector) {
  idx_vector <- c()
  idx_val <- 0
  for (logi in logical_vector) {
    if (logi == 1) {
      idx_val <- idx_val + 1
    }
    idx_vector <- c(idx_vector, idx_val)
  }
  return(idx_vector)
}

make_first_id <- function(vec, val) {
  vec[1] <- val
  return(vec)
}


listing_data <- listing_join
run_first_str_correct <- function(listing_data) {
  first_str_index <- listing_data %>%
    group_by(cluster_code) %>%
    slice(1) %>%
    filter(str_index != 1)
  
  IDs <- first_str_index$id
  correct_listing_data <- data.frame()
  listing_data_sub <- data.frame()
  
  for (ID in IDs) {
    listing_data_sub <- listing_data %>%
      filter(id == ID)
    str_stat <- listing_data_sub$str_stat
    if (str_stat[1] == 0) {
      listing_data_sub$address_of_structure <-
        make_first_id(listing_data_sub$address_of_structure,
                      "Placeholder Address")
      listing_data_sub$str_stat <-
        make_first_id(listing_data_sub$str_stat, 1)
    }
    
    listing_data_sub$str_index <- indexify(listing_data_sub$str_stat)
    correct_listing_data <- rbind(correct_listing_data, listing_data_sub)
  }
  
  return(correct_listing_data)
}

join_corrected <- run_first_str_correct(listing_join)
join_remaining <- listing_join %>% filter (!(id %in% unique(join_corrected$id)))

join_proc <- rbind(join_corrected, join_remaining)


fill_in_NA_prev <- function(vec){
  prev <- ""
  new_vec <- c()
  for (elem in vec){
    if (!is.na(elem)){
      new_vec <- c(new_vec, elem)
      prev <- elem
    } else {
      new_vec <- c(new_vec, prev)
    }
    
  }
  
  return(new_vec)
}

fill_in <- function(listing_data){
  cluster_codes <- unique(listing_data$cluster_code)
  updated_listing <- data.frame()
  
  for (code in cluster_codes){
    listing_data_sub <- listing_data %>%
      filter(cluster_code == code)
    listing_data_sub$mohalla <- fill_in_NA_prev(listing_data_sub$mohalla)
    listing_data_sub$address_of_structure <- fill_in_NA_prev(listing_data_sub$address_of_structure)
    updated_listing <- rbind(updated_listing, listing_data_sub)
  }
  return(updated_listing)
}

listing_filled <- fill_in(join_proc)
