preprocess_listing <- function(listing) {
  listing_trim <- listing %>% select(
    c(
      "id",
      "mohalla",
      "str_stat",
      "str_index",
      "address_of_structure",
      "unit",
      "hh_stat",
      "hh_stat_log",
      "hh_index",
      "head_of_house_hold",
      "datetime_entry",
      "submissions_id",
      "non_dwelling_type",
      "non_dwelling_other",
      "remarks"
    )
  )
  
  names(listing_trim) <-
    c(
      "entry_index",
      "mohalla",
      "str_stat",
      "str_index",
      "address_of_structure",
      "unit",
      "hh_type",
      "hh_stat",
      "hh_index",
      "head_of_house_hold",
      "datetime",
      "id",
      "non_dwelling_type",
      "non_dwelling_other",
      "remarks"
    )
  
  return(listing_trim)
}

listing_trim <- preprocess_listing(listing)

listing_join <- listing_summary_trim %>% left_join(listing_trim)

listing_join$est_hh_who <- as.numeric(listing_join$est_hh_who)





