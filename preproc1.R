preprocess_summary <- function(listing_summary){
  trim_summary <- listing_summary %>% select(
    c(
      "id",                                              
      "enumerator_name",
      "enumerator_mobile",
      "prepopulate_cluster_code",                        
      "detail_province_name",                            
      "detail_division",                                 
      "detail_district_name",                            
      "detail_tehsil_name",                              
      "detail_unname",                                   
      "detail_settlement_name",                          
      "detail_estimated_no_of_households",               
      "detail_cluster_no_by_department",                 
      "start_date",                                      
      "endtime",                                         
      "start_geopoint_longitude",                        
      "start_geopoint_latitude",
      "system_review_state"
    )
  )
  
  names(trim_summary) <- 
    c(
      "id",                                              
      "enumerator_name",
      "enumerator_mobile",
      "cluster_code",                        
      "province_name",                            
      "division",                                 
      "district",                            
      "tehsil",                              
      "uc",                                   
      "settlement",                          
      "est_hh_who",               
      "cluster_no_who",                 
      "start_time",                                      
      "end_time",                                         
      "longitude",                        
      "latitude",
      "review_state"
    )
  
  trim_summary$start_time <- as.POSIXct(trim_summary$start_time)
  trim_summary$end_time <- as.POSIXct(trim_summary$end_time)
  
  return(trim_summary)
}

listing_summary_trim <- preprocess_summary(listing_summary)
