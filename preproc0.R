list_st_pt <- listing_summary %>% 
  select(
    c(
      "id",                                              
      "enumerator_name",                                 
      "enumerator_mobile",                               
      "survey_day",                                      
      "prepopulate_cluster_code",                        
      "detail_province_name",                            
      "detail_division",                                 
      "detail_district_name",                            
      "detail_estimated_no_of_households",               
      "need_segmention",                                 
      "segment_select_segment_divide",                   
      "segment_select_segment_1_name",                   
      "segment_select_segment1_count",                   
      "segment_select_segment_2_name",                   
      "segment_select_segment2_count",                   
      "segment_select_segment_3_name",                   
      "segment_select_segment3_count",                   
      "segment_select_segment_4_name",                   
      "segment_select_segment4_count",                   
      "segment_select_segment_5_name",                  
      "segment_select_segment5_count",                   
      "segment_select_segment_6_name",                   
      "segment_select_segment6_count",                   
      "segment_select_segment_7_name",                   
      "segment_select_segment7_count",                   
      "segment_select_segment_8_name",                   
      "segment_select_segment8_count",                   
      "segment_select_segment_9_name",                   
      "segment_select_segment9_count",                   
      "segment_select_segment_10_name",                  
      "segment_select_segment10_count",                  
      "segment_select_confirmation_segment_work",        
      "segment_select_confirmation_starting_point"      
    )
  ) %>% 
  filter(need_segmention == 1) %>%
  filter(!duplicated(prepopulate_cluster_code))


names(list_st_pt) <- c(
  "id",                                              
  "enumerator_name",                                 
  "enumerator_mobile",                               
  "survey_day",                                      
  "cluster_code",                        
  "province",                            
  "division",                                 
  "district",                            
  "est_hh_who",               
  "need_segmention",                                 
  "n_segments",                   
  "seg1_name",                   
  "seg1_count",                   
  "seg2_name",                   
  "seg2_count",                   
  "seg3_name",                   
  "seg3_count",                   
  "seg4_name",                   
  "seg4_count",                   
  "seg5_name",                  
  "seg5_count",                   
  "seg6_name",                   
  "seg6_count",                   
  "seg7_name",                   
  "seg7_count",                   
  "seg8_name",                   
  "seg8_count",                   
  "seg9_name",                   
  "seg9_count",                   
  "seg10_name",                  
  "seg10_count",                  
  "chosen_segment",        
  "segment_starting_point"      
)


make_segments_df <- function(list_st_pt){
  seg_name_vec <- c()
  seg_count_vec <- c()
  
  for (idx in 1:nrow(list_st_pt)){
    seg_id <- list_st_pt$chosen_segment[idx]
    seg_name <- list_st_pt[[paste0("seg", seg_id, "_name")]][idx]
    seg_count <- list_st_pt[[paste0("seg", seg_id, "_count")]][idx]
    seg_name_vec <- c(seg_name_vec, seg_name)
    seg_count_vec <- c(seg_count_vec, seg_count)
  }
  
  segments_df <- data.frame(
    cluster_code = as.numeric(list_st_pt$cluster_code),
    seg_id = list_st_pt$chosen_segment,
    seg_name = seg_name_vec,
    seg_count = seg_count_vec
  )
  
  return(segments_df)
}

segments_df <- make_segments_df(list_st_pt)
list_st_pt$chosen_segment_name <- segments_df$seg_name
list_st_pt$log1 <- ifelse(list_st_pt$seg1_name == list_st_pt$chosen_segment_name, 1, 0)
list_st_pt$log2 <- ifelse(list_st_pt$seg2_name == list_st_pt$chosen_segment_name, 1, 0)
list_st_pt$log3 <- ifelse(list_st_pt$seg3_name == list_st_pt$chosen_segment_name, 1, 0)
list_st_pt$log4 <- ifelse(list_st_pt$seg4_name == list_st_pt$chosen_segment_name, 1, 0)
list_st_pt$log5 <- ifelse(list_st_pt$seg5_name == list_st_pt$chosen_segment_name, 1, 0)
list_st_pt$log6 <- ifelse(list_st_pt$seg6_name == list_st_pt$chosen_segment_name, 1, 0)
list_st_pt$log7 <- ifelse(list_st_pt$seg7_name == list_st_pt$chosen_segment_name, 1, 0)
list_st_pt$log8 <- ifelse(list_st_pt$seg8_name == list_st_pt$chosen_segment_name, 1, 0)
list_st_pt$log9 <- ifelse(list_st_pt$seg9_name == list_st_pt$chosen_segment_name, 1, 0)
list_st_pt$log10 <- ifelse(list_st_pt$seg10_name == list_st_pt$chosen_segment_name, 1, 0)
