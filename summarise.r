summary <- listing_filled %>%
  group_by(cluster_code, province_name, district, est_hh_who) %>%
  summarise(
    start_date = first(start_time),
    n = n(),
    n_str = sum(str_stat),
    max_str = max(str_index, na.rm = T),
    n_hh = sum(hh_stat),
    max_hh = max(hh_index, na.rm = T),
    n_dwelling = sum(unit == "Dwelling"),
    n_non_dwelling = sum(unit == "Non_Dwelling"),
    n_subs = (length(unique(id)))
  )

summary$cluster_code <- as.numeric(summary$cluster_code)
summary_seg <- summary %>% left_join(segments_df)

summary_seg$est_hh_eff <- ifelse(!is.na(summary_seg$seg_id), summary_seg$seg_count, summary$est_hh_who)
summary_seg$disc_hh <- round(summary_seg$n_hh / summary_seg$est_hh_eff, 2)

summary_seg <- summary_seg %>%
  select(
    c(
      "cluster_code",
      "province_name",
      "district",
      "start_date",
      "n",
      "n_str",
      "max_str",
      "n_hh",
      "max_hh",
      "est_hh_who",
      "disc_hh",
      "n_dwelling",
      "n_non_dwelling",
      "n_subs",
      "seg_id",
      "seg_name",
      "seg_count"
    )
  )

write.csv(summary_seg, "who-dash/Data/summary.csv", row.names = F, na = "")
write.csv(listing_filled,
          "who-dash/Data/listing.csv",
          row.names = F,
          na = "")
write.csv(list_st_pt,
          "who-dash/Data/segments.csv",
          row.names = F,
          na = "")

