already_done_wide <- read_csv('who-dash/Data/sample-wide.csv')
already_done_long <- read_csv('who-dash/Data/sample-long.csv')

already_done_clusts <- already_done_wide$cluster_code

approved_summary <-
  summary_seg %>% 
  filter(review_state == "approved") %>% 
  filter(!(cluster_code %in% already_done_clusts))
approved_listing <-
  listing_filled %>% filter(review_state == "approved") %>%
  filter(!(cluster_code %in% already_done_clusts))
## list fucked up (mismatch clusters)
approved_summary$cluster_code[!approved_summary$n_hh == approved_summary$max_hh]

sys_rand <- function(n, sampSize = 6, clust_code) {
  if (n < sampSize) {
    print("n can't be less than sample size")
  } else {
    interval <- floor(n / sampSize)
    randStart <-
      ifelse(interval > 1, sample(1:interval, 1), sample(1:(n - sampSize), 1))
    sampleVec <- seq(randStart, n, interval)[1:sampSize]
  }
  
  approved_listing_sub <- approved_listing %>% filter(cluster_code == clust_code) %>% 
    filter(hh_stat == 1) %>% 
    filter(hh_index %in% sampleVec)
  
  if (nrow(approved_listing_sub) == 6){
    return(sampleVec)
  } else {
    sys_rand(n, sampSize = 6, clust_code)
  }
}


sample_list <- list()

if (nrow(approved_summary) >= 1){
  for (i in 1:nrow(approved_summary)){
    clust <- approved_summary$cluster_code[i]
    n_hh <- approved_summary$n_hh[i]
    sample_list[[i]] <- sys_rand(n = n_hh, clust_code = clust)  
  }  
}

sampleDF <- data.frame(do.call("rbind", sample_list))

sampleDF$cluster_code <- approved_summary$cluster_code
sampleDF$n_hh <- approved_summary$n_hh


if (nrow(sampleDF)){
  sampleDF <- sampleDF %>% select(c("cluster_code",
                                    "n_hh",
                                    "X1",
                                    "X2",
                                    "X3",
                                    "X4",
                                    "X5",
                                    "X6"))
  
  names(sampleDF) <- c("cluster_code",
                       "n_hh",
                       "idx1",
                       "idx2",
                       "idx3",
                       "idx4",
                       "idx5",
                       "idx6")
}


sampledDF <- data.frame()

if (nrow(approved_listing)){
  for (i in 1:length(approved_summary$cluster_code)){
    clus_cde <- approved_summary$cluster_code[i]
    listing_sub_data <- approved_listing %>% 
      filter(cluster_code == clus_cde) %>% 
      filter(hh_stat == 1) %>% 
      filter(hh_index %in% sample_list[[i]])
    print(nrow(listing_sub_data))
    print(clus_cde)
    sampledDF <- rbind(sampledDF, listing_sub_data)
  }
  
  sampledDF <- sampledDF %>% select(c(
    "cluster_code",
    "province_name",
    "district",
    "hh_index",
    "head_of_house_hold",
    "address_of_structure"
  )) 
}


sampleDF <- rbind(already_done_wide, sampleDF)
sampledDF <- rbind(already_done_long, sampledDF)

write.csv(sampleDF,
          "who-dash/Data/sample-wide.csv",
          row.names = F,
          na = "")
write.csv(sampledDF,
          "who-dash/Data/sample-long.csv",
          row.names = F,
          na = "")
