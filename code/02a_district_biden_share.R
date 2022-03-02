d <- list.dirs("raw_data/dataverse_files")
d <- d[!(d %in% c("raw_data/dataverse_files", "raw_data/dataverse_files/nj_2020",
                  "raw_data/dataverse_files/dc_2020"))]

dat <- rbindlist(lapply(d, function(s){
  state <- substring(s, 26, 27)
  print(state)
  
  lower <- tigris::state_legislative_districts(state = state, house = "lower", class = "sp")
  upper <- tigris::state_legislative_districts(state = state, house = "upper", class = "sp")
  
  pd <- readOGR(paste0("raw_data/dataverse_files/", state, "_2020"),
                paste0(state, "_2020"))
  
  pd <- spTransform(pd, CRS(lower@proj4string@projargs))
  
  pd@data$id <- c(1:nrow(pd@data))
  centroids <- gCentroid(pd, byid=TRUE)
  
  pd@data$lower <- over(centroids, lower)$GEOID
  pd@data$upper <- over(centroids, upper)$GEOID
  
  lower_chambers <- pd@data %>%
    mutate_at(vars(starts_with("G20")), ~ as.integer(gsub(",", "", .))) %>%
    group_by(lower) %>% 
    summarize(across(starts_with("G20PRE"), sum),
              n = n()) %>% 
    mutate(share_dem = G20PREDBID /
             (rowSums(select(., starts_with("G20PRE"))))) %>% 
    select(GEOID = lower, share_dem, n) %>% 
    mutate(state = state,
           chamber = "HD")
  
  upper_chambers <- pd@data %>%
    mutate_at(vars(starts_with("G20")), ~ as.integer(gsub(",", "", .))) %>%
    group_by(upper) %>% 
    summarize(across(starts_with("G20PRE"), sum),
              n = n()) %>% 
    mutate(share_dem = G20PREDBID /
             (rowSums(select(., starts_with("G20PRE"))))) %>% 
    select(GEOID = upper, share_dem, n) %>% 
    mutate(state = state,
           chamber = "SD")
  
  set <- bind_rows(lower_chambers, upper_chambers)
  
  return(set)
}), fill = T)

saveRDS(dat, "temp/district_biden_share.rds")
