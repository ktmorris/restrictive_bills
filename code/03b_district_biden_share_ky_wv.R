

all <- rbindlist(lapply(c("WV", "KY"), function(s){
                                        b <- blocks(s, year = 2020) %>% 
                                          st_drop_geometry() %>% 
                                          mutate(across(starts_with("INTP"), as.numeric))
                                        
                                        upper <- state_legislative_districts(state = s, house = "upper", class = "sp")
                                        lower <- state_legislative_districts(state = s, house = "lower", class = "sp")
                                        
                                        pings  <- SpatialPoints(b[,c('INTPTLON20','INTPTLAT20')], proj4string = upper@proj4string)
                                        b$upper <- over(pings, upper)$GEOID
                                        b$lower <- over(pings, lower)$GEOID
                                        
                                        b <- select(b, GEOID = GEOID20, upper, lower, county = COUNTYFP20, POP20) %>% 
                                          mutate(state = s)
                                      }))

saveRDS(all, "temp/blocks_county_dist_ky_wv.rds")

all <- all %>% 
  mutate(county = paste0(substring(GEOID, 1, 2), county))

results <- fread("./raw_data/county_results_mit/countypres_2000-2020.csv")

results <- filter(results,
                  year == 2020,
                  state_po %in% c("KY", "WV"),
                  party %in% c("REPUBLICAN", "DEMOCRAT")) %>% 
  group_by(county_fips = as.character(county_fips)) %>% 
  summarize(share_dem = sum(candidatevotes * (party == "DEMOCRAT")) / sum(candidatevotes))

all <- left_join(all, results, by = c("county" = "county_fips"))

dist_share <- bind_rows(
  all %>% 
    group_by(GEOID = upper) %>% 
    summarize(share_dem = weighted.mean(share_dem, POP20)) %>% 
    mutate(chamber = "SD"),
  all %>% 
    group_by(GEOID = lower) %>% 
    summarize(share_dem = weighted.mean(share_dem, POP20)) %>% 
    mutate(chamber = "HD")
)

saveRDS(dist_share, "temp/ky_wv_dist_dem_share.rds")
