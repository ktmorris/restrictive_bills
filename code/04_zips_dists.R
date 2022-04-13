

zcta <- zctas(class = "sp")

all <- rbindlist(lapply(unique(filter(fips_codes, state_code <= 56,
                                      state != "DC")$state), function(s){
                                        b <- blocks(s, year = 2020) %>% 
                                          st_drop_geometry() %>% 
                                          mutate(across(starts_with("INTP"), as.numeric))
                                        
                                        upper <- state_legislative_districts(state = s, house = "upper", class = "sp")
                                        lower <- state_legislative_districts(state = s, house = "lower", class = "sp")
                                        
                                        pings  <- SpatialPoints(b[,c('INTPTLON20','INTPTLAT20')], proj4string = upper@proj4string)
                                        b$upper <- over(pings, upper)$GEOID
                                        b$lower <- over(pings, lower)$GEOID
                                        b$zip <- over(pings, zcta)$GEOID
                                        
                                        b <- select(b, GEOID = GEOID20, upper, lower, zip, POP20) %>% 
                                          mutate(state = s)
                                      }))

saveRDS(all, "temp/all_blocks_zips_chambers.rds")


all <- readRDS("temp/all_blocks_zips_chambers.rds")
#########################

cces <- fread("../regular_data/cces/CCES 2020/CES20_Common_OUTPUT_vv.csv",
              select = c("race", "lookupzip", "CC20_441a",
                         "CC20_441b")) %>% 
  filter(race == 1) %>% 
  mutate(CC20_441a = 6 - CC20_441a,
         rr = (CC20_441a + CC20_441b) / 2) %>% 
  mutate(zip = str_pad(zip, width = 5, side = "left", pad = "0")) %>% 
  group_by(zip) %>% 
  summarize(rr = mean(rr, na.rm = T))

all <- left_join(all, cces)

ll <- bind_rows(
  all %>% 
    group_by(GEOID = upper) %>% 
    summarize(resent = weighted.mean(rr, POP20, na.rm = T),
              n = n()) %>% 
    mutate(chamber = "SD"),
  all %>% 
    group_by(GEOID = lower) %>% 
    summarize(resent = weighted.mean(rr, POP20, na.rm = T),
              n = n()) %>% 
    mutate(chamber = "HD")
)


saveRDS(ll, "temp/district_rr.rds")
