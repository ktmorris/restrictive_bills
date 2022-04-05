

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

upper_z <- all %>% 
  group_by(upper, zip) %>% 
  summarize(pop = sum(POP20)) %>% 
  group_by(zip) %>% 
  arrange(-pop) %>% 
  filter(row_number() == 1)

lower_z <- all %>% 
  group_by(lower, zip) %>% 
  summarize(pop = sum(POP20)) %>% 
  group_by(zip) %>% 
  arrange(-pop) %>% 
  filter(row_number() == 1)

saveRDS(upper_z, "temp/upper_zips.rds")
saveRDS(lower_z, "temp/lower_zips.rds")

#########################

cces <- fread("M:/Democracy & Justice/democracy/regular_data/cces/CCES 2020/CES20_Common_OUTPUT_vv.csv") %>% 
  filter(race == 1) %>% 
  select(lookupzip, CC20_441a, CC20_441b) %>% 
  mutate(CC20_441a = 6 - CC20_441a,
         rr = (CC20_441a + CC20_441b) / 2) %>% 
  group_by(zip = lookupzip) %>% 
  summarize(rr = mean(rr)) %>% 
  mutate(zip = str_pad(zip, width = 5, side = "left", pad = "0"))

cces <- left_join(cces, readRDS("temp/upper_zips.rds")) %>% 
  select(-pop)

cces <- left_join(cces, readRDS("temp/lower_zips.rds")) %>% 
  select(-pop)

saveRDS(cces, "temp/district_rr.rds")
