# census <- get_basic_census_stats("state", 2019)
# saveRDS(census, "temp/state_census_2019.rds")
census <- readRDS("temp/state_census_2019.rds")


census_09 <- census_race_ethnicity("state", year = 2009) %>% 
  select(GEOID, nh_white_2009 = nh_white)

state_level <- readRDS("temp/clean_provisions_2021.rds") %>% 
  filter(impact == "R") %>% 
  group_by(state, pass, group) %>% 
  tally()

census <- left_join(census,
                    state_level %>% 
                      group_by(state) %>% 
                      summarize(passed = sum(n * pass),
                                intro = sum(n)),
                    by = c("NAME" = "state")) %>% 
  filter(!(NAME %in% c("Puerto Rico", "District of Columbia"))) %>% 
  mutate(across(c(intro, passed), ~ifelse(is.na(.), 0, .)))

census <- left_join(census, census_09) %>% 
  mutate(change_white = nh_white / nh_white_2009)

############
covi <- readxl::read_xlsx("raw_data/COVI Values 1996-2020old and new.xlsx")

census <- left_join(census,
                    left_join(select(fips_codes, state, state_name) %>% 
                                distinct(),
                              select(covi, state, year, FinalCOVI) %>% 
                                filter(year == 2020) %>% 
                                select(-year)) %>% 
                      select(-state) %>% 
                      rename(state = state_name),
                    by = c("NAME" = "state")) %>% 
  mutate(change_white = change_white - 1,
         change_white_abs = nh_white - nh_white_2009)

comp <- fread("raw_data/1976-2020-president.csv") %>% 
  group_by(state, party_simplified, year) %>% 
  summarize(candidatevotes = sum(candidatevotes)) %>% 
  filter(party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) %>% 
  group_by(state, year) %>% 
  mutate(share = candidatevotes / sum(candidatevotes),
         state = str_to_title(state)) %>% 
  filter(party_simplified == "DEMOCRAT",
         year > 2012) %>% 
  select(state, year, share) %>% 
  pivot_wider(state, names_from = year, values_from = share, names_prefix = "dem_share_")

census <- left_join(census, comp, by = c("NAME" = "state")) %>% 
  mutate(change_dem = dem_share_2020 - dem_share_2016,
         population = log(population),
         median_income = median_income / 10000)
#####################################

m1ab <- lm(intro ~ poly(nh_white, 2) + FinalCOVI, census)

m1a <- lm(intro ~ poly(nh_white, 2) + FinalCOVI +
            change_dem + dem_share_2020 +
            median_income + median_age + population +
            some_college, census)

len <- paste(seq(min(census$nh_white),
                 max(census$nh_white), 0.01), collapse = ", ")

marg <- ggeffect(m1a, terms = c(paste0("nh_white[", len, "]")))

p1 <- ggplot(marg, aes(x = x, y = predicted,
                       ymin = conf.low, ymax = conf.high)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  theme_bc(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "White Share of Population",
       y = "Restrictive Provisions Introduced",
       color = "2020 COVI",
       fill = "2020 COVI") +
  ggtitle("(A) Provisions Introduced")
p1

m2ab <- lm(passed ~ poly(nh_white, 2) + FinalCOVI, census)

m2a <- lm(passed ~ poly(nh_white, 2) + FinalCOVI +
            change_dem + dem_share_2020 +
            median_income + median_age + population +
            some_college, census)

marg <- ggeffect(m2a, terms = c(paste0("nh_white[", len, "]")))

p2 <- ggplot(marg, aes(x = x, y = predicted,
                       ymin = conf.low, ymax = conf.high)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  theme_bc(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "White Share of Population",
       y = "Restrictive Provisions Passed",
       color = "2020 COVI",
       fill = "2020 COVI") +
  ggtitle("(B) Provisions Passed")

p2

p <- plot_grid(p1, p2,
               label_size = 12,
               label_fontfamily = "LM Roman 10")
p

ggdraw(add_sub(p, "   Covariates include median income; median age; change in Democratic voteshare, 2016--2020;
   Biden voteshare, 2020; share with some college; log(population).", x = 0, hjust = 0,
               fontfamily = "BentonSans", size = 12))


#############################################################

m1aa <- lm(intro ~ poly(nh_white, 2) * FinalCOVI, census)

m1 <- lm(intro ~ poly(nh_white, 2) * FinalCOVI +
           change_dem + dem_share_2020 +
           median_income + median_age + population +
           some_college, census)

p <- paste(seq(min(census$nh_white),
               max(census$nh_white), 0.01), collapse = ", ")

marg <- ggeffect(m1, terms = c(paste0("nh_white[", p, "]"), "FinalCOVI[-1, 0, 1]"))

p1 <- ggplot(marg, aes(x = x, y = predicted,
                       color = group, fill = group,
                       ymin = conf.low, ymax = conf.high)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  theme_bc(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "White Share of Population",
       y = "Restrictive Provisions Introduced",
       color = "2020 COVI",
       fill = "2020 COVI") +
  ggtitle("(A) Provisions Introduced")

ggsave("temp/state_intro.png")

m2aa <- lm(passed ~ poly(nh_white, 2) * FinalCOVI, census)

m2 <- lm(passed ~ poly(nh_white, 2) * FinalCOVI +
           change_dem + dem_share_2020 +
           median_income + median_age + population +
           some_college, census)

marg <- ggeffect(m2, terms = c(paste0("nh_white[", p, "]"), "FinalCOVI[-1, 0, 1]"))

p2 <- ggplot(marg, aes(x = x, y = predicted,
                       color = group, fill = group,
                       ymin = conf.low, ymax = conf.high)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  theme_bc(legend.position = "none") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "White Share of Population",
       y = "Restrictive Provisions Passed",
       color = "2020 COVI",
       fill = "2020 COVI") +
  ggtitle("(B) Provisions Passed")

legend_b <- get_legend(
  p1 + theme(legend.position = "bottom")
)

ggsave("temp/state_passed.png")

p <- plot_grid(p1, p2,
               label_size = 12,
               label_fontfamily = "LM Roman 10")

j <- plot_grid(p, legend_b, ncol = 1, rel_heights = c(1, .1))

ggdraw(add_sub(j, "   Covariates include median income; median age; change in Democratic voteshare, 2016--2020;
   Biden voteshare, 2020; share with some college; log(population).", x = 0, hjust = 0,
               fontfamily = "BentonSans", size = 12))

modelsummary(list("Introduced" = m1ab, "Passed" = m2ab,
                  "Introduced" = m1a, "Passed" = m2a,
                  "Introduced" = m1aa, "Passed" = m2aa,
                  "Introduced" = m1, "Passed" = m2),
             statistic = "std.error",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE|F',
             coef_map = c("poly(nh_white, 2)1" = "Nonhispanic White",
                          "poly(nh_white, 2)2" = "Nonhispanic White\\textsuperscript{2}",
                          "FinalCOVI" = "2020 COVI",
                          "poly(nh_white, 2)1:FinalCOVI" = "Nonhispanic White $\\times$ 2020 COVI",
                          "poly(nh_white, 2)2:FinalCOVI" = "Nonhispanic White\\textsuperscript{2} $\\times$ 2020 COVI",
                          "change_dem" = "Change in Dem. Vote Share 2016--2020",
                          "dem_share_2020" = "Biden Voteshare, 2020",
                          "median_income" = "Median Income (\\$10,000s)",
                          "median_age" = "Median Age",
                          "some_college" = "Share with Some College",
                          "population" = "Log(Population)",
                          "(Intercept)" = "Intercept"),
             output = "test.tex",
             escape = F)

###############
cleanup("state_level")

types <- state_level %>% 
  group_by(group) %>% 
  summarize(intro = sum(n),
            passed = sum(n * pass)) %>% 
  mutate(group = case_when(group == "3pvro" ~    "Third-Party Registration Organizations",
                           group == "absentee" ~ "Absentee Voting",
                           group == "disenfranchisement" ~ "Felony Disenfranchisement",
                           group == "early" ~ "Early Voting",
                           group == "voter id" ~ "Voter ID",
                           TRUE ~ str_to_title(group)),
         group = gsub(" ", "\n", group),
         intro2 = ifelse(group == "Other", -999, intro),
         passed2 = ifelse(group == "Other", -999, passed))

p1 <- ggplot(types, aes(y = reorder(group, intro2), x = intro)) +
  geom_col(fill = "white", color = "black") +
  theme_bc() +
  labs(x = "Number of Introduced Provisions",
       y = "Category") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(types$intro) + 5))
p1

p2 <- ggplot(types, aes(y = reorder(group, passed2), x = passed)) +
  geom_col(fill = "white", color = "black") +
  theme_bc() +
  labs(x = "Number of Passed Provisions",
       y = "Category") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(types$passed) + 1))
p2

gr <- plot_grid(p1, p2)
gr
saveRDS(gr, "temp/column_cat_provs.rds")
## pull state shapefiles with tigris
state_map <- states(cb = T) %>% 
  filter(STATEFP <= "56",
         STATEFP != "11",
  )
## shift HI and AK
state_map <- shift_geometry(state_map)

state_map <- as_Spatial(state_map)

state_map <- spTransform(state_map, "+proj=longlat +datum=NAD83 +no_defs")

state_map <- left_join(mutate(state_map@data, id = as.character(row_number())),
                       fortify(state_map))

states <- bind_rows(state_level %>% 
                      group_by(state) %>% 
                      summarize(passed = sum(n * pass),
                                intro = sum(n)),
                    data.table(state = "Vermont", passed = 0, intro = 0))

natural.interval <- classIntervals(states$intro, n = 4, style = 'jenks')$brks
states$introb = cut(states$intro, breaks = natural.interval, include.lowest = TRUE)

natural.interval <- classIntervals(states$passed, n = 4, style = 'jenks')$brks
states$passedb = cut(states$passed, breaks = natural.interval, include.lowest = TRUE)

states <- cSplit(states, "introb")
states <- cSplit(states, "passedb")

states <- states %>% 
  mutate(across(c(introb_1, passedb_1), ~ ifelse(substring(., 1, 1) == "(", as.integer(substring(., 2)) + 1,
                                                 as.integer(substring(., 2)))),
         across(c(introb_2, passedb_2), ~ as.integer(substring(., 1, nchar(.) - 1))),
         introb = paste(introb_1, introb_2, sep = "-"),
         passedb = paste(passedb_1, passedb_2, sep = "-")) %>% 
  select(-ends_with("_1"), -ends_with("_2"))

state_map <- left_join(state_map,
                       states,
                       by = c("NAME" = "state"))

map1 <- ggplot() +
  geom_polygon(data = filter(state_map,
                             long > -130), mapping = aes(x = long, y = lat, group = group,
                                                         fill = introb),
               color = "black") +
  coord_map() +
  theme_bc(base_family = "LM Roman 10") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = bc_colors(state_map$introb)) +
  labs(fill = "", x = NULL, y = NULL) +
  ggtitle("Provisions Introduced")
saveRDS(map1, "temp/map_intro.rds")

map2 <- ggplot() +
  geom_polygon(data = filter(state_map,
                             long > -130), mapping = aes(x = long, y = lat, group = group,
                                                         fill = passedb),
               color = "black") +
  coord_map() +
  theme_bc(base_family = "LM Roman 10") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = bc_colors(state_map$introb)) +
  labs(fill = "", x = NULL, y = NULL) +
  ggtitle("Provisions Passed")
saveRDS(map2, "temp/map_passed.rds")


gr <- plot_grid(map1, map2)
gr
saveRDS(gr, "temp/map_provs.rds")
