
upper_chambers <- left_join(census_race_ethnicity("state legislative district (upper chamber)",
                                                  state = unique(fips_codes$state)[1:51],
                                                  year = 2019) %>% 
                              mutate(chamber = "SD"),
                            left_join(census_median_age("state legislative district (upper chamber)",
                                                        state = unique(fips_codes$state)[1:51],
                                                        year = 2019) %>% 
                                        mutate(chamber = "SD"),
                                      left_join(census_income("state legislative district (upper chamber)",
                                                              state = unique(fips_codes$state)[1:51],
                                                              year = 2019) %>% 
                                                  mutate(chamber = "SD"),
                                                census_education("state legislative district (upper chamber)",
                                                                 state = unique(fips_codes$state)[1:51],
                                                                 year = 2019) %>% 
                                                  mutate(chamber = "SD"))))

lower_chambers <- left_join(census_race_ethnicity("state legislative district (lower chamber)",
                                                  state = unique(fips_codes$state)[1:51],
                                                  year = 2019) %>% 
                              mutate(chamber = "HD"),
                            left_join(census_median_age("state legislative district (lower chamber)",
                                                        state = unique(fips_codes$state)[1:51],
                                                        year = 2019) %>% 
                                        mutate(chamber = "HD"),
                                      left_join(census_income("state legislative district (lower chamber)",
                                                              state = unique(fips_codes$state)[1:51],
                                                              year = 2019) %>% 
                                                  mutate(chamber = "HD"),
                                                census_education("state legislative district (lower chamber)",
                                                                 state = unique(fips_codes$state)[1:51],
                                                                 year = 2019) %>% 
                                                  mutate(chamber = "HD"))))

###############################

mnh <- fread("raw_data/mass_nh_GEOIDS.csv")

spons <- readRDS("temp/sponsor_intro_2021.rds") %>% 
  mutate(party = ifelse(bill == "MA S 2554" & position == 1, "D", party),
         party = ifelse(party == "" & position == 1, "R", party))

spons <- left_join(spons, mnh)

spons <- cSplit(spons, "district", sep = "-", drop = F) %>% 
  rename(dold = district)

spons <- left_join(spons,
                   fips_codes %>% 
                     select(state_name, state_code) %>% 
                     distinct(),
                   by = c("state" = "state_name")) %>% 
  mutate(district_2 = ifelse(state == "Alaska" & district_1 == "SD",
                             paste0("00", district_2), district_2),
         district_2 = ifelse(state == "Minnesota" & district_1 == "HD",
                             substring(district_2, 2), district_2),
         district_2 = ifelse(state == "Washington" & district_1 == "HD",
                             substring(district_2, 1, 3), district_2),
         district_2 = ifelse(state == "Maryland" & district_1 == "HD" &
                               substring(district_2, nchar(district_2)) %in% LETTERS,
                             substring(district_2, 2), district_2),
         district = paste0(state_code, district_2),
         district = ifelse(is.na(GEOID), district, GEOID)) %>% 
  rename(chamber = district_1) %>% 
  select(-state_code, -district_3) %>% 
  group_by(state, district, chamber, impact) %>% 
  tally() %>% 
  filter(!is.na(impact)) %>% 
  pivot_wider(id_cols = c(state, district, chamber), names_from = impact, values_from = n, values_fill = 0)

demos <- bind_rows(
  left_join(upper_chambers,
            filter(spons, chamber == "SD"),
            by = c("GEOID" = "district", "chamber")),
  left_join(lower_chambers,
            filter(spons, chamber == "HD"),
            by = c("GEOID" = "district", "chamber"))
) %>% 
  mutate(across(c(N, R, E), ~ ifelse(is.na(.), 0, .))) %>% 
  pivot_longer(c(N, R, E), names_to = "impact", values_to = "n") %>% 
  mutate(sp = n > 0,
         n2 = as.factor(n),
         state = substring(GEOID, 1, 2))

state_race <- census_race_ethnicity("state", year = 2019)

demos <- left_join(demos,
                   state_race %>% 
                     select(state = GEOID,
                            state_nh_white = nh_white)) %>% 
  filter(!is.na(nh_white)) %>% 
  mutate(bills = n)

demos <- mutate(demos,
                south = substring(GEOID, 1, 2) %in%
                  c("01", "48", "40", "22", "28", "05",
                    "12", "13", "47", "45", "37",
                    "21", "51", "54"))

m1a <- lm(sp ~ poly(nh_white, 2)*state_nh_white +
            median_income + median_age + population +
            some_college, filter(demos, chamber == "HD", impact == "R"))
m1b <- lm(sp ~ poly(nh_white, 2)*state_nh_white +
            median_income + median_age + population +
            some_college, filter(demos, chamber == "SD", impact == "R"))

marg <- bind_rows(
  ggeffect(model = m1a, terms = c("nh_white[all]", "state_nh_white[.5, .65, .8]")) %>% 
    mutate(group = scales::percent(as.numeric(as.character(group))),
           chamber = "Lower Chamber"),
  ggeffect(model = m1b, terms = c("nh_white[all]", "state_nh_white[.5, .65, .8]")) %>% 
    mutate(group = scales::percent(as.numeric(as.character(group))),
           chamber = "Upper Chamber")
)

ggplot(marg, aes(x = x, y = predicted,
                 ymin = conf.low, ymax = conf.high,
                 color = group,
                 fill = group)) +
  facet_grid(. ~ chamber) +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  theme_bc(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(0, 1, .2), labels = scales::percent) +
  labs(x = "White Share of District Population",
       y = "Probability that Representative Sponsored Restrictive Law(s)",
       fill = "White Share of\nState Population",
       color = "White Share of\nState Population",
       caption = "Covariates include median income; median age;
share with some college education; and population") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = bc_colors(c(1:3))) +
  scale_fill_manual(values = bc_colors(c(1:3)))

