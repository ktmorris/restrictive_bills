
upper_chambers <- census_race_ethnicity("state legislative district (upper chamber)",
                                        state = unique(fips_codes$state)[1:51],
                                        year = 2019) %>% 
  mutate(chamber = "SD")

lower_chambers <- census_race_ethnicity("state legislative district (lower chamber)",
                                        state = unique(fips_codes$state)[1:51],
                                        year = 2019) %>% 
  mutate(chamber = "HD")

###############################

mnh <- fread("raw_data/mass_nh_GEOIDS.csv")

spons <- readRDS("temp/sponsors_intro_2022.rds") %>% 
  mutate(party = ifelse(bill == "MA S 2554" & position == 1, "D", party),
         party = ifelse(party == "" & position == 1, "R", party))

spons <- left_join(spons, mnh)

spons <- cSplit(spons, "district", sep = "-")

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
  group_by(state, district, chamber) %>% 
  tally()

demos <- bind_rows(
  left_join(upper_chambers,
            filter(spons, chamber == "SD"),
            by = c("GEOID" = "district", "chamber")),
  left_join(lower_chambers,
            filter(spons, chamber == "HD"),
            by = c("GEOID" = "district", "chamber"))
) %>% 
  mutate(n = ifelse(is.na(n), 0, n),
         sp = n > 0,
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

m1a <- lm(sp ~ poly(nh_white, 2)*state_nh_white, filter(demos, chamber == "HD"))
m1b <- lm(sp ~ poly(nh_white, 2)*state_nh_white, filter(demos, chamber == "SD"))

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
       y = "Probability that Representative Sponsored Regressive Laws",
       fill = "White Share of\nState Population",
       color = "White Share of\nState Population") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = bc_colors(c(1:3))) +
  scale_fill_manual(values = bc_colors(c(1:3)))

#############################

m2 <- lm(sp ~ poly(nh_white, 2) + state, filter(demos, chamber == "HD"))

marg1 <- ggeffect(model = m2, terms = c("nh_white [all]"))

ggplot(marg1, aes(x = x, y = predicted,
                 ymin = conf.low, ymax = conf.high)) +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  theme_bc() +
  scale_x_continuous(breaks = seq(0.2, 1, .2), limits = c(.2, 1),
                     labels = scales::percent) +
  scale_y_continuous(limits = c(0, 4)) +
  labs(x = "White Share of Population",
       y = "Number of Restrictive Bills Sponsored by Representative")

m3 <- lm(sp ~ poly(nh_white, 2) + state, filter(demos, chamber == "SD"))

marg2 <- ggeffect(model = m3, terms = c("nh_white [all]"))

ggplot(marg2, aes(x = x, y = predicted,
                  ymin = conf.low, ymax = conf.high)) +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  theme_bc() +
  scale_x_continuous(breaks = seq(0.2, 1, .2), limits = c(.2, 1),
                     labels = scales::percent) +
  scale_y_continuous(limits = c(0, 6)) +
  labs(x = "White Share of Population",
       y = "Number of Restrictive Bills Sponsored by Representative")


marg <- bind_rows(
  marg1 %>% 
    mutate(chamber = "Lower Chamber"),
  marg2 %>% 
    mutate(chamber = "Upper Chamber")
)

ggplot(data = marg, aes(x = x, y = predicted,
                  ymin = conf.low, ymax = conf.high)) +
  facet_grid(. ~ chamber) +
  geom_ribbon(data = marg, alpha = 0.2) +
  geom_line(data = marg) +
  theme_bc() +
  scale_x_continuous(breaks = seq(0.2, 1, .2), limits = c(.4, 1.001),
                     labels = scales::percent) +
  scale_y_continuous(limits = c(-.1, 0.5), labels = scales::percent) +
  labs(x = "White Share of Population",
       y = "Probability that Representative has Sponsored Restrictive Voting Law",
       caption = "Note: Relationship is net of state-level effects.")

#################

spons <- readRDS("temp/sponsors_intro_2022.rds") %>% 
  mutate(party = ifelse(bill == "MA S 2554" & position == 1, "D", party),
         party = ifelse(party == "" & position == 1, "R", party)) %>% 
  # filter(position == 1) %>%
  group_by(party) %>% 
  tally() %>% 
  mutate(party = case_when(party == "R" ~ "Republican",
                           party == "D" ~ "Democrat",
                           party == "N" ~ "Non-Partisan"))

ggplot(filter(spons, party %in% c("Republican", "Democrat")),
       aes(x = party, y = n, label = scales::comma(n))) +
  geom_col() +
  theme_bc() +
  labs(x = "Party", y = "Number of (Co)Sponsors on Regressive Bills") +
  scale_y_continuous(labels = scales::comma) +
  geom_text(position = position_dodge(width = 1), vjust = -.5, family = "BentonSans")
  
spons <- readRDS("temp/sponsors_intro_2022.rds") %>% 
  mutate(party = ifelse(bill == "MA S 2554" & position == 1, "D", party),
         party = ifelse(party == "" & position == 1, "R", party)) %>% 
  filter(position == 1) %>%
  group_by(party) %>% 
  tally() %>% 
  mutate(party = case_when(party == "R" ~ "Republican",
                           party == "D" ~ "Democrat",
                           party == "N" ~ "Non-Partisan"))

ggplot(filter(spons, party %in% c("Republican", "Democrat")),
       aes(x = party, y = n, label = scales::comma(n))) +
  geom_col() +
  theme_bc() +
  labs(x = "Party", y = "Number of Primary Sponsors") +
  scale_y_continuous(labels = scales::comma) +
  geom_text(position = position_dodge(width = 1), vjust = -.5, family = "BentonSans")

