
upper_chambers <- left_join(census_race_ethnicity("state legislative district (upper chamber)",
                                                  state = unique(fips_codes$state)[1:51],
                                                  year = 2019) %>% 
                              mutate(chamber = "SD"),
                            left_join(census_race_ethnicity("state legislative district (upper chamber)",
                                                            state = unique(fips_codes$state)[1:51],
                                                            year = 2009) %>% 
                                        select(GEOID, nh_white_2009 = nh_white) %>% 
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
                                                            mutate(chamber = "SD")))))

lower_chambers <- left_join(census_race_ethnicity("state legislative district (lower chamber)",
                                                  state = unique(fips_codes$state)[1:51],
                                                  year = 2019) %>% 
                              mutate(chamber = "HD"),
                            left_join(census_race_ethnicity("state legislative district (lower chamber)",
                                                            state = unique(fips_codes$state)[1:51],
                                                            year = 2009) %>%  
                                        select(GEOID, nh_white_2009 = nh_white) %>% 
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
                                                            mutate(chamber = "HD")))))

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
  group_by(state, district, chamber, impact, party) %>% 
  summarize(n = n(),
            n_pass = sum(pass)) %>% 
  filter(!is.na(impact)) %>% 
  pivot_wider(id_cols = c(state, district, chamber, party), names_from = impact,
              values_from = c(n, n_pass), values_fill = 0) %>% 
  group_by(state, district, chamber) %>% 
  mutate(party = ifelse(n() > 1, "M", party)) %>% 
  group_by(state, district, chamber, party) %>% 
  summarize(across(starts_with("n_"), sum))

demos <- bind_rows(
  left_join(upper_chambers,
            filter(spons, chamber == "SD"),
            by = c("GEOID" = "district", "chamber")),
  left_join(lower_chambers,
            filter(spons, chamber == "HD"),
            by = c("GEOID" = "district", "chamber"))
) %>% 
  mutate(across(starts_with("n_"), ~ ifelse(is.na(.), 0, .))) %>% 
  pivot_longer(starts_with("n_"), names_to = "impact", values_to = "n") %>% 
  mutate(sp = n > 0,
         state = substring(GEOID, 1, 2),
         passed = ifelse(grepl("pass", impact), "pass", "intro"),
         impact = substring(impact, nchar(impact))) %>% 
  pivot_wider(names_from = passed, values_from = c(n, sp))

state_race <- census_race_ethnicity("state", year = 2019)

demos <- left_join(demos,
                   state_race %>% 
                     select(state = GEOID,
                            state_nh_white = nh_white)) %>% 
  filter(!is.na(nh_white))

demos <- left_join(demos, readRDS("temp/district_biden_share.rds") %>% 
                     select(-state))

demos <- mutate(demos,
                south = substring(GEOID, 1, 2) %in%
                  c("01", "48", "40", "22", "28", "05",
                    "12", "13", "47", "45", "37",
                    "21", "51", "54"),
                population = log(population),
                median_income = median_income / 10000)

demos$change_white <- demos$nh_white / demos$nh_white_2009

demos <- filter(demos, !is.na(share_dem))

m1a <- lm(sp_intro ~ poly(nh_white, 2)*state_nh_white, filter(demos, chamber == "HD", impact == "R"))
m1b <- lm(sp_intro ~ poly(nh_white, 2)*state_nh_white + share_dem +
            median_income + median_age + population +
            some_college, filter(demos, chamber == "HD", impact == "R"))
m1c <- lm(sp_intro ~ poly(nh_white, 2)*state_nh_white, filter(demos, chamber == "SD", impact == "R"))
m1d <- lm(sp_intro ~ poly(nh_white, 2)*state_nh_white + share_dem +
            median_income + median_age + population +
            some_college, filter(demos, chamber == "SD", impact == "R"))

m2a <- lm(sp_intro ~ poly(nh_white, 2)*state_nh_white, filter(demos, chamber == "HD", impact == "E"))
m2b <- lm(sp_intro ~ poly(nh_white, 2)*state_nh_white + share_dem +
            median_income + median_age + population +
            some_college, filter(demos, chamber == "HD", impact == "E"))
m2c <- lm(sp_intro ~ poly(nh_white, 2)*state_nh_white, filter(demos, chamber == "SD", impact == "E"))
m2d <- lm(sp_intro ~ poly(nh_white, 2)*state_nh_white + share_dem +
            median_income + median_age + population +
            some_college, filter(demos, chamber == "SD", impact == "E"))

models <- list(m1a, m1b, m1c, m1d,
               m2a, m2b, m2c, m2d)

marg <- bind_rows(
  ggeffect(model = m1b, terms = c("nh_white[all]", "state_nh_white[.5, .8]")) %>% 
    mutate(group = scales::percent(as.numeric(as.character(group))),
           chamber = "Lower Chamber",
           type = "Restrictive"),
  ggeffect(model = m1d, terms = c("nh_white[all]", "state_nh_white[.5, .8]")) %>% 
    mutate(group = scales::percent(as.numeric(as.character(group))),
           chamber = "Upper Chamber",
           type = "Restrictive"),
  ggeffect(model = m2b, terms = c("nh_white[all]", "state_nh_white[.5, .8]")) %>% 
    mutate(group = scales::percent(as.numeric(as.character(group))),
           chamber = "Lower Chamber",
           type = "Expansive"),
  ggeffect(model = m2d, terms = c("nh_white[all]", "state_nh_white[.5, .8]")) %>% 
    mutate(group = scales::percent(as.numeric(as.character(group))),
           chamber = "Upper Chamber",
           type = "Expansive")
)

p1 <- ggplot(filter(marg, type == "Restrictive"), aes(x = x, y = predicted,
                 ymin = conf.low, ymax = conf.high,
                 color = group,
                 fill = group,
                 linetype = group)) +
  facet_grid(. ~ chamber) +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  theme_bc(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 1, .25), labels = scales::percent) +
  labs(x = "White Share of District Population",
       y = "P(Sponsored Restrictive Provision(s))",
       fill = "White Share of\nState Population",
       color = "White Share of\nState Population",
       linetype = "White Share of\nState Population") +
  ggtitle("(A) Restrictive Provisions") +
  scale_y_continuous(labels = scales::percent)
p1

p2 <- ggplot(filter(marg, type == "Expansive"), aes(x = x, y = predicted,
                                                      ymin = conf.low, ymax = conf.high,
                                                      color = group,
                                                      fill = group,
                                                      linetype = group)) +
  facet_grid(. ~ chamber) +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  theme_bc(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 1, .25), labels = scales::percent) +
  labs(x = "White Share of District Population",
       y = "P(Sponsored Expansive Provision(s))",
       fill = "White Share of\nState Population",
       color = "White Share of\nState Population",
       linetype = "White Share of\nState Population") +
  ggtitle("(B) Expansive Provisions") +
  scale_y_continuous(labels = scales::percent)
p2

legend_b <- get_legend(
  p1 + theme(legend.position = "bottom")
)

p <- plot_grid(p1, p2,
               label_size = 12, ncol = 1,
               label_fontfamily = "LM Roman 10")
p
j <- plot_grid(p, legend_b, ncol = 1, rel_heights = c(1, .1))
j
f <- ggdraw(add_sub(j, "   Covariates include Biden 2020 voteshare; median income; median age; 
   share with some college; log(population).", x = 0, hjust = 0,
                    fontfamily = "BentonSans", size = 12))
f
saveRDS(f, "temp/mef_chamb_good.rds")

####################################################

j <- sapply(models, function(m){
  k <-  linearHypothesis(m, c("poly(nh_white, 2)1", "poly(nh_white, 2)2"))[2,6]
  star <- ifelse(k < 0.01, "***",
                 ifelse(k < 0.05, "**",
                        ifelse(k < 0.1, "*", "")))
  k <- paste0(ifelse(k < .001, "< 0.001", as.character(round(k, 3))), star)
})

j <- rbind(data.table(V0 = "\\textit{p}-value of Joint Significance Test"),
               cbind(data.table(V0 = "on Nonhispanic White Terms"),
                     setDT(as.list(j))[]), fill = T) %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
attr(j, 'position') <- c(23, 24)

modelsummary(models,
             statistic = "std.error",
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE|F',
             coef_map = c("poly(nh_white, 2)1" = "Nonhispanic White",
                          "poly(nh_white, 2)2" = "Nonhispanic White\\textsuperscript{2}",
                          "state_nh_white" = "State \\% Nonhispanic White",
                          "poly(nh_white, 2)1:state_nh_white" = "Nonhispanic White $\\times$ State \\% Nonhispanic White",
                          "poly(nh_white, 2)2:state_nh_white" = "Nonhispanic White\\textsuperscript{2} $\\times$ State \\% Nonhispanic White",
                          "share_dem" = "Biden 2020 Voteshare",
                          "median_income" = "Median Income (\\$10,000s)",
                          "median_age" = "Median Age",
                          "some_college" = "Share with Some College",
                          "population" = "Log(Population)",
                          "(Intercept)" = "Intercept"),
             escape = F,
             output = "latex",
             title = "\\label{tab:chamb} District-Level Sponsored Provisions, 2021",
             add_rows = j) %>% 
  kable_styling(latex_options = "scale_down") %>% 
  add_header_above(c("", "Lower Chamber" = 2, "Upper Chamber" = 2,
                     "Lower Chamber" = 2, "Upper Chamber" = 2)) %>% 
add_header_above(c("", "Restrictive Provisions" = 4, "Expansive Provisions" = 4)) %>% 
  save_kable("temp/district_reg.tex")

##########################################

marg <- bind_rows(
  ggeffect(model = m1b, terms = c("share_dem[all]")) %>% 
    mutate(group = scales::percent(as.numeric(as.character(group))),
           chamber = "Lower Chamber",
           type = "Restrictive"),
  ggeffect(model = m1d, terms = c("share_dem[all]")) %>% 
    mutate(group = scales::percent(as.numeric(as.character(group))),
           chamber = "Upper Chamber",
           type = "Restrictive"),
  ggeffect(model = m2b, terms = c("share_dem[all]")) %>% 
    mutate(group = scales::percent(as.numeric(as.character(group))),
           chamber = "Lower Chamber",
           type = "Expansive"),
  ggeffect(model = m2d, terms = c("share_dem[all]")) %>% 
    mutate(group = scales::percent(as.numeric(as.character(group))),
           chamber = "Upper Chamber",
           type = "Expansive")
)

p1 <- ggplot(filter(marg, type == "Restrictive"), aes(x = x, y = predicted,
                                                      ymin = conf.low, ymax = conf.high)) +
  facet_grid(. ~ chamber) +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  theme_bc(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 1, .25), labels = scales::percent) +
  labs(x = "Biden Voteshare in 2020",
       y = "P(Sponsored Restrictive Provision(s))",
       fill = "White Share of\nState Population",
       color = "White Share of\nState Population",
       linetype = "White Share of\nState Population") +
  ggtitle("(A) Restrictive Provisions") +
  scale_y_continuous(labels = scales::percent)
p1

p2 <- ggplot(filter(marg, type == "Expansive"), aes(x = x, y = predicted,
                                                    ymin = conf.low, ymax = conf.high)) +
  facet_grid(. ~ chamber) +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  theme_bc(legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 1, .25), labels = scales::percent) +
  labs(x = "Biden Voteshare in 2020",
       y = "P(Sponsored Expansive Provision(s))",
       fill = "White Share of\nState Population",
       color = "White Share of\nState Population",
       linetype = "White Share of\nState Population") +
  ggtitle("(B) Expansive Provisions") +
  scale_y_continuous(labels = scales::percent)
p2

legend_b <- get_legend(
  p1 + theme(legend.position = "bottom")
)

p <- plot_grid(p1, p2,
               label_size = 12, ncol = 1,
               label_fontfamily = "LM Roman 10")
p

f <- ggdraw(add_sub(p, "   Covariates include Biden 2020 voteshare; median income; median age; 
   share with some college; log(population).", x = 0, hjust = 0,
                    fontfamily = "BentonSans", size = 12))
f
