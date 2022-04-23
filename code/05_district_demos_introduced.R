
upper_chambers <- left_join(census_race_ethnicity("state legislative district (upper chamber)",
                                                  state = unique(fips_codes$state)[1:51],
                                                  year = 2020) %>% 
                              mutate(chamber = "SD"),
                            left_join(census_race_ethnicity("state legislative district (upper chamber)",
                                                            state = unique(fips_codes$state)[1:51],
                                                            year = 2009) %>% 
                                        select(GEOID, nh_white_2009 = nh_white) %>% 
                                        mutate(chamber = "SD"),
                                      left_join(census_median_age("state legislative district (upper chamber)",
                                                                  state = unique(fips_codes$state)[1:51],
                                                                  year = 2020) %>% 
                                                  mutate(chamber = "SD"),
                                                left_join(census_income("state legislative district (upper chamber)",
                                                                        state = unique(fips_codes$state)[1:51],
                                                                        year = 2020) %>% 
                                                            mutate(chamber = "SD"),
                                                          census_education("state legislative district (upper chamber)",
                                                                           state = unique(fips_codes$state)[1:51],
                                                                           year = 2020) %>% 
                                                            mutate(chamber = "SD"))))) %>% 
  mutate(state = substring(GEOID, 1, 2)) %>% 
  group_by(state) %>% 
  mutate(w = 1 / n())

lower_chambers <- left_join(census_race_ethnicity("state legislative district (lower chamber)",
                                                  state = unique(fips_codes$state)[1:51],
                                                  year = 2020) %>% 
                              mutate(chamber = "HD"),
                            left_join(census_race_ethnicity("state legislative district (lower chamber)",
                                                            state = unique(fips_codes$state)[1:51],
                                                            year = 2009) %>%  
                                        select(GEOID, nh_white_2009 = nh_white) %>% 
                                        mutate(chamber = "HD"),
                                      left_join(census_median_age("state legislative district (lower chamber)",
                                                                  state = unique(fips_codes$state)[1:51],
                                                                  year = 2020) %>% 
                                                  mutate(chamber = "HD"),
                                                left_join(census_income("state legislative district (lower chamber)",
                                                                        state = unique(fips_codes$state)[1:51],
                                                                        year = 2020) %>% 
                                                            mutate(chamber = "HD"),
                                                          census_education("state legislative district (lower chamber)",
                                                                           state = unique(fips_codes$state)[1:51],
                                                                           year = 2020) %>% 
                                                            mutate(chamber = "HD"))))) %>% 
  mutate(state = substring(GEOID, 1, 2)) %>% 
  group_by(state) %>% 
  mutate(w = 1 / n())

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

state_race <- census_race_ethnicity("state", year = 2020)

demos <- left_join(demos,
                   state_race %>% 
                     select(state = GEOID,
                            state_nh_white = nh_white)) %>% 
  filter(!is.na(nh_white))

demos <- left_join(demos, readRDS("temp/district_biden_share.rds") %>% 
                     select(-state)) %>% 
  mutate(share_rep = 1 - share_dem)

demos <- mutate(demos,
                south = substring(GEOID, 1, 2) %in%
                  c("01", "48", "40", "22", "28", "05",
                    "12", "13", "47", "45", "37",
                    "21", "51", "54"),
                population = log(population),
                median_income = median_income / 10000)

demos$change_white <- demos$nh_white / demos$nh_white_2009

demos <- demos %>% 
  mutate(across(c(nh_white, state_nh_white, some_college, share_rep), ~ . * 100),
         state_code = substring(GEOID, 1, 2))

# demos <- filter(demos, !is.na(share_rep))

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
  pivot_wider(state, names_from = year, values_from = share, names_prefix = "dem_share_") %>% 
  mutate(competitive = dem_share_2020 > 0.45, dem_share_2020 < 0.55)

comp <- left_join(comp,
                  select(fips_codes, state_code, state = state_name) %>% 
                    distinct()) %>% 
  ungroup()

demos <- left_join(demos, select(comp, state_code, competitive))

con <- fread("raw_data/control.csv")

con <- left_join(con,
                  select(fips_codes, state_code, NAME = state_name) %>% 
                    distinct()) %>% 
  ungroup() %>% 
  mutate(r = control == "Rep")

demos <- left_join(demos, select(con, state_code, r))
demos$sp_intro <- demos$sp_intro * 100
demos$sp_pass <- demos$sp_pass * 100
cleanup("demos")

rr <- readRDS("temp/district_rr.rds")

demos <- left_join(select(demos, -n), rr)
demos <- as.data.frame(demos)
demos <- demos[complete.cases(select(demos, sp_intro,
                                     nh_white, state_nh_white, share_rep,
                                     median_income, median_age, population,
                                     some_college, competitive, r)), ]
cleanup("demos")
saveRDS(demos, "temp/demos.rds")
#####################################
#####################################
#####################################

m1a <- feols(sp_intro ~ poly(nh_white, 2)*state_nh_white, filter(demos, chamber == "HD", impact == "R"), vcov = "HC1")
m1aa <- feols(sp_intro ~ poly(share_rep, 2), filter(demos, chamber == "HD", impact == "R"), vcov = "HC1")
m1b <- feols(sp_intro ~ poly(nh_white, 2)*state_nh_white + poly(share_rep, 2) +
            median_income + median_age + population +
            some_college + competitive + r, filter(demos, chamber == "HD", impact == "R"), vcov = "HC1")
m1c <- feols(sp_intro ~ poly(nh_white, 2)*state_nh_white, filter(demos, chamber == "SD", impact == "R"), vcov = "HC1")
m1cc <- feols(sp_intro ~ poly(share_rep, 2), filter(demos, chamber == "SD", impact == "R"), vcov = "HC1")
m1d <- feols(sp_intro ~ poly(nh_white, 2)*state_nh_white + poly(share_rep, 2) +
            median_income + median_age + population +
            some_college + competitive + r, filter(demos, chamber == "SD", impact == "R"), vcov = "HC1")


models <- list(m1a, m1aa, m1b, m1c, m1cc, m1d)

marg <- bind_rows(
  ggeffect(model = m1a, terms = c("nh_white[all]", "state_nh_white[50, 80]")) %>% 
    mutate(group = paste0(group, "%"),
           chamber = "Lower Chamber",
           type = "No Covariates"),
  ggeffect(model = m1b, terms = c("nh_white[all]", "state_nh_white[50, 80]")) %>% 
    mutate(group = paste0(group, "%"),
           chamber = "Lower Chamber",
           type = "Covariates"),
  ggeffect(model = m1c, terms = c("nh_white[all]", "state_nh_white[50, 80]")) %>% 
    mutate(group = paste0(group, "%"),
           chamber = "Upper Chamber",
           type = "No Covariates"),
  ggeffect(model = m1d, terms = c("nh_white[all]", "state_nh_white[50, 80]")) %>% 
    mutate(group = paste0(group, "%"),
           chamber = "Upper Chamber",
           type = "Covariates")
) %>% 
  mutate(across(c(x, predicted, conf.low, conf.high), ~ . / 100))

marg$type <- factor(marg$type, levels = c("No Covariates", "Covariates"))

f <- ggplot(filter(marg), aes(x = x, y = predicted,
                 ymin = conf.low, ymax = conf.high,
                 color = group,
                 fill = group,
                 linetype = group)) +
  facet_grid(type ~ chamber) +
  geom_ribbon(alpha = 0.2, linetype = "solid") +
  geom_line() +
  theme_bc(legend.position = "bottom") +
  scale_color_manual(values = bc_colors(demos$chamber)) +
  scale_fill_manual(values = bc_colors(demos$chamber)) +
  scale_x_continuous(breaks = seq(0, 1, .25), labels = scales::percent,
                     limits = c(.25, 1)) +
  labs(x = "White Share of District Population",
       y = "Probability Representative\nSponsored Restrictive Provision(s)",
       fill = "White Share of\nState Population",
       color = "White Share of\nState Population",
       linetype = "White Share of\nState Population",
       caption = "Covariates include Trump 2020 voteshare; median income; median age; 
share with some college; log(population);
whether the state was competitive in the 2020 election;
whether the state has unified Republican control.") +
  scale_y_continuous(labels = scales::percent)
f
saveRDS(f, "temp/mef_chamb_good.rds")
#####
marg <- bind_rows(
  ggeffect(model = m1aa, terms = c("share_rep[all]")) %>% 
    mutate(group = paste0(group, "%"),
           chamber = "Lower Chamber",
           type = "No Covariates"),
  ggeffect(model = m1b, terms = c("share_rep[all]")) %>% 
    mutate(group = paste0(group, "%"),
           chamber = "Lower Chamber",
           type = "Covariates"),
  ggeffect(model = m1cc, terms = c("share_rep[all]")) %>% 
    mutate(group = paste0(group, "%"),
           chamber = "Upper Chamber",
           type = "No Covariates"),
  ggeffect(model = m1d, terms = c("share_rep[all]")) %>% 
    mutate(group = paste0(group, "%"),
           chamber = "Upper Chamber",
           type = "Covariates")
) %>% 
  mutate(across(c(x, predicted, conf.low, conf.high), ~ . / 100))

marg$type <- factor(marg$type, levels = c("No Covariates", "Covariates"))

f <- ggplot(filter(marg), aes(x = x, y = predicted,
                              ymin = conf.low, ymax = conf.high)) +
  facet_grid(type ~ chamber) +
  geom_ribbon(alpha = 0.2, linetype = "solid") +
  geom_line() +
  theme_bc() +
  scale_x_continuous(breaks = seq(0, 1, .25), labels = scales::percent,
                     limits = c(.0, 1)) +
  labs(x = "Trump 2020 Voteshare",
       y = "Probability Representative\nSponsored Restrictive Provision(s)",
       caption = "Covariates include district and state white share of population (and interaction);
median income; median age; 
share with some college; log(population);
whether the state was competitive in the 2020 election;
whether the state has unified Republican control.") +
  scale_y_continuous(labels = scales::percent)
f
saveRDS(f, "temp/mef_chamb_party.rds")

####################################################

modelsummary(models,
             statistic = "[{conf.low}, {conf.high}]",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE|F',
             coef_map = c("poly(nh_white, 2)1" = "Nonhispanic White",
                          "poly(nh_white, 2)2" = "Nonhispanic White\\textsuperscript{2}",
                          "state_nh_white" = "State \\% Nonhispanic White",
                          "poly(nh_white, 2)1:state_nh_white" = "Nonhispanic White $\\times$ State \\% Nonhispanic White",
                          "poly(nh_white, 2)2:state_nh_white" = "Nonhispanic White\\textsuperscript{2} $\\times$ State \\% Nonhispanic White",
                          "poly(nh_white, 2):state_nh_white1" = "Nonhispanic White $\\times$ State \\% Nonhispanic White",
                          "poly(nh_white, 2):state_nh_white2" = "Nonhispanic White\\textsuperscript{2} $\\times$ State \\% Nonhispanic White",
                          "poly(share_rep, 2)1" = "Trump 2020 Voteshare",
                          "poly(share_rep, 2)2" = "Trump 2020 Voteshare\\textsuperscript{2}",
                          "median_income" = "Median Income (\\$10,000s)",
                          "median_age" = "Median Age",
                          "some_college" = "Share with Some College",
                          "population" = "Log(Population)",
                          "competitiveTRUE" = "State Competitive in 2020",
                          "rTRUE" = "State has Unified Republican Control",
                          "(Intercept)" = "Intercept"),
             escape = F,
             fmt = 1,
             output = "latex",
             title = "\\label{tab:chamb} District-Level Sponsored Provisions, 2021",
             # add_rows = j,
             notes = c("95\\\\% confidence intervals shown below estimates and computed with robust standard errors.","", "and \\\\textit{Share with Some College} can range from 0 to 100.", "The dependent variable, \\\\textit{Nonhispanic White}, \\\\textit{State \\\\% Nonhispanic White},
             \\\\textit{Trump 2020 Voteshare},",
                       "\\\\textit{Trump 2020 Voteshare} and \\\\textit{Trump 2020 Voteshare\\\\textsuperscript{2}} computed using orthogonal polynomials.",
                       "\\\\textit{Nonhispanic White} and \\\\textit{Nonhispanic White\\\\textsuperscript{2}} computed using orthogonal polynomials.")) %>% 
  add_header_above(c("", "Lower Chamber" = 3, "Upper Chamber" = 3)) %>%
  kable_styling(latex_options = "scale_down") %>% 
  save_kable("temp/district_reg.tex")



#####################################
#####################################
#####################################
m1a <- feols(sp_intro ~ resent, filter(demos, chamber == "HD", impact == "R"), vcov = "HC1")
m1b <- feols(sp_intro ~ resent + poly(share_rep, 2) +
               median_income + median_age + population +
               some_college + competitive + r + poly(nh_white, 2), filter(demos, chamber == "HD", impact == "R"), vcov = "HC1")
m1c <- feols(sp_intro ~ resent, filter(demos, chamber == "SD", impact == "R"), vcov = "HC1")
m1d <- feols(sp_intro ~ resent + poly(share_rep, 2) +
               median_income + median_age + population +
               some_college + competitive + r + poly(nh_white, 2), filter(demos, chamber == "SD", impact == "R"), vcov = "HC1")


models <- list(m1a, m1b, m1c, m1d)

marg <- bind_rows(
  ggeffect(model = m1a, terms = c("resent[all]")) %>% 
    mutate(group = paste0(group, "%"),
           chamber = "Lower Chamber",
           type = "No Covariates"),
  ggeffect(model = m1b, terms = c("resent[all]")) %>% 
    mutate(group = paste0(group, "%"),
           chamber = "Lower Chamber",
           type = "Covariates"),
  ggeffect(model = m1c, terms = c("resent[all]")) %>% 
    mutate(group = paste0(group, "%"),
           chamber = "Upper Chamber",
           type = "No Covariates"),
  ggeffect(model = m1d, terms = c("resent[all]")) %>% 
    mutate(group = paste0(group, "%"),
           chamber = "Upper Chamber",
           type = "Covariates")
) %>% 
  mutate(across(c(predicted, conf.low, conf.high), ~ . / 100))

marg$type <- factor(marg$type, levels = c("No Covariates", "Covariates"))

f <- ggplot(filter(marg), aes(x = x, y = predicted,
                              ymin = conf.low, ymax = conf.high)) +
  facet_grid(type ~ chamber) +
  geom_ribbon(alpha = 0.2, linetype = "solid") +
  geom_line() +
  theme_bc(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1, 5, 1),
                     limits = c(1, 5)) +
  labs(x = "District Racial Resentment Score",
       y = "Probability Representative\nSponsored Restrictive Provision(s)",
       caption = "Covariates include Trump 2020 voteshare; median income; median age; 
share with some college; log(population); white share of district;
whether the state was competitive in the 2020 election;
whether the state has unified Republican control.") +
  scale_y_continuous(labels = scales::percent)
f
saveRDS(f, "temp/mef_rr.rds")


modelsummary(models,
             statistic = "[{conf.low}, {conf.high}]",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE|F',
             coef_map = c("resent" = "Racial Resentment Score",
                          "poly(nh_white, 2)1" = "Nonhispanic White",
                          "poly(nh_white, 2)2" = "Nonhispanic White\\textsuperscript{2}",
                          "poly(share_rep, 2)1" = "Trump 2020 Voteshare",
                          "poly(share_rep, 2)2" = "Trump 2020 Voteshare\\textsuperscript{2}",
                          "median_income" = "Median Income (\\$10,000s)",
                          "median_age" = "Median Age",
                          "some_college" = "Share with Some College",
                          "population" = "Log(Population)",
                          "competitiveTRUE" = "State Competitive in 2020",
                          "rTRUE" = "State has Unified Republican Control",
                          "(Intercept)" = "Intercept"),
             escape = F,
             fmt = 1,
             output = "latex",
             title = "\\label{tab:chamb-rr} District-Level Sponsored Provisions, 2021",
             # add_rows = j,
             notes = c("95\\\\% confidence intervals shown below estimates and computed with robust standard errors.","", "and \\\\textit{Share with Some College} can range from 0 to 100.", "The dependent variable, \\\\textit{Nonhispanic White}, \\\\textit{State \\\\% Nonhispanic White},
             \\\\textit{Trump 2020 Voteshare},",
                       "\\\\textit{Trump 2020 Voteshare} and \\\\textit{Trump 2020 Voteshare\\\\textsuperscript{2}} computed using orthogonal polynomials.",
                       "\\\\textit{Nonhispanic White} and \\\\textit{Nonhispanic White\\\\textsuperscript{2}} computed using orthogonal polynomials.")) %>% 
  add_header_above(c("", "Lower Chamber" = 2, "Upper Chamber" = 2)) %>%
  kable_styling(latex_options = "scale_down") %>% 
  save_kable("temp/district_reg_rr.tex")
