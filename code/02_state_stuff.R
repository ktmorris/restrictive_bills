# census <- get_basic_census_stats("state", 2020)
# saveRDS(census, "temp/state_census_2020.rds")
census <- readRDS("temp/state_census_2020.rds")

census_09 <- census_race_ethnicity("state", year = 2010) %>% 
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

census <- left_join(census, comp, by = c("NAME" = "state"))

con <- fread("raw_data/control.csv")
census <- left_join(census, con)

census$r <- census$control == "Rep" | census$NAME == "Nebraska"
census$competitive <- census$dem_share_2020 > 0.45 & census$dem_share_2020 < 0.55

census <- census %>% 
  mutate(change_dem = dem_share_2020 - dem_share_2016,
         population = log(population),
         median_income = median_income / 10000,
         across(c(nh_white, some_college, change_dem), ~ . * 100),
         r = ifelse(r, "Republican", "Other"),
         competitive = ifelse(competitive, "Competitive", "ANC"))

cleanup(c("census", "state_level"))

fwrite(census, "temp/state.csv")

#############################################################
#############################################################

m_intro_white <- rlm(intro ~ poly(nh_white, 2) * r +
                       FinalCOVI + competitive * r +
                       change_dem +
                       median_income + median_age + population +
                       some_college, census, method = "M", maxit = 100)
m_intro2 <- rlm(intro ~ poly(nh_white, 2) * r, census, method = "M", maxit = 100)

m_intro3 <- rlm(intro ~ competitive * r, census, method = "M", maxit = 100)

m_pass_white <- rlm(passed ~ poly(nh_white, 2) * r +
                      FinalCOVI + competitive * r +
                      change_dem +
                      median_income + median_age + population +
                      some_college, census, maxit = 100, method = "M")

m_pass2 <- rlm(passed ~ poly(nh_white, 2) * r, census, method = "M", maxit = 100)

m_pass3 <- rlm(passed ~ competitive * r, census, method = "M", maxit = 100)

p <- paste(seq(40, 93, 1), collapse = ", ")

marg <- ggpredict(m_intro_white, terms = c(paste0("nh_white[", p, "]"), "r")) %>% 
  mutate(x = x / 100,
         group = ifelse(group == "Republican", "Yes", "No"))

marg2 <- ggpredict(m_intro2, terms = c(paste0("nh_white[", p, "]"), "r")) %>% 
  mutate(x = x / 100,
         group = ifelse(group == "Republican", "Yes", "No"))

marg3 <- ggpredict(m_pass_white, terms = c(paste0("nh_white[", p, "]"), "r")) %>% 
  mutate(x = x / 100,
         group = ifelse(group == "Republican", "Yes", "No"))

marg4 <- ggpredict(m_pass2, terms = c(paste0("nh_white[", p, "]"), "r")) %>% 
  mutate(x = x / 100,
         group = ifelse(group == "Republican", "Yes", "No"))

rs <- ggplot() +
  # geom_point(aes(x = nh_white/100, y = intro, fill = r, shape = r), data = census %>%
  #              mutate(r = ifelse(r == 1, "Yes", "No")), size = 2, color = "black") +
  geom_line(data = marg2, aes(x = x, y = predicted,
                             color = group,
                             linetype = group)) +
  geom_ribbon(data = marg2, aes(x = x, y = predicted,
                               color = group, fill = group,
                               ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  theme_bc(legend.position = "none") +
  scale_color_manual(values = bc_colors(census$r)) +
  scale_fill_manual(values = bc_colors(census$r)) +
  scale_x_continuous(labels = scales::percent, limits = c(.4, .95)) +
  scale_y_continuous(labels = scales::comma) +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "White Share of Population",
       y = "Count",
       color = "Unified Republican Control",
       fill = "Unified Republican Control",
       linetype = "Unified Republican Control",
       shape = "Unified Republican Control") +
  ggtitle("(A) Introduced (No Covariates)")

race_intro <- ggplot() +
  # geom_point(aes(x = nh_white/100, y = intro, fill = r, shape = r), data = census %>%
  #              mutate(r = ifelse(r == 1, "Yes", "No")), size = 2, color = "black") +
  geom_line(data = marg, aes(x = x, y = predicted,
                             color = group,
                             linetype = group)) +
  geom_ribbon(data = marg, aes(x = x, y = predicted,
                               color = group, fill = group,
                               ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  theme_bc(legend.position = "none") +
  scale_color_manual(values = bc_colors(census$r)) +
  scale_fill_manual(values = bc_colors(census$r)) +
  scale_x_continuous(labels = scales::percent, limits = c(.4, .95)) +
  scale_y_continuous(labels = scales::comma) +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "White Share of Population",
       y = "Count",
       color = "Unified Republican Control",
       fill = "Unified Republican Control",
       linetype = "Unified Republican Control",
       shape = "Unified Republican Control") +
  ggtitle("(C) Introduced (With Covariates)")
# race_intro

rp <- ggplot() +
  # geom_point(aes(x = nh_white/100, y = passed, fill = r, shape = r), data = census,
  #            size = 2, color = "black") +
  geom_line(data = marg4, aes(x = x, y = predicted,
                             color = group,
                             linetype = group)) +
  geom_ribbon(data = marg4, aes(x = x, y = predicted,
                               color = group, fill = group,
                               ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  theme_bc(legend.position = "none") +
  scale_color_manual(values = bc_colors(census$r)) +
  scale_fill_manual(values = bc_colors(census$r)) +
  scale_x_continuous(labels = scales::percent, limits = c(.4, .95)) +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "White Share of Population",
       y = "Count",
       color = "Unified Republican Control",
       fill = "Unified Republican Control",
       linetype = "Unified Republican Control",
       shape = "Unified Republican Control") +
  ggtitle("(B) Passed (No Covariates)")

race_pass <- ggplot() +
  # geom_point(aes(x = nh_white/100, y = passed, fill = r, shape = r), data = census %>%
  #              mutate(r = ifelse(r == 1, "Yes", "No")), size = 2, color = "black") +
  geom_line(data = marg3, aes(x = x, y = predicted,
                             color = group,
                             linetype = group)) +
  geom_ribbon(data = marg3, aes(x = x, y = predicted,
                               color = group, fill = group,
                               ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  theme_bc(legend.position = "none") +
  scale_color_manual(values = bc_colors(census$r)) +
  scale_fill_manual(values = bc_colors(census$r)) +
  scale_x_continuous(labels = scales::percent, limits = c(.4, .95)) +
  scale_shape_manual(values = c(21, 24)) +
  labs(x = "White Share of Population",
       y = "Count",
       color = "Unified Republican Control",
       fill = "Unified Republican Control",
       linetype = "Unified Republican Control",
       shape = "Unified Republican Control") +
  ggtitle("(D) Passed(With Covariates)")
# race_pass

#############################################################
#############################################################
#############################################################

legend_b <- get_legend(
  race_intro + theme(legend.position = "bottom")
)

p <- plot_grid(rs, rp, race_intro,
               race_pass,
               label_size = 12,
               label_fontfamily = "LM Roman 10")
j <- plot_grid(p, legend_b, ncol = 1, rel_heights = c(1, .1))
# j

f <- ggdraw(add_sub(j, "   Covariates include 2020 COVI; whether the state was competitive in the 2020 election;
         Change in Democratic presidential voteshare, 2016--2020;
         median income; median age; share with some college education; log(population).", x = 0, hjust = 0,
                    fontfamily = "BentonSans", size = 12))
f
saveRDS(f, "temp/mef_race_both.rds")


#####################################################
#####################################################
#####################################################
models <- list(m_intro2,
               m_intro3,
               m_intro_white,
               m_pass2,
               m_pass3,
               m_pass_white)


# j <- rbindlist(lapply(c(1:length(models)), function(i){
#   
#   m <- models[[i]]
#   
#   if((i+1) %% 3 != 0){
#     l <- linearHypothesis(m, c("poly(nh_white, 2)1:FinalCOVI", "poly(nh_white, 2)2:FinalCOVI"))[2,6]
#     star <- ifelse(l < 0.01, "***",
#                    ifelse(l < 0.05, "**",
#                           ifelse(l < 0.1, "*", "")))
#     l <- paste0(ifelse(l < .001, "< 0.001", as.character(round(l, 3))), star)
#     
#     k <-  linearHypothesis(m, c("poly(nh_white, 2)1", "poly(nh_white, 2)2"))[2,6]
#     star <- ifelse(k < 0.01, "***",
#                    ifelse(k < 0.05, "**",
#                           ifelse(k < 0.1, "*", "")))
#     k <- paste0(ifelse(k < .001, "< 0.001", as.character(round(k, 3))), star)
#   }else{
#     l <- ""
#     k <- ""
#   }
#   return(data.table(alone = k, inter = l, m = i))
# })) %>% 
#   pivot_longer(cols = c(alone, inter)) %>% 
#   pivot_wider(id_cols = name, values_from = value, names_from = m) %>% 
#   select(-name)
# 
# j <- rbind(data.table(V0 = "\\textit{p}-value of Joint Significance Test"),
#            cbind(data.table(V0 = "\\hspace{3mm}on Nonhispanic White Terms"),
#                  j[1,]),
#            data.table(V0 = "\\textit{p}-value of Joint Significance Test"),
#            cbind(data.table(V0 = "\\hspace{3mm}on Nonhispanic White $\\times$ COVI Terms"),
#                  j[2,]),
#            fill = T) %>% 
#   mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
# 
# attr(j, 'position') <- c(29:32)

modelsummary(models,
             statistic = "[{conf.low}, {conf.high}]",
             # stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             gof_omit = "converged|BIC",
             coef_map = c("poly(nh_white, 2)1" = "Nonhispanic White",
                          "poly(nh_white, 2)2" = "Nonhispanic White\\textsuperscript{2}",
                          "rRepublican" = "Unified Republican Control",
                          "poly(nh_white, 2)1:rRepublican" = "Nonhispanic White $\\times$ Unified Republican Control",
                          "poly(nh_white, 2)2:rRepublican" = "Nonhispanic White\\textsuperscript{2} $\\times$ Unified Republican Control",
                          "competitiveCompetitive" = "Competitive in 2020",
                          "rRepublican:competitiveCompetitive" = "Unified Republican Control $\\times$ Competitive",
                          "competitiveCompetitive:rRepublican" = "Unified Republican Control $\\times$ Competitive",
                          "FinalCOVI" = "2020 COVI",
                          "change_dem" = "Change in Dem. Vote Share 2016--2020",
                          "median_income" = "Median Income (\\$10,000s)",
                          "median_age" = "Median Age",
                          "some_college" = "Share with Some College",
                          "population" = "Log(Population)",
                          "(Intercept)" = "Intercept"),
             fmt = 1,
             escape = F,
             output = "latex",
             title = "\\label{tab:state} State-Level Restrictive Provisions, 2021",
             # add_rows = j,
             notes = c("95\\\\% confidence intervals shown below estimates.",
                       "",
                       "\\\\textit{Nonhispanic White} and \\\\textit{Share with Some College} can range from 0 to 100.",
                       "\\\\textit{Change in Dem. Vote Share 2016–2020} can range from -100 to 100.",
                       "\\\\textit{Nonhispanic White} and \\\\textit{Nonhispanic White\\\\textsuperscript{2}} computed using orthogonal polynomials.")) %>% 
  add_header_above(c(" " = 1, "Introduced" = 3, "Passed" = 3)) %>% 
  kable_styling(font_size = 8) %>% 
  save_kable("temp/state_reg.tex")

modelsummary(models[c(1,3,4,6)],
             statistic = "[{conf.low}, {conf.high}]",
             # stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             gof_omit = "converged|BIC",
             coef_map = c("poly(nh_white, 2)1" = "Nonhispanic White",
                          "poly(nh_white, 2)2" = "Nonhispanic White\\textsuperscript{2}",
                          "rRepublican" = "Unified Republican Control",
                          "poly(nh_white, 2)1:rRepublican" = "Nonhispanic White $\\times$ Unified Republican Control",
                          "poly(nh_white, 2)2:rRepublican" = "Nonhispanic White\\textsuperscript{2} $\\times$ Unified Republican Control",
                          "competitiveCompetitive" = "Competitive in 2020",
                          "rRepublican:competitiveCompetitive" = "Unified Republican Control $\\times$ Competitive",
                          "competitiveCompetitive:rRepublican" = "Unified Republican Control $\\times$ Competitive",
                          "FinalCOVI" = "2020 COVI",
                          "change_dem" = "Change in Dem. Vote Share 2016--2020",
                          "median_income" = "Median Income (\\$10,000s)",
                          "median_age" = "Median Age",
                          "some_college" = "Share with Some College",
                          "population" = "Log(Population)",
                          "(Intercept)" = "Intercept"),
             fmt = 1,
             escape = F,
             output = "latex",
             title = "\\label{tab:state} State-Level Restrictive Provisions, 2021",
             # add_rows = j,
             notes = c("95\\\\% confidence intervals shown below estimates.",
                       "",
                       "\\\\textit{Nonhispanic White} and \\\\textit{Share with Some College} can range from 0 to 100.",
                       "\\\\textit{Change in Dem. Vote Share 2016–2020} can range from -100 to 100.",
                       "\\\\textit{Nonhispanic White} and \\\\textit{Nonhispanic White\\\\textsuperscript{2}} computed using orthogonal polynomials.")) %>% 
  add_header_above(c(" " = 1, "Introduced" = 2, "Passed" = 2)) %>% 
  kable_styling(font_size = 8) %>% 
  save_kable("temp/state_reg_slim.tex")

cleanup("state_level", "census")

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

