demos <- readRDS("temp/demos.rds")

demos$sp_intro <- demos$sp_intro / 100

m1a <- feglm(sp_intro ~ poly(nh_white, 2)*state_nh_white, filter(demos, chamber == "HD", impact == "R"), family = "binomial")
m1aa <- feglm(sp_intro ~ poly(share_rep, 2), filter(demos, chamber == "HD", impact == "R"), family = "binomial")
m1b <- feglm(sp_intro ~ poly(nh_white, 2)*state_nh_white + poly(share_rep, 2) +
               median_income + median_age + population +
               some_college + competitive + r, filter(demos, chamber == "HD", impact == "R"), family = "binomial")
m1c <- feglm(sp_intro ~ poly(nh_white, 2)*state_nh_white, filter(demos, chamber == "SD", impact == "R"), family = "binomial")
m1cc <- feglm(sp_intro ~ poly(share_rep, 2), filter(demos, chamber == "SD", impact == "R"), family = "binomial")
m1d <- feglm(sp_intro ~ poly(nh_white, 2)*state_nh_white + poly(share_rep, 2) +
               median_income + median_age + population +
               some_college + competitive + r, filter(demos, chamber == "SD", impact == "R"), family = "binomial")


models <- list(m1a, m1aa, m1b, m1c, m1cc, m1d)

modelsummary(models,
             exponentiate = T,
             statistic = "[{conf.low}, {conf.high}]",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             gof_omit = 'DF|Deviance|Within|Pseudo|Log|Std|FE|F|R2*',
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
             title = "\\label{tab:dist-logit} District-Level Sponsored Provisions, 2021",
             # add_rows = j,
             notes = c("95\\\\% confidence intervals shown below estimates. Coefficients are exponentiated.","", "and \\\\textit{Share with Some College} can range from 0 to 100.", "The dependent variable, \\\\textit{Nonhispanic White}, \\\\textit{State \\\\% Nonhispanic White},
             \\\\textit{Trump 2020 Voteshare},",
                       "\\\\textit{Trump 2020 Voteshare} and \\\\textit{Trump 2020 Voteshare\\\\textsuperscript{2}} computed using orthogonal polynomials.",
                       "\\\\textit{Nonhispanic White} and \\\\textit{Nonhispanic White\\\\textsuperscript{2}} computed using orthogonal polynomials.")) %>% 
  add_header_above(c("", "Lower Chamber" = 3, "Upper Chamber" = 3)) %>%
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>% 
  save_kable("temp/district_reg_logit.tex")


###################################

m1a <- feglm(sp_intro ~ resent, filter(demos, chamber == "HD", impact == "R"), family = "binomial")
m1b <- feglm(sp_intro ~ resent + poly(share_rep, 2) +
               median_income + median_age + population +
               some_college + competitive + r + poly(nh_white, 2), filter(demos, chamber == "HD", impact == "R"), family = "binomial")
m1c <- feglm(sp_intro ~ resent, filter(demos, chamber == "SD", impact == "R"), family = "binomial")
m1d <- feglm(sp_intro ~ resent + poly(share_rep, 2) +
               median_income + median_age + population +
               some_college + competitive + r + poly(nh_white, 2), filter(demos, chamber == "SD", impact == "R"), family = "binomial")


models <- list(m1a, m1b, m1c, m1d)

modelsummary(models,
             exponentiate = T,
             statistic = "[{conf.low}, {conf.high}]",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             gof_omit = 'DF|Deviance|Within|Pseudo|Log|Std|FE|F|R2*',
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
             title = "\\label{tab:chamb-rr-logit} District-Level Sponsored Provisions, 2021",
             # add_rows = j,
             notes = c("95\\\\% confidence intervals shown below estimates. Coefficients are exponentiated.","", "and \\\\textit{Share with Some College} can range from 0 to 100.", "The dependent variable, \\\\textit{Nonhispanic White}, \\\\textit{State \\\\% Nonhispanic White},
             \\\\textit{Trump 2020 Voteshare},",
                       "\\\\textit{Trump 2020 Voteshare} and \\\\textit{Trump 2020 Voteshare\\\\textsuperscript{2}} computed using orthogonal polynomials.",
                       "\\\\textit{Nonhispanic White} and \\\\textit{Nonhispanic White\\\\textsuperscript{2}} computed using orthogonal polynomials.")) %>% 
  add_header_above(c("", "Lower Chamber" = 2, "Upper Chamber" = 2)) %>%
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>% 
  save_kable("temp/district_reg_rr_logit.tex")