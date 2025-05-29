library(tidyverse)
library(stargazer)
library(dotwhisker)
library(MetBrewer)

#### create public data ####
key_map <- read_csv("coreidd/key_ut_to_articleid.csv")

df <- read_csv("scripts-steps/impact/input_jif_regression_nosf_20241212.csv") %>% 
  left_join(key_map) %>% select(-UT)
df_gn <- read_csv("scripts-steps/impact/input_jif_regression_gn_nosf_20241212.csv") %>%
  left_join(key_map) %>% select(-UT)
df_sf <- read_csv("scripts-steps/impact/input_jif_regression_yessf_20241212.csv") %>% 
  left_join(key_map) %>% select(-UT)
df_noauth <- read_csv("scripts-steps/impact/input_jif_regression_nosf_noauth_20241212.csv") %>% 
  left_join(key_map) %>% select(-UT)

write_csv(df, "coreidd/input_jif_regression_nosf_20241212_public.csv")
write_csv(df_gn, "coreidd/input_jif_regression_gn_nosf_20241212_public.csv")
write_csv(df_sf, "coreidd/input_jif_regression_yessf_20241212_public.csv")
write_csv(df_noauth, "coreidd/input_jif_regression_nosf_noauth_20241212_public.csv")

#### read in data for analysis
df <- read.csv("coreidd/input_jif_regression_nosf_20241212_public.csv",header=TRUE) %>%
  mutate(log10_citation_rate = log10(citation_rate),
         binJIF = as.integer(JIF>10),
         sqrtJIF = sqrt(JIF),
         pair_race_bin = ifelse(pair_race == "WW", "WW", "PorP")) %>% 
  mutate(pair_gender = as.factor(pair_gender),
         pair_race = as.factor(pair_race),
         pair_race_bin = as.factor(pair_race_bin))

df$pair_race <- factor(df$pair_race, levels = c("WW", "WN", "NW", "NN"))
df$pair_race_bin <- factor(df$pair_race_bin, levels = c("WW", "PorP"))
df$global_north <- factor(df$global_north, levels = c("0", "1"))


### Explore and check data

# Examine distribution of citation rates and journal impact factors
hist(df$log10_citation_rate)
hist(df$JIF)
hist(df$sqrtJIF)


# Check whether first and last author gender and first and last author race are independent. 
# This affects whether we use pair_gender and pair_race variables or independent first and last author variables.

table(df$FA_gender,df$LA_gender) # cols are LA, rows are FA
# chisquared test is to determine whether two categorical variables are independent
Xsq <- chisq.test(table(df$FA_gender,df$LA_gender))
Xsq
Xsq$expected
# column sum * row proportion = expected number in that cell (times bc independent)

table(df$FA_race,df$LA_race) # cols are LA, rows are FA
# chisquared test is to determine whether two categorical variables are independent
Xsq <- chisq.test(table(df$FA_race,df$LA_race))
Xsq
Xsq$expected

# Basically we observe more gender and race assortativity/homophily than expected.

# Figure S18
anova_lm_gender <- lm(sqrtJIF ~ pair_gender, data = df)
summary(anova_lm_gender)
#stargazer(anova_lm_gender, style = "all", covariate.labels = c("MW", "WM", "WW"))
fit_anova_lm_gender <- broom::tidy(anova_lm_gender) %>% # make data frame with regression results
  mutate(model = ifelse(grepl(":", term), "interaction", "solo")) %>% 
  rename(full_term = term) %>% 
  mutate(term = ifelse(grepl(":", full_term), substr(full_term, 9, nchar(full_term)),
                       full_term))

dwplot(fit_anova_lm_gender,
       vline = geom_vline(xintercept = 0, color = "grey60", linetype = 2, linewidth = 1),
       model_order = c("solo", "interaction"),
       dot_args = list(size = 3),
       whisker_args = list(size = 1),
       dodge_size = 0.7) %>% 
  relabel_predictors(c(pair_genderMW = "Man-Woman", pair_genderWM = "Woman-Man",
                       pair_genderWW = "Woman-Woman")
  ) +
  theme_bw() +
  scale_color_met_d(name = "Kandinsky", direction = -1) + 
  labs(x = "Coefficient estimate") +
  theme(axis.text = element_text(size = 14, color = "black"),
        legend.position = "none",
        axis.title = element_text(size = 16, color = "black"))
#ggsave("figures/for-pub/supp/coreidd-anova-lm-gender.pdf", height = 3, width = 5)


anova_gender <- aov(sqrtJIF ~ pair_gender, data = df)
summary(anova_gender)

# Figure S19
anova_lm_race <- lm(sqrtJIF ~ pair_race, data = df)
summary(anova_lm_race)
# stargazer(anova_lm_race, style = "all", covariate.labels = c("EC", "CE", "CC"))
fit_anova_lm_race <- broom::tidy(anova_lm_race) %>% # make data frame with regression results
  mutate(model = ifelse(grepl(":", term), "interaction", "solo")) %>% 
  rename(full_term = term) %>% 
  mutate(term = ifelse(grepl(":", full_term), substr(full_term, 9, nchar(full_term)),
                       full_term))

dwplot(fit_anova_lm_race,
       vline = geom_vline(xintercept = 0, color = "grey60", linetype = 2, linewidth = 1),
       model_order = c("solo", "interaction"),
       dot_args = list(size = 3),
       whisker_args = list(size = 1),
       dodge_size = 0.7) %>% 
  relabel_predictors(c(pair_raceWN = "White-PoC", pair_raceNW = "PoC-White",
                       pair_raceNN = "PoC-PoC")
  ) +
  theme_bw() +
  scale_color_met_d(name = "Kandinsky", direction = -1) + 
  labs(x = "Coefficient estimate") +
  theme(axis.text = element_text(size = 14, color = "black"),
        legend.position = "none",
        axis.title = element_text(size = 16, color = "black"))
# ggsave("figures/for-pub/supp/coreidd-anova-lm-race.pdf", height = 3, width = 5)


anova_race <- aov(sqrtJIF ~ pair_race, data = df)
summary(anova_race)

anova_lm_race_bin <- lm(sqrtJIF ~ pair_race_bin, data = df)
summary(anova_lm_race_bin)
anova_race_bin <- aov(sqrtJIF ~ pair_race_bin, data = df)
summary(anova_race_bin)

anova_lm_tog <- lm(sqrtJIF ~ pair_gender + pair_race, data = df)
summary(anova_lm_tog)

anova_tog <- aov(sqrtJIF ~ pair_gender + pair_race_bin, data = df)
summary(anova_tog)

# do I need to check any ANOVA assumptions?
# 1. normal distribution of data within group
df %>% ggplot(aes(x = sqrtJIF)) + 
  geom_histogram() +
  facet_wrap(~pair_gender) +
  theme_bw()
df %>% ggplot(aes(x = sqrtJIF)) + 
  geom_histogram() +
  facet_wrap(~pair_gender) +
  theme_bw()
# not met, but not too sensitive
# can use kruskal wallis instead

# 2. standard deviations are equal within groups
df %>% group_by(pair_gender) %>% 
  summarise(sd = sd(sqrtJIF))
df %>% group_by(pair_race) %>% 
  summarise(sd = sd(sqrtJIF))
# looks good

#### Run regression models ####

# Figure S20
fit_gender_nojif <- lm(log10_citation_rate ~ pair_gender, data = df)
summary(fit_gender_nojif)
# stargazer(fit_gender_nojif, style = "all", covariate.labels = c("MW", "WM", "WW"))
fit_gender_nojif_df <- broom::tidy(fit_gender_nojif) %>% # make data frame with regression results
  mutate(model = ifelse(grepl(":", term), "interaction", "solo")) %>% 
  rename(full_term = term) %>% 
  mutate(term = ifelse(grepl(":", full_term), substr(full_term, 9, nchar(full_term)),
                       full_term))

dwplot(fit_gender_nojif_df,
       vline = geom_vline(xintercept = 0, color = "grey60", linetype = 2, linewidth = 1),
       model_order = c("solo", "interaction"),
       dot_args = list(size = 3),
       whisker_args = list(size = 1),
       dodge_size = 0.7) %>% 
       relabel_predictors(c(sqrtJIF = "Journal Impact Factor",
                            pair_genderMW = "Man-Woman", pair_genderWM = "Woman-Man",
                            pair_genderWW = "Woman-Woman",
                            pair_raceWN = "White-PoC", pair_raceNW = "PoC-White",
                            pair_raceNN = "PoC-PoC",
                            global_north1 = "Global North Senior Author")
       ) +
  theme_bw() +
  scale_color_met_d(name = "Kandinsky", direction = -1) + 
  labs(x = "Coefficient estimate") +
  theme(axis.text = element_text(size = 14, color = "black"),
        legend.position = "none",
        axis.title = element_text(size = 16, color = "black"))

# ggsave("figures/for-pub/supp/coreidd-gender-nojif.pdf", height = 3, width = 5)

# Figure S21
fit_race_nojif <- lm(log10_citation_rate ~ pair_race, data = df)
summary(fit_race_nojif)
#stargazer(fit_race_nojif, style = "all", covariate.labels = c("EC", "CE", "CC"))
fit_race_nojif_df <- broom::tidy(fit_race_nojif) %>% # make data frame with regression results
  mutate(model = ifelse(grepl(":", term), "interaction", "solo")) %>% 
  rename(full_term = term) %>% 
  mutate(term = ifelse(grepl(":", full_term), substr(full_term, 9, nchar(full_term)),
                       full_term))

dwplot(fit_race_nojif_df,
       vline = geom_vline(xintercept = 0, color = "grey60", linetype = 2, linewidth = 1),
       model_order = c("solo", "interaction"),
       dot_args = list(size = 3),
       whisker_args = list(size = 1),
       dodge_size = 0.7) %>% 
       relabel_predictors(c(sqrtJIF = "Journal Impact Factor",
                            pair_genderMW = "Man-Woman", pair_genderWM = "Woman-Man",
                            pair_genderWW = "Woman-Woman",
                            pair_raceWN = "White-PoC", pair_raceNW = "PoC-White",
                            pair_raceNN = "PoC-PoC",
                            global_north1 = "Global North Senior Author")
       ) +
  theme_bw() +
  scale_color_met_d(name = "Kandinsky", direction = -1) + 
  labs(x = "Coefficient estimate") +
  theme(axis.text = element_text(size = 14, color = "black"),
        legend.position = "none",
        axis.title = element_text(size = 16, color = "black"))

#ggsave("figures/for-pub/supp/coreidd-race-nojif.pdf", height = 3, width = 5)

# Figure S22
fit_gender <- lm(log10_citation_rate ~ sqrtJIF*pair_gender, data = df)
summary(fit_gender)
# stargazer(fit_gender, style = "all", covariate.labels = c("MW", "WM", "WW", "JIF",
#                                                           "MW:JIF", "WM:JIF", "WW:JIF"))
fit_gender_df <- broom::tidy(fit_gender) %>% # make data frame with regression results
  mutate(model = ifelse(grepl(":", term), "interaction", "solo")) %>% 
  rename(full_term = term) %>% 
  mutate(term = ifelse(grepl(":", full_term), substr(full_term, 9, nchar(full_term)),
                       full_term))

dwplot(fit_gender_df,
       vline = geom_vline(xintercept = 0, color = "grey60", linetype = 2, linewidth = 1),
       model_order = c("solo", "interaction"),
       dot_args = list(size = 3),
       whisker_args = list(size = 1),
       dodge_size = 0.7) %>% 
       relabel_predictors(c(sqrtJIF = "Journal Impact Factor",
                            pair_genderMW = "Man-Woman", pair_genderWM = "Woman-Man",
                            pair_genderWW = "Woman-Woman",
                            pair_raceWN = "White-PoC", pair_raceNW = "PoC-White",
                            pair_raceNN = "PoC-PoC",
                            global_north1 = "Global North Senior Author")
       ) +
  theme_bw() +
  scale_color_met_d(name = "Kandinsky", direction = -1) + 
  labs(x = "Coefficient estimate") +
  theme(axis.text = element_text(size = 14, color = "black"),
        legend.position = "none",
        axis.title = element_text(size = 16, color = "black"))

# ggsave("figures/for-pub/supp/coreidd-gender.pdf", height = 4, width = 7)

# Figure S23
fit_race <- lm(log10_citation_rate ~ sqrtJIF*pair_race, data = df)
summary(fit_race)
# stargazer(fit_race, style = "all", covariate.labels = c("EC", "CE", "CC", "JIF",
#                                                           "EC:JIF", "CE:JIF", "CC:JIF"))

fit_race_df <- broom::tidy(fit_race) %>% # make data frame with regression results
  mutate(model = ifelse(grepl(":", term), "interaction", "solo")) %>% 
  rename(full_term = term) %>% 
  mutate(term = ifelse(grepl(":", full_term), substr(full_term, 9, nchar(full_term)),
                       full_term))

dwplot(fit_race_df,
       vline = geom_vline(xintercept = 0, color = "grey60", linetype = 2, linewidth = 1),
       model_order = c("solo", "interaction"),
       dot_args = list(size = 3),
       whisker_args = list(size = 1),
       dodge_size = 0.7) %>% 
       relabel_predictors(c(sqrtJIF = "Journal Impact Factor",
                            pair_genderMW = "Man-Woman", pair_genderWM = "Woman-Man",
                            pair_genderWW = "Woman-Woman",
                            pair_raceWN = "White-PoC", pair_raceNW = "PoC-White",
                            pair_raceNN = "PoC-PoC",
                            global_north1 = "Global North Senior Author")
       ) +
  theme_bw() +
  scale_color_met_d(name = "Kandinsky", direction = -1) + 
  labs(x = "Coefficient estimate") +
  theme(axis.text = element_text(size = 14, color = "black"),
        legend.position = "none",
        axis.title = element_text(size = 16, color = "black"))

# ggsave("figures/for-pub/supp/coreidd-race.pdf", height = 4, width = 7)


# Figure S24
fit_nogn <- lm(log10_citation_rate ~ sqrtJIF*pair_gender + sqrtJIF*pair_race, data = df)
summary(fit_nogn)
# stargazer(fit_nogn, style="all", covariate.labels = c("MW", "WM", "WW", "JIF",
#                                                       "EC", "CE", "CC",
#                                                  "MW:JIF", "WM:JIF", "WW:JIF", "EC:JIF",
#                                                  "CE:JIF", "CC:JIF"))

fit_nogn_df <- broom::tidy(fit_nogn) %>% # make data frame with regression results
  mutate(model = ifelse(grepl(":", term), "interaction", "solo")) %>% 
  rename(full_term = term) %>% 
  mutate(term = ifelse(grepl(":", full_term), substr(full_term, 9, nchar(full_term)),
                       full_term))

dwplot(fit_nogn_df,
       vline = geom_vline(xintercept = 0, color = "grey60", linetype = 2, linewidth = 1),
       model_order = c("solo", "interaction"),
       dot_args = list(size = 3),
       whisker_args = list(size = 1),
       dodge_size = 0.7) %>% 
       relabel_predictors(c(sqrtJIF = "Journal Impact Factor",
                            pair_genderMW = "Man-Woman", pair_genderWM = "Woman-Man",
                            pair_genderWW = "Woman-Woman",
                            pair_raceWN = "White-PoC", pair_raceNW = "PoC-White",
                            pair_raceNN = "PoC-PoC",
                            global_north1 = "Global North Senior Author")
       ) +
  theme_bw() +
  scale_color_met_d(name = "Kandinsky", direction = -1) + 
  labs(x = "Coefficient estimate") +
  theme(axis.text = element_text(size = 14, color = "black"),
        legend.position = "none",
        axis.title = element_text(size = 16, color = "black"))

# ggsave("figures/for-pub/supp/coreidd-nognind.pdf", height = 5, width = 7)

# main text, Figure 4
fit <- lm(log10_citation_rate ~ sqrtJIF*pair_gender + sqrtJIF*pair_race + global_north, data = df)
summary(fit)
fit_df <- broom::tidy(fit) %>% # make data frame with regression results
  mutate(model = ifelse(grepl(":", term), "interaction", "solo")) %>% 
  rename(full_term = term) %>% 
  mutate(term = ifelse(grepl(":", full_term), substr(full_term, 9, nchar(full_term)),
                       full_term))

dwplot(fit_df,
       vline = geom_vline(xintercept = 0, color = "grey60", linetype = 2, linewidth = 1),
       model_order = c("solo", "interaction"),
       dot_args = list(size = 3),
       whisker_args = list(size = 1),
       dodge_size = 0.7) %>% 
       relabel_predictors(c(sqrtJIF = "Journal Impact Factor",
                            pair_genderMW = "Man-Woman", pair_genderWM = "Woman-Man",
                            pair_genderWW = "Woman-Woman",
                            pair_raceWN = "White-PoC", pair_raceNW = "PoC-White",
                            pair_raceNN = "PoC-PoC",
                            global_north1 = "Global North Senior Author")
       ) +
  theme_bw() +
  scale_color_met_d(name = "Kandinsky", direction = -1) + 
  labs(x = "Coefficient estimate") +
  #scale_y_discrete(labels = c(JIF = bquote(sqrt("JIF")))) + # switch to sqrt symbol properly
  #guides(color = guide_legend(title = expression("Interaction with " * sqrt(JIF)))) +
  #labs(color = paste("Interaction\nwith", bquote(sqrt("JIF")))) +
  theme(axis.text = element_text(size = 14, color = "black"),
        legend.position = "none",
        axis.title = element_text(size = 16, color = "black"))
        #legend.text = element_text(size = 10),
        #legend.title = element_text(size = 12),
        #legend.position = c(0.2, 0.9),
        #legend.background = element_rect(fill = "transparent"))

# ggsave("figures/for-pub/coreidd-JIF.pdf", height = 5, width = 7)
# stargazer(fit, style="all", covariate.labels = c("MW", "WM", "WW", "JIF", "EC", "CE", "CC",
#                                                  "Global North Senior Author",
#                                                  "MW:JIF", "WM:JIF", "WW:JIF", "EC:JIF",
#                                                  "CE:JIF", "CC:JIF"))


#### geography sensitivity ####

df_gn <- read_csv("coreidd/input_jif_regression_gn_nosf_20241212_public.csv") %>%
  dplyr::select(article_id, year, num_citations, citation_rate, pair_gender, pair_race, FA_gender, LA_gender, FA_race, LA_race, JIF, WC_1st, num_cited_refs, Country) %>% 
  rename(WC = WC_1st, country = Country) %>% 
  mutate(log10_citation_rate = log10(citation_rate),
         sqrtJIF = sqrt(JIF),
         pair_race_bin = ifelse(pair_race == "WW", "WW", "PorP")) %>% 
  mutate(pair_gender = as.factor(pair_gender),
         pair_race = as.factor(pair_race),
         pair_race_bin = as.factor(pair_race_bin))

df_gn$pair_race <- factor(df_gn$pair_race, levels = c("WW", "WN", "NW", "NN"))
df_gn$pair_race_bin <- factor(df_gn$pair_race_bin, levels = c("WW", "PorP"))

# only gender interactions significant/collinear
anova_lm_gender <- lm(sqrtJIF ~ pair_gender, data = df_gn)
summary(anova_lm_gender)

anova_lm_race <- lm(sqrtJIF ~ pair_race, data = df_gn)
summary(anova_lm_race)

fit_gender <- lm(log10_citation_rate ~ pair_gender*sqrtJIF, data = df_gn)
summary(fit_gender)

# race not predictive when focused on just global north
fit_race <- lm(log10_citation_rate ~ pair_race*sqrtJIF, data = df_gn)
summary(fit_race)

# Figure S26
fit_gn_nosf <- lm(log10_citation_rate ~ sqrtJIF*pair_gender + sqrtJIF*pair_race, data = df_gn)
summary(fit_gn_nosf)
# stargazer(fit, style = "all", covariate.labels = c("MW", "WM", "WW", "JIF", 
#                                                    "WN", "NW", "NN", "MW:JIF",
#                                                    "WM:JIF", "WW:JIF", "EC:JIF",
#                                                    "CE:JIF", "CC:JIF"))

fit_gn_nosf_df <- broom::tidy(fit_gn_nosf) %>% # make data frame with regression results
  mutate(model = ifelse(grepl(":", term), "interaction", "solo")) %>% 
  rename(full_term = term) %>% 
  mutate(term = ifelse(grepl(":", full_term), substr(full_term, 9, nchar(full_term)),
                       full_term))

dwplot(fit_gn_nosf_df,
       vline = geom_vline(xintercept = 0, color = "grey60", linetype = 2, linewidth = 1),
       model_order = c("solo", "interaction"),
       dot_args = list(size = 3),
       whisker_args = list(size = 1),
       dodge_size = 0.7) %>% 
       relabel_predictors(c(sqrtJIF = "Journal Impact Factor",
                            pair_genderMW = "Man-Woman", pair_genderWM = "Woman-Man",
                            pair_genderWW = "Woman-Woman",
                            pair_raceWN = "White-PoC", pair_raceNW = "PoC-White",
                            pair_raceNN = "PoC-PoC",
                            global_north1 = "Global North Senior Author")
       ) +
  theme_bw() +
  scale_color_met_d(name = "Kandinsky", direction = -1) + 
  labs(x = "Coefficient estimate") +
  theme(axis.text = element_text(size = 14, color = "black"),
        legend.position = "none",
        axis.title = element_text(size = 16, color = "black"))

# ggsave("figures/for-pub/supp/coreidd-gn-nosf.pdf", height = 5, width = 7)

#### self citation sensitivity ####

# include self citations
df_sf <- read_csv("coreidd/input_jif_regression_yessf_20241212_public.csv") %>% 
  dplyr::select(article_id, year, num_citations, citation_rate, pair_gender, pair_race, FA_gender,
                LA_gender, FA_race, LA_race, JIF, WC_1st, num_cited_refs, Country, global_north) %>% 
  rename(WC = WC_1st, country = Country) %>% 
  mutate(log10_citation_rate = log10(citation_rate),
         sqrtJIF = sqrt(JIF),
         pair_race_bin = ifelse(pair_race == "WW", "WW", "PorP")) %>% 
  mutate(pair_gender = as.factor(pair_gender),
         pair_race = as.factor(pair_race),
         pair_race_bin = as.factor(pair_race_bin))

df_sf$pair_race <- factor(df_sf$pair_race, levels = c("WW", "WN", "NW", "NN"))
df_sf$pair_race_bin <- factor(df_sf$pair_race_bin, levels = c("WW", "PorP"))
df_sf$global_north <- factor(df_sf$global_north, levels = c("0", "1"))

anova_lm_gender <- lm(sqrtJIF ~ pair_gender, data = df_sf)
summary(anova_lm_gender)

anova_lm_race <- lm(sqrtJIF ~ pair_race, data = df_sf)
summary(anova_lm_race)
#stargazer(anova_lm_race, style = "all")
# now race is collinear

# chisquared test is to determine whether two categorical variables are independent
Xsq <- chisq.test(table(df_sf$FA_gender,df_sf$LA_gender))
Xsq
Xsq <- chisq.test(table(df_sf$FA_race,df_sf$LA_race))
Xsq


fit_gender <- lm(log10_citation_rate ~ pair_gender*sqrtJIF, data = df_sf)
summary(fit_gender)

# Figure S25
fit_yessf <- lm(log10_citation_rate ~ sqrtJIF*pair_gender + sqrtJIF*pair_race + global_north, data = df_sf)
summary(fit_yessf)
# stargazer(fit, style = "all", covariate.labels = c("MW", "WM", "WW", "JIF", "EC", "CE", "CC",
#                                                    "Global North Senior Author", "MW:JIF",
#                                                    "WM:JIF", "WW:JIF", "EC:JIF", "CE:JIF",
#                                                    "CC:JIF"))

fit_yessf_df <- broom::tidy(fit_yessf) %>% # make data frame with regression results
  mutate(model = ifelse(grepl(":", term), "interaction", "solo")) %>% 
  rename(full_term = term) %>% 
  mutate(term = ifelse(grepl(":", full_term), substr(full_term, 9, nchar(full_term)),
                       full_term))

dwplot(fit_yessf_df,
       vline = geom_vline(xintercept = 0, color = "grey60", linetype = 2, linewidth = 1),
       model_order = c("solo", "interaction"),
       dot_args = list(size = 3),
       whisker_args = list(size = 1),
       dodge_size = 0.7) %>% 
       relabel_predictors(c(sqrtJIF = "Journal Impact Factor",
                            pair_genderMW = "Man-Woman", pair_genderWM = "Woman-Man",
                            pair_genderWW = "Woman-Woman",
                            pair_raceWN = "White-PoC", pair_raceNW = "PoC-White",
                            pair_raceNN = "PoC-PoC",
                            global_north1 = "Global North Senior Author")
       ) +
  theme_bw() +
  scale_color_met_d(name = "Kandinsky", direction = -1) + 
  labs(x = "Coefficient estimate") +
  theme(axis.text = element_text(size = 14, color = "black"),
        legend.position = "none",
        axis.title = element_text(size = 16, color = "black"))

# ggsave("figures/for-pub/supp/coreidd-yessf.pdf", height = 5, width = 7)

#### authorship exception sensitivity ####

df_noauth <- read_csv("coreidd/input_jif_regression_nosf_noauth_20241212_public.csv") %>% 
  dplyr::select(article_id, year, num_citations, citation_rate, pair_gender, pair_race, FA_gender,
                LA_gender, FA_race, LA_race, JIF, WC_1st, num_cited_refs, Country, global_north) %>% 
  rename(WC = WC_1st, country = Country) %>% 
  mutate(log10_citation_rate = log10(citation_rate),
         sqrtJIF = sqrt(JIF),
         pair_race_bin = ifelse(pair_race == "WW", "WW", "PorP")) %>% 
  mutate(pair_gender = as.factor(pair_gender),
         pair_race = as.factor(pair_race),
         pair_race_bin = as.factor(pair_race_bin))

df_noauth$pair_race <- factor(df_noauth$pair_race, levels = c("WW", "WN", "NW", "NN"))
df_noauth$pair_race_bin <- factor(df_noauth$pair_race_bin, levels = c("WW", "PorP"))
df_noauth$global_north <- factor(df_noauth$global_north, levels = c("0", "1"))

anova_lm_gender <- lm(sqrtJIF ~ pair_gender, data = df_noauth)
summary(anova_lm_gender)

anova_lm_race <- lm(sqrtJIF ~ pair_race, data = df_noauth)
summary(anova_lm_race)
#stargazer(anova_lm_race, style = "all")
# now race is collinear but gender isn't!
                                                                
# chisquared test is to determine whether two categorical variables are independent
Xsq <- chisq.test(table(df_noauth$FA_gender,df_noauth$LA_gender))
Xsq
Xsq <- chisq.test(table(df_noauth$FA_race,df_noauth$LA_race))
Xsq

# Figure S27
fit_nosf_noauth <- lm(log10_citation_rate ~ sqrtJIF*pair_gender + sqrtJIF*pair_race + global_north, data = df_noauth)
summary(fit)
# stargazer(fit, style = "all", covariate.labels = c("MW", "WM", "WW", "JIF", "EC", "CE", "CC",
#                                                    "Global North Senior Author", "MW:JIF",
#                                                    "WM:JIF", "WW:JIF", "EC:JIF", "CE:JIF",
#                                                    "CC:JIF"))

fit_nosf_noauth_df <- broom::tidy(fit_nosf_noauth) %>% # make data frame with regression results
  mutate(model = ifelse(grepl(":", term), "interaction", "solo")) %>% 
  rename(full_term = term) %>% 
  mutate(term = ifelse(grepl(":", full_term), substr(full_term, 9, nchar(full_term)),
                       full_term))

dwplot(fit_nosf_noauth_df,
       vline = geom_vline(xintercept = 0, color = "grey60", linetype = 2, linewidth = 1),
       model_order = c("solo", "interaction"),
       dot_args = list(size = 3),
       whisker_args = list(size = 1),
       dodge_size = 0.7) %>% 
  relabel_predictors(c(sqrtJIF = "Journal Impact Factor",
                       pair_genderMW = "Man-Woman", pair_genderWM = "Woman-Man",
                       pair_genderWW = "Woman-Woman",
                       pair_raceWN = "White-PoC", pair_raceNW = "PoC-White",
                       pair_raceNN = "PoC-PoC",
                       global_north1 = "Global North Senior Author")
  ) +
  theme_bw() +
  scale_color_met_d(name = "Kandinsky", direction = -1) + 
  labs(x = "Coefficient estimate") +
  theme(axis.text = element_text(size = 14, color = "black"),
        legend.position = "none",
        axis.title = element_text(size = 16, color = "black"))

# ggsave("figures/for-pub/supp/coreidd-nosf-noauth.pdf", height = 5, width = 7)
