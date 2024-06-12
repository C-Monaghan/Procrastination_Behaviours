rm(list = ls())

library(dplyr)
library(ggplot2)
library(gamlss)

path_data <- "./01__Data/02__Processed/"

# Reading in data --------------------------------------------------------------
health_data <- readxl::read_xlsx(file.path(path_data, "Health_data.xlsx"))

# Overall number of people who have drank in the HRS ---------------------------
health_data %>%
  select(Drink_ever) %>%
  mutate(Drink_ever = ifelse(Drink_ever == 1, "Yes", "No"),
         Drink_ever = factor(Drink_ever, levels = c("Yes", "No"))) %>%
  ggplot(aes(x = Drink_ever, fill = Drink_ever)) +
  geom_bar(width = 0.75, colour = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            vjust = -0.5, position = position_dodge(width = 0.75)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  labs(title = "Do you ever drink any alcoholic beverages",
       x = "", y = "Count") +
  theme_minimal() +
  ggeasy::easy_center_title() +
  ggeasy::easy_remove_legend() +
  colorspace::scale_fill_discrete_qualitative(palette = "Set 3") +
  theme(
    plot.title = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.text = element_text(size = 12))

# Days Drinking and Not Drinking -----------------------------------------------
health_data %>%
  select(Days_drink, Days_no_drink) %>%
  rename(`Days drinking` = "Days_drink", `Days not drinking` = "Days_no_drink") %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = "Drink",
                      values_to = "Amount") %>%
  ggplot(aes(x = Amount, fill = Drink)) +
  geom_bar(colour = "black", width = 0.75) +
  scale_x_continuous(breaks = seq(0, 7, by = 1)) +
  labs(title = "How many days per week have you had any alcohol to drink (in last 3 months)",
       x = "", y = "Count") +
  facet_wrap(~ Drink) +
  theme_minimal() +
  ggeasy::easy_center_title() +
  ggeasy::easy_remove_legend() +
  colorspace::scale_fill_discrete_qualitative(palette = "Set 3")

# Days drink vs. days not drink by procrastination (Line Graph)
health_data %>%
  select(Days_drink, Days_no_drink, Total_procrastination) %>%
  group_by(Total_procrastination) %>%
  summarise(`Days Drinking` = mean(Days_drink, na.rm = TRUE),
            `Days not Drinking` = mean(Days_no_drink, na.rm = TRUE)) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = -Total_procrastination,
                      names_to = "Drink",
                      values_to = "Days") %>%
  ggplot(aes(x = Total_procrastination, y = Days, colour = Drink)) +
  geom_line(linewidth = 0.75) +
  scale_x_continuous(breaks = seq(0, 60, by = 10), labels = scales::comma) +
  scale_y_continuous(breaks = seq(0, 7, by = 1), labels = scales::comma) +
  facet_wrap(~ Drink, labeller = as_labeller(c(`Days Drinking` = "Days drinking", `Days not Drinking` = "Days not drinking"))) +
  labs(title = "Mean days drinking / not drinking by procrastination",
       x = "Total Procrastination",
       y = "Mean Days") +
  theme_minimal() +
  ggeasy::easy_center_title() +
  ggeasy::easy_remove_legend() +
  colorspace::scale_color_discrete_qualitative(palette = "Dark 3") +
  theme(
    plot.title = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.text = element_text(size = 12))

# Days drink vs. days not drink by procrastination (Scatter Plot)
health_data %>%
  select(Days_drink, Days_no_drink, Total_procrastination) %>%
  group_by(Total_procrastination) %>%
  summarise(`Days Drinking` = mean(Days_drink, na.rm = TRUE),
            `Days not Drinking` = mean(Days_no_drink, na.rm = TRUE)) %>%
  ungroup() %>%
  tidyr::pivot_longer(cols = -Total_procrastination,
                      names_to = "Drink",
                      values_to = "Days") %>%
  ggplot(aes(x = Total_procrastination, y = Days, colour = Drink)) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(0, 60, by = 10), labels = scales::comma) +
  scale_y_continuous(breaks = seq(0, 7, by = 1), labels = scales::comma) +
  labs(title = "Mean days drinking / not drinking by procrastination",
       x = "Total Procrastination",
       y = "Mean Days") +
  theme_minimal() +
  facet_wrap(~ Drink, labeller = as_labeller(c(`Days Drinking` = "Days drinking", `Days not Drinking` = "Days not drinking"))) +
  ggeasy::easy_center_title() +
  ggeasy::easy_remove_legend() +
  colorspace::scale_color_discrete_qualitative(palette = "Dark 3") +
  theme(
    plot.title = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.text = element_text(size = 12))

# Proportion of days not drinking ----------------------------------------------
health_data %>%
  select(Total_procrastination, Days_drink, Days_no_drink) %>%
  tidyr::pivot_longer(cols = c(Days_drink, Days_no_drink),
                      names_to = "Drink", 
                      values_to = "Days") %>%
  ggplot(aes(x = Total_procrastination, y = Days / 7, colour = Drink)) +
  geom_jitter(height = 0.05, width = 0.05, alpha = 0.8) +
  scale_x_continuous(breaks = seq(0, 60, by = 10), labels = scales::comma) +
  facet_wrap(~Drink, labeller = as_labeller(c("Days_drink" = "Days drinking", "Days_no_drink" = "Days not drinking"))) +
  labs(title = "Proportion of days drinking / not drinking", 
       x = "Total Procrastination", y = "Prob") +
  theme_minimal() +
  ggeasy::easy_center_title() +
  ggeasy::easy_remove_legend() +
  colorspace::scale_color_discrete_qualitative(palette = "Dark 3")


health_data %>%
  select(Days_no_drink, Days_drink) %>%
  filter(complete.cases(.)) %>%
  cbind() %>%
  head()

x <- health_data %>%
  select(Days_no_drink) %>%
  filter(complete.cases(.))

fitdistrplus::descdist(data = x$Days_no_drink / 7, discrete = FALSE)

new_x <- x$Days_no_drink/7

bb_dist <- fitdistrplus::fitdist(new_x/, "beta")

fitdistrplus::plotdist(x$Days_no_drink/7, histo = TRUE, demp = TRUE)


TidyDensity::tidy_empirical(new_x) %>%
  TidyDensity::tidy_autoplot()

TidyDensity::tidy_beta() %>%
  TidyDensity::tidy_autoplot()

plot_dist <- function(mu0, sigma0) {
  
  tibble(days = rBB(n = 10^3, mu = mu0, sigma = sigma0, bd = 28)) %>% 
    count(days) %>% 
    mutate(percent = n / sum(n)) %>% 
    ggplot(aes(days, percent)) + 
    geom_col(fill = "#A02898") + 
    labs(title = expr(mu==!!mu0~and~sigma==!!sigma0)) +
    scale_x_continuous(breaks = seq(0, 28, 4), limits = c(-1, 29))
  
}

(plot_dist(mu0 = 0.7235531, sigma0 = 0.3173789)) 
  
(plot_dist(mu0 = .5, sigma0 = .5)) 



/ 
  (plot_dist(mu0 = .5, sigma0 =  2) + plot_dist(mu0 = .9, sigma0 = .2))

# OLD Code ---------------------------------------------------------------------
# Procrastination
# health_data %>%
#   select(Total_procrastination) %>%
#   ggplot(aes(Total_procrastination)) +
#   geom_density(fill = "powderblue", alpha = 0.8) +
#   scale_x_continuous(breaks = seq(0, 60, by = 10)) +
#   labs(title = "Distribution of Procrastination", 
#        x = "Total Procrastination", y = "") +
#   theme_minimal() +
#   ggeasy::easy_center_title()

# Smokers and drinkers
# health_data %>%
#   select(ID, Smoke_current, Drink_ever) %>%
#   filter(!is.na(Smoke_current)) %>%
#   tidyr::pivot_longer(cols = !ID,
#                       names_to = "Behaviour",
#                       values_to = "Status") %>%
#   mutate(Behaviour = case_when(Behaviour == "Smoke_current" ~ "Smoking",
#                                Behaviour == "Drink_ever" ~ "Drinking"),
#          Status = recode(Status, `0` = "No", `1` = "Yes")) %>%
#   ggplot(aes(x = Status, fill = Behaviour)) +
#   geom_bar(position = "dodge", width = 0.75, colour = "black") +
#   geom_text(stat = "count", aes(label = after_stat(count)), 
#             vjust = -0.5, position = position_dodge(width = 0.75)) +
#   scale_y_continuous(expand = expansion(mult = 0.1)) +
#   labs(title = "Distribution of smoking and drinking behaviour",
#        x = "", y = "Count") +
#   theme_minimal() +
#   ggeasy::easy_center_title() +
#   ggeasy::easy_move_legend(to = "bottom") +
#   ggeasy::easy_add_legend_title("") +
#   colorspace::scale_fill_discrete_qualitative(palette = "Set 3")