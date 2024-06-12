rm(list = ls())

library(dplyr)
library(mgcv)
library(gamlss)
library(ggplot2)

path_data <- "./01__Data/02__Processed/"

# Reading in data --------------------------------------------------------------
health_data <- readxl::read_xlsx(file.path(path_data, "Health_data.xlsx"))

# ZIBB GAM ---------------------------------------------------------------------
# Processing for GAMLSS
gamlss_data <- health_data %>%
  select(Days_drink, Days_no_drink, Total_procrastination, Age) %>%
  filter(complete.cases(.))

# Fitting model (Simple)
fit_1 <- gamlss(formula = cbind(Days_no_drink, Days_drink) ~ Total_procrastination + Age,
                sigma.formula = ~Total_procrastination + Age,
                family = BB(mu.link = "logit"),
                control = gamlss.control(trace = FALSE),
                data = gamlss_data)

# Model Output
summary(fit_1)

# Diagnostic plots
plot(fit_1)
childsds::wormplot_gg(m = fit_1, y.limits = c(-1.5, 1.5))


# Fitting model (penalised beta spline)
fit_2 <- gamlss(formula = cbind(Days_no_drink, Days_drink) ~ pb(Total_procrastination) + Age,
                sigma.formula = ~Total_procrastination,
                family = BB(mu.link = "logit"),
                data = gamlss_data)

# Model Output
summary(fit_2)

# Diagnostic plots
plot(fit_2)
childsds::wormplot_gg(m = fit_2, y.limits = c(-1.5, 1.5))


# Fitting model (smoothing spline)
fit_3 <- gamlss(formula = cbind(Days_no_drink, Days_drink) ~ cs(Total_procrastination) + cs(Age),
                sigma.formula = ~Total_procrastination,
                family = BB(mu.link = "logit"),
                data = gamlss_data)

# Model Output
summary(fit_3)

# Diagnostic plots
plot(fit_3)
childsds::wormplot_gg(m = fit_3, y.limits = c(-1.5, 1.5))

# Making predictions -----------------------------------------------------------
test_data <- expand.grid(
  Total_procrastination = seq(0, 60, length = 200)
)

preds <- predict(fit_1, newdata = test_data, type = "response", data = gamlss_data)
pb_preds <- predict(fit_2, newdata = test_data, type = "response", data = gamlss_data)
cs_preds <- predict(fit_3, newdata = test_data, type = "response", data = gamlss_data)

test_data$linear <- preds
test_data$pb_sline <- pb_preds
test_data$smoothing_sline <- cs_preds


test_data %>%
  tidyr::pivot_longer(cols = c(linear, pb_sline, smoothing_sline),
                      names_to = "Prediction",
                      values_to = "Value") %>%
  ggplot(aes(x = Total_procrastination, y = Value, colour = Prediction)) +
  geom_line(linewidth = 0.75) +
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  ylim(0.6, 1) +
  labs(title = "ZIBB GAMLSS Prediction", x = "Total Procrastination", y = "Prediction") +
  theme_minimal() +
  ggeasy::easy_center_title() +
  ggeasy::easy_remove_legend_title() +
  ggeasy::easy_move_legend(to = "bottom")


# Fitting Model
binomial_gam <- gam(cbind(Days_no_drink, Days_drink) ~ s(Total_procrastination),
                    data = health_data,
                    family = binomial(link = "logit"),
                    method = "REML")

health_data %>%
  select(Days_no_drink) %>%
  complete.cases(.) %>%
  mutate(Days_no_drink / 7)

# Model output
broom::tidy(binomial_gam) %>%
  as.data.frame() %>%
  kableExtra::kbl(align = 'c') %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed", "responsive"))
