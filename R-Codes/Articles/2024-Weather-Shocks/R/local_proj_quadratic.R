# The Dynamic Effects of Weather Shocks on Agricultural Production
# Linear Local Projections with quadratic effects
library(tidyverse)
library(fastDummies)

## 1. Load Data and functions----

# The baseline dataset
load("../data/output/df_lp.rda")

# Functions useful to shape the data for local projections
source("../weatherperu/R/format_data.R")

# Load function in utils
source("../weatherperu/R/utils.R")

# Load detrending functions
source("../weatherperu/R/detrending.R")

# Estimation functions
source("../weatherperu/R/estimations.R")

## 2. Estimation----

# horizons <- 6
# y_name <- "y"
# crop_name <- "Dent corn"
# control_names <- c("rer_hp", "r_hp", "pi", "ind_prod")
# weather_names <- c("temp_max_dev", "precip_sum_dev", "temp_max_dev_sq", "precip_sum_dev_sq")
# group_name <- "region_id"
# share_geo <- NULL
# transition_name <- NULL
# region_id <- 14
# add_month_fe <- TRUE

crops <- df$product_eng |> unique()

df <- 
  df |> 
  mutate(
    temp_max_dev_sq = temp_max_dev^2,
    precip_sum_dev_sq = precip_sum_dev^2
  )


weather_variables <- c(
  "temp_max_dev", "precip_sum_dev",
  "temp_max_dev_sq", "precip_sum_dev_sq"
)
control_variables <- c("rer_hp", "r_hp", "pi", "ind_prod", "ONI", "price_int_inf")



resul_lp_quad <- map(
  crops, ~ estimate_linear_lp(
    df = df,
    horizons = 8,
    y_name = "y",
    group_name = "region_id",
    add_month_fe = FALSE,
    add_intercept = FALSE,
    crop_name = .,
    control_names = control_variables,
    weather_names = weather_variables,
    std = "Cluster",
    other_var_to_keep = "y_new"
  )
)

save(resul_lp_quad, file = "output/resul_lp_quad.rda")



## 3. Plots ----

# Plotting the IRFs

df_irfs_lp_quad <- 
  map(resul_lp_quad, "coefs") |> 
  list_rbind() |> 
  filter(name %in% weather_variables) |> 
  mutate(
    order = ifelse(str_detect(name, "_sq$"), 2, 1),
    name = str_remove(name, "_sq$")
  ) |> 
  pivot_wider(
    names_from = order, 
    values_from = c(value, std, std_shock, median_shock, q05_shock, q95_shock), 
    names_prefix = 'order_') |> 
  rename(d = std_shock_order_1) |> 
  mutate(
    ir = value_order_1 * d + value_order_2 * d^2,
    # IC 95%
    ir_lower_95 = (value_order_1 - qnorm(0.975) * std_order_1) * d + 
      (value_order_2 - qnorm(0.975) * std_order_2) * d^2,
    ir_upper_95 = (value_order_1 + qnorm(0.975) * std_order_1) * d +
      (value_order_2 + qnorm(0.975) * std_order_2) * d^2,
    # IC 68%
    ir_lower_68 = (value_order_1 - qnorm(0.84) * std_order_1) * d + 
      (value_order_2 - qnorm(0.84) * std_order_2) * d^2,
    ir_upper_68 = (value_order_1 + qnorm(0.84) * std_order_1) * d +
      (value_order_2 + qnorm(0.84) * std_order_2) * d^2
  ) |> 
  mutate(
    crop = factor(
      crop, 
      levels = c("Rice", "Dent corn", "Potato", "Cassava"),
      labels = c("Rice", "Maize", "Potato", "Cassava"))
  ) |> 
  mutate(
    name = factor(
      name,
      levels = c(
        "temp_mean_dev",
        "temp_max_dev",
        "temp_daily_mean_dev_normal", 
        "temp_max_dev_normal",
        "perc_gamma_precip_mean",
        "perc_gamma_precip",
        "precip_sum_dev"
      ),
      labels = c(
        "Temp. anomalies", 
        "Temp. anomalies", 
        "Temp. anomalies", 
        "Temp. anomalies", 
        "Precip. anomalies",
        "Precip. anomalies",
        "Precip. anomalies"
      )
    )
  )


df_irfs_lp_quad_ci <- 
  df_irfs_lp_quad |> 
  select(horizon, crop, name, matches("^(ir_lower)|^(ir_upper)", perl = TRUE)) |> 
  pivot_longer(
    cols = matches("^(ir_lower)|^(ir_upper)", perl = TRUE),
    names_pattern = "(.*)_(95|68)$",
    names_to = c(".value", "level")
  ) |> 
  mutate(level = str_c(level, "%"))

p_lp_lin_quad <- 
  ggplot() +
  geom_ribbon(
    data = df_irfs_lp_quad_ci,
    mapping = aes(
      x = horizon,
      ymin = ir_lower, ymax = ir_upper, fill = level),
    alpha = .2
  ) +
  geom_line(
    data = df_irfs_lp_quad,
    mapping = aes(x = horizon, y = ir),
    colour = "#0072B2") +
  geom_hline(yintercept = 0, colour = "#D55E00") +
  ggh4x::facet_grid2(
    name~crop, scales = "free_y", 
    independent = "y", switch = "y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Horizon", y = NULL) +
  scale_fill_manual(
    "C.I. level", 
    values = c("68%" = "gray10", "95%" = "gray60")
  ) +
  theme_paper() +
  theme(strip.placement = "outside")

p_lp_lin_quad

if (1 == 0) {
  # To save plots in PDF using pdflatex
  p_lp_lin_quad <- 
    ggplot() +
    geom_ribbon(
      data = df_irfs_lp_quad_ci |> 
        mutate(
          level = str_replace(level, pattern = "%", replacement = "\\\\%")
        ),
      mapping = aes(
        x = horizon,
        ymin = ir_lower, ymax = ir_upper, fill = level),
      alpha = .2
    ) +
    geom_line(
      data = df_irfs_lp_quad,
      mapping = aes(x = horizon, y = ir),
      colour = "#0072B2",
      linewidth = 1) +
    geom_hline(yintercept = 0, colour = "#D55E00") +
    ggh4x::facet_grid2(
      name~crop, scales = "free_y", 
      independent = "y", switch = "y") +
    scale_y_continuous(labels = scales::label_percent(suffix = "\\%")) +
    labs(x = "Horizon", y = NULL) +
    scale_fill_manual(
      "C.I. level", 
      values = c("68\\%" = "gray10", "95\\%" = "gray60")
    ) +
    theme_paper() +
    theme(strip.placement = "outside")
  
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_lp_lin_quad, 
    path = "../../figs/", 
    filename = "fig_lp_quad",
    width = 7,
    height = 4.5)
}