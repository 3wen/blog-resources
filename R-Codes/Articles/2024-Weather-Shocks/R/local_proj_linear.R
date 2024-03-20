# The Dynamic Effects of Weather Shocks on Agricultural Production
# Linear Local Projections
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

# horizons <- 0
# y_name <- "y"
# crop_name <- "Rice"
# control_names <- c("rer_hp", "r_hp", "pi", "ind_prod", "ONI", "price_int_inf")
# weather_names <- c("temp_max_dev", "perc_gamma_precip")
# group_name <- "region_id"
# add_month_fe <- FALSE
# share_geo <- NULL
# transition_name <- NULL
# region_id <- 14


crops <- df$product_eng |> unique()
weather_variables <- c("temp_max_dev", "precip_sum_dev")
control_variables <- c("rer_hp", "r_hp", "pi", "ind_prod", "ONI", "price_int_inf")


resul_lp <- map(
  crops, ~ estimate_linear_lp(
    df,
    horizons = 12,
    y_name = "y",
    group_name = "region_id",
    add_month_fe = FALSE,
    add_intercept = FALSE,
    crop_name = .x,
    control_names = control_variables,
    weather_names = weather_variables,
    std = "Cluster",
    other_var_to_keep = "y_new"
  )
)


save(resul_lp, file = "output/resul_lp.rda")

## 3. Plots ----

# Plotting the IRFs

df_irfs_lp <- map(resul_lp, "coefs") |> 
  list_rbind() |> 
  filter(name %in% weather_variables) |> 
  mutate(
    shock_1_sd = value * std_shock,
    lower_95 = (value - qnorm(0.975) * std) * std_shock,
    upper_95 = (value + qnorm(0.975) * std) * std_shock,
    lower_68 = (value - qnorm(0.84)  * std) * std_shock,
    upper_68 = (value + qnorm(0.84)  * std) * std_shock
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


df_irfs_lp_ci <- 
  df_irfs_lp |> 
  select(horizon, crop, name, matches("^(lower)|^(upper)", perl = TRUE)) |> 
  pivot_longer(
    cols = matches("^(lower)|^(upper)", perl = TRUE),
    names_pattern = "(.*)_(95|68)$",
    names_to = c(".value", "level")
  ) |> 
  mutate(level = str_c(level, "%"))

p_lp_lin <- 
  ggplot() +
  geom_ribbon(
    data = df_irfs_lp_ci |> filter(horizon <= 8),
    mapping = aes(
      x = horizon,
      ymin = lower, ymax = upper, fill = level),
    alpha = .2
  ) +
  geom_line(
    data = df_irfs_lp |> filter(horizon <= 8),
    mapping = aes(x = horizon, y = shock_1_sd),
    colour = "#0072B2") +
  geom_hline(yintercept = 0, colour = "gray40") +
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

p_lp_lin



save(df_irfs_lp, df_irfs_lp_ci, file = "output/df_irfs_lp.rda")

if (1 == 0) {
  # Save the plots in PDF using pdflatex
  p_lp_lin <- 
    ggplot() +
    geom_ribbon(
      data = df_irfs_lp_ci |> 
        mutate(
          level = str_replace(level, pattern = "%", replacement = "\\\\%")
        ) |> 
        filter(horizon <= 8),
      mapping = aes(
        x = horizon,
        ymin = lower, ymax = upper, fill = level),
      alpha = .2
    ) +
    geom_line(
      data = df_irfs_lp |> filter(horizon <= 8),
      mapping = aes(x = horizon, y = shock_1_sd),
      colour = "#0072B2",
      linewidth = 1) +
    geom_hline(yintercept = 0, colour = "gray40") +
    ggh4x::facet_grid2(
      name~crop, scales = "free_y", 
      independent = "y", switch = "y") +
    scale_y_continuous(labels = scales::label_percent(suffix = "\\%")) +
    labs(x = "Horizon", y = NULL) +
    scale_fill_manual(
      "C.I. level", 
      # values = c("68\\%" = alpha("blue", .8), "95\\%" = alpha("gray90", .8))
      values = c("68\\%" = "gray10", "95\\%" = "gray60")
    ) +
    theme_paper() +
    theme(strip.placement = "outside")
  
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_lp_lin, 
    path = "../../figs/", 
    filename = "fig_lp_lin",
    width = 7,
    height = 4.5)
}





