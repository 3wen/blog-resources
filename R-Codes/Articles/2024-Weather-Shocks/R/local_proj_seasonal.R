# The Dynamic Effects of Weather Shocks on Agricultural Production
# Time-varying exposure to weather shocks

library(tidyverse)
library(fastDummies)
library(cli)

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

## 2. Defining the transition variable----

# Duration of the growing season
gs_duration_df <- tribble(
  ~product_eng, ~tc,
  "Rice", 7,
  "Dent corn", 7,
  "Potato", 9,
  "Cassava", 9
)

crops <- c("Rice", "Dent corn", "Potato", "Cassava")


#' Add lagged columns in a tibble for a variable
#' Source: https://typethepipe.com/vizs-and-tips/how-to-create-multiple-lags-in-r/
#' 
#' @param df tibble
#' @param var name of the variable to get lag of
#' @param lags vector of desired lags
calculate_lags <- function(df, var, lags) {
  map_lag <- lags |> map(~ partial(dplyr::lag, n = .x))
  df |> 
    mutate(
      across(
        .cols = {{var}},
        .fns = map_lag,
        .names = "{.col}_lag{lags}"
      )
    )
}

df_2 <- vector(mode = "list", length = length(crops))
cli_progress_bar(total = length(crops))
for(i_crop in 1:length(crops)) {
  crop <- crops[i_crop]
  df_tmp <- 
    df |> 
    filter(product_eng == crop)
  
  # Duration
  tc <- 
    gs_duration_df |> 
    filter(product_eng == crop) |> 
    pull(tc)
  
  df_tmp <- 
    df_tmp |> 
    group_by(region_id) |> 
    calculate_lags(surf_m, 1:tc) |> 
    calculate_lags(Value_surfR, 1:tc) |> 
    rowwise() |> 
    mutate(
      # Sum of the planted surface over the last `tc` months
      sum_surf_m = sum(
        across(
          c(matches("^surf_m_lag"), surf_m) # planted surface
        ),
        na.rm = TRUE
      ),
      # Sum of the harvested surface over the last `tc` months
      sum_Value_surfR = sum(
        across(
          c(matches("^Value_surfR_lag"), Value_surfR) # harvested surface
        ), 
        na.rm = TRUE
      ),
    ) |> 
    group_by(region_id) |> 
    mutate(flow_y_tmp = sum_surf_m - sum_Value_surfR) |> 
    mutate(flow_y = lag(flow_y_tmp)) |> 
    mutate(flow_y = ifelse(row_number() == 1, flow_y_tmp, flow_y)) |>
    select(
      -matches("^surf_m_lag"), 
      -matches("^Value_surfR_lag"),
      -flow_y_tmp, -sum_surf_m, -sum_Value_surfR
    ) |> 
    ungroup()
  
  # Detrend with HP filter
  df_tmp <- 
    df_tmp |> 
    group_by(region_id) |> 
    mutate(
      flow_y_cycle = as.vector(
        mFilter::hpfilter(
          flow_y, freq = 14400, type = "lambda", drift = FALSE)$cycle
      )
    ) |> 
    mutate(
      # normalizing to have a unit variant (so that gamma in the logistic 
      # function is scale invariant)
      flow_y_pot = flow_y_cycle / sd(flow_y_cycle)
    )
  
  df_2[[i_crop]] <- df_tmp
  cli_progress_update(set = i_crop)
}

df <- df_2 |> bind_rows()

# flow_y_pot: sum planted - sum harvested during the last tc months
# flow_y_pot > 0 <=> (sum harvested < sum planted) which is during growing season



## 3. Estimation----
# 
# horizons <- 6
# y_name <- "y"
# crop_name <- "Rice"
# control_names <- c("rer", "r", "pi", "ind_prod")
# weather_names <- c("temp_max_dev", "perc_gamma_precip")
# group_name <- "region_id"
# region_id <- 14
# share_geo <- NULL
# transition_name <- "flow_y_pot"
# gamma <- 3
# std = "cluster"

plot(density(df$flow_y_pot))

weather_variables <- c("temp_max_dev", "precip_sum_dev")
control_variables <- c("rer_hp", "r_hp", "pi", "ind_prod", "ONI", "price_int_inf")

# Using the PDF of the Normal (0,1) distribution to define states
resul_lp_regime <- 
  map(
    crops, ~ estimate_linear_lp(
      df = df,
      horizons = 8,
      y_name = "y",
      group_name = "region_id",
      add_month_fe = FALSE,
      add_intercept = FALSE,
      crop_name = .x,
      control_names = control_variables,
      weather_names = weather_variables,
      share_geo = NULL,
      transition_name = "flow_y_pot",
      transition_method = "normal",
      state_names = c("planted", "harvested"),
      gamma = NULL,
      std = "cluster"
    )
  )
names(resul_lp_regime) <- crops

## 4. Plots ----

# Plotting the IRFs
weather_variables_season <-str_c(
  rep(c("planted", "harvested"), each = 2),
  rep(weather_variables, 2),
  sep = "_"
)


df_irfs_lp <- 
  map(resul_lp_regime, "coefs") |> 
  list_rbind() |> 
  filter(name %in% weather_variables_season) |> 
  rowwise() |> 
  mutate(
    regime = case_when(
      str_detect(name, "^planted") ~ "Growing season",
      str_detect(name, "^harvested") ~ "Harvesting season",
      TRUE ~ "Error"
    ),
    weather = str_split(name, "(^planted_|^harvested_)")[[1]][2]
  ) |> 
  ungroup() |> 
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
    weather = factor(
      weather,
      levels = c(
        "temp_max_dev",
        "precip_sum_dev"
      ),
      labels = c(
        "Temp. anomalies", 
        "Precip. anomalies"
      )
    ),
    regime = factor(
      regime,
      levels = c("Growing season", "Harvesting season")
    )
  )


df_irfs_lp_ci <- 
  df_irfs_lp |> 
  select(horizon, crop, weather, regime, matches("^(lower)|^(upper)", perl = TRUE)) |> 
  pivot_longer(
    cols = matches("^(lower)|^(upper)", perl = TRUE),
    names_pattern = "(.*)_(95|68)$",
    names_to = c(".value", "level")
  ) |> 
  mutate(level = str_c(level, "%"))

p_lp_seasonal <- 
  ggplot() +
  geom_ribbon(
    data = df_irfs_lp_ci |> filter(level == "68%"),
    mapping = aes(
      x = horizon,
      ymin = lower, ymax = upper, fill = regime
    ),
    alpha = .4
  ) +
  geom_line(
    data = df_irfs_lp,
    mapping = aes(x = horizon, y = shock_1_sd, colour = regime)) +
  geom_hline(yintercept = 0, colour = "#D55E00") +
  ggh4x::facet_grid2(weather~crop, scales = "free_y", independent = "y",
                     switch = "y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Horizon", y = NULL) +
  scale_fill_manual(
    NULL,
    values = c(
      "Growing season" = "#949698",
      "Harvesting season" = "#009E73"
    )
  ) +
  scale_colour_manual(
    NULL,
    values = c(
      "Growing season" = "#949698",
      "Harvesting season" = "#009E73"
    )
  ) +
  theme_paper() +
  theme(strip.placement = "outside")

p_lp_seasonal

if (1 == 0) {
  # Save plot in PDF using pdflatex
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_lp_seasonal + 
      scale_y_continuous(labels = scales::percent_format(suffix = "\\%")),
    path = "../../figs/", 
    filename = "fig_lp_seasonal",
    width = 7,
    height = 4.5)
}


# 6. Comparison with linear LP----

df_irfs_lp_season <- df_irfs_lp
df_irfs_lp_ci_season <- df_irfs_lp_ci

load("output/df_irfs_lp.rda")
df_irfs_lp_season_comparison <- 
  df_irfs_lp |> 
  mutate(regime = "Linear") |> 
  rename(weather = name) |> 
  bind_rows(
    df_irfs_lp_season |> 
      filter(regime == "Growing season") |> 
      mutate(regime = "State-dependent (growing season)")
  ) |> 
  filter(horizon <= 8)

df_irfs_lp_ci_season_comparison <- 
  df_irfs_lp_ci |> 
  mutate(regime = "Linear") |> 
  rename(weather = name) |> 
  bind_rows(
    df_irfs_lp_ci_season |> 
      filter(regime == "Growing season") |> 
      mutate(regime = "State-dependent (growing season)")
  ) |> 
  filter(horizon <= 8)

p_lp_season_comparison <- 
  ggplot() +
  geom_ribbon(
    data = df_irfs_lp_ci_season_comparison |> 
      filter(level == "68%"),
    mapping = aes(
      x = horizon,
      ymin = lower, ymax = upper, fill = regime
    ),
    alpha = .4
  ) +
  geom_line(
    data = df_irfs_lp_season_comparison,
    mapping = aes(x = horizon, y = shock_1_sd, colour = regime)) +
  geom_hline(yintercept = 0, colour = "#D55E00") +
  ggh4x::facet_grid2(
    weather~crop, scales = "free_y", independent = "y",
    switch = "y"
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Horizon", y = NULL) +
  scale_fill_manual(
    NULL,
    values = c(
      "Linear" = "#0072B2",
      "State-dependent (growing season)" = "#E69F00"
    )
  ) +
  scale_colour_manual(
    NULL,
    values = c(
      "Linear" = "#0072B2",
      "State-dependent (growing season)" = "#E69F00"
    )
  ) +
  theme_paper() +
  theme(strip.placement = "outside")

p_lp_season_comparison

if (1 == 0) {
  # To save plot pdf using pdflatex
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_lp_season_comparison + 
      scale_y_continuous(labels = scales::percent_format(suffix = "\\%")),
    path = "../../figs/", 
    filename = "fig_lp_season_comparison",
    width = 7,
    height = 4.5)
}