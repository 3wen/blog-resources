# From Regional to Aggregate Fluctuations
library(tidyverse)
library(labelled)
# {seasonal} and {timetk} are needed here, for X13
source("../weatherperu/R/utils.R")
source("../weatherperu/R/detrending.R")

# 1. Synthetic measure of the weather----

# Data used in the local projections
load("../data/output/df_lp.rda")

average_price_crops <- 
  df |> group_by(product_eng) |> 
  summarise(price_crop = mean(Value_prices, na.rm = TRUE))


# Load the estimated LP
load("output/resul_lp.rda")

# Put the coefficients in a table, for each crop and each time horizon
weather_variables <- c(
  "temp_max_dev", "precip_sum_dev"
  )
coefs_weather <- map(
  .x = resul_lp,
  .f = ~ .x$coefs |> 
    filter(name %in% weather_variables) |> 
    select(horizon, name, value)
)
names(coefs_weather) <- map_chr(resul_lp, "crop_name")
coefs_weather <- 
  list_rbind(coefs_weather, names_to = "crop")



#' Computes the contribution of the weather for a single crop and time horizon
#'
#' @param lp_crop LP for a specific crop
#' @param h horizon (single value)
#' @param weather_variables vector of names of the weather variables
#' @param ic_conf confidence interval used to determine whether a shock has a
#'   significant effect (default to .95)
weather_contrib_crop_h <- function(lp_crop,
                                   h,
                                   weather_variables,
                                   ic_conf = .95) {
  # The data
  data_lp <- lp_crop$data_lp
  data_lp <- data_lp[[which(names(data_lp) == h)]] |> 
    select(region_id, date, product_eng, !!weather_variables) |> 
    ungroup()
  # The coefficients
  lp_coefs <- 
    lp_crop$coefs |> 
    filter(horizon == !!h, name %in% !!weather_variables) |> 
    mutate(
      value_lb = value - qnorm(1 - ((1 - ic_conf) / 2)) * std,
      value_ub = value + qnorm(1 - ((1 - ic_conf) / 2)) * std
    )
  
  # Keeping the values
  lp_coefs_value <- lp_coefs$value
  # The lower and upper bounds
  lp_coefs_value_lb <- lp_coefs$value_lb
  lp_coefs_value_ub <- lp_coefs$value_ub
  
  data_lp |> 
    nest(.by = c(date, region_id)) |> 
    mutate(
      contribution = map(
        .x = data,
        .f = ~ as.matrix(.x[, weather_variables]) %*% lp_coefs_value |> 
          sum()
      ),
      contribution_lb = map(
        .x = data,
        .f = ~ as.matrix(.x[, weather_variables]) %*% lp_coefs_value_lb |> 
          sum()
      ),
      contribution_ub = map(
        .x = data,
        .f = ~ as.matrix(.x[, weather_variables]) %*% lp_coefs_value_ub |> 
          sum()
      )
    ) |> 
    unnest(cols = c(contribution, contribution_lb, contribution_ub)) |> 
    select(-data) |> 
    mutate(
      significant = (contribution_lb > 0 & contribution_ub > 0) | (contribution_lb < 0 & contribution_ub < 0),
      significant = as.numeric(significant)
    ) |> 
    mutate(date = date + lubridate::period(str_c(!!h, " month")))
}


#' Computes the contribution of the weather for a single crop, for all horizons
#'
#' @param lp_crop LP for a specific crop
#' @param weather_variables vector of names of the weather variables
contrib_weather_crop <- function(lp_crop,
                                 weather_variables) {
  horizons <- unique(lp_crop$coefs$horizon)
  
  map(
    .x = horizons,
    .f = ~weather_contrib_crop_h(
      lp_crop = lp_crop, 
      h = .x, 
      weather_variables = weather_variables
    ) |> 
      mutate(horizon = .x)
  ) |> 
    list_rbind() |> 
    # group_by(date) |> 
    # summarise(value = sum(contribution)) |> 
    mutate(crop = lp_crop$crop_name) |> 
    mutate(contribution_signif = contribution * significant)
}

# Apply the `contrib_weather_crop()` function to each crop
# 
weather_measure_crop <- 
  map(
    .x = resul_lp,
    .f = ~contrib_weather_crop(
      lp_crop = .x, 
      weather_variables = weather_variables
    )
  ) |> 
  list_rbind()


weather_measure_crop <- 
  weather_measure_crop |> 
  left_join(
    df |> select(product_eng, region_id, date, y_new, Value_prices),
    by = c("date", "crop" = "product_eng", "region_id")
  ) |> 
  left_join(
    average_price_crops,
    by = c("crop" = "product_eng")
  )

# \gamma_{c,i,t,h}
weather_measure_crop

quantity_weight <- 
  weather_measure_crop |> 
  filter(horizon == 0) |> 
  group_by(crop, date, horizon, price_crop) |> 
  summarise(quantity_weight = sum(price_crop * y_new), .groups = "drop") |> 
  select(crop, date, quantity_weight)


agricultural_output <- 
  quantity_weight |> 
  group_by(date) |> 
  summarise(quantity_weight = sum(quantity_weight), .groups = "drop") |> 
  mutate(
    # Remove seasonality
    q_sa = adj_season_X13(quantity_weight, ymd("2001-01-01")),
    # Extract trend
    q_sa_trend = hp_filter_trend(q_sa, freq = 14400),
    # Percentage dev. from trend
    q = 100 * log(q_sa / q_sa_trend)
  ) |> 
  select(date, q)




weather_adjusted_ag <- 
  weather_measure_crop |> 
  # each group: observations across regions, for a crop x date x horizon
  group_by(crop, date, horizon, price_crop) |> 
  # weather-adjusted agricultural production at each horizon
  # y_{c,t,h}^{w}
  summarise(
    y_w = sum(price_crop * contribution_signif *  y_new / n(), na.rm = TRUE),
    .groups = "drop"
  ) |> 
  group_by(crop, date) |> 
  # weather-adjusted agricultural production summed over horizons
  # y_{c,t}^{w}
  summarise(
    y_w = sum(y_w),
    .groups = "drop"
  )


w_df <- 
  weather_adjusted_ag |> 
  left_join(quantity_weight, by = c("crop", "date")) |> 
  group_by(date) |> 
  summarise(
    w = sum(y_w) / sum(quantity_weight),
    .groups = "drop"
  )

w_df <- w_df |> 
  mutate(
    w_trend = hp_filter_trend(w, freq = 14400),
    w_dt = - 100 * (w - w_trend),
  )

# 2. Macro data----

# See `data-macro.R`
load("../data/output/macro/df_macro.rda")

df_var <- 
  df_macro |> 
  left_join(
    w_df,
    by = "date"
  ) |> 
  left_join(
    agricultural_output,
    by = "date"
  )

# We use the variable w_dt as the "Agricultural losses" variable
df_var <- 
  df_var |> 
  mutate(
    w = w_dt
  )



variable_names <- c(
  "Agricultural losses" = "w",
  "Real exchange rate" = "rer_dt_sa",
  "Inflation rate (pp)" = "pi",
  "Food inflation rate (pp)" = "pia",
  "Exports (pp)" = "x",
  "Agricultural output (pp)" = "q",
  "GDP (pp)" = "y",
  "Interest rate (pp)" = "r"
)

df_var <- 
  df_var |>  
  labelled::set_variable_labels(
    w = "Agricultural losses",
    q = "Agricultural output (pp)"
  ) |> 
  mutate(
    w = w / 100, 
    rer_dt_sa = rer_dt_sa / 100, 
    pi = pi / 100, 
    pia = pia / 100, 
    # x
    q = q / 100, 
    y = y / 100, 
    r = r / 100
  )

p_var_series <- 
  ggplot(
    data = df_var |> 
      filter(date >= "2003-01-01") |> 
      select(date, w, rer_dt_sa, pi, pia, x, q, y, r) |> 
      pivot_longer(cols = -date) |> 
      mutate(
        name = factor(
          name,
          levels = variable_names,
          labels = names(variable_names)
        )
      ),
    mapping = aes(x = date, y = value)
  ) +
  geom_line() +
  facet_wrap(~name, scales = "free_y") +
  theme_paper() +
  labs(x = NULL, y = NULL)

df_var |> 
  filter(date >= "2003-01-01") |> 
  select(-date) |> cor() |> round(2)


p_var_series

if (1 == 0) {
  # To save the plot in PDF using pdflatex
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_var_series,
    path = "../../figs/", 
    filename = "fig_var_series",
    width = 9,
    height = 4.5
  )
}


# 3. Estimation----

start_date <- "2003-01-01"

df_var_ts <-
  df_var |> 
  filter(date >= start_date) |> 
  select(
    w, rer_dt_sa, pi, pia, x, q, y, r
  ) |>
  ts(start = c(year(start_date), month(start_date)), freq = 12)


plot(df_var_ts)
cor(df_var_ts)

info_var_estim <- vars::VARselect(y = df_var_ts, type = "const", lag.max = 6)
info_var_estim

var_l1 <- vars::VAR(y = df_var_ts, p = 2, type = "const")
summary(var_l1)

# Imposing structure
a <- diag(1, 8)
a[lower.tri(a)] <- NA

# Structural VAR
svar_a <- vars::SVAR(var_l1, Amat = a, max.iter = 1000)

save(svar_a, df_var, file = "output/var_objects.rda")


## IRFs----

impulse_name <- "w"

estim_irf <- FALSE

if (estim_irf) {
  
  nb_runs <- 500
  
  irfs_95 <- vars::irf(
    svar_a, impulse = impulse_name, boot = TRUE, ci = .95, 
    n.ahead = 20, runs = nb_runs
  )
  irfs_68 <- vars::irf(
    svar_a, impulse = impulse_name, boot = TRUE, ci = .68, 
    n.ahead = 20, runs = nb_runs
  )
  
  save(irfs_95, file = "output/irfs_95.rda")
  save(irfs_68, file = "output/irfs_68.rda")
} else {
  load("output/irfs_95.rda")
  load("output/irfs_68.rda")
}

## Plot----

levels <- variable_names
labels <- names(variable_names)


df_irfs <- 
  irfs_95$irf[[impulse_name]] |> 
  as_tibble() |> 
  mutate(horizon = row_number() - 1) |> 
  pivot_longer(cols = -horizon) |> 
  mutate(impulse = !!impulse_name)

df_irfs <- 
  df_irfs |> 
  mutate(
    name = factor(
      name,
      levels = !!levels,
      labels = !!labels
    )
  )

# Confidence intervals
df_irfs_ci <- 
  map(
    .x = list(`95` = irfs_95, `68` = irfs_68),
    .f = function(irf_lvl) {
      map(
        .x = list(lower = irf_lvl$Lower[[impulse_name]], upper = irf_lvl$Upper[[impulse_name]]),
        .f = ~ .x |> 
          as_tibble() |> 
          mutate(horizon = row_number() - 1) |> 
          pivot_longer(cols = -horizon, values_to = "bound") |> 
          mutate(impulse = !!impulse_name)
      ) |> 
        list_rbind(names_to = "bound_type")
    }
  ) |> 
  list_rbind(names_to = "level") |> 
  pivot_wider(names_from = bound_type, values_from = bound)


df_irfs_ci <- 
  df_irfs_ci |> 
  mutate(
    name = factor(
      name,
      levels = !!levels,
      labels = !!labels),
    level = factor(level, levels = c(68, 95), labels = c("68%", "95%"))
  )

p_var <- 
  ggplot() +
  geom_ribbon(
    data = df_irfs_ci |> filter(level == "68%"),
    mapping = aes(x = horizon,
                  ymin = lower, ymax = upper,
                  fill = level),
    alpha = .3
  ) +
  geom_line(
    data = df_irfs,
    mapping = aes(x = horizon, y = value),
    colour = "#009E73"
  ) +
  geom_hline(yintercept = 0, colour = "#444444") +
  labs(x = "Horizon", y = NULL) +
  scale_fill_manual(
    "C.I. level", 
    values = c("68%" = "#009E73", "95%" = "#b2df8a"),
    guide = "none") +
  facet_wrap(~name, scales = "free_y", ncol = 4) +
  theme_paper() +
  coord_cartesian(xlim = c(0, 20))

p_var

if (1 == 0) {
  # Save the plot to PDF using pdflatex
  library(tikzDevice)
  library(lemon)
  p_var <- 
    ggplot() +
    geom_ribbon(
      data = df_irfs_ci |> 
        filter(level == "68%") |> 
        mutate(level = str_replace(level, "\\%", "\\\\%")),
      mapping = aes(x = horizon,
                    ymin = lower, ymax = upper,
                    fill = level),
      alpha = .2
    ) +
    geom_line(
      data = df_irfs,
      mapping = aes(x = horizon, y = value),
      colour = "#009E73"
    ) +
    geom_hline(yintercept = 0, colour = "#444444") +
    labs(x = "Horizon", y = NULL) +
    scale_fill_manual(
      "C.I. level", 
      values = c("68\\%" = "gray10", "95\\%" = "gray60"),
      guide = "none") +
    facet_rep_wrap(~name, scales = "free_y", repeat.tick.labels = TRUE, ncol = 4) +
    theme_paper() +
    coord_cartesian(xlim = c(0, 20))
  
  ggplot2_to_pdf(
    plot = p_var,
    path = "../../figs/", 
    filename = "fig_var",
    width = 10,
    height = 5.5)
  
}
