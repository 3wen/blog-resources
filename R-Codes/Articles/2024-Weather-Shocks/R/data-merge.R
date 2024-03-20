library(tidyverse)
library(cli)

# Load Intermediate Data----

## Weather----
load("../data/output/weather/weather_regions_df.rda")
## Agriculture----
load("../data/output/minagri/dataset_agri_2001_2015.rda")
## Macro----
load("../data/output/macro/df_macro.rda")
## Commodity prices----
load("../data/output/macro/df_int_prices.rda")
## Natural Regions----
load("../data/output/natural_region_dep.rda")
## ENSO----
load("../data/output/weather/ONI_temp.rda")

# Merge----
# Compute ENSO-dependant weather anomalies 

Weather <- weather_regions_df |> 
  # Add ENSO data
  left_join(
    ONI_temp |> 
      mutate(
        Year = as.numeric(Year),
        month = as.numeric(month)
      ) |> 
      rename(
        enso_start = date_start,
        enso_end = date_end
      ),
    by = c(
      "year" = "Year",
      "month" = "month"
    )
  ) |> 
  group_by(IDDPTO, month, State) |> 
  mutate( 
    temp_min_dev_ENSO   = temp_min - mean(temp_min),
    temp_max_dev_ENSO   = temp_max - mean(temp_max),
    temp_mean_dev_ENSO  = temp_mean - mean(temp_mean),
    precip_sum_dev_ENSO = precip_sum - mean(precip_sum))|> 
  ungroup() |> 
  labelled::set_variable_labels(
    temp_min_dev_ENSO   = "Deviation of Min. Temperature from ENSO Normals",
    temp_max_dev_ENSO   = "Deviation of Max. Temperature from ENSO Normals",
    temp_mean_dev_ENSO  = "Deviation of Mean Temperature from ENSO Normals",
    precip_sum_dev_ENSO = "Deviation of Total Rainfall from ENSO Normals",
  )


data_total <- 
  data_total |> 
  # Add weather data and ENSO 
  left_join(
    #weather_regions_df |> 
    Weather |> 
      dplyr::select(-IDDPTO),
    by = c(
      "year" = "year",
      "month" = "month",
      "region" = "DEPARTAMEN", 
      "date" = "date"
      )
  ) |> 
  # Add macroeconomic data
  left_join(
    df_macro |> rename(gdp = y),
    by = "date"
  ) |> 
  # Add commodity prices data
  left_join(
    int_prices,
    by =  c(
      "date", "product", "product_eng")
  ) |> 
  # Add share of each type of region
  left_join(
    natural_region_dep,
    by = "region"
  )


save(data_total, file = "../data/output/dataset_2001_2015.rda")
write_csv(data_total, file = "../data/output/dataset_2001_2015.csv")

# Dataset for the Local Projections----

# Recode region as a factor
data_total <- 
  data_total |> 
  mutate(region_id = factor(region_id))

# The crops we focus on
crops <- c("Rice", "Dent corn", "Potato", "Cassava")

# Some desc. stat checks:
# Number of observation in each region, for each crop
data_total |> 
  group_by(product_eng, region_id) |> 
  summarise(n = sum(Value_prod <= 0)) |> 
  arrange(desc(n))

# Corresponding names of crops between Spanish and English
data_total |> 
  dplyr::select(product, product_eng) |> 
  unique()

# Id associated to each region
data_total |> 
  dplyr::select(region, region_id) |> 
  unique()

# Load function in utils
source("../weatherperu/R/utils.R")

# Load detrending functions
source("../weatherperu/R/detrending.R")

## Detrending----

# Detrending the production data for each crop in each region:
# First, table with all combinations of crop x region
product_and_regions <- 
  data_total |> 
  filter(product_eng %in% crops) |> 
  dplyr::select(product_eng, region_id, region) |> 
  unique()

product_and_regions |> filter(region == "PUNO")

# Should the detrended value be transformed in log?
in_log <- FALSE

# Then detrending
df_detrended_production <- vector(mode = "list", length = nrow(product_and_regions))
cli_progress_bar(total = nrow(product_and_regions))
for(i in 1:nrow(product_and_regions)){
  df_detrended_production[[i]] <- detrend_production(
    df = data_total, 
    crop_name = product_and_regions$product_eng[i], 
    region_id = product_and_regions$region_id[i],
    in_log = in_log
  )
  cli_progress_update(set = i)
}
df_detrended_production <- bind_rows(df_detrended_production)


# The number of months with 0 values for ag. production
df_detrended_production |> 
  group_by(product_eng, region_id) |> 
  summarise(nb_0 = sum(y_new == 0)) |> 
  arrange(desc(nb_0))


if (in_log) {
  # Dealing with missing values: if the value is NaN (after the log of a negative 
  # value), we use a linear interpolation. However, if there are more than 3 
  # consecutive NaN values (which corresponds to months with 0 production),
  # we discard the couple region x product.
  df_detrended_production <- 
    df_detrended_production |> 
    nest(.by = c(product_eng, region_id)) |> 
    mutate(
      y_new_2 = map(
        data, ~imputeTS::na_interpolation(.x$y, maxgap = 3)
      )
    ) |> 
    mutate(
      remove = map(
        y_new_2, ~any(is.na(.x))
      )
    ) |> 
    unnest(remove) |> 
    filter(!remove) |> 
    select(-remove) |> 
    unnest(cols = c(data, y_new_2)) |> 
    mutate(
      flag = is.na(y),
      y = ifelse(is.na(y), yes = y_new_2, no = y)
    ) |> 
    select(-y_new_2)
  
  
  # Looking at the number of replaced values with this last interpolation:
  nb_interpolated_val <- 
    df_detrended_production |> 
    filter(flag) |> 
    count(product_eng, region_id) |> 
    arrange(desc(n))
  
  nb_interpolated_val
}


# Add the other characteristics to the detrended production data
df <- df_detrended_production |> 
  left_join(
    data_total,
    join_by(product_eng, region_id, month, date)
  )

## Missing values for the weather variables----

# Let us also impute missing values for the weather variables
weather_variables <- 
  weather_regions_df |> 
  select(where(is.numeric)) |> 
  select(-year, -month) |> 
  colnames()

df <- 
  df |> 
  mutate(
    across(
      .cols = !!weather_variables,
      .fns = ~ imputeTS::na_interpolation(.x, maxgap = 3)
    )
  )

# How many NAs left for those weather variables?
df |> 
  summarise(
    across(
      .cols = !!weather_variables,
      .fns = ~ sum(is.na(.x)),
      .names = "{.col}_nb_na"
    )
  ) |> 
  unlist()


# Add labels to the new columns
df <- 
  df |> 
  labelled::set_variable_labels(
    y_new = "Monthly Agricultural Production (tons)",
    y = "Monthly Agricultural Production (pct. deviation from monthly trend)"
  )


save(df, file = "../data/output/df_lp.rda")
