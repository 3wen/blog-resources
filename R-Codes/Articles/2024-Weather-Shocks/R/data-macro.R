# Macroeconomic Data----


## Load Data----

# We have downloaded these series from the the Banco Central De Reserva Del Perú website.
# Source: https://estadisticas.bcrp.gob.pe/estadisticas/series/

library(tidyverse)
library(labelled)

peru_macro <- 
  readxl::read_excel(
    path = "../data/raw/Macro/data_macro_peru.xlsx",
    skip = 1,
    na = "n.d."
  ) |> 
  mutate(
    month = case_when(
      str_detect(date, "^Ene") ~ 1,
      str_detect(date, "^Feb") ~ 2,
      str_detect(date, "^Mar") ~ 3,
      str_detect(date, "^Abr") ~ 4,
      str_detect(date, "^May") ~ 5,
      str_detect(date, "^Jun") ~ 6,
      str_detect(date, "^Jul") ~ 7,
      str_detect(date, "^Ago") ~ 8,
      str_detect(date, "^Sep") ~ 9,
      str_detect(date, "^Oct") ~ 10,
      str_detect(date, "^Nov") ~ 11,
      str_detect(date, "^Dic") ~ 12
    ),
    year = as.numeric(str_sub(date, -2)) + 2000
  ) |> 
  mutate(date = lubridate::ym(str_c(year, month, sep = "-")))


# Let us rename the columns :
peru_macro <- 
  peru_macro |> 
  select(
    date,
    yy_raw  = PN01773AM, # GDP
    rer_raw = PN01259PM, # Real Exchange Rate
    x_raw   = PN01461BM, # Exports
    r_raw   = PN07819NM, # Interest rate
    pi_raw  = PN01270PM, # CPI
    pia_raw = PN01336PM, # CPI: food
    ya_raw  = PN01755AM, # Agricultural GDP
    ind_prod_raw = PN02079AM # Manufacturing production
  )

## Helper Functions----

# Access to hp_filter_trend() and adj_season_X13()
source("../weatherperu/R/detrending.R")


## Data Pre-Processing----

### GDP----
peru_gdp <- 
  peru_macro |> 
  select(date, yy_raw) |> 
  filter(date >= "2003-01-01") |> 
  mutate(
    # Remove seasonality
    yy_sa  = adj_season_X13(yy_raw, ymd("2003-01-01")),
    # Extract trend
    yy_trend = hp_filter_trend(yy_sa, freq = 14400),
    # Percentage dev. from trend
    y = 100 * log(yy_sa / yy_trend)
  )

###  Agricultural GDP----
peru_ya <- 
  peru_macro |> 
  select(date, ya_raw) |> 
  filter(date >= "2003-01-01") |> 
  mutate(
    # Remove seasonality
    ya_sa  = adj_season_X13(ya_raw, ymd("2003-01-01")),
    # Extract trend
    ya_trend = hp_filter_trend(ya_sa, freq = 14400),
    # Percentage dev. from trend
    ya = 100 * log(ya_sa / ya_trend)
  )


peru_ya |> 
  pull(ya_trend)

### Real Exchange Rate----
peru_rer <- 
  peru_macro |> 
  select(date, rer_raw) |> 
  filter(date >= "2001-01-01") |> 
  mutate(
    # Remove seasonality
    rer_sa = adj_season_X13(rer_raw, ymd("2001-01-01")),
    rer = rer_sa / 100
  ) |> 
  mutate(
    rer_hp_trend = hp_filter_trend(rer_raw, freq = 14400),
    rer_hp = rer_raw - rer_hp_trend,
    rer_trend = hp_filter_trend(rer_sa, freq = 14400),
    rer_dt_sa = 100*log(rer_sa / rer_trend)
  ) |> 
  select(-rer_hp_trend, -rer_trend)

### Exports----
peru_x <- 
  peru_macro |> 
  select(date, x_raw) |> 
  filter(date >= "2001-01-01") |> 
  mutate(
    # Remove seasonality
    x = adj_season_X13(100 + x_raw, ymd("2000-12-01")),
    x = x / 100
  )

### Interest rate----
peru_r <- 
  peru_macro |> 
  select(date, r_raw) |> 
  filter(date >= "2001-01-01") |> 
  rename(r = r_raw) |> 
  mutate(
    r_hp_trend = hp_filter_trend(r, freq = 14400),
    r_hp = r - r_hp_trend
  ) |> 
  select(-r_hp_trend)


### Consumer Price Index-----
peru_cpi <- 
  peru_macro |> 
  select(date, pi_raw) |> 
  mutate(
    # Remove seasonality
    pi_sa = adj_season_X13(pi_raw, ymd("2000-12-01")),
    # Log difference
    pi = c(NA, 100 * diff(log(pi_sa)))
  )


### Consumer Price Index: food----
peru_cpia <- 
  peru_macro |> 
  select(date, pia_raw) |> 
  mutate(
    # Remove seasonality
    pia_sa = adj_season_X13(pia_raw, ymd("2000-12-01")),
    # Log difference
    pia = c(NA, 100 * diff(log(pia_sa)))
  )


### Manufacturing Production----
peru_ind_prod <- 
  peru_macro |> 
  select(date, ind_prod_raw) |> 
  filter(date >= "2001-01-01") |> 
  mutate(
    # Remove seasonality
    ind_prod_sa  = adj_season_X13(ind_prod_raw, ymd("2001-01-01")),
    # Extract trend
    ind_prod_trend = hp_filter_trend(ind_prod_sa, freq = 14400),
    # Percentage dev. from trend
    ind_prod = 100 * log(ind_prod_raw / ind_prod_trend)
  )

### International commodity prices----

library(readxl)
# We have downloaded these series from the IMF Primary Commodity Price System website.  
# Source: https://data.imf.org/?sk=471dddf8-d8a7-499a-81ba-5b332c01f8b9
int_prices <- read_excel(
  path = "../data/raw/Macro/IMF_DATA.xls",
  sheet = "SELECTED",
  col_types = "text") |> 
  mutate(date = lubridate::ymd(str_c(YEAR,  "-", MONTH, "-01"))) |> 
  rename(
    "FPI"           = "FPI_PFOOD", 
    "FERTILIZER"    = "FERTILIZER_PFERT",	
    "IndexOIL"      = "IndexOIL_POILAPSP",
    "PriceOIL"      = "PriceOIL_POILAPSP", 
    "CORN"          = "CORN_PMAIZMT", 
    "ARROZ CÁSCARA" = "RICE_PRICENPQ", 
    "TRIGO"         = "WHEAT_PWHEAMT"
  ) |> 
  mutate(CORN2 = CORN, 
         FPI2 = FPI) |> 
  select(-c(MONTH, YEAR, CPI_Peru_IMF, CPI_US_IMF, IndexOIL, PriceOIL)) |> 
  pivot_longer(cols = !date, names_to = "product", values_to = "price_int") |> 
  mutate(
    product = case_when(
      product == "FPI"  ~ "PAPA", 
      product == "FPI2" ~ "YUCA", 
      product == "CORN" ~ "MAÍZ AMILÁCEO", 
      product == "CORN2"~ "MAÍZ AMARILLO DURO", 
      TRUE ~ product
    ), 
    product_eng = case_when(
      product == "PAPA" ~ "Potato",
      product == "YUCA" ~ "Cassava",
      product == "ARROZ CÁSCARA" ~ "Rice",
      product == "MAÍZ AMARILLO DURO" ~ "Dent corn"
    ),
    price_int = as.numeric(price_int)
  ) |> 
  arrange(product, date) |> 
  group_by(product) |> 
  mutate(price_int_inf = price_int / lag(price_int) - 1) |> 
  ungroup() 


# Add labels to the columns
int_prices <- 
  int_prices |> 
  labelled::set_variable_labels(
    date = "Date",
    price_int = "International commodity price",
    price_int_inf = "Int. commodity inflation rate"
  )

dir.create("../data/output/macro/")
save(int_prices, file = "../data/output/macro/df_int_prices.rda")

## Merging the Data----

# Note: GDP data From January 2003 (2003M1) to December 2015 (2015M12).
df_macro <- 
  peru_gdp |> 
  select(date, y) |> 
  full_join(
    peru_ya |> select(date, ya),
    by = "date"
  ) |> 
  full_join(
    peru_rer |> select(date, rer, rer_hp, rer_dt_sa),
    by = "date"
  ) |> 
  full_join(
    peru_r |> select(date, r, r_hp),
    by = "date"
  ) |> 
  full_join(
    peru_x |> select(date, x),
    by = "date"
  ) |> 
  full_join(
    peru_cpi |> select(date, pi),
    by = "date"
  ) |>
  full_join(
    peru_cpia |> select(date, pia),
    by = "date"
  ) |>
  full_join(
    peru_ind_prod |> select(date, ind_prod),
    by = "date"
  ) |> 
  arrange(date) |> 
  filter(date >= ymd("2001-01-01"))


# Add labels to the columns
df_macro <- 
  df_macro |> 
  labelled::set_variable_labels(
    date = "Date",
    rer_dt_sa = "Real exchange rate",
    rer_hp = "Real exchange rate",
    x  = "Exports",
    pi = "Inflation rate (pp)",
    pia = "Food inflation rate (pp)",
    y = "GDP (pp)",
    ya = "Agricultural GDP (pp)",
    r = "Interest rate (pp)",
    r_hp = "Interest rate (pp)",
    ind_prod = "Manufacturing Prod. (pp)"
  )

# Save:
save(df_macro, file = "../data/output/macro/df_macro.rda")
