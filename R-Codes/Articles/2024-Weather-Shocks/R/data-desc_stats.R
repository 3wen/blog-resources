library(tidyverse)
library(ggpattern)

# ggplot2 theme:
source("../weatherperu/R/utils.R")
# data used in the local projection estimations
load("../data/output/df_lp.rda")
# weather data
load("../data/output/weather/weather_regions_df.rda")

## Crops----

crops <- c("Rice", "Dent corn", "Potato", "Cassava")

### Table----

# Monthly agricultural production (in Tons), per region
df |>
  mutate(product_eng = factor(product_eng, levels = crops)) |> 
  group_by(product_eng) |> 
  summarise(
    prod_mean = mean(y_new),
    prod_median = median(y_new),
    prod_sd = sd(y_new),
    prod_min = min(y_new),
    prod_max = max(y_new),
    nb_regions = length(unique(region)),
    nb_obs = n()
  ) |> 
  knitr::kable("markdown", digits = 0, format.args = list(big.mark = ","))

### National production----

# National monthly crop production for selected cultures (in tons)
p_nat_prod <- 
  ggplot(
  data = df |> 
    group_by(product_eng, date) |> 
    summarise(
      nat_prod = sum(y_new),
      .groups = "drop"
    ) |> 
    mutate(
      product_eng = factor(
        product_eng, 
        levels = c("Cassava", "Dent corn", "Potato", "Rice"),
        labels = c("Cassava", "Maize", "Potato", "Rice")
      )
    )
) +
  geom_line(
    mapping = aes(x = date, y = nat_prod),
    colour = "#1f78b4",
    linewidth = 1
  ) +
  facet_wrap(~product_eng, scales = "free_y", ncol = 2) +
  labs(x = NULL, y=  "Aggregate Production (tons)") +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  theme_paper()

p_nat_prod

if (1 == 0) {
  # To save the graph in PDF, using pdflatex
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_nat_prod,
    path = "../../figs/", 
    filename = "fig_nat_prod",
    width = 6,
    height = 4)
}


### National production by month and geo----

# Crop production by months and natural regions (in tons)
p_regional_monthly_prod <- 
  ggplot(
  data = df |> 
    group_by(product_eng, year, month) |> 
    # Average each month at the national level
    summarise(
      prod_nat_costa = sum(y_new * share_costa),
      prod_nat_selva = sum(y_new * share_selva),
      prod_nat_sierra = sum(y_new * share_sierra),
      .groups = "drop"
    ) |> 
    pivot_longer(
      cols = c(prod_nat_costa, prod_nat_selva, prod_nat_sierra),
      names_to = "geo",
      values_to = "monthly_prod_geo"
    ) |> 
    # Average in each region type for each calendar month
    group_by(product_eng, month, geo) |> 
    summarise(
      monthly_prod_geo = mean(monthly_prod_geo),
      .groups = "drop"
    ) |> 
    mutate(
      product_eng = factor(
        product_eng, 
        levels = c("Cassava", "Dent corn", "Potato", "Rice"),
        labels = c("Cassava", "Maize", "Potato", "Rice")
      ),
      geo = factor(
        geo,
        levels = c("prod_nat_costa", "prod_nat_selva", "prod_nat_sierra"),
        labels = c("Coast", "Forest", "Highlands")
      )
    ),
  mapping = aes(
    x = month, y = monthly_prod_geo, 
    colour = geo, linetype = geo
  )
) +
  geom_line(
    linewidth = 1
  ) +
  facet_wrap(~product_eng, scales = "free") +
  labs(x = NULL, y = "Average Production (tons)") +
  scale_colour_manual(
    NULL,
    values = c(
      "Coast" = "#56B4E9", "Forest" = "#009E73", "Highlands" = "#E69F00"
    )
  ) +
  scale_linetype_discrete(NULL) +
  scale_x_continuous(breaks= 1:12, labels = month.abb) +
  scale_y_continuous(labels = scales::number_format(big.mark = ",")) +
  theme_paper()

p_regional_monthly_prod

if (1 == 0) {
  # To save the graph using pdflatex
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_regional_monthly_prod, 
    path = "../../figs/", 
    filename = "fig_regional_monthly_prod",
    width = 8,
    height = 4
  )
}

### Maps----


map_peru <- sf::st_read("../data/raw/shapefile_peru/departamentos/")
map_peru <-
  rmapshaper::ms_simplify(input = as(map_peru, 'Spatial')) |>
  sf::st_as_sf()

#### Grid----

map_peru_grid <- sf::st_read(
  str_c(
    "../data/raw/shapefile_peru/grid/",
    "cuadro de empalme oficial 100k ign peru geogpsperu/")
)


if (1 == 0) {
  # To save the graph using pdflatex
  library(tikzDevice)
  
  p_map_peru_grid <- 
    ggplot(data = map_peru_grid) + 
    geom_sf(fill = "#009E73", alpha = .3, colour = "white") +
    geom_sf(data = map_peru, fill = NA, colour = "#E69F00", linewidth = .8) +
    theme_map_paper_dark()
}


#### Share agri. land----
# Share of agricultural area in the cell, for each cell of the grid
load("../data/output/land/map_peru_grid_agri.rda")


p_share_agri_land <- 
  ggplot() +
  geom_sf(
    data = map_peru_grid_agri |> 
      mutate(share_cropland = percent_cropland / 100), 
    mapping = aes(fill = share_cropland),
    colour = "grey"
  ) +
  geom_sf(data = map_peru, fill = NA) +
  scale_fill_gradient2(
    "Share of\nagricultural\nland", 
    low = "white", high = "#61C250",
    labels = scales::label_percent()
  ) +
  theme_map_paper()

p_share_agri_land

if (1 == 0) {
  # To save the graph using pdflatex
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_share_agri_land +
      scale_fill_gradient2(
        "Share of\nagricultural\nland", 
        low = "white", high = "#61C250",
        labels = scales::percent_format(suffix = "\\%")
      ),
    path = "./", 
    filename = "fig_share_agri_land",
    width = 6,
    height = 6)
  system("mv fig_share_agri_land.pdf ../../figs/")
}

# The relative share of each cell within each region

get_shares_i <- function(i, weather_variables) {
  map_region_i <- map_peru[i,]
  tmp <- 
    sf::st_intersection(map_peru_grid_agri, map_region_i) |> 
    # Get the area of the intersection between the polygon of the current
    # region and each grid cell that intersects it
    dplyr::mutate(area_cell_intersect = sf::st_area(geometry)) |> 
    dplyr::rename(grid_id = i) |> 
    # Get the weather data for the corresponding grid cells
    # Remove the unit in the area (squared metres)
    dplyr::mutate(
      area_cell_intersect = units::drop_units(area_cell_intersect)
    ) |> 
    # Compute the weights to attribute to each grid cell
    dplyr::mutate(
      w_cropland = cropland / sum(cropland),
      w_area     = area_cell_intersect / sum(area_cell_intersect),
      w          = w_cropland * w_area,
      w          = w / sum(w)
    )
}

cropland_regions_df <- 
  map(
    .x = seq_len(nrow(map_peru)),
    .f = ~get_shares_i(.x), 
    .progress = TRUE
  )

cropland_regions_df <- 
  cropland_regions_df |> bind_rows()

p_regional_share_agri_land <- 
  ggplot() +
  geom_sf(
    data = cropland_regions_df,
    mapping = aes(fill = w),
    colour = "grey"
  ) +
  geom_sf(data = map_peru, fill = NA) +
  scale_fill_gradient2(
    "Regional\nshare of\nagricultural\nland", 
    low = "white", high = "#61C250",
    labels = scales::label_percent()
  ) +
  theme_map_paper()

p_regional_share_agri_land

#### Regions used----

# For each region of the map: is it included or not in the analysis?
df_plot_map_regions <- 
  map_peru |> 
  left_join(
    df |> 
      select(product_eng, region) |> unique() |> 
      mutate(used = TRUE) |> 
      pivot_wider(names_from = product_eng, values_from = used),
    by = c("DEPARTAMEN" = "region")
  )

# Included regions
ggplot(
  data = df_plot_map_regions |> 
    pivot_longer(cols = !!crops, values_to = "included") |> 
    mutate(
      included = replace_na(included, FALSE),
      included = factor(
        included, levels = c(TRUE, FALSE),
        labels = c("Yes", "No")
      )
    )
) +
  geom_sf(mapping = aes(fill = included)) +
  facet_wrap(~name) +
  theme_paper() +
  scale_fill_manual(
    NULL,
    values = c("Yes" = "#009E73", "No" = "#949698"),
    labels = c("Yes" = "Region included", "No" = "Region discarded")) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )


load("../data/output/dataset_2001_2015.rda")

# Let us compare the previous map with the annual production.
# Some regions are discarded because the production is 0 for consecutive
# months.
# This is especially the case for rice production, in regions like
# Lambayeque, La Libertad, or Arequipa.

prod_reg <- 
  data_total |> 
  group_by(region, product_eng) |> 
  summarise(
    total_production = sum(Value_prod, na.rm = T),
    .groups = "drop"
  ) |> 
  unique() |> 
  group_by(product_eng) |> 
  mutate(total_culture = sum(total_production)) |> 
  ungroup() |> 
  mutate(share = total_production / total_culture)


map_production_regions <- NULL
for (i in 1:length(crops)) {
  culture <- as.character(crops[i])
  map_peru_tmp <- map_peru |> 
    left_join(
      prod_reg |> 
        filter(product_eng == !!culture), 
      by = c("DEPARTAMEN" = "region")
    ) |> 
    mutate(product_eng = !!culture)
  map_production_regions <- bind_rows(map_production_regions, map_peru_tmp)
}

ggplot(
  data = map_production_regions
) +
  geom_sf(
    mapping = aes(fill = share, group = DEPARTAMEN)
  ) +
  scale_fill_gradient2(
    "Share", low = "#FFC107", mid = "white", high = "#009E73",
    labels = scales::percent_format()
  ) +
  facet_wrap(~product_eng) +
  theme_map_paper()

included_regions <- 
  df_plot_map_regions |> 
  pivot_longer(cols = !!crops, values_to = "included") |> 
  as_tibble() |> 
  select(IDDPTO, name, included) |> 
  mutate(included = replace_na(included, FALSE)) |> 
  rename(product_eng = name)

p_regions_use_production <- 
  ggplot(
    data = map_production_regions |> 
      left_join(included_regions, by = c("IDDPTO", "product_eng")) |> 
      mutate(
        included = factor(
          included, 
          levels = c(TRUE, FALSE), 
          labels = c("Yes", "No")
        )
      ) |> 
      mutate(
        product_eng = factor(
          product_eng,
          levels = c("Cassava", "Dent corn", "Potato", "Rice"),
          labels = c("Cassava", "Maize", "Potato", "Rice")
        )
      )
  ) +
  geom_sf_pattern(
    mapping = aes(
      pattern = included,
      pattern_type = included, 
      fill = share
    ),
    pattern_fill = "white",
    pattern_alpha = .8
  ) +
  # scale_pattern_type_discrete("Included", values = c("Yes" = "none", "No" = "stripe")) +
  scale_pattern_manual("Included", values = c("No" = "stripe", "Yes" = "none")) +
  scale_pattern_type_manual("Included", values=c(NA, NA)) +
  scale_fill_gradient2(
    "Share", low = "#FFC107", mid = "white", high = "#009E73",
    labels = scales::percent_format()
  ) +
  facet_wrap(~product_eng) +
  theme_map_paper()

p_regions_use_production

if (1 == 0) {
  # To save the graph in PDF using pdflatex
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_regions_use_production + 
      scale_fill_gradient2(
        "Share", low = "#FFC107", mid = "white", high = "#009E73",
        labels = scales::percent_format(suffix = "\\%")
      ),
    path = "./", 
    filename = "fig_regions_use_production",
    width = 7,
    height = 7)
  
  system("mv fig_regions_use_production.pdf ../../figs/")
}

## Natural regions----

# Map of the natural regions 
map_regiones_naturales <-  sf::st_read(
  str_c(
    "../data/raw/shapefile_peru/regiones_naturales/",
    "region natural_geogpsperu_JuanPabloSuyoPomalia.geojson"
  )
)

map_regiones_naturales <-
  rmapshaper::ms_simplify(input = as(map_regiones_naturales, 'Spatial')) |> 
  sf::st_as_sf() |> 
  mutate(
    Natural_region = case_when(
      Nm_RegNat == "Costa" ~ "Coast",
      Nm_RegNat == "Selva" ~ "Forest",
      Nm_RegNat == "Sierra" ~ "Highlands",
    )
  )

# Visual representation 
cols <- c("Coast" = "#56B4E9", "Forest" = "#009E73", "Highlands" = "#E69F00")

# Natural regions in Peru
p_nat_reg_admin_gridded <- 
  ggplot(data = map_regiones_naturales) +
  geom_sf(mapping = aes(fill = Natural_region), lwd = 0) +
  scale_fill_manual(values = cols, name = "Natural region") +
  geom_sf(data = map_peru, fill = NA) +
  geom_sf(data = map_peru_grid_agri, fill = NA, lwd = 0.25) +
  theme_map_paper()


if (1 == 0) {
  # To save the graph in PDF using pdflatex
  library(tikzDevice)
  ggplot2_to_pdf(
    plot = p_nat_reg_admin_gridded,
    path = "../../figs/", 
    filename = "fig_nat_reg_admin_gridded",
    width = 7,
    height = 7)
}

## Correlations----
name_weather_variables <- 
  weather_regions_df |> 
  select(where(is.numeric)) |> 
  select(-year, -month) |> 
  colnames()

# Correlations between the weather variables and the production variable
df |>
  select(product_eng, y, !!!syms(name_weather_variables)) |>
  # mutate(y_lead = dplyr::lead(y, n=1)) |> 
  na.omit() |>
  nest(.by = product_eng) |> 
  mutate(
    correlation = map(
      data, ~cor(.x) |> 
        data.frame() |> 
        as_tibble(rownames = "var")
    )
  ) |> 
  select(-data) |> 
  unnest(correlation) |> 
  select(product_eng, var, y) |> 
  pivot_wider(names_from = product_eng, values_from = y) |> 
  mutate(across(where(is.numeric), ~round(.x, 2))) |> 
  print(n = 35)

## Local Projections data----

# Number of observation in each region and crops left
df |> 
  count(product_eng, region_id) |> 
  arrange(n)

# Number of regions for each crop
df |> 
  group_by(product_eng) |> 
  summarise(nb_regions = length(unique(region_id)))

df |> 
  group_by(product_eng, region_id) |> 
  summarise()

df$y_new |> head()


for (i_crop in 1:length(crops)) {
  current_crop <- crops[i_crop]
  # The series in each region for the current crop
  p_crop_lp <- 
    ggplot(
    data = df |> filter(product_eng == !!current_crop),
    mapping = aes(x = date, y = y)
  ) +
    geom_line() +
    facet_wrap(~region, scales = "free_y") +
    labs(
      title = current_crop, x = NULL,
      y = "Percent deviation from monthly trend") +
    theme_paper()
  print(p_crop_lp)
}


library(gtsummary)

df |>
  tbl_summary(
    include = c(
      "product_eng",
      # Production
      "y_new", "y",
      # Weather
      "temp_max_dev", "perc_gamma_precip",
      # Control
      "rer", "r", "pi", "ind_prod",
      # Type of region
      "share_costa", "share_selva", "share_sierra"
    ),
    by = product_eng,
    type = all_continuous() ~ "continuous2",
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})"),
      all_categorical() ~ "{n} ({p}%)"),
    digits = list(
      all_continuous() ~ 2,
      all_categorical() ~ 0
    )
  ) |> 
  add_overall(col_label = "Whole sample") |> 
  modify_header(label ~ "**Variable**") |> 
  modify_spanning_header(
    c("stat_1", "stat_2", "stat_3", "stat_4") ~ "**Crop**"
  ) |> 
  add_stat_label(
    label = list(
      all_continuous() ~ c("Mean (SD)", "Median (IQR)"),
      all_categorical() ~ "n (%)"
    )
  )
