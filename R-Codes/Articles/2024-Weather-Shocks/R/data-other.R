library(tidyverse)
library(lubridate)
library(sf)
library(rmapshaper)
library(raster)
library(stars)
library(readxl)

# Natural regions----

## Load ----

sf::sf_use_s2(FALSE)

# Map of the administrative regions 
map_peru <- sf::st_read("../data/raw/shapefile_peru/departamentos/", quiet = T)

map_peru <-
  rmapshaper::ms_simplify(input = as(map_peru, 'Spatial')) |>
  st_as_sf()

# Map of the natural regions 
# Source: Geo GPS Peru website
# https://www.geogpsperu.com/2019/11/mapa-de-regiones-naturales-costa-sierra.html
map_regiones_naturales <-  sf::st_read(
  str_c("../data/raw/shapefile_peru/regiones_naturales/",
        "region natural_geogpsperu_JuanPabloSuyoPomalia.geojson"
  ),
   quiet = TRUE
)

map_regiones_naturales <-
  rmapshaper::ms_simplify(input = as(map_regiones_naturales, 'Spatial')) |>
  st_as_sf()

# Visual representation 
ggplot(data = map_regiones_naturales) +
  geom_sf(mapping = aes(fill = Nm_RegNat))


## Share of natural region in each department ----

natural_region_dep <- vector(mode = "list", length = nrow(map_peru))


for (i in 1:nrow(map_peru)) {
  natural_region_dep_i <- 
    st_intersection(map_peru[i,],  map_regiones_naturales) |> 
    dplyr::mutate(area_cell_intersect = st_area(geometry)) |> 
    mutate(area_cell_sqkm = units::drop_units(area_cell_intersect)*10^-6) |> 
    mutate(
      area = sum(area_cell_sqkm),
      area_pct = area_cell_sqkm/area) |> 
    dplyr::select(region = DEPARTAMEN, natural_reg = Nm_RegNat, area_pct) |> 
    as_tibble() |> 
    dplyr::select(-geometry)
  
  natural_region_dep[[i]] <- natural_region_dep_i
}


natural_region_dep <- 
  dplyr::bind_rows(natural_region_dep) |> 
  dplyr::mutate(natural_reg = str_c("share_", str_to_lower(natural_reg))) |> 
  tidyr::pivot_wider(
    names_from = natural_reg, 
    values_from = area_pct, 
    values_fill = 0
  )

natural_region_dep |>
  dplyr::mutate(test = share_sierra + share_selva + share_costa)


# Add labels to the columns
natural_region_dep <- 
  natural_region_dep |> 
  labelled::set_variable_labels(
    region = "Name of the region",
    share_costa = "Share of coastal areas in the region",
    share_selva = "Share of forest areas in the region",
    share_sierra = "Share of highland areas in the region"
  )


save(natural_region_dep, file =  "../data/output/natural_region_dep.rda")

map_peru_nat_regions <- NULL

# nat_level <- "share_sierra"
for (nat_level in c("share_sierra", "share_selva", "share_costa")) {
  map_peru_nat_reg_tmp <- 
    map_peru |> 
    dplyr::left_join(
      natural_region_dep |> 
        tidyr::pivot_longer(
          cols = c(share_sierra, share_selva, share_costa),
          names_to = "natural_region", values_to = "share_natural"
        ) |> 
        filter(natural_region == !!nat_level),
      by = c("DEPARTAMEN" = "region")
    )
  map_peru_nat_regions <- dplyr::bind_rows(
    map_peru_nat_regions, 
    map_peru_nat_reg_tmp
  )
}


source("../weatherperu/R/utils.R")
p <- 
  ggplot(
    data = map_peru_nat_regions |> 
      mutate(
        natural_region = factor(
          natural_region,
          levels = c("share_costa", "share_selva", "share_sierra"),
          labels = c("Coast", "Forest", "Highlands")
        )
      )
    ) +
  geom_sf(mapping = aes(fill = share_natural, group = DEPARTAMEN)) +
  scale_fill_gradient2(
    "Share of\neach natural\ntype", 
    low = "#FFC107", mid = "white", high = "gray15",
    breaks = .25*0:4,
    labels = scales::percent(.25*0:4)
  ) +
  facet_wrap(~natural_region) +
  theme_map_paper()

p



# ENSO----
# El Niño–Southern Oscillation
# Oceanic Niño Index
# Source: https://ggweather.com/enso/oni.htm

ONI_temp <- read_excel(
  path = "../data/raw/weather/ONI.xlsx", 
  sheet = "ONI", 
  col_types = "text") |> 
  pivot_longer(cols = -c(Year), names_to = "month") |> 
  mutate(
    elnino = ifelse(as.numeric(value) >  0.49, yes = 1, no = 0),
    lanina = ifelse(as.numeric(value) < -0.49, yes = 1, no = 0)) |> 
  filter(Year %in% c(1986: 2016)) |> 
  mutate(
    date = lubridate::ymd(str_c(Year,  "-", month, "-01")), 
    State = case_when(
      elnino == 1 ~ "El Niño", 
      lanina == 1 ~ "La Niña", 
      (elnino == 0 & lanina == 0) ~ "Normal",
      TRUE ~ "NA"
    )) |> 
  mutate(
    date_start = ifelse(State != lag(State), yes = 1, no = 0), 
    date_end   = ifelse(State != lead(State), yes = 1, no = 0)) |> 
  rename(ONI = value) |> 
  mutate(ONI = as.numeric(ONI))

ONI_temp$date_start[1] <- 1
ONI_temp$date_end[nrow(ONI_temp)] <- 1

# Add labels to the columns
ONI_temp <- 
  ONI_temp |> 
  labelled::set_variable_labels(
    Year = "Year",
    month = "Month",
    ONI = "Oceanic Niño Index",
    elnino = "Is El-Niño Event",
    lanina = "Is La-Niña Event",
    date = "Date",
    State = "ENSO State",
    date_start = "Is event start month",
    date_end = "Is event end month"
  )



save(ONI_temp, file = "../data/output/weather/ONI_temp.rda")
