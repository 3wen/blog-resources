# Weather Data----

library(tidyverse)
library(rnoaa)
library(lubridate)
library(sf)
library(rmapshaper)
library(raster)
library(stars)

## Maps----

# Source: GEO GPS PERÙ
# https://www.geogpsperu.com/2014/03/base-de-datos-peru-shapefile-shp-minam.html
#
# Departmental boundaries:
# https://www.geogpsperu.com/2018/02/limite-departamental-politico-shapefile.html
# National boundaries:
# https://www.geogpsperu.com/2012/08/cuadro-de-empalme-de-la-cartografia.html

map_peru <- sf::st_read(
  "../data/raw/shapefile_peru/departamentos/", 
                        quiet = TRUE
)
map_peru <-
  rmapshaper::ms_simplify(input = as(map_peru, 'Spatial')) |>
  st_as_sf()

ggplot(data = map_peru) +
  geom_sf()

# Grid covering Peru
map_peru_grid <- sf::st_read(
  str_c(
    "../data/raw/shapefile_peru/grid/",
    "cuadro de empalme oficial 100k ign peru geogpsperu/")
)

ggplot(data = map_peru_grid) + 
  geom_sf(fill = "#00B0CA", alpha = .2, colour = "white")

# Both the grid and the administrative boundaries
ggplot(data = map_peru_grid) + 
  geom_sf(fill = "#00B0CA", alpha = .1, colour = "white") +
  geom_sf(data = map_peru, fill = NA)

## Land Use----

# Source: Copernicus
# https://land.copernicus.eu/global/products/lc
# Resolution: 100m
# Single layer

base_location <- "../data/raw/land_use/global_land_cover/"
end_location <- str_c(
  "_PROBAV_LC100_global_v3.0.1_2015-",
  "base_Crops-CoverFraction-layer_EPSG-4326.tif")

file <- str_c(base_location, "W080N00", end_location)
file_2 <- str_c(base_location, "W100N00", end_location)
file_3 <- str_c(base_location, "W080N20", end_location)


raster_land_1 <- raster(file)
raster_land_2 <- raster(file_2)
raster_land_3 <- raster(file_3)


load_cropped_raster <- FALSE # turn to TRUE to crop yourself
if (load_cropped_raster) {
  # Merge into a single raster:
  raster_land <- raster::merge(raster_land_1, raster_land_2, raster_land_3)
  
  # Focus on the bounding box of Peru:
  raster_land_crop <- crop(raster_land, map_peru)
  
  # Saving this raster for later use:
  raster::writeRaster(
    raster_land_crop,
    filename = str_c(
      "../data/output/land/global_land_cover/",
      "raster_land_2015_cropped.tif"
    )
  )
} else {
  # This file is huge. We will only look at small portions of it, iteratively.
  land <- stars::read_stars(
    "../data/output/land/global_land_cover/raster_land_2015_cropped.tif"
  )
}

# The numerical values associated with each tile of the TIF are the estimated 
# fraction of the 100$m^2$ covered with agricultural land.
names(land)[1] <- "percent_cropland"


# We extract the tiles from the land use raster intersecting with the cell,
# only keeping tiles within the boundaries. 
# Then, we compute the fraction of cropland. 
# The function returns a table with the value of i, the fraction of cropland,
# and the area of the cell within the boundaries (in squared metres).

#' Get the cover fractions of cropland for the ith cell of Peru's grid
#' 
#' @param i index of the cell in `map_peru_grid`
get_percent_cropland_cell <- function(i) {
  # Cell from the grid of Peru
  poly_cell <- map_peru_grid$geometry[i]
  # Part of that cell within the boundaries
  poly_cell_in_map <- st_intersection(poly_cell, map_peru)
  poly_cell_in_map_area <- st_area(poly_cell_in_map) |> sum()
  # Cropping land cover data
  land_crop_try <- try(land_crop <- st_crop(land, poly_cell) |> st_as_sf())
  if (inherits(land_crop_try, "try-error")) {
    new_bbox_border <- function(bbox){
      if (bbox[["xmin"]] < st_bbox(land)[["xmin"]]) {
        bbox[["xmin"]] <- st_bbox(land)[["xmin"]]
      }
      
      if (bbox[["xmax"]] > st_bbox(land)[["xmax"]]) {
        bbox[["xmax"]] <- st_bbox(land)[["xmax"]]
      }
      
      bbox
    }
    new_bbox_poly_cell <- new_bbox_border(st_bbox(poly_cell))
    land_crop <- 
      st_crop(land, new_bbox_poly_cell) |>
      st_as_sf()
  }
  # land cover data within the part of the grid cell within the boundaries
  land_within_cell <- st_intersection(land_crop, poly_cell_in_map)
  
  tibble(
    i = i, 
    percent_cropland = mean(land_within_cell$percent_cropland),
    area_cell = poly_cell_in_map_area
  )
}

if (1 == 0) {
  # This function just needs to be applied to each cell of the grid. 
  # It takes a few hours on a standard 2023 computer.
  # Might be parallelised with progressr...
  
  percent_cropland_cells <- vector(mode = "list", length = nrow(map_peru_grid))
  pb <- txtProgressBar(min = 0, max = nrow(map_peru_grid), style = 3)
  for (i in 1:nrow(map_peru_grid)) {
    percent_cropland_cells[[i]] <- get_percent_cropland_cell(i)
    setTxtProgressBar(pb, i)
  }
  percent_cropland_cells <- bind_rows(percent_cropland_cells)
  
  # Saving the object:
  save(
    percent_cropland_cells, 
    file = "../data/output/land/percent_cropland_cells.rda"
  )
} else {
  load("../data/output/land/percent_cropland_cells.rda")
}


# Let us associate these values to each cell of the grid:

map_peru_grid_agri <- map_peru_grid
map_peru_grid_agri$i <- 1:nrow(map_peru_grid_agri)

map_peru_grid_agri <- 
  map_peru_grid_agri |> 
  left_join(percent_cropland_cells)

# The area of the cell is expressed in squared meters. 
# Let us convert those values to squared kilometres. 
# Then, for each cell, let us compute the cropland area.

map_peru_grid_agri <- 
  map_peru_grid_agri |> 
  mutate(area_cell_sqkm = units::drop_units(area_cell)*10^-6) |> 
  mutate(cropland = (percent_cropland/100) * area_cell_sqkm)
save(map_peru_grid_agri, file = "../data/output/land/map_peru_grid_agri.rda")


# Peru's area is 1,285,216 km2. 
# The grid (some cells of which are partially outside the boundaries) area is:
scales::number(sum(map_peru_grid_agri$area_cell_sqkm), big.mark = ",")

# While this value makes sense, we get an odd value for the agricultural land. 
# According to the World Bank, agricultural lands correspond to roughly 
# 18% of total land in Peru. We only get 1.7%. Scale problem?

100 * sum(map_peru_grid_agri$cropland) / 
  sum(map_peru_grid_agri$area_cell_sqkm) # far from it...

# However, what we are interested in here is the *relative* cropland area. 
# Let us have a look at that.

p <- 
  ggplot() +
  geom_sf(
    data = map_peru_grid_agri,
    mapping = aes(fill = cropland), colour = "white"
  ) +
  geom_sf(data = map_peru, fill = NA) +
  scale_fill_gradient2(
    "Cropland (squared meters)",
    low = "white", high = "#61C250"
  )
p

# Even if the values do not add up to 18% of total land, the relative values 
# seem to be close to what can be observed on the Google Earth Engine.


## Weather Data----


### Rainfall----

# Source: CHIRPS v2.0 database
# Climate Hazards Center of the UC Santa Barbara
# https://www.chc.ucsb.edu/data/chirps
# daily information on rainfall on a 0.05° resolution satellite imagery, 
# from 1981 to present. 
N <- list.files(
  "../data/raw/weather/CHIRPS-2_0_global_daily_p25", 
  pattern = "\\.nc$", full.names = TRUE
)

# The files used are huge. Users are kindly asked to download these themsemves
if (1 == 0) {
  # Looping over the files (one per year)
  for (ii in 1:length(N)) {
    file <- N[ii]
    precip_sf_raster <- raster::raster(file)
    proj4string(precip_sf_raster) <- CRS("+init=EPSG:4326")
    
    precip_sf <- 
      stars::st_as_stars(precip_sf_raster) |> 
      sf::st_as_sf()
    
    sf::sf_use_s2(FALSE)
    precip_sf$geometry <-
      precip_sf$geometry |>
      s2::s2_rebuild() |>
      sf::st_as_sfc()
    
    
    precip_grille_i <- vector(mode = "list", length = nrow(map_peru_grid))
    pb <- txtProgressBar(min=0, max=nrow(map_peru_grid), style=3)
    for (k in 1:nrow(map_peru_grid)) {
      
      # Average precipitations in the k-th cell
      tmp <- suppressMessages(
        st_interpolate_aw(precip_sf, map_peru_grid[k,], extensive = FALSE)
      )
      # Each column of `tmp` gives the average for a given date
      tmp <- tibble(tmp)
      
      # Let us transform `tmp` so that each row gives the average precipitations 
      # in the k-th cell, for a specific date
      
      tmp_2 <- 
        tmp |> 
        tidyr::pivot_longer(cols = -c(geometry), names_to = "date_v") |> 
        dplyr::mutate(date = str_remove(date_v, "^X") |> lubridate::ymd()) |> 
        dplyr::select(-date_v, -geometry) |> 
        dplyr::mutate(grid_id = k)
      # Add the ID of the k-th cell
      precip_grille_i[[k]] <- tmp_2
      setTxtProgressBar(pb, k)
    }
    
    precip_grille <- 
      precip_grille_i |> bind_rows()
    
    file_name <- str_replace(file, "\\.nc$", ".rda")
    file_name <- str_replace(file_name, "data/raw/", "data/output/")
    
    save(precip_grille, file = file_name)
  }
  # The intermediate results can be loaded again, and merged to a single tibble:
  N <- list.files(
    "../data/output/weather/CHIRPS-2_0_global_daily_p25", 
    pattern = "days_p25\\.rda$", 
    full.names = TRUE
  )
  load_precip <- function(x) {load(x) ; precip_grille}
  precip_grille <- map(N, load_precip) |> list_rbind()
  
  
  # The resulting tibble can be saved:
  save(
    precip_grille,
    file = "../data/output/weather/CHIRPS-2_0_global_daily_p25/precip_grille.rda"
  )
} else {
  load("../data/output/weather/CHIRPS-2_0_global_daily_p25/precip_grille.rda")
}


### Temperatures----

# Source: PISCO temperature dataset version 1.1c (PISCOt)
# https://github.com/adrHuerta/PISCOt
# gridded product of maximum (tx) temperature and minimum (tn) temperature 
# at a spatial resolution of 0.1° for Peru. 
# It is available at daily (d), monthly (m), and annual (a) scales.
# Here: daily

#### Minimum Temperatures----

if (1 == 0) {
  # daily minimum temperatures
  tmin_sf_raster <- raster::raster(
    "../data/raw/weather/PISCO_temperature/PISCOdtn_v1.1.nc"
  )
  
  # start date of the data
  # create the vector of dates for the observations
  start_date <- tmin_sf_raster@z[[1]] |> ymd()
  nbands <- tmin_sf_raster@file@nbands
  dates <- seq(start_date, by="day", length.out = nbands)
  dates_tbl <- tibble(date = dates) |> 
    mutate(time = row_number())
  
  # Load the data
  library(tidync)
  tmin <- tidync::tidync("./data/raw/weather/PISCO_temperature/PISCOdtn_v1.1.nc")
  
  # Looping over the cells of the grid
  # For each cell, we will load chunks of data iteratively 
  # (because the size of the data is too large to be loaded in memory).
  # Let us consider 500 chunks.
  chunk_size <- 500
  a_parcourir <- tibble(
    start = c(1, seq(1, length(dates))[seq(1, length(dates)) %% chunk_size == 0])
  ) |> 
    mutate(end = lead(start)-1)
  
  if (last(a_parcourir$start) < length(dates)) {
    a_parcourir <- a_parcourir |> 
      mutate(end = ifelse(is.na(end), yes = length(dates), no = end))
  }
  
  # a_parcourir: identifies the dates of the elements in each of the 500 chunks
  # we can loop over the grid cells thanks to that object
  
  tmin_grille_k <- vector(mode = "list", length = nrow(map_peru_grid))
  pb <- txtProgressBar(min=0, max=nrow(map_peru_grid), style=3)
  # Loop over the cells  of the grid
  for (k in 1:nrow(map_peru_grid)) {
    # Identifying the kth cell
    cell <- map_peru_grid[k,]
    
    grid_tmin <- 
      tmin |> 
      tidync::hyper_filter(time = time ==0) |> 
      tidync::hyper_tibble() |> 
      sf::st_as_sf(
        coords = c("longitude", "latitude"),
        crs="+init=EPSG:4326",
        remove=FALSE
      )
    # Keeping only data in the kth cell: recovering coordinates
    ind_cell <- st_contains(cell, grid_tmin)[[1]]
    
    coords_keep <- 
      grid_tmin |> 
      slice(ind_cell) |> 
      as_tibble() |> 
      dplyr::select(longitude, latitude)
    
    tmin_grid_j <- vector(mode="list", length = nrow(a_parcourir))
    # Loop over chunks of dates
    for(j in 1:nrow(a_parcourir)){
      start <- a_parcourir$start[j]
      end <- a_parcourir$end[j]
      
      tmin_grid_j[[j]] <- 
        tmin |> 
        tidync::hyper_filter(time = dplyr::between(time, start, end)) |>
        tidync::hyper_tibble() |> 
        semi_join(coords_keep, by = c("longitude", "latitude")) |> 
        dplyr::left_join(dates_tbl, by = "time") |> 
        dplyr::filter(!is.na(date)) |> 
        dplyr::select(tn, date) |> 
        group_by(date) |> 
        summarise(tn = mean(tn, na.rm=TRUE)) |> 
        dplyr::mutate(grid_id = k)
    }
    
    tmin_grid_j <- bind_rows(tmin_grid_j)
    
    tmin_grille_k[[k]] <- tmin_grid_j
    setTxtProgressBar(pb, k)
  }
  
  tmin_grille <- bind_rows(tmin_grille_k)
  
  save(
    tmin_grille, 
    file = "data/output/weather/PISCO_temperature/tmin_grille.rda"
  )
}





#### Maximum Temperatures----

# We follow the same procedure for the maximum temperatures.

if (1 == 0) {
  tmax_sf_raster <- raster::raster(
    "data/raw/weather/PISCO_temperature/PISCOdtx_v1.1.nc"
  )
  
  start_date <- tmax_sf_raster@z[[1]] |> ymd()
  nbands <- tmax_sf_raster@file@nbands
  dates <- seq(start_date, by="day", length.out = nbands)
  dates_tbl <- tibble(date = dates) |> 
    mutate(time = row_number())
  dates_tbl
  
  library(tidync)
  tmax <- tidync::tidync(
    "./data/raw/weather/PISCO_temperature/PISCOdtx_v1.1.nc"
  )
  
  chunk_size <- 500
  a_parcourir <- tibble(
    start = c(1, seq(1, length(dates))[seq(1, length(dates)) %% chunk_size == 0])
  ) |> 
    mutate(end = lead(start) - 1)
  
  if (last(a_parcourir$start) < length(dates)) {
    a_parcourir <- a_parcourir |> 
      mutate(end = ifelse(is.na(end), yes = length(dates), no = end))
  }
  
  
  tmax_grille_k <- vector(mode = "list", length = nrow(map_peru_grid))
  pb <- txtProgressBar(min=0, max=nrow(map_peru_grid), style = 3)
  # Loop over the cells  of the grid
  for(k in 1:nrow(map_peru_grid)){
    # Identifying the kth cell
    cell <- map_peru_grid[k,]
    
    grid_tmax <- 
      tmax |> 
      tidync::hyper_filter(time = time ==0) |> 
      tidync::hyper_tibble() |> 
      sf::st_as_sf(
        coords = c("longitude", "latitude"),
        crs="+init=EPSG:4326",
        remove=FALSE
      )
    # Keeping only data in the kth cell: recovering coordinates
    ind_cell <- st_contains(cell, grid_tmax)[[1]]
    
    coords_keep <- 
      grid_tmax |> 
      slice(ind_cell) |> 
      as_tibble() |> 
      dplyr::select(longitude, latitude)
    
    tmax_grid_j <- vector(mode="list", length = nrow(a_parcourir))
    # Loop over chunks of dates
    for (j in 1:nrow(a_parcourir)) {
      start <- a_parcourir$start[j]
      end <- a_parcourir$end[j]
      
      tmax_grid_j[[j]] <- 
        tmax |> 
        tidync::hyper_filter(time = dplyr::between(time, start, end)) |>
        tidync::hyper_tibble() |> 
        semi_join(coords_keep, by = c("longitude", "latitude")) |> 
        dplyr::left_join(dates_tbl, by = "time") |> 
        dplyr::filter(!is.na(date)) |> 
        dplyr::select(tx, date) |> 
        group_by(date) |> 
        summarise(tx = mean(tx, na.rm=TRUE)) |> 
        dplyr::mutate(grid_id = k)
    }
    
    tmax_grid_j <- bind_rows(tmax_grid_j)
    
    tmax_grille_k[[k]] <- tmax_grid_j
    setTxtProgressBar(pb, k)
  }
  
  tmax_grille <- bind_rows(tmax_grille_k)
  
  save(
    tmax_grille, 
    file = "data/output/weather/PISCO_temperature/tmax_grille.rda"
  )
}

#### Merging Temperatures----

# Merge the daily temperature data at the grid level together in a single 
# tibble.

if (1 == 0) {
  temperatures_grid <- 
    tmax_grille |> 
    left_join(tmin_grille)
  
  # daily mean temperatures: average between min and max temperatures:
  temperatures_grid <- 
    temperatures_grid |> 
    rename(temp_max = tx, temp_min = tn) |> 
    mutate(temp_mean = (temp_min + temp_max) / 2)
  
  save(
    temperatures_grid, 
    file = "../data/output/weather/PISCO_temperature/temperatures_grid.rda"
  )
} else {
  load("../data/output/weather/PISCO_temperature/temperatures_grid.rda")
}


# Looking at 2010 values
# We compute the monthly average of daily mean, min, and maximum temperatures.
map_peru_grid_temp <- 
  map_peru_grid |> 
  mutate(grid_id = row_number()) |> 
  left_join(
    temperatures_grid |> 
      filter(year(date) == 2010) |> 
      mutate(
        month = month(date, abbr = FALSE, label = TRUE, locale = "en_US")
      ) |> 
      # Monthly average
      group_by(grid_id, month) |> 
      summarise(
        mean_temp_mean = mean(temp_mean),
        mean_temp_min = mean(temp_min),
        mean_temp_max = mean(temp_max),
        .groups = "drop"
      )
  )

# To have a similar color scale for each variable:
range_temperatures <- c(
  min(map_peru_grid_temp$mean_temp_min, na.rm = TRUE),
  max(map_peru_grid_temp$mean_temp_max, na.rm = TRUE)
)


##### Mean temp.----


# Monthly Mean of Daily Average Temperature 2010 in Peru
p_tmp_mean <- 
  ggplot() +
  geom_sf(
    data = map_peru_grid_temp |> filter(!is.na(mean_temp_mean)),
    mapping = aes(fill = mean_temp_mean)
  ) +
  geom_sf(data = map_peru, fill = NA) +
  scale_fill_gradient2(
    low = "#005A8B", mid = "white", high = "#AA2F2F", 
    midpoint = 0, limits = range_temperatures
  ) +
  facet_wrap(~month)
p_tmp_mean


##### Min temp.----

# Monthly Mean of Daily Minimum Temperature 2010 in Peru
p_tmp_min <-
  ggplot() +
  geom_sf(
    data = map_peru_grid_temp |> filter(!is.na(mean_temp_min)),
    mapping = aes(fill = mean_temp_min)
  ) +
  geom_sf(data = map_peru, fill = NA) +
  scale_fill_gradient2(
    low = "#005A8B", mid = "white", high = "#AA2F2F",
    midpoint = 0, limits = range_temperatures
  ) +
  facet_wrap(~month)
p_tmp_min



##### Max temp.----

# Monthly Mean of Daily Maximym Temperature 2010 in Peru
p_tmp_max <- 
  ggplot() +
  geom_sf(
    data = map_peru_grid_temp |> filter(!is.na(mean_temp_max)),
    mapping = aes(fill = mean_temp_max)
  ) +
  geom_sf(data = map_peru, fill = NA) +
  scale_fill_gradient2(
    low = "#005A8B", mid = "white", high = "#AA2F2F", 
    midpoint = 0, limits = range_temperatures,
  ) +
  facet_wrap(~month)
p_tmp_max


## Creation of Variables on the Grid----

# Let us merge the temperature and precipitation data in a single tibble:
weather_peru_daily_grid <- 
  precip_grille |> 
  rename(precip = value) |> 
  left_join(temperatures_grid)

# There are some issues with the 501th cell Let us remove that cell.
weather_peru_daily_grid <- 
  weather_peru_daily_grid |> 
  filter(grid_id != 501)

### Monthly Aggregation----

# To convert the daily grid data into monthly data, we aggregate the values 
# on a monthly basis while preserving the spatial scale of the grid cells. 
# This allows us to analyze the data at a monthly resolution and maintain 
# consistency with the original grid structure.
# Note: we only keep data from 1986 to 2015.

monthly_weather_data <- 
  weather_peru_daily_grid |> 
  mutate(
    year       = lubridate::year(date),
    month      = lubridate::month(date),
    month_name = lubridate::month(
      date, abbr = FALSE, label = TRUE, locale = "en_US"
    )
  ) |> 
  # Keeping 30 years of data
  filter(year >= 1986, year <= 2015) |> 
  # Monthly aggregates, at the grid cell level
  group_by(year, month, grid_id) |> 
  summarise(
    # Average min temperature
    temp_min = mean(temp_min, na.rm = TRUE),
    # Average max temperature
    temp_max = mean(temp_max, na.rm = TRUE),
    # Average mean temperature
    temp_mean = mean(temp_mean, na.rm = TRUE),
    # Total rainfall
    precip_sum = sum(precip, na.rm = TRUE)
  )


# We do not need to keep track of the unit for the weather data. 
# Let us drop that information.
monthly_weather_data <- units::drop_units(monthly_weather_data)

### Quarterly Aggregation----

# To convert the daily grid data into quarterly data, we aggregate the values 
# on a quarter basis while preserving the spatial scale of the grid cells.
# Note: we only keep data from 1986 to 2015.

quarterly_weather_data <- 
  weather_peru_daily_grid |> 
  mutate(
    year       = lubridate::year(date),
    quarter = lubridate::quarter(date)
  ) |> 
  # Keeping 30 years of data
  filter(year >= 1986, year <= 2015) |> 
  # Quartlery aggregates, at the grid cell level
  group_by(year, quarter, grid_id) |> 
  summarise(
    # Average min temperature
    temp_min = mean(temp_min, na.rm = TRUE),
    # Average max temperature
    temp_max = mean(temp_max, na.rm = TRUE),
    # Average mean temperature
    temp_mean = mean(temp_mean, na.rm = TRUE),
    # Total rainfall
    precip_sum = sum(precip, na.rm = TRUE)
  )


# We do not need to keep track of the unit for the weather data. 
# Let us drop that information.
quarterly_weather_data <- units::drop_units(quarterly_weather_data)



### Climate Normals----

# Climate normals: long-term averages
# We compute the means of the weather variables over a period of 30 years.

monthly_weather_data <- 
  monthly_weather_data |> 
  group_by(month, grid_id) |> 
  mutate(
    temp_min_monthly_lt   = mean(temp_min),
    temp_max_monthly_lt   = mean(temp_max),
    temp_mean_monthly_lt  = mean(temp_mean),
    precip_sum_monthly_lt = mean(precip_sum)
  ) |> 
  ungroup()


quarterly_weather_data <- 
  quarterly_weather_data |> 
  group_by(quarter, grid_id) |> 
  mutate(
    temp_min_quarterly_lt   = mean(temp_min),
    temp_max_quarterly_lt   = mean(temp_max),
    temp_mean_quarterly_lt  = mean(temp_mean),
    precip_sum_quarterly_lt = mean(precip_sum)
  ) |> 
  ungroup()

### Rainfall Shock----

# See @Burke_2014_EJ 
# Precipitation shocks defined using a Gamma distribution. 
# The historical data of monthly rainfall at each grid point are fitted
# to a grid-specific gamma distribution, and each year at the grid point
# is assigned to its corresponding percentile in that distribution. 
# Note that @Burke_2014_EJ uses crop-year data, whereas in our analysis, 
# we use grid-month data.


#' Estimate the shape and scale parameters of a Gamma distribution on the
#' monthly sum of precipitation for a given cell, then returns the
#' corresponding percentiles in the estimated distribution
#' 
#' @param x data frame with `precip_sum`, for a cell in a given calendar month
percentile_gamma_dist <- function(x) {
  # Estimate the shape and scale parameters of a Gamma distribution
  estim_gamma_precip <- EnvStats::egamma(x$precip_sum)
  estim_g_shape <- estim_gamma_precip$parameters[["shape"]]
  estim_g_scale <- estim_gamma_precip$parameters[["scale"]]
  # Corresponding percentile in the estimated distribution
  pgamma(q = x$precip_sum, shape = estim_g_shape, scale = estim_g_scale)
}

# Aggregate the observations by grid cell and month.
monthly_weather_data <- 
  monthly_weather_data |> 
  ungroup() |> 
  nest(.by = c(grid_id, month)) |> 
  mutate(perc_gamma_precip = map(data, ~percentile_gamma_dist(.x))) |> 
  unnest(c(data, perc_gamma_precip))

# Aggregate the observations by grid cell and quarter
quarterly_weather_data <- 
  quarterly_weather_data |> 
  ungroup() |> 
  nest(.by = c(grid_id, quarter)) |> 
  mutate(perc_gamma_precip = map(data, ~percentile_gamma_dist(.x))) |> 
  unnest(c(data, perc_gamma_precip))


### Deviations from Normals----

# deviation from the climate normals for each grid cell and each month.

monthly_weather_data <- 
  monthly_weather_data |> 
  mutate(
    temp_min_dev   = temp_min - temp_min_monthly_lt,
    temp_max_dev   = temp_max - temp_max_monthly_lt,
    temp_mean_dev  = temp_mean - temp_mean_monthly_lt,
    precip_sum_dev = precip_sum - precip_sum_monthly_lt
  )


quarterly_weather_data <- 
  quarterly_weather_data |> 
  mutate(
    temp_min_dev   = temp_min - temp_min_quarterly_lt,
    temp_max_dev   = temp_max - temp_max_quarterly_lt,
    temp_mean_dev  = temp_mean - temp_mean_quarterly_lt,
    precip_sum_dev = precip_sum - precip_sum_quarterly_lt
  )

### SPEI----

# Standardized Precipitation-Evapotranspiration Index (SPEI)
# See https://spei.csic.es/

# To calculate evapotranspiration, we need to provide the latitude parameter 
# to the `thornthwaite()` function from the {SPIE} package. 
# For each grid cell, we use the latitude of its barycenter as the input.

if (1 == 0) {
  centroids <- st_centroid(map_peru_grid$geometry)
  centroids_grid <- as_tibble(st_coordinates(centroids)) |> 
    mutate(grid_id = row_number()) |> 
    rename(longitude = X, latitude = Y)
  dir.create("../data/output/shapefile_peru/")
  save(centroids_grid, file = "../data/output/shapefile_peru/centroids_grid.rda")
} else {
  load("../data/output/shapefile_peru/centroids_grid.rda")
}


# The centroids of the cells are added to the `monthly_weather_data` tibble:

monthly_weather_data <- 
  monthly_weather_data |> 
  left_join(centroids_grid)


quarterly_weather_data <- 
  quarterly_weather_data |> 
  left_join(centroids_grid)


#' Returns the SPI and SPEI on a monthly basis for a specific grid cell at
#' different scales
#'
#' @param grid_id id of the grid cell
#' @param scale vector of scales for the spi and spei functions
#' @returns a tibble at the grid x monthly scale with the spi and the spei at
#'   different scales
compute_spei_cell <- function(grid_id, scale = c(1,3,6,12)) {
  lat <- centroids_grid |> 
    filter(grid_id == !!grid_id) |> 
    pull(latitude)  |> 
    unique() |> 
    as.numeric()
  
  tmp_grid <- 
    monthly_weather_data |> 
    # Focus on grid cell
    filter(grid_id == !!grid_id) |> 
    ungroup() |> 
    arrange(year, month) |> 
    dplyr::select(year, month, grid_id, precip_sum, temp_mean, latitude) |> 
    mutate(
      PET = SPEI::thornthwaite(temp_mean, lat, verbose = FALSE), 
      BAL = precip_sum - PET
    )
  
  df_spi <- 
    map(
      .x = scale,
      .f = ~tibble(
        scale = .x,
        spi = SPEI::spi(tmp_grid$precip_sum, scale = .x, verbose = FALSE)$fitted,
        spei = SPEI::spei(tmp_grid$precip_sum, scale = .x, verbose = FALSE)$fitted
      ) |> 
        mutate(t = row_number()) |> 
        mutate(
          spi = ifelse(is.infinite(spi), yes = NA, no = spi),
          spei = ifelse(is.infinite(spei), yes = NA, no = spei)
        )
    ) |> 
    list_rbind() |> 
    pivot_wider(names_from = scale, values_from = c(spi, spei)) |> 
    dplyr::select(-t)
  
  bind_cols(tmp_grid, df_spi) |> 
    dplyr::select(-precip_sum, -temp_mean, -latitude)
  
}


# Apply the  `compute_spei_cell()` function to all the grid cells
resul_spei <- 
  map(
    .x = unique(monthly_weather_data$grid_id),
    .f = compute_spei_cell,
    .progress = TRUE
  )

# Bind the results
resul_spei <- 
  resul_spei |> 
  list_rbind(names_to = "grid_id")


# The SPI and SPEI can be added to the weather tibble:
monthly_weather_data <- 
  monthly_weather_data |> 
  ungroup() |> 
  left_join(resul_spei)



## Aggregation at the (monthly) regional level----


# For each region, we calculate the average of the values from each cell,
# weighting each term according to two measures. 
# 1.  The proportion that the cell represents in the total surface area of 
#   the region.
# 2.  The proportion that the cell represents in the agricultural production 
#   of the region.

# We focus on the following weather variables:
weather_variables <- c(
  "temp_min", "temp_max", "temp_mean", "precip_sum",
  "perc_gamma_precip",
  "temp_min_dev", "temp_max_dev", "temp_mean_dev", "precip_sum_dev"
)
weather_spi_variables <- colnames(resul_spei |> dplyr::select(matches("^spe?i")))
weather_variables <- c(weather_variables, weather_spi_variables)
weather_variables

#' Aggregates the weather data at the region level
#' @description For a specific region, the grid cells within that region are
#'   first identified. Then, the area of the intersection between each grid cell
#'   and the region are computed. The share of agricultural lands in each cell
#'   is also computed. Combined together, the area of the intersection and the
#'   agricultural share are used as weights to aggregate the data at the
#'   regional level.
#'
#' @param i row index of the region in `map_peru`
#' @param weather_variables vector of name of the weather variables to aggregate
aggregate_region_i <- function(i, weather_variables) {
  map_region_i <- map_peru[i,]
  tmp <- 
    sf::st_intersection(map_peru_grid_agri, map_region_i) |> 
    # Get the area of the intersection between the polygon of the current
    # region and each grid cell that intersects it
    dplyr::mutate(area_cell_intersect = sf::st_area(geometry)) |> 
    dplyr::rename(grid_id = i) |> 
    # Get the weather data for the corresponding grid cells
    dplyr::left_join(
      monthly_weather_data,
      by = "grid_id"
    ) |> 
    dplyr::filter(!is.na(year)) |> 
    # Remove the unit in the area (squared metres)
    dplyr::mutate(
      area_cell_intersect = units::drop_units(area_cell_intersect)
    ) |> 
    dplyr::group_by(month, year) |> 
    # Compute the weights to attribute to each grid cell
    dplyr::mutate(
      w_cropland = cropland / sum(cropland),
      w_area     = area_cell_intersect / sum(area_cell_intersect),
      w          = w_cropland * w_area,
      w          = w / sum(w)) |> 
    # Weighted weather variables
    dplyr::mutate(
      dplyr::across(
        .cols = !!weather_variables,
        .fns = ~ .x * w
      )
    ) |> 
    dplyr::select(-geometry) |> 
    tibble::as_tibble() |> 
    dplyr::group_by(year, month) |> 
    dplyr::select(-geometry) |> 
    tibble::as_tibble() |> 
    dplyr::group_by(year, month) |> 
    dplyr::summarise(
      dplyr::across(
        .cols = !!weather_variables,
        .fns = ~sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |> 
    dplyr::mutate(IDDPTO = map_region_i$IDDPTO)
}


# Apply the `aggregate_region_i()`{.R} function to each region:
weather_regions_df <- 
  map(
    .x = seq_len(nrow(map_peru)),
    .f = ~aggregate_region_i(.x, weather_variables), 
    .progress = TRUE
  )
weather_regions_df |> 
  list_rbind() |> 
  filter(is.na(year)) |> 
  dplyr::select(IDDPTO)

# Add the department name:
weather_regions_df <- 
  weather_regions_df |> 
  list_rbind() |> 
  left_join(
    map_peru |> dplyr::select(IDDPTO, DEPARTAMEN) |>
      as_tibble() |> dplyr::select(-geometry),
    by = "IDDPTO"
  )


### Add labels to the columns----

weather_regions_df <- 
  weather_regions_df |> 
  labelled::set_variable_labels(
    year = "Year",
    month = "Month",
    temp_min = "Min. Temperature",
    temp_max = "Max. Temperature",
    temp_mean = "Mean Temperature",
    precip_sum = "Total Rainfall",
    perc_gamma_precip = "Precipitation Shock (Percentile from Gamma Dist.)",
    temp_min_dev = "Deviation of Min. Temperature from Normals",
    temp_max_dev = "Deviation of Max. Temperature from Normals",
    temp_mean_dev = "Deviation of Mean Temperature from Normals",
    precip_sum_dev = "Deviation of Total Rainfall from Normals",
    spi_1 = "SPI Index (scale = 1)",
    spi_3 = "SPI Index (scale = 3)",
    spi_6 = "SPI Index (scale = 6)",
    spi_12 = "SPI Index (scale = 12)",
    spei_1 = "SPEI Index (scale = 1)",
    spei_3 = "SPEI Index (scale = 3)",
    spei_6 = "SPEI Index (scale = 6)",
    spei_12 = "SPEI Index (scale = 12)",
    IDDPTO = "Region ID",
    DEPARTAMEN = "Region Name"
  )

# And the results are saved:
save(
  weather_regions_df,
  file = "../data/output/weather/weather_regions_df.rda"
)

load("../data/output/weather/weather_regions_df.rda")


# Some monthly statistics over the period can be computed.
monthly_temp_peru_monthly_avg <- 
  weather_regions_df |> 
  dplyr::group_by(month) |> 
  dplyr::summarise(
    across(
      .cols = c(
        "temp_min", "temp_max", "temp_mean", "precip_sum",
        "perc_gamma_precip",
        "temp_min_dev", "temp_max_dev", "temp_mean_dev", "precip_sum_dev"
      ),
      .fns = list(mean = mean, sd = sd)
    )
  )


# Let us have a look at average values.

### Mean temp.----

# Average monthly temperatures in Peru (1988-2015)
ggplot(data = monthly_temp_peru_monthly_avg |> 
         mutate(
           month = month.abb[month],
           month = factor(month, levels = month.abb)
         )
) +
  geom_bar(
    mapping = aes(x = month, y = temp_mean_mean),
    stat="identity",
    fill="#5482AB", alpha=0.7
  ) +
  geom_errorbar(
    mapping = aes(
      x = month,
      ymin = temp_mean_mean - temp_mean_sd,
      ymax = temp_mean_mean + temp_mean_sd
    ),
    width = 0.4, colour = "#005A8B", alpha = 0.9, linewidth = 1.3
  ) +
  labs(
    x = NULL, y = "Temperatures (°C)"
  ) +
  scale_y_continuous(breaks = seq(0, 30, by = 2))


### Min temp.----

# Average monthly minimum temperatures in Peru (1988-2015)
ggplot(data = monthly_temp_peru_monthly_avg |> 
         mutate(
           month = month.abb[month],
           month = factor(month, levels = month.abb)
         )
) +
  geom_bar(
    mapping = aes(x = month, y = temp_min_mean),
    stat="identity",
    fill="#5482AB", alpha=0.7
  ) +
  geom_errorbar(
    mapping = aes(
      x = month,
      ymin = temp_min_mean - temp_min_sd,
      ymax = temp_min_mean + temp_min_sd
    ),
    width = 0.4, colour = "#005A8B", alpha = 0.9, linewidth = 1.3
  ) +
  labs(
    x = NULL, y = "Temperatures (°C)"
  ) +
  scale_y_continuous(breaks = seq(0, 30, by = 2))

### Max temp.----

# Average monthly maximum temperatures in Peru (1988-2015)
ggplot(data = monthly_temp_peru_monthly_avg |> 
         mutate(
           month = month.abb[month],
           month = factor(month, levels = month.abb)
         )
) +
  geom_bar(
    mapping = aes(x = month, y = temp_max_mean),
    stat="identity",
    fill="#5482AB", alpha=0.7
  ) +
  geom_errorbar(
    mapping = aes(
      x = month,
      ymin = temp_max_mean - temp_max_sd,
      ymax = temp_max_mean + temp_max_sd
    ),
    width=0.4, colour = "#005A8B", alpha = 0.9, linewidth = 1.3
  ) +
  labs(
    x = NULL, y = "Temperatures (°C)"
  ) +
  scale_y_continuous(breaks = seq(0, 30, by = 2))


### Precipitation----

# Average monthly precipitation in Peru (1988-2015)
ggplot(data = monthly_temp_peru_monthly_avg |> 
         mutate(
           month = month.abb[month],
           month = factor(month, levels = month.abb)
         )
) +
  geom_bar(
    mapping = aes(x = month, y = precip_sum_mean),
    stat="identity",
    fill="#5482AB", alpha=0.7
  ) +
  geom_errorbar(
    mapping = aes(
      x = month,
      ymin = precip_sum_mean - precip_sum_sd,
      ymax = precip_sum_mean + precip_sum_sd
    ),
    width=0.4, colour = "#005A8B", alpha = 0.9, linewidth = 1.3
  ) +
  labs(
    x = NULL, y = "Precipiation (mm)"
  )

# Choropleth maps with the weather values.
# First, we define a tibble with the values for 2010 only.
map_peru_regional_weather <- 
  map_peru |> 
  left_join(
    weather_regions_df |> 
      dplyr::mutate(
        month = factor(month.abb[month], levels = month.abb)) |> 
      filter(year == 2010)
  )



### Mean temp. (map)----

# Regional mean temperatures in Peru - 2010
b <- as.numeric(
  quantile(
    map_peru_regional_weather$temp_mean,
    probs = seq(0,1, by = .1),
    na.rm=TRUE
  )
)
cols <- viridis::magma(6) |> rev()
cols <- cols[-length(cols)]
p <- 
  ggplot() + 
  geom_sf(
    data = map_peru_regional_weather |> filter(!is.na(month)),
    mapping = aes(fill = temp_mean),
    colour = "white"
  ) +
  scale_fill_gradientn(name = "Temperature (°C)", colours = cols) +
  facet_wrap(~month, ncol = 4)

panel_height <- unit(1,"npc") - 
  sum(ggplotGrob(p)[["heights"]][-3]) - unit(1,"line")

p + guides(fill = guide_colorbar(barheight = panel_height))

### Min temp. (map)----

# Regional minimum temperatures in Peru - 2010
b <- as.numeric(
  quantile(
    map_peru_regional_weather$temp_min,
    probs = seq(0,1, by = .1),
    na.rm=TRUE
  )
)
cols <- viridis::magma(6) |> rev()
cols <- cols[-length(cols)]
p <- 
  ggplot() + 
  geom_sf(
    data = map_peru_regional_weather |> filter(!is.na(month)),
    mapping = aes(fill = temp_min),
    colour = "white"
  ) +
  scale_fill_gradientn(name = "Temperature (°C)", colours = cols) +
  facet_wrap(~month, ncol = 4)

panel_height <- unit(1,"npc") - 
  sum(ggplotGrob(p)[["heights"]][-3]) - unit(1,"line")

p + guides(fill = guide_colorbar(barheight = panel_height))

### Max temp. (map)----

# Regional maximum temperatures in Peru - 2010
b <- as.numeric(
  quantile(
    map_peru_regional_weather$temp_max,
    probs = seq(0,1, by = .1),
    na.rm=TRUE
  )
)
cols <- viridis::magma(6) |> rev()
cols <- cols[-length(cols)]
p <- 
  ggplot() + 
  geom_sf(
    data = map_peru_regional_weather |> filter(!is.na(month)),
    mapping = aes(fill = temp_max),
    colour = "white"
  ) +
  scale_fill_gradientn(name = "Temperature (°C)", colours = cols) +
  facet_wrap(~month, ncol = 4)

panel_height <- unit(1,"npc") - 
  sum(ggplotGrob(p)[["heights"]][-3]) - unit(1,"line")

p + guides(fill = guide_colorbar(barheight = panel_height))


### Precipitation (map)----

# Regional precipitation in Peru - 2010
b <- as.numeric(
  quantile(
    map_peru_regional_weather$precip_sum,
    probs = seq(0,1, by = .1)
  )
)

cols <- rainbow(6)
cols <- cols[-length(cols)]

p <- 
  ggplot() + 
  geom_sf(
    data = map_peru_regional_weather,
    mapping = aes(fill = precip_sum),
    colour = "white"
  ) +
  scale_fill_continuous(
    "Rainfall (mm/month)", 
    low = "white", high = "#005A8B", na.value = "grey50",
    breaks = round(b), labels = round(b)
  ) +
  facet_wrap(~month, ncol = 4)

panel_height <- unit(1,"npc") - 
  sum(ggplotGrob(p)[["heights"]][-3]) - unit(1,"line")

p_2 <- p + guides(fill= guide_colorbar(barheight=panel_height))
p_2
