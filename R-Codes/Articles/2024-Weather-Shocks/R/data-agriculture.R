library(tidyverse)
library(readxl)

# Load functions to download data from MINAGRI
source("../weatherperu/R/download_minagri.R")

# Load functions to import data from the downloaded files
source("../weatherperu/R/import_minagri.R")

# Should the data be downloaded again?
download_again <- FALSE

# 1. Download data----
# --------------------

if (download_again) {
  
  for (y in c(2:14)) {
    year <-  ifelse(y < 10, paste("200", y, sep=""), paste("20", y, sep=""))
    print(year)
   
     # Code for years in PDF files
    if (y %in% c(2,3,4)) {
      for (m in 12:1) {
        print(m)
        #Months not available in PDF Files
        if ( m == 3  & y == 3) {next}
        if ( m == 3  & y == 2) {next}
        if ( m == 6  & y == 2) {next}
        if ( m == 9  & y == 2) {next}
        if ( m == 12 & y == 2) {next}
        
        # URL to download the pdf files
        link <- str_c(
          "https://www.midagri.gob.pe/portal/download/pdf",
          "/herramientas/boletines/boletineselectronicos/",
          "estadisticaagrariamensual/", year, "/EAM",
          str_sub(year, 3, 4),
          ifelse(m < 10, paste("0", m, sep = ""), m),
          ".pdf"
        )
        
        # # Production - Pages 44,45, 46 and 47 of the PDF Files
        name <- str_c(
          "../data/raw/minagri/Production/Production_", year, ".xlsx"
        )
        for (i in 0:1) {
          page1 <- extract_pdf_data(
            annee = y,
            mois = m,
            adresse = link,
            page = 44 + 2 * i, 
            Cell1 = "Departamento"
          )
          page2 <- extract_pdf_data(
            annee = y, 
            mois = m, 
            adresse = link,
            page = 45 + 2 * i,
            Cell1 = "Departamento"
          )
          Tableau <- rbind(page1, page2)
          
          write.xlsx(
            Tableau, 
            name, 
            sheetName = str_c(
              ifelse(m < 10, paste("0",m, sep=""), m),
              year,
              i + 1
            ),
            append = TRUE,
            showNA = FALSE
          )
          print(paste("Production", i, "ok "))
        }
        # Planted surface - Pages 30, 31, 32, 33 of the PDF Files
        name <- str_c(
          "../data/raw/minagri/Surface/Superficies_", year, ".xlsx"
        )
        for (i in 0:1) {
          page1 <- extract_pdf_data(
            annee = y,
            mois = m,
            adresse = link,
            page = 30 + 2 * i,
            Cell1 = "Departamento"
          )
          page2 <- extract_pdf_data(
            annee = y, 
            mois = m,
            adresse = link,
            page = 31 + 2 * i,
            Cell1 = "Departamento"
          )
          Tableau <-  rbind(page1, page2)
          write.xlsx(
            Tableau,
            name,
            sheetName = str_c(
              ifelse(m < 10, paste("0", m, sep = ""), m), 
              year,
              i + 1
            ), 
            append = TRUE, 
            showNA = FALSE
          )
          print(paste("Surface", i, "ok "))
        }
        
        # Harvested surface - Pages 38,45, 46 and 47 of the PDF Files
        name <- str_c(
          "../data/raw/minagri/Surface_R/Superficies_R_", year, ".xlsx"
        )
        for (i in 0:1) {
          page1 <- extract_pdf_data(
            annee = y,
            mois = m, 
            adresse = link, 
            page = 38 + 2 * i,
            Cell1 = "Departamento"
          )
          page2 <- extract_pdf_data(
            annee = y, 
            mois = m, 
            adresse = link, 
            page = 39 + 2 * i, 
            Cell1 = "Departamento"
          )
          Tableau <-  rbind(page1, page2)
          write.xlsx(
            Tableau,
            name,
            sheetName = str_c(
              ifelse(m < 10, paste("0", m, sep = ""), m),
              year,
              i + 1
            ), 
            append = TRUE,
            showNA = FALSE
          )
          print(paste("Surface R", i, "ok "))
        }
        
        # Prices - Pages 109,110, 111, 112, 113, 114 or 115 depending on the PDF Files
        name <- str_c(
          "../data/raw/minagri/Prices/Prices_", year, ".xlsx"
        )
        for (i in 0:1) {
          if (y < 3 & m < 8) {
            page2002 <- case_when(m == 7 ~ 4, TRUE ~ 3)
          } else {
            page2002 <- 0
          }
          page1 <- extract_pdf_data(
            annee = y,
            mois = m,
            adresse = link, 
            page = 112 +2 * i - page2002, 
            Cell1 = "Departamento"
          )
          page2 <- extract_pdf_data(
            annee = y,
            mois = m,
            adresse = link, 
            page = 113 + 2 * i - page2002,
            Cell1 = "Departamento"
          )
          
          if((ncol(page1) != ncol(page2)) & 
             any(str_detect(page2[,ncol(page2)], "Plátano"))) {
            page2 <- page2[,-ncol(page2)]
          }
          Tableau <-  rbind(page1, page2)
          write.xlsx(
            Tableau, 
            name,
            sheetName = str_c(
              ifelse(m < 10, paste("0", m, sep = ""), m),
              year,
              i + 1
            ),
            append = TRUE,
            showNA = FALSE
          )
        }
      }
    } else {
      # Code for years in Excel files - 2006 and above
      # Remark : 2005 data obtained from the 2006 files
      if(y == 5) {
        next
      } else {
        # Downloading and extracting the excel sheets of interest
        download.data(y, out_folder = "../data/raw/minagri/")
      }
    }
  }
}

# Data frame init----

# The years
my_y <- 2:16

# Production 
data_P_TOTAL <- vector(mode = "list", length = length(my_y))
# Planted surface
data_S_TOTAL <- vector(mode = "list", length = length(my_y))
# Harvested surface 
data_SR_TOTAL <- vector(mode = "list", length = length(my_y))
# Prices
data_Px_TOTAL <- vector(mode = "list", length = length(my_y))

# library(beepr)


# 2. Import Data----
# ------------------

cli::cli_progress_bar(total = length(2:16))

# y_ix <- 7
for (y_ix in 1:length(my_y)) {
  y <- my_y[y_ix]
  print(str_c("Year: ", 2000+y-1))
  if (y %in% c(5,8,16)) {
    # Extracting the data of year y (only for the last year)
    # last digits of the year
    # (Since the files are not provided)
    fn_digits_year <- ifelse(y < 10, str_c("0", y - 1), y - 1)
    
    data_P  <- import_monthly_regional_values_P(
      file = str_c(
        "../data/raw/minagri/Production/Production_20", 
        fn_digits_year, ".xlsx"
      ),
      anneesup = 1,
      timescale = 1
    )
    data_Px <- import_monthly_regional_values_Px(
      file = str_c(
        "../data/raw/minagri/Prices/Prices_20", 
        fn_digits_year, ".xlsx"
      ),
      anneesup = 1,
      timescale = 1
    )
    data_S  <- import_monthly_regional_values_S(
      file = str_c(
        "../data/raw/minagri/Surface/Superficies_20", 
        fn_digits_year, ".xlsx"
      ),
      anneesup = 1, 
      timescale = 1
    )
    data_SR <- import_monthly_regional_values_SR(
      file = str_c(
        "../data/raw/minagri/Surface_R/Superficies_R_20", 
        fn_digits_year, ".xlsx"
      ),
      anneesup = 1, 
      timescale = 1
    )
    
  } else {
    # Extracting the data of year y-1 for the remaining years
    fn_digits_year <- str_pad(y, width = 2, side = "left", pad = 0)
    data_P  <- import_monthly_regional_values_P(
      file = str_c(
        "../data/raw/minagri/Production/Production_20", 
        fn_digits_year, ".xlsx"
      ),
      anneesup = 0,
      timescale = 1
    )
    data_S  <- import_monthly_regional_values_S(
      file = str_c(
        "../data/raw/minagri/Surface/Superficies_20", 
        fn_digits_year, ".xlsx"
      ),
      anneesup = 0,
      timescale = 1
    )
    data_SR <- import_monthly_regional_values_SR(
      file = str_c(
        "../data/raw/minagri/Surface_R/Superficies_R_20",
        fn_digits_year, ".xlsx"
      ),
      anneesup = 0, 
      timescale = 1
    )
    data_Px <- import_monthly_regional_values_Px(
      file = str_c(
        "../data/raw/minagri/Prices/Prices_20",
        fn_digits_year, ".xlsx"
      ),
      anneesup = 0,
      timescale = 1
    )
 
  }
  
  data_P <-
    data_P |>
    mutate(date = lubridate::ymd(str_c(year, month, "01", sep = "-")))
  colnames(data_P) <- c(
    "region", "year", "product", "month", "Value_prod", "date"
  )

  data_S <-
    data_S |>
    mutate(date = lubridate::ymd(str_c(year, month, "01", sep = "-")))
  colnames(data_S) <- c(
    "region", "campaign", "month", "product", "year","Value_surf", "date"
  )
  
  data_SR <-
    data_SR |>
    mutate(date = lubridate::ymd(str_c(year, month, "01", sep = "-")))
  colnames(data_SR) <- c(
    "region", "year", "product", "month", "Value_surfR", "date"
  )
  
  data_Px <-
    data_Px |>
    mutate(date = lubridate::ymd(str_c(year, month, "01", sep = "-")))
  colnames(data_Px) <- c(
    "region", "year", "month", "product", "Value_prices", "date"
  )

  # # Adding the new year to the global mensual files
  data_P_TOTAL[[y_ix]]  <- data_P
  data_S_TOTAL[[y_ix]]  <- data_S
  data_SR_TOTAL[[y_ix]] <- data_SR
  data_Px_TOTAL[[y_ix]] <- data_Px
  
  
  rm(
    data_P, data_S, data_SR, data_Px
    )
  cli::cli_progress_update()
} # End of Loop 1
# beep("fanfare")

# All the year-elements in a single tibble
data_P_TOTAL    <- list_rbind(data_P_TOTAL)
data_S_TOTAL    <- list_rbind(data_S_TOTAL)
data_SR_TOTAL   <- list_rbind(data_SR_TOTAL)
data_Px_TOTAL   <- list_rbind(data_Px_TOTAL)



## Campaign Data----
# Adding the agricultural campaign data to the Planted surfaces data

data_S_TOTAL <- data_S_TOTAL |> 
  mutate(
    campaign = as.numeric(str_sub(campaign, 3, 4)),
    campaign_plain = str_c(campaign,"/",campaign + 1)) |> 
  unique() |> 
  # Retrieving the campaign month (starting in August)
  mutate(
    month_campaign = case_when(
      month == 1  ~ 6,
      month == 2  ~ 7,
      month == 3  ~ 8,
      month == 4  ~ 9,
      month == 5  ~ 10,
      month == 6  ~ 11,
      month == 7  ~ 12,
      month == 8  ~ 1,
      month == 9  ~ 2,
      month == 10 ~ 3,
      month == 11 ~ 4,
      month == 12 ~ 5
    )
  ) |> 
  mutate(
    product = ifelse(
      product == "maíz duro",
      yes =  "maíz amarillo duro",
      no = product
    )
  ) |> 
  group_by(region, product) |> 
  # Harmonizing the cumulative values of surface (if the lead and the lag are equals)
  mutate(
    Value_surf = ifelse(
      lead(Value_surf) == lag(Value_surf, default = 0) & 
        Value_surf < lag(Value_surf, default = 0),
      yes = lag(Value_surf),
      no = Value_surf
    )
  )

# Checking for errors  
dup_surf <- data_S_TOTAL |>
  filter(! product == "total") |> 
  filter(! product == "TOTAL") |> 
  group_by(region, date, product) |> 
  mutate(n = n()) |> 
  filter(n > 1) |> 
  select(date) |> 
  unique()


# Determining the growth duration and the corresponding lags from the
#   Agricultural calendars
# See: `./data-agriculture-calendar.R`

load("../data/output/Calendario agricola/calendar2.rda")
calendar2 <- calendar2 |> 
  mutate(
    region = str_replace_all(region, "á", "a"),
    region = str_replace_all(region, "í", "i"),
    region = str_replace_all(region, "é", "e"),
    region = str_replace_all(region, "ó", "o"),
    region = str_replace_all(region, "ú", "u"),
    region = str_replace_all(region, "ñ", "n")
  ) |> 
  mutate(
    region = toupper(region), 
    product = toupper(product),
    product = ifelse(str_detect(product, "ARROZ"), "ARROZ CÁSCARA", product)
  )

# Computing the difference of the cumulative sum, stating in August, to retrieve
#   the value of the monthly planted surface. 

data_S_TOTAL <- data_S_TOTAL |>
  group_by(region, product, campaign) |>
  arrange(region, product, campaign, month_campaign) |> 
  mutate(
    surf_m = case_when(
      month == "8" ~ Value_surf,
      month != "8" ~ Value_surf - lag(Value_surf))
  ) |>
  ungroup() |>  
  group_by(region, product) |>
  mutate(id = row_number()) |> 
  ungroup() |> 
  mutate(
    region = str_replace_all(region, "á", "a"),
    region = str_replace_all(region, "í", "i"),
    region = str_replace_all(region, "é", "e"),
    region = str_replace_all(region, "ó", "o"),
    region = str_replace_all(region, "ú", "u"),
    region = str_replace_all(region, "ñ", "n")
  ) |> 
  mutate(
    region = toupper(iconv(region, to = "ASCII//TRANSLIT")),
    product = toupper(product)
  ) |>
  left_join(calendar2, by = c("region","product"))


# Associating the harvested and planted surfaces, according to the growth duration 
# obtained. 

data_S_TOTAL <- 
  data_S_TOTAL |> 
  arrange(region, product, id) |> 
  group_by(region, product) |>
  mutate(
    surf_lag_calend = case_when(
      growth_duration == 1  & !is.na(lag(surf_m, 1))  ~ lag(surf_m, 1),
      growth_duration == 2  & !is.na(lag(surf_m, 2))  ~ lag(surf_m, 2),
      growth_duration == 3  & !is.na(lag(surf_m, 3))  ~ lag(surf_m, 3),
      growth_duration == 4  & !is.na(lag(surf_m, 4))  ~ lag(surf_m, 4),
      growth_duration == 5  & !is.na(lag(surf_m, 5))  ~ lag(surf_m, 5),
      growth_duration == 6  & !is.na(lag(surf_m, 6))  ~ lag(surf_m, 6),
      growth_duration == 7  & !is.na(lag(surf_m, 7))  ~ lag(surf_m, 7),
      growth_duration == 8  & !is.na(lag(surf_m, 8))  ~ lag(surf_m, 8),
      growth_duration == 9  & !is.na(lag(surf_m, 9))  ~ lag(surf_m, 9),
      growth_duration == 10 & !is.na(lag(surf_m, 10)) ~ lag(surf_m, 10),
      growth_duration == 11 & !is.na(lag(surf_m, 11)) ~ lag(surf_m, 11),
      growth_duration == 12 & !is.na(lag(surf_m, 12)) ~ lag(surf_m, 12),
      TRUE ~ NA
    )
  )

# Harmonizing the data

data_S_TOTAL <- data_S_TOTAL |>  
  mutate(
    surf_lag_calend = ifelse(id <= growth_duration, NA, surf_lag_calend)
  ) |> 
  filter(! product %in% c("TOTAL", "total")) |> 
  rename(
    "gr_duration_calend" = "growth_duration", 
    "month_plan_calend" = "month.y", 
    "month_harv_calend" = "month.x"
  ) |> 
  select(- period.x, - pct.x, -pct.y, -period.y)

rm(calendar2)

## Saving the raw data----

save(data_P_TOTAL, file = "../data/output/minagri/data_P_TOTAL.rda")
save(data_S_TOTAL, file = "../data/output/minagri/data_S_TOTAL.rda")
save(data_SR_TOTAL, file = "../data/output/minagri/data_SR_TOTAL.rda")
save(data_Px_TOTAL, file = "../data/output/minagri/data_Px_TOTAL.rda")
 

# 3. Aggregation of Agricultural Datasets ---
# -------------------------------------------
library(tidyverse)
library(lubridate)

load("../data/output/minagri/data_P_TOTAL.rda")
load("../data/output/minagri/data_S_TOTAL.rda")
load("../data/output/minagri/data_SR_TOTAL.rda")
load("../data/output/minagri/data_Px_TOTAL.rda")

## Harmonization----

# Harmonizing the production, surfaces and prices datasets

data_Px_TOTAL <- data_Px_TOTAL |> 
  mutate(
    region = str_replace_all(region, "á", "a"),
    region = str_replace_all(region, "í", "i"),
    region = str_replace_all(region, "é", "e"),
    region = str_replace_all(region, "ó", "o"),
    region = str_replace_all(region, "ú", "u"),
    region = str_replace_all(region, "ñ", "n")
  ) |> 
  mutate(region = toupper(iconv(region, to = "ASCII//TRANSLIT"))) |> 
  mutate(
    region = toupper(region), 
    product = toupper(product), 
    product = ifelse(
      product == "FRIJOL GRANO SECO**",
      yes = "FRIJOL GRANO SECO", 
      no = product
    )
  )

data_SR_TOTAL <- data_SR_TOTAL |> 
  mutate(
    region = str_replace_all(region, "á", "a"),
    region = str_replace_all(region, "í", "i"),
    region = str_replace_all(region, "é", "e"),
    region = str_replace_all(region, "ó", "o"),
    region = str_replace_all(region, "ú", "u"),
    region = str_replace_all(region, "ñ", "n")
  ) |> 
  mutate(region = toupper(iconv(region, to = "ASCII//TRANSLIT"))) |> 
  mutate(region = toupper(region), 
         product = toupper(product), 
         product = ifelse(
           product == "FRIJOL GRANO SECO**",
           yes = "FRIJOL GRANO SECO",
           no = product)
  )


# Selecting the total value (December) as an additional month 
# temp <- data_S_TOTAL |> 
#   select(
#     region, product, campaign_plain, month_campaign, Value_surf, campaign
#   ) |> 
#   filter(month_campaign == 12) |> 
#   mutate(month_campaign = 13) |>
#   rename(surf_m = Value_surf)


## Aggregation in a single tibble----

data <- data_P_TOTAL |> 
  mutate(
    region = str_replace_all(region, "á", "a"),
    region = str_replace_all(region, "í", "i"),
    region = str_replace_all(region, "é", "e"),
    region = str_replace_all(region, "ó", "o"),
    region = str_replace_all(region, "ú", "u"),
    region = str_replace_all(region, "ñ", "n")) |> 
  mutate(region = toupper(iconv(region, to = "ASCII//TRANSLIT"))
  ) |> 
  mutate(
    region = toupper(region), 
    product = toupper(product), 
    month = as.numeric(month)
  ) |> 
  full_join(
    data_S_TOTAL |> 
      select(-c(year,month)), 
    by = c("region","product","date")
  ) |>  
  filter(! is.na(year)) |> 
  # No consequences on the aggregation because the missing data are not in the
  # product sample used hereafter. (except for Lima but without production value)
  full_join(
    data_Px_TOTAL |> 
      select(-c(year,month)), 
    by = c("region","product","date")
  ) |>  
  filter(product %in% c(
    "PAPA", "CEBADA GRANO", "MAÍZ AMARILLO DURO", "MAÍZ AMILÁCEO",
    "ARROZ CÁSCARA", "SORGO GRANO", "YUCA", "TRIGO")
  ) |> 
  select(
    region, product, year, month, date, 
    Value_prod, surf_m, surf_lag_calend, Value_prices, 
    campaign, month_campaign, campaign_plain, id
  ) |> 

  full_join(
    data_SR_TOTAL |> 
      select(- month, - year), 
    by = c("region","product","date")
  ) |>
  filter(product %in% c(
    "PAPA", "CEBADA GRANO", "MAÍZ AMARILLO DURO", "MAÍZ AMILÁCEO",
    "ARROZ CÁSCARA", "SORGO GRANO", "YUCA", "TRIGO")
  ) |>
  filter(! region %in% c(
    "Lima Metropolitana", "Callao", "LIMA METROPOLITANA",
    "CALLAO", "PROMEDIO NACIONAL")
  ) |>
  filter(! is.na(region)) |> 
  filter(! product %in% c("SORGO GRANO", "CEBADA GRANO"))


missing_values <- readxl::read_excel(
  path = "../data/raw/Macro/Datos_INEI1.xlsx",
  sheet = "SELECTED2",
  col_types = "text"
) |> 
  mutate(
    region = str_replace_all(region, "á", "a"),
    region = str_replace_all(region, "í", "i"),
    region = str_replace_all(region, "é", "e"),
    region = str_replace_all(region, "ó", "o"),
    region = str_replace_all(region, "ú", "u"),
    region = str_replace_all(region, "ñ", "n")
  ) |> 
  mutate(
    region = toupper(iconv(region, to = "ASCII//TRANSLIT")),
    region = toupper(region), 
    product = toupper(product)
  ) |> 
  select(-Indicador) |> 
  pivot_longer(cols = c(NOV,DEC), names_to = "name_month") |> 
  mutate(
    year = 2008, 
    month = ifelse(name_month == "NOV", 11, 12)
  ) |> 
  select(-name_month)


data_agri <- data |> 
  left_join(
    missing_values,
    by = c("region" ,"product" ,"month","year")
  ) |> 
  mutate(
    Value_prod = ifelse(
      # The price data in Nov. 2008 are the same as that of Oct...
      year == 2008 & month %in% c(11,12),
      yes = as.numeric(value), 
      no = Value_prod
    )
  ) |> 
  select(-value) |> 
  select(-id) |> 
  relocate(
    region, product, year, month, date, 
    Value_prod, surf_m,
    Value_surfR, Value_prices, 
    campaign, campaign_plain, month_campaign
  ) |> 
  filter(
    ! is.na(year), 
    ! region %in% c(
      "TOTAL NACIONAL", "CALLAO", "LIMA METROPOLITANA",
      "PROMEDIO NACIONAL", "LIMA PROVINCIAS"
    )
  )
rm(data)

save(data_agri, file = "../data/output/minagri/data_agri.rda")

# 4. Final Aggregation ----
# -------------------------

load("../data/output/minagri/data_agri.rda")

length(unique(data_agri$region)) * 
  length(unique(data_agri$product)) *
  length(unique(data_agri$date))

# if some rows are missing for the triplet region x product x date: fill with NA
data_agri <- 
  data_agri |> 
  complete(region, product, date)

# Checking if there is any duplicates
data_agri |> 
  group_by(region, product, date) |> 
  count() |> 
  filter(n>1)


data_total <-  
  data_agri |> 
  unique() |> 
  filter(! region == "TOTAL NACIONAL") |> 
    # Adding the english names of the selected crops
  mutate(
    product_eng = case_when(
      product == "ARROZ CÁSCARA"      ~ "Rice",
      product == "MAÍZ AMARILLO DURO" ~ "Dent corn",
      product == "MAÍZ AMILÁCEO"      ~ "Amylaceous corn", 
      product == "PAPA"               ~ "Potato", 
      product == "TRIGO"              ~ "Wheat", 
      product == "YUCA"               ~ "Cassava", 
      TRUE ~ product)
  ) |> 
  group_by(region, product, year) |> 
  # Computing the share of the annual production harvested at month m 
  mutate(
    perc_product = ifelse(Value_prod == 0, NA, Value_prod) / 
      sum( ifelse(Value_prod == 0, NA, Value_prod), na.rm = T)
  ) |> 
  ungroup() |> 
  group_by(region, product, month) |> 
  # Computing the average share of the annual production harvested at month m
  mutate(perc_product_mean = mean(perc_product, na.rm = T)) |> 
   ungroup()


data_total <- data_total |>
  arrange(region, product, date) |> 
  group_by(region) |> 
  mutate(region_id = cur_group_id()) |> 
  ungroup() |> 
    # Computing the log of the quantities and prices
  mutate(
    ln_prices = log(Value_prices + 1), 
    ln_produc = log(Value_prod + 1)
  ) |> 
  relocate(region_id, region, product, date, ln_prices, ln_produc)


data_total <- 
  data_total |> 
  group_by(region, product) |> 
  # Removing regions that do not produce at all
  mutate(tot_prod = sum(Value_prod, na.rm = T)) |> 
  ungroup() |> 
  filter(! tot_prod == 0) |> 
  select(-tot_prod) |> 
  group_by(year, product, region) |> 
  ungroup() |> 
  unique()


data_total <- 
  data_total |> 
  arrange(region, product_eng, date) |> 
  # Computing the difference between planted and harvested surfaces at time t 
  mutate(diff_plant_harv = surf_m - Value_surfR) |> 
  group_by(region, product_eng) |> 
 # Computing the cumulative difference and normalizing the detrended component
  mutate(exposition = cumsum(replace_na(diff_plant_harv, 0))) |> 
  mutate(exposition_trend = as.vector(
    mFilter::hpfilter(
      exposition, freq = 14400, type = "lambda", drift = FALSE
    )$trend)
  ) |> 
  mutate(exposition_detrended = exposition - exposition_trend) |> 
  mutate(
    exposition_norm = (exposition_detrended - min(exposition_detrended)) / 
      (max(exposition_detrended) - min(exposition_detrended))
  ) |> 
  ungroup() 


# Adding labels to the final variables  
data_total <- 
  data_total |> 
  labelled::set_variable_labels(
    region_id = "Region numerical ID",
    region = "Name of the region",
    product = "Name of the crop (in Spanish)",
    date = "Date (YYYY-MM-DD)",
    ln_prices = "Product price (log)",
    ln_produc = "Production (log of tons)",
    year = "Year (YYYY)",
    month = "Month (MM)",
    Value_prod = "Production (tons)",
    surf_m = "Planted Surface during the current month (hectares)",
    surf_lag_calend = "Planted Surface laggued by the growth duration computed from the caledars (hectares)",
    Value_surfR = "Harvested Surface (hectares)",
    Value_prices = "Unit Price (Pesos)",
    campaign = "ID of the planting campaing (starting in August)",
    campaign_plain = "Years of the planting campaing (starting in August)",
    month_campaign = "Month of the planting campaing (August = 1)",
    product_eng = "Name of the Product (in English)",
    perc_product = "Share of the annual production harvested at month m",
    perc_product_mean = "Average share of the annual production harvested at month m",
    diff_plant_harv = "Difference between planted and harvested surfaces during month m",
    exposition = "Cumulative difference between planted and harvested surfaces",
    exposition_trend = "Trend of the exposition using HP filter",
    exposition_detrended = "Difference between the exposition and its trend",
    exposition_norm = "Normalisation of the detrended exposition"
  )


# Saving the final agricultural dataset
save(data_total, file = "../data/output/minagri/dataset_agri_2001_2015.rda")
write_csv(data_total, "../data/output/minagri/dataset_agri_2001_2015.rda")

load("../data/output/minagri/dataset_agri_2001_2015.rda")
