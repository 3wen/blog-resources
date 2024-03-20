# Soya seems to be missing

library(tidyverse)
library(readxl)
N <- list.files(
  path = "../data/raw/Calendario agricola/", 
  pattern = "xls$",
  full.names = TRUE
)


#' Import calendar from the Excel files
#' 
#' @param x full path to a Calendar (Excel file)
import_calendar <- function(x) {
  region <- str_extract(x, "//cal_(.*)\\.xls") |> 
    str_remove("//cal_") |> 
    str_remove("\\.xls")
  
  df_cal_1 <- read_excel(x)
  
  row_prod_mes <- str_which(
    df_cal_1$`CALENDARIO AGRICOLA NACIONAL`, "Producto/Mes"
  )
  ind_first <- first(row_prod_mes)
  ind_last <- last(row_prod_mes)
  
  df_cal_planting <- read_excel(
    x, 
    skip = ind_first, 
    n_max = ind_last-ind_first - 3
  )
  df_cal_planting <- 
    df_cal_planting |> 
    pivot_longer(
      cols = -`Producto/Mes`,
      names_to = "month_spanish",
      values_to = "pct"
    ) |> 
    mutate(region = region, period = "planting")
  
  df_cal_harvest <- read_excel(x, skip = ind_last)
  df_cal_harvest <- 
    df_cal_harvest |> 
    filter(!is.na(`Producto/Mes`)) |> 
    mutate(across(-`Producto/Mes`, ~as.numeric(.))) |> 
    pivot_longer(
      cols = -`Producto/Mes`,
      names_to = "month_spanish",
      values_to = "pct"
    ) |> 
    mutate(region = region, period = "harvest")
  
  df_cal_planting |> 
    bind_rows(df_cal_harvest)
}

calendar <- map(N, import_calendar) |> 
  list_rbind()

calendar <- 
  calendar |> 
  filter(month_spanish != "...14")


spanish_months <- 
  tibble(
    month_spanish = c(
      "Ene", "Feb", "Mar", "Abr", "May", "Jun",
      "Jul", "Ago", "Set", "Oct", "Nov", "Dic"
    ),
    month = 1:12
  )

calendar <- 
  calendar |> 
  left_join(spanish_months) |> 
  select(-month_spanish)

calendar <- calendar |> 
  mutate(
    region = ifelse(region == "lalibertad", "la libertad", region),
    region = ifelse(region == "madrededios", "madre de dios", region),
    region = ifelse(region == "sanmartin", "san martin", region)
  ) |> 
  mutate(region = str_to_upper(region))

calendar <- 
  calendar |> 
  rename(product = `Producto/Mes`) |> 
  mutate(pct = ifelse(is.na(pct), 0, pct))

calendar1 <- calendar |> 
  filter(period == "planting") |> 
  group_by(region, product) |> 
  mutate(val_max = max(pct)) |> 
  ungroup() |> 
  slice(which(pct == val_max)) |> 
  select(-val_max) 

calendar2 <- calendar |> 
  filter(period == "harvest") |> 
  group_by(region, product) |> 
  mutate(val_max = max(pct)) |> 
  ungroup() |> 
  slice(which(pct == val_max)) |> 
  slice(-2) |> 
  slice(-163) |> 
  select(-val_max) |> 
  full_join(calendar1, by = c("region","product")) |> 
  mutate(
    growth_duration = month.x - month.y,
    growth_duration = ifelse(
      growth_duration < 0, 
      yes = month.x + 13 - month.y, 
      no = growth_duration
    ),
    growth_duration = ifelse(growth_duration == 0,12, growth_duration)
  ) |> 
  slice(-which(is.na(growth_duration))) |> 
  mutate(
    region = toupper(iconv(region, to = "ASCII//TRANSLIT")),
    product = toupper(product)
  )


calendar3 <- calendar |> 
  filter(period == "harvest") |> 
  mutate(month =   str_c("month", month)) |> 
  pivot_wider(names_from = month, values_from = pct) |> 
  mutate(
    cum_sum1  = month1, 
    cum_sum2  = month2 + cum_sum1, 
    cum_sum3  = month3 + cum_sum2, 
    cum_sum4  = month4 + cum_sum3, 
    cum_sum5  = month5 + cum_sum4, 
    cum_sum6  = month6 + cum_sum5, 
    cum_sum7  = month7 + cum_sum6, 
    cum_sum8  = month8 + cum_sum7, 
    cum_sum9  = month9 + cum_sum8, 
    cum_sum10 = month10 + cum_sum9, 
    cum_sum11 = month11 + cum_sum10, 
    cum_sum12 = month12 + cum_sum11) |> 
  select(
    product, region,
    cum_sum1, cum_sum2, cum_sum3, cum_sum4,cum_sum5, cum_sum6,
    cum_sum7, cum_sum8, cum_sum9, cum_sum10, cum_sum11, cum_sum12
  ) |> 
  pivot_longer(
    cols = c(
      cum_sum1, cum_sum2, cum_sum3, cum_sum4,cum_sum5, cum_sum6,
      cum_sum7, cum_sum8, cum_sum9, cum_sum10, cum_sum11, cum_sum12
    ), 
    names_to = "month"
  ) |> 
  rename(perc_cum_harv = value) |> 
  mutate(
    month = case_when(
      month == "cum_sum1"  ~ 1, 
      month == "cum_sum2"  ~ 2, 
      month == "cum_sum3"  ~ 3, 
      month == "cum_sum4"  ~ 4, 
      month == "cum_sum5"  ~ 5, 
      month == "cum_sum6"  ~ 6, 
      month == "cum_sum7"  ~ 7, 
      month == "cum_sum8"  ~ 8, 
      month == "cum_sum9"  ~ 9, 
      month == "cum_sum10" ~ 10, 
      month == "cum_sum11" ~ 11, 
      month == "cum_sum12" ~ 12, 
    )
  )

calendar4 <- calendar |> 
  filter(period == "planting") |> 
  mutate(month =   str_c("month", month)) |> 
  pivot_wider(names_from = month, values_from = pct) |> 
  mutate(
    cum_sum1  = month8, 
    cum_sum2  = month9 + cum_sum1, 
    cum_sum3  = month10 + cum_sum2, 
    cum_sum4  = month11 + cum_sum3, 
    cum_sum5  = month12 + cum_sum4, 
    cum_sum6  = month1 + cum_sum5, 
    cum_sum7  = month2 + cum_sum6, 
    cum_sum8  = month3 + cum_sum7, 
    cum_sum9  = month4 + cum_sum8, 
    cum_sum10 = month5 + cum_sum9, 
    cum_sum11 = month6 + cum_sum10, 
    cum_sum12 = month7 + cum_sum11
  ) |> 
  select(
    product, region, 
    cum_sum1, cum_sum2, cum_sum3, cum_sum4,cum_sum5, cum_sum6, 
    cum_sum7, cum_sum8, cum_sum9, cum_sum10, cum_sum11, cum_sum12
  ) |> 
  pivot_longer(
    cols = c(
      cum_sum1, cum_sum2, cum_sum3, cum_sum4,cum_sum5, cum_sum6, 
      cum_sum7, cum_sum8, cum_sum9, cum_sum10, cum_sum11, cum_sum12
    ), names_to = "month") |> 
  rename(perc_cum_plan = value) |> 
  mutate(
    month = case_when(
      month == "cum_sum1"  ~ 8, 
      month == "cum_sum2"  ~ 9, 
      month == "cum_sum3"  ~ 10, 
      month == "cum_sum4"  ~ 11, 
      month == "cum_sum5"  ~ 12, 
      month == "cum_sum6"  ~ 1, 
      month == "cum_sum7"  ~ 2, 
      month == "cum_sum8"  ~ 3, 
      month == "cum_sum9"  ~ 4, 
      month == "cum_sum10" ~ 5, 
      month == "cum_sum11" ~ 6, 
      month == "cum_sum12" ~ 7, 
    )
  )

dir.create("../data/output/Calendario agricola/")
save(calendar, file = "../data/output/Calendario agricola/calendar.rda")
save(calendar2, file = "../data/output/Calendario agricola/calendar2.rda")
save(calendar3, file = "../data/output/Calendario agricola/calendar3.rda")
save(calendar4, file = "../data/output/Calendario agricola/calendar4.rda")
