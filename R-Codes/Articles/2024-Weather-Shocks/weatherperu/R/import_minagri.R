#' Importing data where table is missing for a given month by taking its value
#' in the excel file of the next year.
#'
#' @param y last digit of the year (20..)
#' @param mm month (numeric)
#' @param type type of variable ("Production" for production, "Superficies_R"
#'   for harvested surface, "Superficies" for planted surface)
#'
#' @importFrom dplyr relocate select mutate rename
#' @importFrom sringr str_remove str_c
missing_table <- function(y,
                          mm,
                          type = c("Production", "Superficies_R",
                                   "Superficies")) {
  data_mm <- NULL

  for (ii in 0:1) {
    if (mm == 12) {
      m_1 <- mm - 1
      y1  <- y + 1
      year <- 2000 + y
      year1 <- 2000 + y + 1

      if (type == "Production") {
        ## Production, m==12----
        # File name of the next year
        file_name <- str_c(
          "../data/raw/minagri/", type, "/", type, "_", year1, ".xlsx"
        )
        sheet <- str_c(mm, year1, ii + 1)
        data_m_y1 <- import_monthly_regional_values_P_year_P(
          sheet_name = sheet,
          file = file_name,
          anneesup = 0
        ) |>
          mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
          mutate(value_m_y1 = as.numeric(gsub(" ", "", value))) |>
          select(-value)

        sheet <- str_c(m_1, year1, ii + 1)
        data_m_1_y1 <- import_monthly_regional_values_P_year_P(
          sheet_name = sheet,
          file = file_name,
          anneesup = 0
        ) |>
          mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
          mutate(value_m_1_y1 = as.numeric(gsub(" ", "", value))) |>
          rename(month_1 = month) |>
          select(-value)

        # Current year
        file_name <- str_c(
          "../data/raw/minagri/", type, "/", type, "_", year, ".xlsx"
        )
        sheet <- str_c(m_1, year, ii + 1)
        data_m_1_y <- import_monthly_regional_values_P_year_P(
          sheet_name = sheet,
          file = file_name,
          anneesup = 0
        ) |>
          mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
          mutate(value_m_1_y = as.numeric(gsub(" ", "", value))) |>
          select(-value)

        data_mm_y <- merge(data_m_y1,data_m_1_y1) |>
          mutate(diff = value_m_y1 - value_m_1_y1) |>
          select(region, product, diff) |>
          merge(data_m_1_y) |>
          mutate(value_num = value_m_1_y + diff) |>
          select(-diff, - value_m_1_y) |>
          mutate(month = as.numeric(month + 1)) |>
          relocate(product, .after = month)

        data_mm <- rbind(data_mm, data_mm_y)
      }

      if (type == "Superficies_R") {
        ## Superficies_R, m==12----
        # Following year
        file_name <- str_c(
          "../data/raw/minagri/Surface_R/", type, "_", year1, ".xlsx"
        )
        sheet <- str_c(mm,year1, ii + 1)
        data_m_y1 <- import_monthly_regional_values_year_SR(
          sheet_name = sheet,
          file = file_name,
          anneesup = 0
        ) |>
          mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
          mutate(value_m_y1 = as.numeric(gsub(" ", "", value))) |>
          select(-value)

        sheet <- str_c(m_1, year1, ii + 1)
        data_m_1_y1 <- import_monthly_regional_values_year_SR(
          sheet_name = sheet,
          file = file_name,
          anneesup = 0
        ) |>
          mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
          mutate(value_m_1_y1 = as.numeric(gsub(" ", "", value))) |>
          rename(month_1 = month) |>
          select(-value)

        # Current year
        file_name <- str_c(
          "../data/raw/minagri/Surface_R/", type, "_", year, ".xlsx"
        )
        sheet <- str_c(m_1, year, ii + 1)
        data_m_1_y <- import_monthly_regional_values_year_SR(
          sheet_name = sheet,
          file = file_name,
          anneesup = 0)  |>
          mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
          mutate(value_m_1_y = as.numeric(gsub(" ", "", value))) |>
          select(-value)

        data_mm_y <- merge(data_m_y1, data_m_1_y1) |>
          mutate(diff = value_m_y1 - value_m_1_y1) |>
          select(region, product, diff) |>
          merge(data_m_1_y) |>
          mutate(value_num = value_m_1_y + diff) |>
          select(-diff, - value_m_1_y) |>
          mutate(month = as.numeric(month + 1)) |>
          relocate(product, .after = month)

        data_mm <- rbind(data_mm, data_mm_y)
      }

      if (type == "Superficies") {
        ## Superficies, m==12----
        # Following year
        file_name <- str_c(
          "../data/raw/minagri/Surface/", type, "_", year1, ".xlsx"
        )

        sheet <- str_c(mm, year1, ii + 1)
        data_m_y1 <- import_monthly_regional_values_year_S(
          sheet_name = sheet,
          file = file_name,
          anneesup = 0
        ) |>
          mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
          mutate(value_m_y1 = as.numeric(gsub(" ", "", value))) |>
          select(-value)

        sheet <- str_c(m_1, year1, ii + 1)
        data_m_1_y1 <- import_monthly_regional_values_year_S(
          sheet_name = sheet,
          file = file_name,
          anneesup = 0
        ) |>
          mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
          mutate(value_m_1_y1 = as.numeric(gsub(" ", "", value))) |>
          rename(month_1 = month) |>
          select(-value)

        # Current year
        file_name <- str_c(
          "../data/raw/minagri/Surface/", type, "_", year, ".xlsx"
        )
        sheet <- str_c(m_1, year, ii + 1)
        data_m_1_y <- import_monthly_regional_values_year_S(
          sheet_name = sheet,
          file = file_name,
          anneesup = 0
        )  |>
          mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
          mutate(value_m_1_y = as.numeric(gsub(" ", "", value))) |>
          select(-value)

        data_mm_y <- merge(data_m_y1, data_m_1_y1) |>
          mutate(diff = value_m_y1 - value_m_1_y1) |>
          select(region, product, diff) |>
          merge(data_m_1_y) |>
          mutate(value_num = value_m_1_y + diff) |>
          select(-diff, - value_m_1_y) |>
          mutate(month = as.numeric(month + 1)) |>
          relocate(product, .after = month)

        data_mm <- rbind(data_mm, data_mm_y)
      }
    } else {
      # If m != 12
      m_1 <- mm - 1
      m1  <- mm + 1
      year <- 2000 + y

      if (type == "Production") {
        ## Production, m!=12----
        file_name <- str_c(
          "../data/raw/minagri/", type, "/", type, "_", year, ".xlsx"
        )

        # previous month
        sheet <- str_c(
          str_pad(m_1, width = 2, side = "left", pad = "0"),
          year,
          ii + 1
        )
        data_m_1 <- import_monthly_regional_values_P_year_P(
          sheet_name = sheet,
          file = file_name,
          anneesup = 0
        ) |>
          mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
          mutate(value_m_1 = as.numeric(gsub(" ", "", value))) |>
          rename(month_1 = month) |>
          select(-value)

        # following month
        sheet <- str_c(
          str_pad(m1, width = 2, side = "left", pad = "0"),
          year,
          ii + 1
        )
        data_m1 <- import_monthly_regional_values_P_year_P(
          sheet_name = sheet,
          file = file_name,
          anneesup = 0
        ) |>
          mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
          mutate(value_m1 = as.numeric(gsub(" ", "", value))) |>
          rename(month1 = month) |>
          select(-value)
      }

      if (type == "Superficies_R"){
        ## Superficies_R, m!=12----
        file_name <- str_c(
          "../data/raw/minagri/Surface_R/", type,"_", year, ".xlsx"
        )

        # previous month
        sheet <- str_c(
          str_pad(m_1, width = 2, side = "left", pad = "0"),
          year,
          ii + 1
        )
        data_m_1 <- import_monthly_regional_values_year_SR(
          sheet_name = sheet,
          file = file_name,
          anneesup = 0
        ) |>
          mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
          mutate(value_m_1 = as.numeric(gsub(" ", "", value))) |>
          rename(month_1 = month) |>
          select(-value)

        # following month
        sheet <- str_c(
          str_pad(m1, width = 2, side = "left", pad = "0"),
          year,
          ii + 1
        )
        data_m1 <- import_monthly_regional_values_year_SR(
          sheet_name = sheet,
          file = file_name,
          anneesup = 0
        ) |>
          mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
          mutate(value_m1 = as.numeric(gsub(" ", "", value))) |>
          rename(month1 = month) |>
          select(-value)
      }

      if (type == "Superficies") {
        ## Superficies, m!=12----
        file_name <- str_c(
          "../data/raw/minagri/Surface/", type, "_", year, ".xlsx"
        )

        # previous month
        sheet <- str_c(
          str_pad(m_1, width = 2, side = "left", pad = "0"),
          year,
          ii + 1
        )
        data_m_1 <- import_monthly_regional_values_year_S(
          sheet_name = sheet,
          file = file_name,
          anneesup = 0
        ) |>
          mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
          mutate(value_m_1 = as.numeric(gsub(" ", "", value))) |>
          rename(month_1 = month) |>
          select(-value)

        # following month
        sheet <- str_c(
          str_pad(m1, width = 2, side = "left", pad = "0"),
          year,
          ii + 1
        )
        data_m1 <- import_monthly_regional_values_year_S(
          sheet_name = sheet,
          file = file_name,
          anneesup = 0
        ) |>
          mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
          mutate(value_m1 = as.numeric(gsub(" ", "", value))) |>
          rename(month1 = month) |>
          select(-value)
      }

      data_mm_y <- merge(data_m1,data_m_1) |>
        mutate(
          month = as.numeric(month_1 + 1),
          value_num = (value_m1 - value_m_1) / 2 + value_m_1) |>
        select(-value_m1, -month1, -value_m_1, -month_1) |>
        relocate(product, .after = month)

      data_mm <- rbind(data_mm, data_mm_y)

    }
  }
  data_mm
}

#' Format header (removing unecessary values)
#'
#' @param x (vector) of string
#'
#' @importFrom tidyr replace_na
#' @importFrom stringr str_replace_all str_remove str_c str_to_lower str_trim
format_header <- function(x) {
  if (all(is.na(x))) {
    return("")
  }
  replace_na(x, "") |>
    str_replace_all("\\.{3}", "") |>
    str_remove("[[:digit:]]/") |>
    str_c(collapse = " ") |>
    str_to_lower() |>
    str_replace_all("[[:blank:]]{2,}", " ") |>
    str_remove("- ") |>
    str_trim()
}

# Regional Production----

#' Import monthly regional agricultural values from the Excel file, for a given
#' month
#'
#' @param sheet_name name of the Excel sheet
#' @param file path to the Excel File
#' @param anneesup integer, if different from 1, keeping only the last year available
#'
#' @importFrom dplyr mutate select rename first across summarise filter_all
#'   any_vars
#' @importFrom stringr str_c str_sub str_which regex str_detect
#' @importFrom readxl read_excel
#' @importFrom tidyselect everything
#' @importFrom tidyr pivot_longer fill
import_monthly_regional_values_P_year_P <- function(sheet_name,
                                                    file,
                                                    anneesup) {
  # The first two digits of the name: month
  # the next four: the four digits of the year
  # last digit: index of the sheet for a given month and
  # year (we will note use it)
  year  <- str_sub(sheet_name, 3, 6) |> as.numeric()
  month <- str_sub(sheet_name, 1, 2) |> as.numeric()
  tmp   <- suppressMessages(
    read_excel(
      path = file,
      sheet = sheet_name,
      col_types = "text", n_max = 15, col_names = FALSE
    )
  )

  # We can use the first occurrence of "Años" to determine
  # the beginning of the table
  ind_row_year <- str_which(tmp[[3]], regex("años?", ignore_case = TRUE)) |>
    first()
  if (year == 2005) {
    ind_row_year <- str_which(tmp[[2]], regex("años?", ignore_case = TRUE)) |>
      first()
  }

  # The first row of the header of the table is contained in the previous line
  skip_head <- ind_row_year

  # The body of the table
  prod_region_tmp <-
    suppressMessages(
      read_excel(
        path = file,
        sheet = sheet_name,
        col_types = "text", skip = skip_head, col_names = F
      )
    )

  header_1 <- suppressMessages(
    read_excel(
      path = file,
      sheet = sheet_name,
      skip = skip_head-2, n_max = 3, col_names = F
    )
  )
  if (year < 2005) {
    header_1 <- header_1[-nrow(header_1), ]
  }

  header_2 <- header_1 |>
    summarise(
      across(
        .cols = everything(),
        .fns = ~format_header(.x)
      )
    )

  colnames(prod_region_tmp) <- as.character(header_2)
  if (colnames(prod_region_tmp)[2] == "") {
    colnames(prod_region_tmp)[2] <- "departamento"
  }
  if (any(str_detect(colnames(prod_region_tmp), "departamento"), na.rm = T)) {
    ind_dep_current <- which(str_detect(colnames(prod_region_tmp), "departamento"))
    colnames(prod_region_tmp)[ind_dep_current] <- "departamento"
  }

  # Removing columns with no name
  ind <- !is.na(colnames(prod_region_tmp)) & (colnames(prod_region_tmp) != "")
  prod_region_tmp <-
    prod_region_tmp |>
    dplyr::select(!!!colnames(prod_region_tmp)[ind])

  # Removing rows with all NAs
  prod_region_tmp <-
    prod_region_tmp |>
    filter_all(any_vars(!is.na(.)))

  # Removing rows where "Continúa" is found
  prod_region_tmp <-
    prod_region_tmp |>
    filter_all(
      any_vars(
        !str_detect(., regex(pattern = "Continúa", ignore_case = TRUE))
      )
    )

  # Removing the first colomn if row index
  if (str_detect(colnames(prod_region_tmp)[1], "[aeiou]") == FALSE) {
    prod_region_tmp <- prod_region_tmp[-1]
  }

  # The name of the first column differs accross sheets (departemento or region)
  name_first_col <- colnames(prod_region_tmp)[1]

  if (year < 2015) {
    if (! name_first_col %in% c("departamento", "región")) {
      warning(str_c("Issue with sheet: ", sheet_name))
      return(NULL)
    }
  }

  prod_region_tmp <-
    prod_region_tmp |>
    rename(region := !!name_first_col)

  if (any(str_detect(colnames(prod_region_tmp), "^año$"))) {
    prod_region_tmp <-
      prod_region_tmp |> rename(year = año)
  } else {
    prod_region_tmp <-
      prod_region_tmp |> rename(year = años)
  }

  prod_region_tmp <-
    prod_region_tmp |>
    filter(!is.na(year), ! year %in% c("Años", "Año"))

  # The sheet may contain values for the department Cajamarca
  # AND the capital of that department named also Cajamarca
  prod_region_tmp <-
    prod_region_tmp |>
    group_by(region, year) |>
    mutate(
      region = ifelse(
        region == "Cajamarca" & row_number() == 1,
        yes = "Cajamarca_R",
        no = region
      )
    ) |>
    ungroup()

  prod_region_tmp <-
    prod_region_tmp |>
    mutate(
      region = ifelse(region == "Lima provincias", yes = "Lima", no = region)
    )


  # The production of each product is given in two rows, but the product name
  # is not repeated
  prod_region_tmp <-
    prod_region_tmp |>
    fill(region, .direction = "down")
  if (year == 2005) {
    prod_region_tmp$region[1]<- "Total Nacional"
  }

  # Removing sub-regional data
  prod_region_tmp <-
    prod_region_tmp |>
    filter(! region %in% c("Cajamarca", "Chota", "Jaén",
                           "Abancay","Andahuaylas")
    )

  if (year == 2015 & any(colnames(prod_region_tmp) == "región")) {
    q <- which(colnames(prod_region_tmp) == "región")
    if (q > 0) {
      prod_region_tmp <- prod_region_tmp |>
        dplyr::select(-all_of(q))
    }
  }

  resul <-
    prod_region_tmp |>
    pivot_longer(cols = -c(region, year), names_to = "product")

  if (anneesup == 1) {
    resul <-
      resul |>
      filter(year == max(year))
  } else {
    # Keeping only the last year available
    resul <-
      resul |>
      filter(year == min(year))
  }

  resul <-
    resul |>
    mutate(
      product = ifelse(product == "arveja gr. seco", yes = "arveja grano seco", no = product),
      product = ifelse(product == "frijol grano seco**", yes = "frijol grano seco", no = product),
      product = ifelse(product == "frijol palo", yes = "frijol de palo", no = product),
      product = ifelse(product == "frijol castlla", yes = "frijol castilla", no = product),
      product = ifelse(product == "lentaja", yes = "lenteja", no = product),
      product = ifelse(product == "frijol gr. seco", yes = "frijol grano seco", no = product),
      product = ifelse(product == "haba gr. seco", yes = "haba grano seco", no = product),
      product = ifelse(product == "maíz a. duro", yes = "maíz amarillo duro", no = product),
      product = ifelse(product == "arveja gr. verde", yes = "arveja grano verde", no = product),
      product = ifelse(product == "arveja gr. verde", yes = "arveja grano verde", no = product),
      product = ifelse(product == "haba gr. verde", yes = "haba grano verde", no = product),
      product = ifelse(product == "espá-rrago", yes = "espárrago", no = product),
      product = ifelse(product == "manda-rina", yes = "mandarina", no = product),
      product = ifelse(product == "maíz duro", yes = "maíz amarillo duro", no = product),
      product = ifelse(product == "maíz am. duro", yes = "maíz amarillo duro", no = product),
      product = ifelse(product == "pallar gr. seco", yes = "pallar grano seco", no = product),
      product = ifelse(product == "pallar seco", yes = "pallar grano seco", no = product),
      product = ifelse(product == "maíz amarillo duro", yes = "maíz amarillo duro", no = product)
    )

  resul |>
    dplyr::mutate(month = month) |>
    dplyr::select(region, year, month, product, value)
}


#' Import monthly regional agricultural values from the Excel file,
#' for all the months of the file.
#'
#' @param file path to the Excel File
#' @param anneesup integer, if different from 1, keeping only the last year available
#' @param timescale
#'
#' @importFrom dplyr mutate select rename filter case_when arrange desc
#' @importFrom stringr str_remove str_detect
#' @importFrom readxl excel_sheets
#' @importFrom purrr map list_rbind
#' @importFrom tidyr pivot_wider pivot_longer
import_monthly_regional_values_P <- function(file,
                                             anneesup,
                                             timescale) {

  sheet_names <- excel_sheets(file)
  prod_region_monthly <- map(
    sheet_names,
    import_monthly_regional_values_P_year_P,
    file = file,
    anneesup = anneesup
  ) |>
    list_rbind()

  # Cleaning years and changing type of values: from str to num
  prod_region_monthly <-
    prod_region_monthly |>
    mutate(
      year = str_remove(year, "p/?") |> as.numeric(),
      value_num = as.numeric(gsub(" ", "", value)),
      month = as.numeric(month)
    )

  # Values in 2014 to 2016 are expressed in thousands of tonnes
  # These need to be expressed in tonnes
  prod_region_monthly <-
    prod_region_monthly |>
    mutate(
      value_num = ifelse(year %in% 2014:2015, value_num * 10^3, value_num)
    )

  # In addition, for the `Production_2014.xlsx` file, months 10 to 12
  # report values in thousands of tonnes as well
  if (str_detect(file, "Production_2014.xlsx$")) {
    prod_region_monthly <-
      prod_region_monthly |>
      mutate(
        value_num = ifelse(
          month %in% c(10,11,12),
          yes = value_num * 10^3,
          no = value_num
        )
      )
  }


  # Removing the `value` columns to keep only the column with numerical values
  prod_region_monthly <-
    prod_region_monthly |>
    dplyr::select(-value)

  if (str_detect(file, "Production_2002.xlsx$")) {
    for (i in c(3,6,9,12)) {
      temp <- missing_table(y = 2, mm = i, type = "Production")
      prod_region_monthly <- rbind(prod_region_monthly, temp)
    }
  }

  if (str_detect(file, "Production_2003.xlsx$")) {
    temp <- missing_table(y = 3, mm = 3, type = "Production")
    prod_region_monthly <- rbind(prod_region_monthly, temp)

  }

  if (str_detect(file, "Production_2008.xlsx$")) {
    # Problem with value in June 2007 for Cassava: same value as in May
    # Let us put the value for June as NA
    # (For some regions)
    prod_region_monthly <-
      prod_region_monthly |>
      mutate(
        value_num = ifelse(
          month == 6 & product == "yuca" & region %in% c(
            "Apurímac", "Arequipa", "Ayacucho", "Cusco", "Huancavelica",
            "Junín", "Loreto", "Madre de Dios", "Moquegua", "Moquegua",
            "Puno", "San Martín", "Tacna", "Ucayali"),
          yes = NA,
          no = value_num
            )
      )
  }

  # Cleaning region names
  # prod_region_monthly$region |> unique() |> sort()
  prod_region_monthly <-
    prod_region_monthly |>
    mutate(region = replace(region, region=="Apurimac", "Apurímac")) |>
    mutate(region = replace(region, region=="Cajamarca_R", "Cajamarca")) |>
    mutate(region = replace(
      x = region,
      list = region %in% c("Nacional", "Total nacional", "TOTAL NACIONAL"),
      values = "Total Nacional")
    ) |>
    filter(! product == 0)


  if (str_detect(file, "Production_201(4|5).xlsx")) {
    prod_region_monthly <- prod_region_monthly |>
      mutate(
        product = case_when(
          str_detect(product, "arroz")            ~  "arroz cáscara",
          str_detect(product, "banano")           ~  "plátano",
          str_detect(product, "café")             ~  "café",
          str_detect(product, "banano")           ~  "plátano",
          str_detect(product, "caña")             ~  "caña de azúcar",
          str_detect(product, "chocho")           ~  "chocho",
          str_detect(product, "gar")              ~  "garbanzo",
          str_detect(product, "caña")             ~  "caña de azúcar",
          str_detect(product, "algodón")          ~  "algodón rama",
          str_detect(product, "maíz amilaceo")    ~  "maíz amiláceo",
          str_detect(product, "maíz amílaceo")    ~  "maíz amiláceo",
          str_detect(product, "caña de azúcar para azúcar")   ~  "caña de azúcar",
          str_detect(product, "arveja seca")      ~  "arveja grano seco",
          str_detect(product, "arveja verde")     ~  "arveja grano verde",
          str_detect(product, "loctao")           ~  "frijol loctao",
          str_detect(product, "frijol seco")      ~  "frijol grano seco",
          str_detect(product, "frijol castill")   ~  "frijol castilla",
          str_detect(product, "haba seca")        ~  "haba grano seco",
          str_detect(product, "haba seco")        ~  "haba grano seco",
          str_detect(product, "haba verde")       ~  "haba grano verde",
          str_detect(product, "limón sutil")      ~  "limón",
          str_detect(product, "haba verde")       ~  "haba grano verde",
          str_detect(product, "algodón rama")     ~  "algodón rama",
          str_detect(product, "café")             ~  "café",
          str_detect(product, "cañihua")          ~  "cañihua",
          str_detect(product, "espárrago")        ~  "espárrago",
          str_detect(product, "limón")            ~  "limón",
          str_detect(product, "maíz amiláceo")    ~  "maíz amiláceo",
          str_detect(product, "maíz choclo")      ~  "maíz choclo",
          str_detect(product, "piña")             ~  "piña",
          str_detect(product, "zaran-daja")       ~  "zarandaja",
          TRUE ~ product
        )
      )

  }

  Production <- prod_region_monthly |>
    arrange(region, product, desc(month)) |>
    mutate(value_num = ifelse(is.na(value_num), yes = 0, no = value_num))

  if (str_detect(file, "Production_2008.xlsx$")) {
    # Problem with value in June 2007 for Cassava: same value as in May
    # Let us put the value for June as NA
    # (For some regions)
    Production <-
      Production |>
      mutate(
        value_num = ifelse(
          month == 6 & product == "yuca" & region %in% c(
            "Apurímac", "Arequipa", "Ayacucho", "Cusco", "Huancavelica",
            "Junín", "Loreto", "Madre de Dios", "Moquegua", "Moquegua",
            "Puno", "San Martín", "Tacna", "Ucayali"),
          yes = NA,
          no = value_num
        )
      )
  }

  Production <- Production |>
    pivot_wider(names_from = month, values_from = value_num)

  if (timescale == 1) {
    # The production data in the file is a cumulative sum over the months
    Production <-
      cbind(
        Production[1:3],
        Production[4:14] - Production[5:15],
        Production[15]
      ) |>
      pivot_longer(cols = -c(region, year, product), names_to = "month")
    Production  <-
      Production[with(Production, order(region, year, as.numeric(month), product, value)), ]
  }

  if (timescale == 3) {
    Production <-
      cbind(
        Production[1:3],
        Production[4] - Production[7],
        Production[7] - Production[10],
        Production[10] - Production[13],
        Production[13]
      ) |>
      pivot_longer(cols = -c(region, year, product), names_to = "trim")
    Production  <-
      Production[with(Production, order(region, year, as.numeric(trim), product, value)), ]
  }

  if (timescale == 4) {
    Production <-
      cbind(
        Production[1:3],
        Production[4] - Production[8],
        Production[8] - Production[12],
        Production[12]
      ) |>
      pivot_longer(cols = -c(region, year, product), names_to = "quadrim")
    Production  <-
      Production[with(Production, order(region, year, as.numeric(quadrim), product, value)), ]
  }

  if (timescale == 6) {
    Production <-
      cbind(
        Production[1:3],
        Production[4] - Production[10],
        Production[10]
      ) |>
      pivot_longer(cols = -c(region, year, product), names_to = "biannual")
    Production  <-
      Production[with(Production, order(region, year, as.numeric(biannual), product, value)), ]
  }

  if (timescale == 12) {
    Production <-
      cbind(Production[1:3], Production[4]) |>
      rename(Prod_annual = "12")
  }

  if (str_detect(file, "Production_2015.xlsx$")) {
    if(timescale == 12) {
      Production <- Production |>
        filter(!is.na(Prod_annual))
    } else {
      Production <- Production |>
        filter(!is.na(value))
    }

  }
  Production
}

# Regional Planted Surface----


#' Import monthly regional agricultural values from the Excel file,
#' for a given month
#'
#' @param sheet_name name of the Excel sheet
#' @param file path to the Excel File
#' @param anneesup integer, if different from 1, keeping only the last year available
#'
#' @importFrom dplyr mutate select rename first across summarise filter_all
#'   any_vars
#' @importFrom stringr str_c str_sub str_which regex str_detect
#' @importFrom readxl read_excel
#' @importFrom tidyselect everything
#' @importFrom tidyr pivot_longer fill
import_monthly_regional_values_year_S <- function(sheet_name,
                                                  file,
                                                  anneesup) {
  # The first two digits of the name: month
  # the next four: the four digits of the year
  # last digit: index of the sheet for a given month and year (we will note use it)

  month <- str_sub(sheet_name, 1, 2) |> as.numeric()
  year  <- str_sub(sheet_name, 3, 6) |> as.numeric()

  tmp   <- suppressMessages(
    read_excel(
      path = file,
      sheet = sheet_name,
      col_types = "text", n_max = 15, col_names = FALSE)
  )

  # We can use the first occurrence of "Campaña" to determine the beginning of the table
  ind_row_year <- str_which(tmp[[3]], regex("Cam?", ignore_case = TRUE)) |>
    first()
  if (sheet_name == "062015") {ind_row_year <-  7}

  # The first row of the header of the table is contained in the previous line
  skip_head <- ind_row_year

  # The body of the table
  sup_region_tmp <-
    suppressMessages(
      read_excel(
        path = file,
        sheet = sheet_name,
        col_types = "text", skip = skip_head, col_names = F
      )
    )

  header_1 <-
    suppressMessages(
      read_excel(
        path = file,
        sheet = sheet_name,
        skip = skip_head-2, n_max = 3, col_names = F
      )
    )

  if (sheet_name == "0420112") {
    header_1 <-
      suppressMessages(
        read_excel(
          path = file,
          sheet = sheet_name,
          skip = skip_head-2, n_max = 2, col_names = F
        )
      )
  }

  if (sheet_name == "062015") {
    header_1 <-
      suppressMessages(
        read_excel(
          path = file,
          sheet = sheet_name,
          skip = skip_head, n_max = 1, col_names = F
        )
      )
  }

  if (str_detect(file, "Superficies_2016.xlsx$")) {
    header_1 <-
      suppressMessages(
        read_excel(
          path = file,
          sheet = sheet_name,
          skip = skip_head-1, n_max = 1, col_names = F
        )
      )
  }

  if (year < 2005) {
    header_1 <- header_1[-dim(header_1)[1], ]
  }

  header_2 <-
    header_1 |>
    summarise(
      across(
        .cols = everything(),
        .fns = ~format_header(.x)
      )
    )

  colnames(sup_region_tmp) <- as.character(header_2)
  if (colnames(sup_region_tmp)[2] == ""){
    colnames(sup_region_tmp)[2] <- "departamento"
  }
  if (any(str_detect(colnames(sup_region_tmp), "departamento"), na.rm = T)) {
    ind_dep_current <- which(str_detect(colnames(sup_region_tmp), "departamento"))
    colnames(sup_region_tmp)[ind_dep_current] <- "departamento"
  }

  # Removing columns with no name
  ind <- !is.na(colnames(sup_region_tmp)) & (colnames(sup_region_tmp) != "")
  sup_region_tmp <-
    sup_region_tmp |>
    dplyr::select(!!!colnames(sup_region_tmp)[ind])


  if (str_detect(file, "Superficies_2016.xlsx$")) {
    sup_region_tmp <- sup_region_tmp |>
      mutate(
        campaña = ifelse(
          str_detect(campaña, regex(pattern = "Ago 14", ignore_case = TRUE)),
          yes = "14-15",
          no = campaña
        ),
        campaña = ifelse(
          str_detect(campaña, regex(pattern = "Ago 2014", ignore_case = TRUE)),
          yes = "14-15",
          no = campaña
        ),
        campaña = ifelse(
          str_detect(campaña, regex(pattern = "Ago 15", ignore_case = TRUE)),
          yes = "15-16",
          no = campaña
        ),
        campaña = ifelse(
          str_detect(campaña, regex(pattern = "Ago 2015", ignore_case = TRUE)),
          yes = "15-16",
          no = campaña
        ),
        campaña = ifelse(
          str_detect(campaña, regex(pattern = "Ago 16", ignore_case = TRUE)),
          yes = "16-17",
          no = campaña
        ),
        campaña = ifelse(
          str_detect(campaña, regex(pattern = "Ago 2016", ignore_case = TRUE)),
          yes = "16-17",
          no = campaña
        )
      )
  }

  # Removing rows with all NAs
  sup_region_tmp <-
    sup_region_tmp |>
    filter_all(any_vars(!is.na(.)))

  # Removing rows where "Continúa" is found
  sup_region_tmp <-
    sup_region_tmp |>
    filter_all(
      any_vars(
        !str_detect(., regex(pattern = "Continúa", ignore_case = TRUE))
      )
    )

  # Removing the first colomn if row index
  if (str_detect(colnames(sup_region_tmp)[1], "[aeiou]") == FALSE) {
    sup_region_tmp <-   sup_region_tmp[-1]
  }
  # The name of the first column differs accross sheets (departemento or region)
  name_first_col <- colnames(sup_region_tmp)[1]
  if (! name_first_col %in% c("departamento", "región")) {
    warning(str_c("Issue with sheet: ", sheet_name))
    return(NULL)
  }

  sup_region_tmp <-
    sup_region_tmp |>
    rename(region := !!name_first_col)

  if (sheet_name == "062015") {
    sup_region_tmp <-
      sup_region_tmp |>
      rename(campaña = colnames(sup_region_tmp)[2])
  }

  sup_region_tmp$campaña <-
    str_c("20",str_sub(sup_region_tmp$campaña, 1, 2)) |>
    as.numeric()

  sup_region_tmp <-
    sup_region_tmp |> rename(year = campaña)

  # The sheet may contain values for the department Cajamarca
  # AND the capital of that department named also Cajamarca
  sup_region_tmp <-
    sup_region_tmp |>
    group_by(region, year) |>
    mutate(
      region = ifelse(
        region == "Cajamarca" & row_number() == 1,
        yes = "Cajamarca_R",
        no = region
      )
    ) |>
    ungroup()

  sup_region_tmp <-
    sup_region_tmp |>
    mutate(
      region = ifelse(region == "Lima provincias", yes = "Lima", no = region)
    )

  sup_region_tmp <-
    sup_region_tmp |>
    filter(!is.na(year), ! year %in% "campaña")

  # The production of each product is given in two rows, but the product name is not repeated
  sup_region_tmp <-
    sup_region_tmp |>
    fill(region, .direction = "down")

  # Removing sub-regional data
  sup_region_tmp <-
    sup_region_tmp |>
    filter(
      ! region %in% c("Cajamarca", "Chota", "Jaén", "Abancay","Andahuaylas")
    )
  if (any(str_detect(colnames(sup_region_tmp), regex(pattern = "ago", ignore_case = TRUE)) == T)) {
    p <- which(str_detect(colnames(sup_region_tmp), regex(pattern = "ago", ignore_case = TRUE))==T)
    colnames(sup_region_tmp)[p] <- "TOTAL"
  }

  resul <-
    sup_region_tmp |>
    pivot_longer(cols = -c(region, year), names_to = "product")

  if (anneesup == 1) {
    resul <-
      resul |>
      filter(year == max(year))
  } else {
    resul <-
      resul |>
      filter(year == min(year))
  }

  resul <-
    resul |> rename(campana = year)

  resul <- resul |>
    mutate(month = month) |>
    dplyr::select(region, campana, month, product, value)

  resul <-
    resul |>
    mutate(
      product = ifelse(
        product == "arveja gr. seco",
        yes = "arveja grano seco",
        no = product
      ),
      product = ifelse(
        product == "frijol grano seco**",
        yes = "frijol grano seco",
        no = product
      ),
      product = ifelse(
        product == "frijol palo",
        yes = "frijol de palo",
        no = product
      ),
      product = ifelse(
        product == "frijol castlla",
        yes = "frijol castilla",
        no = product
      ),
      product = ifelse(
        product == "lentaja",
        yes = "lenteja",
        no = product
      ),
      product = ifelse(
        product == "frijol gr. seco",
        yes = "frijol grano seco",
        no = product
      ),
      product = ifelse(
        product == "haba gr. seco",
        yes = "haba grano seco",
        no = product
      ),
      product = ifelse(
        product == "maíz a. duro",
        yes = "maíz amarillo duro",
        no = product
      ),
      product = ifelse(
        product == "arveja gr. verde",
        yes = "arveja grano verde",
        no = product
      ),
      product = ifelse(
        product == "arveja gr. verde",
        yes = "arveja grano verde",
        no = product
      ),
      product = ifelse(
        product == "haba gr. verde",
        yes = "haba grano verde",
        no = product
      ),
      product = ifelse(
        product == "espá-rrago",
        yes = "espárrago",
        no = product
      ),
      product = ifelse(
        product == "maíz duro",
        yes = "maíz amarillo duro",
        no = product
      ),
    ) |>
    filter(! str_detect(product, "contin")) |>
    filter(! str_detect(product, "conclusi"))
  resul
}

#' Import monthly regional agricultural values from the Excel file,
#' for all the months of the file.
#'
#' @param file path to the Excel File
#' @param anneesup integer, if different from 1, keeping only the last year available
#' @param timescale
#'
#' @importFrom dplyr mutate select rename filter case_when arrange desc
#' @importFrom stringr str_remove str_detect
#' @importFrom readxl excel_sheets
#' @importFrom purrr map list_rbind
#' @importFrom tidyr pivot_wider pivot_longer
import_monthly_regional_values_S <- function(file,
                                             anneesup,
                                             timescale){

  sheet_names <- excel_sheets(file)
  surf_region_monthly <- map(
    sheet_names,
    import_monthly_regional_values_year_S,
    file = file,
    anneesup = anneesup) |>
    list_rbind()

  # Cleaning years and changing type of values: from str to num
  surf_region_monthly <-
    surf_region_monthly |>
    mutate(year = str_sub(sheet_names[1], 3, 6) |> as.numeric()) |>
    mutate(year = ifelse(anneesup == 0, year - 1,  year)) |>
    mutate(campana = str_remove(campana, "p/?") |> as.numeric()) |>
    mutate(value_num = as.numeric(gsub(" ", "", value)))

  # Values in 2014 to 2016 are expressed in thousands of tonnes
  # These need to be expressed in tonnes
  surf_region_monthly <-
    surf_region_monthly |>
    mutate(
      value_num = ifelse(year %in% 2014:2015, value_num * 10^3, value_num),
      value_num = ifelse(
        str_detect(!!file, "Superficies_2014.xlsx$") & month %in% c(10,11,12),
        yes = value_num * 10^3,
        no = value_num
      )
    )


  # Removing the `value` columns to keep only the column with numerical values
  surf_region_monthly <-
    surf_region_monthly |>
    dplyr::select(-value)


  if (str_detect(file, "Superficies_2002.xlsx$")) {
    for (i in c(3,6,9,12)) {
      temp <- missing_table(y = 2, mm = i, type = "Superficies") |>
        mutate(year = year - 1)
      surf_region_monthly <- rbind(surf_region_monthly, temp)
    }
  }

  if (str_detect(file, "Superficies_2003.xlsx$")) {
    temp <- missing_table(y = 3, mm = 3, type = "Superficies") |>
      mutate(year = year -1)
    surf_region_monthly <- rbind(surf_region_monthly, temp)

  }

  # Cleaning region names
  surf_region_monthly$region |> unique() |> sort()
  surf_region_monthly <-
    surf_region_monthly |>
    mutate(region = replace(region, region=="Apurimac", "Apurímac")) |>
    mutate(region = replace(region, region=="Cajamarca_R", "Cajamarca")) |>
    mutate(region = replace(
      x = region,
      list = region %in% c("Nacional", "Total nacional", "TOTAL NACIONAL",
                           "TOTAL  NACIONAL"),
      values = "Total Nacional")
    )

  surf_region_monthly <- surf_region_monthly |>
    arrange(region, product, desc(month))

  # Cleaning product names
  surf_region_monthly <-
    surf_region_monthly |>
    # Replace extra spacing with only one space
    mutate(product = str_replace_all(product, "[[:blank:]]{2,}", " "))

  surf_region_monthly <-
    surf_region_monthly |>
    mutate(
      product = ifelse(
        product == "arveja gr. seco",
        yes = "arveja grano seco",
        no = product
      ),
      product = ifelse(
        product == "frijol castlla",
        yes = "frijol castilla",
        no = product
      ),
      product = ifelse(
        product == "frijol gr. seco",
        yes = "frijol grano seco",
        no = product
      ),
      product = ifelse(
        product == "haba gr. seco",
        yes = "haba grano seco",
        no = product
      ),
      product = ifelse(
        product == "maíz a. duro",
        yes = "maíz amarillo duro",
        no = product
      ),
      product = ifelse(
        product == "algodón",
        yes = "algodón rama",
        no = product
      ),
      product = ifelse(
        product == "arroz",
        yes = "arroz cáscara",
        no = product
      ),
      product = ifelse(
        product == "arveja grano",
        yes = "arveja grano seco",
        no = product
      ),
      product = ifelse(
        product == "frijol",
        yes = "frijol grano seco",
        no = product
      ),
      product = ifelse(
        product == "haba grano",
        yes = "haba grano seco",
        no = product
      ),
      product = ifelse(
        product == "	maíz am duro",
        yes = "maíz amarillo duro",
        no = product
      )
    )


  if (str_detect(file, "Superficies_201(4|5).xlsx$")) {
    surf_region_monthly <- surf_region_monthly |>
      mutate(
        product = case_when(
          str_detect(product, "arroz")           ~  "arroz cáscara",
          str_detect(product, "banano")          ~  "plátano",
          str_detect(product, "café")            ~  "café",
          str_detect(product, "banano")          ~  "plátano",
          str_detect(product, "caña")            ~  "caña de azúcar",
          str_detect(product, "chocho")          ~  "chocho",
          str_detect(product, "gar")             ~  "garbanzo",
          str_detect(product, "caña")            ~  "caña de azúcar",
          str_detect(product, "algodón")         ~  "algodón rama",
          str_detect(product, "maíz a. duro")    ~  "maíz amarillo duro",
          str_detect(product, "maíz amilaceo")   ~  "maíz amiláceo",
          str_detect(product, "maíz amílaceo")   ~  "maíz amiláceo",
          str_detect(product, "maíz amiláceo")   ~  "maíz amiláceo",
          str_detect(product, "caña de azúcar para azúcar")   ~  "caña de azúcar",
          str_detect(product, "arveja seca")     ~  "arveja grano seco",
          str_detect(product, "arveja verde")    ~  "arveja grano verde",
          str_detect(product, "loctao")          ~  "frijol loctao",
          str_detect(product, "frijol seco")     ~  "frijol grano seco",
          str_detect(product, "frijol castill")  ~  "frijol castilla",
          str_detect(product, "haba seca")       ~  "haba grano seco",
          str_detect(product, "haba seco")       ~  "haba grano seco",
          str_detect(product, "haba verde")      ~  "haba grano verde",
          str_detect(product, "limón sutil")     ~  "limón",
          str_detect(product, "haba verde")      ~  "haba grano verde",
          str_detect(product, "algodón rama")    ~  "algodón rama",
          str_detect(product, "café")            ~  "café",
          str_detect(product, "cañihua")         ~  "cañihua",
          str_detect(product, "espárrago")       ~  "espárrago",
          str_detect(product, "limón")           ~  "limón",
          str_detect(product, "maíz amiláceo")   ~  "maíz amiláceo",
          str_detect(product, "maíz choclo")     ~  "maíz choclo",
          str_detect(product, "piña")            ~  "piña",
          str_detect(product, "zaran-daja")      ~  "zarandaja",
          TRUE ~ product
        )
      )
  }

  surf_region_monthly <-
    surf_region_monthly |>
    mutate(value_num = ifelse(is.na(value_num), yes = 0, no = value_num))|>
    mutate(date = str_c(year, month, sep = "-")) |>
    unique()

  if (timescale == 12){
    surf_region_monthly <- surf_region_monthly |>
      filter(month == 7 ) |>
      dplyr::select(region, product, year, value_num) |>
      rename(surf_annual = value_num)
  }
  surf_region_monthly
}

# Regional Harvested Surface----

#' Import monthly regional agricultural values from the Excel file,
#' for a given month
#'
#' @param sheet_name name of the Excel sheet
#' @param file path to the Excel File
#' @param anneesup integer, if different from 1, keeping only the last year available
#'
#' @importFrom dplyr mutate select rename first across summarise filter_all
#'   any_vars
#' @importFrom stringr str_c str_sub str_which regex str_detect
#' @importFrom readxl read_excel
#' @importFrom tidyselect everything
#' @importFrom tidyr pivot_longer fill
import_monthly_regional_values_year_SR <- function(sheet_name,
                                                   file,
                                                   anneesup) {
  # The first two digits of the name: month
  # the next four: the four digits of the year
  # last digit: index of the sheet for a given month and year (we will note use it)

  year  <- str_sub(sheet_name, 3, 6) |> as.numeric()
  month <- str_sub(sheet_name, 1, 2) |> as.numeric()
  tmp   <- suppressMessages(
    read_excel(
      path = file,
      sheet = sheet_name,
      col_types = "text", n_max = 15, col_names = FALSE
    )
  )

  # We can use the first occurrence of "Años" to determine the beginning of the table
  ind_row_year <- str_which(tmp[[3]], regex("años?", ignore_case = TRUE))
  if(length(ind_row_year) > 0) ind_row_year <- first(ind_row_year)
  name_camp <- 0
  if (year == 2005) {
    ind_row_year <- str_which(tmp[[2]], regex("años?", ignore_case = TRUE)) |>
      first()
  }
  if (length(ind_row_year) == 0) {
    ind_row_year <- str_which(tmp[[3]], regex("Campaña?", ignore_case = TRUE)) |>
      first()
    name_camp <- 1
  }
  # The first row of the header of the table is contained in the previous line
  skip_head <- ind_row_year

  # The body of the table
  surfR_region_tmp <-
    suppressMessages(
      read_excel(
        path = file,
        sheet = sheet_name,
        col_types = "text", skip = skip_head, col_names = F
      )
    )

  header_1 <-
    suppressMessages(
      read_excel(
        path = file,
        sheet = sheet_name,
        skip = skip_head-2, n_max = 3, col_names = F
      )
    )

  if (year < 2005) {
    header_1 <- header_1[-dim(header_1)[1], ]}

  header_2 <-
    header_1 |>
    summarise(across(.cols = everything(), .fns = ~format_header(.x)))

  colnames(surfR_region_tmp) <- as.character(header_2)
  if (colnames(surfR_region_tmp)[2] == "") {
    colnames(surfR_region_tmp)[2] <- "departamento"
  }

  # Removing columns with no name
  ind <- !is.na(colnames(surfR_region_tmp)) & (colnames(surfR_region_tmp) != "")
  surfR_region_tmp <-
    surfR_region_tmp |> dplyr::select(!!!colnames(surfR_region_tmp)[ind])

  # Removing rows with all NAs
  surfR_region_tmp <-
    surfR_region_tmp |>
    filter_all(any_vars(!is.na(.)))

  # Removing rows where "Continúa" is found
  surfR_region_tmp <-
    surfR_region_tmp |>
    filter_all(
      any_vars(!str_detect(., regex(pattern = "Continúa", ignore_case = TRUE)))
    )

  # Removing the first colomn if row index
  if (str_detect(colnames(surfR_region_tmp)[1], "[aeiou]") == FALSE) {
    surfR_region_tmp <-  surfR_region_tmp[-1]
  }
  # The name of the first column differs accross sheets (departemento or region)
  name_first_col <- colnames(surfR_region_tmp)[1]
  if (name_first_col %in% c("(ha) departamento","mes : enero 2002-2003* departamento")) {
    colnames(surfR_region_tmp)[1] <- "departamento"
    name_first_col <- colnames(surfR_region_tmp)[1]
  }
  if (! name_first_col %in% c("departamento", "región")) {
    warning(str_c("Issue with sheet: ", sheet_name))
    return(NULL)
    # stop("First column is not region")
  }

  surfR_region_tmp <-
    surfR_region_tmp |>
    rename(region := !!name_first_col)
  if (name_camp == 1) {
    surfR_region_tmp <-
      surfR_region_tmp |> rename(años = campaña)
  }
  if (any(str_detect(colnames(surfR_region_tmp), "^año$"))) {
    surfR_region_tmp <-
      surfR_region_tmp |> rename(year = año)
  } else {
    surfR_region_tmp <-
      surfR_region_tmp |> rename(year = años)
  }


  if (any(str_detect(colnames(surfR_region_tmp), "ene"))) {
    p <- which(str_detect(colnames(surfR_region_tmp), "ene"))
    surfR_region_tmp <- surfR_region_tmp |>
      dplyr::select(-p)
  }

  surfR_region_tmp <-
    surfR_region_tmp |>
    filter(!is.na(year), ! year %in% c("Años", "Año"))

  # The sheet may contain values for the department Cajamarca
  # AND the capital of that department named also Cajamarca
  surfR_region_tmp <-
    surfR_region_tmp |>
    group_by(region, year) |>
    mutate(
      region = ifelse(
        region == "Cajamarca" & row_number() == 1,
        yes = "Cajamarca_R",
        no = region
      )
    ) |>
    ungroup()

  surfR_region_tmp <-
    surfR_region_tmp |>
    mutate(
      region = ifelse(region == "Lima provincias", yes = "Lima", no = region)
    )

  # The production of each product is given in two rows, but the product name is not repeated
  surfR_region_tmp <-
    surfR_region_tmp |>
    fill(region, .direction = "down")
  if (year == 2005) {
    surfR_region_tmp$region[1]<- "Total Nacional"
  }

  # Removing sub-regional data
  surfR_region_tmp <-
    surfR_region_tmp |>
    filter(
      ! region %in% c("Cajamarca", "Chota", "Jaén", "Abancay","Andahuaylas")
    ) |>
    filter(! region == "Región")

  resul <-
    surfR_region_tmp |>
    pivot_longer(cols = -c(region, year), names_to = "product")

  if (sheet_name == "062015") {
    resul <- resul |>
      filter(! year %in% c("Continúa", "29"))
  }
  if (anneesup == 1) {
    resul <-
      resul |>
      filter(year == max(year))
  } else {
    resul <-
      resul |>
      filter(year == min(year))
  }


  resul <-
    resul |>
    mutate(
      product = ifelse(
        product == "arveja gr. seco",
        yes = "arveja grano seco",
        no = product
      ),
      product = ifelse(
        product == "frijol grano seco**",
        yes = "frijol grano seco",
        no = product
      ),
      product = ifelse(
        product == "frijol palo",
        yes = "frijol de palo",
        no = product
      ),
      product = ifelse(
        product == "frijol castlla",
        yes = "frijol castilla",
        no = product
      ),
      product = ifelse(
        product == "lentaja",
        yes = "lenteja",
        no = product
      ),
      product = ifelse(
        product == "frijol gr. seco",
        yes = "frijol grano seco",
        no = product
      ),
      product = ifelse(
        product == "haba gr. seco",
        yes = "haba grano seco",
        no = product
      ),
      product = ifelse(
        product == "maíz a. duro",
        yes = "maíz amarillo duro",
        no = product
      ),
      product = ifelse(
        product == "arveja gr. verde",
        yes = "arveja grano verde",
        no = product
      ),
      product = ifelse(
        product == "haba gr. verde",
        yes = "haba grano verde",
        no = product
      ),
      product = ifelse(
        product == "espá-rrago",
        yes = "espárrago",
        no = product
      ),
      product = ifelse(
        product == "maíz duro",
        yes = "maíz amarillo duro",
        no = product
      ),
    )

  resul |>
    mutate(month = month) |>
    dplyr::select(region, year, month, product, value) |>
    filter(! product == "total") |>
    filter(! str_detect(product, "contin")) |>
    filter(! str_detect(product, "conclusi"))

}

#' Import monthly regional agricultural values from the Excel file,
#' for all the months of the file.
#'
#' @param file path to the Excel File
#' @param anneesup integer, if different from 1, keeping only the last year available
#' @param timescale
#'
#' @importFrom dplyr mutate select rename filter case_when arrange desc
#' @importFrom stringr str_remove str_detect
#' @importFrom readxl excel_sheets
#' @importFrom purrr map list_rbind
#' @importFrom tidyr pivot_wider pivot_longer
import_monthly_regional_values_SR<- function(file,
                                             anneesup,
                                             timescale) {
  sheet_names <- excel_sheets(file)
  surfR_region_monthly <- map(
    sheet_names,
    import_monthly_regional_values_year_SR,
    file = file,
    anneesup = anneesup) |>
    list_rbind()

  # Cleaning years and changing type of values: from str to num
  surfR_region_monthly <-
    surfR_region_monthly |>
    mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
    mutate(value_num = as.numeric(gsub(" ", "", value)))


  # Values in 2014 to 2016 are expressed in thousands of tonnes
  # These need to be expressed in tonnes
  surfR_region_monthly <-
    surfR_region_monthly |>
    mutate(
      value_num = ifelse(year %in% 2014:2015, value_num * 10^3, value_num),
      value_num = ifelse(
        str_detect(file, "Superficies_R_2014.xlsx$") & month %in% c(10,11,12),
        yes = value_num * 10^3,
        no = value_num
      )
    )



  # Removing the `value` columns to keep only the column with numerical values
  surfR_region_monthly <-
    surfR_region_monthly |>
    dplyr::select(-value)


  if (str_detect(file, "Superficies_R_2002.xlsx$")) {
    for (i in c(3,6,9,12)) {
      temp <- missing_table(y = 2, mm = i, type = "Superficies_R")
      surfR_region_monthly <- rbind(surfR_region_monthly, temp)
    }
  }

  if (str_detect(file, "Superficies_R_2003.xlsx$")) {
    temp <- missing_table(y = 3, mm = 3, type = "Superficies_R")
    surfR_region_monthly <- rbind(surfR_region_monthly, temp)
  }
  # Cleaning region names
  surfR_region_monthly <-
    surfR_region_monthly |>
    unique() |>
    mutate(region = replace(region, region=="Apurimac", "Apurímac")) |>
    mutate(region = replace(region, region=="Cajamarca_R", "Cajamarca")) |>
    mutate(
      region = replace(
        x = region,
        list = region %in% c("Nacional", "Total nacional", "TOTAL NACIONAL"),
        values = "Total Nacional"
      )
    )


  surfR_region_monthly <-
    surfR_region_monthly |>
    mutate(
      product = ifelse(
        product == "arveja gr. seco",
        yes = "arveja grano seco",
        no = product
      ),
      product = ifelse(
        product == "frijol castlla",
        yes = "frijol castilla",
        no = product
      ),
      product = ifelse(
        product == "frijol gr. seco",
        yes = "frijol grano seco",
        no = product
      ),
      product = ifelse(
        product == "haba gr. seco",
        yes = "haba grano seco",
        no = product
      ),
      product = ifelse(
        product == "maíz a. duro",
        yes = "maíz amarillo duro",
        no = product
      ),
      product = ifelse(
        product == "algodón",
        yes = "algodón rama",
        no = product
      ),
      product = ifelse(
        product == "arroz",
        yes = "arroz cáscara",
        no = product
      ),
      product = ifelse(
        product == "arveja grano",
        yes = "arveja grano seco",
        no = product
      ),
      product = ifelse(
        product == "frijol",
        yes = "frijol grano seco",
        no = product
      ),
      product = ifelse(
        product == "haba grano",
        yes = "haba grano seco",
        no = product
      ),
      product = ifelse(
        product == "	maíz am duro",
        yes = "maíz amarillo duro",
        no = product
      )
    )


  if (str_detect(file, "Superficies_R_201(4|5).xlsx$")) {
    surfR_region_monthly <- surfR_region_monthly |>
      mutate(
        product = case_when(
          str_detect(product, "arroz")     ~  "arroz cáscara",
          str_detect(product, "banano")    ~  "plátano",
          str_detect(product, "café")      ~  "café",
          str_detect(product, "banano")    ~  "plátano",
          str_detect(product, "caña")      ~  "caña de azúcar",
          str_detect(product, "chocho")    ~  "chocho",
          str_detect(product, "gar")       ~  "garbanzo",
          str_detect(product, "caña")      ~  "caña de azúcar",
          str_detect(product, "algodón")   ~  "algodón rama",
          str_detect(product, "maíz amilaceo")   ~  "maíz amiláceo",
          str_detect(product, "maíz amílaceo")   ~  "maíz amiláceo",
          str_detect(product, "caña de azúcar para azúcar")   ~  "caña de azúcar",
          str_detect(product, "arveja seca")     ~  "arveja grano seco",
          str_detect(product, "arveja verde")    ~  "arveja grano verde",
          str_detect(product, "loctao")          ~  "frijol loctao",
          str_detect(product, "frijol seco")     ~  "frijol grano seco",
          str_detect(product, "frijol castill")  ~  "frijol castilla",
          str_detect(product, "haba seca")       ~  "haba grano seco",
          str_detect(product, "haba seco")       ~  "haba grano seco",
          str_detect(product, "haba verde")      ~  "haba grano verde",
          str_detect(product, "limón sutil")     ~  "limón",
          str_detect(product, "haba verde")      ~  "haba grano verde",
          str_detect(product, "algodón rama")    ~  "algodón rama",
          str_detect(product, "café")            ~  "café",
          str_detect(product, "cañihua")         ~  "cañihua",
          str_detect(product, "espárrago")       ~  "espárrago",
          str_detect(product, "limón")           ~  "limón",
          str_detect(product, "maíz amiláceo")   ~  "maíz amiláceo",
          str_detect(product, "maíz choclo")     ~  "maíz choclo",
          str_detect(product, "piña")            ~  "piña",
          str_detect(product, "zaran-daja")      ~  "zarandaja",
          TRUE ~ product
        )
      )
  }

  SurfaceR <- surfR_region_monthly |>
    arrange(region, product, desc(month)) |>
    mutate(value_num = ifelse(is.na(value_num), yes = 0, no = value_num))|>
    pivot_wider(names_from = month, values_from = value_num)

  if (timescale == 1) {
    SurfaceR <-
      cbind(SurfaceR[1:3], SurfaceR[4:14] - SurfaceR[5:15], SurfaceR[15]) |>
      pivot_longer(cols = -c(region, year, product), names_to = "month")
    SurfaceR  <- SurfaceR[with(SurfaceR, order(region, year, as.numeric(month), product, value)), ]
  }

  if (timescale == 3) {
    SurfaceR <-
      cbind(
        SurfaceR[1:3],
        SurfaceR[4] - SurfaceR[7],
        SurfaceR[7] - SurfaceR[10],
        SurfaceR[10] - SurfaceR[13],
        SurfaceR[13]
      ) |>
      pivot_longer(cols = -c(region, year, product), names_to = "trim")
    SurfaceR <- SurfaceR[with(SurfaceR, order(region, year, as.numeric(trim), product, value)), ]
  }

  if (timescale == 4) {
    SurfaceR <-
      cbind(
        SurfaceR[1:3],
        SurfaceR[4] - SurfaceR[8],
        SurfaceR[8] - SurfaceR[12],
        SurfaceR[12]
      ) |>
      pivot_longer(cols = -c(region, year, product), names_to = "quadrim")
    SurfaceR <- SurfaceR[with(SurfaceR, order(region, year, as.numeric(quadrim), product, value)), ]
  }

  if (timescale == 6) {
    SurfaceR <-
      cbind(
        SurfaceR[1:3],
        SurfaceR[4] - SurfaceR[10],
        SurfaceR[10]
      ) |>
      pivot_longer(cols = -c(region, year, product), names_to = "biannual")
    SurfaceR <- SurfaceR[with(SurfaceR, order(region, year, as.numeric(biannual), product, value)), ]
  }

  if (timescale == 12) {
    SurfaceR <-
      cbind(SurfaceR[1:3], SurfaceR[4]) |>
      rename(surf_R_annual = "12")
  }

  SurfaceR
}

# Regional Prices----


#' Import monthly regional agricultural values from the Excel file,
#' for a given month
#'
#' @param sheet_name name of the Excel sheet
#' @param file path to the Excel File
#' @param anneesup integer, if different from 1, keeping only the last year available
#'
#' @importFrom dplyr mutate select rename first across summarise filter_all
#'   any_vars
#' @importFrom stringr str_c str_sub str_which regex str_detect
#' @importFrom readxl read_excel
#' @importFrom tidyselect everything
#' @importFrom tidyr pivot_longer fill
import_monthly_regional_values_year_Px <- function(sheet_name,
                                                   file,
                                                   anneesup) {
  # The first two digits of the name: month
  # the next four: the four digits of the year
  # last digit: index of the sheet for a given month and year (we will note use it)
  year  <- str_sub(sheet_name, 3, 6) |> as.numeric()
  month <- str_sub(sheet_name, 1, 2) |> as.numeric()
  tmp   <- suppressMessages(
    read_excel(
      path = file,
      sheet = sheet_name,
      col_types = "text",
      n_max = 15,
      col_names = FALSE
    )
  )

  # We can use the first occurrence of "Año" to determine the beginning of the table
  ind_row_year <- str_which(tmp[[4]], regex("año.?", ignore_case = TRUE))
  if (length(ind_row_year) == 0) {
    ind_row_year <- str_which(tmp[[3]], regex("año.?", ignore_case = TRUE)) |>
      first()
  } else {
    ind_row_year <- first(ind_row_year)
  }

  if (sheet_name == "062015") {ind_row_year <- 8}

  # Replacing column 2 into column 3
  if (y > 4) {
    tmp[1:ind_row_year+2, 3] <- tmp[1:ind_row_year + 2, 2]
    tmp[1:ind_row_year +2, 2] <- NA
  }

  # The first row of the header of the table is contained in the previous line
  skip_head <- ind_row_year

  # The body of the table
  prx_region_tmp <-
    suppressMessages(
      read_excel(
        path = file,
        sheet = sheet_name,
        col_types = "text", col_names = F
      )
    )

  positions <-
    which(prx_region_tmp[,4] == regex(pattern = "Año", ignore_case = TRUE))


  if (str_detect(file, "Prices_2015.xlsx$")) {
    positions <-
      which(prx_region_tmp[,3] == regex(pattern = "Año", ignore_case = TRUE))
  }
  if (length(positions) == 0) {
    positions <-
      which(prx_region_tmp[,3] == regex(pattern = "Año", ignore_case = TRUE))
  }

  resul <-NULL

  for (i in 1:length(positions)) {
    prx_region_tmp <-
      suppressMessages(
        read_excel(
          path = file,
          sheet = sheet_name,
          col_types = "text",
          col_names = F,
          skip = positions[i] - 1,
          n_max = ifelse(
            i == length(positions),
            yes = 1000,
            no = positions[i + 1] - positions[i]
          )-1
        )
      )

    header_1 <- prx_region_tmp[1:2, ]
    if (y < 6) {header_1 <- prx_region_tmp[1, ]}
    header_2 <-
      header_1 |>
      summarise(
        across(
          .cols = everything(),
          .fns = ~format_header(.x)
        )
      )
    if (str_detect(header_2[1,2], regex("Gobierno Regional", ignore_case = TRUE)) == T){
      header_2[2] <-  "departamento"
    }

    header_2[3] <- header_2[2]
    header_2[2] <- ""

    if (str_detect(file, "Prices_2015.xlsx$")) {
      header_2 <-
        header_1 |>
        summarise(
          across(
            .cols = everything(),
            .fns = ~format_header(.x)
          )
        )
    }

    colnames(prx_region_tmp) <- as.character(header_2)

    if (y < 6) {
      if (colnames(prx_region_tmp)[2] == "") {
        colnames(prx_region_tmp)[2] <- "departamento"
        colnames(prx_region_tmp)[3] <- "año"
      }
      prx_region_tmp <- prx_region_tmp[-1, ]
    } else {
      if (colnames(prx_region_tmp)[3] == "") {
        colnames(prx_region_tmp)[2] <- "departamento"
      }
      prx_region_tmp <- prx_region_tmp[-c(1:2), ]
    }

    cell <- ifelse(str_detect(file, "Prices_2015.xlsx$"), yes = 2, no = 3)

    if (!is.na(prx_region_tmp[1, 2]) &
        str_detect(prx_region_tmp[1, 2], regex("Promedio Nacional", ignore_case = TRUE)
        ) == T){
      prx_region_tmp[1, cell]  <- "PROMEDIO NACIONAL"
    }
    if (!is.na(prx_region_tmp[1, 2]) &
        str_detect(prx_region_tmp[1, 2], regex("Gobierno Regional", ignore_case = TRUE)) == T) {
      prx_region_tmp[1, cell]  <- "PROMEDIO NACIONAL"
    }

    if (sheet_name == "072007") {prx_region_tmp[, 3] <- prx_region_tmp[2]}
    # Removing columns with no name
    ind <- !is.na(colnames(prx_region_tmp)) & (colnames(prx_region_tmp) != "")
    prx_region_tmp <-
      prx_region_tmp |> dplyr::select(!!!colnames(prx_region_tmp)[ind]) |>
      {\(x) x[, -1]}()

    # Removing rows with all NAs
    prx_region_tmp <-
      prx_region_tmp |>
      filter_all(any_vars(!is.na(.)))

    # Removing rows where "Continúa" is found
    prx_region_tmp <-
      prx_region_tmp |>
      filter_all(
        any_vars(
          !str_detect(., regex(pattern = "Continúa", ignore_case = TRUE))
        )
      )

    # Removing the first colomn if row index
    if (str_detect(colnames(prx_region_tmp)[1], "[aeiou]") == FALSE) {
      prx_region_tmp <- prx_region_tmp[-1]
    }
    # The name of the first column differs accross sheets (departemento or region)
    name_first_col <- colnames(prx_region_tmp)[1]
    if (! name_first_col %in% c("departamento", "región")) {
      warning(str_c("Issue with sheet: ", sheet_name))
      return(NULL)
    }

    prx_region_tmp <-
      prx_region_tmp |>
      rename(region := !!name_first_col)

    prx_region_tmp <-
      prx_region_tmp |> rename(year = año)

    # The sheet may contain values for the department Cajamarca
    # AND the capital of that department named also Cajamarca
    prx_region_tmp <-
      prx_region_tmp |>
      group_by(region, year) |>
      mutate(
        region = ifelse(
          region == "Cajamarca" & row_number() == 1,
          yes = "Cajamarca_R",
          no = region
        )
      ) |>
      ungroup()

    prx_region_tmp <-
      prx_region_tmp |>
      mutate(
        region = ifelse(region == "Lima provincias", yes = "Lima", no = region)
      )

    # The production of each product is given in two rows, but the product name is not repeated
    prx_region_tmp <-
      prx_region_tmp |>
      fill(region, .direction = "down")

    # Removing sub-regional data
    prx_region_tmp <-
      prx_region_tmp |>
      filter(
        ! region %in% c("Cajamarca", "Chota", "Jaén", "Abancay","Andahuaylas")
      ) |>
      filter(! year %in% c(NA))

    resul_current <-
      prx_region_tmp |>
      pivot_longer(cols = -c(region, year), names_to = "product")

    resul <- resul |>
      bind_rows(resul_current)

    if(sheet_name == "062015"){
      resul <- resul |>
        filter(year %in% c(2014, 2015))
    }
  }

  # Correction for product names
  resul <-
    resul |>
    mutate(
      product = ifelse(
        product == "arveja gr. seco",
        yes = "arveja grano seco",
        no = product
      ),
      product = ifelse(
        product == "frijol grano seco**",
        yes = "frijol grano seco",
        no = product
      ),
      product = ifelse(
        product == "frijol palo",
        yes = "frijol de palo",
        no = product
      ),
      product = ifelse(
        product == "frijol castlla",
        yes = "frijol castilla",
        no = product
      ),
      product = ifelse(
        product == "lentaja",
        yes = "lenteja",
        no = product
      ),
      product = ifelse(
        product == "frijol gr. seco",
        yes = "frijol grano seco",
        no = product
      ),
      product = ifelse(
        product == "haba gr. seco",
        yes = "haba grano seco",
        no = product
      ),
      product = ifelse(
        product == "maíz a. duro",
        yes = "maíz amarillo duro",
        no = product
      ),
      product = ifelse(
        product == "arveja gr. verde",
        yes = "arveja grano verde",
        no = product
      ),
      product = ifelse(
        product == "haba gr. verde",
        yes = "haba grano verde",
        no = product
      ),
      product = ifelse(
        product == "espá-rrago",
        yes = "espárrago",
        no = product
      ),
      product = ifelse(
        product == "maíz duro",
        yes = "maíz amarillo duro",
        no = product
      ),
    )

  if (anneesup == 1) {
    resul <-
      resul |>
      filter(year == max(year))
  } else {
    # Keeping only the last year available
    resul <-
      resul |>
      filter(year == min(year))
  }

  resul |>
    mutate(month = month) |>
    dplyr::select(region, year, month, product, value)
}

#' Import monthly regional agricultural values from the Excel file,
#' for all the months of the file.
#'
#' @param file path to the Excel File
#' @param anneesup integer, if different from 1, keeping only the last year available
#' @param timescale
#'
#' @importFrom dplyr mutate select rename filter case_when arrange desc
#' @importFrom stringr str_remove str_detect
#' @importFrom readxl excel_sheets
#' @importFrom purrr map list_rbind
#' @importFrom tidyr pivot_wider pivot_longer
import_monthly_regional_values_Px <- function(file,
                                              anneesup,
                                              timescale) {
  sheet_names <- excel_sheets(file)
  prx_region_monthly <- map(
    sheet_names,
    import_monthly_regional_values_year_Px,
    file = file,anneesup= anneesup
  ) |>
    list_rbind()
  # Cleaning years and changing type of values: from str to num
  prx_region_monthly <-
    prx_region_monthly |>
    mutate(year = str_remove(year, "p/?") |> as.numeric()) |>
    mutate(
      value_num = str_remove_all(value, pattern = " ") |>
        str_replace_all(pattern = ",", replacement = ".") |>
        as.numeric()
    )

  # Values in 2015 are expressed in thousands of tonnes
  # These need to be expressed in tonnes
  if (str_detect(file, "Prices_2015.xlsx$")) {
    prx_region_monthly <-
      prx_region_monthly |>
      mutate(
        value_num = value_num,
        value_num = ifelse(month > 4,
          yes = value_num / 10^3,
          no = value_num
        )
      )
  }


  # Removing the `value` columns to keep only the column with numerical values
  prx_region_monthly <-
    prx_region_monthly |>
    dplyr::select(-value)


  # Cleaning region names
  prx_region_monthly$region |> unique() |> sort()
  prx_region_monthly <-
    prx_region_monthly |>
    mutate(region = replace(region, region=="Apurimac", "Apurímac")) |>
    mutate(region = replace(region, region=="Cajamarca_R", "Cajamarca")) |>
    mutate(
      region = replace(
        x = region,
        list = region %in% c("Nacional", "Promedio nacional", "PROMEDIO NACIONAL"),
        values = "Total Nacional"
      )
    )

  # Cleaning product names
  prx_region_monthly <- prx_region_monthly
  prx_region_monthly <- prx_region_monthly |>
    # Replace extra spacing with only one space
    mutate(product = str_replace_all(product, "[[:blank:]]{2,}", " "))

  prx_region_monthly <-
    prx_region_monthly |>
    mutate(
      product = ifelse(
        product == "arveja gr. seco", yes = "arveja grano seco", no = product
      ),
      product = ifelse(
        product == "frijol castlla", yes = "frijol castilla", no = product
      ),
      product = ifelse(
        product == "frijol gr. seco", yes = "frijol grano seco", no = product
      ),
      product = ifelse(
        product == "haba gr. seco", yes = "haba grano seco", no = product
      ),
      product = ifelse(
        product == "maíz a. duro", yes = "maíz amarillo duro", no = product
      )
    )


  if (str_detect(file, "Prices_201(4|5).xlsx$")) {
    prx_region_monthly <- prx_region_monthly |>
      mutate(
        product = case_when(
          str_detect(product, "arroz")           ~  "arroz cáscara",
          str_detect(product, "banano")          ~  "plátano",
          str_detect(product, "café")            ~  "café",
          str_detect(product, "banano")          ~  "plátano",
          str_detect(product, "caña")            ~  "caña de azúcar",
          str_detect(product, "chocho")          ~  "chocho",
          str_detect(product, "gar")             ~  "garbanzo",
          str_detect(product, "caña")            ~  "caña de azúcar",
          str_detect(product, "algodón")         ~  "algodón rama",
          str_detect(product, "maíz amilaceo")   ~  "maíz amiláceo",
          str_detect(product, "maíz amílaceo")   ~  "maíz amiláceo",
          str_detect(product, "caña de azúcar para azúcar")   ~  "caña de azúcar",
          str_detect(product, "arveja seca")     ~  "arveja grano seco",
          str_detect(product, "arveja verde")    ~  "arveja grano verde",
          str_detect(product, "loctao")          ~  "frijol loctao",
          str_detect(product, "frijol seco")     ~  "frijol grano seco",
          str_detect(product, "frijol castill")  ~  "frijol castilla",
          str_detect(product, "haba seca")       ~  "haba grano seco",
          str_detect(product, "haba seco")       ~  "haba grano seco",
          str_detect(product, "haba verde")      ~  "haba grano verde",
          str_detect(product, "limón sutil")     ~  "limón",
          str_detect(product, "haba verde")      ~  "haba grano verde",
          str_detect(product, "algodón rama")    ~  "algodón rama",
          str_detect(product, "café")            ~  "café",
          str_detect(product, "cañihua")         ~  "cañihua",
          str_detect(product, "espárrago")       ~  "espárrago",
          str_detect(product, "limón")           ~  "limón",
          str_detect(product, "maíz amiláceo")   ~  "maíz amiláceo",
          str_detect(product, "maíz choclo")     ~  "maíz choclo",
          str_detect(product, "piña")            ~  "piña",
          str_detect(product, "zaran-daja")      ~  "zarandaja",
          TRUE ~ product
        )
      )
  }

  prx_region_monthly <-
    prx_region_monthly |>
    mutate(value_num = ifelse(is.na(value_num), yes = 0, no = value_num))|>
    mutate(date = str_c(year, month, sep = "-")) |>
    unique()

  prx_region_monthly
}
