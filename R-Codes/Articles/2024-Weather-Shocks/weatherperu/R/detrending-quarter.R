#' Applies HP filter for quarterly data and returns the trend component
#'
#' @param x vector of quarterly observations
hp_filter_trend <- function(x, freq = 1600) {
  res_hp_filter <- mFilter::hpfilter(
    x,
    freq = freq,
    type = "lambda",
    drift = FALSE
  )
  as.vector(res_hp_filter$trend)
}

#' Removes the seasonality of a vector of quarterly values
#'
#' @param x vector of numerical values
#' @param start_date start date of the values
#' @param remove_trend should the estimated trend be removed? Default to ``FALSE
adj_season_X13 <- function(x,
                           start_date,
                           remove_trend = FALSE) {
  x_ts <- ts(x, start = c(year(start_date), quarter(start_date)), freq = 4)
  # Seasonal Adjustment with X-13ARIMA-SEATS
  x_ts_season <- seasonal::seas(
    x_ts, estimate.maxiter=10000,
    x11 = ""
  )
  x_ts_season_df <- timetk::tk_tbl(x_ts_season$data) |>
    mutate(date = lubridate::my(index))

  df_resul <- tibble(
    date = seq(start_date, last(x_ts_season_df$date), by = "quarter")
  ) |>
    left_join(x_ts_season_df, by = "date") |>
    mutate(val = seasonaladj)

  if (remove_trend) {
    df_resul <- df_resul |>
      mutate(val = seasonaladj - trend)
  }

  df_resul |> pull(val)
}

#' Computes the percentage deviation from trend
#'
#' @description After estimating the trend and seasonal components of a quarterly
#'   time series using X-13 ARIMA-SEATS method, the percentage deviation of the
#'   series without seasonality to the trend id computed and returned in a
#'   tibble.
#'
#' @param x vector of numerical quarterly values
#' @param start_date start date of the values
get_pct_dev_trend <- function(x, start_date) {
  x_ts <- ts(x, start = c(year(start_date), quarter(start_date)), freq = 4)
  # Seasonal Adjustment with X-13ARIMA-SEATS
  x_ts_season <- seasonal::seas(x_ts, estimate.maxiter=10000)
  x_ts_season_df <- timetk::tk_tbl(x_ts_season$data) |>
    mutate(
      date = lubridate::my(index),
      x = !!x
    )

  res <-
    tibble(
    date = seq(start_date, last(x_ts_season_df$date), by = "quarter")
  ) |>
    left_join(x_ts_season_df, by = "date")

  if ("seasonal" %in% colnames(res)) {
    res <- res |>
      mutate(val = (final - seasonal - trend) / trend)
  } else {
    res <- res |>
      mutate(val = (final - trend) / trend)
  }
  res <- res |>
    mutate(quarter = quarter(date), year = year(date)) |>
    pull(val)
}


#' Detrends the production data, using OLS
#'
#' @param df data
#' @param crop_name name of the crop
#' @param region_id id of the region
#' @param in_log if TRUE, the detrended values are expressed in log
#'
#' @returns data frame with the product, the region id, the date, and the
#'   detrended value for the production. If `in_log = TRUE` and there are zeros
#'   the function returns `NULL`. Similarly, if there are more than two
#' @export
#' @importFrom dplyr filter arrange mutate select row_number group_by
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom imputeTS na_interpolation
#' @importFrom stats lm predict residuals
detrend_production <- function(df,
                               crop_name,
                               region_id,
                               in_log = FALSE) {
  # The current data
  df_current <-
    df |>
    filter(
      product_eng == !!crop_name,
      region_id == !!region_id
    ) |>
    arrange(year, quarter)

  ## Dealing with missing values ----
  # Look for negative production values
  df_current <-
    df_current |>
    mutate(
      y_new = ifelse(Value_prod < 0, NA, Value_prod)
    )

  if (any(is.na(df_current$y_new))) {

    # Replacing NAs by interpolation
    # If there are more than two contiguous NAs, they are not replaced
    df_current <-
      df_current |>
      mutate(
        y_new = imputeTS::na_interpolation(y_new, maxgap = 3)
      )

    # Removing obs at the beginning/end if they are still missing
    df_current <-
      df_current |>
      mutate(
        row_to_keep = !(is.na(y_new) & row_number() %in% c(1:2, (n()-1):(n())))
      ) |>
      filter(row_to_keep) |>
      select(-row_to_keep)

    # Keeping the longest series of continuous non-NA values
    df_current <-
      df_current |>
      mutate(
        row_to_keep = keep_values_longest_non_na(y_new)
      ) |>
      filter(row_to_keep) |>
      select(-row_to_keep)
  }

  rle_y_new <- rle(df_current$y_new)
  check_contiguous_zeros <- rle_y_new$lengths[rle_y_new$values==0]

  if (any(check_contiguous_zeros >= 2)) {
    resul <- NULL
  } else {

    ## Detrending
    df_current <-
      df_current |>
      group_by(quarter) |>
      mutate(
        y_new_normalized = y_new / median(y_new)
      )

    if (any(is.infinite(df_current$y_new_normalized))) {
      resul <- NULL
    } else {
      resul <-
        df_current |>
        select(product_eng, region_id, year, quarter, y_new, y_new_normalized) |>
        group_by(quarter) |>
        arrange(year) |>
        mutate(t = row_number()) |>
        ungroup() |>
        nest(.by = c(product_eng, region_id, quarter)) |>
        # distinct OLS per quarter
        mutate(
          ols_fit   = map(data, ~ lm(y_new_normalized ~ -1 + t + I(t^2), data = .x)),
          resid     = map(ols_fit, residuals),
          fitted    = map(ols_fit, predict)
          # intercept = map(ols_fit, ~ coef(.x)[["(Intercept)"]])
        ) |>
        # unnest(cols = c(data, resid, intercept)) |>
        unnest(cols = c(data, resid, fitted)) |>
        group_by(quarter) |>
        mutate(
          y = resid + mean(fitted)
          # y = resid
        ) |>
        select(product_eng, region_id, year, quarter, y_new, y) |>
        ungroup() |>
        arrange(year)

      if (in_log) {
        resul <- resul |>
          mutate(y = log(y))
      }
    }
  }
  resul
}
