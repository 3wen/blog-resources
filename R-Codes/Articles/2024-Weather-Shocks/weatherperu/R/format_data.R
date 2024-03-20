#' Get the data in a table for the local projections, for a specific crop
#'
#' @param df original dataset
#' @param horizons number of horizons
#' @param y_name name of the exogenous variable
#' @param group_name name of the group variable
#' @param crop_name name of the crop to focus on
#' @param control_names vector of names of the control variables
#' @param weather_names vector of names of the weather variables
#' @param add_month_fe should columns with month dummy variables be added?
#'   Default to `TRUE`
#' @param share_geo vector of names of the variables that contain the share of
#'   each type of geographical pattern. By default `NULL`: no share used
#' @param transition_name name of the variable used to define the transition to
#'   the two states. By default `NULL`
#' @param transition_method if transition function, name of the method to use:
#'   `logistic` or `normal` (default to `NULL`, i.e., no transition)
#' @param state_names name of the two states in a vector of characters (only if
#'   `transition_name` is not `NULL`). First period corresponds to mapped values
#'   of `transition_name` close to 0, which is for large positive values of
#'   `transition_name`
#' @param gamma logistic growth rate (default to 3, only used if
#'   `transition_name` is not `NULL`)
#' @param other_var_to_keep vector of names of other variables to keep (default
#'   to `NULL`: no additional vairable kept)
#' @export
#' @importFrom dplyr filter select mutate sym group_by across rowwise arrange
#'   slice lead ends_with
#' @importFrom fastDummies dummy_cols
#' @importFrom stringr str_c str_detect
get_data_lp <- function(df,
                        horizons,
                        y_name,
                        group_name,
                        crop_name,
                        control_names,
                        weather_names,
                        add_month_fe = TRUE,
                        share_geo = NULL,
                        transition_name = NULL,
                        transition_method = NULL,
                        state_names = c("planted", "harvested"),
                        gamma = 3,
                        other_var_to_keep = NULL) {

  if (!is.null(share_geo) & !is.null(transition_name)) {
    stop("You can only use one between share_geo and transition_name")
  }

  # Init empty object to return: list of length horizons
  df_horizons <- vector(mode = "list", length = horizons + 1)

  # Keep only the variables needed
  df_focus <-
    df |>
    filter(product_eng == !!crop_name) |>
    select(
      !!y_name,
      !!group_name,
      date,
      product_eng,
      !!!control_names,
      !!!weather_names,
      !!!share_geo,
      !!transition_name,
      !!other_var_to_keep
    ) |>
    mutate(
      !!group_name := factor(!!sym(group_name))
    )

  # Month dummy fixed-effects
  if (add_month_fe) {
    df_focus <- df_focus |>
      mutate(
        month = as.character(lubridate::month(date))
      ) |>
      dummy_cols(select_columns = "month", remove_first_dummy = FALSE)
  }


  # For each region, only keep the longest sequence of non NA values found in
  # the weather variables
  df_focus <-
    df_focus |>
    group_by(region_id) |>
    mutate(
      across(
        .cols  = !!weather_names,
        .fns   = keep_values_longest_non_na,
        .names = "{.col}_keep"
      )
    ) |>
    rowwise() |>
    mutate(keep_cols = all(across(ends_with("_keep")))) |>
    ungroup() |>
    filter(keep_cols) |>
    select(-keep_cols, -!!paste0(weather_names, "_keep"))

  if (!is.null(share_geo)) {
    # For each geographical type, multiply the weather variables by the share
    # that the geo. type represents
    for(share_geo_type in share_geo) {
      df_focus <-
        df_focus |>
        mutate(
          across(
            .cols  = !!weather_names,
            .fns   = ~ .x * !!sym(share_geo_type),
            .names = str_c("{.col}_", share_geo_type)
          )
        )
    }
  }

  if (!is.null(transition_name)) {

    state_1_name <- state_names[1]
    state_2_name <- state_names[2]

    if (transition_method == "logistic") {
      df_focus <-
        df_focus |>
        mutate(
          fz = logist(!!sym(transition_name), gamma = gamma)
        )
    } else if (transition_method == "normal") {
      df_focus <-
        df_focus |>
        mutate(
          fz = pnorm(-!!sym(transition_name))
        )
    } else {
      stop("transition method must be either \"losistic\" or \"normal\"")
    }
    df_focus <- df_focus |>
      dummy_cols(group_name, remove_first_dummy = FALSE)

    ind_dummies_group <- str_detect(colnames(df_focus), str_c(group_name, "_"))
    dummies_group_name <- colnames(df_focus)[ind_dummies_group]

    if (add_month_fe) {
      ind_dummies_month <- str_detect(colnames(df_focus), "^month_")
      dummies_month_name <- colnames(df_focus)[ind_dummies_month]
      dummies_group_name <- c(dummies_group_name, dummies_month_name)
    }

    df_focus <-
      df_focus |>
      mutate(
        # First regime:
        across(
          .cols  = c(!!!control_names, !!!weather_names, !!!dummies_group_name),
          .fns   = list(
            state_1_name = ~ (1 - fz) * .x,
            state_2_name = ~ fz * .x
          ),
          .names = "{fn}_{col}"
        )
      ) |>
      rename_with(
        .fn = ~str_replace(string = .x, pattern = "state_1_name", replacement = state_1_name),
        .cols = starts_with("state_1_name")
      ) |>
      rename_with(
        .fn = ~str_replace(string = .x, pattern = "state_2_name", replacement = state_2_name),
        .cols = starts_with("state_2_name")
      )
  } else {
    df_focus <-
      df_focus |>
      dummy_cols(group_name, remove_first_dummy = FALSE)
  }


  # Prepare the values for y at t+h
  for (h in 0:horizons) {
    df_horizons[[h+1]] <-
      df_focus |>
      group_by(!!sym(group_name)) |>
      arrange(date) |>
      mutate(y_lead = dplyr::lead(!!sym(y_name), n = h)) |>
      slice(1:(n()-h))
  }
  names(df_horizons) <- 0:horizons
  df_horizons
}
