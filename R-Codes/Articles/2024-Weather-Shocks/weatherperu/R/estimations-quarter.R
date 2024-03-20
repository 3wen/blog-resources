#' Estimate Local Projections
#'
#' @param df original dataset
#' @param horizons number of horizons
#' @param y_name name of the exogenous variable
#' @param group_name name of the group variable
#' @param crop_name name of the crop to focus on
#' @param control_names vector of names of the control variables
#' @param weather_names vector of names of the weather variables
#' @param add_quarter_fe should columns with quarter dummy variables be added?
#'   Default to `TRUE`
#' @param add_intercept should an intercept we added to the regressions?
#'   (default to `FALSE`)
#' @param share_geo vector of names of the variables that contain the share of
#'   each type of geographical pattern. By default `NULL`: no share used
#' @param std type of standard error (`"NW"` for Newey-West, `"Cluster"`,
#'   `"Standard"` otherwise)
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
#' @param other_var_to_keep vector of names of other variables to keep in the
#'   returned dataset (default to `NULL`: no additional vairable kept)
#' @export
#' @importFrom dplyr mutate sym ungroup summarise across left_join
#' @importFrom stringr str_c str_detect
#' @importFrom purrr map map_dbl list_rbind
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_longer
#' @importFrom sandwich NeweyWest
#' @importFrom stats sd model.matrix nobs residuals lm coef
estimate_linear_lp <- function(df,
                              horizons,
                              y_name,
                              group_name,
                              crop_name,
                              control_names,
                              weather_names,
                              add_quarter_fe = TRUE,
                              add_intercept = FALSE,
                              share_geo = NULL,
                              transition_name = NULL,
                              transition_method = NULL,
                              state_names = c("planted", "harvested"),
                              gamma = 3,
                              std = c("nw", "cluster", "standard"),
                              other_var_to_keep = NULL) {

  # Format the dataset
  data_lp <-
    get_data_lp(
      df = df,
      horizons = horizons,
      y_name = y_name,
      group_name = group_name,
      crop_name = crop_name,
      control_names = control_names,
      weather_names = weather_names,
      share_geo = share_geo,
      transition_name = transition_name,
      transition_method = transition_method,
      state_names = state_names,
      gamma = gamma,
      other_var_to_keep = other_var_to_keep
    )

  # Recode levels for the groups
  for(h in 0:horizons){
    data_lp[[h + 1]] <-
      data_lp[[h + 1]] |>
      mutate(
        !!group_name := as.factor(as.character(!!sym(group_name)))
      )
  }

  control_names_full <- control_names
  weather_names_full <- weather_names
  ind_names_groups <- str_detect(
    colnames(data_lp[[1]]), str_c("^", group_name, "_")
  )
  group_names_full <- colnames(data_lp[[1]])[ind_names_groups]

  if (!is.null(share_geo)) {
    # Name of the weather variables
    weather_names_full <- paste(
      rep(weather_names, each = length(share_geo)),
      share_geo,
      sep = "_"
    )
  }

  if (!is.null(transition_name)) {

    state_1_name <- str_c(state_names[1], "_")
    state_2_name <- str_c(state_names[2], "_")

    # Name of the variables
    weather_names_full <- str_c(
      rep(
        c(state_1_name, state_2_name),
        each = length(weather_names)
      ),
      rep(weather_names, 2)
    )
    control_names_full <- str_c(
      rep(
        c(state_1_name, state_2_name),
        each = length(control_names)
      ),
      rep(control_names, 2)
    )
    ind_names_groups <- str_detect(
      colnames(data_lp[[1]]),
      str_c("(^", state_1_name, "|", state_2_name, ")", group_name, "_")
    )
    group_names_full <- colnames(data_lp[[1]])[ind_names_groups]
  }

  # Observed standard deviations in the data
  sd_weather_shock <-
    map(
      .x = data_lp,
      .f = ~ungroup(.x) |>
        summarise(
          across(
            .cols  = c(!!control_names_full, !!weather_names_full, !!share_geo),
            .fns   = sd
          )
        )
    ) |>
    list_rbind(names_to = "horizon") |>
    pivot_longer(cols = -horizon, names_to = "name", values_to = "std_shock") |>
    mutate(horizon = as.numeric(horizon))

  # Observed median value in the data
  median_weather_shock <-
    map(
      .x = data_lp,
      .f = ~ungroup(.x) |>
        summarise(
          across(
            .cols  = c(!!control_names_full, !!weather_names_full, !!share_geo),
            .fns   = ~quantile(.x, probs = .5)
          )
        )
    ) |>
    list_rbind(names_to = "horizon") |>
    pivot_longer(cols = -horizon, names_to = "name", values_to = "median_shock") |>
    mutate(horizon = as.numeric(horizon))

  # Observed quantile of order 0.05 value in the data
  q05_weather_shock <-
    map(
      .x = data_lp,
      .f = ~ungroup(.x) |>
        summarise(
          across(
            .cols  = c(!!control_names_full, !!weather_names_full, !!share_geo),
            .fns   = ~quantile(.x, probs = .05)
          )
        )
    ) |>
    list_rbind(names_to = "horizon") |>
    pivot_longer(cols = -horizon, names_to = "name", values_to = "q05_shock") |>
    mutate(horizon = as.numeric(horizon))

  # Observed quantile of order 0.95 value in the data
  q95_weather_shock <-
    map(
      .x = data_lp,
      .f = ~ungroup(.x) |>
        summarise(
          across(
            .cols  = c(!!control_names_full, !!weather_names_full, !!share_geo),
            .fns   = ~quantile(.x, probs = .95)
          )
        )
    ) |>
    list_rbind(names_to = "horizon") |>
    pivot_longer(cols = -horizon, names_to = "name", values_to = "q95_shock") |>
    mutate(horizon = as.numeric(horizon))


  # Formula for the regressions
  formula_lp <- paste0(
    "y_lead",
    " ~ -1+",
    # " ~ 1+", # intercept
    paste(weather_names_full, collapse = " + "),
    " + ",
    paste(control_names_full, collapse = " + "),
    " + ",
    ifelse(
      add_intercept,
      # removing last group
      yes = paste(group_names_full[-length(group_names_full)], collapse = " + "),
      # keeping last group
      no = paste(group_names_full, collapse = " + ")
    )
  )

  if (add_quarter_fe) {
    formula_lp <- paste0(
      formula_lp, " + ", paste(paste0("quarter_", 1:11), collapse = " + ")
    )
  }

  # Regressions
  reg_lp <- map(data_lp, ~ lm(formula = formula_lp, data = .))

  # Standard error of the residuals
  sig_ols <- map_dbl(.x = reg_lp, .f = ~sd(.x$residuals))

  log_likelihood <- map_dbl(
    .x = reg_lp,
    .f = function(reg) {
      err_x <- reg$residuals
      sig_ols <- sd(err_x)
      sum(log(1 / sqrt(2 * pi * sig_ols^2) * exp(-err_x^2 / (2 * sig_ols^2))))
    }
  )

  mse <- map_dbl(
    .x = reg_lp,
    .f = function(reg) {
      err_x <- reg$residuals
      mean(err_x^2)
    }
  )

  coefs <- map(
    reg_lp,
    ~ enframe(coef(.))
  ) |>
    list_rbind(names_to = "horizon")

  if (std == "nw") {
    # Newey-West
    cov_nw_pw <- map(
      reg_lp,
      ~ sandwich::NeweyWest(.x, lag = 1, prewhite = FALSE)
    )
    se_df <- map(
      cov_nw_pw,
      ~ enframe(sqrt(diag(.)), value = "std")
    ) |>
      list_rbind(names_to = "horizon")
  } else if (std == "Cluster") {
    cl_std <- vector(mode = "list", length = horizons + 1)
    names(cl_std) <- 0:horizons
    for (h in 0:horizons) {
      ind_NA_coef <- which(is.na(reg_lp[[h + 1]]$coefficients))
      X <- model.matrix(reg_lp[[h + 1]])
      if (length(ind_NA_coef) > 1) {
        X <- X[, -ind_NA_coef]
      }

      errx <- residuals(reg_lp[[h + 1]])
      omega <- 0
      m_clust <- ifelse(
        add_intercept,
        yes = length(group_names_full[-length(group_names_full)]),
        no = length(group_names_full)
      )
      n_clust <- length(errx)
      k_clust <- ncol(X)
      for (ri in 1:m_clust) {
        # Identify id row within the cluster
        idx <- which(X[, group_names_full[ri]] == 1)
        omega <- omega + t(X[idx, ]) %*% errx[idx] %*% t(errx[idx]) %*% X[idx, ]
      }

      omega <- m_clust / (m_clust - 1) * (n_clust - 1) / (n_clust - k_clust) * omega
      t1 <- solve(t(X) %*% X)
      colSums(t1)
      v_cluster <- t1 %*% omega %*% t1
      cl_std[[h + 1]] <- sqrt(diag(v_cluster))
      names(cl_std[[h + 1]]) <- colnames(omega)



    }
    se_df <-
      map(cl_std, ~enframe(., value = "std")) |>
      list_rbind(names_to = "horizon")
  } else {
    se_df <- map(
      reg_lp,
      ~ enframe(sqrt(diag(vcov(.))), value = "std")
    ) |>
      list_rbind(names_to = "horizon")
  }

  coefs <-
    coefs |>
    left_join(se_df, by = c("horizon", "name")) |>
    mutate(
      crop = crop_name,
      horizon = as.numeric(horizon)
    ) |>
    left_join(sd_weather_shock, by = c("horizon", "name")) |>
    left_join(median_weather_shock, by = c("horizon", "name")) |>
    left_join(q05_weather_shock, by = c("horizon", "name")) |>
    left_join(q95_weather_shock, by = c("horizon", "name"))

  list(
    # reg_lp = reg_lp,
    coefs = coefs,
    horizons = horizons,
    log_likelihood = log_likelihood,
    mse = mse,
    crop_name = crop_name,
    data_lp = data_lp
  )
}
