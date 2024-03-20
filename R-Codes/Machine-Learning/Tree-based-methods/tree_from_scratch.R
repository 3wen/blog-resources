# Regression and Classification Tree
# From scratch

# Impurity Metrics----


#' Calculate Gini impurity for a given set of labels
#'
#' @param labels vector of labels
calculate_gini_impurity <- function(labels) {
  label_counts <- table(labels)
  proportions <- label_counts / sum(label_counts)
  gini_impurity <- 1 - sum(proportions^2)
  gini_impurity
}

#' Calculate entropy for a given set of labels
#'
#' @param labels vector of labels
calculate_entropy <- function(labels) {
  label_counts <- table(labels)
  proportions <- label_counts / sum(label_counts)
  # 0*log(0) must be treated as 0
  proportions[proportions == 0] <- 1
  entropy <- -sum(proportions * log2(proportions))
  entropy
}

#' Calculate impurity for a given set of labels
#'
#' @param labels vector of labels
#' @param split metric to use: `"gini"` or `"entropy"`
calculate_impurity <- function(labels, split = c("gini", "entropy")) {
  if (split == "gini") {
    impurity <- calculate_gini_impurity(labels = labels)
  } else if (split == "entropy") {
    impurity <- calculate_entropy(labels = labels)
  }
  impurity
}

#' Calculate total impurity for a given split
#'
#' @param data_split list of two elements: `left` and `right` which contain the
#'  left and right node, respectively
#' @param y_name name of the target variable
#' @param split metric to use: `"gini"` or `"entropy"`
calculate_total_impurity <- function(data_split, y_name, split = c("giny", "entropy")) {
  n_parent <- nrow(data_split$left) + nrow(data_split$right)
  n_left <- nrow(data_split$left)
  n_right <- n_parent - n_left
  weight_left <- n_left / n_parent
  weight_right <- 1 - weight_left
  impurity_left <- calculate_impurity(
    labels = pull(data_split$left, y_name), split = split
  )
  impurity_right <- calculate_impurity(
    labels = pull(data_split$right, y_name), split = split
  )
  total_impurity <- weight_left * impurity_left + weight_right * impurity_right
  c(
    total_impurity = total_impurity,
    n_parent = n_parent,
    n_left = n_left,
    n_right = n_right
  )
}

# Splitting Functions----

#' Split dataset based on a given numerical feature and its split_val
#'
#' @param data dataset
#' @param feature name of the feature on which to split on
#' @param split_val threshold to use to split the data
split_dataset_num <- function(data, feature, split_val) {
  ind_left <- which(pull(data, !!feature) < split_val)
  list(
    ind_left = ind_left,
    left = data |> slice(ind_left),
    right = data |> slice(-ind_left)
  )
}

#' Split dataset based on a given binary feature
#'
#' @param data dataset
#' @param feature name of the feature on which to split on
split_dataset_binary <- function(data, feature) {
  if (class(pull(data, feature)) == "character") {
    ref <- sort(unique(pull(data, feature)))[1]
  } else if (class(pull(data, feature)) == "factor") {
    ref <- levels(pull(data, feature))[1]
  } else if (class(pull(data, feature)) == "logical") {
    ref <- TRUE
  } else {
    stop("Wrong frature type for the split: expected character or factor or logical")
  }

  ind_left <- which(pull(data, !!feature) == ref)
  list(
    ind_left = ind_left,
    left = data |> slice(ind_left),
    right = data |> slice(-ind_left),
    feature = feature,
    ref = ref
  )
}


#' Split dataset based on a given categorical feature
#'
#' @param data dataset
#' @param feature name of the feature on which to split on
#' @param split_val splitting groups to use to split the data
split_dataset_categ <- function(data, feature, split_val) {
  ind_left <- which(pull(data, !!feature) %in% split_val)
  left <- data |> slice(ind_left)
  if (is_empty(ind_left)){
    right <- data
  } else {
    right <- data |> slice(-ind_left)
  }
  list(
    ind_left = ind_left,
    left = left,
    right = right
  )
}

#' Split a dataset for a given feature and a splitting value
#' @param data dataset
#' @param feature name of the feature on which to split on
#' @param class_feature class of feature
#' @param split_val splitting value (or groups) to use to split the data
split_dataset <- function(data, feature, class_feature, split_val = NULL) {
  if (class_feature == "numeric") {
    data_split <- split_dataset_num(
      data = data, feature = feature, split_val = split_val
    )
  } else {
    nb_different_val <- length(unique(pull(data, feature)))
    if (nb_different_val > 2) {
      data_split <- split_dataset_categ(
        data = data, feature = feature, split_val = split_val
      )
    } else {
      # Binary feature
      data_split <- split_dataset_binary(
        data = data, feature = feature
      )
    }
  }
  data_split
}


## Candidate Selection----

#' Select the best feature splitting value to split the dataset on
#'
#' @param data dataset
#' @param feature name of the feature to use for the split
#' @param params list of parameters (split, target name, ...)
select_best_split_val <- function(data, feature, params) {

  split <- params$split
  y_name <- params$y_name
  tb_variable_types <- params$tb_variable_types
  class_feature <- tb_variable_types |>
    filter(var_name == feature) |> pull("var_type")
  feature_val <- pull(data, feature)

  if (class_feature == "numeric") {
    # Different threshold values to consider
    threshold_values <- tb_variable_types |>
      filter(var_name == feature) |> pull("labels") |>
      pluck(1)
    # Total impurity for each value of the threshold
    total_impurity <- map(
      .x = threshold_values,
      .f = ~{
        data_split <- split_dataset_num(
          data = data, feature = feature, split_val = .x
        )
        calculate_total_impurity(
          data_split = data_split, y_name = y_name, split = split
        )
      }
    )
    # Identify lowest total impurity
    ind_min_impurity <- which.min(map_dbl(total_impurity, "total_impurity"))
    if (is_empty(ind_min_impurity)) {
      # Case when all values are identical for the candidate variable
      ind_min_impurity <- 1
    }
    best_split_val <- threshold_values[ind_min_impurity]
    best_total_impurity <- total_impurity[[ind_min_impurity]]
  } else {
    # Not numeric
    feature_labels <- tb_variable_types |>
      filter(var_name == feature) |> pull("labels") |>
      pluck(1)
    nb_different_val <- length(feature_labels)
    if (nb_different_val > 2) {
      # Categorical variable with more than 2 classes
      groups <- NULL
      for (k in 1:(floor(nb_different_val / 2))) {
        groups <- c(
          groups,
          combn(x = feature_labels, m = k, simplify = FALSE)
        )
      }

      total_impurity <- map(
        .x = groups,
        .f = ~{
          data_split <- split_dataset_categ(
            data = data, feature = feature, split_val = .x
          )
          calculate_total_impurity(
            data_split = data_split, y_name = y_name, split = split
          )
        }
      )
      ind_min_impurity <- which.min(map_dbl(total_impurity, "total_impurity"))
      if (is_empty(ind_min_impurity)) ind_min_impurity <- 1
      best_split_val <- groups[ind_min_impurity]
      best_total_impurity <- total_impurity[[ind_min_impurity]]
    } else {
      # Binary feature
      data_split <- split_dataset_binary(data = data, feature = feature)
      best_split_val <- data_split$ref # reference in left node
      best_total_impurity <- calculate_total_impurity(
        data_split = data_split, y_name = y_name, split = split
      )
    }
  }

  list(
    feature = feature,
    split_val = best_split_val,
    best_total_impurity = best_total_impurity,
    class_feature = class_feature
  )
}


#' Select the best split
#'
#' @param data dataset
#' @param candidates name of the features to consider for a split
#' @param params list of parameters (split, target name, ...)
select_best_split <- function(data, candidates, params) {

  split <- params$split
  y_name <- params$y_name

  # Best splits for each candidate
  best_splits_features <- map(
    .x = candidates,
    .f = ~select_best_split_val(data = data, feature = .x, params = params)
  )
  # Identify best candidate
  ind_best_candidate <- map(best_splits_features, "best_total_impurity") |>
    map_dbl("total_impurity") |>
    which.min()
  best_splits_features[[ind_best_candidate]]
}

## Splitting a Node----

#' Screen candidates for a split, performs the split if necessary
#' then returns the current node information, and the left and right nodes if
#' a split was performed
#'
#' @param data dataset
#' @param node_id ID of the node on which a split will be attempted
#' @param node_ids vector of node ids
#' @param node_list list with information on each node of the current tree
#' @param params list of parameters (split, target name, ...)
split_node <- function(data, node_id, node_ids, node_list, params) {

  split <- params$split
  y_name <- params$y_name
  cp <- params$cp
  min_bucket_size <- params$min_bucket_size
  tb_variable_types <- params$tb_variable_types

  # Identify the current node in `node_list`
  ind_current_node <- which(map_dbl(node_list, "node_id") == node_id)
  node_current <- node_list[[ind_current_node]]
  node_data_ind <- node_current$node_data_ind
  node_data <- data |> slice(node_data_ind)

  # Impurity before split
  impurity_before <- calculate_impurity(labels = pull(node_data, y_name), split = split)

  candidates <- tb_variable_types$var_name[!tb_variable_types$var_name %in% y_name]
  best_split <- select_best_split(data = node_data, candidates = candidates, params = params)

  if ((impurity_before - best_split$best_total_impurity["total_impurity"] >= cp) &
      (best_split$best_total_impurity["n_left"] >= min_bucket_size &
       best_split$best_total_impurity["n_right"] >= min_bucket_size)) {
    improvement <- TRUE
    # Improvement made with the split and enough observation in the leaves
    data_split <- split_dataset(
      data = node_data,
      feature = best_split$feature,
      class_feature = tb_variable_types |>
        filter(var_name == best_split$feature) |>
        pull(var_type),
      split_val = best_split$split_val
    )

    # New entry for the tree info table
    max_node_id <- node_ids[length(node_ids)]
    var_type <-
      tb_variable_types |> filter(var_name == best_split$feature) |>
      pull(var_type)
    tree_info_new <- tibble(
      node_id = node_id,
      left_child = max_node_id + 1,
      right_child = max_node_id + 2,
      split_var_name = best_split$feature,
      split_var_type = var_type,
      split_val = list(best_split$split_val),
      terminal = FALSE,
      prediction = list(NA)
    )

    # Identify index of data in the children
    ind_left <- data_split$ind_left
    ind_right <- (1:nrow(node_data))[-ind_left]
    node_left <-
      list(
        node_id = max_node_id + 1,
        node_data_ind = node_data_ind[ind_left]
      )
    node_right <-
      list(
        node_id = max_node_id + 2,
        node_data_ind = node_data_ind[ind_right]
      )
  } else {
    # No improvement
    improvement <- FALSE

    # Prediction in the leaf
    prediction <- NULL
    if (params$tree_type == "regression") {
      # Average in the leaf
      prediction <- mean(pull(node_data, y_name))
    } else if (params$tree_type == "classification") {
      # Freq. of each class
      prediction <- prop.table(table(pull(node_data, y_name)))
    }

    tree_info_new <- tibble(
      node_id = node_id,
      left_child = NA,
      right_child = NA,
      split_var_name = NA,
      split_var_type = NA,
      split_val = list(NA),
      terminal = TRUE,
      prediction = list(prediction)
    )
    node_left <- NULL
    node_right <- NULL
  }
  node_current <-
    list(
      node_id = node_id
    )

  list(
    improvement = improvement,
    tree_info_new = tree_info_new,
    node_current = node_current,
    node_left = node_left,
    node_right = node_right
  )
}

# Building Tree----

#' Recursive function to build the decision tree
#'
#' @param data dataset
#' @param params parameters of the tree:
#'  - split: "gini" or "entropy"
#'  - y_name: name of the target variable
#'  - cp: complexity parameter
#'  - min_bucket_size: minimal number of observations in terminal leaves
build_tree <- function(data, params) {

  # Table with variable types
  tb_variable_types <- tibble(
    var_name = colnames(data),
    var_type = unlist(sapply(data, class))
  )

  var_labels <- vector(mode = "list", length = nrow(tb_variable_types))
  for (i_var in 1:nrow(tb_variable_types)) {
    if (tb_variable_types$var_type[i_var] %in% c("integer", "numeric")) {
      var_labels[[i_var]] <-
        pull(data, tb_variable_types$var_name[i_var]) |>
        quantile(probs = seq(.1, .9, by = .1)) |>
        as.vector() |> unique()
    } else if (tb_variable_types$var_type[i_var] == "character") {
      var_labels[[i_var]] <- sort(unique(pull(data, tb_variable_types$var_name[i_var])))
    } else if (tb_variable_types$var_type[i_var] == "factor") {
      var_labels[[i_var]] <- levels(pull(data, tb_variable_types$var_name[i_var]))[1]
    }
  }
  tb_variable_types <-
    tb_variable_types |>
    mutate(
      labels = var_labels
    )

  params$tb_variable_types <- tb_variable_types

  # Type of tree: regression or classification
  if (is.numeric(data |> pull(params$y_name))) {
    tree_type <- "regression"
  } else if (is.character(data |> pull(params$y_name)) |
             is.factor(data |> pull(params$y_name))) {
    tree_type <- "classification"
    # Make sure the variable is a factor
    if (! is.factor(data |> pull(params$y_name))) {
      data <- data |>
        mutate(!!(sym(params$y_name)) := as.factor(!!sym(params$y_name)))
    }
  } else {
    stop(str_c(
      "Response variable should be numerical (for regression), ",
      "or character or factor (for classification)")
    )
  }
  params$tree_type <- tree_type

  # Init node ids
  node_ids <- c(1)
  # Init tibble with tree info
  tree_info <- tibble()
  # Init node list
  node_list <- list(
    list(
      node_id = 1,
      node_data_ind = 1:nrow(data)
    )
  )
  # Init vector of node IDs to explore
  nodes_to_explore <- node_ids

  while (length(nodes_to_explore) > 0) {
    node_id <- nodes_to_explore[1]
    # Perform the split (if possible)
    split_res <- split_node(
      data = data,
      node_id = node_id,
      node_ids = node_ids,
      node_list = node_list,
      params = params
    )
    if(split_res$improvement == TRUE) {
      # Update the node ids
      node_ids <- c(
        node_ids,
        split_res$node_left$node_id,
        split_res$node_right$node_id
      )
      # Update the tree info table
      tree_info <- tree_info |> bind_rows(split_res$tree_info_new)
      # Update the node list (to remove the index)
      node_list[[which(map_dbl(node_list, "node_id") == node_id)]] <- split_res$node_current
      # Add children nodes
      node_list <- c(
        node_list,
        list(split_res$node_left),
        list(split_res$node_right)
      )
      # Remove the current node to the vector of node IDs to explore
      nodes_to_explore <- nodes_to_explore[-which(nodes_to_explore == node_id)]
      # Add the children IDs to the exploration vector
      nodes_to_explore <- c(
        nodes_to_explore,
        split_res$node_left$node_id,
        split_res$node_right$node_id
      )
    } else {
      # No improvement: do not make split (terminal leave)
      # Update the tree info table
      tree_info <- tree_info |> bind_rows(split_res$tree_info_new)
      # Update the node list (to remove the index)
      node_list[[which(map_dbl(node_list, "node_id") == node_id)]] <- split_res$node_current
      # Remove the current node to the vector of node IDs to explore
      nodes_to_explore <- nodes_to_explore[-which(nodes_to_explore == node_id)]
    }
  }

  list(
    tree = tree_info,
    params = params
  )
}

# Prediction Functions----

#' Make a single prediction using the built decision tree
#'
#' @param tree estimated tree
#' @param x observation for which to make a prediction
predict_tree_val <- function(tree, x) {
  # Init
  current_node <- tree$tree |> filter(node_id == 1)
  while (current_node$terminal == FALSE) {
    split_var_name <- current_node$split_var_name
    if (current_node$split_var_type == "numeric") {
      next_node_id <- ifelse(
        pull(x, split_var_name) < current_node$split_val[[1]],
        yes = current_node$left_child,
        no = current_node$right_child
      )
      current_node <- tree$tree |> filter(node_id == next_node_id)
    } else {
      next_node_id <- ifelse(
        pull(x, split_var_name) %in% current_node$split_val[[1]],
        yes = current_node$left_child,
        no = current_node$right_child
      )
      current_node <- tree$tree |> filter(node_id == next_node_id)
    }
  }
  current_node$prediction[[1]]
}


#' Make predictions using the built decision tree
#'
#' @param tree estimated tree
#' @param newdata data on which to predict values with the tree
predict_tree <- function(tree, newdata) {
  if (tree$params$tree_type == "regression") {
    predicted_val <- map_dbl(
      .x = 1:nrow(newdata),
      .f = ~predict_tree_val(tree = tree, x = newdata |> slice(.x))
    )
  } else {
    predicted_val <- map(
      .x = 1:nrow(newdata),
      .f = ~predict_tree_val(tree = tree, x = newdata |> slice(.x))
    )
    predicted_val <- do.call("rbind", predicted_val)
  }
  predicted_val
}
