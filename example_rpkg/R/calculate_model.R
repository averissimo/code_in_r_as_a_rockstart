
calculate_model <- function(seed, data, prop, nfolds, model_fun = glmnet::cv.glmnet) {
  set.seed(seed)
  # Split the data in 80% training / 20% testing
  split <- rsample::initial_split(data, prop = prop, strata = "event")
  lung_train <- gen_x_y(rsample::training, split)
  cvfit <- model_fun(lung_train$x, lung_train$y, nfolds = nfolds, family = "cox") # fit model

  lung_test <- gen_x_y(rsample::testing, split)
  set.seed(seed * 42) # Just in case
  tryCatch( # Logrank test and extract p-value (defaults to 1 if only 1 group
    cvfit |>
      stats::predict(newx = lung_test$x, s = "lambda.min", type = "link") |>
      dplyr::tibble(risk_score = _, y = lung_test$y) |>
      dplyr::mutate(risk_group = dplyr::if_else(risk_score > median(risk_score), "high", "low")) |>
      dplyr::select(-risk_score) |>
      survival::survdiff(y ~ risk_group, data = _) |>
      purrr::pluck("pvalue"),
    error = \(e) 1 # In case there is only 1 group
  )
}

#' Generate x and y from a split
gen_x_y <- function(fun, split, dat = fun(split)) list(
  x = stats::model.matrix(surv_obj ~ age + sex + ph_ecog + wt_loss, data = dat)[, -1],
  y = dat$surv_obj
)
