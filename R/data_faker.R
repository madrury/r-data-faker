.make_binary_matrix <- function(n_rows, n_columns) {
  matrix(
    sample(c(0, 1), n_rows*n_columns, replace=TRUE, prob=c(.5, .5)),
    nrow=n_rows, ncol=n_columns
  )
}

make_linmod_data <- function(n_rows, n_columns,
                               n_columns_with_large_effects=0,
                               n_nuisance_columns=0,
                               n_hidden_columns=0,
                               beta_sd=1,
                               beta_inflate_factor=2.5,
                               noise_sd=2.5) {
  n_common_columns <- n_columns - n_nuisance_columns
  if(n_common_columns < 0) {
    stop("Illogical arguments: n_nuisance_columns > n_columns.")
  }
  if(n_columns_with_large_effects > n_common_columns) {
    stop("Illogical arguments: n_columns_with_large_effects > number of effects.")
  }
  X_common <- make_binary_matrix(n_rows, n_common_columns)
  X_nuisance <- make_binary_matrix(n_rows, n_nuisance_columns)
  X_hidden <- make_binary_matrix(n_rows, n_hidden_columns)
  beta_common <- rnorm(n_common_columns, mean=0, sd=beta_sd)
  beta_common[1:n_columns_with_large_effects] <- (
    beta_inflate_factor * beta_common[1:n_columns_with_large_effects])
  beta_nuisance <- rnorm(n_nuisance_columns, mean=0, sd=beta_sd)
  beta_hidden <- rnorm(n_hidden_columns, mean=0, sd=beta_sd)
  Y <- (
    cbind(X_common, X_hidden) %*% c(beta_common, beta_hidden)
    + rnorm(n_rows, mean=0, sd=noise_sd))
  X <- cbind(X_common, X_nuisance)
  list(X=X,
       Y=Y,
       beta_common=beta_common,
       beta_nuisance=beta_nuisance,
       beta_hidden=beta_hidden)
}
