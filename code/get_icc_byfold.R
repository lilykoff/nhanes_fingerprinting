# train - test ICC
library(tidyverse)
library(tidymodels)
source(here::here("code", "R", "utils.R"))
fold = NULL
rm(list = c("fold"))
force = FALSE

I2C2 <- function(
    y,
    id,
    visit,
    symmetric = FALSE,
    truncate = FALSE,
    twoway = TRUE,
    demean = TRUE,
    return_demean = TRUE,
    ...){


  L = check_id_visit(y = y, id = id, visit = visit)
  rm(y); gc()
  n = L$n
  y = L$y
  id = L$id
  I = L$I
  visit = L$visit
  rm(L);
  gc(); gc();
  # p = L$p

  n_I0 = as.numeric(table(id))  # visit number for each id cluster
  k2 = sum(n_I0 ^ 2) # \sum J_i^2

  ### If demean == TRUE, we calculate the overall mean function and subtract
  ### the mean function from the data
  ### If twoway mean subtraction is needed ("twoway==TRUE"),  the visit specific
  ### mean function also computed and removed from the raw data.
  demean = as.logical(demean)
  tol = 0

  if (demean) {
    # need resd as in output
    resd = demean_matrix(y = y, visit = visit, twoway = twoway, tol = tol)
    rm(y); gc()
    W = resd
    if (!return_demean) {
      rm(resd)
    }
  } else {
    W <- y
    rm(y); gc()
  }

  # population average for the demeaned dataset W
  Wdd = colMeans(W)

  # subject-specific sum for the demeaned dataset W
  Ni = !is.na(W)
  class(Ni) = "numeric"
  Ni = rowsum(Ni, group = id)

  Si = rowsum(W, group = id)
  Si = Si / Ni

  # repeat the rows in order for the ids
  Wi = Si[id,]
  ### If symmetric is FALSE, use the method of moments estimator
  ### formula from the manuscript; otherwise, use pairwise symmetric sum estimator

  if (!symmetric) {
    trKu <- sum((W - Wi) ^ 2) / (n - I)
    trKw <- sum((t(W) - Wdd) ^ 2) / (n - 1)
    trKx <- (trKw - trKu) #/ (1 + (1 - k2 / n) / (n - 1))  #removed constant in the denominator
  } else {
    Si = Si * Ni
    trKu <- (sum(W ^ 2 * n_I0[id]) - sum(Si ^ 2)) / (k2 - n)
    trKw <-
      (sum(W ^ 2) * n - sum((n * Wdd) ^ 2) - trKu * (k2 - n)) / (n ^ 2 - k2)
    trKx <-  trKw - trKu
  }

  ## estimated I2C2 values
  lambda <-  trKx / (trKx + trKu)
  if (truncate) {
    lambda[ lambda <= 0] = 0
    ## If trun==TRUE, truncate negative lambdas to 0
  }

  ###  Return the results from I2C2 calculation as a list, with 'lambda' as I2C2 value,
  ###  Kx and Ku being the trace of between and within cluster variance operators;
  ###  If demean == TRUE, also return demeaned data
  L = list(
    lambda = lambda,
    Kx = trKx,
    Ku = trKu
  )
  if (demean & return_demean) {
    class(resd)  = c(class(resd), "demeaned_matrix")
    L$demean_y = resd
  }

  return(L)

}


#' @title Check ID and visit specification for I2C2
#' @description Checks the input for I2C2
#' @param y An n by p data matrix containing n vectorized image data with p voxels.
#' Each row contains one observed image data at a particular visit for one subject.
#' Each column contains image values for all subjects and visits at a particular voxel.
#'
#' The rows are organized by subjects and then visits, EX)
#' (Y11, Y12, Y21, Y22, ... , YI1 , YI2)
#' @param id Vector of IDs, EX) c(1, 1, 2, 2, 3, 3, 4, 4, ... , I, I)
#' @param visit Vector of visits, EX) (1, 2, 1, 2, 1, 2, ... , 1, 2)
#' @export
#'
#' @return List of elements: reordered data, id, visit, I (number of ids),
#' number of rows (n), and number of columns (p)
#' @examples
#' id = c(1:10, 10:1)
#' visit = rep(1:2, each = 10)
#' visit = as.character(visit)
#' n = length(id)
#' p = 100
#' y = matrix(rnorm(n * p), nrow = n, ncol = p)
#' check_id_visit(y =y, id = id, visit = visit)
check_id_visit = function(y, id, visit) {
  p = ncol(y)
  n = nrow(y)
  if (n == 1) {
    stop("only one observation!")
  }
  uid = unique(id)
  I = length(uid)

  if (length(id) != n) {
    stop("Number of ids not equal to number of rows of y")
  }
  if (length(visit) != n) {
    stop("Number of visits not equal to number of rows of y")
  }

  if (is.data.frame(y)) {
    y = as.matrix(y)
  }
  if (!is.matrix(y)) {
    stop("y is not a matrix!")
  }
  if (!typeof(y) %in% c("integer", "double", "logical", "numeric")) {
    stop("y is not a numeric/integer/logical type!")
  }

  # reset the id number to be arithmetic sequence starting from 1
  id <- as.numeric(factor(id))
  visit <- as.numeric(factor(visit))

  # reorder the rows in order of the ids
  ord = order(id, visit)

  # only reorder if needed
  # saving for memory
  if (!all(ord == seq_along(ord))) {
    # order visit
    visit = visit[ord]
    id = id[ord]
    y = y[ord,]
  }

  L = list(y = y, id = id, visit = visit, n = n, p = p,
           I = I)
  return(L)
}

demean_matrix = function(y, visit, twoway = TRUE, tol = 0) {

  # p = ncol(y)
  # mu <- colMeans(y)
  y = scale(y, center = TRUE, scale = FALSE)
  attr(y, "scaled:center") = NULL

  if (twoway) {
    resd <- y # not defined yet
    # get unique visits
    uvisit = sort(unique(visit))
    # eta <- matrix(0, length(uvisit), p)

    ##########
    # ETA ISN'T USED HERE
    ########3
    for (ivisit in seq_along(uvisit)) {
      j = uvisit[ivisit]

      ###########################
      # Maybe include a tolerance measure
      ###########################
      if (tol == 0) {
        ind = visit == j
      } else {
        ind = (visit - j) <= tol
      }
      # grab these visits
      # mat = y[ ind, , drop = FALSE]
      # visit_mean = colMeans(y[ ind, , drop = FALSE])
      # eta[ ivisit, ] = visit_mean - mu


      ### Calculate residuals by subtracting visit-specific mean from
      ### original functions for'twoway == TRUE', or subtracting
      ### overall mean function for 'twoway == FALSE'.
      # mat = y[ ind, , drop = FALSE]
      mat = resd[ ind, , drop = FALSE]
      mat = scale(mat, scale = FALSE, center = TRUE)
      resd[ind, ] = mat
      # resd[, ind] = resd[, ind] - visit_mean
    }
    # y = resd
    return(resd)
    # resd = t(resd)
  }
  # else {
  #   y = scale(y, center = mu, scale = FALSE)
  #   attr(y, "scaled:center") = NULL
  #   # resd <- t(resd - mu) #twoway == FALSE
  # }
  return(y)
}

filenames = readRDS(here::here("data", "lily", "data", "fingerprint_folds.rds"))

xdf = readr::read_csv(here::here("data", "lily", "data", "all_grid_cells.csv.gz"))

get_icc = function(n_subs) {
  temp_fnames =
    filenames %>%
    mutate(fold_new = rep(1:ceiling(nrow(filenames)/n_subs), each = n_subs)[1:nrow(filenames)])

  temp_fnames =
    temp_fnames %>%
    group_by(fold_new) %>%
    mutate(n = n()) %>%
    filter(n == n_subs)

  folds = unique(temp_fnames$fold_new)

  map_dfr(.x = folds,
      .f = function(x){
        fdf = temp_fnames %>%
          filter(fold_new == x)
        df =
          xdf %>%
          filter(id %in% fdf$id)

        set.seed(123)
        initialsplit = initial_split(df, prop = 3 / 4, strata = id)

        data_train = training(initialsplit)
        data_test = testing(initialsplit)

        data_train_summ =
          data_train %>%
          group_by(id) %>%
          summarize(across(starts_with("x"), mean), .groups = "drop") %>%
          mutate(visit = 1)

        data_test_summ =
          data_test %>%
          group_by(id) %>%
          summarize(across(starts_with("x"), mean), .groups = "drop") %>%
          mutate(visit = 2)

        y = data_train_summ %>% select(starts_with("x")) %>%
          bind_rows(data_test_summ %>% select(starts_with("x"))) %>%
          as.matrix()

        ids = c(data_train_summ$id, data_test_summ$id)
        visits = c(data_train_summ$visit, data_test_summ$visit) %>% as.character()

        res = I2C2(y,
                   id = ids,
                   visit = visits,
                   demean = FALSE)
        tibble(i2c2 = res$lambda,
               n = nrow(fdf),
               fold = x)
      })
}


all = map_dfr(.x = c(100, 200, 300, 500, 1000, 2500, 5000),
              .f = get_icc)

saveRDS(all, here::here("data", "lily", "data", "icc.rds"))

all = readRDS(here::here("data", "lily", "data", "icc.rds"))

all %>%
  group_by(n) %>%
  summarize(across(i2c2, list(mean, max, min)))



