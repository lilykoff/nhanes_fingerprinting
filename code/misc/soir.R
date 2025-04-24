# install Sarim:
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
devtools::install_github("chkue/Sarim")

devtools::install_github("RaphaelRe/SOIR")



library(Sarim)
library(SOIR)
library(magrittr)

# generate images (100 images each 32x32, vectorized)
ims <- replicate(100, (1:(32*32))+ rnorm(32*32, sd = 2)) %>% t

# generate true coefficient image
grid <- seq(0,1,len = 32)
beta <- smoothBeta(grid, grid) %>% as.vector()

# generate response
y <- ims  %*% beta + rnorm(100, sd = 5)

# fit model (small number of iterations - only for understanding)
mod <- Sarim::sarim(y ~ SOIR(ims, add_diag = 0.1, neighbours = "2dfirst",
                             ka_a = 10, ka_b = 1e-3, ka_start = 0.1), nIter = 100)

# visualize result (results are of course bad)
get_beta(mod, intercept = FALSE, burnin = 10, reduce = TRUE) %>%
  set_dim(c(32,32)) %>%
  plot_coefficient_image

## apply on our data
library(tidyverse)
data = read_csv(here::here("data", "sample_grid_cells.csv.gz"))

data_mat = data %>%
  select(starts_with("x")) %>%
  as.matrix() %>% unname()

y = data %>% mutate(y = (id == id[1] )* 1) %>% pull(y)
dim(data_mat) <- as.integer(dim(data_mat))

mod2 = Sarim::sarim(y ~ SOIR(data_mat, dimension = c(36, 12)), family = "binomial", link = "logit",
                    nIter = 1000)
get_beta(mod2, intercept = FALSE, burnin = 10, reduce = TRUE) %>%
  set_dim(c(36, 12)) %>%
  plot_coefficient_image

predict(mod2, data_mat)

eta_hat <- data_mat %*% mod2$coef_results[[1]][,1]  # Linear predictor
pred_values <- 1 / (1 + exp(-eta_hat))
unique(pred_values)
