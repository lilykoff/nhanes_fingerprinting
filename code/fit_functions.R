library(Matrix)

fit_model = function(subject, train, test, weighted = FALSE) {
  class = as.integer(train$id == subject)

  X = train
  X$id = NULL
  X$class = class
  X = sparse.model.matrix(class ~ ., data = X)

  Xt = test
  Xt$id = NULL
  Xt = sparse.model.matrix(~ ., data = Xt)

  if(weighted){
    class_counts = tabulate(class + 1L)

    wts = ifelse(class == 1,
                  1 / class_counts[2],
                  1 / class_counts[1])
    mod = glm.fit(
      x = X,
      y = class,
      family = binomial(),
      weights = wts)
  } else {
    mod = glm.fit(
      x = X,
      y = class,
      family = binomial())
  }

  preds = plogis(Xt %*% coef(mod) %>% as.vector)
  rm(mod)
  return(preds)
}
