library(data.table)
library(mlr3)
library(mlr3pipelines)

test_that("PipeOpSpatialLag basics", {
  task = tsk('boston_housing')
  target = task$target_names

  pop = PipeOpSpatialLag$new(param_vals = list(k = 7))
  pop$param_set$values$affect_columns = selector_name(c('lat', 'lon'))
  expect_equal(pop$param_set$values$k, 7)
  expect_equal(pop$param_set$values$kernel, 'inv')

  res = pop$train(list(task))$output
  expect_true("cmedv_lag7_inv" %in% names(res$data()))

  preds = pop$predict(list(task))$output$data()
  expect_true("cmedv_lag7_inv" %in% names(preds))

  expect_error({pop$param_set$values$k = 1}, regexp = NULL)
  expect_error({pop$param_set$values$k = -1}, regexp = NULL)
  expect_error({pop$param_set$values$kernel = "none"}, regexp = NULL)

  pop$param_set$values$kernel = NULL
  expect_error(pop$train(list(task))$output, regexp = NULL)
})
