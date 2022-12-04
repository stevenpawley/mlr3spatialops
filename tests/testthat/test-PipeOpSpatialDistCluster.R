library(data.table)
library(mlr3)
library(mlr3pipelines)

test_that("test PipeOpSpatialDistCluster basic usage", {
  task = tsk('boston_housing')
  pop = PipeOpSpatialDistCluster$new()
  pop$param_set$values$lat = 'lat'
  pop$param_set$values$lon = 'lon'
  pop$train(list(task))

  result = pop$predict(list(task))[[1]]$data()
  expect_named(result,
               c(task$feature_names, task$target_names, paste0("geodist", 1:5)),
               ignore.order = TRUE)

})

test_that("test PipeOpSpatialDistCluster calculation accuracy", {
  df = data.table(x = 1:5, y = 1:5, z = 1)
  centres = kmeans(df, 1)
  feature_names = c("x", "y")
  ref = centres$centers[, feature_names]

  dists = apply(df[, feature_names], 1, function(x, ref) {
    sqrt((x[1] - ref[1])^2 + (x[2] - ref[2])^2)
  }, ref)

  task = mlr3::as_task_regr(df, target = "z")
  pop = PipeOpSpatialDistCluster$new()
  pop$param_set$values$lat = 'x'
  pop$param_set$values$lon = 'y'
  pop$param_set$values$k = 1
  result = pop$train(list(task))$output

  expect_equal(result$data()$geodist, dists)
})
