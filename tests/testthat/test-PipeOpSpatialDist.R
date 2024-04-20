library(data.table)
library(mlr3)
library(mlr3pipelines)

test_that("test PipeOpSpatialDist basic usage", {
  task = tsk('boston_housing')
  pop = PipeOpSpatialDist$new()
  pop$param_set$values$ycolname = "lat"
  pop$param_set$values$xcolname = "lon"
  pop$param_set$values$ref_ys = c(min(task$data()$lat), max(task$data()$lat))
  pop$param_set$values$ref_xs = c(min(task$data()$lon), max(task$data()$lon))

  pop$train(list(task))

  result = pop$predict(list(task))[[1]]$data()
  expect_named(result,
               c(task$feature_names, task$target_names, paste0("geodist", 1:2)),
               ignore.order = TRUE)

  pop$param_set$values$minimum = TRUE
  pop$train(list(task))
  result = pop$predict(list(task))[[1]]$data()
  expect_named(result,
               c(task$feature_names, task$target_names, "geodist1"),
               ignore.order = TRUE)
})

test_that("test PipeOpSpatialDist with clustering option", {
  task = tsk('boston_housing')
  pop = PipeOpSpatialDist$new()
  pop$param_set$values$ycolname = 'lat'
  pop$param_set$values$xcolname = 'lon'
  pop$param_set$values$k = 7
  pop$train(list(task))

  result = pop$predict(list(task))[[1]]$data()
  expect_named(result,
               c(task$feature_names, task$target_names, paste0("geodist", 1:7)),
               ignore.order = TRUE)
})
