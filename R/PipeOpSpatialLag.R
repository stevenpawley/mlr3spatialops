#' @title Add aggregated values of nearest neighbours as a spatial lag features
#'
#' @description
#' Adds new variables based on the weighted aggregation of the target variable
#' of neighbouring observations. The closest neighbours are determined based on
#' the available features set by 'affect_columns' using the `nabors` package for
#' fast KNN queries.
#'
#' @return An `PipeOpSpatialLag` object.
#' @export
#'
#' @examples
#' library(data.table)
#' library(mlr3)
#' library(mlr3pipelines)
#'
#' task = tsk('boston_housing')
#' pop = PipeOpSpatialLag$new()
#' pop$param_set$values$affect_columns = selector_name(c('lat', 'lon'))
#' res = pop$train(list(task))
#' res$output$data()
#' pop$predict(list(task))$output$data()
PipeOpSpatialLag = R6::R6Class(
  classname = 'PipeOpSpatialLag',
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,

  public = list(

    #' @description
    #' Create a new PipeOpSpatialLag object.
    #' @param id character, identifier for the class instance.
    #' @param param_vals named list of hyperparameters.
    #' @return A new `PipeOpSpatialLag` object.
    initialize =
      function(id = 'lag', param_vals = list()) {
        ps = paradox::ParamSet$new(params = list(
          paradox::ParamInt$new(
            id = 'k',
            default = 5,
            lower = 3,
            upper = Inf,
            tags = 'train'
          ),
          paradox::ParamFct$new(
            id = 'kernel',
            default = 'inv',
            levels = c('inv', 'rectangular', 'gaussian'),
            tags = 'train'
          )
        ))

        ps$values = list(k = 5, kernel = 'inv')

        super$initialize(
          id,
          param_set = ps,
          param_vals = param_vals,
          packages = 'nabor',
          feature_types = c('numeric', 'integer')
        )
      }
  ),

  private = list(
    .get_state = function(task) {
      list(training_task = task$clone())
    },

    gaussian_kernel = function(distances, k) {
      # standardize distances and add small constant to avoid zero distances
      maxdist = distances[, k]
      maxdist[maxdist < 1.0e-6] = 1.0e-6
      W = distances / maxdist
      W = pmin(W, 1 - (1e-6))
      W = pmax(W, 1e-6)

      # create adaptive window width based on the number of neighbours
      p = 1 / (2 * k)
      qua = abs(qnorm(p))
      Wq = W * qua
      dnorm(Wq, sd = 1)
    },

    knn_train = function(target_name, feature_names, x, y, k,
                         weight_func, type) {
      train_data = x[, .SD, .SDcols = feature_names]
      query_data = y[, .SD, .SDcols = feature_names]

      if (identical(train_data, query_data)) {
        nn = nabor::knn(data = train_data, query = query_data, k = k + 1)
        nn$nn.idx = nn$nn.idx[, 2:ncol(nn$nn.idx)]
        nn$nn.dists = nn$nn.dists[, 2:ncol(nn$nn.dists)]
      } else {
        nn = nabor::knn(data = train_data, query = query_data, k = k)
      }

      # get ids and distances to neighbors
      neighbor_ids = nn$nn.idx
      D = nn$nn.dists
      D[D < 1e-6] = 1e-6

      # get values of neighbors
      neighbor_vals = x[as.integer(neighbor_ids), ][[target_name]]
      neighbor_vals = matrix(neighbor_vals, ncol = k)

      # calculate weights
      W = switch(
        weight_func,
        inv = 1 / D,
        rectangular = matrix(1, nrow = nrow(neighbor_vals), ncol = k),
        gaussian = private$gaussian_kernel(D, k)
      )

      # calculate weighted mean/mode of neighbours
      if (type == 'regr') {
        denom = rowSums(W)
        num = rowSums(neighbor_vals * W)
        fitted = num / denom
      } else if (type == 'classif') {
        fitted = sapply(seq_len(nrow(W)), function(i) {
          collapse::fmode(x = neighbor_vals[i, ], w = W[i, ])
        })
        target_type = typeof(x[[target_name]])
        fitted = as(fitted, target_type)
        fitted = factor(fitted, levels = levels(x[[target_name]]))
      }

      new_feature_name = paste(target_name, 'lag', k, weight_func, sep = '_')
      fitted = as.data.table(fitted)
      data.table::setnames(fitted, new_feature_name)
      return(fitted)
    },

    .transform = function(task) {
      output_task = task

      stopifnot(!is.null(self$param_set$values$kernel))
      stopifnot(!is.na(self$param_set$values$kernel))

      new_x = private$knn_train(
        target_name = task$target_names,
        feature_names = task$feature_names,
        x = self$state$training_task$data(),
        y = task$data(),
        k = self$param_set$values$k,
        weight_func = self$param_set$values$kernel,
        type = task$task_type
      )

      return(output_task$cbind(new_x))
    }
  )
)
