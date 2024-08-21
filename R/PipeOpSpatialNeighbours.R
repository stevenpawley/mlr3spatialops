#' @title Use the values of nearest neighboring observations as features
#'
#' @description
#' Adds new variables based on the the target variable of k-nearest neighbouring
#' observations. The closest neighbours are determined based on the available
#' features set by 'affect_columns' using the `nabors` package for fast
#' KNN queries.
#'
#' @return a`PipeOpSpatialNeighbours` object.
#' @export
#'
#' @examples
#' library(data.table)
#' library(mlr3)
#' library(mlr3pipelines)
#'
#' task = tsk('boston_housing')
#' pop = PipeOpSpatialNeighbours$new()
#' pop$param_set$values$affect_columns = selector_name(c('lat', 'lon'))
#' res = pop$train(list(task))
#' res$output$data()
#' pop$predict(list(task))$output$data()
PipeOpSpatialNeighbours = R6::R6Class(
  'PipeOpSpatialNeighbours',
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    #' @description
    #' Create a new PipeOpSpatialNeighbours object.
    #' @param id character, identifier for the class instance.
    #' @param param_vals named list of hyperparameters.
    #' @return A new `PipeOpSpatialNeighbours` object.
    initialize =
      function(id = 'neighbours', param_vals = list(k = 5, prefix = 'nn')) {
        ps = paradox::ps(
          k = paradox::p_int(default = 5, lower = 1, tags = c("train", "k")),
          prefix = paradox::p_uty(default = "nn")
        )
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
      # nothing happens at training - just copy the training data
      # overwrite the .get_state method so that it always returns the training data
      list(training_task = task$clone())
    },

    get_neighbours = function(target_name, feature_names, train, query, k = 5,
                              prefix) {
      train_X = train[, .SD, .SDcols = feature_names]
      query_X = query[, .SD, .SDcols = feature_names]

      if (identical(train_X, query_X)) {
        nn = nabor::knn(data = train_X, query = query_X, k = k + 1)
        nn$nn.idx = nn$nn.idx[, 2:ncol(nn$nn.idx)]
        nn$nn.dists = nn$nn.dists[, 2:ncol(nn$nn.dists)]
      } else {
        nn = nabor::knn(data = train_X, query = query_X, k = k)
      }

      # get values and distances of neighbors
      idx = as.numeric(nn$nn.idx)
      dists = matrix(nn$nn.dists, ncol = k)
      nn_target_values = train[idx, ][[target_name]]
      nn_target_values = matrix(nn_target_values, ncol = k)

      # return as data.table
      dist_prefix = paste(prefix, 'dist', sep = '_')

      colnames(nn_target_values) =
        paste0(prefix, seq_len(ncol(nn_target_values)))

      colnames(dists) =
        paste0(dist_prefix, seq_len(ncol(dists)))

      nn_target_values = data.table::as.data.table(nn_target_values)
      dists = data.table::as.data.table(dists)

      return(cbind(nn_target_values, dists))
    },

    .transform = function(task) {
      new_x = private$get_neighbours(
        target_name = task$target_names,
        feature_names = task$feature_names,
        train = self$state$training_task$data(),
        query = task$data(),
        k = self$param_set$values$k,
        prefix = self$param_set$values$prefix
      )

      task$cbind(new_x)
    }
  )
)
