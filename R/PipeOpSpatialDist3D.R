#' @title Add buffer distances to 3D reference locations as features
#'
#' @description
#' Adds new variables based on the 3D Euclidean distances to reference locations.
#'
#' @return a `PipeOpSpatialDist3D` R6 class object
#' @export
#'
#' @examples
#' library(mlr3)
#' library(mlr3pipelines)
#'
#' task = tsk('boston_housing')
#' pop = PipeOpSpatialDist3D$new()
#' pop$param_set$values$lat = 'lat'
#' pop$param_set$values$lon = 'lon'
#' pop$train(list(task))
#' pop$predict(list(task))[[1]]$data()
PipeOpSpatialDist3D = R6::R6Class(
  "PipeOpSpatialDist3D",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,

  public = list(

    #' @description
    #' Create a new PipeOpSpatialDist3D object.
    #' @param id character, identifier for the class instance.
    #' @param param_vals named list of hyperparameters.
    #' @return A new `PipeOpSpatialDist3D` object.
    initialize =
      function(id = "geodist3d",
               param_vals = list(prefix = "geodist", minimum = TRUE)) {
        ps = paradox::ParamSet$new(
          params = list(
            paradox::ParamUty$new(
              id = "lat",
              tags = c("train", "predict", "required")
            ),
            paradox::ParamUty$new(
              id = "lon",
              tags = c("train", "predict", "required")
            ),
            paradox::ParamUty$new(
              id = "depth",
              tags = c("train", "predict", "required")
            ),
            paradox::ParamUty$new(
              id = "ref_lat",
              tags = c("train", "predict", "required")
            ),
            paradox::ParamUty$new(
              id = "ref_lon",
              tags = c("train", "predict", "required")
            ),
            paradox::ParamUty$new(
              id = "ref_depth",
              tags = c("train", "predict", "required")
            ),
            paradox::ParamLgl$new(
              id = "minimum",
              tags = c("train", "predict", "required"),
              default = TRUE
            ),
            paradox::ParamUty$new(
              id = "prefix",
              tags = c("train", "predict"),
              default = "geodist"
            )
          )
        )

        super$initialize(
          id,
          param_set = ps,
          param_vals = param_vals,
          feature_types = c("numeric", "integer")
        )
      }
  ),

  private = list(
    geo_dist_3d_calc = function(x, a, b, c) {
      apply(x, 1, function(x, a, b, c) {
        sqrt((x[1] - a) ^ 2 + (x[2] - b) ^ 2 + (x[3] - c) ^ 2)
      },
      a = a, b = b, c = c)
    },

    .transform = function(task) {
      cols = c(
        self$param_set$values$lat,
        self$param_set$values$lon,
        self$param_set$values$depth
      )

      if (isFALSE(self$param_set$values$minimum)) {
        dist_vals = private$geo_dist_3d_calc(
          df = task$data()[, .SD, .SDcols = cols],
          a = self$state$ref_lat,
          b = self$state$ref_lon,
          c = self$state$ref_depth
        )

      } else {
        refs = data.table(
          y = self$param_set$values$ref_lat,
          x = self$param_set$values$ref_lon,
          z = self$param_set$values$ref_depth
        )
        nn = nabor::knn(
          data = as.matrix(refs),
          query = as.matrix(task$data()[, .SD, .SDcols = cols]),
          k = 1
        )
        dist_vals <- as.numeric(nn$nn.dists)
      }

      if (inherits(dist_vals, "numeric")) {
        dist_vals = data.table(dist_vals)
        data.table::setnames(dist_vals, self$param_set$values$prefix)
      } else if (inherits(dist_vals, "matrix")) {
        dist_vals = as.data.table(t(dist_vals))
        data.table::setnames(
          dist_vals,
          paste0(self$param_set$values$prefix, seq_len(ncol(dist_vals)))
        )
      }

      task$cbind(dist_vals)
    }
  )
)
