#' @title Add buffer distances to 3D reference locations as features
#'
#' @description
#' Adds new variables based on the 3D Euclidean distances to reference locations.
#'
#' @return a `PipeOpSpatialDist` R6 class object
#' @export
#' @examples
#' library(data.table)
#' library(mlr3)
#' library(mlr3pipelines)
#' library(mlr3spatialops)
#'
#' # create a dataset consisting of two coordinate features
#' df = expand.grid(x = 1:10, y = 1:10)
#' df$class = as.factor(c(rep("class1", 50), rep("class2", 50)))
#' task = as_task_classif(df, target = "class")
#'
#' pop = PipeOpSpatialDist$new()
#' pop$param_set$values$ycolname = "y"
#' pop$param_set$values$xcolname = "x"
#' pop$param_set$values$ref_ys = c(min(task$data()$y), max(task$data()$y))
#' pop$param_set$values$ref_xs = c(min(task$data()$x), max(task$data()$x))
#' pop$param_set$values$minimum = FALSE
#'
#' pop$train(list(task))
#' result = pop$predict(list(task))
#' result$output$data()
PipeOpSpatialDist = R6::R6Class(
  "PipeOpSpatialDist",
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    #' @description
    #' Create a new PipeOpSpatialDist3D object.
    #' @param id character, identifier for the class instance.
    #' @param param_vals named list of hyperparameters.
    #' @return A new `PipeOpSpatialDist3D` object.
    initialize =
      function(id = "geodist", param_vals = list(prefix = "geodist", minimum = FALSE)) {
        ps = paradox::ps(
          xcolname = paradox::p_uty(tags = c("train", "predict", "required")),
          ycolname = paradox::p_uty(tags = c("train", "predict", "required")),
          zcolname = paradox::p_uty(tags = c("train", "predict")),
          ref_xs = paradox::p_uty(tags = c("train", "predict")),
          ref_ys = paradox::p_uty(tags = c("train", "predict")),
          ref_zs = paradox::p_uty(tags = c("train", "predict")),
          k = paradox::p_int(tags = c("train", "predict"), default = 5),
          minimum = paradox::p_lgl(default = FALSE, tags = c("train", "predict")),
          prefix = paradox::p_uty(default = "geodist", tags = c("train", "predict"))
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
    geodist = function(a, b) {
      apply(a, 1, function(a) dist(rbind(a, b)))
    },

    .get_state = function(task) {
      cols = c(
        self$param_set$values$ycolname,
        self$param_set$values$xcolname,
        self$param_set$values$zcolname
      )
      data = task$data()[, .SD, .SDcols = cols]

      # create matrix of reference locations (kmeans or user-defined)
      if ("k" %in% names(self$param_set$values)) {
        km = stats::kmeans(data, centers = self$param_set$values$k)
        refs = data.table::as.data.table(km$centers)
      } else {
        refs = cbind(
          self$param_set$values$ref_ys,
          self$param_set$values$ref_xs,
          self$param_set$values$ref_zs
        )
      }

      # some checks
      if (is.null(refs)) {
        stop("No reference locations provided and `k` is not set.")
      }

      if (ncol(refs) != ncol(data)) {
        stop("Reference columns refer to columns not present in the data.")
      }

      list(cols = cols, refs = refs)
    },

    .transform = function(task) {
      data = task$data()[, .SD, .SDcols = self$state$cols]

      if (isFALSE(self$param_set$values$minimum)) {
        dist_vals = apply(self$state$refs, 1, function(ref, data) {
          private$geodist(data, ref)
        }, data = data)

      } else {
        dist_vals = nabor::knn(
          data = as.matrix(self$state$refs),
          query = as.matrix(data),
          k = 1
        )$nn.dists
      }

      dist_vals = data.table::as.data.table(dist_vals)
      data.table::setnames(
        dist_vals,
        paste0(self$param_set$values$prefix, seq_len(ncol(dist_vals)))
      )

      # update task by changing in-place
      task$cbind(dist_vals)
    }
  )
)
