% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PipeOpSpatialLag.R
\name{PipeOpSpatialLag}
\alias{PipeOpSpatialLag}
\title{Add aggregated values of nearest neighbours as a spatial lag features}
\value{
An `PipeOpSpatialLag` object.
}
\description{
Adds new variables based on the weighted aggregation of the target variable
of neighbouring observations. The closest neighbours are determined based on
the available features set by 'affect_columns' using the `nabors` package for
fast KNN queries.
}
\examples{
library(data.table)
library(mlr3)
library(mlr3pipelines)

task = tsk('boston_housing')
pop = PipeOpSpatialLag$new()
pop$param_set$values$affect_columns = selector_name(c('lat', 'lon'))
res = pop$train(list(task))
res$output$data()
pop$predict(list(task))$output$data()
}
\section{Super classes}{
\code{\link[mlr3pipelines:PipeOp]{mlr3pipelines::PipeOp}} -> \code{\link[mlr3pipelines:PipeOpTaskPreproc]{mlr3pipelines::PipeOpTaskPreproc}} -> \code{\link[mlr3pipelines:PipeOpTaskPreprocSimple]{mlr3pipelines::PipeOpTaskPreprocSimple}} -> \code{PipeOpSpatialLag}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PipeOpSpatialLag-new}{\code{PipeOpSpatialLag$new()}}
\item \href{#method-PipeOpSpatialLag-clone}{\code{PipeOpSpatialLag$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="help"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-help'><code>mlr3pipelines::PipeOp$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="predict"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-predict'><code>mlr3pipelines::PipeOp$predict()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="print"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-print'><code>mlr3pipelines::PipeOp$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3pipelines" data-topic="PipeOp" data-id="train"><a href='../../mlr3pipelines/html/PipeOp.html#method-PipeOp-train'><code>mlr3pipelines::PipeOp$train()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PipeOpSpatialLag-new"></a>}}
\if{latex}{\out{\hypertarget{method-PipeOpSpatialLag-new}{}}}
\subsection{Method \code{new()}}{
Create a new PipeOpSpatialLag object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PipeOpSpatialLag$new(id = "lag", param_vals = list())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{character, identifier for the class instance.}

\item{\code{param_vals}}{named list of hyperparameters.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new `PipeOpSpatialLag` object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PipeOpSpatialLag-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PipeOpSpatialLag-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PipeOpSpatialLag$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
