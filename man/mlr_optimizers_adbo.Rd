% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OptimizerADBO.R
\name{mlr_optimizers_adbo}
\alias{mlr_optimizers_adbo}
\alias{OptimizerADBO}
\title{Asynchronous Decentralized Bayesian Optimization}
\description{
\code{OptimizerADBO} class that implements Asynchronous Decentralized Bayesian Optimization (ADBO).
ADBO is a variant of Asynchronous Model Based Optimization (AMBO) that uses \link{AcqFunctionStochasticCB} with exponential lambda decay.

Currently, only single-objective optimization is supported and \link{OptimizerADBO} is considered an experimental feature and API might be subject to changes.
}
\note{
The lambda parameter of the confidence bound acquisition function controls the trade-off between exploration and exploitation.
A large lambda value leads to more exploration, while a small lambda value leads to more exploitation.
The initial lambda value of the acquisition function used on each worker is drawn from an exponential distribution with rate \code{1 / lambda}.
ADBO can use periodic exponential decay to reduce lambda periodically for a given time step \code{t} with the formula \code{lambda * exp(-rate * (t \%\% period))}.
The \link{SurrogateLearner} is configured to use a random forest and the \link{AcqOptimizer} is a random search with a batch size of 1000 and a budget of 10000 evaluations.
}
\section{Parameters}{

\describe{
\item{\code{lambda}}{\code{numeric(1)}\cr
Value used for sampling the lambda for each worker from an exponential distribution.}
\item{\code{rate}}{\code{numeric(1)}\cr
Rate of the exponential decay.}
\item{\code{period}}{\code{integer(1)}\cr
Period of the exponential decay.}
\item{\code{initial_design}}{\code{data.table::data.table()}\cr
Initial design of the optimization.
If \code{NULL}, a design of size \code{design_size} is generated with the specified \code{design_function}.
Default is \code{NULL}.}
\item{\code{design_size}}{\code{integer(1)}\cr
Size of the initial design if it is to be generated.
Default is \code{100}.}
\item{\code{design_function}}{\code{character(1)}\cr
Sampling function to generate the initial design.
Can be \code{random} \link[paradox:generate_design_random]{paradox::generate_design_random}, \code{lhs} \link[paradox:generate_design_lhs]{paradox::generate_design_lhs}, or \code{sobol} \link[paradox:generate_design_sobol]{paradox::generate_design_sobol}.
Default is \code{sobol}.}
\item{\code{n_workers}}{\code{integer(1)}\cr
Number of parallel workers.
If \code{NULL}, all rush workers specified via \code{\link[rush:rush_plan]{rush::rush_plan()}} are used.
Default is \code{NULL}.}
}
}

\examples{
\donttest{
if (requireNamespace("rush") &
    requireNamespace("mlr3learners") &
    requireNamespace("DiceKriging") &
    requireNamespace("rgenoud")) {

  if (redis_available()) {

    library(bbotk)
    library(paradox)
    library(mlr3learners)

    fun = function(xs) {
      list(y = xs$x ^ 2)
    }
    domain = ps(x = p_dbl(lower = -10, upper = 10))
    codomain = ps(y = p_dbl(tags = "minimize"))
    objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)

    instance = OptimInstanceAsyncSingleCrit$new(
      objective = objective,
      terminator = trm("evals", n_evals = 10))

    rush::rush_plan(n_workers=2)

    optimizer = opt("adbo", design_size = 4, n_workers = 2)

    optimizer$optimize(instance)
  } else {
    message("Redis server is not available.\nPlease set up Redis prior to running the example.")
  }
}
}
}
\references{
\itemize{
\item Egelé, Romain, Guyon, Isabelle, Vishwanath, Venkatram, Balaprakash, Prasanna (2023).
\dQuote{Asynchronous Decentralized Bayesian Optimization for Large Scale Hyperparameter Optimization.}
In \emph{2023 IEEE 19th International Conference on e-Science (e-Science)}, 1--10.
}
}
\section{Super classes}{
\code{\link[bbotk:Optimizer]{bbotk::Optimizer}} -> \code{\link[bbotk:OptimizerAsync]{bbotk::OptimizerAsync}} -> \code{\link[mlr3mbo:OptimizerAsyncMbo]{mlr3mbo::OptimizerAsyncMbo}} -> \code{OptimizerADBO}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-OptimizerADBO-new}{\code{OptimizerADBO$new()}}
\item \href{#method-OptimizerADBO-optimize}{\code{OptimizerADBO$optimize()}}
\item \href{#method-OptimizerADBO-clone}{\code{OptimizerADBO$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Optimizer" data-id="format"><a href='../../bbotk/html/Optimizer.html#method-Optimizer-format'><code>bbotk::Optimizer$format()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Optimizer" data-id="help"><a href='../../bbotk/html/Optimizer.html#method-Optimizer-help'><code>bbotk::Optimizer$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3mbo" data-topic="OptimizerAsyncMbo" data-id="print"><a href='../../mlr3mbo/html/OptimizerAsyncMbo.html#method-OptimizerAsyncMbo-print'><code>mlr3mbo::OptimizerAsyncMbo$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3mbo" data-topic="OptimizerAsyncMbo" data-id="reset"><a href='../../mlr3mbo/html/OptimizerAsyncMbo.html#method-OptimizerAsyncMbo-reset'><code>mlr3mbo::OptimizerAsyncMbo$reset()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-OptimizerADBO-new"></a>}}
\if{latex}{\out{\hypertarget{method-OptimizerADBO-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OptimizerADBO$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-OptimizerADBO-optimize"></a>}}
\if{latex}{\out{\hypertarget{method-OptimizerADBO-optimize}{}}}
\subsection{Method \code{optimize()}}{
Performs the optimization on an \link[bbotk:OptimInstanceAsyncSingleCrit]{bbotk::OptimInstanceAsyncSingleCrit} until termination.
The single evaluations will be written into the \link[bbotk:ArchiveAsync]{bbotk::ArchiveAsync}.
The result will be written into the instance object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OptimizerADBO$optimize(inst)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{inst}}{(\link[bbotk:OptimInstanceAsyncSingleCrit]{bbotk::OptimInstanceAsyncSingleCrit}).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{\link[data.table:data.table]{data.table::data.table()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-OptimizerADBO-clone"></a>}}
\if{latex}{\out{\hypertarget{method-OptimizerADBO-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OptimizerADBO$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
