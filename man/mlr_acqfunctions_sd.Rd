% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AcqFunctionSD.R
\name{mlr_acqfunctions_sd}
\alias{mlr_acqfunctions_sd}
\alias{AcqFunctionSD}
\title{Acquisition Function Standard Deviation}
\description{
Posterior Standard Deviation.
}
\section{Dictionary}{

This \link{AcqFunction} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary}
\link{mlr_acqfunctions} or with the associated sugar function \code{\link[=acqf]{acqf()}}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{mlr_acqfunctions$get("sd")
acqf("sd")
}\if{html}{\out{</div>}}
}

\examples{
if (requireNamespace("mlr3learners") &
    requireNamespace("DiceKriging") &
    requireNamespace("rgenoud")) {
  library(bbotk)
  library(paradox)
  library(mlr3learners)
  library(data.table)

  fun = function(xs) {
    list(y = xs$x ^ 2)
  }
  domain = ps(x = p_dbl(lower = -10, upper = 10))
  codomain = ps(y = p_dbl(tags = "minimize"))
  objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)

  instance = OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = trm("evals", n_evals = 5))

  instance$eval_batch(data.table(x = c(-6, -5, 3, 9)))

  learner = default_gp()

  surrogate = srlrn(learner, archive = instance$archive)

  acq_function = acqf("sd", surrogate = surrogate)

  acq_function$surrogate$update()
  acq_function$update()
  acq_function$eval_dt(data.table(x = c(-1, 0, 1)))
}
}
\seealso{
Other Acquisition Function: 
\code{\link{AcqFunction}},
\code{\link{mlr_acqfunctions}},
\code{\link{mlr_acqfunctions_aei}},
\code{\link{mlr_acqfunctions_cb}},
\code{\link{mlr_acqfunctions_ehvi}},
\code{\link{mlr_acqfunctions_ehvigh}},
\code{\link{mlr_acqfunctions_ei}},
\code{\link{mlr_acqfunctions_ei_log}},
\code{\link{mlr_acqfunctions_eips}},
\code{\link{mlr_acqfunctions_mean}},
\code{\link{mlr_acqfunctions_multi}},
\code{\link{mlr_acqfunctions_pi}},
\code{\link{mlr_acqfunctions_smsego}},
\code{\link{mlr_acqfunctions_stochastic_cb}},
\code{\link{mlr_acqfunctions_stochastic_ei}}
}
\concept{Acquisition Function}
\section{Super classes}{
\code{\link[bbotk:Objective]{bbotk::Objective}} -> \code{\link[mlr3mbo:AcqFunction]{mlr3mbo::AcqFunction}} -> \code{AcqFunctionSD}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-AcqFunctionSD-new}{\code{AcqFunctionSD$new()}}
\item \href{#method-AcqFunctionSD-clone}{\code{AcqFunctionSD$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="eval"><a href='../../bbotk/html/Objective.html#method-Objective-eval'><code>bbotk::Objective$eval()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="format"><a href='../../bbotk/html/Objective.html#method-Objective-format'><code>bbotk::Objective$format()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="help"><a href='../../bbotk/html/Objective.html#method-Objective-help'><code>bbotk::Objective$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="print"><a href='../../bbotk/html/Objective.html#method-Objective-print'><code>bbotk::Objective$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3mbo" data-topic="AcqFunction" data-id="eval_dt"><a href='../../mlr3mbo/html/AcqFunction.html#method-AcqFunction-eval_dt'><code>mlr3mbo::AcqFunction$eval_dt()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3mbo" data-topic="AcqFunction" data-id="eval_many"><a href='../../mlr3mbo/html/AcqFunction.html#method-AcqFunction-eval_many'><code>mlr3mbo::AcqFunction$eval_many()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3mbo" data-topic="AcqFunction" data-id="reset"><a href='../../mlr3mbo/html/AcqFunction.html#method-AcqFunction-reset'><code>mlr3mbo::AcqFunction$reset()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3mbo" data-topic="AcqFunction" data-id="update"><a href='../../mlr3mbo/html/AcqFunction.html#method-AcqFunction-update'><code>mlr3mbo::AcqFunction$update()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AcqFunctionSD-new"></a>}}
\if{latex}{\out{\hypertarget{method-AcqFunctionSD-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AcqFunctionSD$new(surrogate = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{surrogate}}{(\code{NULL} | \link{SurrogateLearner}).}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AcqFunctionSD-clone"></a>}}
\if{latex}{\out{\hypertarget{method-AcqFunctionSD-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AcqFunctionSD$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
