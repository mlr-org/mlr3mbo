# mlr3mbo (development version)

* fix: `acqo()` now raises an error when `terminator` or `callbacks` are combined with a dictionary key as `optimizer`, because these arguments were silently discarded before.
* fix: `AcqOptimizer` now respects `warmstart_size` when warm-starting a single-objective acquisition function on a multi-objective archive, instead of evaluating the entire non-dominated front.
* fix: `AcqOptimizer` and its subclasses gained `$label` and `$man` fields, so that `as.data.table(mlr_acqoptimizers)` no longer errors.
* fix: `bayesopt_mpcl()` now logs a warning when a surrogate or acquisition function error is caught and a randomly sampled point is proposed, consistent with the other loop functions.
* fix: `bayesopt_parego()` now subsets the minimization multiplier to the target columns, so the scalarization signs no longer misalign when the codomain holds non-target columns.
* fix: `bayesopt_ego()`, `bayesopt_emo()`, `bayesopt_mpcl()`, `bayesopt_parego()`, and `bayesopt_smsego()` no longer silently disable random interleaving when the archive already contains a user-supplied initial design and `init_design_size` is `NULL`.
* fix: `bayesopt_parego()` no longer silently degrades to random search when an objective is constant, because the zero-range scaling now maps the constant objective to a constant value instead of `NaN`.
* fix: `AcqFunctionAEI` no longer errors during `$update()` when the surrogate model is not a `"regr.km"` model and now falls back to a noise variance of `0` as documented.
* fix: `AcqFunctionEHVI`, `AcqFunctionEHVIGH`, and `AcqFunctionSmsEgo` now apply the surrogate's output transformation to `ys_front`, so the front and the reference point are compared on the same scale.
* fix: `AcqFunctionEI`, `AcqFunctionEILog`, `AcqFunctionPI`, and `AcqFunctionStochasticEI` now ignore the missing outcomes of pending evaluations when determining the best observed value, so `y_best` is no longer `NA` on an asynchronous archive with more than one worker.
* fix: `AcqFunctionEIPS` now correctly divides the expected improvement by the predicted time instead of behaving like plain expected improvement.
* fix: `AcqFunctionSmsEgo` now accepts a `lambda` below `1` during construction, consistent with the lower bound of its `constants` parameter set.
* fix: `AcqFunctionMulti` now raises an informative error when the wrapped acquisition functions do not share the same domain instead of failing with a cryptic type error.
* fix: `AcqFunctionMulti` can now be deep cloned without error and preserves the shared surrogate across the wrapped acquisition functions.
* fix: `AcqFunctionStochasticCB` now clears the sampled lambda on `$reset()`, so a reused optimizer draws a fresh initial lambda for each run as documented.
* BREAKING CHANGE: `AcqOptimizerDirect` no longer supports restarts and its `restart_strategy` and `max_restarts` parameters have been removed, because the deterministic `NLOPT_GN_DIRECT_L` algorithm ignores the starting point, so restarts only repeated the identical search with a smaller evaluation budget.
* fix: `AcqOptimizerDirect`, `AcqOptimizerLbfgsb`, `AcqOptimizerLocalSearch`, and `AcqOptimizerRandomSearch` now support the `skip_already_evaluated` parameter (default `TRUE`) and reject an already evaluated candidate, so an `AcqOptimizerLbfgsb` starting at the incumbent no longer re-proposes and re-evaluates the same point every iteration.
* fix: `AcqOptimizerDirect`, `AcqOptimizerLbfgsb`, `AcqOptimizerLocalSearch`, and `AcqOptimizerRandomSearch` can now be deep cloned without error.
* fix: `AcqOptimizerDirect` and `AcqOptimizerLbfgsb` no longer expose the `minf_max` parameter, which was not a valid `nloptr` option and was silently ignored.
* fix: `AcqOptimizerDirect` and `AcqOptimizerLbfgsb` now accept `maxeval = -1L` to deactivate the evaluation limit, as documented.
* fix: `AcqOptimizerDirect` and `AcqOptimizerLbfgsb` now raise an informative error for non-numeric search spaces instead of failing confusingly inside `nloptr` or silently degrading to random search under `catch_errors = TRUE`.
* fix: `AcqOptimizerDirect` and `AcqOptimizerLbfgsb` now reset their `state` at the start of each `optimize()` call and clear it on `reset()`, so the `state` no longer grows unboundedly across a Bayesian optimization loop.
* fix: `AcqOptimizerLbfgsb` now raises a catchable acquisition function optimizer error instead of an unrelated error when no restart produces a valid solution, so the loop function can fall back to a randomly sampled point.
* fix: `AcqOptimizerLbfgsb` no longer fails when the incumbent lies on a search space bound, which previously caused the optimization to silently degenerate into random search.
* fix: `default_acqfunction()` now raises an informative error for unsupported instance classes instead of returning invisible `NULL`.
* fix: `AcqOptimizerLocalSearch` now populates its `state` field with the result of the last `bbotk::local_search()` call and clears it on `reset()`, and no longer references the unavailable `cmaes` package in its documentation.
* fix: `OptimizerADBO` and `TunerADBO` now draw the initial lambda from an exponential distribution as documented, so that the `lambda` parameter has an effect.
* fix: `OptimizerAsyncMbo` now proposes a randomly sampled point when the archive contains no finished evaluations yet, instead of failing to train the surrogate on an empty archive.
* fix: `mlr_loop_functions$get()` now raises an error when additional arguments are passed, because loop functions are stored as plain values and the arguments were silently discarded before.
* fix: `OutputTrafo$max_to_min` now requires a named vector whose names match `$cols_y`, so invalid assignments fail immediately instead of causing a subscript error during the transformation.
* fix: `OptimizerAsyncMbo` and `TunerAsyncMbo` now raise an informative error when the `param_set` construction argument is not a `ParamSet`.
* fix: `OptimizerAsyncMbo` no longer ignores its `id` construction argument, so `OptimizerADBO` and `TunerADBO` now correctly report the id `"adbo"` instead of `"async_mbo"`.
* fix: `OptimizerMbo` and `TunerMbo` now update the surrogate a final time even when the optimization exits through a termination error, e.g., when the archive is already at budget or the terminator triggers between two evaluations.
* fix: `OutputTrafoLog` and `OutputTrafoStandardize` no longer produce `NaN` or `Inf` values when all observed outcomes are identical.
* fix: `OutputTrafoLog` no longer produces infinite values when the range of the observed outcomes is tiny relative to their magnitude, because the epsilon padding is now floored at the local floating point precision.
* fix: `ResultAssignerSurrogate` no longer errors when the archive contains duplicated x-configurations.
* fix: `srlrn()` now correctly unwraps a single learner supplied in a list instead of erroring.
* fix: `srlrn()` now implements the documented replication of a single learner when `cols_y` or the `archive` reference more than one target variable, returning a `SurrogateLearnerCollection` with deep clones of the learner.
* fix: `TunerAsyncMbo` now accepts the documented `result_assigner` construction argument and forwards it to `OptimizerAsyncMbo`.
* fix: `Surrogate` now provides default `output_trafo` and `output_trafo_must_be_considered` fields, so third-party subclasses work with the acquisition functions without implementing output transformation support.
* fix: `SurrogateLearner$predict()` no longer modifies the data.table passed as `xdt` by reference.
* fix: `SurrogateLearner` and `SurrogateLearnerCollection` now validate assignments to the `learner`, `input_trafo`, and `output_trafo` fields, so invalid values are rejected immediately instead of failing later during updating or predicting.
* fix: `SurrogateLearner$predict()` now always returns a `data.table` with columns `mean` and `se` as documented, instead of a bare named list in configurations without an inverting output transformation.

# mlr3mbo 1.1.1

* fix: `acqo()` now correctly returns the `mlr_acqoptimizers` dictionary when called with no arguments (#211).
* fix: `AcqFunctionEILog` now provides a more informative error message when the surrogate is not configured with the correct output transformation.
* fix: `AcqOptimizerDirect` and `AcqOptimizerLbfgsb` now correctly enforce the `max_restarts` limit in all cases.
* fix: `SurrogateLearner` and `SurrogateLearnerCollection` now correctly apply their output transformation after imputing running evaluations.

# mlr3mbo 1.1.0

* compatibility: rush 1.0.0 (#202).
* feat: `Surrogate` gained a `$check()` method (#200).

# mlr3mbo 1.0.0

* feat: Added `mlr_acqoptimizers` dictionary with pre-defined acquisition function optimizers (`AcqOptimizerDirect`, `AcqOptimizerLbfgsb`, `AcqOptimizerLocalSearch`, `AcqOptimizerRandomSearch`).
* perf: Default surrogate model, acquisition function, optimizer, and further settings of `OptimizerMbo` are now empirically derived from a large-scale benchmark study, significantly improving out-of-the-box optimization performance.
* feat: Added `Mlr3ErrorMbo*` condition classes.

# mlr3mbo 0.3.3

* compatibility: bbotk 1.7.0

# mlr3mbo 0.3.2

* compatibility: mlr3learners 0.13.0

# mlr3mbo 0.3.1

* chore: maintainer change.
* chore: work with new mlr3pipelines version 0.9.0 (fix for tests only).
* test: `expect_rush_reset` changes related to rush developments.
* fix: allow `InputTrafoUnitcube` to work in mixed spaces.

# mlr3mbo 0.3.0

* fix: logger changes related to bbotk.
* fix: assure that candidates after acquisition function optimization are always within bounds.
* perf: minor changes to speed up predictions with `SurrogateLearner` and `SurrogateLearnerCollection`.
* feat: added supported for input and output transformations (see `InputTrafo`, `OutputTrafo` and the related classes).
* refactor: dropped functionality to assert insample performance of the surrogate model completely.

# mlr3mbo 0.2.9

* chore: silence rush logger and fixed some partial matches, depend on mlr3 >= 0.22.1.
* test: fix `test_AcqFunctionMulti`, robustify helper and loading.
* test: fix `test_ResultAssignerArchive` and `test_ResultAssignerSurrogate` due to upcoming changes of internal tuned values in mlr3tuning 1.3.1.

# mlr3mbo 0.2.8

* docs: gracefully exit examples of `OptimizerAsyncMbo`, `OptimizerADBO`, `TunerAsyncMbo`, and `TunerADBO` if Redis is not available.
* test: skip tests involving asynchronous logic if Redis is not available.

# mlr3mbo 0.2.7

* refactor: refactored `SurrogateLearner` and `SurrogateLearnerCollection` to allow updating on an asynchronous `Archive`.
* feat: added experimental `OptimizerAsyncMbo`, `OptimizerADBO`, `TunerAsyncMbo`, and `TunerADBO` that allow for asynchronous optimization.
* feat: added `AcqFunctionStochasticCB` and `AcqFunctionStochasticEI` that are useful for asynchronous optimization.
* docs: minor changes to highlight differences between batch and asynchronous objects related to asynchronous support.
* refactor: `AcqFunction`s and `AcqOptimizer` gained a `reset()` method.

# mlr3mbo 0.2.6

* refactor: extract internal tuned values in instance.

# mlr3mbo 0.2.5

* docs: move vignette to mlr3book.
* feat: add `AcqFunctionMulti` that can wrap multiple acquisition functions resulting in a multi-objective acquisition function problem.
* feat: support callbacks in `AcqOptimizer`.
* feat: allow `AcqFunctionEI` to be adjusted by epsilon to strengthen exploration.

# mlr3mbo 0.2.4

* fix: improve runtime of `AcqOptimizer` by setting `check_values = FALSE`.

# mlr3mbo 0.2.3

* compatibility: work with new bbotk and mlr3tuning version 1.0.0.

# mlr3mbo 0.2.2

* refactor: compatibility with upcoming paradox upgrade.
* feat: `OptimizerMbo` and `TunerMbo` now update the `Surrogate` a final time after the optimization process finished to
        ensure that the `Surrogate` correctly reflects the state of being trained on all data seen during optimization.
* fix: `AcqFunction` domain construction now respects `Surrogate` cols_x field.
* feat: support more than one candidate point as a result of acquisition function optimization even for
        non-batch acquisition functions.
* feat: added `default_gp` and `default_rf` helpers that allow for construction of a default
        Gaussian Process and random forest as for example used within `default_surrogate`.
* refactor: changed Gaussian Process and random forest defaults (in `default_gp` and `default_rf` and therefore also in
            `default_surrogate`). Gaussian Process now uses a `"matern5_2"` kernel. Random forest now uses 100 trees.
            The number of trees used in the fallback random forest was reduced to 10.

# mlr3mbo 0.2.1

* docs: updated some references in vignette.
* refactor: minor clean up of the internal structure of all loop functions.
* perf: default initial design constructed based on a Sobol sequence in all loop functions.
* refactor: no longer depend on `mlr3tuning` but import instead.
* refactor: `srlrn` sugar function now can construct both a `SurrogateLearner` and
            `SurrogateLearnerCollection`; dropped `srlrnc`.
* feat: added `AcqFunctionSD`, `AcqFunctionEHVI` and `AcqFunctionEHVIGH`, introduced
        `bayesopt_emo` loop function.
* feat: `AcqFunction`s now include a `$packages` field stating required packages which are checked
        for whether their namespace can be loaded prior to optimization.
* fix: fixed bug in `fix_xdt_missing()` helper function.
* BREAKING CHANGE: renaming `default_loopfun` -> `default_loop_function`,
                   `default_acqfun` -> `default_acqfunction`,
                   `default_acqopt` -> `default_acqoptimizer`.
* BREAKING CHANGE: `result_function`s now replaced by `ResultAssigner`s.
* BREAKING CHANGE: renamed `$model` field of all `Surrogate` classes to `$learner`.
* BREAKING CHANGE: For all `Surrogate` and `AcquisitionFunction` classes fields `*_cols` renamed to
                   `cols_*` (e.g., `x_cols` to `cols_x`).

# mlr3mbo 0.1.2

* refactor: adapt to mlr3tuning 0.18.0.
* feat: acquisition functions now assert whether surrogates match their required predict type.
* fix: unloading `mlr3mbo` removes optimizers and tuners from the dictionaries.
* docs: faster examples.
* feat: characters in surrogate regression tasks are no longer automatically converted to factors.
        `default_surrogate` now respects this and gained an appropriate pipeline step.
* feat: `AcqFunctionAEI` added.
* docs: fix of docs, README and bibentries.

# mlr3mbo 0.1.1

* Initial upload to CRAN.

