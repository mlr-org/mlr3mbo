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

