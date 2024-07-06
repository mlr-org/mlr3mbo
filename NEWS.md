# mlr3mbo 0.2.4

* fix: Improve runtime of `AcqOptimizer` by setting `check_values = FALSE`.

# mlr3mbo 0.2.3

* compatibility: Work with new bbotk and mlr3tuning version 1.0.0.

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
* feat: Acquisition functions now assert whether surrogates match their required predict type.
* fix: Unloading `mlr3mbo` removes optimizers and tuners from the dictionaries.
* docs: faster examples.
* feat: characters in surrogate regression tasks are no longer automatically converted to factors.
        `default_surrogate` now respects this and gained an appropriate pipeline step.
* feat: `AcqFunctionAEI` added.
* docs: fix of docs, README and bibentries.

# mlr3mbo 0.1.1

* Initial upload to CRAN.

