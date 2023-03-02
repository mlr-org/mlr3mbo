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

