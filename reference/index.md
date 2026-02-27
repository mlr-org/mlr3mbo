# Package index

## All functions

- [`AcqFunction`](https://mlr3mbo.mlr-org.com/reference/AcqFunction.md)
  : Acquisition Function Base Class
- [`AcqOptimizer`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizer.md)
  : Acquisition Function Optimizer
- [`AcqOptimizerDirect`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizerDirect.md)
  : Direct Acquisition Function Optimizer
- [`AcqOptimizerLbfgsb`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizerLbfgsb.md)
  : L-BFGS-B Acquisition Function Optimizer
- [`AcqOptimizerLocalSearch`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizerLocalSearch.md)
  : Local Search Acquisition Function Optimizer
- [`AcqOptimizerRandomSearch`](https://mlr3mbo.mlr-org.com/reference/AcqOptimizerRandomSearch.md)
  : Random Search Acquisition Function Optimizer
- [`InputTrafo`](https://mlr3mbo.mlr-org.com/reference/InputTrafo.md) :
  Input Transformation Base Class
- [`InputTrafoUnitcube`](https://mlr3mbo.mlr-org.com/reference/InputTrafoUnitcube.md)
  : Input Transformation Unitcube
- [`OutputTrafo`](https://mlr3mbo.mlr-org.com/reference/OutputTrafo.md)
  : Output Transformation Base Class
- [`OutputTrafoLog`](https://mlr3mbo.mlr-org.com/reference/OutputTrafoLog.md)
  : Output Transformation Log
- [`OutputTrafoStandardize`](https://mlr3mbo.mlr-org.com/reference/OutputTrafoStandardize.md)
  : Output Transformation Standardization
- [`ResultAssigner`](https://mlr3mbo.mlr-org.com/reference/ResultAssigner.md)
  : Result Assigner Base Class
- [`Surrogate`](https://mlr3mbo.mlr-org.com/reference/Surrogate.md) :
  Surrogate Model
- [`SurrogateLearner`](https://mlr3mbo.mlr-org.com/reference/SurrogateLearner.md)
  : Surrogate Model Containing a Single Learner
- [`SurrogateLearnerCollection`](https://mlr3mbo.mlr-org.com/reference/SurrogateLearnerCollection.md)
  : Surrogate Model Containing Multiple Learners
- [`acqf()`](https://mlr3mbo.mlr-org.com/reference/acqf.md) : Syntactic
  Sugar Acquisition Function Construction
- [`acqfs()`](https://mlr3mbo.mlr-org.com/reference/acqfs.md) :
  Syntactic Sugar Acquisition Functions Construction
- [`acqo()`](https://mlr3mbo.mlr-org.com/reference/acqo.md) : Syntactic
  Sugar Acquisition Function Optimizer Construction
- [`default_acqfunction()`](https://mlr3mbo.mlr-org.com/reference/default_acqfunction.md)
  : Default Acquisition Function
- [`default_acqoptimizer()`](https://mlr3mbo.mlr-org.com/reference/default_acqoptimizer.md)
  : Default Acquisition Function Optimizer
- [`default_gp()`](https://mlr3mbo.mlr-org.com/reference/default_gp.md)
  : Default Gaussian Process
- [`default_loop_function()`](https://mlr3mbo.mlr-org.com/reference/default_loop_function.md)
  : Default Loop Function
- [`default_result_assigner()`](https://mlr3mbo.mlr-org.com/reference/default_result_assigner.md)
  : Default Result Assigner
- [`default_rf()`](https://mlr3mbo.mlr-org.com/reference/default_rf.md)
  : Default Random Forest
- [`default_surrogate()`](https://mlr3mbo.mlr-org.com/reference/default_surrogate.md)
  : Default Surrogate
- [`it()`](https://mlr3mbo.mlr-org.com/reference/it.md) : Syntactic
  Sugar Input Trafo Construction
- [`loop_function`](https://mlr3mbo.mlr-org.com/reference/loop_function.md)
  : Loop Functions for Bayesian Optimization
- [`mbo_defaults`](https://mlr3mbo.mlr-org.com/reference/mbo_defaults.md)
  : Defaults for OptimizerMbo
- [`mlr3mbo`](https://mlr3mbo.mlr-org.com/reference/mlr3mbo-package.md)
  [`mlr3mbo-package`](https://mlr3mbo.mlr-org.com/reference/mlr3mbo-package.md)
  : mlr3mbo: Flexible Bayesian Optimization
- [`error_random_interleave()`](https://mlr3mbo.mlr-org.com/reference/mlr3mbo_conditions.md)
  [`error_surrogate_update()`](https://mlr3mbo.mlr-org.com/reference/mlr3mbo_conditions.md)
  [`error_acq_optimizer()`](https://mlr3mbo.mlr-org.com/reference/mlr3mbo_conditions.md)
  : Condition Classes for mlr3mbo
- [`mlr_acqfunctions`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions.md)
  : Dictionary of Acquisition Functions
- [`mlr_acqfunctions_aei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_aei.md)
  [`AcqFunctionAEI`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_aei.md)
  : Acquisition Function Augmented Expected Improvement
- [`mlr_acqfunctions_cb`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_cb.md)
  [`AcqFunctionCB`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_cb.md)
  : Acquisition Function Confidence Bound
- [`mlr_acqfunctions_ehvi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvi.md)
  [`AcqFunctionEHVI`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvi.md)
  : Acquisition Function Expected Hypervolume Improvement
- [`mlr_acqfunctions_ehvigh`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvigh.md)
  [`AcqFunctionEHVIGH`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ehvigh.md)
  : Acquisition Function Expected Hypervolume Improvement via
  Gauss-Hermite Quadrature
- [`mlr_acqfunctions_ei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ei.md)
  [`AcqFunctionEI`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ei.md)
  : Acquisition Function Expected Improvement
- [`mlr_acqfunctions_ei_log`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ei_log.md)
  [`AcqFunctionEILog`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_ei_log.md)
  : Acquisition Function Expected Improvement on Log Scale
- [`mlr_acqfunctions_eips`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_eips.md)
  [`AcqFunctionEIPS`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_eips.md)
  : Acquisition Function Expected Improvement Per Second
- [`mlr_acqfunctions_mean`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_mean.md)
  [`AcqFunctionMean`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_mean.md)
  : Acquisition Function Mean
- [`mlr_acqfunctions_multi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_multi.md)
  [`AcqFunctionMulti`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_multi.md)
  : Acquisition Function Wrapping Multiple Acquisition Functions
- [`mlr_acqfunctions_pi`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_pi.md)
  [`AcqFunctionPI`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_pi.md)
  : Acquisition Function Probability of Improvement
- [`mlr_acqfunctions_sd`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_sd.md)
  [`AcqFunctionSD`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_sd.md)
  : Acquisition Function Standard Deviation
- [`mlr_acqfunctions_smsego`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_smsego.md)
  [`AcqFunctionSmsEgo`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_smsego.md)
  : Acquisition Function SMS-EGO
- [`mlr_acqfunctions_stochastic_cb`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_stochastic_cb.md)
  [`AcqFunctionStochasticCB`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_stochastic_cb.md)
  : Acquisition Function Stochastic Confidence Bound
- [`mlr_acqfunctions_stochastic_ei`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_stochastic_ei.md)
  [`AcqFunctionStochasticEI`](https://mlr3mbo.mlr-org.com/reference/mlr_acqfunctions_stochastic_ei.md)
  : Acquisition Function Stochastic Expected Improvement
- [`mlr_acqoptimizers`](https://mlr3mbo.mlr-org.com/reference/mlr_acqoptimizers.md)
  : Dictionary of Acquisition Function Optimizers
- [`mlr_input_trafos`](https://mlr3mbo.mlr-org.com/reference/mlr_input_trafos.md)
  : Dictionary of Input Transformations
- [`mlr_loop_functions`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions.md)
  : Dictionary of Loop Functions
- [`bayesopt_ego()`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_ego.md)
  : Sequential Single-Objective Bayesian Optimization
- [`bayesopt_emo()`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_emo.md)
  : Sequential Multi-Objective Bayesian Optimization
- [`bayesopt_mpcl()`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_mpcl.md)
  : Single-Objective Bayesian Optimization via Multipoint Constant Liar
- [`bayesopt_parego()`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_parego.md)
  : Multi-Objective Bayesian Optimization via ParEGO
- [`bayesopt_smsego()`](https://mlr3mbo.mlr-org.com/reference/mlr_loop_functions_smsego.md)
  : Sequential Multi-Objective Bayesian Optimization via SMS-EGO
- [`mlr_optimizers_adbo`](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_adbo.md)
  [`OptimizerADBO`](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_adbo.md)
  : Asynchronous Decentralized Bayesian Optimization
- [`mlr_optimizers_async_mbo`](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_async_mbo.md)
  [`OptimizerAsyncMbo`](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_async_mbo.md)
  : Asynchronous Model Based Optimization
- [`mlr_optimizers_mbo`](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_mbo.md)
  [`OptimizerMbo`](https://mlr3mbo.mlr-org.com/reference/mlr_optimizers_mbo.md)
  : Model Based Optimization
- [`mlr_output_trafos`](https://mlr3mbo.mlr-org.com/reference/mlr_output_trafos.md)
  : Dictionary of Output Transformations
- [`mlr_result_assigners`](https://mlr3mbo.mlr-org.com/reference/mlr_result_assigners.md)
  : Dictionary of Result Assigners
- [`mlr_result_assigners_archive`](https://mlr3mbo.mlr-org.com/reference/mlr_result_assigners_archive.md)
  [`ResultAssignerArchive`](https://mlr3mbo.mlr-org.com/reference/mlr_result_assigners_archive.md)
  : Result Assigner Based on the Archive
- [`mlr_result_assigners_surrogate`](https://mlr3mbo.mlr-org.com/reference/mlr_result_assigners_surrogate.md)
  [`ResultAssignerSurrogate`](https://mlr3mbo.mlr-org.com/reference/mlr_result_assigners_surrogate.md)
  : Result Assigner Based on a Surrogate Mean Prediction
- [`mlr_tuners_adbo`](https://mlr3mbo.mlr-org.com/reference/mlr_tuners_adbo.md)
  [`TunerADBO`](https://mlr3mbo.mlr-org.com/reference/mlr_tuners_adbo.md)
  : TunerAsync using Asynchronous Decentralized Bayesian Optimization
- [`mlr_tuners_async_mbo`](https://mlr3mbo.mlr-org.com/reference/mlr_tuners_async_mbo.md)
  [`TunerAsyncMbo`](https://mlr3mbo.mlr-org.com/reference/mlr_tuners_async_mbo.md)
  : TunerAsync using Asynchronous Model Based Optimization
- [`mlr_tuners_mbo`](https://mlr3mbo.mlr-org.com/reference/mlr_tuners_mbo.md)
  [`TunerMbo`](https://mlr3mbo.mlr-org.com/reference/mlr_tuners_mbo.md)
  : TunerBatch using Model Based Optimization
- [`ot()`](https://mlr3mbo.mlr-org.com/reference/ot.md) : Syntactic
  Sugar Output Trafo Construction
- [`ras()`](https://mlr3mbo.mlr-org.com/reference/ras.md) : Syntactic
  Sugar Result Assigner Construction
- [`redis_available()`](https://mlr3mbo.mlr-org.com/reference/redis_available.md)
  : Check if Redis Server is Available
- [`srlrn()`](https://mlr3mbo.mlr-org.com/reference/srlrn.md) :
  Syntactic Sugar Surrogate Construction
