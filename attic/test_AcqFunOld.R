test_that("AcqFunctionEI is faster than old", {
  surrogate = SURR_KM_DETERM
  inst = MAKE_INST_1D()
  design = MAKE_DESIGN(inst)
  inst$eval_batch(design)

  acqf_old = AcqFunctionEIOld$new(surrogate = surrogate)
  acqf_old$setup(inst$archive)
  acqf_old$surrogate$update(xydt = archive_xy(inst$archive), y_cols = inst$archive$cols_y) #update surrogate model with new data
  acqf_old$update(inst$archive)

  acqf = AcqFunctionEI$new(surrogate = surrogate)
  acqf$setup(inst$archive)
  acqf$surrogate$update(xydt = archive_xy(inst$archive), y_cols = inst$archive$cols_y)
  acqf$update(inst$archive)


  xdt = MAKE_DESIGN(inst, n = 50)

  res = rbenchmark::benchmark(acqf_old$eval_dt(xdt), acqf$eval_dt(xdt), replications = 100)
  res

})