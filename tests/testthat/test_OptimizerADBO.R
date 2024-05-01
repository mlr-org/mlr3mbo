test_that("adbo optimizer works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
  instance = oi_async(
    objective = OBJ_2D,
    search_space = PS_2D,
    terminator = trm("evals", n_evals = 100),
  )
  optimizer = opt("adbo", design_size = 4)
  optimizer$optimize(instance)
})

