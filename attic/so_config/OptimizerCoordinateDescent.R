OptimizerCoordinateDescent = R6Class("OptimizerCoordinateDescent", inherit = bbotk::Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        n_coordinate_tryouts = p_int(lower = 1L, default = 10L, tags = "required"),
        max_gen = p_int(lower = 1L, default = 10L, tags = "required"),
        rds_name = p_uty(default = "cd_instance.rds", tags = "required")
      )
      param_set$values = list(n_coordinate_tryouts = 10L, max_gen = 10L, rds_name = "cd_instance.rds")

      super$initialize(
        id = "coordinate_descent",
        param_set = param_set,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("single-crit", "dependencies"),
        label = "Coordinate Descent",
        man = ""
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      n_coordinate_tryouts = self$param_set$values$n_coordinate_tryouts
      if (inst$archive$n_evals == 0L) {
        xdt = generate_design_random(inst$search_space, n = 1L)$data
        inst$eval_batch(xdt)
      }
      # check if .gen is already present, if yes continue from there
      gen = if (inst$archive$n_evals > 0L & ".gen" %in% colnames(inst$archive$data)) {
        max(inst$archive$data[[".gen"]], na.rm = TRUE)
      } else {
        0L
      }
      set(inst$archive$data, j = ".gen", value = gen)  # 0 for first batch

      y_col = inst$archive$cols_y
      y_col_orig = paste0(y_col, "_orig")

      inst$archive$data[.gen == gen, (y_col_orig) := get(y_col)]

      incumbent = inst$archive$best()[, inst$archive$cols_x, with = FALSE]

      repeat {
        gen = gen + 1L
        for (param_id in shuffle(inst$search_space$ids())) {
          if (!is.na(incumbent[[param_id]])) {
            xdt = get_xdt_coordinate(copy(incumbent), param = inst$search_space$params[[param_id]], n_coordinate_tryouts = n_coordinate_tryouts)
            # previously inactive parameters can now be active and need a value
            if (inst$search_space$has_deps & param_id %in% inst$search_space$deps$on) {
              deps = inst$search_space$deps[on == param_id, ]
              for (i in seq_len(nrow(deps))) {
                to_replace = which(map_lgl(xdt[[param_id]], function(x) deps$cond[[i]]$test(x)))
                if (test_r6(inst$search_space$params[[deps$id[[i]]]]$default, classes = "NoDefault")) {
                  set(xdt, i = to_replace, j = deps$id[[i]], value = sample_random(inst$search_space$params[[deps$id[[i]]]], n = length(to_replace)))  # random
                } else {
                  set(xdt, i = to_replace, j = deps$id[[i]], value = inst$search_space$params[[deps$id[[i]]]]$default)  # default
                }
              }
            }
            xdt = Design$new(inst$search_space, data = xdt, remove_dupl = TRUE)$data  # fixes potentially broken dependencies
            set(xdt, j = ".param", value = param_id)
            xdt = rbind(xdt, incumbent, fill = TRUE)  # also reevaluate the incumbent to see how noisy evaluations are
            set(xdt, j = ".gen", value = gen)
            set(xdt, i = nrow(xdt), j = ".param", value = "incumbent")
            set(xdt, j = "incumbent", value = list(inst$archive$best()[, c(inst$archive$cols_x, inst$archive$cols_y), with = FALSE]))
            inst$eval_batch(xdt)
            incumbent = inst$archive$best()[, inst$archive$cols_x, with = FALSE]
            saveRDS(inst, file = self$param_set$values$rds_name)
          }
        }

        # after each gen, update target evaluations of incumbents that have been evaluated multiple times by their mean
        tmp = copy(inst$archive$data)
        hashes = as.numeric(as.factor(map_chr(seq_len(nrow(tmp)), function(i) paste0(tmp[i, inst$archive$cols_x, with = FALSE], collapse = "_"))))
        tmp[, hash := hashes]
        tmp[.gen == gen, (y_col_orig) := get(y_col)]
        tmp[, n_incumbent_repls := .N, by = .(hash)]
        tmp[, (y_col) := mean(get(y_col_orig)), by = .(hash)]
        inst$archive$data = tmp
        incumbent = inst$archive$best()[, inst$archive$cols_x, with = FALSE]

        if (gen >= self$param_set$values$max_gen) {
          break
        }
      }
    }
  )
)

get_xdt_coordinate = function(incumbent, param, n_coordinate_tryouts) {
  if (param$class == "ParamDbl") {
    x = runif(n = n_coordinate_tryouts, min = param$lower, max = param$upper)
  } else if (param$class == "ParamInt") {
    levels = setdiff(seq(param$lower, param$upper, by = 1L), incumbent[[param$id]])
    n_coordinate_tryouts = min(n_coordinate_tryouts, length(levels))
    x = sample(levels, size = n_coordinate_tryouts, replace = FALSE)
  } else {
    n_coordinate_tryouts = min(n_coordinate_tryouts, param$nlevels - 1L)
    x = sample(x = setdiff(param$levels, incumbent[[param$id]]), size = n_coordinate_tryouts, replace = FALSE)
  }
  xdt = incumbent[rep(1L, n_coordinate_tryouts), ]
  set(xdt, j = param$id, value = x)
  xdt
}

sample_random = function(param, n) {
  if (param$class == "ParamDbl") {
    x = runif(n = n, min = param$lower, max = param$upper)
  } else if (param$class == "ParamInt") {
    levels = seq(param$lower, param$upper, by = 1L)
    x = sample(levels, size = n, replace = TRUE)
  } else {
    x = sample(param$levels, size = n, replace = TRUE)
  }
  x
}

