# FIXME: check log EI again

source("LearnerRegrRangerCustom.R")

bayesopt_trafbo = function(instance, init_design_size = NULL) {
  if (is.null(init_design_size)) {
    init_design_size = ceiling(instance$terminator$param_set$values$n_evals / 4L)
  }
  design =  generate_design_sobol(instance$search_space, init_design_size)$data
  instance$eval_batch(design)
  repeat {
    if (runif(1) <= 0.1) {
      candidate = generate_design_random(instance$search_space, 1L)$data
      instance$eval_batch(candidate)
    } else {
      # FIXME: adapt for arbitrary target, optim direction and search space
      dat = copy(instance$archive$data[, c(instance$archive$cols_x, instance$archive$cols_y), with = FALSE])
      fix_NA_and_chars(dat)
      max_to_min = instance$archive$codomain$tags[[instance$archive$cols_y]] == "maximize"

      y = dat[[instance$archive$cols_y]]
      if (max_to_min) y = - y
      min_y = min(y) - 0.05 * diff(range(y))
      max_y = max(y)
      y_log = log((y - min_y) / (max_y - min_y), base = 10L)

      lower = min(y_log)
      upper = max(y_log)
      support = c(lower - 0.2 * (upper - lower), upper + 0.2 * (upper - lower))
      #support = c(pmin(support[1L], exp(support[1L])), pmax(support[2L], exp(support[2L])))  # density transformation FIXME: exp()

      dat$y_log = y_log

      y_var = numeric_var("y_log", support = support, bounds = support)
      y_bern = Bernstein_basis(y_var, order = 8, ui = "increasing")

      ctmm = ctm(response = y_bern, todistr = "Normal")
      ctrl = ctree_control(minbucket = 10L, alpha = 0.05, maxdepth = Inf)
      tree = trafotree(ctmm, formula = as.formula(paste0("y_log ~ ", paste0(instance$archive$cols_x, collapse = " + "))), data = dat, control = ctrl, min_update = 1L)
      plot(tree, tp_args = list(type = "density", id = FALSE, ylines = 0, K = 200))

      q = seq(support[1L], lower, length.out = 1001L)
      #imp = pmax(lower - q, 0)
      imp = pmax(exp(lower) - exp(q), 0)  # density transformation

      nodes = nodeids(tree, terminal = TRUE)

      xdt = copy(dat)
      xdt[, .node := predict(tree, dat[, instance$archive$cols_x, with = FALSE])]

      if (length(nodes) > 1L) {
        # actually log EI
        eis = map_dbl(nodes, function(node) {
          xdt_node = xdt[.node == node, ][, instance$archive$cols_x, with = FALSE][1L, ]
          d = predict(tree, xdt_node, type = "density", q = q)[, 1L]
          d = d * abs((1 / exp(q)))  # density transformation FIXME: exp

          ei = sum(d * rev(diff(rev(c(imp, 0)))), na.rm = TRUE)
          if (ei < sqrt(.Machine$double.eps)) ei = 0
          ei
        })
        names(eis) = nodes

        best_node = as.numeric(names(shuffle(which(eis == max(eis)))[1L]))  # random tie breaks which can occur due to min_update > minbucket

        # FIXME: we could also proceed to evaluate every other second best node

        tmp = dat[predict(tree, dat[, instance$archive$cols_x, with = FALSE]) == best_node, ][, c("y_log", instance$archive$cols_x), with = FALSE]
        tmp_ss = create_tmp_ss(instance$search_space, tree = tree, node = best_node)
      } else {
        tmp = dat[, c("y_log", instance$archive$cols_x), with = FALSE]
        tmp_ss = instance$search_space$clone(deep = TRUE)
      }

      task = TaskRegr$new("inner", target = "y_log", backend = tmp)

      ranger = LearnerRegrRangerCustom$new()
      ranger$param_set$values$se.method = "simple"
      ranger$param_set$values$splitrule = "extratrees"
      ranger$param_set$values$num.random.splits = 1L
      ranger$param_set$values$num.trees = 10L
      ranger$param_set$values$replace = TRUE
      ranger$param_set$values$sample.fraction = 1
      ranger$param_set$values$min.node.size = 1
      ranger$param_set$values$mtry.ratio = 1
      learner = GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>% po("imputeoor", multiplier = 3) %>>% ranger)
      learner$predict_type = "se"
      learner$train(task)
      logger = lgr::get_logger("bbotk")
      logger$set_threshold("warn")
      if (runif(1) <= 0.5) {
        f_instance = make_greedy_f_instance(tmp_ss, learner = learner, n_evals = 10010L, lower = lower)
        best = copy(instance$archive$data)
        best_tmp = best[, instance$archive$cols_x, with = FALSE]
        fix_NA_and_chars(best_tmp)
        if (length(nodes) > 1L) {
          best[, .node := predict(tree, best_tmp)]
          best = best[.node == best_node, ]
        }
        if (max_to_min) {
          setorderv(best, col = instance$archive$cols_y, order = -1L)
        } else {
          setorderv(best, col = instance$archive$cols_y, order = 1L)
        }
        # FIXME: actually, we should evaluate all in the archive and then choose the 10 best within Local Search
        best = best[1:min(10, nrow(best)), instance$archive$cols_x, with = FALSE]
        f_instance$eval_batch(best)
        opt("local_search", n_points = 100L)$optimize(f_instance)
        f_instance$terminator$param_set$values$n_evals = 20010L
        opt("random_search", batch_size = 10000L)$optimize(f_instance)
      } else {
        f_instance = make_greedy_f_instance(tmp_ss, learner = learner, n_evals = 20000L, lower = lower)
        opt("random_search", batch_size = 10000L)$optimize(f_instance)
      }
      if ("x_domain" %nin% colnames(f_instance$archive$data))  {
        f_instance$archive$data[, x_domain := list()]  # FIXME: why is this necessary
      }

      # FIXME: we want to make sure we evaluate only within the best node
      #if (length(nodes) > 1L) {
      #  tmp = f_instance$archive$data[, f_instance$archive$cols_x, with = FALSE]
      #  fix_NA_and_chars(tmp)
      #  f_instance$archive$data = f_instance$archive$data[predict(tree, tmp) == best_node, ]
      #}

      candidate = mlr3mbo:::get_best_not_evaluated(f_instance, evaluated = instance$archive$data)
      logger$set_threshold("info")
      instance$eval_batch(candidate[, instance$archive$cols_x, with = FALSE])
    }
    if (instance$is_terminated) break
  }
}

create_tmp_ss = function(ss, tree, node) {
  tmp_ss = ss$clone(deep = TRUE)
  splits = partykit:::.list.rules.party(tree)[[as.character(node)]]
  split_info = strsplit(splits, " & ")[[1L]]
  
  larger = which(sapply(split_info, grepl, pattern = ">"))
  smaller = which(sapply(split_info, grepl, pattern = "<="))
  subset = which(sapply(split_info, grepl, pattern = "%in%"))
  
  split_info = strsplit(split_info, " > | <= | %in%")

  for (i in larger) {
    if (tmp_ss$params[[split_info[[i]][[1]]]]$class == "ParamDbl") {
      tmp_ss$params[[split_info[[i]][1]]]$lower = as.numeric(split_info[[i]][2])
    } else if (tmp_ss$params[[split_info[[i]][[1]]]]$class == "ParamInt") {
      tmp_ss$params[[split_info[[i]][1]]]$lower = min(as.integer(split_info[[i]][2]) + 1L, tmp_ss$params[[split_info[[i]][1]]]$upper)
    }
  }

  for (i in smaller) {
    tmp_ss$params[[split_info[[i]][1]]]$upper = as.numeric(split_info[[i]][2])
  }

  for (i in subset) {
    new_levels = unlist(map(tmp_ss$params[[split_info[[i]][1]]]$levels, function(pattern) if (grepl(pattern, x = split_info[[i]][2])) pattern else NULL))
    new_levels = setdiff(new_levels, c(".MISSING"))  # FIXME: depends on imputeoor
    if (length(new_levels) > 0L) {
      tmp_ss$params[[split_info[[i]][1]]]$levels = new_levels
    }
  }

  tmp_ss
}

## Mean
#make_greedy_f_instance = function(ss, learner, n_evals, lower) {
#  ss = ss$clone(deep = TRUE)
#  ss$trafo = NULL
#  f = ObjectiveRFunDt$new(
#    fun = function(xdt) data.table(mean = learner$predict_newdata(fix_NA_and_chars(xdt))$response),
#    domain = ss,
#    codomain = ps(mean = p_dbl(tags = "minimize"))
#  )
#  f_instance = OptimInstanceSingleCrit$new(
#    objective = f,
#    search_space = ss,
#    terminator = trm("evals", n_evals = n_evals)
#  )
#  f_instance
#}

## EI
#make_greedy_f_instance = function(ss, learner, n_evals, lower) {
#  ss = ss$clone(deep = TRUE)
#  ss$trafo = NULL
#  f = ObjectiveRFunDt$new(
#    fun = function(xdt) {
#      p = learner$predict_newdata(fix_NA_and_chars(xdt))
#      mu = p$response
#      se = p$se
#      d = lower - mu
#      d_norm = d / se
#      ei = d * pnorm(d_norm) + se * dnorm(d_norm)
#      ei = ifelse(se < 1e-20, 0, ei)
#      data.table(acq_ei = ei)
#    },
#    domain = ss,
#    codomain = ps(acq_ei = p_dbl(tags = "maximize"))
#  )
#  f_instance = OptimInstanceSingleCrit$new(
#    objective = f,
#    search_space = ss,
#    terminator = trm("evals", n_evals = n_evals)
#  )
#  f_instance
#}

# Log EI
make_greedy_f_instance = function(ss, learner, n_evals, lower) {
  ss = ss$clone(deep = TRUE)
  ss$trafo = NULL
  f = ObjectiveRFunDt$new(
    fun = function(xdt) {
      p = learner$predict_newdata(fix_NA_and_chars(xdt))
      mu = p$response
      se = p$se
      d = lower - mu
      d_norm = d / se
      ei = (exp(lower) * pnorm(d_norm)) - (exp((0.5 * se ^ 2) + mu) * pnorm(d_norm - se))
      ei = ifelse(se < 1e-20, 0, ei)
      data.table(acq_log_ei = ei)
    },
    domain = ss,
    codomain = ps(acq_log_ei = p_dbl(tags = "maximize"))
  )
  f_instance = OptimInstanceSingleCrit$new(
    objective = f,
    search_space = ss,
    terminator = trm("evals", n_evals = n_evals)
  )
  f_instance
}

fix_NA_and_chars = function(xydt) {
  chr_cols = names(xydt)[map_chr(xydt, class) == "character"]
  if (length(chr_cols)) {
    xydt[, `:=`((chr_cols), map(.SD, as_factor_NA_fix)), .SDcols = chr_cols]
  }
  xydt
}

as_factor_NA_fix = function(x) {
  x[is.na(x)] = ".MISSING"
  as.factor(x)
}
