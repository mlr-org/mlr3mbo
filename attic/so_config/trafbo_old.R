bayesopt_trafbo = function(instance, init_design_size = NULL) {
  if (is.null(init_design_size)) {
    init_design_size = ceiling(instance$terminator$param_set$values$n_evals / 4L)
  }
  design =  generate_design_random(instance$search_space, init_design_size)$data
  instance$eval_batch(design)
  repeat {
    if (runif(1) <= 0.25) {
      candidate = generate_design_random(instance$search_space, 1L)$data
      instance$eval_batch(candidate)
    } else {
      # FIXME: adapt for arbitrary target, optim direction and search space
      dat = copy(instance$archive$data[, c(instance$archive$cols_x, instance$archive$cols_y), with = FALSE])

      y = - dat$val_accuracy
      min_y = min(y) - 0.05 * diff(range(y))
      max_y = max(y)
      y_log = log((y - min_y) / (max_y - min_y))

      lower = min(y_log)
      upper = max(y_log)
      support = c(lower - 0.05 * (upper - lower), upper + 0.05 * (upper - lower))

      dat$val_accuracy = y_log

      y = numeric_var("val_accuracy", support = support, bounds = support)
      yy = Bernstein_basis(y, order = 4, ui = "increasing")

      ctmm = ctm(response = yy, todistr = "Normal")
      ctrl = ctree_control(minbucket = 10L, alpha = 0.05, maxdepth = 10L)
      tree = trafotree(ctmm, formula = val_accuracy ~ ., data = dat, control = ctrl, min_update = 10L)
      plot(tree, tp_args = list(type = "density", id = FALSE, ylines = 0, K = 200))

      q = seq(support[1L], support[2L], length.out = 10001L)
      imp = pmax(lower - q, 0)

      nodes = nodeids(tree, terminal = TRUE)

      if (length(nodes) > 1L) {
        eis = map_dbl(nodes, function(node) {
          xdt = generate_design_random(create_tmp_ss(instance$search_space, tree = tree, node = node), 100L)$data
          xdt[predict(tree, xdt) == node, ][1L, ]
          d = predict(tree, xdt, type = "density", q = q)[, 1L]
          ei = sum(d / sum(d, na.rm = TRUE) * imp, na.rm = TRUE)
          if (ei < sqrt(.Machine$double.eps)) ei = 0
          ei
        })

        best_node = nodes[which.max(eis)]
        tmp = dat[predict(tree, dat) == best_node, ]
        tmp_ss = create_tmp_ss(instance$search_space, tree = tree, node = best_node)
      } else {
        tmp = dat
        tmp_ss = instance$search_space$clone(deep = TRUE)
      }

      task = TaskRegr$new("inner", target = "val_accuracy", backend = tmp)
      learner = as_learner(po("imputeoor", multiplier = 10L) %>>% lrn("regr.ranger", splitrule = "extratrees", num.trees = 1000L))
      learner$train(task)
      f_instance = make_greedy_f_instance(tmp_ss, learner = learner, n_evals = 10010L)
      setorderv(tmp, col = "val_accuracy")
      logger = lgr::get_logger("bbotk")
      logger$set_threshold("warn")
      f_instance$eval_batch(tmp[1:10, ])
      opt("local_search")$optimize(f_instance)
      f_instance$archive$data = f_instance$archive$data[11:10010, ]
      f_instance$terminator$param_set$values$n_evals = 20000L
      opt("random_search", batch_size = 10000L)$optimize(f_instance)
      f_instance$archive$data[, x_domain := list()]  # FIXME: needed due to f_instance ObjectiveRFunDt

      if (length(nodes) > 1L) {
        f_instance$archive$data = f_instance$archive$data[predict(tree, f_instance$archive$data[, f_instance$archive$cols_x, with = FALSE]) == best_node, ]
      }

      candidate = mlr3mbo:::get_best_not_evaluated(f_instance, evaluated = instance$archive$data)
      logger$set_threshold("info")
      instance$eval_batch(candidate[, instance$archive$cols_x, with = FALSE])
    }
    if (instance$is_terminated) break
  }
}

# FIXME: fails with everything not numeric/integer
create_tmp_ss = function(ss, tree, node) {
  tmp_ss = ss$clone(deep = TRUE)
  splits = partykit:::.list.rules.party(tree)[[as.character(node)]]
  split_info = strsplit(splits, " & ")[[1L]]
  
  larger = which(sapply(split_info, grepl, pattern = ">"))
  smaller = which(sapply(split_info, grepl, pattern = "<="))
  
  split_info = strsplit(split_info, " > | <= ")

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

  tmp_ss
}

make_greedy_f_instance = function(ss, learner, n_evals) {
  ss = ss$clone(deep = TRUE)
  ss$trafo = NULL
  f = ObjectiveRFunDt$new(
    fun = function(xdt) data.table(mean = learner$predict_newdata(xdt)$response),
    domain = ss,
    codomain = ps(mean = p_dbl(tags = "minimize"))
  )
  f_instance = OptimInstanceSingleCrit$new(
    objective = f,
    search_space = ss,
    terminator = trm("evals", n_evals = n_evals)
  )
  f_instance
}
