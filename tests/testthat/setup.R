old_threshold_bbotk = lg$threshold
lg$set_threshold("warn")

lg_mlr3 = lgr::get_logger("mlr3")
old_threshold_mlr3 = lg_mlr3$threshold
lg_mlr3$set_threshold("warn")

