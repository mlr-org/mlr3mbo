lg_bbotk = lgr::get_logger("bbotk")
old_threshold_bbotk = lg_bbotk$threshold
lg_bbotk$set_threshold("warn")

lg_mlr3 = lgr::get_logger("mlr3")
old_threshold_mlr3 = lg_mlr3$threshold
lg_mlr3$set_threshold("warn")

