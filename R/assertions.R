assert_xdt = function(xdt) {
  assert_data_table(xdt)
}

assert_xydt = function(xydt, y_cols = NULL) {
  assert_data_table(xydt)
  assert_names(y_cols, "unique", subset.of = names(xydt))
}
