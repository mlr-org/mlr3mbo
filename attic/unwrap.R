# unwrap a data.table with list columns
# taken (and adapted) from the R package batchtools by Michel Lang 03/15/2019
unwrap = function(x) {
  cols = names(x)[vapply(x, is.list, logical(1))]
  browser()
  res = data.table(.row = seq_len(nrow(x)), key = ".row")
  extra.cols = chsetdiff(names(x), cols)
  if (length(extra.cols))
    res = cbind(res, x[, extra.cols, with = FALSE])

  for (col in cols) {
    xc = x[[col]]

    new.cols = lapply(xc, function(x) {
      if (!is.null(x)) {
        ii = !vapply(x, checkmate::qtest, c("l", "d", "v1"), logical(1)) # FIXME: add parameter `which` to qtestr
        x[ii] = lapply(x[ii], list)
        na = which(is.na(names2(x)))
        if (length(na) > 0L)
          names(x)[na] = sprintf("%s.%i", col, seq_along(na))
      }
      x
    })
    new.cols = rbindlist(new.cols, fill = TRUE, idcol = ".row")

    if (ncol(new.cols) > 1L) {
      if (nrow(new.cols) > nrow(x) || anyDuplicated(new.cols, by = ".row") > 0L)
        stopf("Some rows are unsuitable for unnesting. Unwrapping row in column '%s' leads to multiple rows", col)
      clash = chsetdiff(chintersect(names(res), names(new.cols)), ".row")
      if (length(clash) > 0L)
        stopf("Name clash while unwrapping data.table: Duplicated column names: %s", stri_flatten(clash, ", "))
      res = merge(res, new.cols, all.x = TRUE, by = ".row")
    }
  }

  res[, ".row" := NULL]
  kx = key(x)
  if (!is.null(kx) && all(kx %chin% names(res)))
    setkeyv(res, kx)
  res[]
}

chsetdiff = function(x, y) {
  # Note: assumes that x has no duplicates
  x[chmatch(x, y, 0L) == 0L]
}

chintersect = function(x, y) {
  # Note: assumes that x has no duplicates
  x[chmatch(y, x, 0L)]
}
