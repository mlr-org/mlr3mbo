format_bib = function(...) {
  str = vapply(list(...), function(entry) tools::toRd(bibentries[[entry]]), FUN.VALUE = "")
  paste0(str, collapse = "\n\n")
}

#' @importFrom utils bibentry
bibentries = c(
  huang_2012 = bibentry("article",
    doi = "10.1007/s10898-011-9821-z",
    year = "2012",
    month = "jan",
    volume = "54",
    number = "2",
    pages = "431--431",
    author = "D. Huang and T. T. Allen and W. I. Notz and N. Zheng",
    title = "Erratum to: Global optimization of stochastic black-box systems via sequential kriging meta-models",
    journal = "Journal of Global Optimization"
  ),

  jones_1998 = bibentry("article",
    title = "Efficient Global Optimization of Expensive Black-Box Functions",
    author = "Jones, Donald R. and Schonlau, Matthias and Welch, William J.",
    year = "1998",
    volume = "13",
    pages = "455--492",
    doi = "10.1023/A:1008306431147",
    journal = "Journal of Global optimization",
    number = "4"
  )
)
