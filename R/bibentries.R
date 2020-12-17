format_bib = function(...) {
  str = vapply(list(...), function(entry) tools::toRd(bibentries[[entry]]), FUN.VALUE = "")
  paste0(str, collapse = "\n\n")
}

#' @importFrom utils bibentry
bibentries = c(
  huang_2012 = bibentry("article",
    title     = "Erratum to: Global optimization of stochastic black-box systems via sequential kriging meta-models",
    author    = "D. Huang and T. T. Allen and W. I. Notz and N. Zheng",
    year      = "2012",
    volume    = "54",
    number    = "2",
    pages     = "431--431",
    journal   = "Journal of Global Optimization",
    doi       = "10.1007/s10898-011-9821-z"
  ),

  jones_1998 = bibentry("article",
    title    = "Efficient Global Optimization of Expensive Black-Box Functions",
    author   = "Jones, Donald R. and Schonlau, Matthias and Welch, William J.",
    year     = "1998",
    volume   = "13",
    number   = "4",
    pages    = "455--492",
    journal  = "Journal of Global optimization",
    doi      = "10.1023/A:1008306431147"
  ),

  ding_2012 = bibentry("article",
    title    = "An investigation of missing data methods for classification trees applied to binary response data.",
    author   = "Ding, Yufeng and Simonoff, Jeffrey S",
    year     = "2010",
    volume   = "11",
    number   = "1",
    pages    = "131--170",
    journal  = "Journal of Machine Learning Research"
  )
)
