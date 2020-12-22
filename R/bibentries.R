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
  ),

  beume_2007 = bibentry("article",
    doi = "10.1016/j.ejor.2006.08.008",
    year = "2007",
    month = "sep",
    publisher = "Elsevier",
    volume = "181",
    number = "3",
    pages = "1653--1669",
    author = "Nicola Beume and Boris Naujoks and Michael Emmerich",
    title = "{SMS}-{EMOA}: Multiobjective selection based on dominated hypervolume",
    journal = "European Journal of Operational Research"
  ),

  snoek_2012 = bibentry("inproceedings",
    author = "Snoek, Jasper and Larochelle, Hugo and Adams, Ryan P",
    booktitle = "Advances in Neural Information Processing Systems",
    editor = "F. Pereira and C. J. C. Burges and L. Bottou and K. Q. Weinberger",
    pages = "2951--2959",
    publisher = "Curran Associates, Inc.",
    title = "Practical Bayesian Optimization of Machine Learning Algorithms",
    url = "https://proceedings.neurips.cc/paper/2012/file/05311655a15b75fab86956663e1819cd-Paper.pdf",
    volume = "25",
    year = "2012"
  )
)
