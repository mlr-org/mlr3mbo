format_bib = function(...) {
  str = vapply(list(...), function(entry) tools::toRd(bibentries[[entry]]), FUN.VALUE = "")
  paste0(str, collapse = "\n\n")
}

#' @importFrom utils bibentry
bibentries = c(
  huang_2012 = bibentry("article",
    title    = "Erratum to: Global optimization of stochastic black-box systems via sequential kriging meta-models",
    author   = "D. Huang and T. T. Allen and W. I. Notz and N. Zheng",
    year     = "2012",
    volume   = "54",
    number   = "2",
    pages    = "431--431",
    journal  = "Journal of Global Optimization"
  ),

  jones_1998 = bibentry("article",
    title    = "Efficient Global Optimization of Expensive Black-Box Functions",
    author   = "Jones, Donald R. and Schonlau, Matthias and Welch, William J.",
    year     = "1998",
    volume   = "13",
    number   = "4",
    pages    = "455--492",
    journal  = "Journal of Global optimization"
  ),

  ding_2010 = bibentry("article",
    title   = "An investigation of missing data methods for classification trees applied to binary response data.",
    author  = "Ding, Yufeng and Simonoff, Jeffrey S",
    year    = "2010",
    volume  = "11",
    number  = "1",
    pages   = "131--170",
    journal = "Journal of Machine Learning Research"
  ),

  beume_2007 = bibentry("article",
    title    = "{SMS}-{EMOA}: Multiobjective selection based on dominated hypervolume",
    author   = "Nicola Beume and Boris Naujoks and Michael Emmerich",
    year     = "2007",
    volume   = "181",
    number   = "3",
    pages    = "1653--1669",
    journal  = "European Journal of Operational Research"
  ),

  snoek_2012  = bibentry("inproceedings",
    title     = "Practical Bayesian Optimization of Machine Learning Algorithms",
    author    = "Snoek, Jasper and Larochelle, Hugo and Adams, Ryan P",
    year      = "2012",
    booktitle = "Advances in Neural Information Processing Systems",
    editor    = "F. Pereira and C. J. C. Burges and L. Bottou and K. Q. Weinberger",
    pages     = "2951--2959",
    volume    = "25"
  ),

  kushner_1964 = bibentry("article",
    title      = "A New Method of Locating the Maximum Point of an Arbitrary Multipeak Curve in the Presence of Noise",
    author     = "Kushner, H. J.",
    year       = "1964",
    journal    = "Journal of Basic Engineering",
    volume     = "86",
    number     = "1",
    pages      = "97-106",
  ),

  ponweiser_2007 = bibentry("inproceedings",
    title        = "Multiobjective Optimization on a Limited Budget of Evaluations Using Model-Assisted S-Metric Selection",
    author       = "Ponweiser, Wolfgang and Wagner, Tobias and Biermann, Dirk and Vincze, Markus",
    year         = "2008",
    booktitle    = "Proceedings of the 10th International Conference on Parallel Problem Solving from Nature",
    pages        = "784–794"
  )
)
