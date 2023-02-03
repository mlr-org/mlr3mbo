format_bib = function(...) {
  str = vapply(list(...), function(entry) tools::toRd(bibentries[[entry]]), FUN.VALUE = "")
  paste0(str, collapse = "\n\n")
}

#' @importFrom utils bibentry
bibentries = c(
  huang_2012 = bibentry("article",
    title    = "Erratum To: Global Optimization of Stochastic Black-box Systems via Sequential Kriging Meta-Models",
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
    title   = "An Investigation of Missing Data Methods for Classification Trees Applied to Binary Response Data",
    author  = "Ding, Yufeng and Simonoff, Jeffrey S",
    year    = "2010",
    volume  = "11",
    number  = "1",
    pages   = "131--170",
    journal = "Journal of Machine Learning Research"
  ),

  beume_2007 = bibentry("article",
    title    = "SMS-EMOA: Multiobjective selection based on dominated hypervolume",
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
    pages      = "97--106",
  ),

  ponweiser_2008 = bibentry("inproceedings",
    title        = "Multiobjective Optimization on a Limited Budget of Evaluations Using Model-Assisted S-Metric Selection",
    author       = "Ponweiser, Wolfgang and Wagner, Tobias and Biermann, Dirk and Vincze, Markus",
    year         = "2008",
    booktitle    = "Proceedings of the 10th International Conference on Parallel Problem Solving from Nature",
    pages        = "784--794"
  ),

  wang_2020 = bibentry("article",
    title   = "Parallel Bayesian Global Optimization of Expensive Functions",
    author  = "Wang, Jialei and Clark, Scott C. and Liu, Eric and Frazier, Peter I.",
    year    = "2020",
    journal = "Operations Research",
    volume  = "68",
    number  = "6",
    pages   = "1850--1865"
  ),

  knowles_2006 = bibentry("article",
    title      = "ParEGO: A Hybrid Algorithm With On-Line Landscape Approximation for Expensive Multiobjective Optimization Problems",
    volume     = "10",
    number     = "1",
    journal    = "IEEE Transactions on Evolutionary Computation",
    author     = "Knowles, Joshua",
    year       = "2006",
    pages      = "50--66"
  ),

  horn_2015    = bibentry("inproceedings",
    title      = "Model-Based Multi-objective Optimization: Taxonomy, Multi-Point Proposal, Toolbox and Benchmark",
    author     = "Horn, Daniel and Wagner, Tobias and Biermann, Dirk and Weihs, Claus and Bischl, Bernd",
    year       = "2015",
    booktitle  = "International Conference on Evolutionary Multi-Criterion Optimization",
    pages      = "64--78"
  ),

  ginsbourger_2008 = bibentry("misc",
    title          = "A Multi-Points Criterion for Deterministic Parallel Global Optimization Based on Gaussian Processes",
    author         = "Ginsbourger, David and Le Riche, Rodolphe and Carraro, Laurent",
    year           = "2008"
  ),

  song_2022 = bibentry("article",
    title    = "A General Recipe for Likelihood-free {B}ayesian Optimization",
    author   = "Song, Jiaming and Yu, Lantao and Neiswanger, Willie and Ermon, Stefano",
    year     = "2022",
    volume   = "162",
    pages    = "20384--20404",
    journal  = "Proceedings of Machine Learning Research"
  )
)
